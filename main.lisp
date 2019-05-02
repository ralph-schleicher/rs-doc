;;; main.lisp --- entry point for the documentation generator.

;; Copyright (C) 2019 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :rs-doc)

(defun %typep (symbol)
  "True if SYMBOL denotes a type."
  (or (when (subtypep symbol 'condition)
	:condition)
      (alexandria:when-let ((class (find-class symbol nil)))
	(cond ((typep class 'structure-class)
	       :structure)
	      ((typep class 'standard-class)
	       :class)))
      (and #+sbcl
	   (sb-int:info :type :kind symbol)
	   #-(or sbcl)
	   (fixme)
	   :type)))

(defun %variablep (symbol)
  "True if SYMBOL denotes a variable."
  (first (member (variable-information symbol)
		 '(:constant :symbol-macro :special))))

(defun %functionp (symbol)
  "True if SYMBOL denotes a function."
  (or (when (and (fboundp symbol)
		 (typep (fdefinition symbol) 'standard-generic-function))
	:generic-function)
      (first (member (function-information symbol)
		     '(:special-form :macro :function)))))

(defun %function-lambda-list (function)
  "Return the lambda list for function object FUNCTION."
  (trivial-arguments:arglist function))

(defun %method-lambda-list (method)
  "Return the specialized lambda list for method object METHOD."
  (let ((lambda-list (method-lambda-list method))
	(specializers (method-specializers method)))
    (nconc (mapcar (lambda (parameter specializer)
		     (let ((spec (class-name specializer)))
		       (cond ((eq spec t)
			      parameter)
			     (t
			      (list parameter spec)))))
		   lambda-list specializers)
	   (nthcdr (length specializers) lambda-list))))

(defun %method-specializers (method)
  "Return the method specializers for method object METHOD."
  (mapcar #'class-name (method-specializers method)))

(defvar *id-namespace* (uuid:make-uuid-from-string "ed8931fc-9ce6-4b3d-9f51-2c0b2bd1ec71")
  "Namespace for creating a named UUID.")

(defun make-doc (&key kind symbol lambda-list documentation method)
  "Create a new documentation item."
  ;; Create a named UUID for the documentation item.
  (flet ((write-symbol (symbol stream)
	   (write-string (or (alexandria:when-let
				 ((package (symbol-package symbol)))
			       (package-name package)) "") stream)
	   (write-char #\: stream)
	   (write-string (symbol-name symbol) stream)))
    (let ((id (uuid:make-v5-uuid *id-namespace*
	       (with-output-to-string (stream)
		 (write-string (symbol-name kind) stream)
		 (write-char #\: stream)
		 (write-symbol symbol stream)
		 (when (and (eq kind :method) method)
		   (dolist (specializer (method-specializers method))
		     (write-char #\: stream)
		     (write-symbol (class-name specializer) stream)))))))
      ;; Create the documentation item.
      (make-doc-item :id id
		     :kind kind
		     :symbol symbol
		     :lambda-list lambda-list
		     :documentation documentation
		     :method method))))

(defun get-doc (symbol)
  "Return all documentation items for SYMBOL as a list."
  (nconc
   (alexandria:when-let ((kind (%typep symbol)))
     (list (make-doc
	    :kind kind :symbol symbol
	    :documentation (documentation symbol 'type))))
   (alexandria:when-let ((kind (%variablep symbol)))
     (list (make-doc
	    :kind kind :symbol symbol
	    :documentation (documentation symbol 'variable))))
   (alexandria:when-let ((kind (%functionp symbol)))
     (cons (make-doc
	    :kind kind :symbol symbol
	    :documentation (documentation symbol 'function)
	    :lambda-list (%function-lambda-list (fdefinition symbol)))
	   ;; TODO: Only return methods where the specialized lambda
	   ;; list contains types listed in *SYMBOLS*.  Add an option
	   ;; whether or not to include the generic function when the
	   ;; number of methods is greater than zero.
	   (when (eq kind :generic-function)
	     (mapcar (lambda (method)
		       (make-doc
			:kind :method :symbol symbol
			:documentation (or (documentation method t)
					   (documentation symbol 'function))
			:lambda-list (%method-lambda-list method)
			:method method))
		     (generic-function-methods (fdefinition symbol))))))))

(defparameter *sort-order* (mapcar #'first *kind-alist*)
  "Order for sorting equal symbol names.")

(export 'compare-doc-item)
(defun compare-doc-item (a b)
  "Predicate for sorting dictionary entries."
  (let* ((sym-a (doc-item-symbol a))
	 (name-a (symbol-name sym-a))
	 (kind-a (doc-item-kind a))
	 (pos-a (position kind-a *sort-order*))
	 (sym-b (doc-item-symbol b))
	 (name-b (symbol-name sym-b))
	 (kind-b (doc-item-kind b))
	 (pos-b (position kind-b *sort-order*)))
    (cond ((and (eq (category kind-a) :type)
		(eq (category kind-b) :type))
	   (or (< pos-a pos-b) (string-lessp name-a name-b)))
	  ((eq (category kind-a) :type)
	   t)
	  ((eq (category kind-b) :type)
	   nil)
	  ((string-equal name-a name-b)
	   (< pos-a pos-b))
	  ((string-lessp name-a name-b)))))

(export 'generate-doc)
(defun generate-doc (&key
		       package symbols include exclude sort-predicate
		       title subtitle prologue epilogue (print-case :downcase)
		       (output t) (output-format :text))
  "Generate documentation for Lisp symbols.

Keyword argument PACKAGE denotes a package.  Value is either a
 string designator or a package object.

Keyword argument TITLE is the title text.  Default is the package name
of the PACKAGE keyword argument.

Keyword argument PROLOGUE is the prologue text.  Default is the
package documentation string of the PACKAGE keyword argument."
  ;; Resolve package.
  (unless (or (null package) (packagep package))
    (let ((tem (find-package package)))
      (when (null tem)
	(error "Unknown package ‘~A’." package))
      (setf package tem)))
  ;; Resolve symbols.
  (cond ((and package (null symbols))
	 (do-external-symbols (symbol package)
	   (push symbol symbols))
	 ;; Force sorting.
	 (unless sort-predicate
	   (setf sort-predicate #'compare-doc-item)))
	((and symbols (or include exclude))
	 ;; Make SYMBOLS modifiable.
	 (setf symbols (copy-list symbols))))
  ;; Merge other symbols.
  (when include
    (setf symbols (nunion symbols (copy-list include)
			  :key #'first-safe :test #'eq)))
  (when exclude
    (setf symbols (nset-difference symbols exclude
				   :key #'first-safe :test #'eq)))
  ;; Gather documentation strings.
  (setf *dictionary* (mapcan #'get-doc symbols))
  ;; Remove undocumented elements.
  (setf *dictionary* (delete nil *dictionary* :key #'doc-item-documentation))
  (when sort-predicate
    (setf *dictionary* (stable-sort *dictionary* sort-predicate)))
  ;; Generate output.
  (labels ((ensure-dir (destination)
	     (ignore-errors
	      (ensure-directories-exist
	       (make-pathname
		:directory (pathname-directory (pathname destination))))))
	   (generate ()
	     (ecase output-format
	       (:text
		(text-doc))
	       (:html
		(html-doc)))))
    (with-standard-io-syntax
      (let ((*title* (or title
			 (and (packagep package)
			      (package-name package))))
	    (*subtitle* subtitle)
	    (*prologue* (or prologue
			    (and (packagep package)
				 (documentation package t))))
	    (*epilogue* epilogue)
	    (*print-case* print-case)
	    (*print-escape* nil))
	(cond ((eq output t)
	       (ensure-dir *standard-output*)
	       (generate))
	      ((streamp output)
	       (ensure-dir output)
	       (let ((*standard-output* output))
		 (generate)))
	      ((pathnamep output)
	       (ensure-dir output)
	       (with-open-file (stream output
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
		 (let ((*standard-output* stream))
		   (generate))))
	      ((null output)
	       (with-output-to-string (stream)
		 (let ((*standard-output* stream))
		   (generate))))
	      ((stringp output)
	       (with-output-to-string (stream output)
		 (let ((*standard-output* stream))
		   (generate))))
	      (t
	       (fixme)))
	))))

;;; main.lisp ends here
