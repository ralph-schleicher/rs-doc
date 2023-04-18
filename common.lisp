;;; common.lisp --- common definitions for all output formats

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

(defmacro fixme ()
  `(error "Should not happen."))

(defun first-safe (object)
  "Return the first element denoted by OBJECT.

If OBJECT is an atom, return OBJECT.
Otherwise, return ‘(first OBJECT)’."
  (if (atom object) object (first object)))

(defun rest-safe (object)
  "Return the remaining elements denoted by OBJECT.

If OBJECT is an atom, return nil.
Otherwise, return ‘(rest OBJECT)’."
  (if (atom object) nil (rest object)))

(defvar *title* nil
  "The title (a string).")

(defvar *subtitle* nil
  "The subtitle (a string).")

(defvar *prologue* nil
  "The prologue (a string).")

(defvar *dictionary* nil
  "The dictionary (a list of documentation items).")

(defvar *epilogue* nil
  "The epilogue (a string).")

(defparameter *category-alist*
  '((:package . "Package")
    ;; See ‘%typep’ function.
    (:condition . "Condition")
    (:structure . "Structure")
    (:class . "Class")
    (:type . "Type")
    ;; See ‘%variablep’ function.
    (:constant . "Constant")
    (:symbol-macro . "Symbol Macro")
    (:special . "Special Variable")
    ;; See ‘%functionp’ function.
    (:special-form . "Special Form")
    (:macro . "Macro")
    (:function . "Function")
    (:generic-function . "Generic Function")
    (:method . "Method"))
  "Alist of symbol types.  List elements are cons cells of the form

     (CATEGORY . NAME)

where CATEGORY is a keyword and NAME is a string.")

(defun category-name (category)
  "Return the name of symbol type CATEGORY."
  (or (cdr (assoc category *category-alist*))
      (fixme)))

(defun namespace (category)
  "Return the namespace of symbol type CATEGORY.

Value is :type, :variable, or :function if CATEGORY denotes a type,
variable, or function respectively.  Otherwise, return CATEGORY."
  (case category
    ((:condition :structure :class :type)
     :type)
    ((:constant :symbol-macro :special)
     :variable)
    ((:special-form :macro :function :generic-function :method)
     :function)
    (t
     category)))

(defparameter *namespace-alist*
  '((:type . "Type")
    (:variable . "Variable")
    (:function . "Function"))
  "Alist of namespaces.  List elements are cons cells of the form

     (NAMESPACE . NAME)

where NAMESPACE is a keyword and NAME is a string.")

(defun namespace-name (namespace)
  "Return the name of namespace NAMESPACE."
  (or (cdr (assoc namespace *namespace-alist*))
      (category-name namespace)))

;;;; Documentation items, a.k.a. dictionary entries.

(defun make-doc-item (&key namespace category package symbol documentation)
  "Create a new documentation item."
  (list :namespace namespace
	:category category
	:package package
	:symbol symbol
	:documentation documentation))

(defun get-doc-item (doc key)
  "Return the property value of KEY."
  (getf doc key))

(defun get-doc-items (doc &rest keys)
  "Get the property values of KEYS as multiple values."
  (values-list
   (mapcar (lambda (key) (get-doc-item doc key)) keys)))

(defun set-doc-item (doc key value)
  "Set KEY to VALUE and return DOC."
  (if (get-properties doc (list key))
      (setf (getf doc key) value) ;KEY exists.
    (nconc doc (list key value)))
  doc)

(defun set-doc-items (doc &rest properties)
  "Set PROPERTIES (a plist) and return DOC."
  (iter (for (key value) :on properties :by #'cddr)
	(set-doc-item doc key value))
  doc)

(defun doc-item-category (doc)
  (get-doc-item doc :category))

(defun doc-item-symbol (doc)
  (get-doc-item doc :symbol))

(defun doc-item-documentation (doc)
  (get-doc-item doc :documentation))

(defun doc-item-lambda-list (doc)
  (get-doc-item doc :lambda-list))

(defun signature (doc)
  "Return the signature of a documentation item.  Value is a string."
  (with-output-to-string (stream)
    (let ((category (getf doc :category))
	  (package (getf doc :package))
	  (symbol (getf doc :symbol)))
      (write-string (string category) stream)
      (write-char #\: stream)
      (write-string (or (ignore-errors (package-name package)) "") stream)
      (write-char #\: stream)
      (write-string (symbol-name symbol) stream)
      (when (eq category :method)
	(let ((*package* (find-package :common-lisp))
	      (*print-case* :upcase))
	  (iter (for qualifier :in (getf doc :method-qualifiers))
		(write-char #\Space stream)
		(prin1 qualifier stream))
	  (write-char #\Space stream)
	  (prin1 (getf doc :method-specializers) stream))))))

;;;; Miscellaneous definitions.

(defvar *id-namespace* (uuid:make-uuid-from-string "ed8931fc-9ce6-4b3d-9f51-2c0b2bd1ec71")
  "Namespace for creating a named UUID.")

(defun make-id (name)
  "Create an identifier for string NAME.
If special variable ‘*id-namespace*’ is an ‘uuid:uuid’ object, create
a named UUID with a leading underscore character.  If ‘*id-namespace*’
is an integer, create an identifier with ‘*id-namespace*’ base 32
digits starting with a letter."
  (etypecase *id-namespace*
    (uuid:uuid
     (nstring-downcase
      (with-output-to-string (stream)
        (write-char #\_ stream)
        (print-object (uuid:make-v5-uuid *id-namespace* name) stream))))
    ((integer 4 16)
     ;; A 160 bit hash value creates a sequence of 32 base 32 digits.
     (let* ((seq (rs-basen:basen-encode
                  nil (ironclad:digest-sequence :ripemd-160
                       (babel:string-to-octets
                        name :encoding :utf-8 :use-bom nil :errorp t))
                  :alphabet rs-basen:human-base32-alphabet))
            (start (position-if #'alpha-char-p seq)))
       (let* ((len *id-namespace*)
              (limit (- (length seq) len)))
         (when (or (null start) (> start limit))
           (error "Probability ~R to one against and falling."
                  (ceiling (/ (expt 6/32 limit)))))
         (subseq seq start (+ start len)))))
    (function
     (funcall *id-namespace* name))))

(defun make-keyword (&rest strings)
  "Concatenate one or more string designators
and return a keyword with this name."
  (intern (apply #'concatenate 'string (mapcar #'string strings))
	  (find-package :keyword)))

(defparameter *use-list* (list (find-package :common-lisp))
  "List of accessible packages.")

(defun %symbol-name (symbol &optional (use-list *use-list*))
  "Return the symbol name of SYMBOL including the package prefix.
The package prefix is omitted if the symbol's package is a member
of optional argument USE-LIST.  If USE-LIST is true, always omit
the package prefix."
  (let ((*package* (or (if (atom use-list)
			   (symbol-package symbol)
			 (first (member (symbol-package symbol) use-list :test #'eq)))
		       (find-package :keyword))))
    (prin1-to-string symbol)))

(defvar *lambda-list-init-form* t
  "Whether or not to include the initialization form
for optional and keyword parameters.")

(defun map-lambda-list (function lambda-list &optional recursivep separatorp)
  "Map over the elements of a lambda list.

First argument FUNCTION is the call-back function.  The calling
 convention is documented below.
Second argument LAMBDA-LIST is a lambda list.
Optional third argument RECURSIVEP controls the interpretation of
 a list as a required parameter.  True means it is an inner lambda
 list of a macro or destructuring lambda list.  False means it is
 a specialized parameter of a specialized lambda list.
Optional fourth argument SEPARATORP is the initial value of the same
 parameter of the call-back function.

Value is the list of call-back function return values.

The call-back function has a lambda list of the form

     (ELEMENT CATEGORY SEPARATORP)

First argument OBJECT is an element of the lambda list.
Second argument CATEGORY is a keyword identifying the type of OBJECT.
 Value is either ‘:keyword’ (a lambda list keyword), ‘:parameter’ (a
 variable name), ‘:specialized-parameter’, ‘:optional-parameter’,
 ‘:keyword-parameter’, ‘:auxiliary-variable’, or ‘:lambda-list’.
Third argument SEPARATORP is true if OBJECT is not the first element."
  (iter (with lambda-list-keyword)
	(for object :in lambda-list)
	(if (listp object)
	    (cond ((null lambda-list-keyword)
		   ;; A required parameter.
		   (if recursivep
		       ;; An inner lambda list.
		       (collect (funcall function (map-lambda-list function object recursivep)
					 :lambda-list separatorp))
		     ;; A specialized parameter.
		     (collect (funcall function object :specialized-parameter separatorp))))
		  ((member lambda-list-keyword '(&optional &key &aux))
		   ;; For optional and keyword parameters, reduce
		   ;; ‘(VAR [INIT-FORM [SUPPLIEDP]])’ to VAR.
		   (let ((variable (first object))
			 (init-form (second object)))
		     ;; For keyword parameters, further reduce
		     ;; ‘(KEYWORD-NAME VAR)’ to KEYWORD-NAME.
		     (when (consp variable)
		       (setf variable (first variable)))
		     ;; An optional or keyword parameter.
		     (collect (if (and *lambda-list-init-form* init-form)
				  (funcall function (list variable init-form)
					   (ecase lambda-list-keyword
					     (&optional
					      :optional-parameter)
					     (&key
					      :keyword-parameter)
					     (&aux
					      :auxiliary-variable)) separatorp)
				(funcall function variable :parameter separatorp)))))
		  ((error 'program-error)))
	  (cond ((eq object '&aux)
		 (finish))
		((member object lambda-list-keywords :test #'eq)
		 ;; A lambda list keyword.
		 (setf lambda-list-keyword object)
		 (collect (funcall function object :keyword separatorp)))
		((symbolp object)
		 ;; A parameter.
		 (collect (funcall function object :parameter separatorp))
		 (when (eq lambda-list-keyword '&whole)
		   ;; The next parameter is a required parameter.
		   (setf lambda-list-keyword nil)))
		((error 'program-error))))
	(setf separatorp t)))

(defun map-lambda-list-identity (object &rest rest)
  "Identity call-back function for ‘map-lambda-list’."
  (declare (ignore rest)) object)

(defun canonical-lambda-list (lambda-list recursivep)
  "Simplify a lambda list.

   * Remove auxiliary variables.
   * Remove bogus parameter specifications."
  (map-lambda-list #'map-lambda-list-identity lambda-list recursivep))

(defun ensure-string (object &optional readable)
  "Ensure that OBJECT is a string.
If OBJECT is already a string, return it unmodified.  Otherwise,
 return the printed representation of OBJECT as a new string.
If optional second argument READABLE is true, print OBJECT by
 calling ‘prin1’.  Otherwise, use ‘princ’."
  (if (stringp object)
      object
    (funcall (if readable
		 #'prin1-to-string
	       #'princ-to-string)
	     object)))

(defun pr1 (object)
  "Convert all elements of OBJECT into their printed representation."
  (cond ((symbolp object)
	 (%symbol-name object))
	((atom object)
	 (prin1-to-string object))
	((not (listp (rest object)))
	 (cons (pr1 (first object)) (pr1 (rest object))))
	((and (= (length object) 2) (eq (first object) 'quote))
	 (concatenate 'string "'" (str (second object))))
	((mapcar #'pr1 object))))

(defun str (object)
  "Convert OBJECT into its printed representation."
  (ensure-string (pr1 object)))

;;; common.lisp ends here
