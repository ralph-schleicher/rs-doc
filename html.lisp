;;; html.lisp --- HTML output format.

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

(export '*html-template*)
(defparameter *html-template* (merge-pathnames
			       #P"rs-doc.html.in"
			       (asdf:system-source-directory :rs-doc))
  "The HTML template file.

The template file is coded up by the user.  The ‘generate-doc’
function uses Edi Weitz's HTML-TEMPLATE library for filling in
the documentation strings.  Below is the list of template tags
together with their meaning.

TMPL_VAR TITLE
     Value of the TITLE keyword argument.

TMPL_VAR SUBTITLE
     Value of the SUBTITLE keyword argument.

TMPL_VAR PROLOGUE
     Value of the PROLOGUE keyword argument.

TMPL_LOOP DICTIONARY
     List of dictionary entries.

     TMPL_VAR ID
          The named UUID of the dictionary entry.  Can be used as part
          of an element identifier.  Please note that an UUID may start
          with a decimal digit.

     TMPL_VAR CATEGORY
          The symbol's category, for example, ‘Type’, ‘Variable’, or
          ‘Function’.

     TMPL_VAR SYMBOL
          The symbol name.

     TMPL_VAR LAMBDA-LIST
          The symbol's lambda list.  Value is either nil or a string
          with embedded HTML tags.  That is,

          <span class=\"parameter\">foo</span>
               The parameter ‘foo’.

          <span class=\"lambda-list-keyword\">&amp;key</span>
               The lambda list keyword ‘&key’.

          <span class=\"method-specializer\">(eql 0)</span>
               The method specializer ‘(eql 0)’.  If the class name
               is t, the method specializer is omitted.

     TMPL_VAR METHOD-SPECIALIZERS
          Like the symbol's lambda list above, but only listing the
          method specializers (class name t is not omitted).

     TMPL_VAR DOCUMENTATION
          The documentation string itself.

TMPL_VAR EPILOGUE
     Value of the EPILOGUE keyword argument.")

(export '*html-values*)
(defparameter *html-values* ()
  "Additional HTML template values.")

(export '*html-resources*)
(defparameter *html-resources* ()
  "List of files required by the HTML template file.
For example, CSS style sheets or image files.

The ‘generate-doc’ function will copy these files into the directory
of the generated HTML page.")

(defun str (object)
  (or (and (stringp object) object)
      (with-output-to-string (stream)
	(princ object stream))))

(defun esc (object)
  (cl-who:escape-string (str object)))

(defun element (tag attributes contents)
  "Build a HTML element."
  (cons (if (null attributes) tag (cons tag attributes)) contents))

(defun span (class &rest contents)
  "Create a ‘span’ element."
  (element :span (list :class (str class)) contents))

(defun flatten (tree)
  "Remove redundant list nesting."
  (let (list)
    (labels ((walk (object)
               (cond ((null object))
		     ((atom object)
		      (push object list))
		     ((or (keywordp (car object))
			  (and (consp (car object))
			       (keywordp (caar object))))
		      ;; (:tag ...) or ((:tag ...) ...)
		      (push (cons (car object) (flatten (cdr object))) list))
		     (t
		      (walk (car object))
		      (walk (cdr object))))))
      (walk tree))
    (nreverse list)))

(defparameter *space* " ")

(defun serialize (function objects &key (separator *space*))
  (iter (with insert-separator)
	(for object :in objects)
	(when (and separator insert-separator)
	  (collect separator))
	(collect (funcall function object))
	(setf insert-separator t)))

(defun html-lambda-list (lambda-list)
  (eval `(cl-who:with-html-output-to-string (stream)
	   ,@(flatten
	      (list "("
		    (serialize
		     (lambda (object)
		       (cond ((member object lambda-list-keywords)
			      (span :lambda-list-keyword (esc object)))
			     ((atom object)
			      (span :parameter (esc object)))
			     (t
			      (list "("
				    (span :parameter (esc (first object)))
				    *space*
				    (serialize #'esc (rest object))
				    ")"))))
		     lambda-list)
		    ")")))))

(defun html-method-specializers (method-specializers)
  (eval `(cl-who:with-html-output-to-string (stream)
	   ,@(flatten
	      (list "("
		    (serialize
		     (lambda (object)
		       (span :method-specializer (esc object)))
		     (mapcar #'class-name method-specializers))
		    ")")))))

(defun html-values ()
  "HTML template values."
  (let (values)
    (when *title*
      (push (list :title (esc *title*)) values))
    (when *subtitle*
      (push (list :subtitle (esc *subtitle*)) values))
    (when *prologue*
      (push (list :prologue (esc *prologue*)) values))
    (let ((items (iter (for doc :in *dictionary*)
		       (for kind = (doc-item-kind doc))
		       (for symbol = (doc-item-symbol doc))
		       (for text = (doc-item-documentation doc))
		       (collect `(:id ,(doc-item-id doc)
				  :symbol ,(esc symbol)
				  :category ,(esc (kind-name kind))
				  ,@(when (eq (category kind) :function)
				      (list :lambda-list
					    (html-lambda-list
					     (doc-item-lambda-list doc))))
				  ,@(when (eq kind :method)
				      (list :method-specializers
					    (html-method-specializers
					     (method-specializers
					      (doc-item-method doc)))))
				  :documentation ,(esc text))))))
      (push (list :dictionary items) values))
    (when *epilogue*
      (push (list :epilogue (esc *epilogue*)) values))
    ;; Miscellaneous.
    (let ((system (asdf:find-system :rs-doc)))
      (mapc (lambda (list)
	      (alexandria:when-let ((value (second list)))
		(push (list (first list) (esc value)) values)))
	    `((:documentation-tool-name ,(asdf:component-name system))
	      (:documentation-tool-description ,(asdf:system-description system))
	      (:documentation-tool-author ,(asdf:system-author system))
	      (:documentation-tool-license ,(asdf:system-license system))
	      (:documentation-tool-version ,(asdf:component-version system)))))
    ;; Make VALUES a property list.
    (reduce #'nconc (nreverse values))))

(defun html-doc ()
  "Generate HTML."
  (let ((html-template:*string-modifier* #'identity)
	(html-template:*ignore-empty-lines* t))
    (html-template:fill-and-print-template
     *html-template* (nconc (html-values) *html-values*)
     :stream *standard-output*))
  ;; Copy resources to the output directory.
  (alexandria:when-let
      ((resources *html-resources*)
       (output (ignore-errors
		(pathname *standard-output*))))
    (dolist (resource resources)
      (alexandria:when-let*
	  ((from (truename resource))
	   (to (make-pathname
		:host (pathname-host output)
		:device (pathname-device output)
		:directory (pathname-directory output)
		:name (pathname-name from)
		:type (pathname-type from)))
	   (distinct (not (equal from (probe-file to)))))
	(uiop:copy-file from to)))))

;; html.lisp ends here
