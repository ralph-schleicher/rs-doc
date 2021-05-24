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
          of an HTML element identifier.  Please note that an UUID may
          start with a decimal digit.

     TMPL_VAR NAMESPACE
          The symbol's namespace.  Value is either ‘Type’, ‘Variable’,
          or ‘Function’.  See also the IN-FOO-NAMESPACE conditionals
          below.

     TMPL_IF IN-TYPE-NAMESPACE
     TMPL_IF IN-VARIABLE-NAMESPACE
     TMPL_IF IN-FUNCTION-NAMESPACE
          True if the symbol is defined in the type, variable, or
          function namespace respectively.  These conditionals are
          mutually exclusive.

     TMPL_VAR CATEGORY
          The symbol's category.  Value is either ‘Condition’,
          ‘Structure’, ‘Class’, ‘Type’, ‘Constant’, ‘Symbol Macro’,
          ‘Special Variable’, ‘Special Form’, ‘Macro’, ‘Function’,
          ‘Generic Function’, or ‘Method’.  See also the IS-FOO
          conditionals below.

     TMPL_IF IS-CONDITION
     TMPL_IF IS-STRUCTURE
     TMPL_IF IS-CLASS
     TMPL_IF IS-TYPE
     TMPL_IF IS-CONSTANT
     TMPL_IF IS-SYMBOL-MACRO
     TMPL_IF IS-SPECIAL
     TMPL_IF IS-SPECIAL-FORM
     TMPL_IF IS-MACRO
     TMPL_IF IS-FUNCTION
     TMPL_IF IS-GENERIC-FUNCTION
     TMPL_IF IS-METHOD
          True if the dictionary entry documents a condition,
          structure, class, type specifier, constant, symbol macro,
          special variable, special form, macro, function, generic
          function or method respectively.  These conditionals are
          mutually exclusive.

     TMPL_VAR PACKAGE
          The symbol's package name.  This tag can be used as a
          conditional, too.  False means that the symbol has no
          home package.

     TMPL_VAR SYMBOL
          The symbol name.

     TMPL_LOOP LAMBDA-LIST
          The symbol's lambda list.

          TMPL_VAR LAMBDA-LIST-KEYWORD
               True if the element is a lambda list keyword.  Value
               is the lambda list keyword, e.g. ‘&key’.  Otherwise,
               the element is a parameter specifier.

          TMPL_VAR KEYWORD
               The explicit keyword name for a keyword parameter.

          TMPL_VAR VARIABLE
               The variable name.

          TMPL_VAR INIT-FORM
               The initialization form of a parameter specifier.

          TMPL_VAR METHOD-SPECIALIZER
               True if the element is a method specializer.  Value
               is either a class name, e.g. ‘string’, or a ‘eql’
               specializer, e.g. ‘(eql 0)’.  If the class name of
               the method specializer is t, the value of the HTML
               template variable METHOD-SPECIALIZER is false.

          TMPL_IF IS-EQL-SPECIALIZER
               True if the method specializer is an ‘eql’ specializer.

          TMPL_VAR EQL-SPECIALIZER-OBJECT
               The ‘eql’ specializer object, e.g. ‘0’.

          TMPL_VAR SEPARATOR
               True if this is not the first element.  Value is a
               single space character.

     TMPL_LOOP METHOD-QUALIFIERS
          List of method qualifiers.

          TMPL_VAR METHOD-QUALIFIER
               The method qualifier, e.g. ‘:around’ or ‘list’.

          TMPL_VAR SEPARATOR
               True if this is not the first element.  Value is a
               single space character.

     TMPL_LOOP METHOD-SPECIALIZERS
          List of method specializers (class name t is not omitted).

          TMPL_VAR METHOD-SPECIALIZER
               The method specializer, for exmaple, ‘t’ or ‘(eql 0)’.

          TMPL_IF IS-EQL-SPECIALIZER
               True if the method specializer is an ‘eql’ specializer.

          TMPL_VAR EQL-SPECIALIZER-OBJECT
               The ‘eql’ specializer object, e.g. 0.

          TMPL_VAR SEPARATOR
               True if this is not the first element.  Value is a
               single space character.

     TMPL_VAR DOCUMENTATION
          The documentation string itself.

TMPL_VAR EPILOGUE
     Value of the EPILOGUE keyword argument.

TMPL_VAR DOCUMENTATION-TOOL-NAME
TMPL_VAR DOCUMENTATION-TOOL-DESCRIPTION
TMPL_VAR DOCUMENTATION-TOOL-AUTHOR
TMPL_VAR DOCUMENTATION-TOOL-LICENSE
TMPL_VAR DOCUMENTATION-TOOL-VERSION
     Information about the documentation tool.")

(defparameter *html-values* ()
  "Additional HTML template values.")

(defparameter *html-resources* ()
  "List of files required by the HTML template file.
For example, CSS style sheets or image files.

The ‘generate-doc’ function will copy these files into the directory
of the generated HTML page.")

(defun dup (object)
  (cond ((symbolp object)
	 (%symbol-name object))
	((atom object)
	 object)
	(t
	 (mapcar #'dup object))))

(defun str (object)
  (or (and (stringp object) object)
      (with-output-to-string (stream)
	(princ (dup object) stream))))

(defun esc (object)
  (cl-who:escape-string-minimal (str object)))

(defparameter *space* " "
  "Non-null separator.")

(defun html-lambda-list (lambda-list &optional separator)
  (mapcar (lambda (object)
	    (prog1
		(cond ((member object lambda-list-keywords)
		       (list :lambda-list-keyword (esc object)
			     :separator separator))
		      ((atom object)
		       (list :variable (esc (%symbol-name object t))
			     :separator separator))
		      (t
		       (list :variable (esc (%symbol-name (first object) t))
			     :init-form (esc (second object))
			     :separator separator)))
	      (setf separator *space*)))
	  lambda-list))

(defun html-method-qualifiers (qualifiers &optional separator)
  (mapcar (lambda (qualifier)
	    (prog1
		(list :method-qualifier (esc qualifier)
		      :separator separator)
	      (setf separator *space*)))
	  qualifiers))

(defun html-method-specializer (specializer)
  (nconc (list :method-specializer (esc specializer))
	 (and (consp specializer) (eq (first specializer) 'eql)
	      (list :is-eql-specializer t
		    :eql-specializer-object (esc (second specializer))))))

(defun html-method-specializers (specializers &optional separator)
  (mapcar (lambda (specializer)
	    (prog1
		(nconc (html-method-specializer specializer)
		       (list :separator separator))
	      (setf separator *space*)))
	  specializers))

(defun html-method-lambda-list (lambda-list specializers &optional separator)
  (nconc (mapcar (lambda (object specializer)
		   (prog1
		       ;; OBJECT is already properly formatted.
		       (if (atom object)
			   (list :variable (esc (%symbol-name object t))
				 :separator separator)
			 (nconc (list :variable (esc (%symbol-name (first object) t)))
				(html-method-specializer specializer)
				(list :separator separator)))
		     (setf separator *space*)))
		 lambda-list specializers)
	 (nthcdr (length specializers) (html-lambda-list lambda-list *space*))))

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
		       (for namespace = (get-doc-item doc :namespace))
		       (for category = (get-doc-item doc :category))
		       (for package = (get-doc-item doc :package))
		       (for symbol = (get-doc-item doc :symbol))
		       (collect `(:id ,(get-doc-item doc :id)
				  :namespace ,(esc (namespace-name namespace))
				  ,(make-keyword "IN-" namespace "-NAMESPACE") t
				  :category ,(esc (category-name category))
				  ,(make-keyword "IS-" category) t
				  :package ,(and package (esc (package-name package)))
				  :symbol ,(esc symbol)
				  ,@(when (eq namespace :function)
				      (list :lambda-list
					    (if (eq category :method)
						(html-method-lambda-list
						 (get-doc-item doc :lambda-list)
						 (get-doc-item doc :method-specializers))
					      (html-lambda-list
					       (get-doc-item doc :lambda-list)))))
				  ,@(when (eq category :method)
				      (list :method-qualifiers
					    (html-method-qualifiers
					     (get-doc-item doc :method-qualifiers))
					    :method-specializers
					    (html-method-specializers
					     (get-doc-item doc :method-specializers))))
				  :documentation ,(esc (get-doc-item doc :documentation)))))))
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

;;; html.lisp ends here
