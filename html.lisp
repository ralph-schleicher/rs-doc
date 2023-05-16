;;; html.lisp --- HTML output format

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

          TMPL_IF IS-KEYWORD
          TMPL_IF IS-PARAMETER
          TMPL_IF IS-LAMBDA-LIST
               True if the element is a lambda list keyword, a
               parameter specification, or an inner lambda list
               respectively.  These conditionals are mutually
               exclusive.

          TMPL_VAR KEYWORD
               The lambda list keyword, e.g. ‘&key’.

          TMPL_VAR VARIABLE
               The variable name of a parameter specifier.  If the
               element is a keyword parameter, this is the keyword
               name and not the variable name.

          TMPL_VAR INIT-FORM
               The initialization form of a parameter specifier.
               Can only be true if the element is an optional or
               keyword parameter.

          TMPL_VAR METHOD-SPECIALIZER
               True if the element is a method specializer.  Value
               is either a class name, e.g. ‘string’, or a ‘eql’
               specializer, e.g. ‘(eql 0)’.  If the class name of
               the method specializer is ‘t’, the value of the HTML
               template variable METHOD-SPECIALIZER is false.

          TMPL_IF IS-EQL-SPECIALIZER
               True if the method specializer is an ‘eql’ specializer.

          TMPL_VAR EQL-SPECIALIZER-OBJECT
               The ‘eql’ specializer object, e.g. ‘0’.

          TMPL_LOOP LAMBDA-LIST
               The inner lambda list.

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
          List of method specializers (class name ‘t’ is not omitted).

          TMPL_VAR METHOD-SPECIALIZER
               The method specializer, e.g. ‘t’ or ‘(eql 0)’.

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

(defun esc (object)
  (cl-who:escape-string-minimal (if (stringp object) object (str object))))

(defparameter *space* " "
  "Non-null separator.")

(defun html-lambda-list-element (object category separatorp)
  (let ((separator (and separatorp *space*)))
    (if (consp object)
        (ecase category
          (:lambda-list
           (list :is-lambda-list t
                 :lambda-list object
                 :separator separator))
          ((:optional-parameter
            :keyword-parameter
            :auxiliary-variable)
           (list :is-parameter t
                 :variable (esc (%symbol-name (first object) t))
                 :init-form (esc (pr1 (second object)))
                 :separator separator)))
      (ecase category
        (:keyword
         (list :is-keyword t
               :keyword (esc object)
               :separator separator))
        (:parameter
         (list :is-parameter t
               :variable (esc (%symbol-name object t))
               :separator separator))))))

(defun html-lambda-list (lambda-list &optional recursivep separatorp)
  (map-lambda-list #'html-lambda-list-element lambda-list recursivep separatorp))

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
                           (list :is-parameter t
                                        :variable (esc (%symbol-name object t))
                                 :separator separator)
                         `(:is-parameter t
                           :variable ,(esc (%symbol-name (first object) t))
                           ,@(html-method-specializer specializer)
                           :separator ,separator))
                     (setf separator *space*)))
                 lambda-list specializers)
         (html-lambda-list (nthcdr (length specializers) lambda-list) nil t)))

(defvar *html-cross-reference-template* "<a href=\"#<!-- TMPL_VAR ID -->\"><span class=\"symbol\"><!-- TMPL_VAR SYMBOL --></span></a>"
  "The HTML template for a cross reference.
See the ‘*html-template*’ special variable for a description of the
template tags.  Template values are provided by the dictionary entry
of the cross reference target.")

;; Work variables.
(defvar html-cross-reference-alist)
(defvar html-cross-reference-regex)
(defvar html-cross-reference-source)
(defvar html-cross-reference-template)

(defmacro with-html-cross-references (dictionary &body body)
  "An environment for processing HTML cross references.

Argument DICTIONARY is the list of dictionary entries for the
 HTML template.  See the ‘html-values’ function for details.

The body calls ‘html-add-cross-references’ to replace symbol
names with the content of ‘*html-cross-reference-template*’ in
a documentation string."
  `(let ((html-cross-reference-alist ())
         (html-cross-reference-regex ())
         (html-cross-reference-source nil)
         (html-cross-reference-template nil))
     (html-setup-cross-references ,dictionary)
     ,@body))

(defun html-setup-cross-references (items)
  "Initialize HTML cross references."
  ;; Collect all symbol names.
  (iter (for item :in items)
        ;; This is the escaped symbol name as it may appear in a
        ;; documentation string.
        (for symbol = (getf item :symbol))
        ;; Be case insensitive.
        (for cell = (assoc symbol html-cross-reference-alist :test #'string-equal))
        (if (null cell)
            (push (list symbol item) html-cross-reference-alist)
          (push item (cdr cell))))
  ;; Create the regular expressions (a property list).  Documentation
  ;; strings should use phrases like “See ‘fubar’” or “See the ‘fubar’
  ;; function”.
  (setf html-cross-reference-regex
        (list :symbol (let ((category #.(concatenate 'string
                                                     "package|"
                                                     "condition|structure|class|type|"
                                                     "constant|symbol\\s+macro|(?:special\\s+)?variable|"
                                                     "special\\s+form|macro|function|generic\\s+function|"
                                                     "methods?")))
                        ;; First group matches an optional category prefix,
                        ;; second group matches the symbol name, third group
                        ;; matches an optional category suffix.
                        (cl-ppcre:create-scanner
                         (with-output-to-string (out)
                           (format out "(?:\\b(~A)\\s+)?[‘`](" category)
                           (iter (for cell :in html-cross-reference-alist)
                                 (for symbol = (car cell))
                                 (unless (first-iteration-p)
                                   (write-string "|" out))
                                 (write-string (cl-ppcre:quote-meta-chars symbol) out))
                           (format out ")[’'](?:\\s+(~A)\\b)?" category))
                         :case-insensitive-mode t))
              ;; Scanners for evaluating a category prefix/suffix.
              :package (cl-ppcre:create-scanner
                        "\\A(?:package)\\z"
                        :case-insensitive-mode t)
              :type (cl-ppcre:create-scanner
                     "\\A(?:condition|structure|class|type)\\z"
                     :case-insensitive-mode t)
              :variable (cl-ppcre:create-scanner
                         "\\A(?:constant|symbol\\s+macro|(?:special\\s+)?variable)\\z"
                         :case-insensitive-mode t)
              :function (cl-ppcre:create-scanner
                         "\\A(?:special\\s+form|macro|function|generic\\s+function)\\z"
                         :case-insensitive-mode t)
              :method (cl-ppcre:create-scanner
                       "\\A(?:methods?)\\z"
                       :case-insensitive-mode t)))
  ;; Create the HTML template printer for a cross reference.
  (setf html-cross-reference-template
        (html-template:create-template-printer *html-cross-reference-template*))
  ())

(defun html-cross-reference-regex (tag)
  "Return the CL-PPCRE scanner for TAG."
  (getf html-cross-reference-regex tag))

(defun html-replace-cross-reference (string start end match-start match-end group-start group-end)
  "Call-back function for ‘cl-ppcre:regex-replace’."
  (declare (ignorable start end))
  (let* ((symbol (subseq string (aref group-start 1) (aref group-end 1)))
         (items (cdr (assoc symbol html-cross-reference-alist :test #'string-equal)))
         (item (case (length items)
                 (0
                  ;; No choice.
                  nil)
                 (1
                  ;; Single choice.
                  (first items))
                 (t
                  ;; Multiple choices.
                  (let* ((category (or (when (aref group-start 2) ;suffix
                                         (subseq string (aref group-start 2) (aref group-end 2)))
                                       (when (aref group-start 0) ;prefix
                                         (subseq string (aref group-start 0) (aref group-end 0)))))
                         (namespace (when category
                                      (cond ((cl-ppcre:scan (html-cross-reference-regex :package) category)
                                             :package)
                                            ((cl-ppcre:scan (html-cross-reference-regex :type) category)
                                             :type)
                                            ((cl-ppcre:scan (html-cross-reference-regex :variable) category)
                                             :variable)
                                            ((cl-ppcre:scan (html-cross-reference-regex :function) category)
                                             :function)
                                            ((cl-ppcre:scan (html-cross-reference-regex :method) category)
                                             :method)))))
                    (or (when namespace
                          (if (eq namespace :method)
                              ;; Search for the default method or fall
                              ;; back to the generic function.
                              (or (find-if (lambda (item)
                                             (and (getf item :is-method)
                                                  (null (getf item :method-qualifiers))
                                                  (every (lambda (spec)
                                                           (eq (getf spec :method-specializer) t))
                                                         (getf item :method-specializers))))
                                           items)
                                  (find-if (lambda (item)
                                             (getf item :is-generic-function))
                                           items))
                            ;; Search for a symbol in the respective
                            ;; namespace.
                            (find-if (lambda (item)
                                       (string-equal (getf item :namespace) namespace))
                                     items)))
                        ;; Prefer functions over variables and types.
                        (find-if (lambda (item)
                                   (getf item :in-function-namespace))
                                 items)
                        (find-if (lambda (item)
                                   (getf item :in-variable-namespace))
                                 items)
                        (find-if (lambda (item)
                                   (getf item :in-type-namespace))
                                 items)))))))
    (if (or (null item)
            ;; Omit self links.
            (eq item html-cross-reference-source))
        (subseq string match-start match-end)
      (concatenate 'string
                   (subseq string match-start (aref group-start 1))
                   (with-output-to-string (stream)
                     (html-template:fill-and-print-template
                      html-cross-reference-template item :stream stream))
                   (subseq string (aref group-end 1) match-end)))))

(defun html-add-cross-references (string &optional item)
  "Add cross references to a documentation string.

First argument STRING is the documentation string.
Optional second argument ITEM is the dictionary entry of the
 documentation string, i.e. the source dictionary entry.

Return value is the modified string.  Secondary value is true
if STRING is actually modified."
  (when (stringp string)
    (let ((html-cross-reference-source item))
      (cl-ppcre:regex-replace-all
       (html-cross-reference-regex :symbol) string #'html-replace-cross-reference))))

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
                       (collect `(:id ,(make-id (get-doc-item doc :signature))
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
                                               (get-doc-item doc :lambda-list)
                                               (eq category :macro)))))
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
              (when-let ((value (second list)))
                (push (list (first list) (esc value)) values)))
            `((:documentation-tool-name ,(asdf:component-name system))
              (:documentation-tool-description ,(asdf:system-description system))
              (:documentation-tool-author ,(asdf:system-author system))
              (:documentation-tool-license ,(asdf:system-license system))
              (:documentation-tool-version ,(asdf:component-version system)))))
    ;; Make VALUES a property list.
    (setf values (reduce #'nconc (nreverse values)))
    ;; Resolve cross references.
    (when *cross-references*
      (with-html-cross-references (getf values :dictionary)
        (multiple-value-bind (string modified)
            (html-add-cross-references (getf values :prologue))
          (when modified (setf (getf values :prologue) string)))
        (multiple-value-bind (string modified)
            (html-add-cross-references (getf values :epilogue))
          (when modified (setf (getf values :epilogue) string)))
        (iter (for item :in (getf values :dictionary))
              (multiple-value-bind (string modified)
                  (html-add-cross-references (getf item :documentation) item)
                (when modified (setf (getf item :documentation) string))))))
    ;; Return value.
    values))

(defun html-doc ()
  "Generate HTML."
  (let ((html-template:*string-modifier* #'identity)
        (html-template:*ignore-empty-lines* t))
    (html-template:fill-and-print-template
     *html-template* (nconc (html-values) *html-values*)
     :stream *standard-output*))
  ;; Copy resources to the output directory.
  (when-let ((resources *html-resources*)
             (output (ignore-errors
                      (pathname *standard-output*))))
    (dolist (resource resources)
      (when-let* ((from (truename resource))
                  (to (make-pathname
                       :host (pathname-host output)
                       :device (pathname-device output)
                       :directory (pathname-directory output)
                       :name (pathname-name from)
                       :type (pathname-type from)))
                  (distinct (not (equal from (probe-file to)))))
        (uiop:copy-file from to)))))

;;; html.lisp ends here
