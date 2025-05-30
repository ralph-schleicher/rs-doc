;;; main.lisp --- entry point for the documentation generator

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

(defun %externalp (symbol)
  "True if SYMBOL is an exported symbol in its package."
  (multiple-value-bind (found-symbol status)
      (find-symbol (symbol-name symbol) (symbol-package symbol))
    (and (eq found-symbol symbol) (eq status :external))))

(defun %typep (symbol)
  "True if SYMBOL denotes a type."
  (and (ignore-errors
        ;; Check if SYMBOL is a type specifier.
        #+sbcl
        (sb-int:info :type :kind symbol)
        #-(or sbcl)
        (or (eq symbol 'eql)
            (subtypep symbol t)))
       (or (when (ignore-errors (subtypep symbol 'condition))
             :condition)
           (alexandria:when-let ((class (find-class symbol nil)))
             (cond ((typep class 'structure-class)
                    ;; All classes defined by means of ‘defstruct’ are
                    ;; instances of the class ‘structure-class’.
                    :structure)
                   ((not (typep class 'built-in-class))
                    :class)))
           ;; Any other type.
           :type)))

(defun %variablep (symbol)
  "True if SYMBOL denotes a variable."
  (first (member (variable-information symbol)
                 '(:constant :symbol-macro :special))))

(defun %functionp (symbol)
  "True if SYMBOL denotes a function."
  (or (when (and (fboundp symbol) (typep (fdefinition symbol) 'generic-function))
        :generic-function)
      (first (member (function-information symbol)
                     '(:special-form :macro :function)))))

(defun %function-lambda-list (symbol &optional recursivep)
  "Return the lambda list of the SYMBOL function."
  ;; The function object.
  (let ((function #+(or sbcl clozure)
                  symbol ;a function designator
                  #-(or sbcl clozure)
                  (fdefinition symbol)))
    (canonical-lambda-list (trivial-arguments:arglist function) recursivep)))

(defun %method-qualifiers (method)
  "Return the method qualifiers for method object METHOD."
  (method-qualifiers method))

(defun %method-specializers (method)
  "Return the method specializers for method object METHOD."
  (mapcar (lambda (specializer)
            (if (typep specializer 'closer-mop:eql-specializer)
                (list 'eql (closer-mop:eql-specializer-object specializer))
              (class-name specializer)))
          (method-specializers method)))

(defun %method-lambda-list (method)
  "Return the specialized lambda list for method object METHOD."
  (let ((lambda-list (canonical-lambda-list (method-lambda-list method) nil))
        (specializers (%method-specializers method)))
    (nconc (mapcar (lambda (parameter specializer)
                     (if (eq specializer t)
                         parameter
                       (list parameter specializer)))
                   lambda-list specializers)
           (nthcdr (length specializers) lambda-list))))

(defun make-doc (category symbol documentation &rest properties)
  "Create a new documentation item."
  (let ((doc (make-doc-item
              :namespace (namespace category)
              :category category
              :package (symbol-package symbol)
              :symbol symbol
              :documentation documentation)))
    (when properties
      (apply #'set-doc-items doc properties))
    (let ((signature (signature doc)))
      (set-doc-items doc
       :signature signature)) ;For information only.
    doc))

(defun get-doc (symbol)
  "Return all documentation items for SYMBOL as a list.
A symbol can have multiple definitions.  For each definition,
the ‘get-doc’ function creates one documentation item.

A documentation item is a property list.  Below is the list of
documentation item properties together with their meaning.

:namespace
     The definition's namespace.  Value is either ‘:type’,
     ‘:variable’, or ‘:function’.

:category
     The definition's category, i.e. a refinement of the
     namespace.  A symbol can only have one definition per
     namespace.  The exception are methods.

     For a type, the category is either ‘:condition’,
     ‘:structure’, ‘:class’, or ‘:type’.
     For a variable, the category is either ‘:constant’,
     ‘:symbol-macro’, or ‘:special’.
     For a function, the category is either ‘:special-form’,
     ‘:macro’, ‘:function’, ‘:generic-function’, or ‘:method’.

:package
     The symbol's package; ‘nil’ means that the symbol has no
     home package.

:symbol
     The symbol itself.

:documentation
     The documentation string itself, or ‘nil’ if it is not
     documented.

:lambda-list
     The symbol's lambda list.  Only meaningful for functions.

:method-qualifiers
     A list of method qualifiers, e.g. ‘:around’ or ‘list’.
     Only meaningful for methods.

:method-specializers
     A list of method specializers, e.g., ‘t’ or ‘(eql 0)’.
     Only meaningful for methods.

:setf-function
     Whether or not the symbol has a ‘setf’ function definition.
     This property also applies to generic functions and methods.

     A value of ‘nil’ means that no ‘setf’ function exists.
     A value of ‘:reader’ means that a ‘setf’ function exists
     but this documentation item does not document the ‘setf’
     function.
     A value of ‘:writer’ means that this is the documentation
     item for the ‘setf’ function.
     A value of ‘:accessor’ means that a ‘setf’ function exists
     and this is the documentation item for both functions.

:setf-lambda-list
     The lambda list of the ‘setf’ function.  May only be
     different from ‘:lambda-list’ for an accessor.  The
     lambda list does not include the new value parameter.

:setf-value-parameter
     The new value parameter of a ‘setf’ function lambda
     list.

:signature
     The symbol's signature, i.e. a string of the form
     ‘CATEGORY:PACKAGE-NAME:SYMBOL-NAME’.  For methods,
     the signature is extended by the method qualifiers
     and method specializers.

Examples:

     (get-doc 'pi)
      ⇒ ((:namespace :variable
          :category :constant
          :package #<PACKAGE \"COMMON-LISP\">
          :symbol pi
          :documentation nil
          :signature \"CONSTANT:COMMON-LISP:PI\"))

     (get-doc 'print-object)
      ⇒ ((:namespace :function
          :category :generic-function
          :package #<PACKAGE \"COMMON-LISP\">
          :symbol print-object
          :documentation nil
          :lambda-list (object stream)
          :signature \"GENERIC-FUNCTION:COMMON-LISP:PRINT-OBJECT\")
         (:namespace :function
          :category :method
          :package #<PACKAGE \"COMMON-LISP\">
          :symbol print-object
          :documentation nil
          :lambda-list ((object symbol) stream)
          :method-qualifiers nil
          :method-specializers (symbol t)
          :signature \"METHOD:COMMON-LISP:PRINT-OBJECT (SYMBOL T)\")
         ...)"
  (nconc
   (when-let ((category (%typep symbol)))
     (list (make-doc category symbol (documentation symbol 'type))))
   (when-let ((category (%variablep symbol)))
     (list (make-doc category symbol (documentation symbol 'variable))))
   (let* ((reader-symbol symbol)
          (reader (%functionp reader-symbol))
          (writer-symbol `(setf ,symbol))
          (writer (%functionp writer-symbol)))
     (when-let ((category (or reader writer)))
       (when (and reader writer (not (eq reader writer)))
         (fixme))
       (nconc
        ;; First the function, macro, special form, or generic function.
        (let (reader-documentation reader-lambda-list
              writer-documentation writer-lambda-list writer-value-parameter)
          (when reader
            (setf reader-documentation (documentation reader-symbol 'function)
                  reader-lambda-list (%function-lambda-list reader-symbol (eq category :macro))))
          (when writer
            (setf writer-documentation (documentation writer-symbol 'function))
            ;; Remove the new value parameter from the lambda list of
            ;; the ‘setf’ function.
            (let ((lambda-list (%function-lambda-list writer-symbol)))
              (setf writer-lambda-list (rest lambda-list)
                    writer-value-parameter (first lambda-list))))
          (cond ((not writer)
                 ;; A regular function, macro, or special form.
                 (list (make-doc category symbol reader-documentation
                                 :lambda-list reader-lambda-list)))
                ((not reader)
                 ;; A ‘setf’ function only.
                 (list (make-doc category symbol writer-documentation
                                 :lambda-list writer-lambda-list
                                 :setf-function :writer
                                 :setf-lambda-list writer-lambda-list
                                 :setf-value-parameter writer-value-parameter)))
                ((not writer-documentation)
                 (list (make-doc category symbol reader-documentation
                                 :lambda-list reader-lambda-list
                                 :setf-function :accessor
                                 :setf-lambda-list writer-lambda-list
                                 :setf-value-parameter writer-value-parameter)))
                ;; An accessor with individual documentation strings
                ;; for the reader and the writer.
                ((list (make-doc category symbol reader-documentation
                                 :lambda-list reader-lambda-list
                                 :setf-function :reader)
                       (make-doc category symbol writer-documentation
                                 :lambda-list writer-lambda-list
                                 :setf-function :writer
                                 :setf-lambda-list writer-lambda-list
                                 :setf-value-parameter writer-value-parameter)))))
        ;; Now the methods.
        ;;
        ;; TODO: Only return methods where the specialized lambda
        ;; list contains types listed in *SYMBOLS*.  Add an option
        ;; whether or not to include the generic function when the
        ;; number of methods is greater than zero.
        (when (eq category :generic-function)
          (let* ((reader-methods (when reader
                                   (mapcar (lambda (method)
                                             (list method
                                                   (%method-qualifiers method)
                                                   (%method-specializers method)))
                                           (generic-function-methods
                                            (fdefinition reader-symbol)))))
                 (writer-methods (when writer
                                   (mapcar (lambda (method)
                                             (list method
                                                   (%method-qualifiers method)
                                                   ;; Ignore the specializer of
                                                   ;; the new value parameter.
                                                   (rest (%method-specializers method))))
                                           (generic-function-methods
                                            (fdefinition writer-symbol)))))
                 ;; Accessor methods.
                 (reader-methods* (intersection
                                   reader-methods writer-methods
                                   :key #'rest :test #'equal))
                 (writer-methods* (intersection
                                   writer-methods reader-methods
                                   :key #'rest :test #'equal))
                 ;; Any writers?
                 (writerp (not (null writer-methods))))
            ;; Filter out the accessor methods.
            (setf reader-methods (nset-difference
                                  reader-methods reader-methods*
                                  :key #'first :test #'eq)
                  writer-methods (nset-difference
                                  writer-methods writer-methods*
                                  :key #'first :test #'eq))
            (nconc
             (mapcar (lambda (list)
                       (destructuring-bind (method qualifiers specializers) list
                         (make-doc :method symbol
                                   (or (documentation method t)
                                       ;; Documentation of the generic function.
                                       (when (not qualifiers)
                                         (documentation reader-symbol 'function)))
                                   :lambda-list (%method-lambda-list method)
                                   :method-qualifiers qualifiers
                                   :method-specializers specializers
                                   :setf-function (when writerp :reader))))
                     reader-methods)
             (mapcar (lambda (list)
                       (destructuring-bind (method qualifiers specializers) list
                         (let ((lambda-list (%method-lambda-list method)))
                           (make-doc :method symbol
                                     (or (documentation method t)
                                         (when (not qualifiers)
                                           (documentation writer-symbol 'function)))
                                     :lambda-list (rest lambda-list)
                                     :method-qualifiers qualifiers
                                     :method-specializers specializers
                                     :setf-function :writer
                                     :setf-lambda-list (rest lambda-list)
                                     :setf-value-parameter (first lambda-list)))))
                     writer-methods)
             (mapcar (lambda (list)
                       (destructuring-bind (reader-method qualifiers specializers) list
                         (let* ((writer-method (first (find (rest list) writer-methods*
                                                            :key #'rest :test #'equal)))
                                (writer-lambda-list (%method-lambda-list writer-method)))
                           (make-doc :method symbol
                                     (or (documentation reader-method t)
                                         (when (not qualifiers)
                                           (documentation reader-symbol 'function)))
                                     :lambda-list (%method-lambda-list reader-method)
                                     :method-qualifiers qualifiers
                                     :method-specializers specializers
                                     :setf-function :accessor
                                     :setf-lambda-list (rest writer-lambda-list)
                                     :setf-value-parameter (first writer-lambda-list)))))
                     reader-methods*)))))))))

(defvar *sort-order* (mapcar #'first *category-alist*)
  "Order for sorting equal symbol names.")

(defun compare-doc-item (a b)
  "Predicate for sorting dictionary entries."
  (let* ((sym-a (doc-item-symbol a))
         (name-a (symbol-name sym-a))
         (category-a (doc-item-category a))
         (pos-a (position category-a *sort-order*))
         (sym-b (doc-item-symbol b))
         (name-b (symbol-name sym-b))
         (category-b (doc-item-category b))
         (pos-b (position category-b *sort-order*)))
    (cond ((and (eq (namespace category-a) :type)
                (eq (namespace category-b) :type))
           (or (< pos-a pos-b) (string-lessp name-a name-b)))
          ((eq (namespace category-a) :type)
           t)
          ((eq (namespace category-b) :type)
           nil)
          ((string-equal name-a name-b)
           (< pos-a pos-b))
          ((string-lessp name-a name-b)))))

(defun gather-doc (&key
                     package symbols include exclude undocumented
                     (generic-functions t) (methods t)
                     sort-predicate
                     title subtitle prologue epilogue
                     (print-case :downcase)
                   &allow-other-keys)
  "Gather documentation for Lisp symbols.

Keyword argument PACKAGE denotes a package.  Value is either a string
 designator or a package object.
Keyword argument SYMBOLS is a list of symbols to be documented.
 Null means to document all external symbols of PACKAGE.
Keyword argument INCLUDE is a list of additionals symbols to be
 documented.
Keyword argument EXCLUDE is a list of symbols not to be documented.
Keyword argument UNDOCUMENTED determines whether or not to document
 undocumented symbols.  A value of ’nil‘ means to exclude undocumented
 symbols.  Any other value means to include undocumented symbols.  If
 the value is a string, it will be used as the documentation string.
 Otherwise, use the string ‘Not documented.’.  Default is to exclude
 undocumented symbols.
Keyword argument GENERIC-FUNCTIONS determines whether or not to
 document generic functions.  A value of ‘t’ means to include all
 generic functions, ‘nil’ means to exclude all generic functions, a
 list of symbols means to include the listed generic functions, a
 list starting with the symbol ‘not’ means to exclude the listed
 generic functions.  Default is to include all generic functions.
Keyword argument METHODS determines whether or not to document
 methods.  A value of ‘t’ means to include all methods, ‘nil’ means
 to exclude all methods, a list of symbols means to include the listed
 methods, a list starting with the symbol ‘not’ means to exclude the
 listed methods.  Default is to include all methods.
Keyword argument SORT-PREDICATE is a predicate function for sorting
 documentation items.
Keyword argument TITLE is the title text.  Default is the package
 name of PACKAGE.
Keyword argument SUBTITLE is the subtitle text.  Default is empty.
Keyword argument PROLOGUE is the prologue text.  Default is the
 package documentation string of PACKAGE.
Keyword argument EPILOGUE is the epilogue text.  Default is empty.
Keyword argument PRINT-CASE is the value of ‘*print-case*’ for
 printing symbol names.

Return value is a documentation data structure (a plist) suitable
for the ‘:data’ keyword argument of the ‘generate-doc’ function.

If you want to create the same documentation in different output
formats, then you can call the ‘gather-doc’ function once and pass
the return value to ‘generate-doc’ with different output format
parameters.  For example:

     (let ((doc (gather-doc :package :foo)))
       (generate-doc :data doc
                     :output-format :text
                     :output #P\"foo.txt\")
       (generate-doc :data doc
                     :output-format :html
                     :output #P\"foo.html\"))

This may save some processing time."
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
  (if (null undocumented)
      (setf *dictionary* (delete nil *dictionary* :key #'doc-item-documentation))
    (let ((undocumented (if (stringp undocumented) undocumented "Not documented.")))
      (iter (for doc :in *dictionary*)
            (when (null (get-doc-item doc :documentation))
              (set-doc-item doc :documentation undocumented)))))
  ;; Optionally remove generic functions and/or methods.
  (cond ((null generic-functions)
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :generic-function)
                                                               t)))))
        ((and (consp generic-functions) (eq (car generic-functions) 'not))
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :generic-function)
                                                               (member (get-doc-item doc :symbol) (rest generic-functions))
                                                               t)))))
        ((consp generic-functions)
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :generic-function)
                                                               (not (member (get-doc-item doc :symbol) generic-functions))
                                                               t))))))
  (cond ((null methods)
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :method)
                                                               t)))))
        ((eq methods :default)
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :method)
                                                               (not (every (lambda (type) (eq type t)) (get-doc-item doc :method-specializers)))
                                                               t)))))
        ((and (consp methods) (eq (car methods) 'not))
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :method)
                                                               (member (get-doc-item doc :symbol) (rest methods))
                                                               t)))))
        ((consp methods)
         (setf *dictionary* (delete t *dictionary* :key (lambda (doc)
                                                          (and (eq (get-doc-item doc :category) :method)
                                                               (not (member (get-doc-item doc :symbol) methods))
                                                               t))))))
  ;; Sort documentation items.
  (when sort-predicate
    (setf *dictionary* (stable-sort *dictionary* sort-predicate)))
  ;; Return value.
  (let ((*title* (or title
                     (and (packagep package)
                          (package-name package))))
        (*subtitle* subtitle)
        (*prologue* (or prologue
                        (and (packagep package)
                             (documentation package t))))
        (*epilogue* epilogue)
        (*use-list* (let ((use-list (copy-list *use-list*)))
                      (and package (pushnew package use-list))
                      (iter (for doc :in *dictionary*)
                            (for p = (get-doc-item doc :package))
                            (and p (pushnew p use-list)))
                      use-list)))
    `(:title ,*title*
      :subtitle ,*subtitle*
      :prologue ,*prologue*
      :epilogue ,*epilogue*
      :print-case ,print-case
      :dictionary ,*dictionary*
      :use-list ,*use-list*)))

(defun generate-doc (&rest
                       arguments
                     &key
                       package symbols include exclude undocumented
                       (generic-functions t) (methods t)
                       sort-predicate
                       title subtitle prologue epilogue
                       (print-case :downcase print-case-supplied-p)
                       data (output t) (output-format :text))
  "Generate documentation for Lisp symbols.

Keyword arguments PACKAGE, SYMBOLS, INCLUDE, EXCLUDE,
 GENERIC-FUNCTIONS, METHODS, SORT-PREDICATE, TITLE, SUBTITLE,
 PROLOGUE, EPILOGUE, and PRINT-CASE are equal to the respective
 keyword argument of the ‘gather-doc’ function.  However, these
 parameters are only used if the DATA keyword argument is null.
Keyword argument DATA is a documentation data structure as returned
 by the ‘gather-doc’ function.
Keyword argument OUTPUT is the output destination.  Value is either
 an output stream, a pathname, or a string.  A value of ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a new string.
 Default is ‘t’.
Keyword argument OUTPUT-FORMAT is the output file format.  Value is
 either ‘:text’ or ‘:html’.  Default is to generate plain text."
  (declare (ignorable package symbols include exclude undocumented
                      generic-functions methods
                      sort-predicate
                      title subtitle prologue epilogue))
  (when (null data)
    (setf data (apply #'gather-doc arguments)))
  ;; Generate output.
  (with-standard-io-syntax
    (let ((*title* (getf data :title))
          (*subtitle* (getf data :subtitle))
          (*prologue* (getf data :prologue))
          (*epilogue* (getf data :epilogue))
          (*dictionary* (getf data :dictionary))
          (*use-list* (getf data :use-list))
          (*print-case* (if print-case-supplied-p print-case (getf data :print-case)))
          (*print-escape* nil))
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
