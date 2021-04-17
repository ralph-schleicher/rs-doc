;;; common.lisp --- common definitions for all output formats.

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
  "Create a named UUID for string NAME.
Value is a string."
  (nstring-downcase
   (with-output-to-string (stream)
     (print-object (uuid:make-v5-uuid *id-namespace* name) stream))))

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

;;; common.lisp ends here
