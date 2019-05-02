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

;; A documentation item.
(defstruct doc-item
  (id nil :read-only t)
  (kind nil :read-only t)
  (symbol nil :read-only t)
  (lambda-list nil :read-only t)
  (documentation nil :read-only t)
  ;; Method object.
  (method nil :read-only t))
;; Export these symbols so that the user can write its own
;; predicate function for sorting documentation items.
(export '(doc-item-kind
	  doc-item-symbol))

(defparameter *kind-alist*
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

     (KIND . NAME)

where KIND is a keyword and NAME is a string.")

(defun kind-name (kind)
  "Return the name of symbol type KIND."
  (or (cdr (assoc kind *kind-alist*))
      (fixme)))

(defun category (kind)
  "Return the category of symbol type KIND.

Value is :type, :variable, or :function if KIND denotes a type,
variable, or function respectively.  Otherwise, return KIND."
  (case kind
    ((:condition :structure :class :type)
     :type)
    ((:constant :symbol-macro :special)
     :variable)
    ((:special-form :macro :function :generic-function :method)
     :function)
    (t
     kind)))

;;; common.lisp ends here
