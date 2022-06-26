;;; package.lisp --- package definition

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

(in-package :common-lisp-user)

(defpackage :rs-doc
  (:use :common-lisp
	:iterate)
  (:import-from #:closer-mop
		#:generic-function-methods
		#:method-lambda-list
		#:method-specializers
		#:method-qualifiers)
  (:import-from #:cl-environments
		#:variable-information
		#:function-information)
  (:export #:gather-doc
	   #:generate-doc
	   #:*text-width*
	   #:*text-wrap*
	   #:*text-indent*
	   #:*html-template*
	   #:*html-values*
	   #:*html-resources*
	   ;; Export these symbols so that the user can write its own
	   ;; predicate function for sorting documentation items.
	   #:doc-item-category
	   #:doc-item-symbol
	   #:compare-doc-item)
  (:documentation "Yet another documentation generator."))

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

;;; package.lisp ends here
