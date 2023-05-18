;;; text.lisp --- plain text output format

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

(defvar *text-width* 80
  "The text width.  Value has to be at least 40.")

(defvar *text-wrap* t
  "Whether or not to wrap long lambda lists.")

(defvar *text-indent* 3
  "Whether or not to indent the documentation.
Non-null is the number of characters.")

(defun text-lambda-list-element (object category separatorp)
  (declare (ignore separatorp))
  (if (consp object)
      (ecase category
        (:lambda-list
         object)
        ((:specialized-parameter
          :optional-parameter
          :keyword-parameter
          :auxiliary-variable)
         (if (second object)
             (list (%symbol-name (first object) t) (pr1 (second object)))
           (%symbol-name (first object) t))))
    (ecase category
      (:keyword
       object)
      (:parameter
       (%symbol-name object t)))))

(defun text-lambda-list (lambda-list &optional recursivep)
  (map-lambda-list #'text-lambda-list-element lambda-list recursivep))

(defun text-doc ()
  (check-type *text-width* (integer 40))
  (check-type *text-indent* (or null (integer 0)))
  (let ((*print-right-margin* *text-width*)
        (*print-pretty* (not (null *text-wrap*)))
        (indent (when (and *text-indent* (plusp *text-indent*))
                  (make-string *text-indent* :initial-element #\Space)))
        (first-paragraph t))
    (labels ((new-paragraph ()
               (if (not first-paragraph)
                   (terpri)
                 (setf first-paragraph nil))))
      (when *title*
        (new-paragraph)
        (write-string *title*)
        (terpri))
      (when *subtitle*
        (new-paragraph)
        (write-string *subtitle*)
        (terpri))
      (when *prologue*
        (new-paragraph)
        (write-string *prologue*)
        (terpri))
      (iter (for doc :in *dictionary*)
            (for category = (doc-item-category doc))
            (for symbol = (doc-item-symbol doc))
            (new-paragraph)
            (format t "~V<[~A]~>~%~A~:[~; ~:A~]~%"
                    *text-width* (category-name category) symbol
                    (eq (namespace category) :function)
                    (text-lambda-list
                     (get-doc-item doc :lambda-list)
                     (eq category :macro)))
            (for text = (doc-item-documentation doc))
            (if (not indent)
                (progn
                  ;; Insert an empty line after the definition.
                  (terpri)
                  (write-string text)
                  (terpri))
              (with-input-from-string (stream text)
                (iter (for line = (read-line stream nil))
                      (while (stringp line))
                      (unless (zerop (length line))
                        (write-string indent)
                        (write-string line))
                      (terpri)))))
      (when *epilogue*
        (new-paragraph)
        (write-string *epilogue*)
        (terpri)))))

;;; text.lisp ends here
