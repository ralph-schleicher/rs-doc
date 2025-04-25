;;; user.lisp --- convenience functions for the user

;; Copyright (C) 2025 Ralph Schleicher

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

(defun rs-doc (&rest options &key title package name &allow-other-keys)
  "Create the documentation files for a package.

Keyword arguments TITLE, PACKAGE, and NAME specify the title, package,
 and output file name respectively.  If the PACKAGE keyword argument
 is ‘nil’, attempt to infer the package name from the TITLE keyword
 argument.  If the NAME keyword argument is ‘nil’, use the lowercase
 package name as the output file name.

The ‘rs-doc’ function calls ‘gather-doc’ with all arguments.  Then
it generates the package documentation in text and HTML file format.
The user can bind the ‘*default-pathname-defaults*’ special variable
to specify the destination, e.g. the directory, for the output files.
The ‘*text-pathname-type*’ and ‘*html-pathname-type*’ special
variables provide the file type for the respective file format.

Return value is the documentation data structure as returned by the
‘gather-doc’ function."
  (check-type title (or string null))
  (check-type package (or package symbol string null))
  (check-type name (or string null))
  ;; The package designator.
  (when (null package)
    (when (null title)
      (error "Missing required argument; neither the ‘:package’ nor the ‘:title’ keyword argument is provided."))
    (setf package (nsubstitute #\- #\Space (string-upcase title))))
  ;; File name of the documentation files.
  (when (null name)
    (setf name (string-downcase
                (if (packagep package)
                    (package-name package)
                  (string package)))))
  ;; Remove captured keyword arguments.
  (dolist (key '(:package :name))
    (iter (while (remf options key))))
  ;; Create the documentation files.
  (let ((data (apply #'gather-doc :package package options)))
    (generate-doc
     :data data
     :output-format :text
     :output (make-pathname :name name :type *text-pathname-type* :defaults *default-pathname-defaults*))
    (generate-doc
     :data data
     :output-format :html
     :output (make-pathname :name name :type *html-pathname-type* :defaults *default-pathname-defaults*))
    data))

;;; user.lisp ends here
