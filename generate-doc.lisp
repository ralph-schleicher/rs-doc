(ql:quickload :rs-doc)

(in-package :rs-doc-user)

(defvar *prefix* (asdf:system-source-directory "rs-doc"))

(let ((doc (gather-doc
            :title "RS-DOC"
            :package :rs-doc
            :symbols '(get-doc
                       gather-doc
                       generate-doc
                       *cross-references*
                       *lambda-list-init-form*
                       *text-width*
                       *text-wrap*
                       *text-indent*
                       *html-template*
                       *html-values*
                       *html-resources*))))
  (generate-doc :data doc
                :output-format :text
                :output (merge-pathnames #P"doc/rs-doc.txt" *prefix*))
  (generate-doc :data doc
                :output-format :html
                :output (merge-pathnames #P"doc/rs-doc.html" *prefix*)))
