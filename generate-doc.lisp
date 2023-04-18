(ql:quickload :rs-doc)

(let ((prefix (asdf:system-source-directory :rs-doc))
      (doc (rs-doc:gather-doc
            :package :rs-doc
            :symbols '(rs-doc:gather-doc
                       rs-doc:generate-doc
                       rs-doc:*text-width*
                       rs-doc:*text-wrap*
                       rs-doc:*text-indent*
                       rs-doc:*html-template*
                       rs-doc:*html-values*
                       rs-doc:*html-resources*))))
  (rs-doc:generate-doc :data doc
                       :output-format :text
                       :output (merge-pathnames #P"doc/rs-doc.txt" prefix))
  (rs-doc:generate-doc :data doc
                       :output-format :html
                       :output (merge-pathnames #P"doc/rs-doc.html" prefix)))
