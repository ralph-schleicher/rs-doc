(ql:quickload :rs-doc)

(let ((symbols '(rs-doc:generate-doc
		 rs-doc:*text-width*
		 rs-doc:*text-wrap*
		 rs-doc:*text-indent*
		 rs-doc:*html-template*
		 rs-doc:*html-values*
		 rs-doc:*html-resources*)))
  (rs-doc:generate-doc
   :package :rs-doc
   :symbols symbols
   :output-format :text
   :output #P"doc/rs-doc.txt")
  (rs-doc:generate-doc
   :package :rs-doc
   :symbols symbols
   :output-format :html
   :output #P"doc/rs-doc.html"))
