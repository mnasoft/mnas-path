;;;; mnas-path.asd

(defsystem #:mnas-path
  :description "Describe mnas-path here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on (#:cl-annot #:cl-fad)
  :components ((:file "package")
	       (:file "mnas-path" :depends-on ("package"))))
