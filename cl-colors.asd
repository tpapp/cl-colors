(defsystem #:cl-colors
  :description "Simple color library for Common Lisp"
  :version "0.2"
  :author "Tamas K Papp"
  :license "Boost Software License - Version 1.0"
  :serial t
  :components ((:file "package")
               (:file "colors")
	       (:file "colornames"))
  :depends-on (:alexandria :let-plus))
