(defsystem #:cl-colors
  :description "Simple color library for Common Lisp"
  :version "0.2"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :serial t
  :components ((:file "package")
               (:file "colors")
               (:file "colornames")
               (:file "hexcolors"))
  :depends-on (#:alexandria #:let-plus))

(defsystem #:cl-colors-tests
  :description "Unit tests for CL-COLORS."
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :serial t
  :components ((:file "test"))
  :depends-on (#:cl-colors #:lift))
