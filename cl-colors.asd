(defpackage #:cl-colors-asd
  (:use :cl :asdf))

(in-package :cl-colors-asd)

(defsystem #:cl-colors
  :description "Simple color library for Common Lisp"
  :version "0.1"
  :author "Tamas K Papp"
  :license "GPL"
  :components ((:file "package")
               (:file "colors" :depends-on ("package"))
	       (:file "colornames" :depends-on ("colors")))
  :depends-on (:cl-utilities))
