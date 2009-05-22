;; parse X11's rgb.txt

(require :cl-ppcre)

(let ((color-scanner		     ; will only take names w/o spaces
       (cl-ppcre:create-scanner 
	"^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([\\s\\w]+\?)\\s*$"
	:extended-mode t))
      (comment-scanner
       (cl-ppcre:create-scanner
	"^\\s*!")))
  (with-open-file (s "/usr/share/X11/rgb.txt" 
		   :direction :input
			      :if-does-not-exist :error)
    (with-open-file (colornames "colornames.lisp"
		     :direction :output
				:if-exists :overwrite
					   :if-does-not-exist :create)
      (format colornames ";;;; This file was generated automatically ~
by parse-x11.lisp~%~
;;;; Please do not edit directly.~%~
 (in-package :cl-colors)~%~
 (defmacro define-rgb-color (name red green blue)
   `(progn
      (defconstant ,name (if (boundp ',name)
                             (symbol-value ',name)
                             (make-instance 'rgb
                              :red ,red
                              :green ,green
                              :blue ,blue)))
      (export ',name)))~%")
      (labels ((string-to-float (string)
		 (let ((i (read-from-string string)))
		   (assert (and (typep i 'integer) (<= i 255)))
		   (/ i 255d0))))
	(do ((line (read-line s nil nil) (read-line s nil nil)))
	  ((not line))
	(unless (cl-ppcre:scan-to-strings comment-scanner line)
	  (multiple-value-bind (match registers)
	      (cl-ppcre:scan-to-strings color-scanner line)
	    (if (and match (not (find #\space (aref registers 3))))
		(format colornames
			"(define-rgb-color +~A+ ~A ~A ~A)~%"
			(string-downcase (aref registers 3))
			(string-to-float (aref registers 0))
			(string-to-float (aref registers 1))
			(string-to-float (aref registers 2)))
		(format t "ignoring line ~A~%" line)))))))))
