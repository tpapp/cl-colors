;;; parse X11's rgb.txt
;;;
;;; no packages defined as this should just be run as a script.

(require :cl-ppcre)
(require :alexandria)

(defun write-package-file (colornames
                           &key (package-template-path "package-template.lisp")
                                (package-file-path "package.lisp"))
  "Write a package definition file, exporting COLORNAMES, using the given template."
  (let* ((package-template (alexandria:read-file-into-string package-template-path))
         (colornames-export
           (reduce (lambda (a b) (format nil "~A~%~A" a b))
                   colornames
                   :key (lambda (colorname)
                          (format nil "   #:+~A+" colorname)))))
    (with-open-file (package-file package-file-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (format package-file package-template colornames-export))
    (values)))

(defun parse-and-write-color-definitions (&key
                                          (source-path "/usr/share/X11/rgb.txt")
                                          (destination-path "colornames.lisp"))
  "Parse color definitions and write them into a file. Return the list of colors (for exporting)."
  (let ((color-scanner                  ; will only take names w/o spaces
          (cl-ppcre:create-scanner
           "^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([\\s\\w]+\?)\\s*$"
           :extended-mode t))
        (comment-scanner (cl-ppcre:create-scanner "^\\s*!"))
        colornames)
    (with-open-file (source source-path
                       :direction :input
                       :if-does-not-exist :error)
      (with-open-file (colordefs destination-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
        (format colordefs ";;;; This file was generated automatically ~
by parse-x11.lisp~%~
;;;; Please do not edit directly, just run make if necessary (but should not be).~2%~
 (in-package #:cl-colors)~2%")
        (labels ((parse-channel (string)
                   (let ((i (read-from-string string)))
                     (assert (and (typep i 'integer) (<= i 255)))
                     (/ i 255))))
          (do ((line (read-line source nil nil) (read-line source nil nil)))
              ((not line))
            (unless (cl-ppcre:scan-to-strings comment-scanner line)
              (multiple-value-bind (match registers)
                  (cl-ppcre:scan-to-strings color-scanner line)
                (if (and match (not (find #\space (aref registers 3))))
                    (let ((colorname (string-downcase (aref registers 3))))
                      (format colordefs
                              "(define-rgb-color ~A ~A ~A ~A)~%"
                              colorname
                              (parse-channel (aref registers 0))
                              (parse-channel (aref registers 1))
                              (parse-channel (aref registers 2)))
                      (push colorname colornames))
                    (format t "ignoring line ~A~%" line)))))))
      (nreverse colornames))))

(let ((colornames (parse-and-write-color-definitions)))
  (write-package-file colornames))
