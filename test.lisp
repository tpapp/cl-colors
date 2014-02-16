(in-package #:cl-user)

(defpackage #:cl-colors-tests
  (:use #:alexandria #:common-lisp #:cl-colors #:let-plus #:lift)
  (:export #:run))

(in-package #:cl-colors-tests)

(deftestsuite cl-colors-tests () ())

(defun run ()
  "Run all the tests for CL-COLORS-TESTS."
  (run-tests :suite 'cl-colors-tests))

(defun eps= (a b &optional (epsilon 1e-10))
  (<= (abs (- a b)) epsilon))

(defun rgb= (rgb1 rgb2 &optional (epsilon 1e-10))
  "Compare RGB colors for (numerical) equality."
  (let+ (((&rgb red1 green1 blue1) rgb1)
         ((&rgb red2 green2 blue2) rgb2))
    (and (eps= red1 red2 epsilon)
         (eps= green1 green2 epsilon)
         (eps= blue1 blue2 epsilon))))

(defun random-rgb ()
  (rgb (random 1d0) (random 1d0) (random 1d0)))

(addtest (cl-colors-tests)
  rgb<->hsv
  (loop repeat 100 do
    (let ((rgb (random-rgb)))
      (ensure-same rgb (as-rgb (as-hsv rgb)) :test #'rgb=))))

;; (defun test-hue-combination (from to positivep)
;;   (dotimes (i 21)
;;     (format t "~a " (hue-combination from to (/ i 20) positivep))))

(addtest (cl-colors-tests)
  print-hex-rgb
  (let ((rgb (rgb 0.070 0.203 0.337)))
    (ensure-same "#123456" (print-hex-rgb rgb))
    (ensure-same "123456" (print-hex-rgb rgb :hash nil))
    (ensure-same "#135" (print-hex-rgb rgb :short t))
    (ensure-same "135" (print-hex-rgb rgb :hash nil :short t))
    (ensure-same "#12345678" (print-hex-rgb rgb :alpha 0.47))
    (ensure-same "12345678" (print-hex-rgb rgb :alpha 0.47 :hash nil))
    (ensure-same "#1357" (print-hex-rgb rgb :alpha 0.47 :short t))
    (ensure-same "1357" (print-hex-rgb rgb :alpha 0.47 :hash nil :short t))))

(addtest (cl-colors-tests)
  parse-hex-rgb
  (let ((rgb (rgb 0.070 0.203 0.337)))
    (ensure-same rgb (parse-hex-rgb "#123456") :test (rcurry #'rgb= 0.01))
    (ensure-same rgb (parse-hex-rgb "123456") :test (rcurry #'rgb= 0.01))
    (ensure-same rgb (parse-hex-rgb "#135") :test (rcurry #'rgb= 0.01))
    (ensure-same rgb (parse-hex-rgb "135") :test (rcurry #'rgb= 0.01))

    (flet ((aux (list1 list2)
             (and (rgb= (car list1) (car list2) 0.01)
                  (eps= (cadr list1) (cadr list2) 0.01))))
      (ensure-same (list rgb 0.47) (multiple-value-list (parse-hex-rgb "#12345678")) :test #'aux)
      (ensure-same (list rgb 0.47) (multiple-value-list (parse-hex-rgb "12345678")) :test #'aux)
      (ensure-same (list rgb 0.47) (multiple-value-list (parse-hex-rgb "#1357")) :test #'aux)
      (ensure-same (list rgb 0.47) (multiple-value-list (parse-hex-rgb "1357")) :test #'aux))))

(addtest (cl-colors-tests)
  print-hex-rgb/format
  (ensure-same "#123456" (with-output-to-string (*standard-output*)
                           (print-hex-rgb (rgb 0.070 0.203 0.337)
                                          :destination T))))

(addtest (cl-colors-tests)
  hex<->rgb
  (loop repeat 100 do
    (let ((rgb (random-rgb)))
      (ensure-same rgb (parse-hex-rgb (print-hex-rgb rgb)) :test (rcurry #'rgb= 0.01)))))

(addtest (cl-colors-tests)
  parse-hex-rgb-ranges
  (ensure-same (rgb 0.070 0.203 0.337) (parse-hex-rgb "foo#123456zzz" :start 3 :end 10)
               :test (rcurry #'rgb= 0.001)))
