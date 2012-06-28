(in-package #:cl-user)

(defpackage #:cl-colors-tests
  (:use #:alexandria #:common-lisp #:cl-colors #:let-plus #:lift)
  (:export #:run))

(in-package #:cl-colors-tests)

(deftestsuite cl-colors-tests () ())

(defun run ()
  "Run all the tests for CL-COLORS-TESTS."
  (run-tests :suite 'cl-colors-tests))

(defun rgb= (rgb1 rgb2 &optional (epsilon 1e-10))
  "Compare RGB colors for (numerical) equality."
  (let+ (((&flet eps= (a b) (<= (abs (- a b)) epsilon)))
         ((&rgb red1 green1 blue1) rgb1)
         ((&rgb red2 green2 blue2) rgb2))
    (and (eps= red1 red2) (eps= green1 green2) (eps= blue1 blue2))))

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
