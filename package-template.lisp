;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:common-lisp-user)

(defpackage #:cl-colors
  (:use #:alexandria
        #:common-lisp
        #:let-plus)
  (:export
   #:rgb #:rgb-red #:rgb-green #:rgb-blue #:gray #:&rgb
   #:hsv #:hsv-hue #:hsv-saturation #:hsv-value #:&hsv
   #:rgb-to-hsv #:hsv-to-rgb #:hex-to-rgb #:as-hsv #:as-rgb
   #:rgb-combination #:hsv-combination
   #:parse-hex-rgb #:print-hex-rgb

   ;; predefined color names
~A))
