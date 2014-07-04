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

(defun hsv= (hsv1 hsv2 &optional (epsilon 1e-10))
  "Compare HSV colors for (numerical) equality."
  (let+ (((&hsv hue1 saturation1 value1) hsv1)
         ((&hsv hue2 saturation2 value2) hsv2))
    (and (eps= hue1 hue2 epsilon)
         (eps= saturation1 saturation2 epsilon)
         (eps= value1 value2 epsilon))))

(defun hsl= (hsl1 hsl2 &optional (epsilon 1e-10))
  "Compare HSL colors for (numerical) equality."
  (let+ (((&hsl hue1 saturation1 lightness1) hsl1)
         ((&hsl hue2 saturation2 lightness2) hsl2))
    (and (eps= hue1 hue2 epsilon)
         (eps= saturation1 saturation2 epsilon)
         (eps= lightness1 lightness2 epsilon))))

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

(addtest (cl-colors-tests)
  color-conversions
  (loop
    for color in '(((0.070588235294d0 0.203921568627d0 0.33725490196d0)
                    (210 0.7906976744d0 0.3372549019d0)
                    (210 0.6538461538d0 0.2039215686d0)
                    "#123456")
                   ((0.513725490196d0 0.274509803921d0 0.101960784313d0)
                    (25.1428571428d0 0.8015267175d0 0.5137254901d0)
                    (25.1428571428d0 0.6687898089d0 0.3078431372d0)
                    "#83461a"))
    for rgb = (apply #'rgb (car color))
    for hsv = (apply #'hsv (cadr color))
    for hsl = (apply #'hsl (caddr color))
    do (progn
         (ensure-same hsv (as-hsv rgb) :test #'hsv=)
         (ensure-same hsl (as-hsl rgb) :test #'hsl=)
         (ensure-same hsl (as-hsl (as-hsv rgb)) :test #'hsl=)
         (ensure-same hsv (as-hsv (as-hsl rgb)) :test #'hsv=))))

(addtest (cl-colors-tests)
  lightness
  (loop repeat 100 do
    (let* ((hsl (as-hsl (random-rgb)))
           (factor (random 0.5d0))
           (lighter (lighter hsl factor))
           (darker (darker hsl factor)))
      (ensure-same (min 1 (max 0 (+ (hsl-lightness hsl) factor)))
                   (hsl-lightness lighter))
      (ensure-same (min 1 (max 0 (- (hsl-lightness hsl) factor)))
                   (hsl-lightness darker)))))
