(in-package :cl-colors)

;;; color representations

(deftype unit-real ()
  "Real number in [0,1]."
  '(real 0 1))

(defstruct (rgb (:constructor rgb (red green blue)))
  "RGB color."
  (red nil :type unit-real :read-only t)
  (green nil :type unit-real :read-only t)
  (blue nil :type unit-real :read-only t))

(defun gray (value)
  "Create an RGB representation of a gray color (value in [0,1)."
  (rgb value value value))

(define-structure-let+ (rgb) red green blue)

(defstruct (hsv (:constructor hsv (hue saturation value)))
  "HSV color."
  (hue nil :type (real 0 360) :read-only t)
  (saturation nil :type unit-real :read-only t)
  (value nil :type unit-real :read-only t))

(define-structure-let+ (hsv) hue saturation value)

(defstruct (hsl (:constructor hsl (hue saturation lightness)))
  "HSL color."
  (hue nil :type (real 0 360) :read-only t)
  (saturation nil :type unit-real :read-only t)
  (lightness nil :type unit-real :read-only t))

(define-structure-let+ (hsl) hue saturation lightness)

(defun normalize-hue (hue)
  "Normalize hue to the interval [0,360)."
  (mod hue 360))

;;; conversions

(defun normalized-hue (red green blue saturation value delta undefined-hue)
  (flet ((normalize (constant right left)
           (let ((hue (+ constant (/ (* 60 (- right left)) delta))))
             (if (minusp hue)
                 (+ hue 360)
                 hue))))
    (cond
      ((zerop saturation) undefined-hue)         ; undefined
      ((= red value) (normalize 0 green blue))   ; dominant red
      ((= green value) (normalize 120 blue red)) ; dominant green
      (t (normalize 240 red green)))))

(defun rgb-to-hsv (rgb &optional (undefined-hue 0))
  "Convert RGB to HSV representation.  When hue is undefined (saturation is
zero), UNDEFINED-HUE will be assigned."
  (let+ (((&rgb red green blue) rgb)
         (value (max red green blue))
         (delta (- value (min red green blue)))
         (saturation (if (plusp value)
                         (/ delta value)
                         0)))
    (hsv (normalized-hue
          red green blue
          saturation value delta
          undefined-hue)
         saturation
         value)))

(defun hsv-to-rgb (hsv)
  "Convert HSV to RGB representation.  When SATURATION is zero, HUE is
ignored."
  (let+ (((&hsv hue saturation value) hsv))
    ;; if saturation=0, color is on the gray line
    (when (zerop saturation)
      (return-from hsv-to-rgb (gray value)))
    ;; nonzero saturation: normalize hue to [0,6)
    (let+ ((h (/ (normalize-hue hue) 60))
           ((&values quotient remainder) (floor h))
           (p (* value (- 1 saturation)))
           (q (* value (- 1 (* saturation remainder))))
           (r (* value (- 1 (* saturation (- 1 remainder)))))
           ((&values red green blue) (case quotient
                                       (0 (values value r p))
                                       (1 (values q value p))
                                       (2 (values p value r))
                                       (3 (values p q value))
                                       (4 (values r p value))
                                       (t (values value p q)))))
      (rgb red green blue))))

;;; both of the functions below follow the wikipedia description very
;;; closely, thus may not be optimal in terms of performance

(defun rgb-to-hsl (rgb &optional (undefined-hue 0))
  "Convert RGB to HSL representation.  When hue is undefined (saturation is
zero), UNDEFINED-HUE will be assigned."
  (let+ (((&rgb red green blue) rgb)
         (max (max red green blue))
         (min (min red green blue))
         (delta (- max min))
         (lightness (/ (+ max min) 2))
         (saturation (if (plusp delta)
                         (/ delta (- 1 (abs (1- (* 2 lightness)))))
                         0)))
    (hsl (normalized-hue
          red green blue
          saturation max delta
          undefined-hue)
         saturation
         lightness)))

(defun hsl-to-rgb (hsl)
  "Convert HSL to RGB representation.  When SATURATION is zero, HUE is
ignored."
  (let+ (((&hsl hue saturation lightness) hsl)
         (c (* saturation (- 1 (abs (1- (* 2 lightness))))))
         (m (- lightness (/ c 2))))
    ;; if saturation=0, color is on the gray line
    (when (zerop saturation)
      (return-from hsl-to-rgb (gray m)))
    ;; nonzero saturation: normalize hue to [0,6)
    (let+ ((h (/ (normalize-hue hue) 60))
           (x (* c (- 1 (abs (1- (mod h 2))))))
           ((&values red green blue) (case (floor h)
                                       (0 (values c x 0))
                                       (1 (values x c 0))
                                       (2 (values 0 c x))
                                       (3 (values 0 x c))
                                       (4 (values x 0 c))
                                       (t (values c 0 x)))))
      (rgb (+ red m) (+ green m) (+ blue m)))))

(defun hex-to-rgb (string)
  "Parse hexadecimal notation (eg ff0000 or f00 for red) into an RGB color."
  (let+ (((&values width max)
          (case (length string)
            (3 (values 1 15))
            (6 (values 2 255))
            (t (error "string ~A doesn't have length 3 or 6, can't parse as ~
                       RGB specification" string))))
         ((&flet parse (index)
            (/ (parse-integer string :start (* index width)
                                     :end (* (1+ index) width)
                                     :radix 16)
               max))))
    (rgb (parse 0) (parse 1) (parse 2))))



;;; conversion with generic functions

(defgeneric as-hsv (color &optional undefined-hue)
  (:method ((color t) &optional (undefined-hue 0))
    (as-hsv (as-rgb color) undefined-hue))
  (:method ((color rgb) &optional (undefined-hue 0))
    (rgb-to-hsv color undefined-hue))
  (:method ((color hsv) &optional undefined-hue)
    (declare (ignore undefined-hue))
    color))

(defgeneric as-hsl (color &optional undefined-hue)
  (:method ((color t) &optional (undefined-hue 0))
    (as-hsl (as-rgb color) undefined-hue))
  (:method ((color rgb) &optional (undefined-hue 0))
    (rgb-to-hsl color undefined-hue))
  (:method ((color hsl) &optional undefined-hue)
    (declare (ignore undefined-hue))
    color))

(defgeneric as-rgb (color)
  (:method ((rgb rgb))
    rgb)
  (:method ((hsv hsv))
    (hsv-to-rgb hsv))
  (:method ((hsl hsl))
    (hsl-to-rgb hsl))
  (:method ((string string))
    ;; TODO in the long run this should recognize color names too
    (hex-to-rgb string)))



;;; combinations

;;; internal functions

(declaim (inline cc))
(defun cc (a b alpha)
  "Convex combination (1-ALPHA)*A+ALPHA*B, ie  ALPHA is the weight of A."
  (declare (type (real 0 1) alpha))
  (+ (* (- 1 alpha) a) (* alpha b)))

(defun rgb-combination (color1 color2 alpha)
  "Color combination in RGB space."
  (let+ (((&rgb red1 green1 blue1) (as-rgb color1))
         ((&rgb red2 green2 blue2) (as-rgb color2))
         ((&flet c (c1 c2) (cc c1 c2 alpha))))
    (rgb (c red1 red2)
         (c green1 green2)
         (c blue1 blue2))))

(defun hsv-combination (hsv1 hsv2 alpha &optional (positive? t))
  "Color combination in HSV space.  POSITIVE? determines whether the hue
combination is in the positive or negative direction on the color wheel."
  (let+ (((&hsv hue1 saturation1 value1) (as-hsv hsv1))
         ((&hsv hue2 saturation2 value2) (as-hsv hsv2))
         ((&flet c (c1 c2) (cc c1 c2 alpha))))
    (hsv (cond
           ((and positive? (> hue1 hue2))
            (normalize-hue (c hue1 (+ hue2 360))))
           ((and (not positive?) (< hue1 hue2))
            (normalize-hue (c (+ hue1 360) hue2)))
           (t (c hue1 hue2)))
         (c saturation1 saturation2)
         (c value1 value2))))

(defun hsl-combination (hsl1 hsl2 alpha &optional (positive? t))
  "Color combination in HSL space.  POSITIVE? determines whether the hue
combination is in the positive or negative direction on the color wheel."
  (let+ (((&hsl hue1 saturation1 lightness1) (as-hsl hsl1))
         ((&hsl hue2 saturation2 lightness2) (as-hsl hsl2))
         ((&flet c (c1 c2) (cc c1 c2 alpha))))
    (hsv (cond
           ((and positive? (> hue1 hue2))
            (normalize-hue (c hue1 (+ hue2 360))))
           ((and (not positive?) (< hue1 hue2))
            (normalize-hue (c (+ hue1 360) hue2)))
           (t (c hue1 hue2)))
         (c saturation1 saturation2)
         (c lightness1 lightness2))))

(defun add-lightness (color amount)
  "Adjusts the LIGHTNESS of COLOR in HSL space by AMOUNT."
  (let+ (((&hsl hue saturation lightness) (as-hsl color))
         (clamped (min 1 (max 0 (+ lightness amount)))))
    (if (= lightness clamped)
        color
        (hsl hue saturation clamped))))

(defun darker (color &optional (factor (/ 10)))
  "Decreases the LIGHTNESS by AMOUNT (default 0.1).  See ADD-LIGHTNESS."
  (add-lightness color (- factor)))

(defun lighter (color &optional (factor (/ 10)))
  "Increases the LIGHTNESS by AMOUNT (default 0.1).  See ADD-LIGHTNESS."
  (add-lightness color factor))



;;; macros used by the autogenerated files

(defmacro define-rgb-color (name red green blue)
  "Macro for defining and automatically exporting color constants.  Used by
the automatically generated color file."
  (let ((constant-name (symbolicate #\+ name #\+)))
    `(progn
       (defparameter ,constant-name (rgb ,red ,green ,blue)
         ,(format nil "X11 color ~A." name))
       (export ',constant-name))))
