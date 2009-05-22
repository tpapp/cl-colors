(defpackage :cl-colors
  (:use :common-lisp :cl-utilities)
  (:export rgb red green blue
	   rgba alpha add-alpha
	   hsv hue saturation value
	   rgb->hsv hsv->rgb ->hsv ->rgb
	   convex-combination hue-combination rgb-combination 
	   rgba-combination hsv-combination))
