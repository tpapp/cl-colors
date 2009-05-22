(in-package :cl-colors)

(defun rgb= (rgb1 rgb2 &optional (epsilon 1e-10))
  (flet ((eps= (a b)
	   (<= (abs (- a b)) epsilon)))
    (with-slots ((red1 red) (green1 green) (blue1 blue)) rgb1
      (with-slots ((red2 red) (green2 green) (blue2 blue)) rgb2
	(and (eps= red1 red2) (eps= green1 green2) (eps= blue1 blue2))))))

(defun test-hsv-rgb ()
  (let* ((rgb (make-instance 'rgb 
	      :red (random 1d0)
	      :green (random 1d0)
	      :blue (random 1d0)))
	 (hsv (rgb->hsv rgb))
	 (rgb2 (hsv->rgb hsv))
	 (result (rgb= rgb rgb2)))
    (unless result
      (format t "~a does not equal ~a~%" rgb rgb2))
    result))
	
(dotimes (i 1000) (test-hsv-rgb))

(defun test-hue-combination (from to positivep)
  (dotimes (i 21)
    (format t "~a " (hue-combination from to (/ i 20) positivep))))

