(in-package #:cl-colors)

;;; parsing and printing of CSS-like colors

(defun print-hex-rgb (color &key short (hash T) alpha destination)
  "Converts a COLOR to its hexadecimal RGB string representation.  If
SHORT is specified each component gets just one character.

A hash character (#) is prepended if HASH is true (default).

If ALPHA is set it is included as an ALPHA component.

DESTINATION is the first argument to FORMAT, by default NIL."
  (let+ (((&rgb red green blue) (as-rgb color))
         (factor (if short 15 255))
         ((&flet c (x) (round (* x factor)))))
    (format destination (if short
                            "~@[~C~]~X~X~X~@[~X~]"
                            "~@[~C~]~2,'0X~2,'0X~2,'0X~@[~X~]")
            (and hash #\#)
            (c red) (c green) (c blue)
            (and alpha (c alpha)))))

;; TODO: a JUNK-ALLOWED parameter, like for PARSE-INTEGER, would be nice
(defun parse-hex-rgb (string &key (start 0) end)
  "Parses a hexadecimal RGB(A) color string.  Returns a new RGB color value
and an alpha component if present."
  (let* ((length (length string))
         (end (or end length))
         (sub-length (- end start)))
    (cond
      ;; check for valid range, we need at least three and accept at most
      ;; nine characters
      ((and (<= #.(length "fff") sub-length)
            (<= sub-length #.(length "#ffffff00")))
       (when (char= (char string start) #\#)
         (incf start)
         (decf sub-length))
       (labels ((parse (string index offset)
                  (parse-integer string :start index :end (+ offset index)
                                        :radix 16))
                (short (string index)
                  (/ (parse string index 1) 15))
                (long (string index)
                  (/ (parse string index 2) 255)))
         ;; recognize possible combinations of alpha component and length
         ;; of the rest of the encoded color
         (multiple-value-bind (shortp alphap)
             (case sub-length
               (#.(length "fff") (values T NIL))
               (#.(length "fff0") (values T T))
               (#.(length "ffffff") (values NIL NIL))
               (#.(length "ffffff00") (values NIL T)))
           (if shortp
               (values
                (rgb
                 (short string start)
                 (short string (+ 1 start))
                 (short string (+ 2 start)))
                (and alphap (short string (+ 3 start))))
               (values
                (rgb
                 (long string start)
                 (long string (+ 2 start))
                 (long string (+ 4 start)))
                (and alphap (long string (+ 6 start))))))))
      (T
       (error "not enough or too many characters in indicated sequence: ~A"
              (subseq string start end))))))
