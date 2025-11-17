;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the string perquisition and manipulation
;; operations appertaining to the "5" programming language's
;; requisitums.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE, either
   producing a fresh instance or, upon its compliance with this type,
   responding with the unmodified input itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun string-is-a-singleton-p (source)
  "Determines whether the SOURCE represents a string whose componency
   amounts to exactly one character, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (resolve-to-a-boolean-value
      (= (length source)
         1))))

;;; -------------------------------------------------------

(defun extract-a-string-literal-from-the-5-code (source start)
  "Proceeding from the inclusive START position into the SOURCE, and
   expecting this inchoate index to signify the introducing \"`\"
   character, extracts a \"5\" string literal, concluding with a second
   \"`\" sentinel, and returns two values:
     (1) A fresh simple string entailing the characters betwixt, but
         not including, the jumelle of \"`\" characters.
     (2) The position into the SOURCE immediately succeeding the
         concluding \"`\" character."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (let ((end-position
          (or (position #\` source :start (1+ start) :test #'char=)
              (error "Unterminated string literal starting at ~
                      position ~d into the string ~s."
                start source))))
    (declare (type fixnum end-position))
    (the (values simple-string fixnum)
      (values
        (subseq source (1+ start) end-position)
        (1+ end-position)))))

;;; -------------------------------------------------------

(defun extract-a-number-from-the-5-code (source start)
  "Proceeding from the inclusive START position into the SOURCE,
   extracts a sequence composed of one or more decimal digits and
   returns two values:
     (1) The extracted unsigned integer number.
     (2) The position into the SOURCE immediately succeeding the tmema
         occupied by the extracted digits."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the (values (integer 0 *) fixnum)
    (parse-integer source
      :start start
      :end   (or (position-if-not #'digit-char-p source :start start)
                 (length source)))))
