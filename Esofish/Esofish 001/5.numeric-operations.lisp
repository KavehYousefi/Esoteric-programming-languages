;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements those operations whose governance extends over
;; the handling of numeric devers in a "5" program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of numeric operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replace-all-ones-by-fives (number)
  "Returns a variation of the NUMBER with all occurrencies of the digit
   one (1) substituted by the digit five (5)."
  (declare (type integer number))
  (the integer
    (parse-integer
      (substitute #\5 #\1
        (write-to-string number)
        :test #'char=))))

;;; -------------------------------------------------------

(defun count-the-number-of-fives (number)
  "Returns the tally of the digit five (5) entailed in the NUMBER."
  (declare (type integer number))
  (the (integer 0 *)
    (count #\5
      (write-to-string number)
      :test #'char=)))
