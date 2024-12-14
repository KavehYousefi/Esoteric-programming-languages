;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file applies itself to the provision of the input and output
;; operations upon which the interpreter expresses its dependency.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-input ()
  "Queries the standard input for a line, preceded by a prompt message's
   display, and returns the received string object."
  (format T "~&>> ")
  (finish-output)
  (the string
    (prog1
      (read-line NIL NIL "")
      (clear-input))))

;;; -------------------------------------------------------

(defun parse-user-input (input)
  "Parses the INPUT received from a responsible conduit and returns a
   covenable representation thereof, its preference appertaining to an
   integer object, but resorting to the verbatim string datum upon its
   ineligibility."
  (declare (type string input))
  (the (or integer string)
    (or (ignore-errors
          (parse-integer input))
        input)))

;;; -------------------------------------------------------

(defun wait (milliseconds)
  "Pauses the program for the number of MILLISECONDS and returns no
   value."
  (declare (type integer milliseconds))
  (sleep (/ milliseconds 1000))
  (values))
