;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the operations commorant in the wike of "5"
;; programs' input and output communications.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-a-line-of-input ()
  "Queries the standard input conduit for a line of input and returns
   the response, without its terminating linebreak, as a fresh simple
   string."
  (format T "~&5> ")
  (finish-output)
  (the simple-string
    (prog1
      (convert-into-a-simple-string
        (read-line NIL NIL ""))
      (clear-input))))

;;; -------------------------------------------------------

(defun attempt-to-parse-as-an-integer (source)
  "Attempts to parse the SOURCE as an unsigned integer number, its
   conformation that of a catena comprehending one or more decimal
   digits, on success returning the represented non-negative integer
   datum; otherwise responds with ``NIL''."
  (declare (type string source))
  (the (or null (integer 0 *))
    (ignore-errors
      (parse-integer source))))

;;; -------------------------------------------------------

(defun attempt-to-extract-the-character-code (source)
  "If the SOURCE represents a string comprehending a single digit,
   returns its character code; otherwise responds with ``NIL''."
  (declare (type string source))
  (the (or null fixnum)
    (and
      (string-is-a-singleton-p source)
      (char-code (char source 0)))))

;;; -------------------------------------------------------

(defun report-an-invalid-input (input)
  "Signals an error of the type ``Invalid-Input-Error'' whose purpose
   involves the apprizal about the INPUT's status as neither an unsigned
   integer number nor a single character."
  (declare (type string input))
  (error 'Invalid-Input-Error :offending-input input))

;;; -------------------------------------------------------

(defun parse-the-user-input (input)
  "Parses the INPUT either as an unsigned integer number, if thilk
   comprehends decimal digits only; or, upon its status as a singleton
   character sequence, produces its character code; in any other case
   signaling an error of an unspecified type."
  (declare (type string input))
  (the (integer 0 *)
    (or (attempt-to-parse-as-an-integer        input)
        (attempt-to-extract-the-character-code input)
        (report-an-invalid-input               input))))
