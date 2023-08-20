;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "QKAS", invented by the user "Prince" and presented on June
;; 10th, 2015, the competence of which is restricted to the printing of
;; messages only, employing for this telos' pursuit a scheme that naits
;; the position of a letter on the QWERTY keyboard, encoded in a
;; corresponding tally of "+" or "-" characters, while other content may
;; be inserted directly into the program.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-08
;; 
;; Sources:
;;   [esolang2020QKAS]
;;   The Esolang contributors, "QKAS", April 13th, 2020
;;   URL: "https://esolangs.org/wiki/QKAS"
;;   
;;   [hteam2016QKAS]
;;   hteam, "QKAS QWERTY Keyboard Addition/Subtraction.", 2016
;;   URL: "https://web.archive.org/web/20160322075508/
;;         http://hteam.co/qkas/"
;;   Notes:
;;     - The H-Team's QKAS website.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 26) +LOWERCASE-CHARACTERS+))
(declaim (type (simple-string 26) +UPPERCASE-CHARACTERS+))

;;; -------------------------------------------------------

(defparameter +LOWERCASE-CHARACTERS+ "qwertyuiopasdfghjklzxcvbnm"
  "The zero-based lookup table, associating with the position of a
   keyboard key, decremented by one, the respective minuscular
   character.")

(defparameter +UPPERCASE-CHARACTERS+ "QWERTYUIOPASDFGHJKLZXCVBNM"
  "The zero-based lookup table, associating with the position of a
   keyboard key, decremented by one, the respective majuscular
   character.")

;;; -------------------------------------------------------

(defun get-lowercase-character (run-length)
  "Returns the lowercase character associated with the one-based
   RUN-LENGTH."
  (declare (type (integer 1 *) run-length))
  (the character (schar +LOWERCASE-CHARACTERS+ (1- run-length))))

;;; -------------------------------------------------------

(defun get-uppercase-character (run-length)
  "Returns the uppercase character associated with the one-based
   RUN-LENGTH."
  (declare (type (integer 1 *) run-length))
  (the character (schar +UPPERCASE-CHARACTERS+ (1- run-length))))

;;; -------------------------------------------------------

(defun interpret-QKAS (code)
  "Interprets the piece of QKAS source CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      
      (labels
          ((advance ()
            "Moves the POSITION to the next character, if possible,
             updating on success the CHARACTER, and returns no value."
            (if (< position (1- (length code)))
              (setf character (char code (incf position)))
              (setf character NIL))
            (values))
           
           (count-run-length (expected-character)
            "Starting at the current POSITION, consume a series of zero
             or more adjacent instances of the EXPECTED-CHARACTER,
             returning their tally."
            (the (integer 0 *)
              (loop
                while (and character
                           (char= character expected-character))
                count 1
                do    (advance))))
           
           (expect-separator ()
            "Checks whether the current CHARACTER constitutes a
             separator, throwing an error on mismatch, and advancing
             to the next character on success without returning a value."
            (when (and character (char/= character #\o))
              (error "Expected the separator 'o' at position ~d, ~
                      but encountered ~s."
                position character))
            (advance)
            (values)))
        
        (loop do
          (cond
            ((null character)
              (loop-finish))
            
            ((char= character #\-)
              (let ((run-length (count-run-length #\-)))
                (declare (type (integer 1 *) run-length))
                (write-char (get-lowercase-character run-length)))
              (expect-separator))
            
            ((char= character #\+)
              (let ((run-length (count-run-length #\+)))
                (declare (type (integer 1 *) run-length))
                (write-char (get-uppercase-character run-length)))
              (expect-separator))
            
            ;; Letters are not homologated in the CODE.
            ((alpha-char-p character)
              (error "Error at position ~d: The letter ~s is not permitted."
                position character))
            
            ;; Any other character is printed verbatim.
            (T
              (write-char character)
              (advance)
              (expect-separator)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-QKAS "++++++++++++++++o---o-------------------o-------------------o---------o,o o++o---------o----o-------------------o-------------o!")
