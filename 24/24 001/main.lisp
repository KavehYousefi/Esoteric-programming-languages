;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "24", invented by the Esolang user "Cinnamony" and presented
;; on June 19th, 2023, the sole authority of which entails the
;; determination whether the two desinent digits of a year are less than
;; or equal to the value 24, in which case the message "ham n eggs" will
;; be produced, otherwise retaining abstinence from any further actions.
;; 
;; 
;; Concept
;; =======
;; The 24 programming language constitutes an output-only species,
;; restricted in its competence to an aefauld text's ostentation, the
;; same's production depends on the current year.
;; 
;; If the current year's last two digits are less than or equal to the
;; number 24, the message "ham n eggs" is issued to the standard output;
;; apart from this, no response will be accompassed.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-03
;; 
;; Sources:
;;   [esolang202324]
;;   The Esolang contributors, "24", June 19th, 2023
;;   URL: "https://esolangs.org/wiki/24"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-last-two-characters (string)
  "Extracts from the STRING the last zero, one, or two characters, and
   returns these as a string."
  (declare (type string string))
  (the string
    (subseq string
      (max (- (length string) 2) 0)
      (length string))))

;;; -------------------------------------------------------

(defun get-last-two-digits-of-year (year)
  "Returns the last one or two digits of the YEAR as an integer number."
  (declare (type integer year))
  (the (integer 0 99)
    (parse-integer
      (extract-last-two-characters
        (write-to-string year)))))

;;; -------------------------------------------------------

(defun year-less-than-or-equal-to-24-p (year)
  "Determines whether the YEAR's last two digits are less than or equal
   to the value 24, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type integer year))
  (the boolean
    (not (null
      (<= (get-last-two-digits-of-year year)
          24)))))

;;; -------------------------------------------------------

(defun get-current-year ()
  "Returns the current year as an integer number."
  (the integer
    (nth-value 5
      (get-decoded-time))))

;;; -------------------------------------------------------

(defun parse-input (input)
  "Attempts to construe the INPUT as a signed or unsigned integer
   number, on success returning the parsed numeric value, otherwise
   returning ``NIL''."
  (declare (type string input))
  (the (or null integer)
    (handler-case
      (parse-integer input)
      (error () NIL))))

;;; -------------------------------------------------------

(defun show-prompt ()
  "Prints to the standard output a message querying the user for a year,
   mentioning the current one, finishes all pending output operations,
   and returns no value."
  (format *standard-output*
    "~&Please enter a year. The current one is ~d.~
     ~%>> "
    (get-current-year))
  (finish-output *standard-output*)
  (values))

;;; -------------------------------------------------------

(defun query-for-year ()
  "Repeatedly queries the standard input for a year, expected to be a
   signed or unsigned integer value, until a valid response has been
   delivered, and returns the parsed integer object."
  (the integer
    (loop
      initially
        (show-prompt)
      
      for input
        of-type (or null string)
        =       (prog1
                  (read-line *standard-input* NIL "")
                  (clear-input *standard-input*))
      for probed-year
        of-type (or null integer)
        =       (parse-input input)
      
      if probed-year do
        (return probed-year)
      else do
        (format *standard-output*
          "The input ~s does not represent a valid year." input)
        (show-prompt))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-24 (&key (interactive-p NIL))
  "Commences the 24 interpretation process, either automatically
   determining and employing the current year, if INTERACTIVE-P equals
   ``NIL'', or, in the opposite case, querying the user for a year to
   probe, which is essayed, in any case returning no value."
  (declare (type boolean interactive-p))
  (let ((probed-year
          (if interactive-p
            (query-for-year)
            (get-current-year))))
    (declare (type integer probed-year))
    (when (year-less-than-or-equal-to-24-p probed-year)
      (format T "~&ham n eggs")))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Probe the current year.
(interpret-24)

;;; -------------------------------------------------------

;; Probe the current year.
(interpret-24 :interactive-p NIL)

;;; -------------------------------------------------------

;; Interactively query for a year to probe.
(interpret-24 :interactive-p T)
