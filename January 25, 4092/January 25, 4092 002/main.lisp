;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "January 25, 4092", invented by the Esolang user
;; "Threesodas" and presented on February 2nd, 2022, the kenspeckle
;; proprium of which wones in the a twissel of a attributes; imprimis,
;; and most conspicuously, its admission of a single statement only,
;; and, second in this enumeration but paravaunt in its gravity, the
;; requirement to instigate the program's execution exactly on January
;; 25th, 4092.
;; 
;; 
;; Concept
;; =======
;; The "January 25, 4092" programming language subsumed into the
;; ludibund subspecies of the esoteric realm, imposing the stipulation
;; of its aefauld statement's execution to concur with a specific date,
;; January 25th, 4092.
;; 
;; == A SINGLE STATEMENT GOVERNS THE LANGUAGE ==
;; A program in this language is ligated to obedience to the following
;; general forbisen, destitute, however, of whitespaces' impositions:
;; 
;;   CheckDate(01254092) => True = {
;;     Execute(main);
;;   };
;; 
;; == EXECUTION CONSTITUTES A DEPENDENCY ON APPOINTMENT ==
;; A corollary of its agnomination, a program may only be compiled and
;; execute during the specific date of January 25th, 4092. Any trial to
;; request this service at a discrepant occasion will signal an error
;; of an unspecified type.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp proceeds in the source
;; code's evaluation by its segregation into string-valued tokens, ere
;; a juxtaposition with the expected objects either yields a congruency,
;; or incites an error.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-06-29
;; 
;; Sources:
;;   [esolang2022January254092]
;;   The Esolang contributors, "January 25, 4092", May 15th, 2022
;;   URL: "https://esolangs.org/wiki/January_25,_4092"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*) (size '*))
  "The ``list-of'' type defines a list whose elements in their entirety
   comply with the ELEMENT-TYPE, and whose length, upon the SIZE
   imposition's specification, is equinumerant to the same."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (eq size '*)
                (= (length (the list candidate))
                   size))
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of calendar.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Calendar
  "The ``Calendar'' class encapsulates a specific date, specified by its
   year, month, and day, in a Gregorian calendar."
  (year  (error "Missing year.")  :type (integer *  *) :read-only T)
  (month (error "Missing month.") :type (integer 1 12) :read-only T)
  (day   (error "Missing day.")   :type (integer 1 31) :read-only T))

;;; -------------------------------------------------------

(defun make-calendar-from-year-month-and-day (year month day)
  "Returns a fresh ``Calendar'' whose days are appropriated from the
   specified YEAR, MONTH, and DAY."
  (declare (type (integer *  *) year))
  (declare (type (integer 1 12) month))
  (declare (type (integer 1 31) day))
  (the Calendar
    (make-calendar :year year :month month :day day)))

;;; -------------------------------------------------------

(defun make-calendar-from-timestamp (timestamp)
  "Returns a fresh ``Calendar'' derived from the TIMESTAMP, this
   specifying the non-negative tally of seconds having elapsed since
   January 1st, 1900 GMT, at 00:00:00 of time."
  (declare (type (integer 0 *) timestamp))
  (multiple-value-bind
      (second minute hour
       day month year day-of-week
       uses-daylight-saving-time-p time-zone)
      (decode-universal-time timestamp)
    (declare (type (integer 0 59) second))
    (declare (ignore              second))
    (declare (type (integer 0 59) minute))
    (declare (ignore              minute))
    (declare (type (integer 0 23) hour))
    (declare (ignore              hour))
    (declare (type (integer 1 31) day))
    (declare (type (integer 1 12) month))
    (declare (type integer        year))
    (declare (type (integer 0  6) day-of-week))
    (declare (ignore              day-of-week))
    (declare (type boolean        uses-daylight-saving-time-p))
    (declare (ignore              uses-daylight-saving-time-p))
    (declare (type integer        time-zone))
    (declare (ignore              time-zone))
    (the Calendar
      (make-calendar :year year :month month :day day))))

;;; -------------------------------------------------------

(defun make-current-calendar ()
  "Returns a fresh ``Calendar'' initialized with the current date."
  (the Calendar
    (make-calendar-from-timestamp
      (get-universal-time))))

;;; -------------------------------------------------------

(defun calendars-match-p (first-calendar second-calendar)
  "Determines whether the FIRST-CALENDAR and the SECOND-CALENDAR specify
   the same point in time, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Calendar first-calendar))
  (declare (type Calendar second-calendar))
  (the boolean
    (not (null
      (and
        (= (calendar-year  first-calendar)
           (calendar-year  second-calendar))
        (= (calendar-month first-calendar)
           (calendar-month second-calendar))
        (= (calendar-day   first-calendar)
           (calendar-day   second-calendar)))))))

;;; -------------------------------------------------------

(defmethod print-object ((calendar Calendar) (stream T))
  (declare (type Calendar                        calendar))
  (declare (type (or null (eql T) string stream) stream))
  (format stream "~d-~2,'0d-~2,'0d"
    (calendar-year  calendar)
    (calendar-month calendar)
    (calendar-day   calendar)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition J254092-Error (simple-error)
  ()
  (:documentation
    "The ``J254092'' condition type establishes a common substratum for
     all conditions nuncupated to the representation of anomalous
     situation in the course of a \"January 25, 4092\" program's
     evaluation."))

;;; -------------------------------------------------------

(define-condition Invalid-Program-Error (J254092-Error)
  ((offending-program
    :initarg       :offending-program
    :initform      (error "Missing offending program.")
    :reader        invalid-program-error-offending-program
    :type          string
    :documentation "The invalid \"January 25, 4092\" program attemped
                    to compile or execution."))
  (:documentation
    "The ``Invalid-Program-Error'' condition type serves to apprize
     about an erroneous structure in the sole admissible
     \"January 25, 4092\" program."))

;;; -------------------------------------------------------

(define-condition Invalid-Date-Error (J254092-Error)
  ((offending-date
    :initarg       :offending-date
    :initform      (error "Missing offending date.")
    :reader        invalid-date-error-offending-date
    :type          Calendar
    :documentation "The invalid date on which a compilation or
                    execution has been endeavored."))
  (:documentation
    "The ``Invalid-Date-Error'' condition type serves to apprize about
     a \"January 25, 4092\" program's compilation and/or execution at
     an invalid date."))

;;; -------------------------------------------------------

(defun signal-invalid-program-error (offending-program)
  "Signals an error of the type ``Invalid-Program-Error'' which
   communicates the attempted, and invalid, \"January 25, 4092\"
   OFFENDING-PROGRAM's involvement."
  (declare (type string offending-program))
  (error 'Invalid-Program-Error
    :offending-program offending-program
    :format-control    "The program ~s is invalid."
    :format-arguments  (list offending-program)))

;;; -------------------------------------------------------

(defun signal-invalid-date-error (offending-date)
  "Signals an error of the type ``Invalid-Date-Error'' which
   communicates the attempted date of a \"January 25, 4092\" program's
   compilation or execution."
  (declare (type Calendar offending-date))
  (error 'Invalid-Date-Error
    :offending-date   offending-date
    :format-control   "A \"January 25, 4092\" program ~
                       cannot be executed on ~a."
    :format-arguments (list offending-date)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of string 15) +EXPECTED-PROGRAM-TOKENS+))
(declaim (type Calendar            +VALID-COMPILATION-DATE+))

;;; -------------------------------------------------------

(defparameter +EXPECTED-PROGRAM-TOKENS+
  '("CheckDate" "(" "01254092" ")" "=>" "True" "=" "{"
    "Execute" "(" "main" ")" ";" "}" ";")
  "Defines the tokens, in their correct order, comprising a valid
   \"January 25, 4092\" program.")

(defparameter +VALID-COMPILATION-DATE+
  (make-calendar-from-year-month-and-day 4092 1 25)
  "Defines the aefauld date valid for a \"January 25, 4092\" program's
   compilation and execution.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   amplecting in its diorism's perimeter the space, horizontal tab, and
   newline specimens, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline))))))

;;; -------------------------------------------------------

(defun punctuation-character-p (candidate)
  "Determines whether the CANDIDATE represents a punctuation character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "(){};" :test #'char=)))))

;;; -------------------------------------------------------

(defun operator-character-p (candidate)
  "Determines whether the CANDIDATE represents an operator character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "=>" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position into
   the SOURCE immediately succeeding the thus traversed parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-identifier (source start)
  "Proceeding from the START position into the SOURCE, reads an
   alphanumeric identifier and returns two values:
     (1) The thus consumed token in a fresh string form.
     (2) The position into the SOURCE immediately succeeding the matched
         parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (or (position-if-not #'alphanumericp source :start start)
                 (length source))))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (subseq source start end)
        end))))

;;; -------------------------------------------------------

(defun read-operator (source start)
  "Proceeding from the START position into the SOURCE, reads an operator
   and returns two values:
     (1) The thus consumed token in a fresh string form.
     (2) The position into the SOURCE immediately succeeding the matched
         parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (or (position-if-not #'operator-character-p source
                                  :start start)
                 (length source))))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (subseq source start end)
        end))))

;;; -------------------------------------------------------

(defun split-into-words (source)
  "Splits the SOURCE into a list of zero or more tokens, each
   represented by a string, and returns the same."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (setf position (skip-whitespaces source position))
    (loop while (< position (length source)) collect
      (prog1
        (cond
          ((alphanumericp (char source position))
            (multiple-value-bind (next-word new-position)
                (read-identifier source position)
              (declare (type string next-word))
              (declare (type fixnum new-position))
              (setf position new-position)
              next-word))
          ((punctuation-character-p (char source position))
            (prog1
              (string (char source position))
              (incf position)))
          ((operator-character-p (char source position))
            (multiple-value-bind (next-word new-position)
                (read-operator source position)
              (declare (type string next-word))
              (declare (type fixnum new-position))
              (setf position new-position)
              next-word))
          (T
            (error "Invalid character \"~c\" at position ~d."
              (char source position) position)))
        (setf position (skip-whitespaces source position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equal-lists-p (first-list second-list)
  "Determines whether the two lists of strings FIRST-LIST and
   SECOND-LIST are equal, that is, of enumerant conformation and
   absolute equivalency in their elements at the respective positions,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type (list-of string *) first-list))
  (declare (type (list-of string *) second-list))
  (the boolean
    (not (null
      (and
        (= (length first-list)
           (length second-list))
        (every #'string= first-list second-list))))))

;;; -------------------------------------------------------

(defun validate-program (source)
  "Determines whether the SOURC, represents an admissible
   \"January 25, 4092\" program, returning on confirmation no value;
   otherwise signals an error of the type ``Invalid-Program-Error''."
  (declare (type string source))
  (unless (equal-lists-p (split-into-words source)
                         +EXPECTED-PROGRAM-TOKENS+)
    (signal-invalid-program-error source))
  (values))

;;; -------------------------------------------------------

(defun validate-date (date)
  "Determines whether the probed DATE is reckoned as admissible for a
   \"January 25, 4092\" program's execution, returning on confirmation
   no value; otherwise signals an error of the type
   ``Invalid-Date-Error''."
  (declare (type Calendar date))
  (unless (calendars-match-p date +VALID-COMPILATION-DATE+)
    (signal-invalid-date-error date))
  (values))

;;; -------------------------------------------------------

(defun interpret-January-25-4092
    (code
     &key (simulated-date
            (make-current-calendar))
          (action
            #'(lambda ()
                (format T "~&Program executed successfully."))))
  "Interprets the piece of \"January 25, 4092\" source CODE, optionally
   imputing the SIMULATED-DATE in lieu of the current one, executing the
   ACTION on a successful evaluation, and returns no value."
  (declare (type string          code))
  (declare (type Calendar        simulated-date))
  (declare (type (function () *) action))
  (validate-program code)
  (validate-date    simulated-date)
  (funcall action)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Execute while simulating the accepted date of January 25th, 4092.
(interpret-January-25-4092
  "CheckDate(01254092) => True = {
     Execute(main);
   };"
  :simulated-date
    (make-calendar-from-year-month-and-day 4092 1 25))

;;; -------------------------------------------------------

;; Execute while simulating the accepted date of January 25th, 4092,
;; and employing a bespoke action.
(interpret-January-25-4092
  "CheckDate(01254092) => True = {
     Execute(main);
   };"
  :simulated-date
    (make-calendar-from-year-month-and-day 4092 1 25)
  :action
    #'(lambda ()
        (format T "~&Hello, Time Traveler!")))

;;; -------------------------------------------------------

;; Execute on the current date, which most probably will not conflate
;; with the accepted of January 25th, 4092.
(interpret-January-25-4092
  "CheckDate(01254092) => True = {
     Execute(main);
   };")
