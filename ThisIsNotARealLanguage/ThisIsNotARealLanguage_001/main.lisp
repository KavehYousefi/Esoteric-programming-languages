;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ThisIsNotARealLanguage", a joke language introduced by the
;; Esolang user "Maxsteele2" in the year 2009, based around the
;; philosophical inquisition about the contingence of a programming
;; language to be concomitantly existent and not.
;; 
;; Concept
;; =======
;; The only program produced by the specification resolves to
;; 
;;   NOT A CODE
;;   DON'T PRINT "Hello, World!"
;; 
;; While simultaneously it is stated that this does NOT replicate a
;; piece of code in the language --- succeeded by the claim about the
;; ThisIsNotARealLanguage being not a veridical language.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-19
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/ThisIsNotARealLanguage"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype success-handler ()
  "The ``success-handler'' type defines a function intended for its
   invocation in the case of the successful interpretation of a
   ThisIsNotARealLanguage program, expecting the assayed and accepted
   source code, while ignoring the result."
  '(function (string) *))

;;; -------------------------------------------------------

(deftype failure-handler ()
  "The ``success-handler'' type defines a function intended for its
   invocation in the case of a failed interpretation of a
   ThisIsNotARealLanguage program, expecting the assayed, erroneous,
   source code, while ignoring the result."
  '(function (string) *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 38)        +VALID-CODE+))
(declaim (type (simple-array string (*)) +SUCCESS-MESSAGES+))
(declaim (type success-handler           +DEFAULT-SUCCESS-HANDLER+))
(declaim (type success-handler           +DEFAULT-FAILURE-HANDLER+))

;;; -------------------------------------------------------

(defparameter +VALID-CODE+
  "NOT A CODE
DON'T PRINT \"Hello, World!\""
  "The exclusive valid piece of ThisIsNotARealLanguage source code.")

;;; -------------------------------------------------------

(defparameter +SUCCESS-MESSAGES+
  (the (simple-array string (*))
    (coerce
      '("Hello, World!"
        "AndThisIsNotARealOutput"
        "And this is not a real interpreter..."
        "But this is a real pipe."
        "Magritte disagrees."
        "Are you sure?"
        "However...")
      '(simple-array string (*))))
  "Maintains a list of possible messages to print upon a successful
   interpretation of a ThisIsNotARealLanguage program.")

;;; -------------------------------------------------------

(setf *random-state* (make-random-state T))

;;; -------------------------------------------------------

(defun select-success-message ()
  "Randomly chooses and returns one of the messages maintained by the
   +SUCCESS-MESSAGES+."
  (the string
    (aref +SUCCESS-MESSAGES+
      (random (length +SUCCESS-MESSAGES+)))))

;;; -------------------------------------------------------

(defparameter +DEFAULT-SUCCESS-HANDLER+
  #'(lambda (code)
      (declare (type string code))
      (declare (ignore      code))
      (format T "~&~a" (select-success-message))
      (values))
  "The default handler to invoke upon the successful interpretation of a
   ThisIsNotARealLanguage program, printing to the standard output a
   fresh line with one of the +SUCCESS-MESSAGES+ chosen in an aleatory
   manner.")

;;; -------------------------------------------------------

(defparameter +DEFAULT-FAILURE-HANDLER+
  #'(lambda (code)
      (declare (type string code))
      (error "Your code ~s might be right or wrong --- who knows?"
        code))
  "The default handler to invoke upon the failed interpretation of a
   ThisIsNotARealLanguage program, signaling an error of an unspecified
   type informing about the failure in conjunction with the assayed
   piece of code.")

;;; -------------------------------------------------------

(defun interpret-ThisIsNotARealLanguage
    (code
     &key (success-handler +DEFAULT-SUCCESS-HANDLER+)
          (failure-handler +DEFAULT-FAILURE-HANDLER+))
  "Interprets the piece of ThisIsNotARealLanguage source CODE, on
   acceptance invoking the SUCCESS-HANDLER with the CODE, otherwise
   calling the FAILURE-HANDLER with the same input, in any case
   returning no value."
  (declare (type string          code))
  (declare (type success-handler success-handler))
  (declare (type failure-handler failure-handler))
  (if (string= code +VALID-CODE+)
    (funcall success-handler code)
    (funcall failure-handler code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default success handler.
(interpret-ThisIsNotARealLanguage
  "NOT A CODE
DON'T PRINT \"Hello, World!\"")

;;; -------------------------------------------------------

;; Default failure handler.
(interpret-ThisIsNotARealLanguage
  "BUT THIS IS SOME CODE.")

;;; -------------------------------------------------------

;; Custom success handler.
(interpret-ThisIsNotARealLanguage
  "NOT A CODE
DON'T PRINT \"Hello, World!\""
  :success-handler
    #'(lambda (code)
        (declare (type string code))
        (format T "~&Yet the input ~s succeeds." code)
        (values)))

;;; -------------------------------------------------------

;; Custom failure handler.
(interpret-ThisIsNotARealLanguage
  "BUT THIS IS SOME CODE."
  :failure-handler
    #'(lambda (code)
        (declare (type string code))
        (format T "~&The input ~s was even less of a program." code)
        (values)))
