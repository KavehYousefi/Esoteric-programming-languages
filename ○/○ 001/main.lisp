;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "○", or stevened "circle", invented by the "AnotherUser05"
;; and presented on January 27th, 2024, the diorism in its existence
;; appertains to its deployment of disciform symbols only in order to
;; operate on two registers.
;; 
;; 
;; Concept
;; =======
;; The ○ programming language's kenspeckle proprium wones in its
;; deployment of discoidal symbols for its triad of instructions, the
;; telos ensuing from their appropriation realized in the manipulation
;; of two registers, one a paravaunt, the other a paregal instance, for
;; exclusive display purposes.
;; 
;; == ○ ENCOMPASSES TWO REGISTERS ==
;; ○ employs a twissel of registers, one a main component, whose
;; amenability is restricted to incrementations, the second a parhedral
;; temporary salvatory, the dever apportioned to whom constitutes the
;; generation of random integer numbers for the paravaunt accumulator's
;; alteration.
;; 
;; Both registers assume at the program's inchoation the default state
;; of zero (0), their efficacy born from their engagement in a
;; champarty.
;; 
;; 
;; Instructions
;; ============
;; The ○ language enumerates a triad membership of instructions, its
;; capacitation airted at the temporary and main register's modulation,
;; as well as an aefauld output operation.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall encompass a cursory mete of
;; gnarity's communication anenst the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ○       | Generates a random integer number in the closed interval
;;           | [0, 255] and stores the same in the temporary register.
;;   ..................................................................
;;   ◯       | Increments the main register's value by the temporary
;;           | register's value.
;;   ..................................................................
;;   °       | Prints the main register's numeric value to the standard
;;           | output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-23
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", April 29th, 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2024○]
;;   The Esolang contributors, "○", March 15th, 2024
;;   URL: "https://esolangs.org/wiki/%E2%97%8B"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-○ (code)
  "Interprets the piece of ○ source CODE and returns no value."
  (declare (type string code))
  (setf *random-state* (make-random-state T))
  (let ((main-register      0)
        (temporary-register 0))
    (declare (type (integer 0 *)   main-register))
    (declare (type (integer 0 255) temporary-register))
    (loop for token of-type character across code do
      (case token
        (#\○       (setf temporary-register (random 256)))
        (#\◯       (incf main-register temporary-register))
        (#\°       (format T "~d " main-register))
        (otherwise NIL))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print a random integer value in the closed interval [0, 255].
(interpret-○ "○◯°")

;;; -------------------------------------------------------

;; Print several accumulator values.
(interpret-○ "○◯○◯○◯○◯○◯°○◯○◯○◯○◯○◯○◯○◯○◯○◯○◯○◯°○◯○◯○◯○◯○◯○◯○◯○◯°°○◯○◯○◯○◯°○◯○◯○◯○◯○◯○◯○◯○◯°")

;;; -------------------------------------------------------

;; Generate a 16-digit key.
(interpret-○ "○◯°○◯○◯°○◯°○◯○◯°○◯°○◯○◯°○◯°○◯○◯°○◯°○◯○◯°○◯°○◯○◯°○◯°○◯○◯°○◯°○◯○◯°")
