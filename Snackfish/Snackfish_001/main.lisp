;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Snackfish, invented by the Esolang user "Cinnamony" and
;; presented on June 23rd, 2023, the foundation of which bifurcates into
;; the unit of information designed by the same author, the "SNACBIT",
;; and Jonathan Todd Skinner's language "Deadfish".
;; 
;; 
;; Concept
;; =======
;; The Snackfish programming language establishes a jocular species, the
;; telos of which is constrained to a mere printing of states desumed
;; from the set of SNACBIT units.
;; 
;; == SNACBIT: A DELECTABLE UNIT OF INFORMATION ==
;; The SNACBIT, invented by the same author as this language, represents
;; an information unit exhausted by eleven members which partake of an
;; ordinal ordonnance:
;; 
;;   ------------------------------
;;   Number | SNACBIT state
;;   -------+----------------------
;;   1      | Peanut butter cracker
;;   ..............................
;;   2      | Cosmic brownie
;;   ..............................
;;   3      | Cheezit
;;   ..............................
;;   4      | Cheeto
;;   ..............................
;;   5      | Dorito
;;   ..............................
;;   6      | Tortilla chip
;;   ..............................
;;   7      | Cheese cracker
;;   ..............................
;;   8      | Potato chip
;;   ..............................
;;   9      | Applesauce
;;   ..............................
;;   10     | Peanut butter cup
;;   ..............................
;;   11     | Pickle
;;   ------------------------------
;; 
;; == SNACKFISH: SNACBIT STATE SELECTOR AND STATE PRINTER ==
;; Snackfish operates on a register or accumulator that maintains the
;; currently selected entry among the SNACBIT states, at the program's
;; inchoation defaulting to the first member "Peanut butter cracker",
;; and amenable to basic arithmetics pursuing the state's modification.
;; The only competence beside such alterations constitutes the active
;; entry's display on the standard output.
;; 
;; The entries are traversed with a wrapping behavior, that is, an
;; attempt to pass beyond the desinent member automatically relocates
;; the cursor to the incipient one; the athwart case, a seeking of the
;; first state's predecessor terminates in the last entry. Generally
;; speaking, any state number is normalized through overflowing or
;; underflowing into the range [1, 11].
;; 
;; Snackfish adheres to Deadfish's interactive nature: The interpreter
;; repeatedly queries the user for a line of code, processing the
;; comprehended instructions. Tokens not allied with any meaningful
;; causatum simply respond with a newline output.
;; 
;; 
;; Instructions
;; ============
;; The extent of Snackfish's quadruple command repertoire embraces a
;; treble of arithmetic operations and a single output conduit.
;; 
;; == OVERVIEW ==
;; The four commands to the programmer's avail shall now be a cursory
;; treatise's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the current snack state number by one.
;;           | If the new state number trespasses the desinent state,
;;           | the first entry, "Peanut butter cracker" is assumed.
;;   ..................................................................
;;   d       | Decrements the current snack state number by one.
;;           | If the new state number trespasses the first state, the
;;           | last entry, "Pickle", is assumed.
;;   ..................................................................
;;   s       | Squares the current snack state number.
;;           | If the new state number trespasses the valid range
;;           | [1, 11], it is wrapped around into this interval.
;;   ..................................................................
;;   o       | Prints the current snack to the standard output.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-11
;; 
;; Sources:
;;   [esolang2023Snackfish]
;;   The Esolang contributors, "Snackfish", August 7th, 2023
;;   URL: "https://esolangs.org/wiki/Snackfish"
;;   Notes:
;;     - Specification of the Snackfish programming language.
;;   
;;   [esolang2023SnackfishConst]
;;   The Esolang contributors, "Snackfish/Constants", June 23rd, 2023
;;   URL: "https://esolangs.org/wiki/Snackfish/Constants"
;;   Notes:
;;     - Demonstrates the Snackfish language's response to several
;;       inputs.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype state-number ()
  "The ``state-number'' type defines the range of valid state numbers,
   positions that enumerate the SNACBIT states in their natural order,
   being tantamount to the closed intervall [1, 11]."
  '(integer 1 11))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of SNACBIT states.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array simple-string (11)) +SNACBIT-STATES+))

;;; -------------------------------------------------------

(defparameter +SNACBIT-STATES+
  (make-array 11
    :element-type 'simple-string
    :initial-contents
      '("Peanut butter cracker"
        "Cosmic brownie"
        "Cheezit"
        "Cheeto"
        "Dorito"
        "Tortilla chip"
        "Cheese cracker"
        "Potato chip"
        "Applesauce"
        "Peanut butter cup"
        "Pickle")
    :adjustable   NIL
    :fill-pointer NIL)
  "Maintains the SNACBIT states in a one-dimensional simple array of its
   string representations.
   ---
   The vector qua a zero-based collection requires the one-based SNACBIT
   state numbers to be reduced by one (1) in order to map to the name at
   the respective vector index. The state with the number one (1),
   norned \"Peanut butter cracker\", as a corollary, resides at the
   +SNACBIT-STATES+ array's first position zero (0).")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize-state-number (state-number)
  "Wraps the STATE-NUMBER into the valid SNACBIT number range [1, 11]
   and returns the thus produced value."
  (declare (type integer state-number))
  (the state-number
    (+ 1
       (mod (- state-number 1)
            11))))

;;; -------------------------------------------------------

(defun interpret-Snackfish (&optional (initial-code ""
                                       initial-code-supplied-p))
  "Commences the Snackfish interpreter, contingently processing the
   INITIAL-CODE, if supplied, and repeatedly quering for commands until
   a completely empty line is supplied, ultimately returning no value."
  (declare (type string initial-code))
  (declare (type T      initial-code-supplied-p))
  (let ((state-number 1))
    (declare (type state-number state-number))
    (flet ((process-commands (commands)
            "Processes the COMMANDS and returns no value."
            (declare (type string commands))
            (loop for token of-type character across commands do
              (case token
                (#\i
                  (setf state-number
                    (normalize-state-number
                      (1+ state-number))))
                (#\d
                  (setf state-number
                    (normalize-state-number
                      (1- state-number))))
                (#\s
                  (setf state-number
                    (normalize-state-number
                      (* state-number state-number))))
                (#\o
                  (format T "~&~a"
                    (aref +SNACBIT-STATES+
                      (1- state-number))))
                (otherwise
                  (terpri))))
            (values)))
    
    (when initial-code-supplied-p
      (process-commands initial-code))
    
    (loop
      for input
        of-type string
        =       (prog2
                  (format T "~&>> ")
                  (read-line *standard-input* NIL "")
                  (clear-input *standard-input*))
      until (zerop (length input))
      do    (clear-input)
      do    (process-commands input))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Traverse and print the SNACBIT states in reverse direction,
;; commencing from "Pickle" and finishing at the same entry.
(interpret-Snackfish "dodododododododododododo")

;;; -------------------------------------------------------

;; Print "Cheeto".
(interpret-Snackfish "is")
