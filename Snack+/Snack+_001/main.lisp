;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Snack+", invented by the Esolang user "Cinnamony" and
;; presented on June 23rd, 2023, the foundation of which constitutes
;; the operation on a unit of information designed by the same author,
;; the "SNACBIT", for printing purposes.
;; 
;; 
;; Concept
;; =======
;; The Snack+ programming language establishes a jocular species, the
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
;; == Snack+: SNACBIT STATE SELECTOR AND STATE PRINTER ==
;; The Snack+ programming language found upon the maintenance and
;; transition of the current SNACBIT state, prosecuting its display on
;; the standard output.
;; 
;; In its inchoation pointing to the first state,
;; "Peanut butter cracker", commands exist for advancing the same to the
;; subsequent member and printing it to the standard output.
;; 
;; Upon its desinent element's transcendence, the pointer returns to the
;; first state.
;; 
;; 
;; Instructions
;; ============
;; The extent of Snack+'s quintuple command repertoire presumes a
;; meretricious variety that in veridical terms does not exist,
;; forecause a mere twain of causata, advancement of the state and its
;; display, entalent it with competence.
;; 
;; == OVERVIEW ==
;; The five commands to the programmer's avail shall now be a cursory
;; treatise's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   S       | Prints the current snack.
;;   ..................................................................
;;   n       | Prints the current snack.
;;   ..................................................................
;;   a       | Prints the current snack.
;;   ..................................................................
;;   c       | Prints the current snack.
;;   ..................................................................
;;   k       | Prints the current snack.
;;   ..................................................................
;;   +       | Increments the current stack.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-11
;; 
;; Sources:
;;   [esolang2023Snack+]
;;   The Esolang contributors, "Snack+", June 23rd, 2023
;;   URL: "https://esolangs.org/wiki/Snack%2B"
;;   Notes:
;;     - Specifies the programming language "Snack+".
;;   
;;   [esolang2023SNACBIT]
;;   The Esolang contributors, "SNACBIT", June 23rd, 2023
;;   URL: "https://esolangs.org/wiki/SNACBIT"
;;   Notes:
;;     - Specifies the unit of information "SNACBIT".
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype snacbit ()
  "The ``snacbit'' type enumerates the recognized variants of SNACBIT
   states."
  '(member
    :peanut-butter-cracker
    :cosmic-brownie
    :cheezit
    :cheeto
    :dorito
    :tortilla-chip
    :cheese-cracker
    :potato-chip
    :applesauce
    :peanut-butter-cup
    :pickle))

;;; -------------------------------------------------------

(defgeneric print-snacbit (state)
  (:method ((state (eql :peanut-butter-cracker)))
    (declare (type snacbit state) (ignore state))
    (format T "Peanut butter cracker"))
  
  (:method ((state (eql :cosmic-brownie)))
    (declare (type snacbit state) (ignore state))
    (format T "Cosmic brownie"))
  
  (:method ((state (eql :cheezit)))
    (declare (type snacbit state) (ignore state))
    (format T "Cheezit"))
  
  (:method ((state (eql :cheeto)))
    (declare (type snacbit state) (ignore state))
    (format T "Cheeto"))
  
  (:method ((state (eql :dorito)))
    (declare (type snacbit state) (ignore state))
    (format T "Dorito"))
  
  (:method ((state (eql :tortilla-chip)))
    (declare (type snacbit state) (ignore state))
    (format T "Tortilla chip"))
  
  (:method ((state (eql :cheese-cracker)))
    (declare (type snacbit state) (ignore state))
    (format T "Cheese cracker"))
  
  (:method ((state (eql :potato-chip)))
    (declare (type snacbit state) (ignore state))
    (format T "Potato chip"))
  
  (:method ((state (eql :applesauce)))
    (declare (type snacbit state) (ignore state))
    (format T "Applesauce"))
  
  (:method ((state (eql :peanut-butter-cup)))
    (declare (type snacbit state) (ignore state))
    (format T "Peanut butter cup"))
  
  (:method ((state (eql :pickle)))
    (declare (type snacbit state) (ignore state))
    (format T "Pickle"))
  
  (:documentation
    "Prints the SNACBIT STATE to the standard output and returns
     ``NIL''."))

;;; -------------------------------------------------------

(defgeneric get-next-state (state)
  (:method ((state (eql :peanut-butter-cracker)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :cosmic-brownie))
  
  (:method ((state (eql :cosmic-brownie)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :cheezit))
  
  (:method ((state (eql :cheezit)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :cheeto))
  
  (:method ((state (eql :cheeto)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :dorito))
  
  (:method ((state (eql :dorito)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :tortilla-chip))
  
  (:method ((state (eql :tortilla-chip)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :cheese-cracker))
  
  (:method ((state (eql :cheese-cracker)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :potato-chip))
  
  (:method ((state (eql :potato-chip)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :applesauce))
  
  (:method ((state (eql :applesauce)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :peanut-butter-cup))
  
  (:method ((state (eql :peanut-butter-cup)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :pickle))
  
  (:method ((state (eql :pickle)))
    (declare (type snacbit state) (ignore state))
    (the snacbit :peanut-butter-cracker))
  (:documentation
    "Returns the SNACBIT element following the STATE."))

;;; -------------------------------------------------------

(defun interpret-Snack+ (code)
  "Interprets the piece of Snack+ source CODE and returns no value."
  (declare (type string code))
  (let ((current-snack :peanut-butter-cracker))
    (declare (type snacbit current-snack))
    (loop for token of-type character across code
      if (find token "Snack" :test #'char=) do
        (fresh-line)
        (print-snacbit current-snack)
      else if (char= token #\+) do
        (setf current-snack (get-next-state current-snack))
      else do
        (error "Unrecognized command: \"~c\"." token)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print all states in their natural order, recurring to the incipient
;; one for its display with an endeictic telos tantamount to a docimasy.
(interpret-Snack+ "S+S+S+S+S+S+S+S+S+S+S+S+")
