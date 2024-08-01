;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deadfih", invented by the Esolang user "Cleverxia" and
;; presented on April 1st, 2024, itself being a derivation of Jonathan
;; Todd Skinner's "Deadfish", ligated in its employment of an aefauld
;; scalar integer register, or "accumulator", and the interactivity of
;; the instructions' reception, concomitant to its eloignment from the
;; faculty of squaring the accumulator's state replicating the same in
;; its ASCII character form in lieu of an ipsissima verba output.
;; 
;; 
;; Concept
;; =======
;; The Deadfih programming language's cleronomy proceeds from Deadfish,
;; partaking of the architectural foundry in the aefauld integer-valued
;; accumulator, deploying instructions composed of singleton symbols,
;; the program operating in a system of perpetuation which introduces
;; a cycle by querying for a line of input, processing its commands,
;; and repeating the actions.
;; 
;; 
;; Instructions
;; ============
;; Deadfih's instruction set intrines facilities for the incrementation,
;; deduction, and printing of the accumulator.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall wone in the explication of the
;; language's operative facilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator by one. If the new value
;;           | equals -1 or 256, the accumulator relapses to the
;;           | incipial state of zero (0).
;;   ..................................................................
;;   d       | Decrements the accumulator by one. If the new value
;;           | equals -1 or 256, the accumulator relapses to the
;;           | incipial state of zero (0).
;;   ..................................................................
;;   o       | Prints the character whose ASCII code corresponds to
;;           | the accumulator value to the standard output.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-07-31
;; 
;; Sources:
;;   [esolang2024Deadfih]
;;   The Esolang contributors, "Deadfih", June 27th, 2024
;;   URL: "https://esolangs.org/wiki/Deadfih"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Deadfih (&optional (initial-code ""))
  "Starts the Deadfih interpreter, in its inchoation processing the
   INITIAL-CODE, and subsequently in a mode of perpetuation querying
   the standard input for a line of code, evaluates the same,
   progresses iterum, and returns no value."
  (declare (type string initial-code))
  (prog ((accumulator 0)
         (input       NIL)
         (ip          0))
    (declare (type integer          accumulator))
    (declare (type (or null string) input))
    (declare (type fixnum           ip))
    
    ;; Contingently process the INITIAL-CODE at the program's
    ;; inchoation.
    (when (plusp (length initial-code))
      (psetf input initial-code
             ip    0)
      (go process-command))
    
    query-for-input
      (format T "~&>> ")
      (finish-output)
      (psetf input (read-line NIL NIL NIL)
             ip    0)
      (clear-input)
      (unless input
        (go terminate-program))
    
    check-if-exhausted
      (if (< ip (length input))
        (go process-command)
        (go query-for-input))
    
    process-command
      (case (char input ip)
        (#\i       (incf accumulator))
        (#\d       (decf accumulator))
        (#\o       (format T "~c" (code-char accumulator)))
        (otherwise (format T "~%")))
      ;; Normalize accumulator.
      (when (or (= accumulator -1) (= accumulator 256))
        (setf accumulator 0))
      (incf ip)
      ;; Check for continuation.
      (go check-if-exhausted)
    
    terminate-program
      NIL)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A".
(interpret-Deadfih
  "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio")
