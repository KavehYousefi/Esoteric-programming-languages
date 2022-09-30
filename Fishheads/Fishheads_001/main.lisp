;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Fishheads", designed by the Esolang user "_hyperdawg" in
;; the year 2021, and based upon the extant language "Deadfish" by
;; Jonathan Todd Skinner and its super-set "Deadfish~" by the same
;; author, intended as an extension of the former by character output
;; and user input, among other features.
;; 
;; Instructions
;; ============
;; The Deadfish cleronomy contributes the approximate moeity of
;; Fishheads's complete functionality, the five remaining members posing
;; as representatives of its augmented nature.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer a cursory nortelry regarding
;; the language's capabilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator by one.
;;   ..................................................................
;;   d       | Decrements the accumulator by one.
;;   ..................................................................
;;   s       | Squares the accumulator.
;;   ..................................................................
;;   o       | Outputs the accumulator as an integer to the standard
;;           | output.
;;   ..................................................................
;;   c       | Outputs the accumulator as the ASCII character whose
;;           | character code corresponds to the value to the standard
;;           | output.
;;   ..................................................................
;;   g       | Queries the user for an integer input and stores the
;;           | same in the accumulator.
;;   ..................................................................
;;   r       | Resets the accumulator to its initial value of zero (0).
;;   ..................................................................
;;   h       | Halts the program.
;;   ..................................................................
;;   l       | Clears the console.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-30
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Fishheads"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Fishheads (&key (initial-code   "")
                                 (console-height 20))
  "Executes the Fishheads shell and interprets the user inputs,
   optionally incepting with the INITIAL-CODE.
   ---
   The CONSOLE-HEIGHT may be configured for the exclusive usance in the
   console clearance instruction \"l\", simulating for the employed
   output channel the window's vertical expansion which shall be shifted
   upwards, and thus out of sight, by aid of newline characters."
  (declare (type string        initial-code))
  (declare (type (integer 0 *) console-height))
  
  (let ((accumulator 0))
    (declare (type integer accumulator))
    
    (flet
        ((print-shell-prompt ()
          "Prints the shell prompt message to the standard output and
           returns no value."
          (format T "~&>> ")
          (values))
         
         (print-accumulator-input-prompt ()
          "Prints the user input prompt message for the \"g\" command to
           the standard output and returns no value."
          (format T "~&Please input an integer: ")
          (values))
         
         (normalize-accumulator ()
          "Normalizes the ACCUMULATOR by reseting it to zero (0) if it
           currently equals -1 or 256 and returns no value."
          (when (or (= accumulator -1) (= accumulator 256))
            (setf accumulator 0))
          (values)))
      
      (loop
        named shell
        for   initial-run-p of-type boolean = T then NIL
        do
          (print-shell-prompt)
          
          (let ((input (if initial-run-p
                         initial-code
                         (read-line))))
            (declare (type string input))
            
            (clear-input)
            
            ;; Interpret the INPUT.
            (loop for command of-type character across input do
              (case command
                ;; Increment the accumulator by one.
                (#\i
                  (incf accumulator)
                  (normalize-accumulator))
                
                ;; Decrement the accumulator by one.
                (#\d
                  (decf accumulator)
                  (normalize-accumulator))
                
                ;; Square the accumulator.
                (#\s
                  (setf accumulator (* accumulator accumulator))
                  (normalize-accumulator))
                
                ;; Output the accumulator verbatim, that is, as a number.
                (#\o
                  (format T "~d" accumulator))
                
                ;; Output the ASCII character corresponding to the
                ;; accumulator.
                (#\c
                  (format T "~c" (code-char accumulator)))
                
                ;; Prompt user input and assign it to the accumulator.
                ;; A non-integer user input is treated in the exact
                ;; manner as an erroneous shell command input, that is, 
                ;; a single newline character is printed without further
                ;; notifications or intrusions.
                (#\g
                  (print-accumulator-input-prompt)
                  (let ((user-input (read-line)))
                    (declare (type string user-input))
                    (clear-input)
                    (handler-case
                      (setf accumulator (parse-integer user-input))
                      (error ()
                        (terpri))))
                  (normalize-accumulator))
                
                ;; Reset the accumulator.
                (#\r
                  (setf accumulator 0))
                
                ;; Halt.
                (#\h
                  (return-from shell))
                
                ;; Clear the console.
                ;; This is simulated by printing a specified amount of
                ;; newline character that shift the preceding lines
                ;; upward and out of the display.
                (#\l
                  (format T "~v%" console-height))
                
                ;; Unrecognized characters incite a newline output.
                (otherwise
                  (terpri))))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hello world".
(interpret-Fishheads :initial-code "riiiiiiiiiisiiiicdddciiiiiiicciiicriiiisiiiiiiiiiiiiiiiicriiiiiiiiiisiiiiiiiiiiiiiiiiiiicddddddddciiicddddddcddddddddc")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-Fishheads :initial-code "go")
