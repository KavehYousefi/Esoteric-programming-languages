;; 
;; Common patterns
;; ---------------
;; 
;; == TRANSFERENCE OF REGISTER VALUES ==
;; Copies value of register X to register Y, while simultaneously
;; decrements the source register X to zero:
;;   
;;   X-
;;   {-Y...+X}
;; 
;; == ENSURING THE CONDITION STATE ==
;; If optating to ascertain a particular state of the condition register
;; "c", a custom logic gate can be implemented founded upon negation and
;; conditional execution:
;; 
;; Ensure true (tautology):
;;   (!)!
;; 
;; Ensure false (contradiction):
;;   !(!)
;; 
;; --------------------------------------------------------------------
;; 
;; Date:   2022-01-15
;; Author: Kaveh Yousefi
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Minscode"
;;   -> "https://sites.millersville.edu/bikenaga/math-proof/truth-tables/truth-tables.html"
;;       o Truth tables for tautology and contradiction.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype register-name ()
  "The ``register-name'' type enumerates the valid register names."
  '(member :A :B :C))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each of which conforms to the ELEMENT-TYPE, the same
   defaults to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Minscode (code)
  "Interprets the piece of Miscode CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      
      (let ((register-A        0)
            (register-B        0)
            (register-C        0)
            (condition-c       NIL)
            (current-register :A)
            
            ;; List of fixnums, each the position of a loop body start.
            (loops            NIL)
            ;; LIst of fixnums, each the position of an "if".
            (conditionals     NIL)
            )
        (declare (type integer          register-A))
        (declare (type integer          register-B))
        (declare (type integer          register-C))
        (declare (type boolean          condition-c))
        (declare (type register-name    current-register))
        (declare (type (list-of fixnum) loops))
        (declare (type (list-of fixnum) conditionals))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character, if
               possible, updates the current CHARACTER, and returs no
               value."
              (setf character
                (when (< position (1- (length code)))
                  (char code (incf position))))
              (values))
             
             (move-to (new-position)
              "Translates the POSITION cursor to the NEW-POSITION,
               updates the current CHARACTER, and returns no value."
              (declare (type fixnum new-position))
              (setf position  new-position)
              (setf character (char code position))
              (values))
             
             (find-terminator (instigator terminator)
              "Starting at the current POSITION, seeks the TERMINATOR
               corresponding the INSTIGATOR on the matching level, and
               returns the TERMINATOR's location in the CODE>
               ---
               Upon failure to locate the suiting TERMINATOR, an error
               is signaled."
              (declare (type character instigator))
              (declare (type character terminator))
              (let ((return-position position))
                (declare (type fixnum return-position))
                (the fixnum
                  (loop with level of-type integer = 0 do
                    (cond
                      ((null character)
                        (error "Unterminated section started by '~c', ~
                                but lacking a closing '~c'."
                          instigator terminator))
                      
                      ((char= character instigator)
                        (incf level)
                        (advance))
                      
                      ((char= character terminator)
                        (cond
                          ((zerop level)
                            (return
                              (prog1 position
                                (move-to return-position))))
                          (T
                            (decf level)
                            (advance))))
                      
                      (T
                        (advance)))))))
             
             
             (switch-to-register (register-name)
              "Sets the register designated by the REGISTER-NAME as the
               active one and returns no value."
              (declare (type register-name register-name))
              (setf current-register register-name)
              (values))
             
             (current-register-value ()
              "Returns the value of the active register."
              (the integer
                (case current-register
                  (:A register-A)
                  (:B register-B)
                  (:C register-C)
                  (otherwise
                    (error "Invalid current register: ~s."
                      current-register)))))
             
             ((setf current-register-value) (new-value)
              "Sets the value of the active register to the NEW-VALUE
               and returns no value."
              (declare (type integer new-value))
              (case current-register
                (:A (setf register-A new-value))
                (:B (setf register-B new-value))
                (:C (setf register-C new-value))
                (otherwise
                  (error "Invalid current register: ~s."
                    current-register)))
              (values))
             
            )
          
          (loop do
            (case character
              ;; End of program?
              ;; => Terminate.
              ((NIL)
                (loop-finish))
              
              ;; Increment current register.
              (#\+
                (incf (current-register-value))
                (advance))
              
              ;; If current register > 0 then
              ;;   Decrement current register
              ;;   Set condition to false
              ;; Else
              ;;   Set condition to true
              ;; End if
              (#\-
                (cond
                  ((plusp (current-register-value))
                    (decf (current-register-value))
                    (setf condition-c NIL))
                  (T
                    (setf condition-c T)))
                (advance))
              
              ;; Switch to register A.
              (#\A
                (setf current-register :A)
                (advance))
              
              ;; Switch to register B.
              (#\B
                (setf current-register :B)
                (advance))
              
              ;; Switch to register C.
              (#\C
                (setf current-register :C)
                (advance))
              
              ;; Negate condition register c.
              (#\!
                (setf condition-c (not condition-c))
                (advance))
              
              ;; Execute "(...)" if condition register c is true.
              (#\(
                (cond
                  ;; Condition register "c" is true?
                  ;; => Execute parenthesized commands.
                  (condition-c
                    (push position conditionals)
                    (advance))
                  ;; Condition register "c" is false?
                  ;; => Skip past closing ")".
                  (T
                    (advance)
                    (move-to (find-terminator #\( #\)))
                    (advance))))
              
              ;; Designate the current conditional as terminated.
              (#\)
                (cond
                  ;; Inside of an "if"?
                  ;; => Confirm sane termination of current conditional.
                  (conditionals
                    (pop conditionals)
                    (advance))
                  ;; Not inside an "if"?
                  ;; => Invalid encounter.
                  (T
                    (error "Unmatched closing ')' at position ~d."
                      position))))
              
              ;; Loop while condition register c is false.
              (#\{
                (cond
                  ;; Condition register "c" is true?
                  ;; => Skip past closing "}".
                  (condition-c
                    (advance)
                    (move-to (find-terminator #\{ #\}))
                    (advance))
                  ;; Condition register "c" is false?
                  ;; => Execute until true.
                  (T
                    (advance)
                    (let ((body-start position))
                      (declare (type fixnum body-start))
                      (push body-start loops)))))
              
              ;; Restart or terminate the current loop.
              (#\}
                (cond
                  ;; In loop, and loop shall be terminated?
                  ((and loops condition-c)
                    (pop loops)
                    (advance))
                  ;; In loop, and loop shall repeat?
                  ((and loops (not condition-c))
                    (move-to (first loops)))
                  ;; Outside of loop?
                  ;; => Should not occur.
                  (T
                    (error "Unmatched closing '}' at position ~d."
                      position))))
              
              ;; Output current register value.
              (#\,
                
                (write #\Space :stream T :escape NIL)
                
                (write (current-register-value) :stream T :escape NIL)
                (advance))
              
              ;; Prompt user for number to store into current register.
              (#\.
                (format T "~&Please input a number: ")
                (let ((input (read-line)))
                  (declare (type string input))
                  (clear-input)
                  (setf (current-register-value)
                        (parse-integer input)))
                (advance))
              
              (otherwise
                (advance))))))))
    (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Count from three down to zero, printing the values:
;;   3 2 1 0
(interpret-Minscode
  "A+++
   {,-}")

;;; -------------------------------------------------------

;; Input a number into register "A", copy its value into register "B"
;; while printing "A"'s decreasing value, and output the thus computed
;; value of register "B".
(interpret-Minscode
  "A.
   ,
   A-
   {,-B+A}
   B,
  ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Minscode
  ".
   -
   {+,-}
   ,
  ")

;;; -------------------------------------------------------

;; Print the Fibonacci numbers (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...).
;; 
;; Underlying formula:
;;   F(0) = 0
;;   F(1) = 1
;;   F(n) = F(n-1) + F(n-2).
;; 
;; Concept:
;;   - The register "A" initially stores F(0) and later F(n-2).
;;   - The register "B" initially stores F(1) and later F(n-1).
;;   - During each iteration:
;;     o The register "C" stores F(n) = F(n-1) + F(n-2) = "A" + "B".
;;     o While doing so, the register "A" is set to zero.
;;     o The register "B" copies its value into "C" and "A", thus
;;       setting "A" to "B": F(n-2) = F(n-1), while "B" decreases to
;;       zero.
;;     o After being printed, "C" copies its value to "B", thus setting
;;       F(n-1) = F(n) for the next iteration, while simultaneously
;;       resetting "C" to zero.
;;     o The condition register "c" must be negated to permit a next
;;       repetition of the loop.
;; 
;; Pseudocode representation of the process:
;;   Set A to 0                { F(0) = 0. }
;;   Output A                  { Print F(0) = 0. }
;;   Set B to 1                { F(1) = 1. }
;;   Output B (= 1)            { Print F(1) = 1. }
;;   
;;   Repeat infinitely
;;     Transfer A to C         { C = A. }
;;     Transfer B to C and A   { C = A + B. }
;;     Output C                { Prints A + B. }
;;     Reset C                 { C = 0 for computing (C = A + B) in next iteration. }
;;     Negate condition c      { Set condition c to false to start next iteration. }
;;   End repeat
;; 
;; Note that the last Minscode line used for negating the condition
;; register "c"
;;   !
;; can be substituted by
;;   A+-
(interpret-Minscode
  "
  A
  ,
  
  B+
  ,
  
  {
    A-
    {-C+A}
    
    B-
    {-C+A+B}
    
    C,
    -
    {-B+C}
    
    !
  }
  ")

;;; -------------------------------------------------------

;; Counts the register "A" from three down to zero, but only prints
;; its value if the condition register "c" contains true, which occurs
;; at the end of the encompassing loop's last iteration.
(interpret-Minscode
  "A+++
   {
     -
     (,)
   }")

;;; -------------------------------------------------------

;; Example for a tautology, that is, an ascertainment that the condition
;; register "c" is true. This program prints the value of the register
;; "A", albeit, at the start of the program, the condition register "c"
;; was false.
(interpret-Minscode
  "A+++
   (!)!
   (,)")

;;; -------------------------------------------------------

;; Example for a tautology, that is, an ascertainment that the condition
;; register "c" is true. This program prints the value of the register
;; "A", albeit, retaining the state of the condition register "c" being
;; true.
(interpret-Minscode
  "A+++
   !
   (!)!
   (,)")

;;; -------------------------------------------------------

;; Example for a contradiction, that is, an ascertainment that the
;; condition register "c" is false. This program does not print the
;; value of the register "A", albeit, it is temporarily set to true, as
;; by passing through the "!(!)" its value is ensured to be rendered
;; false.
(interpret-Minscode
  "A+++
   !
   !(!)
   (,)")

;;; -------------------------------------------------------

;; Example for a contradiction, that is, an ascertainment that the
;; condition register "c" is false. This program does not print the
;; value of the register "A", passing its false state through the "!(!)"
;; gate.
(interpret-Minscode
  "A+++
   !(!)
   (,)")
