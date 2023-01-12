;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Mukha", presented by the Esolang user "ChuckEsoteric08" in
;; the year 2022, and based upon the bidirectional navigation across
;; a one-dimensional piece of code, employing an accumulator as well as
;; a stack for skipping commands or realizing goto navigations.
;; 
;; 
;; Concept
;; =======
;; The Mukha programming language deploys the contingency of
;; boustrophedon navigation in one-dimensional programs, operating
;; concomitantly on an integer-valued accumulator and a stack capable of
;; storing elements of the same type.
;; 
;; == CODE CAN BE TRAVERSED IN TWO DIRECTIONS ==
;; The most kenspeckle attribute of Mukha, operations exist to indagate
;; and affect the instruction pointer's direction, which is bifurcated
;; into the potential for sinistral as well as dextral precession.
;; 
;; Airthed into the sinistrodextral default at a program's inchoation,
;; the program advances until it arrives at the end of the code. If
;; moving from left to right, the rightmost position terminates the
;; program; otherwise, in the right-to-left mode, the left march
;; designates the cessation.
;; 
;; == CONTROL FLOW: SKIP, JUMP, AND TURN ==
;; The capacitation of switching the direction actually participates in
;; a general array of control flow manipulations, the treble's orrels
;; being exhausted by the twain of skipping and jumping, or goto.
;; 
;; Counterdistinguished from the directional turn partaking always in an
;; unconditional activation, the skip and jump companions' enactment may
;; be a dependent of a predicate, such as the stack's content, the
;; accumulator value, or the current direction.
;; 
;; The skipping, constantly in reference to the next instruction,
;; evantuates in all cases based upon a particular requirement's
;; fulfilment, enumerating:
;; 
;;   (a) The accumulator value is negative.
;;   (b) The stack is empty.
;;   (c) The instruction pointer (IP) travels in a certain direction.
;; 
;; Two goto or jump constructs exist, with the first unconditionally
;; returning to the start of the program, whereas the second depends
;; upon the stack's content, triggering solely if a top element exists
;; that can be appropriated in the context of a instruction pointer
;; position, relocating the same to its value.
;; 
;; == PROGRAM MEMORY: ACCUMULATOR AND STACK ==
;; The Mukha program memory describes a composition of two constituents:
;; an accumulator and a stack.
;; 
;; The accumulator comprehends a scalar signed integer whose paravaunt
;; application relates to the enabling of a conditional command skipping
;; if involved in a negative state.
;; 
;; The stack's nature, while being entwined in a partial manner with the
;; accumulator's properties, including its storage of signed integers
;; and a prerequisite to some command skips, assumes a superior role's
;; echolon.
;; 
;; The ability to memorize the current instruction pointer (IP) location
;; and redirect it to the same empowers the language with a goto or jump
;; facility.
;; 
;; 
;; Instructions
;; ============
;; Mukha's instruction set tallies a decimal membership account, with
;; a vast preponderance among its participants contributing either
;; primarily or in an epiphenomenal function to the control flow's
;; helming.
;; 
;; == OVERVIEW ==
;; An incipient intelligence shall be accoutred by the following apercu
;; regarding the language's instructions:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the accumulator by one.
;;   ..................................................................
;;   -       | Decrements the accumulator by one.
;;           | If the accumulator value is negative by now, skips the
;;           | next command.
;;           | Otherwise, advances as usual.
;;   ..................................................................
;;   (       | Pushes the instruction pointer position unto the stack.
;;   ..................................................................
;;   )       | If the stack is not empty, pops its top element and
;;           | relocates the instruction pointer to the position equal
;;           | to the just removed value.
;;           | If the stack is empty, advances as usual.
;;   ..................................................................
;;   ;       | If the stack is not empty, pops its top element.
;;           | If the stack is empty, skips the next command.
;;   ..................................................................
;;   :       | Returns to the start of the program.
;;   ..................................................................
;;   >       | Sets the instruction pointer direction to right.
;;   ..................................................................
;;   <       | Sets the instruction pointer direction to left.
;;   ..................................................................
;;   /       | If the instruction pointer is moving right, skips the
;;           | next instruction.
;;           | Otherwise, advances as usual.
;;   ..................................................................
;;   \       | If the instruction pointer is moving left, skips the
;;           | next instruction.
;;           | Otherwise, advances as usual.
;;   ------------------------------------------------------------------
;; 
;; == INSTRUCTIONS AND THEIR EFFECT ON THE CONTROL FLOW ==
;; The treble navigation modifier categories --- jump, skip, and
;; turn --- shall now be a listing's subject, with the command and
;; contingent condition ligated into apposition:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect | Conditional?  | If conditional: depends upon...
;;   ------------------------------------------------------------------
;;   :       | jump   | unconditional | -
;;   ..................................................................
;;   )       | jump   | conditional   | stack
;;   ..................................................................
;;   -       | skip   | conditional   | accumulator
;;   ..................................................................
;;   ;       | skip   | conditional   | stack
;;   ..................................................................
;;   \       | skip   | conditional   | direction
;;   ..................................................................
;;   /       | skip   | conditional   | direction
;;   ..................................................................
;;   >       | turn   | unconditional | -
;;   ..................................................................
;;   <       | turn   | unconditional | -
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-29
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Mukha"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Mukha instruction
   types."
  '(member
    :increment
    :decrement-and-skip
    :push-ip
    :pop-and-jump
    :pop-and-skip
    :restart
    :move-ip-right
    :move-ip-left
    :skip-if-moving-left
    :skip-if-moving-right))

;;; -------------------------------------------------------

(deftype mukha-program ()
  "The ``mukha-program'' defines a program in the Mukha programming
   language as a vector of commands."
  '(vector command *))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized directions into
   which the instruction pointer (IP) may proceed."
  '(member :left :right))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based last-in-first-out (LIFO)
   storage."
  '(list-of integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of Mukha source CODE a one-dimensional simple
   array of commands."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      do
        (case token
          (#\+ (push :increment            instructions))
          (#\- (push :decrement-and-skip   instructions))
          (#\( (push :push-ip              instructions))
          (#\) (push :pop-and-jump         instructions))
          (#\; (push :pop-and-skip         instructions))
          (#\: (push :restart              instructions))
          (#\> (push :move-ip-right        instructions))
          (#\< (push :move-ip-left         instructions))
          (#\\ (push :skip-if-moving-left  instructions)) 
          (#\/ (push :skip-if-moving-right instructions))
          ((#\Space #\Tab #\Newline) NIL)
          (otherwise
            (error "Invalid character ~s at position ~d."
              token position))))
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Evaluates the INSTRUCTIONS and returns no value."
  (declare (type mukha-program instructions))
  
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (direction           :right)
          (accumulator         0)
          (stack               NIL))
      (declare (type integer           ip))
      (declare (type (or null command) current-instruction))
      (declare (type direction         direction))
      (declare (type integer           accumulator))
      (declare (type stack             stack))
      
      (labels
          ((get-next-ip (&optional (number-of-steps 1))
            "Returns the resulting instruction pointer (IP) location if
             moving the NUMBER-OF-STEPS into the current direction."
            (declare (type integer number-of-steps))
            (the integer
              (case direction
                (:left  (- ip number-of-steps))
                (:right (+ ip number-of-steps))
                (otherwise
                  (error "Invalid direction: ~s." direction)))))
           
           (advance (&optional (number-of-steps 1))
            "Moves the instruction pointer IP the NUMBER-OF-STEPS into
             the current direction, updates the CURRENT-INSTRUCTION, and
             returns no value."
            (declare (type integer number-of-steps))
            (let ((next-ip (get-next-ip number-of-steps)))
              (declare (type integer next-ip))
              (setf ip next-ip)
              (setf current-instruction
                (when (array-in-bounds-p instructions ip)
                  (aref instructions ip))))
            (values))
           
           (skip-next-instruction ()
            "Skips the next instruction via advancing the instruction
             pointer IP by two steps and returns no value."
            (advance 2)
            (values))
           
           (move-to (new-position)
            "Relocates the instruction pointer IP to the NEW-POSITION,
             updates the CURRENT-INSTRUCTION, and returns no value."
            (declare (type integer new-position))
            (setf ip new-position)
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (print-memory ()
            "Prints the accumulator and stack content, the latter in
             decreasing order of its elements, to the standard output
             and returns no value."
            (format T "~&Accumulator: ~d"           accumulator)
            (format T "~&Stack:       [~{~d~^, ~}]" stack)
            (values)))
        
        (loop while current-instruction do
          (case current-instruction
            ((NIL)
              (loop-finish))
            
            (:increment
              (incf accumulator)
              (advance))
            
            (:decrement-and-skip
              (decf accumulator)
              (if (minusp accumulator)
                (skip-next-instruction)
                (advance)))
            
            (:push-ip
              (push ip stack)
              (advance))
            
            (:pop-and-jump
              (if stack
                (move-to (pop stack))
                (advance)))
            
            (:pop-and-skip
              (cond
                (stack
                  (pop stack)
                  (advance))
                (T
                  (skip-next-instruction))))
            
            (:restart
              (move-to 0))
            
            (:move-ip-right
              (setf direction :right)
              (advance))
            
            (:move-ip-left
              (setf direction :left)
              (advance))
            
            (:skip-if-moving-left
              (if (eq direction :left)
                (skip-next-instruction)
                (advance)))
            
            (:skip-if-moving-right
              (if (eq direction :right)
                (skip-next-instruction)
                (advance)))
            
            (otherwise
              (error "Invalid instruction: ~s."
                current-instruction))))
        
        (print-memory))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Mukha (code)
  "Interprets the piece of Mukha source CODE and returns no value."
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the accumulator to 3 and repeatedly decrement it until it is
;; negative, in which case its skips the conditional goto instruction
;; ")" and terminates the program.
;; As a consequence thereof, the stack will yet contain the unused
;; instruction pointer (IP) position 3.
(interpret-Mukha "+++(-)")

;;; -------------------------------------------------------

;; Set the accumulator to 3 and skip the decrementing operation.
(interpret-Mukha ">+++/-")

;;; -------------------------------------------------------

;; Push the instruction pointer positions 0, 2 and 4 unto the stack,
;; increment the accumulator, reverse the direction to left, increment
;; the accumulator again, and let the program terminate on the sinistral
;; march, retaining both the accumulator and stack states.
(interpret-Mukha "(\\(\\(\\+<")

;;; -------------------------------------------------------

;; Push the instruction position 0 unto the stack, move right while
;; incrementing the accumulator to 3 and avoiding reductions using "/"
;; for conditional skipping; then move left while decrementing the same
;; 0, this time preventing the accretion by adminiculum of "\"; and
;; terminate the program at the left bourne.
;; Please note how the stack output attests the recurrence of the
;; instruction pointer during its sinistral traversal by comprehending
;; two copies of the position 0, both accomplished via the "("
;; command. Concomitantly, the position 13, immediately preceding the
;; "<" operation, constitutes a testimony to the dextral march's
;; sojourn.
(interpret-Mukha "(+\\/-+\\/-+\\/-(\\<")
