;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Line numbering
;; ==============
;; Yielded as a conjecture by perusal of the infinite loop example,
;; instructions are enumerated starting at one (1); a fact endowed with
;; pertinence in regard to the goto or jump command "^".
;; 
;; Confrontend with a deficiency regarding the behavior of a goto
;; destination outside of the range [1, number_of_instructions], the
;; adjudgment have been concluded that an infringement upon the lower
;; bound, that is, a destination less than or equal to zero, shall
;; default to one (1), whereas a contralateral transgression terminates
;; the program in the exact same manner as a usual progression of the
;; instruction pointer beyond the desinent instruction would incur.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-28
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Xaxa"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized Xaxa command names."
  '(member
    :move-right
    :decrement
    :move-left
    :start-skip
    :end-skip
    :jump
    :output
    :input))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of Xaxa CODE the incorporated instructions
   and returns these in a one-dimensional simple array."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type list instructions))
    (loop for token of-type character across code do
      (case token
        (#\> (push :move-right instructions))
        (#\- (push :decrement  instructions))
        (#\< (push :move-left  instructions))
        (#\? (push :start-skip instructions))
        (#\! (push :end-skip   instructions))
        (#\^ (push :jump       instructions))
        (#\. (push :output     instructions))
        (#\, (push :input      instructions))
        ((#\Space #\Tab #\Newline) NIL)
        (otherwise (error "Invalid character '~c'." token))))
    (the (simple-array command (*))
      (coerce (nreverse instructions) '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Processes and interprets the Xaxa INSTRUCTIONS and returns no value."
  (declare (type (vector command *) instructions))
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (memory      (make-hash-table :test #'eql))
          (pointer     0))
      (declare (type fixnum            ip))
      (declare (type (or null command) instruction))
      (declare (type hash-table        memory))
      (declare (type fixnum            pointer))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the current INSTRUCTION, and returns
             no value."
            (setf instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to (new-position)
            "Moves the instruction pointer IP to the one-based
             NEW-POSITION, updates the current INSTRUCTION, and returns
             no value."
            (declare (type fixnum new-position))
            (setf ip (1- (max 1 new-position)))
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (current-cell ()
            "Returns the value stored in the active cell."
            (the integer (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Sets the active cell's content to the NEW-VALUE and returns
             no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (setf (current-cell) 0)
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            ;; Move the cell pointer one cell to the right and increment
            ;; the active cell's value.
            (:move-right
              (incf pointer)
              (incf (current-cell))
              (advance))
            
            ;; Decrement the active cell's value.
            (:decrement
              (decf (current-cell))
              (advance))
            
            ;; Move the cell pointer one cell to the left.
            (:move-left
              (decf pointer)
              (advance))
            
            ;; If the active cell value equals zero, skip the
            ;; instructions up until and including the next "!".
            (:start-skip
              (cond
                ((zerop (current-cell))
                  (advance)
                  (loop do
                    (case instruction
                      ((NIL)
                        (error "Unterminated '?'. No matching '!' ~
                                found."))
                      (:end-skip
                        (advance)
                        (loop-finish))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Only significant in conjunction with the "?" instruction
            ;; as a demarcation of the section to skip.
            (:end-skip
              (advance))
            
            ;; Jump to the instruction whose one-based index in the
            ;; INSTRUCTIONS vector equals the active cell's value.
            (:jump
              (jump-to (current-cell)))
            
            ;; Output the active cell's value verbatim.
            (:output
              (format T "~d" (current-cell))
              (advance))
            
            ;; Input an integer number and store it in the active cell.
            (:input
              (format T "~&Please input an integer: ")
              (let ((input (parse-integer (read-line))))
                (declare (type integer input))
                (clear-input)
                (setf (current-cell) input))
              (advance))
            
            ;; Invalid instruction encountered? => Error.
            (otherwise
              (error "Invalid instruction '~s' at position ~d."
                instruction ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Xaxa (code)
  "Interprets the piece of Xaxa CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move one cell to the right, increment its value, and print it.
(process-instructions
  (extract-instructions ">."))

;;; -------------------------------------------------------

;; Move one cell to the right, increment its value, and print it.
(interpret-Xaxa ">.")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-Xaxa ">^")

;;; -------------------------------------------------------

;; Infinite cat program.
;; Please note that for each user query a new cell is appended to the
;; memory, thus elicitating a contingency for the environment memory's
;; exhaustion.
(interpret-Xaxa ",.>^")

;;; -------------------------------------------------------

;; Skip the printing of the active cell's value as its content equals
;; zero.
(interpret-Xaxa "?.!")

;;; -------------------------------------------------------

;; Prompt the user for an integer number. If the input does not equal
;; zero, print it, otherwise abstain from any further actions.
(interpret-Xaxa ",?.!")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Xaxa ",?><>-.<>^!")
