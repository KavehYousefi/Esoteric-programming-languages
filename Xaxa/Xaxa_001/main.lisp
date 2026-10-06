;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Xaxa", invented by the Esolang user "ChuckEsoteric08" and
;; presented on March 26th, 2022, the hupokeimenon of its conception
;; maintains its commorancy in a derivation from Urban Mueller's
;; "brainfuck", upon whom is adhibited a mickleness of modulations,
;; in particular concerning the control flow mechanisms.
;; 
;; 
;; Concept
;; =======
;; The Xaxa programming language's firmament ostends an establishment
;; upon the brainfuck programming language's foundational principles,
;; including the bilaterally infinite dispansion of cells, the
;; expression of behests by adminiculum of one-symbol identifiers, as
;; well as the zero-based conditionals.
;; 
;; Maugre its entheus' influence, a certain set of deviations do not
;; escape their involvement: The tape wists of a vouchsafement in the
;; single cells' capacity from the unsigned byte realm to the bournless
;; mickleness of arbitrary integer objects, as well as both
;; lateralities' homologation for the numbers' polarity.
;; 
;; A further aspect of peisant novelty wones in the control flow
;; ductions' transformation from the stock-father's zero-based iterance
;; construct towards the twissel of unbridled jumping to zero-based
;; instruction positions, as well as a command skipping facility.
;; 
;; 
;; Instructions
;; ============
;; Xaxa's instruction set entertains an octuple cardinality of operative
;; warklumes, the bailiwicks of their attrectation wisting of such
;; subjects as memory management, basic arithmetics, input and output
;; commerce, as well as a twissel of navigational avails.
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right and
;;           | increments the newly selected cell's value by a
;;           | mountance of one (1).
;;   ..................................................................
;;   -       | Decrements the current cell's value by a mountance of
;;           | one (1).
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   ?       | Prints the current cell's value in its verbatim numeric
;;           | form to the standard output conduit.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a signed or
;;           | unsigned integer number and stores the response in the
;;           | currently selected cell.
;;   ..................................................................
;;   ?       | If the current cell value equals zero (0), skips all
;;           | subsequent commands, until the next "!" token is
;;           | encountered; otherwise, for a non-zero cell content,
;;           | perges as usual.
;;   ..................................................................
;;   !       | Demarcates the desinence of the prevenient "?"
;;           | instruction.
;;   ..................................................................
;;   ^       | Relocates the instruction pointer (IP) to the n-th
;;           | instruction, with n constituting the current cell value.
;;           |---------------------------------------------------------
;;           | The commands are enumerated commence with the inclusive
;;           | subscript zero (0).
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   ip <- memory[cellPointer]
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-10-06
;; 
;; Sources:
;;   [esolang:2026:Xaxa]
;;   The Esolang contributors, "Xaxa", October 4th, 2026
;;   URL: "https://esolangs.org/wiki/Xaxa"
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
  (the (simple-array command (*))
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
          (otherwise
            (error
              "The character \"~c\" does not designate a Xaxa ~
               instruction."
              token))))
      (coerce
        (nreverse instructions)
        '(simple-array command (*))))))



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
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the current INSTRUCTION, and returns
             no value."
            (setf instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to (new-position)
            "Moves the instruction pointer IP to the zero-based
             NEW-POSITION, updates the current INSTRUCTION, and returns
             no value."
            (declare (type fixnum new-position))
            (setf ip (max 0 new-position))
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (current-cell ()
            "Returns the value stored in the active cell."
            (the integer
              (gethash pointer memory 0)))
           
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
            
            ;; Move the cell pointer one cell to the right and
            ;; increment the new active cell's value.
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
              (if (zerop (current-cell))
                (loop
                  initially
                    (advance)
                  do
                    (case instruction
                      ((NIL)
                        (error
                          "An unterminated \"?\" instruction has been ~
                           processed: No matching \"!\" could be ~
                           located."))
                      (:end-skip
                        (advance)
                        (loop-finish))
                      (otherwise
                        (advance))))
                  (advance)))
            
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
              (format T "~&~d" (current-cell))
              (advance))
            
            ;; Input an integer number and store it in the active cell.
            (:input
              (format T "~&Please input an integer: ")
              (force-output)
              (let ((input (parse-integer (read-line))))
                (declare (type integer input))
                (clear-input)
                (setf (current-cell) input))
              (advance))
            
            ;; Invalid instruction encountered?
            ;; => Error.
            (otherwise
              (error "An invalid instruction, \"~s\", has been ~
                      encountered at the position ~d."
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
(interpret-Xaxa ">.")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-Xaxa ">^")

;;; -------------------------------------------------------

;; Infinite cat program.
(interpret-Xaxa ">,.<^")

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
(interpret-Xaxa ",?.^!.")
