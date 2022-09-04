;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Consequential", invented by the Esolang user "Xanman12321",
;; and being a derivate of Urban Mueller's "brainfuck", retaining the
;; foundational concepts of minimalism in the syntaxis and an
;; integer-valued tape, while inducing a kenspeckle set of side effects
;; into each of its eight instructions.
;; 
;; Concepts
;; ========
;; The Consequential programming language belongs to the esoteric
;; species, based upon brainfuck, with a verbatim replication of its
;; inspiration's tape-based architecture and command set. As its emblem,
;; the derivation diverges by an introduction of epiphenonemal actions
;; incorporated into each instruction in order to apply an vouchsafe of
;; ludibundness.
;; 
;; 
;; Instructions
;; ============
;; A donat from its stock-father, the tally and basic notions of the
;; operational octuple have remained faithful in agnomination and
;; effect --- natheless, each instruction is encumbered with a variety
;; of epiphenomena, the implication of which constitutes the imposition
;; of the challenge in determining a command sequence sufficiently
;; endowed with the desired effectuality's potence.
;; 
;; == OVERVIEW ==
;; Consequential's heritage tallies in its account an exact quantitative
;; and operational simulacrum of brainfuck, but distorted by kensback
;; adscititious functionality in each member. An apercu shall relate of
;; this:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Decrements the value in the cell immediately preceding
;;           | the current cell, and moves the cell pointer one step to
;;           | right.
;;   ..................................................................
;;   <       | Sets the current cell value to zero (0), and moves the
;;           | cell pointer one step to the left.
;;   ..................................................................
;;   +       | Increments the current cell value, and moves the pointer
;;           | two steps to the left.
;;   ..................................................................
;;   -       | Decrements the current cell value, and increments both
;;           | the cell immediately preceding the current cell and that
;;           | immediately succeeding it.
;;   ..................................................................
;;   .       | Prints to the standard output the ASCII character
;;           | associated with the integer value x yielded by the
;;           | formula
;;           |   x = (currentCellValue mod 96) + 32
;;           | Then moves the pointer dextrally until a current cell
;;           | with the value zero (0) is found.
;;           | The current cell value will not be modified by this
;;           | operation.
;;   ..................................................................
;;   ,       | Prompts the user for an ASCII character and stores the
;;           | corresponding character code into both the cell
;;           | immediately preceding the current cell and that
;;           | immediately succeeding it, but not in the current cell
;;           | itself.
;;   ..................................................................
;;   [       | Stores in the cell preceding the current cell the value
;;           | four (4). If the current cell contains zero (0), jumps
;;           | to the first position immediately following the matching
;;           | jump end bracket "]"; otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | Stores in the cell four steps to the right of the
;;           | current cell the value x, with
;;           |   x = floor(currentCellValue / 2).
;;           | If the current cell does not contain zero, jumps back
;;           | to the position immediately following the matching jump
;;           | start bracket "["; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; The diorisms commorant in Consequential's operations may serve to
;; alienate at a first, or even later, conspectuity. A warkloom of
;; enhanced apprehension, the below tabular explications juxtapose
;; each instruction with a pseudocode description thereof.
;; 
;; The following symbol are enlisted as participants in the informal
;; treatise:
;; 
;;   tape    --- The memory or tape, composed of cells and amenable to
;;               integer subscripts in the form
;;                 tape[index]
;;               The current cell is designated by the position stored
;;               in the cell pointer, or simply POINTER, which please
;;               see below:
;;                 tape[pointer]
;;   pointer --- The cell pointer as the index into the TAPE designating
;;               the current cell, that is:
;;                 tape[pointer]
;;   ip      --- The instruction pointer, which maintains a reference to
;;               the currently processed instruction. It is implicitly
;;               increased after each successful operation handling,
;;               thus moving to the next instruction via
;;                 ip <- ip + 1
;;               This behavior also retains validity after a jumping
;;               with the commands "[" or "]".
;; 
;; The aforementioned command-pseudocode vinculum shall now be
;; delivered:
;; 
;;   ------------------------------------------------------------------
;;   Command | Pseudocode
;;   --------+---------------------------------------------------------
;;   >       | tape[pointer-1] <- tape[pointer-1] - 1
;;           | pointer         <- pointer + 1
;;   ..................................................................
;;   <       | tape[pointer] <- 0
;;           | pointer       <- pointer - 1
;;   ..................................................................
;;   +       | tape[pointer] <- tape[pointer] + 1
;;           | pointer       <- pointer - 2
;;   ..................................................................
;;   -       | tape[pointer]   <- tape[pointer]   - 1
;;           | tape[pointer-1] <- tape[pointer-1] + 1
;;           | tape[pointer+1] <- tape[pointer+1] + 1
;;   ..................................................................
;;   .       | let asciiCode <- (tape[pointer] mod 96) + 32
;;           | print(asciiCode)
;;           | 
;;           | while (tape[pointer] != 0)
;;           |   pointer <- pointer + 1
;;           | end while
;;   ..................................................................
;;   ,       | let inputCharacter <- getUserInputCharacter()
;;           | let inputASCIICode <- getASCIICodeFor(inputCharacter)
;;           | tape[pointer-1] <- inputASCIICode
;;           | tape[pointer+1] <- inputASCIICode
;;   ..................................................................
;;   [       | tape[pointer-1] <- 4
;;           | 
;;           | if (tape[pointer] = 0) then
;;           |   ip <- positionOfMatchingJumpEnd(ip)
;;           | end if
;;   ..................................................................
;;   ]       | tape[pointer+4] <- floor(tape[pointer] / 2)
;;           |
;;           | if (tape[pointer] != 0) then
;;           |   ip <- positionOfMatchingJumpStart(ip)
;;           | end if
;;   ------------------------------------------------------------------
;; 
;; == OUTPUT ==
;; The peculiar translation function deployed for the current cell
;; value's transformation ere its display
;; 
;;   currentCellValue = (currentCellValue mod 96) + 32
;; 
;; if applied to a non-negative datum, effectively wraps the original
;; value inside of the range [32, 127]. As forbisens, the following
;; mappings hold:
;; 
;;   ---------------------------------
;;   Original value | Translated value
;;   ---------------+-----------------
;;   0              | 32
;;   1              | 33
;;   2              | 34
;;   ...            | ...
;;   95             | 127
;;   96             | 32
;;   97             | 33
;;   98             | 34
;;   ...            | ...
;;   ---------------------------------
;; 
;; The thus covered range ensconces, among other entities, the space,
;; several punctuation marks, all decimal digits, majuscular and
;; minuscular letters --- arguably the ASCII section of most utility.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-04
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Consequential"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping from jump start positions
   to their matching end positions in an instruction sequence, and vice
   versa, as a hash table which associates such a march's fixnum
   location in the sequence to the opposite's position."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines the tape-like program memory as a hash
   table mapping integer-valued cell indices to the respective cell
   values of the same set."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized commands in the
   Consequential programming language."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts and returns from the piece of Consequential CODE a
   one-dimensional simple array of commands representing its
   instructions.
   ---
   Non-command tokens in the CODE will be ignored and as such do not
   contribute to the function's output."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (loop for token of-type character across code do
      (case token
        (#\>       (push :move-right   instructions))
        (#\<       (push :move-left    instructions))
        (#\+       (push :increment    instructions))
        (#\-       (push :decrement    instructions))
        (#\.       (push :output       instructions))
        (#\,       (push :input        instructions))
        (#\[       (push :jump-forward instructions))
        (#\]       (push :jump-back    instructions))
        (otherwise NIL)))
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Constructs and returns a jump table which maps each jump start
   position in the INSTRUCTIONS to the matching end position, and vice
   versa."
  (declare (type (vector command *) instructions))
  
  (let ((jump-table  (make-hash-table :test #'eql))
        (loop-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) loop-starts))
    
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0
      do
        (case instruction
          ((NIL)
            (loop-finish))
          (:jump-forward
            (push position loop-starts))
          (:jump-back
            (let ((loop-start (pop loop-starts)))
              (declare (type (or null fixnum) loop-start))
              (cond
                (loop-start
                  (setf (gethash loop-start jump-table) position)
                  (setf (gethash position   jump-table) loop-start))
                (T
                  (error "Unmatched \"]\" at position ~d." position)))))
          (otherwise
            NIL)))
    
    (when loop-starts
      (error "Unterminated \"[\" instances: ~{~d~^, ~}." loop-starts))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the INSTRUCTIONS and returns no value."
  (declare (type (vector command *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (jump-table          (build-jump-table instructions))
          (tape                (make-hash-table :test #'eql))
          (pointer             0))
      (declare (type fixnum            ip))
      (declare (type (or null command) current-instruction))
      (declare (type jump-table        jump-table))
      (declare (type tape              tape))
      (declare (type integer           pointer))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next character, if
             possible, updates the CURRENT-INSTRUCTION, and returns no
             value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (move-to (new-position)
            "Moves the instruction pointer IP to the NEW-POSITION, if
             possible, updates the CURRENT-INSTRUCTION, and returns no
             value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (jump-beyond-target (source-position)
            "Moves the instruction pointer IP to the position
             immediately following the jump start or end location
             matching the SOURCE-POSITION and returns no value."
            (declare (type fixnum source-position))
            (move-to (gethash source-position jump-table))
            (advance)
            (values))
           
           (current-cell ()
            "Returns the value of the current cell."
            (the integer
              (gethash pointer tape 0)))
           
           ((setf current-cell) (new-value)
            "Sets the value of the current cell to the NEW-VALUE and
             returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer tape 0) new-value)
            (values))
           
           (cell-at (offset)
            "Returns the value of the cell located OFFSET units from the
             cell pointer."
            (declare (type integer offset))
            (the integer
              (gethash (+ pointer offset) tape 0)))
           
           ((setf cell-at) (new-value offset)
            "Sets the value of the cell located OFFSET units from the
             cell pointer to the NEW-VALUE and returns no value."
            (declare (type integer new-value))
            (declare (type integer offset))
            (setf (gethash (+ pointer offset) tape 0) new-value)
            (values))
           
           (translate-character-code (original-code)
            "Returns the character associated with the ASCII code
             obtained by transformating the ORIGINAL-CODE using the
             formula
               (originalCode mod 96) + 32."
            (declare (type fixnum original-code))
            (the character
              (code-char (+ (rem original-code 96) 32)))))
        
        (loop while current-instruction do
          (case current-instruction
            ((NIL)
              (loop-finish))
            
            (:move-right
              (decf (cell-at -1))
              (incf pointer)
              (advance))
            
            (:move-left
              (setf (current-cell) 0)
              (decf pointer)
              (advance))
            
            (:increment
              (incf (current-cell))
              (decf pointer 2)
              (advance))
            
            (:decrement
              (decf (current-cell))
              (incf (cell-at -1))
              (incf (cell-at +1))
              (advance))
            
            (:output
              (write-char
                (translate-character-code
                  (current-cell)))
              ;; Move forward to the next zero-valued cell.
              (loop until (zerop (current-cell)) do
                (incf pointer))
              (advance))
            
            (:input
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (cell-at -1) (char-code input))
                (setf (cell-at +1) (char-code input)))
              (advance))
            
            (:jump-forward
              (setf (cell-at -1) 4)
              (if (zerop (current-cell))
                (jump-beyond-target ip)
                (advance)))
            
            (:jump-back
              (setf (cell-at 4)
                    (floor (current-cell) 2))
              (if (not (zerop (current-cell)))
                (jump-beyond-target ip)
                (advance)))
            
            (otherwise
              (error "Unrecognized instruction at position ~d: ~s."
                ip current-instruction)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Consequential (code)
  "Interprets the piece of Consequential CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A" by setting the output cell to the value 33,
;; which, when translated using the formula
;;   (currentCellValue mod 96) + 32
;; yields 65 --- the ASCII code for the character "A".
(interpret-Consequential
  "+>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>>
   +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>> +>>
   .") 

;;; -------------------------------------------------------

;; An infinitely repeating cat program which, however, outputs
;; translated characters.
(interpret-Consequential
  "<,>.<
   [<,>.<]")
