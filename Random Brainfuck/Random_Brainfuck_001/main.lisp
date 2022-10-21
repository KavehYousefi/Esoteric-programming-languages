;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Random Brainfuck", invented by the Esolang user "Somelauw"
;; in the year 2015, and based upon Urban Mueller's "brainfuck",
;; extending its heritage by a single operation which sets the current
;; cell to a random byte value from the range [0, 255].
;; 
;; Instructions
;; ============
;; Random Brainfuck's cleronomy amounts to its stock-father's complete
;; octuple facilities, however, subject to an augmentation by a
;; significant ninth member in the form of a random cell value
;; generator.
;; 
;; == OVERVIEW ==
;; The language's nine instructions shall be illustrated in the coming
;; apercu:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   +       | Increments the current cell value by one.
;;           | If the new value exceeds the upper limit of 255, it is
;;           | wrapped around to 0.
;;   ..................................................................
;;   -       | Decrements the current cell value by one.
;;           | If the new value transgresses the lower bound of zero
;;           | (0), it is wrapped around to 255.
;;   ..................................................................
;;   .       | Prints the ASCII character associated with the current
;;           | cell value.
;;   ..................................................................
;;   ,       | Queries the user for an input ASCII character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer forward to the position immediately
;;           | following the matching "]".
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell does not equal zero (0), moves the
;;           | instruction pointer back to the character immediately
;;           | following the matching "[".
;;   ..................................................................
;;   ?       | Sets the current cell to a random byte value from the
;;           | range [0, 255].
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-10-21
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Random_Brainfuck"
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized instruction types."
  '(member
    :move-left
    :move-right
    :increment
    :decrement
    :jump-forward
    :jump-back
    :output
    :input
    :randomize))

;;; -------------------------------------------------------

(deftype jump-table ()
  "Associates the index of each forward jump instruction in an
   instruction vector to the respective back jump operation's position,
   and vice versa, manifesting this mapping in the form of a
   hash table from fixnum keys to values of the same type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight bits,
   occupying the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a mapping of signed
   integer cell indices to the bytes stored at the respective locations,
   realized in the form of a hash table from integer keys to octet
   values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character command) +COMMAND-TABLE+))

;;; -------------------------------------------------------

(defparameter +COMMAND-TABLE+ (make-hash-table :test #'eql)
  "Associates the tokens with the respective Random Brainfuck
   instructions.")

;;; -------------------------------------------------------

(flet ((register-command (token command)
        "Associates the instruction TOKEN with the Random Brainfuck
         COMMAND in the +COMMAND-TABLE+ and returns no value.
         ---
         Any extant entry with the TOKEN as its key will be superseded
         in a tacit manner."
        (declare (type character token))
        (declare (type command   command))
        (setf (gethash token +COMMAND-TABLE+) command)
        (values)))
  (register-command #\< :move-left)
  (register-command #\> :move-right)
  (register-command #\+ :increment)
  (register-command #\- :decrement)
  (register-command #\. :output)
  (register-command #\, :input)
  (register-command #\[ :jump-forward)
  (register-command #\] :jump-back)
  (register-command #\? :randomize)
  (values))

;;; -------------------------------------------------------

(defun command-character-p (character)
  "Checks whether the CHARACTER represents a Random Brainfuck command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (nth-value 1
        (gethash character +COMMAND-TABLE+))))))

;;; -------------------------------------------------------

(defun get-command-for-token (character)
  "Returns the Random Brainfuck command associated with the CHARACTER,
   or signals an error of an unspecified type in the case of any
   correspondence's absence."
  (declare (type character character))
  (the command
    (or (gethash character +COMMAND-TABLE+)
        (error "No command token: ~s." character))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns the instructions from the piece of Random
   Brainfuck CODE."
  (declare (type string code))
  (the (simple-array command (*))
    (coerce
      (loop
        for token of-type character across code
        when (command-character-p token)
          collect (get-command-for-token token))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type random-state +DEFAULT-RANDOM-NUMBER-GENERATOR+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-RANDOM-NUMBER-GENERATOR+
  (make-random-state T)
  "The default random number generator for the randomization
   instruction.")

;;; -------------------------------------------------------

(defun build-jump-table (instructions)
  "Generates and returns a jump table which associates each forward jump
   instruction's index in the INSTRUCTIONS vector to the corresponding
   back jump position, and vice versa."
  (declare (type (vector command *) instructions))
  
  (let ((jump-table             (make-hash-table :test #'eql))
        (forward-jump-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-positions))
    
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0
      do
        (case instruction
          (:jump-forward
            (push position forward-jump-positions))
          
          (:jump-back
            (cond
              (forward-jump-positions
                (let ((forward-jump-position
                        (pop forward-jump-positions)))
                  (declare (type fixnum forward-jump-position))
                  (setf (gethash forward-jump-position jump-table)
                        position)
                  (setf (gethash position jump-table)
                        forward-jump-position)))
              (T
                (error "Unmatched back jump instruction at position ~d."
                  position))))
          
          (otherwise
            NIL)))
    
    (when forward-jump-positions
      (error "Unmatched forward jump instructions at positions ~
              ~{~d~^, ~}."
        forward-jump-positions))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions
    (instructions
     &key (random-number-generator +DEFAULT-RANDOM-NUMBER-GENERATOR+))
  "Processes the Random Brainfuck INSTRUCTIONS, utilizing the
   RANDOM-NUMBER-GENERATOR, and returns no value."
  (declare (type (vector command *) instructions))
  (declare (type random-state       random-number-generator))
  
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (jump-table          (build-jump-table instructions))
          (memory              (make-hash-table :test #'eql))
          (pointer             0))
      (declare (type fixnum            ip))
      (declare (type (or null command) current-instruction))
      (declare (type jump-table        jump-table))
      (declare (type memory            memory))
      (declare (type integer           pointer))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the INSTRUCTIONS vector, if possible, updates the
             CURRENT-INSTRUCTION, and returns no value."
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
           
           (jump-to-opposite-boundary ()
            "Expecting the instruction pointer IP to reside as a forward
             or back jump instruction, relocates it to the opposite
             boundary, updates the CURRENT-INSTRUCTION, and returns no
             value."
            (move-to (gethash ip jump-table))
            (values))
           
           (current-cell ()
            "Return the current cell's value."
            (the octet
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the current cell, contingently
             preceded by its wrapping into the byte range [0, 255], and
             returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0)
                  (if (minusp new-value)
                    (mod new-value 255)
                    (mod new-value 256)))
            (values))
           
           (generate-random-byte ()
            "Returns a random number in the byte range [0, 255]."
            (the octet
              (random 256 random-number-generator))))
        
        (loop while current-instruction do
          (case current-instruction
            ((NIL)
              (loop-finish))
            
            (:move-left
              (decf pointer))
            
            (:move-right
              (incf pointer))
            
            (:increment
              (incf (current-cell)))
            
            (:decrement
              (decf (current-cell)))
            
            (:output
              (write-char (code-char (current-cell))))
            
            (:input
              (format T "~&Please input an ASCII character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input))
            
            (:jump-forward
              (when (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (:jump-back
              (unless (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (:randomize
              (setf (current-cell)
                    (generate-random-byte)))
            
            (otherwise
              (error "Invalid instruction: ~s at position ~d."
                current-instruction ip)))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Random-Brainfuck
    (code
     &key (random-number-generator +DEFAULT-RANDOM-NUMBER-GENERATOR+))
  "Interprets the piece of Random Brainfuck CODE, utilizing the
   RANDOM-NUMBER-GENERATOR, and returns no value."
  (declare (type string       code))
  (declare (type random-state random-number-generator))
  (process-instructions
    (extract-instructions code)
    :random-number-generator random-number-generator)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simulate a die by producing and printing a random number in the
;; integer range [1, 6].
(interpret-Random-Brainfuck
  "
  ; n = random byte
  ?
  ; cell 0| n

  ; Use the divmod golf trick
  ; Now we have a number from 1 to 6 in cell 1
  >++++++<[->-[>+>]>[+[-<+>]>>]<<<<]#
  ; cell 0| 0
  ; cell 1| n minus (n mod 6)
  ; cell 2| n mod 6

  ; Add 48 to cell 1 for converting it to a decimal
  ++++++[->++++++++<]>

  ; Print the result
  .
  ")

;;; -------------------------------------------------------

;; Print an infinite sequence of random ASCII characters.
(interpret-Random-Brainfuck "+[>?.<]")
