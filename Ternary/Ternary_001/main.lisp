;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Ternary", created by the Esolang user "zerosum0x0" in the
;; year 2015, and intended as a syntactical reformulation of Urban
;; Mueller's "brainfuck" programming language, with the eight
;; instruction tokens encoded as jumelles of digits in the base-3, or
;; ternary, number system.
;; 
;; 
;; Architecture
;; ============
;; Ternary subscribes to the native tenets of its brainfuck ancestor,
;; maintaining a linear sequence of unsigned-byte-valued cells,
;; admitting the integer range of [0, 255], however, not necessitated to
;; accommodate the fixed 30,000 in tally, nor constrained to
;; non-negative indices.
;; 
;; 
;; Instructions
;; ============
;; Ternary, as brainfuck's legacy, embraces its octet instructions,
;; however, ostended in a new guise.
;; 
;; == OVERVIEW ==
;; The following table shall exercises a conspectus' adhibition anenst
;; the eight instructions commorant in the language:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   00      | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   01      | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   02      | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "12" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   10      | Decrements the current cell value by one. If the new
;;           | value violates the lower unsigned byte boundary of zero
;;           | (0), wraps the value around to the maximum of 255.
;;   ..................................................................
;;   11      | Increments the current cell value by one. If the new
;;           | value violates the upper unsigned byte boundary of 255,
;;           | wraps the value around to the minimum of zero (0).
;;   ..................................................................
;;   12      | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) backward to the position
;;           | immediately succeeding the matching "02" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   20      | Prints to the standard output the character
;;           | corresponding to the current cell value when construed
;;           | as an ASCII code.
;;   ..................................................................
;;   21      | Queries the user for an input character and stores its
;;           | ASCII code in the current cell.
;;   ------------------------------------------------------------------
;; 
;; == TERNARY AND BRAINFUCK EQUIVALENCY ==
;; A consectary begotten from its status as a trivial brainfuck
;; substitution, each Ternary command token partakes of an affiliation
;; with exactly one brainfuck operation, and vice versa. The tabular
;; exposition below shall equiparate the two specimens:
;; 
;;   ----------------------------------
;;   Ternary command | brainfuck analog
;;   ----------------+-----------------
;;   00              | <
;;   ..................................
;;   01              | >
;;   ..................................
;;   02              | [
;;   ..................................
;;   10              | -
;;   ..................................
;;   11              | +
;;   ..................................
;;   12              | ]
;;   ..................................
;;   20              | .
;;   ..................................
;;   21              | ,
;;   ----------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-05-13
;; 
;; Sources:
;;   [esolang2022ternary]
;;   The Esolang contributors, "Ternary", 2022
;;   URL: "https://esolangs.org/wiki/Ternary"
;;   
;;   [esolang2023trivialbfsub]
;;   The Esolang contributors, "Trivial brainfuck substitution", 2023
;;   URL: "https://esolangs.org/wiki/Trivial_brainfuck_substitution"
;;   Notes:
;;     - Describes the family of trivial brainfuck substitutions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command-token ()
  "The ``command-token'' type defines a two-digit token representing a
   Ternary command identifier."
  '(string 2))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Ternary commands."
  '(member
    :move-left
    :move-right
    :jump-forward
    :decrement
    :increment
    :jump-back
    :output
    :input))

;;; -------------------------------------------------------

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
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype ternary-program ()
  "The ``ternary-program'' type defines an executable representation of
   a program in the Ternary programming language as a vector of zero or
   more ``command''s."
  '(vector command *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping from the positions of the
   forward jump instructions in a Ternary program to the matching back
   jump locations, and vice versa, realized as a hash table of fixnum
   keys to values of the same type, both representative of indices."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   adjacent bits, thus constituting a commorant of the integer range
   [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array command (8)) +COMMAND-TABLE+))

;;; -------------------------------------------------------

(defparameter +COMMAND-TABLE+
  (make-array 8
    :element-type 'command
    :initial-contents
      '(:move-left
        :move-right
        :jump-forward
        :decrement
        :increment
        :jump-back
        :output
        :input)
    :adjustable   NIL
    :fill-pointer NIL)
  "Associates the decimal values corresponding with the Ternary command
   identifiers, which concomitantly represent base-3 numbers in the
   range [0, 7], with the Ternary ``command'' objects.
   ---
   The following associations betwixt the +COMMAND-TABLE+ indices, the
   corresponding base-3 analogs, being equivalent to the Ternary command
   identifiers, and their effects hold:
   
     ------------------------------------------------------------------
     Decimal index | Ternary command | Effect
     --------------+-----------------+---------------------------------
     0             | 00              | Move cell pointer left.
     ..................................................................
     1             | 01              | Move cell pointer right.
     ..................................................................
     2             | 02              | Jump forward.
     ..................................................................
     3             | 10              | Decrement current cell.
     ..................................................................
     4             | 11              | Increment current cell.
     ..................................................................
     5             | 12              | Jump back.
     ..................................................................
     6             | 20              | Output current cell.
     ..................................................................
     7             | 21              | Input into current cell.
     ------------------------------------------------------------------
   ")

;;; -------------------------------------------------------

(defun command-for (token)
  "Returns the command corresponding with the TOKEN, or signals an error
   of an unspecified type upon its disrespondency."
  (declare (type command-token token))
  (the command
    (aref +COMMAND-TABLE+
      (parse-integer token :radix 3))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent admissive
   in a Ternary command identifier, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (digit-char-p candidate 3)))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of Ternary source CODE a
   one-dimensional simple array of its instructions."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (if (plusp (length code))
      (let ((position 0))
        (declare (type fixnum position))
        (labels
            ((command-character-encountered-p ()
              "Determines whether the character at the current POSITION
               into the CODE represents a Ternary command constituent,
               returning on confirmation a ``boolean'' value of ``T'',
               otherwise ``NIL''."
              (the boolean
                (command-character-p
                  (char code position))))
             
             (has-next-character-p ()
              "Determines whether a further character follows in the
               CODE succeeding the current POSITION, returning on
               confirmation a ``boolean'' value of ``T'', otherwise
               ``NIL''."
              (the boolean
                (not (null
                  (array-in-bounds-p code (1+ position))))))
             
             (next-character ()
              "Returns the character in the CODE succeeding the current
               POSITION, or signals an error of an unspecified type upon
               its boundaries' transgression."
              (the character
                (char code (1+ position))))
             
             (command-character-follows-p ()
              "Determines whether the character succeeding the current
               POSITION into the CODE represents a Ternary command
               constituent, returning on confirmation a ``boolean''
               value of ``T'', otherwise ``NIL''."
              (the boolean
                (not (null
                  (and (has-next-character-p)
                       (command-character-p (next-character)))))))
             
             (command-token-detected-p ()
              "Determines whether the compound of the character at the
               current POSITION into the CODE and the succeeding one
               amount to a Ternary command token, returning on
               confirmation a ``boolean'' value of ``T'', otherwise
               ``NIL''."
              (the boolean
                (and (command-character-encountered-p)
                     (command-character-follows-p))))
             
             (build-command-token ()
              "Creates and returns from the character at the current
               POSITION into the CODE and its successor a two-digit
               Ternary command token."
              (the command-token
                (format NIL "~c~c"
                  (char code position)
                  (char code (1+ position))))))
          
          (loop while (< position (length code)) do
            (cond
              ((command-token-detected-p)
                (push (command-for (build-command-token)) instructions)
                (incf position 2))
              (T
                (incf position)))))))
    
    (the ternary-program
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :accessor      memory-cells
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector of unsigned-byte-valued cells, with
                    the hash table keys designating the cell indices,
                    associating with the cell values.")
   (pointer
    :initarg       :pointer
    :initform      0
    :accessor      memory-pointer
    :type          integer
    :documentation "The cell pointer, responsible for designating the
                    current memory cell storing its index among the
                    CELLS table keys."))
  (:documentation
    "The ``Memory'' class implements a Ternary program's memory in terms
     of a sparse octet vector, amenable to a cell pointer in order to
     signify the currently active cell."))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the current MEMORY cell's value."
  (declare (type Memory memory))
  (the octet
    (gethash (memory-pointer memory) (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE into the current MEMORY cell, contingently
   wrapping the same around to respect the unsigned byte range [0, 255]
   prior to the admission, and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (gethash (memory-pointer memory) (memory-cells memory) 0)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the current MEMORY cell's value by one, upon transcendence
   of the upper byte march of 255 wrapping it around to the minimum of
   zero (0), and returns no value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the current MEMORY cell's value by one, upon transcedence
   of the lower byte march of zero (0) wrapping it around to the maximum
   of 255, and returns no value."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Generates and returns for the INSTRUCTIONS a jump table, matching
   every forward jump instruction's position in the program to the
   matching back jump location, and vice versa."
  (declare (type ternary-program instructions))
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
            (if forward-jump-positions
              (let ((start-position (pop forward-jump-positions))
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (setf (gethash start-position jump-table) end-position)
                (setf (gethash end-position jump-table) start-position))
              (error "Unmatched back jump instruction at program ~
                      position ~d."
                position)))
          (otherwise
            NIL)))
    
    (when forward-jump-positions
      (error "Unmatched forward jump instructions at ~
              positions ~{~d~^, ~}."
        forward-jump-positions))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the Ternary INSTRUCTIONS and returns no value."
  (declare (type ternary-program instructions))
  (when (plusp (length instructions))
    (let ((memory      (make-instance 'Memory))
          (ip          0)
          (instruction NIL)
          (jump-table  (build-jump-table instructions)))
      (declare (type Memory            memory))
      (declare (type fixnum            ip))
      (declare (type (or null command) instruction))
      (declare (type jump-table        jump-table))
      
      (loop while (< ip (length instructions)) do
        (setf instruction (aref instructions ip))
        
        (case instruction
          ((NIL)
            (loop-finish))
          
          (:move-right
            (memory-move-right memory))
          
          (:move-left
            (memory-move-left memory))
          
          (:increment
            (memory-increment memory))
          
          (:decrement
            (memory-decrement memory))
          
          (:output
            (write-char
              (code-char
                (memory-current-cell memory))))
          
          (:input
            (format T "~&>> ")
            (setf (memory-current-cell memory)
              (prog1
                (char-code (read-char))
                (clear-input))))
          
          (:jump-forward
            (when (zerop (memory-current-cell memory))
              (setf ip
                (or (gethash ip jump-table)
                    (error "Unrecognized forward jump position: ~d."
                      ip)))))
          
          (:jump-back
            (unless (zerop (memory-current-cell memory))
              (setf ip
                (or (gethash ip jump-table)
                    (error "Unrecognized back jump position :~d."
                      ip)))))
          
          (otherwise
            (error "Unrecognized instruction: ~s." instruction)))
        
        (incf ip))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Ternary (code)
  "Interprets the piece of Ternary source CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-Ternary
  "11111111111111110201111111110201111101111111011111110111000000001012011101110110010111020012
   00101201012001101010201111111111111120201111112001012000102000201111112010101010101020101010
   1010101010200101112001111120")

;;; -------------------------------------------------------

;; An infinitely repeating cat program which terminates on a user input
;; producing the null character.
(interpret-Ternary
  "2102202112")
