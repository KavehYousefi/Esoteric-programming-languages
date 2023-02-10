;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainfuck instruction extension", incited by the Esolang
;; user "A" in the year 2018, and founded upon the idea of enhancing
;; Urban Mueller's "brainfuck" by various beneficial operations.
;; 
;; 
;; Concept
;; =======
;; The brainfuck instruction extension programming language constitutes
;; an enhancement of the original specimen by operations capacitated to
;; iterate during a zero-valued state, specifically set the current cell
;; value, and apply the testing for a particular value.
;; 
;; == 75 NEW INSTRUCTIONS ARE INTRODUCED ==
;; The extension's contribution relates to the introduction of 75 new
;; commands, capable of a subsumption into three tiers:
;; 
;;   ------------------------------------------------------------------
;;   Subject            | Effect
;;   -------------------+----------------------------------------------
;;   Iterations         | Looping while the current cell value equals 0
;;                      | (zero):
;;                      |   ()
;;   ..................................................................
;;   Cell value setters | Setting the current cell value to an integer
;;                      | in the range [0, 35], specified by a base-36
;;                      | digit:
;;                      |   =0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
;;   ..................................................................
;;   Value testers      | Testing the current cell value for equality
;;                      | with an integer in the range [0, 35],
;;                      | specified by a base-36 digit:
;;                      |   $§¥€¢£₩«»•abcdefghijklmnopwrstuvwxyz
;;   ------------------------------------------------------------------
;; 
;; == THE TAPE-LIKE MEMORY REMAINS ==
;; Maugre its departure from the throughout compendious brainfuck
;; principle in operation regards, the extended variant retains the most
;; liberal interpretation of the memory architecture.
;; 
;; The memory is defined as a theoretically infinite linear arrangement
;; of cells, extending bilaterally. Each component contains a signed
;; integer of arbitrary magnitude, initialized to the value 0 (zero).
;; 
;; A cursor, known as the "cell pointer", or simply "pointer", selects
;; at any instant the current active cell, the sole entity amenable to
;; perquisitions and manipulations. At the program's inchoation empight
;; on the first unit, instructions may be applied for this cursor's
;; translation in both the sinistral and dextral airt.
;; 
;; 
;; Instructions
;; ============
;; The language's instruction set is compact of 83 members, embracing
;; the complete octuple inherited from the original brainfuck,
;; adhibiting operations for iterating while the current cell contains
;; 0 (zero), as well as setting the same unit to an integer in the range
;; [0, 35], and simulating a Boolean facility targeting this base-36
;; interval to supply the test values to participate in equality
;; predicates.
;; 
;; == OVERVIEW ==
;; A foundational apprehension, compendious in its circumference, shall
;; be administered by the tabular illustration below.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell's value by one.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   -       | Decrements the current cell's value by one.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   >       | Moves the cell pointer one step to the right.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   ,       | Queries for an input character and stores its numeric
;;           | ASCII code in the current cell.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   .       | Prints the character associated with the current cell
;;           | value, when interpreted as an ASCII character code, to
;;           | the standard output.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   [       | If the current cell value equals 0 (zero), moves the
;;           | instruction pointer forward to the position immediately
;;           | succeeding the matching "]" instruction.
;;           | Otherwise proceeds as usual.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   ]       | If the current cell value does not equal 0 (zero), moves
;;           | the instruction pointer back to the position immediately
;;           | succeeding the matching "[" instruction.
;;           | Otherwise proceeds as usual.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   (       | If the current cell value does not equal 0 (zero), moves
;;           | the instruction pointer forward to the position
;;           | immediately succeeding the matching ")" instruction.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   )       | If the current cell value equals 0 (zero), moves the
;;           | instruction pointer back to the position immediately
;;           | succeeding the matching "(" instruction.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   =       | Sets the current cell value to 0.
;;   ..................................................................
;;   0       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 0, that is, to a decimal 0.
;;   ..................................................................
;;   1       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 1, that is, to a decimal 1.
;;   ..................................................................
;;   2       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 2, that is, to a decimal 2.
;;   ..................................................................
;;   3       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 3, that is, to a decimal 3.
;;   ..................................................................
;;   4       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 4, that is, to a decimal 4.
;;   ..................................................................
;;   5       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 5, that is, to a decimal 5.
;;   ..................................................................
;;   6       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 6, that is, to a decimal 6.
;;   ..................................................................
;;   7       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 7, that is, to a decimal 7.
;;   ..................................................................
;;   8       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 8, that is, to a decimal 8.
;;   ..................................................................
;;   9       | Sets the current cell value to the decimal value of the
;;           | base-36 digit 9, that is, to a decimal 9.
;;   ..................................................................
;;   A       | Sets the current cell value to the decimal value of the
;;           | base-36 digit A, that is, to a decimal 10.
;;   ..................................................................
;;   B       | Sets the current cell value to the decimal value of the
;;           | base-36 digit B, that is, to a decimal 11.
;;   ..................................................................
;;   C       | Sets the current cell value to the decimal value of the
;;           | base-36 digit C, that is, to a decimal 12.
;;   ..................................................................
;;   D       | Sets the current cell value to the decimal value of the
;;           | base-36 digit D, that is, to a decimal 13.
;;   ..................................................................
;;   E       | Sets the current cell value to the decimal value of the
;;           | base-36 digit E, that is, to a decimal 14.
;;   ..................................................................
;;   F       | Sets the current cell value to the decimal value of the
;;           | base-36 digit F, that is, to a decimal 15.
;;   ..................................................................
;;   G       | Sets the current cell value to the decimal value of the
;;           | base-36 digit G, that is, to a decimal 16.
;;   ..................................................................
;;   H       | Sets the current cell value to the decimal value of the
;;           | base-36 digit H, that is, to a decimal 17.
;;   ..................................................................
;;   I       | Sets the current cell value to the decimal value of the
;;           | base-36 digit I, that is, to a decimal 18.
;;   ..................................................................
;;   J       | Sets the current cell value to the decimal value of the
;;           | base-36 digit J, that is, to a decimal 19.
;;   ..................................................................
;;   K       | Sets the current cell value to the decimal value of the
;;           | base-36 digit K, that is, to a decimal 20.
;;   ..................................................................
;;   L       | Sets the current cell value to the decimal value of the
;;           | base-36 digit L, that is, to a decimal 21.
;;   ..................................................................
;;   M       | Sets the current cell value to the decimal value of the
;;           | base-36 digit M, that is, to a decimal 22.
;;   ..................................................................
;;   N       | Sets the current cell value to the decimal value of the
;;           | base-36 digit N, that is, to a decimal 23.
;;   ..................................................................
;;   O       | Sets the current cell value to the decimal value of the
;;           | base-36 digit O, that is, to a decimal 24.
;;   ..................................................................
;;   P       | Sets the current cell value to the decimal value of the
;;           | base-36 digit P, that is, to a decimal 25.
;;   ..................................................................
;;   Q       | Sets the current cell value to the decimal value of the
;;           | base-36 digit Q, that is, to a decimal 26.
;;   ..................................................................
;;   R       | Sets the current cell value to the decimal value of the
;;           | base-36 digit R, that is, to a decimal 27.
;;   ..................................................................
;;   S       | Sets the current cell value to the decimal value of the
;;           | base-36 digit S, that is, to a decimal 28.
;;   ..................................................................
;;   T       | Sets the current cell value to the decimal value of the
;;           | base-36 digit T, that is, to a decimal 29.
;;   ..................................................................
;;   U       | Sets the current cell value to the decimal value of the
;;           | base-36 digit U, that is, to a decimal 30.
;;   ..................................................................
;;   V       | Sets the current cell value to the decimal value of the
;;           | base-36 digit V, that is, to a decimal 31.
;;   ..................................................................
;;   W       | Sets the current cell value to the decimal value of the
;;           | base-36 digit W, that is, to a decimal 32.
;;   ..................................................................
;;   X       | Sets the current cell value to the decimal value of the
;;           | base-36 digit X, that is, to a decimal 33.
;;   ..................................................................
;;   Y       | Sets the current cell value to the decimal value of the
;;           | base-36 digit Y, that is, to a decimal 34.
;;   ..................................................................
;;   Z       | Sets the current cell value to the decimal value of the
;;           | base-36 digit Z, that is, to a decimal 35.
;;   ..................................................................
;;   $       | If the current cell value equals 0, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   §       | If the current cell value equals 1, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   ¥       | If the current cell value equals 2, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   €       | If the current cell value equals 3, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   ¢       | If the current cell value equals 4, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   £       | If the current cell value equals 5, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   ₩       | If the current cell value equals 6, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   «       | If the current cell value equals 7, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   »       | If the current cell value equals 8, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   •       | If the current cell value equals 9, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   a       | If the current cell value equals 10, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   b       | If the current cell value equals 11, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   c       | If the current cell value equals 12, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   d       | If the current cell value equals 13, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   e       | If the current cell value equals 14, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   f       | If the current cell value equals 15, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   g       | If the current cell value equals 16, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   h       | If the current cell value equals 17, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   i       | If the current cell value equals 18, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   j       | If the current cell value equals 19, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   k       | If the current cell value equals 20, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   l       | If the current cell value equals 21, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   m       | If the current cell value equals 22, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   n       | If the current cell value equals 23, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   o       | If the current cell value equals 24, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   p       | If the current cell value equals 25, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   w       | If the current cell value equals 26, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   r       | If the current cell value equals 27, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   s       | If the current cell value equals 28, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   t       | If the current cell value equals 29, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   u       | If the current cell value equals 30, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   v       | If the current cell value equals 31, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   w       | If the current cell value equals 32, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   x       | If the current cell value equals 33, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   y       | If the current cell value equals 34, changes it to 1,
;;           | otherwise sets it to 0.
;;   ..................................................................
;;   z       | If the current cell value equals 35, changes it to 1,
;;           | otherwise sets it to 0.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-02-08
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BF_instruction_extension"
;;   -> "https://esolangs.org/wiki/Brainfuck"
;;       o Specification of the original brainfuck programming language.
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

(deftype command ()
  "The ``command'' type enumerates the recognized extended brainfuck
   instruction variants."
  '(member
    :increment                  ;; +
    :decrement                  ;; -
    :move-right                 ;; >
    :move-left                  ;; <
    :execute-if-not-zero        ;; [
    :repeat-if-not-zero         ;; ]
    :input                      ;; ,
    :output                     ;; .
    :execute-if-zero            ;; (
    :repeat-if-zero             ;; )
    :set-to-value               ;; =1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ
    :set-to-binary))            ;; $§¥€¢£₩«»•abcdefghijklmnopwrstuvwxyz

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer greater than or
   equal to zero, but with no upper bound, preponderantly applicable in
   the context of instruction operands."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype operand ()
  "The ``operand'' type defines an argument to an instruction, which
   might either be a non-negative integer or the ``NIL'' value, the
   latter serving as a sentinel for a niladic instruction."
  '(or null non-negative-integer))

;;; -------------------------------------------------------

(deftype instruction-table ()
  "The ``instruction-table'' defines a mapping of tokens to instructions
   in the form of a hash table that associates with characters the
   represented ``Instruction'' objects."
  '(hash-table-of character Instruction))

;;; -------------------------------------------------------

(deftype brainfuck-program ()
  "The ``brainfuck-program'' type defines a sequence of zero or more
   executable brainfuck instructions, modeled either as a vector of
   ``Instruction'' objects or a one-dimensional simple array amplecting
   the same element type."
  '(or (vector       Instruction  *)
       (simple-array Instruction (*))))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of jump start positions in
   an instruction sequence to the corresponding end positions, and vice
   versa, implemented by a hash table that affiliates fixnum locations
   as keys to the same as values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' type defines a sparse vector of cells, indexed by
   adminiculum of a signed integer subscript which points to a scalar
   object of the same type, and realized in the form of a hash table
   mapping integer keys to integer values."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (operand NIL))))
  "The ``Instruction'' class encapsulates the concept of an extended
   brainfuck operation, compact of a mandatory categorizing type and an
   optional non-negative integer operand."
  (type    (error "Missing instruction type.") :type command)
  (operand NIL                                 :type operand))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of instruction table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type instruction-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'eql)
  "Associates the recognized extended brainfuck instruction tokens with
   the respective ``Instruction'' objects.")

;;; -------------------------------------------------------

(flet ((register-identifier (token instruction-type
                             &optional (operand NIL))
        "Creates a new ``Instruction'' delineated by the categorizing
         INSTRUCTION-TYPE and an optional OPERAND, associates the same
         in the +IDENTIFIERS+ table with the TOKEN, and returns no
         value."
        (declare (type character token))
        (declare (type command   instruction-type))
        (declare (type operand   operand))
        (setf (gethash token +IDENTIFIERS+)
              (make-instruction instruction-type operand))
        (values)))
  (register-identifier #\+ :increment)
  (register-identifier #\- :decrement)
  (register-identifier #\> :move-right)
  (register-identifier #\< :move-left)
  (register-identifier #\, :input)
  (register-identifier #\. :output)
  (register-identifier #\[ :execute-if-not-zero)
  (register-identifier #\] :repeat-if-not-zero)
  (register-identifier #\( :execute-if-zero)
  (register-identifier #\) :repeat-if-zero)
  (register-identifier #\= :set-to-value 0)
  (register-identifier #\0 :set-to-value 0)
  (register-identifier #\1 :set-to-value 1)
  (register-identifier #\2 :set-to-value 2)
  (register-identifier #\3 :set-to-value 3)
  (register-identifier #\4 :set-to-value 4)
  (register-identifier #\5 :set-to-value 5)
  (register-identifier #\6 :set-to-value 6)
  (register-identifier #\7 :set-to-value 7)
  (register-identifier #\8 :set-to-value 8)
  (register-identifier #\9 :set-to-value 9)
  (register-identifier #\A :set-to-value 10)
  (register-identifier #\B :set-to-value 11)
  (register-identifier #\C :set-to-value 12)
  (register-identifier #\D :set-to-value 13)
  (register-identifier #\E :set-to-value 14)
  (register-identifier #\F :set-to-value 15)
  (register-identifier #\G :set-to-value 16)
  (register-identifier #\H :set-to-value 17)
  (register-identifier #\I :set-to-value 18)
  (register-identifier #\J :set-to-value 19)
  (register-identifier #\K :set-to-value 20)
  (register-identifier #\L :set-to-value 21)
  (register-identifier #\M :set-to-value 22)
  (register-identifier #\N :set-to-value 23)
  (register-identifier #\O :set-to-value 24)
  (register-identifier #\P :set-to-value 25)
  (register-identifier #\Q :set-to-value 26)
  (register-identifier #\R :set-to-value 27)
  (register-identifier #\S :set-to-value 28)
  (register-identifier #\T :set-to-value 29)
  (register-identifier #\U :set-to-value 30)
  (register-identifier #\V :set-to-value 31)
  (register-identifier #\W :set-to-value 32)
  (register-identifier #\X :set-to-value 33)
  (register-identifier #\Y :set-to-value 34)
  (register-identifier #\Z :set-to-value 35)
  (register-identifier #\$ :set-to-binary 0)
  (register-identifier #\§ :set-to-binary 1)
  (register-identifier #\¥ :set-to-binary 2)
  (register-identifier #\€ :set-to-binary 3)
  (register-identifier #\¢ :set-to-binary 4)
  (register-identifier #\£ :set-to-binary 5)
  (register-identifier #\₩ :set-to-binary 6)
  (register-identifier #\« :set-to-binary 7)
  (register-identifier #\» :set-to-binary 8)
  (register-identifier #\• :set-to-binary 9)
  (register-identifier #\a :set-to-binary 10)
  (register-identifier #\b :set-to-binary 11)
  (register-identifier #\c :set-to-binary 12)
  (register-identifier #\d :set-to-binary 13)
  (register-identifier #\e :set-to-binary 14)
  (register-identifier #\f :set-to-binary 15)
  (register-identifier #\g :set-to-binary 16)
  (register-identifier #\h :set-to-binary 17)
  (register-identifier #\i :set-to-binary 18)
  (register-identifier #\j :set-to-binary 19)
  (register-identifier #\k :set-to-binary 20)
  (register-identifier #\l :set-to-binary 21)
  (register-identifier #\m :set-to-binary 22)
  (register-identifier #\n :set-to-binary 23)
  (register-identifier #\o :set-to-binary 24)
  (register-identifier #\p :set-to-binary 25)
  (register-identifier #\w :set-to-binary 26)
  (register-identifier #\r :set-to-binary 27)
  (register-identifier #\s :set-to-binary 28)
  (register-identifier #\t :set-to-binary 29)
  (register-identifier #\u :set-to-binary 30)
  (register-identifier #\v :set-to-binary 31)
  (register-identifier #\w :set-to-binary 32)
  (register-identifier #\x :set-to-binary 33)
  (register-identifier #\y :set-to-binary 34)
  (register-identifier #\z :set-to-binary 35)
  (values))

;;; -------------------------------------------------------

(defun instruction-token-p (token)
  "Checks whether the TOKEN corresponding to a recognized extended
   brainfuck instruction, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash token +IDENTIFIERS+))))))

;;; -------------------------------------------------------

(defun get-instruction (token)
  "Returns the instruction in correspondence with the TOKEN, or signals
   an error of an unspecified type if no association holds."
  (declare (type character token))
  (the Instruction
    (or (gethash token +IDENTIFIERS+)
        (error "No instruction token: ~c." token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts and returns from the piece of extended brainfuck source CODE
   a simple one-dimensional array of instructions."
  (declare (type string code))
  (the brainfuck-program
    (coerce
      (loop
        :for token :of-type character :across code
        :when (instruction-token-p token)
        :collect (get-instruction token))
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class implements the program memory, manifesting in a
   theoretically infinite tally of signed-integer-valued cells, the
   current instance of which is designated by a mobile pointer."
  (cells   (make-hash-table :test #'eql) :type cell-map)
  (pointer 0                             :type integer))

;;; --------------------------------------------------

(defmacro with-memory ((memory) &body body)
  "Evaluates the MEMORY, binds its slots ``cells'' and ``pointer'' to
   eponymous local symbol macros, evaluates the BODY forms, and returns
   the last processed form's results."
  (let ((evaluated-memory (gensym)))
    (declare (type symbol evaluated-memory))
    `(let ((,evaluated-memory ,memory))
       (declare (type Memory ,evaluated-memory))
       (declare (ignorable   ,evaluated-memory))
       (symbol-macrolet
           ((cells
              (the cell-map
                (memory-cells ,evaluated-memory)))
            (pointer
              (the integer
                (memory-pointer ,evaluated-memory))))
         (declare (type cell-map cells))
         (declare (ignorable     cells))
         (declare (type integer  pointer))
         (declare (ignorable     pointer))
         ,@body))))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns no
   value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns no
   value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory  memory))
  (with-memory (memory)
    (the integer
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Sets the MEMORY's current cell to the NEW-VALUE and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-memory (memory)
    (setf (gethash pointer cells 0)
          new-value))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell value by one and returns no
   value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell value by one and returns no
   value."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-set-to-binary-if-equals (memory expected-value)
  "If the MEMORY's current cell contains the EXPECTED-VALUE, replaces
   the same with the value 1 (one), otherwise with 0 (zero), and returns
   no value."
  (declare (type Memory  memory))
  (declare (type integer expected-value))
  (setf (memory-current-cell memory)
    (if (= (memory-current-cell memory) expected-value)
      1
      0))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Generates and returns for the INSTRUCTIONS a jump table which
   associates each jump start position in the sequence with the matching
   end location, and vice versa."
  (declare (type brainfuck-program instructions))
  (let ((jump-table           (make-hash-table :test #'eql))
        (non-zero-jump-starts NIL)  ;; Positions of "[" instructions.
        (zero-jump-starts     NIL)) ;; Positions of "(" instructions.
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) non-zero-jump-starts))
    (declare (type (list-of fixnum) zero-jump-starts))
    (loop
      for instruction of-type Instruction across instructions
      and position    of-type fixnum      from   0 by 1
      do
        (case (instruction-type instruction)
          ;; "[" encountered?
          (:execute-if-not-zero
            (push position non-zero-jump-starts))
          ;; "]" encountered?
          (:repeat-if-not-zero
            (if non-zero-jump-starts
              (let ((jump-start (pop non-zero-jump-starts))
                    (jump-end   position))
                (declare (type fixnum jump-start))
                (declare (type fixnum jump-end))
                (setf (gethash jump-start jump-table) jump-end)
                (setf (gethash jump-end   jump-table) jump-start))
              (error "Unmatched \"]\" instruction at position ~d."
                position)))
          ;; "(" encountered?
          (:execute-if-zero
            (push position zero-jump-starts))
          ;; ")" encountered?
          (:repeat-if-zero
            (if zero-jump-starts
              (let ((jump-start (pop zero-jump-starts))
                    (jump-end   position))
                (declare (type fixnum jump-start))
                (declare (type fixnum jump-end))
                (setf (gethash jump-start jump-table) jump-end)
                (setf (gethash jump-end   jump-table) jump-start))
              (error "Unmatched \")\" instruction at position ~d."
                position)))
          (otherwise
            NIL)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the extended brainfuck INSTRUCTIONS and returns no value."
  (declare (type brainfuck-program instructions))
  (when (plusp (length instructions))
    (let ((ip         0)
          (jump-table (build-jump-table instructions))
          (memory     (make-memory)))
      (declare (type fixnum     ip))
      (declare (type jump-table jump-table))
      (declare (type Memory     memory))
      (flet
          ((advance ()
            "Advances the instruction pointer IP to the next position in
             the INSTRUCTIONS sequence and returns no value."
            (incf ip)
            (values))
           
           (move-to-opposite-boundary ()
            "Expecting the instruction pointer IP to reside at a jump
             boundary, relocates the same to the opposite bourn, and
             returns no value."
            (setf ip
              (or (gethash ip jump-table)
                  (error "No opposite boundary defined for position ~d."
                         ip)))
            (values))
           
           (has-more-instructions-p ()
            "Checks whether the instruction pointer IP currently resides
             on an element of the INSTRUCTIONS sequence, returning on
             confirmation a ``boolean'' value of ``T'', otherwise
             ``NIL''."
            (the boolean
              (not (null
                (array-in-bounds-p instructions ip)))))
           
           (get-current-instruction ()
            "Returns the instruction answering to the instruction
             pointer (IP) location, or signals an error of an
             unspecified type upon the instruction pointer's
             transgression of the defined sequence bounds."
            (the Instruction
              (aref instructions ip))))
        
        (loop while (has-more-instructions-p) do
          (let ((current-instruction (get-current-instruction)))
            (declare (type Instruction current-instruction))
            (case (instruction-type current-instruction)
              ;; +
              (:increment
                (memory-increment memory))
              
              ;; -
              (:decrement
                (memory-decrement memory))
              
              ;; >
              (:move-right
                (memory-move-right memory))
              
              ;; <
              (:move-left
                (memory-move-left memory))
              
              ;; ,
              (:input
                (format T "~&Please enter a character: ")
                (setf (memory-current-cell memory)
                      (char-code (read-char)))
                (clear-input))
              
              ;; .
              (:output
                (write-char
                  (code-char
                    (memory-current-cell memory))))
              
              ;; [
              (:execute-if-not-zero
                (when (zerop (memory-current-cell memory))
                  (move-to-opposite-boundary)))
              
              ;; ]
              (:repeat-if-not-zero
                (unless (zerop (memory-current-cell memory))
                  (move-to-opposite-boundary)))
              
              ;; (
              (:execute-if-zero
                (unless (zerop (memory-current-cell memory))
                  (move-to-opposite-boundary)))
              
              ;; )
              (:repeat-if-zero
                (when (zerop (memory-current-cell memory))
                  (move-to-opposite-boundary)))
              
              ;; =0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
              (:set-to-value
                (setf (memory-current-cell memory)
                  (instruction-operand current-instruction)))
              
              ;; $§¥€¢£₩«»•abcdefghijklmnopwrstuvwxyz
              (:set-to-binary
                (memory-set-to-binary-if-equals memory
                  (instruction-operand current-instruction)))
              
              (otherwise
                (error "Invalid instruction ~s at position ~d."
                  current-instruction ip))))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-brainfuck-instruction-extension (code)
  "Interprets the piece of extended brainfuck source CODE and returns no
   value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the first cell to the value 33 (= "X" in base-36) and output its
;; associated character, which constitutes the ecphoneme "!".
(interpret-brainfuck-instruction-extension "X.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-brainfuck-instruction-extension "=(,.=)")

;;; -------------------------------------------------------

;; Print "Hello World!" to the standard output.
(interpret-brainfuck-instruction-extension
  ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->
   +++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+.")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-brainfuck-instruction-extension
  ">>>,.[[->+<<+>]>-]<<<[<<]>[.]")

;;; -------------------------------------------------------

;; A truth machine which employs the Boolean predicate check.
;; 
;;   ------------------------------------------------------------------
;;   Line # | Code           | Effect
;;   -------+----------------+-----------------------------------------
;;   1      | ,              | Store the user input in cell[0].
;;   ..................................................................
;;   2      | [->+>+<<]      | Copy the value of cell[0] to both
;;          |                | cell[1] and cell[2], while concomitantly
;;          |                | reducing cell[0] to 0.
;;          |                | cell[1] will be the test cell, holding
;;          |                | the "Boolean" result of testing against
;;          |                | the value 1.
;;          |                | cell[2] will be the print cell, which,
;;          |                | for a Boolean true, prints "1".
;;   ..................................................................
;;   3      | >.             | Move to the first copy of cell[0], that
;;          |                | is, to cell[1], and print its character,
;;          |                | which should either produce "0" or "1".
;;   ..................................................................
;;   4      | -------------- | Reduce the character --- either 48 for
;;          |                | an input of "0" or 49 for a "1" --- to
;;          |                | the value 34 or 35, capable of being
;;          |                | juxtaposed in a Boolean test with a
;;          |                | base-36 number.
;;   ..................................................................
;;   5      | z              | Check the cell value against the decimal
;;          |                | value 35 (= base-36 digit: z). Upon its
;;          |                | equality with 35, set the cell value to
;;          |                | 1, otherwise to 0.
;;   ..................................................................
;;   6      | [>.<]          | If the test cell[1] contains the value
;;          |                | 1, which is tantamount to a Boolean
;;          |                | true, repeatedly print cell[2], that is,
;;          |                | the character "1".
;;   ------------------------------------------------------------------
(interpret-brainfuck-instruction-extension
  ",
   [->+>+<<]
   >.
   --------------
   z
   [>.<]")
