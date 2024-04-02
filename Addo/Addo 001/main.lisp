;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Addo", presented by the Esolang user "ChuckEsoteric08" on
;; August 15th, 2022, and participating in the nature of the
;; "One Instruction Set Computer" (OISC), thus being defined by a single
;; instruction whose triplet of operands determines the concrete
;; behavior, one capacitated to add memory items, as well as jump in
;; dependence upon the result.
;; 
;; 
;; Concept
;; =======
;; The Addo programming language subsumbes into the category of the
;; "One Instruction Set Computer" (OISC) languages, employing a single
;; instruction, compact of a triple of operands, which serve to add two
;; memory items, and contingently jump to a third for the next
;; iteration.
;; 
;; == [ADD] AND JUMP IF [O]DD ==
;; The basic deportment reverberates in the language's agnomination, as
;; "Addo" refers to its procedure, proceeding to *Add* and jump if
;; *o*dd.
;; 
;; == PROCEDURE ==
;; Given the three memory elements a, b and c, commencing at the memory
;; pointer's current location, the following procedure applies:
;; 
;;   (1) Queries the memory element m[a] at the address a.
;;   (2) Queries the memory element m[b] at the address b.
;;   (3) Stores the sum of m[a] and m[b] in the memory cell at the
;;       address a:
;;         m[a] <- m[a] + m[b].
;;   (4) If the new value of the memory element m[a] at the address a
;;       constitutes an odd number, relocates the memory pointer to the
;;       address c:
;;         if (m[a] mod 2) != 0 then
;;           pointer <- c
;;         end if
;;       If the pointer location c designates a negative location, the
;;       program immediately ceases:
;;         if pointer < 0 then
;;           terminate program
;;         end if
;;       For an even-valued m[a], the control flow instead moves the
;;       pointer to the next command by translating it three positions
;;       forward:
;;         if (m[a] mod 2) = 0 then
;;           pointer <- pointer + 3
;;         end if
;; 
;; A more compendious parlecue shall be limned by the following
;; pseudocode fragment:
;; 
;;   let pointer <- 0
;;   
;;   repeat do
;;     let a <- pointer
;;     let b <- pointer + 1
;;     let c <- pointer + 2
;;     
;;     memory[a] <- memory[a] + memory[b]
;;     
;;     if (memory[a] mod 2) != 0 then
;;       pointer <- c
;;     else
;;       pointer <- pointer + 3
;;     end if
;;     
;;     if pointer < 0 then
;;       terminate program
;;     end if
;;   end repeat
;; 
;; == MEMORY: A UNIFIED LOCATION FOR PROGRAM AND DATA ==
;; As consectary of its OISC cleronomy, the program and data constitute
;; occupants of a single random-access memory space, compact of a
;; theoretically infinite span of integer-valued cells, signed and of no
;; bourne in any airt, initialized with a default value of zero (0). All
;; cells are amenable to an integer index, known as the address or
;; location.
;; 
;; The program's inchoation issuing from the cell at the address 0,
;; three consecutive elements are embraced in order to execute the first
;; instruction cycle, whence the iterative process ensues, until a
;; negative jump location is encountered.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-04-22
;; 
;; Sources:
;;   [esolang2023Addo]
;;   The Esolang contributors, "Addo", April 29th, 2023
;;   URL: "https://esolangs.org/wiki/Addo"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(deftype sparse-vector ()
  "The ``sparse-vector'' type defines a sparse vector of signed
   integers, enumerated with subscripts desumed from the same space, as
   a sparse mapping from signed integer addresses to signed integer
   cells values, realized as a hash table of integer keys and values."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism of which enumerates, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (integer Memory integer) (values))
                (setf memory-at)))
(declaim (ftype (function (Memory integer) integer)
                memory-at))

;;; -------------------------------------------------------

(defstruct (Memory
  (:constructor make-empty-memory ()))
  "The ``Memory'' class implements the infinite program memory as a
   sparse vector of signed integer number, enumerated by adminiculum of
   signed integer addresses."
  (cells           (make-hash-table :test #'eql)
                   :type      sparse-vector
                   :read-only T)
  (lowest-address  0
                   :type      integer
                   :read-only NIL)
  (highest-address 0
                   :type      integer
                   :read-only NIL))

;;; -------------------------------------------------------

(defun make-memory-from (elements)
  "Creates and returns a new memory object composed of the ELEMENTS,
   occupying the first locations by commencing with the address zero
   (0)."
  (declare (type (list-of integer) elements))
  (the Memory
    (loop
      with    memory of-type Memory = (make-empty-memory)
      for     location of-type integer from 0 by 1
      for     element  of-type integer in   elements
      do      (setf (memory-at memory location) element)
      finally (return memory))))

;;; -------------------------------------------------------

(defun update-address-range (memory accessed-address)
  "Determines whether the ACCESSED-ADDRESS constitutes transcends the
   MEMORY's currently circumscribed index range, upon confirmation
   updating the MEMORY's state, in any case returns no value."
  (declare (type Memory  memory))
  (declare (type integer accessed-address))
  (setf (memory-lowest-address memory)
    (min accessed-address
      (memory-lowest-address memory)))
  (setf (memory-highest-address memory)
    (max accessed-address
      (memory-highest-address memory)))
  (values))

;;; -------------------------------------------------------

(defun memory-at (memory address)
  "Returns the MEMORY element at the ADDRESS."
  (declare (type Memory  memory))
  (declare (type integer address))
  (update-address-range memory address)
  (the integer
    (nth-value 0
      (gethash address (memory-cells memory) 0))))

;;; -------------------------------------------------------

(defun (setf memory-at) (new-value memory address)
  "Stores the NEW-VALUE in the MEMORY's cell at the ADDRESS and returns
   no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (declare (type integer address))
  (setf (gethash address (memory-cells memory) 0) new-value)
  (update-address-range memory address)
  (values))

;;; -------------------------------------------------------

(defun memory-extract-instruction (memory address)
  "Queries the three MEMORY elements commencing at the ADDRESS and
   returns these as three values in the correct order."
  (declare (type Memory  memory))
  (declare (type integer address))
  (the (values integer integer integer)
    (values
      (memory-at memory (+ address 0))
      (memory-at memory (+ address 1))
      (memory-at memory (+ address 2)))))

;;; -------------------------------------------------------

(defun memory-add (memory augend-address addend-address)
  "Increments the MEMORY cell at the AUGEND-ADDRESS by the value of the
   cell at the ADDEND-ADDRESS and returns no value."
  (declare (type Memory  memory))
  (declare (type integer augend-address))
  (declare (type integer addend-address))
  (incf (memory-at memory augend-address)
        (memory-at memory addend-address))
  (values))

;;; -------------------------------------------------------

(defun memory-odd-p (memory address)
  "Determines whether the MEMORY cell at the ADDRESS represents an odd
   number, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory  memory))
  (declare (type integer address))
  (the boolean
    (not (null
      (oddp (memory-at memory address))))))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory      memory))
  (declare (type destination stream))
  (format stream "Memory:")
  (loop
    for current-address
      of-type integer
      from    (memory-lowest-address  memory)
      to      (memory-highest-address memory)
    do
      (format stream "~&~2t[~d] = ~d" current-address
        (memory-at memory current-address))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun eof-p (source position)
  "Determines whether the POSITION constitutes an invalid location in
   the SOURCE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (array-in-bounds-p source position))))

;;; -------------------------------------------------------

(defun whitespace-at-p (source position)
  "Determines whether the character at the POSITION in the SOURCE
   represents a whitespace, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (null
      (and (array-in-bounds-p      source position)
           (whitespace-character-p (char source position)))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Ensuing from the START position in the SOURCE, skips a sequence of
   zero or more accolent whitespaces, and returns the location in the
   SOURCE of the first non-whitespace character following the START."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start by 1
      while   (whitespace-at-p source position)
      finally (return position))))

;;; -------------------------------------------------------

(defun digit-at-p (source position)
  "Determines whether the character at the POSITION in the SOURCE
   represents a decimal digit, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (null
      (and (array-in-bounds-p source position)
           (digit-char-p      (char source position)))))))

;;; -------------------------------------------------------

(defun expect-digit (source position)
  "Determines whether the POSITION in the SOURCE entails a decimal
   digit, on confirmation returning no value, otherwise signaling an
   error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum position))
  (cond
    ((digit-at-p source position)
      NIL)
    ((eof-p source position)
      (error "Expected a digit at position ~d, but encountered EOF."
        position))
    (T
      (error "Expected a digit at position ~d, but encountered ~c."
        position (char source position))))
  (values))

;;; -------------------------------------------------------

(defun sign-at-p (source position)
  "Determines whether the character at the POSITION in the SOURCE
   represents a matematical sign, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (null
      (and (array-in-bounds-p source position)
           (sign-character-p (char source position)))))))

;;; -------------------------------------------------------

(defun read-number (source start)
  "Ensuing from the START position in the SOURCE, reads a signed or
   unsigned integer number, contingently preceded by whitespaces, and
   returns two values:
     (1) the consumed and parsed signed or unsigned integer value
     (2) the position in the SOURCE immediately succeeding the extracted
         NUMBER."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values integer fixnum)
    (with-open-stream (digits (make-string-output-stream))
      (declare (type string-stream digits))
      (let ((position (skip-whitespaces source start)))
        (declare (type fixnum position))
        (when (sign-at-p source position)
          (write-char (char source position) digits)
          (incf position))
        (expect-digit source position)
        (loop
          while (digit-at-p source position)
          do    (write-char (char source position) digits)
          do    (incf position)
          finally
            (return
              (values
                (parse-integer
                  (get-output-stream-string digits))
                position)))))))

;;; -------------------------------------------------------

(defun parse-memory (source)
  "Extracts from the SOURCE a sequence of integer elements and returns a
   new memory object comprehending the same."
  (declare (type string source))
  (the Memory
    (loop
      with    position of-type fixnum = (skip-whitespaces source 0)
      while   (not (eof-p source position))
      collect
        (multiple-value-bind (address new-position)
            (read-number source position)
          (declare (type integer address))
          (declare (type fixnum  new-position))
          (prog1 address
            (setf position
              (skip-whitespaces source new-position))))
        into addresses
      finally
        (return
          (make-memory-from addresses)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((memory
    :initarg       :memory
    :initform      (error "Missing program memory.")
    :type          Memory
    :documentation "The program memory, comprehending both the
                    instructions and the data.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          integer
    :documentation "The address in the MEMORY referencing the current
                    instruction's first parameter.")
   (halted-p
    :initarg       :halted-p
    :initform      NIL
    :type          boolean
    :documentation "A flag that determines whether the program has
                    ceased."))
  (:documentation
    "The ``Interpreter'' class serves to operate an Addo program on a
     piece of memory, maintaining a pointer for the instruction
     evaluation."))

;;; -------------------------------------------------------

(defun make-interpreter (memory)
  "Creates and returns a new ``Interpreter'', responsible for operating
   on the MEMORY."
  (declare (type memory memory))
  (the Interpreter
    (make-instance 'Interpreter :memory memory)))

;;; -------------------------------------------------------

(defun interpreter-process-instruction (interpreter)
  "Processes the INTERPRETER's current instruction and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (memory pointer halted-p) interpreter
    (declare (type memory  memory))
    (declare (type integer pointer))
    (declare (ignorable    pointer))
    (declare (type boolean halted-p))
    (declare (ignorable    halted-p))
    
    (multiple-value-bind (a b c)
        (memory-extract-instruction memory pointer)
      (declare (type integer a))
      (declare (type integer b))
      (declare (type integer c))
      (declare (ignorable    c))
      
      ;; Add.
      (memory-add memory a b)
      
      ;; Jump if odd.
      (if (memory-odd-p memory a)
        (setf pointer c)
        (incf pointer 3))
      
      ;; Probe for termination condition.
      (when (minusp pointer)
        (setf halted-p T))))
  
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Applies the INTERPRETER to the code under its castaldy and returns
   no value."
  (declare (type Interpreter interpreter))
  (loop until (slot-value interpreter 'halted-p) do
    (interpreter-process-instruction interpreter))
  (format T "~&~a"
    (slot-value interpreter 'memory))
  (values))

;;; -------------------------------------------------------

(defun interpret-Addo (code)
  "Interprets the piece of Addo source CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parse-memory code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increment the cell memory[3] (= 1) by memory[4] (= 2), rendering the
;; former to memory[3] = 3, jump to the last line, increment
;; memory[3] (= 3) again by memory[4] (= 2) to memory[3] = 5, and jump
;; to the position -1, which designates the program's termination.
(interpret-Addo
  "3  4  6
   1  2  3
   3  4 -1")
