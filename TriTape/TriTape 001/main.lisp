;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "TriTape", invented by the Esolang user "Joaozin003" and
;; presented on May 21st, 2023, the indicium of which wones in the
;; deployment of ternary bits, or "trits", in its memory, compact of a
;; unilateral infinite tape of cells and a scalar accumulator which
;; through their collaboration capacitate a program's execution.
;; 
;; 
;; Concept
;; =======
;; The TriTape programming language is founded upon the manipulation of
;; ternary bits, or "trits", bits extended to a treble state, and
;; encoded in the integral range [0, 2], both as participants in a
;; unilaterally infinite tape and a scalar accumulator.
;; 
;; 
;; Architecture
;; ============
;; TriTape's memory tallies a dyadic componency, the more potent moeity
;; among this twissel's presence is contributed by a unilaterally
;; infinite dispansion of ternary bits, the parvipoent supplement
;; empighted by an accumulator as a scalar desumed from this realm.
;; 
;; == THE TAPE: AN INFINITE VECTOR OF TERNARY BITS ==
;; The paravaunt data salvatory manifests in a tape of ternary digits,
;; enumerated commencing with the subscript zero (0), and extending
;; in a bourneless manner along the positive axis.
;; 
;; == EACH TAPE CELL ENCOMPASSES A SINGLE TRIT ==
;; Each cell, at the program's inchoation, entails a default value of
;; zero (0), the state of which may be modulated via basic arithmetics
;; and input transfers, always obeying the ternary digit's integral
;; range of [0, 2]. Upon any of its bournes' transgression, the state
;; wraps around to into the obverse march.
;; 
;; == A CELL POINTER DESIGNATES THE CURRENTLY ACTIVE UNIT ==
;; At any instant during a program's execution, the cell pointer marks
;; the currently active cell, the sole instance amenable to
;; perquisitions and modifications. Several instructions capacitate the
;; pointer's gradual translation along the tape.
;; 
;; == THE ACCUMULATOR: A SCALAR TERNARY BIT ==
;; A parhedral memory component, the accumulator partakes of the same
;; capacity as a tape cell, lending harborage to a scalar ternary digit
;; from the integral range [0, 2], and equally wrapping around its
;; marches as a consequence of its extrema's transgression.
;; 
;; == TAPE AND ACCUMULATOR ENGAGE IN A COEFFICIENCY ==
;; The discrepancies of the tape cells' and accumulator's competences,
;; as well as their intersection in certain instructions, serves to
;; achieve an intercourse whence is an amplified potential for the data
;; management begotten.
;; 
;; 
;; Data Types
;; ==========
;; The TriTape programming language's data aspects wist of an aefauld
;; object species only, whence its agnomination's derivation ensues:
;; ternary numbers, which in the context of computer science relates to
;; ternary bits, or "trits", an integral number commorant in the closed
;; interval [0, 2].
;; 
;; 
;; Syntax
;; ======
;; Programs in TriTape constitute a composition of single-character
;; instructions, with a concomitant desistence from apportioning effect
;; to non-operation characters, such may be deployed for the purpose of
;; commentary insertions.
;; 
;; 
;; Instructions
;; ============
;; The TriTape programming language enumerates a cardinality of 13
;; members, the circumference of which amplects basic arithmetics
;; defined on the accumulator and the tape, cell pointer management,
;; input and output facilities, and two control flow constructs.
;; 
;; == OVERVIEW ==
;; An apercu shall impart a cursory mete of gnarity anenst the
;; language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ^       | Increments the accumulator by one. If the new value
;;           | exceeds the upper extremum of two (2), wraps its state
;;           | around to the minimum of zero (0).
;;   ..................................................................
;;   v       | Decrements the accumulator by one. If the new value
;;           | transcends alow the lower extremum of zero (0), wraps
;;           | its state around to the maximum of two (2).
;;   ..................................................................
;;   >       | Translates the tape's cell pointer one step to the
;;           | right.
;;   ..................................................................
;;   <       | If the tape's cell pointer does not reside in the first
;;           | cell, translates one step to the left. Otherwise, if the
;;           | first cell is selected, instead copies the value of the
;;           | accumulator into the current, first, cell.
;;   ..................................................................
;;   =       | Copies the tape's current cell value into the
;;           | accumulator.
;;   ..................................................................
;;   0       | Resets the tape's current cell value to zero (0).
;;   ..................................................................
;;   +       | Increments the tape's current cell value by the
;;           | accumulator's value. If the new cell value transcends
;;           | the upper bourne of two (2), wraps the state around.
;;   ..................................................................
;;   ,       | Queries the standard input for a ternary digit and
;;           | stores the same in the tape's current cell.
;;   ..................................................................
;;   .       | Prints the tape's current cell value verbatim to the
;;           | standard output.
;;   ..................................................................
;;   [       | If the accumulator value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   }       | Moves the instruction pointer (IP) back to the position
;;           | of the matching "[" instruction; otherwise proceeds as
;;           | usual.
;;   ..................................................................
;;   {       | If the accumulator value does not equal zero (0), moves
;;           | the instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "}" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   }       | Moves the instruction pointer (IP) back to the position
;;           | of the matching "{" instruction; otherwise proceeds as
;;           | usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-10
;; 
;; Sources:
;;   [esolang2023TriTape]
;;   The Esolang contributors, "TriTape", June 26th, 2023
;;   URL: "https://esolangs.org/wiki/TriTape"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype trit ()
  "The ``trit'' type defines a ternary bit, or \"trit\", as a base-3
   or ternary digit, in terms of a non-negative integer whose range is
   restricted to [0, 2]."
  '(integer 0 2))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), but without an upper extremum's
   impositions, and thus, as a corollary, an integral object in the
   interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variants of
   TriTape operations."
  '(member
    :increment-accumulator
    :decrement-accumulator
    :move-pointer-right
    :move-pointer-left
    :copy-cell-to-accumulator
    :reset-cell
    :add-accumulator-to-cell
    :input
    :output
    :jump-forward-if-zero
    :jump-back-if-zero
    :jump-forward-if-not-zero
    :jump-back-if-not-zero))

;;; -------------------------------------------------------

(deftype triTape-program ()
  "The ``triTape-program'' type defines an executable TriTape program as
   a one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based stack composed of zero or
   more elements, each member of which conforms to the ELEMENT-TYPE, and
   defaults to the comprehensive ``T''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a ``boolean'' representation of the OBJECT which derives its
   dichotomy by the generalized boolean's diorism, returning for a
   non-``NIL'' OBJECT the value ``T'', otherwise, for a ``NIL'' input,
   ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-into-trit-range (value)
  "Wraps the signed integer VALUE into the valid ternary digit range of
   [0, 2] and returns this resulting datum."
  (declare (type integer value))
  (the trit
    (mod value 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-trit ()
  "Queries the standard input for a ternary digit, or trit, until such
   is supplied, and returns the thus adduced integral object.
   ---
   The query cycle is perpetuated until a valid trit object is
   committed."
  (prog ((raw-input    NIL)
         (parsed-input NIL))
    (declare (type (or null string)  raw-input))
    (declare (type (or null integer) parsed-input))
    query-for-input
      (format T "~&>> ")
      (handler-case
        (setf raw-input (read-line))
        (error ()
          (format T "~&*** No valid input: ~s. ***" raw-input)
          (go try-again)))
      (handler-case
        (setf parsed-input (parse-integer raw-input))
        (error ()
          (format T "~&*** No integer input: ~s. ***" raw-input)
          (go try-again)))
      (cond
        ((typep parsed-input 'trit)
          (go accept-input))
        (T
          (format T "~&*** No trit: ~s. ***" raw-input)
          (go try-again)))
      (go accept-input)
    try-again
      (format T "~&*** Please repeat your input. ***")
      (go query-for-input)
    accept-input
      (the trit
        (return parsed-input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 13)              +IDENTIFIER-TABLE+))
(declaim (type (simple-array instruction (13)) +INSTRUCTION-TABLE+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIER-TABLE+
  "^v><=0+,.[]{}"
  "Enumerates the recognized instruction identifiers, their positions
   into this string is correlated with the matching instruction's
   location into the +INSTRUCTION-TABLE+, which please see.")

(defparameter +INSTRUCTION-TABLE+
  (make-array 13
    :element-type     'instruction
    :initial-contents '(:increment-accumulator
                        :decrement-accumulator
                        :move-pointer-right
                        :move-pointer-left
                        :copy-cell-to-accumulator
                        :reset-cell
                        :add-accumulator-to-cell
                        :input
                        :output
                        :jump-forward-if-zero
                        :jump-back-if-zero
                        :jump-forward-if-not-zero
                        :jump-back-if-not-zero)
    :adjustable       NIL
    :fill-pointer     NIL)
  "Enumerates the recognized instructions, their positions into this
   vector correlates with the matching identifier's location into the
   +IDENTIFIER-TABLE+ table, which please see.")

;;; -------------------------------------------------------

(defun instruction-character-p (candidate)
  "Determines whether the CANDIDATE represents a recognized instruction
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate +IDENTIFIER-TABLE+ :test #'char=))))

;;; -------------------------------------------------------

(defun get-identifier-index (identifier)
  "Returns the position of the IDENTIFIER in the +IDENTIFIER-TABLE+,
   or signals an error of an unspecified type upon its absence
   therefrom."
  (declare (type character identifier))
  (the fixnum
    (or (position identifier +IDENTIFIER-TABLE+ :test #'char=)
        (error "Unrecognized instruction identifier: \"~c\"."
          identifier))))

;;; -------------------------------------------------------

(defun parse-instruction (identifier)
  "Returns the instruction associated with the IDENTIFIER, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type character identifier))
  (the instruction
    (aref +INSTRUCTION-TABLE+
      (get-identifier-index identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-program (code)
  "Parses the piece of TriTape source CODE and returns a one-dimensional
   simple array of its incorporated instructions."
  (declare (type string code))
  (the (simple-array instruction (*))
    (coerce
      (loop
        for token of-type character across code
        when (instruction-character-p token)
          collect (parse-instruction token))
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Jump-Table
  (:constructor make-empty-jump-table ()))
  "The ``Jump-Table'' class realizes a bilateral nexus betwixt forward
   and back jump locations in a TriTape program."
  (connections (make-hash-table :test #'eql)
               :type      (hash-table-of fixnum fixnum)
               :read-only NIL))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Affiliates the START-POINT and END-POINT in a bilateral manner in the
   JUMP-TABLE and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point
      (jump-table-connections jump-table))
    end-point
    (gethash end-point
      (jump-table-connections jump-table))
    start-point)
  (values))

;;; -------------------------------------------------------

(defun compute-jump-table (program)
  "Creates and returns a jump table for the TriTape PROGRAM, connecting
   its jump points by adminiculum of their locations inside of the
   instruction sequence."
  (declare (type triTape-program program))
  (let ((jump-table           (make-empty-jump-table))
        (bracket-start-points NIL)    ;; Locations of "[" tokens.
        (brace-start-points   NIL))   ;; Locations of "{" tokens.
    (declare (type Jump-Table        jump-table))
    (declare (type (stack-of fixnum) bracket-start-points))
    (declare (type (stack-of fixnum) brace-start-points))
    (loop
      for instruction of-type instruction across program
      and position    of-type fixnum      from   0 by 1
      do
        (case instruction
          (:jump-forward-if-zero
            (push position bracket-start-points))
          (:jump-back-if-zero
            (if bracket-start-points
              (connect-jump-points jump-table
                (pop bracket-start-points)
                position)
              (error "Unmatched \"]\" instruction at position ~d."
                position)))
          (:jump-forward-if-not-zero
            (push position brace-start-points))
          (:jump-back-if-not-zero
            (if brace-start-points
              (connect-jump-points jump-table
                (pop brace-start-points)
                position)
              (error "Unmatched \"}\" instruction at position ~d."
                position)))
          (otherwise
            NIL))
      finally
        (when bracket-start-points
          (error "Unmatched \"[\" token~p at position~:p ~{~d~^, ~}."
            (length bracket-start-points) bracket-start-points))
        (when brace-start-points
          (error "Unmatched \"{\" token~p at position~:p ~{~d~^, ~}."
            (length brace-start-points) brace-start-points)))
    (the Jump-Table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table jump-point)
  "Returns the obverse point amenable to the JUMP-POINT in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     jump-point))
  (the fixnum
    (or (gethash jump-point (jump-table-connections jump-table))
        (error "No destination associated with the jump point ~d."
          jump-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type non-negative-integer +INITIAL-TAPE-CAPACITY+))

;;; -------------------------------------------------------

(defparameter +INITIAL-TAPE-CAPACITY+ 10
  "The incipial tally of cells allotted to the ``Tape'' class'
   capacity.")

;;; -------------------------------------------------------

(defstruct (Tape
  (:constructor make-tape ())
  (:copier      NIL))
  "The ``Tape'' class serves in the representation of the TriTape
   program memory, a tape-like vector of ternary digits, commencing with
   the index zero (0), endowed with a unilaterally infinite expansion
   along the positive axis, and operated upon by a cell pointer to whom
   the dever is apportioned to designated at any instant the currently
   active cell."
  (cells   (make-array +INITIAL-TAPE-CAPACITY+
             :element-type    'trit
             :initial-element 0
             :adjustable      NIL
             :fill-pointer    NIL)
           :type      (simple-array trit (*))
           :read-only NIL)
  (pointer 0
           :type      non-negative-integer
           :read-only NIL))

;;; -------------------------------------------------------

(defun ensure-tape-capacity (tape)
  "Ascertains the TAPE's capacity for accommodating its cell pointer's
   current location by contingently resizing the underlying array, in
   any case returning no value."
  (declare (type Tape tape))
  (when (< (length (tape-cells tape)) (tape-pointer tape))
    (setf (tape-cells tape)
      (adjust-array
        (tape-cells tape)
        (1+ (tape-pointer tape))
        :initial-element 0)))
  (values))

;;; -------------------------------------------------------

(defun cell-pointer-can-move-left-p (tape)
  "Determines whether the TAPE's cell pointer may be translated
   sinistrally by at least one step, which case applies if the same is
   not empight on the first cell at the index zero (0), returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (plusp (tape-pointer tape)))))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left, if possible,
   otherwise accompasses no causatum, in any case returning no value."
  (declare (type Tape tape))
  (when (cell-pointer-can-move-left-p tape)
    (decf (tape-pointer tape)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (ensure-tape-capacity tape)
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the ternary digit stored in the current TAPE cell."
  (declare (type Tape tape))
  (the trit
    (aref (tape-cells tape)
      (tape-pointer tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the current TAPE cell, contingently preceded
   by a wrapping around of the input into the admissive trit range
   [0, 2], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (aref (tape-cells tape)
      (tape-pointer tape))
    (wrap-into-trit-range new-value))
  (values))

;;; -------------------------------------------------------

(defun reset-current-cell (tape)
  "Resets the current TAPE cell's value to its default state of zero (0)
   and returns no value."
  (declare (type Tape tape))
  (setf (current-cell-value tape) 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
    (program
     &aux (jump-table (compute-jump-table program)))))
  "The ``Interpreter'' class implements an entity responsible for the
   execution of a TriTape program."
  (program     (error "Missing TriTape program.")
               :type      triTape-program
               :read-only T)
  (ip          0
               :type      fixnum
               :read-only NIL)
  (jump-table  (error "Missing jump table.")
               :type      Jump-Table
               :read-only T)
  (accumulator 0
               :type      trit
               :read-only NIL)
  (tape        (make-tape)
               :type      Tape
               :read-only T))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the program governed by the INTERPRETER's castaldy
   has achieved its patration, this being the case for the instruction
   pointer's (IP) residence beyond the admissible marches, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (array-in-bounds-p
        (interpreter-program interpreter)
        (interpreter-ip      interpreter)))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its maintained program, if possible, and returns no
   value."
  (declare (type Interpreter interpreter))
  (unless (program-completed-p interpreter)
    (incf (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-destination (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to
   contemporaneously occupy a forward jump instruction, relocates the
   same to the opposite end point, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (get-jump-destination
      (interpreter-jump-table interpreter)
      (interpreter-ip         interpreter)))
  (values))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction currently designated by the INTERPRETER's
   instruction pointer (IP), or signals an error of an unspecified type
   if the latter resides outside of the admissible marches."
  (declare (type Interpreter interpreter))
  (the instruction
    (aref
      (interpreter-program interpreter)
      (interpreter-ip      interpreter))))

;;; -------------------------------------------------------

(defun accumulator-value (interpreter)
  "Returns the trit stored in the INTERPRETER's accumulator."
  (declare (type Interpreter interpreter))
  (the trit
    (interpreter-accumulator interpreter)))

;;; -------------------------------------------------------

(defun (setf accumulator-value) (new-value interpreter)
  "Stores the NEW-VALUE in the INTERPRETER's accumulator, contingently
   wrapping the same around into the admissive integer range [0, 2] as a
   prevenient step to the transfer, and returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (setf (interpreter-accumulator interpreter)
    (wrap-into-trit-range new-value))
  (values))

;;; -------------------------------------------------------

(defun accumulator-contains-zero-p (interpreter)
  "Determines whether the INTERPRETER's accumulator contains the value
   zero (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (zerop
        (accumulator-value interpreter)))))

;;; -------------------------------------------------------

(defun copy-accumulator-to-tape (interpreter)
  "Transfers the INTERPRETER accumulator's value into its current tape
   cell and returns no value."
  (declare (type Interpreter interpreter))
  (setf (current-cell-value
          (interpreter-tape interpreter))
    (accumulator-value interpreter))
  (values))

;;; -------------------------------------------------------

(defun copy-tape-to-accumulator (interpreter)
  "Transfers the INTERPRETER tape's current cell value into its
   accumulator and returns no value."
  (declare (type Interpreter interpreter))
  (setf (accumulator-value interpreter)
    (current-cell-value
      (interpreter-tape interpreter)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Processes the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter)
            (instruction (eql :increment-accumulator)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (incf (accumulator-value interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :decrement-accumulator)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (decf (accumulator-value interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :move-pointer-right)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (move-cell-pointer-right (interpreter-tape interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :move-pointer-left)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (if (cell-pointer-can-move-left-p (interpreter-tape interpreter))
      (move-cell-pointer-left (interpreter-tape interpreter))
      (copy-accumulator-to-tape interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :copy-cell-to-accumulator)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (copy-tape-to-accumulator interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :reset-cell)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (reset-current-cell
      (interpreter-tape interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :add-accumulator-to-cell)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (incf
      (current-cell-value (interpreter-tape interpreter))
      (accumulator-value interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :input)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (setf (current-cell-value (interpreter-tape interpreter))
      (query-trit))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :output)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (format T "~d"
      (current-cell-value
        (interpreter-tape interpreter)))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :jump-forward-if-zero)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (when (accumulator-contains-zero-p interpreter)
      (jump-to-destination interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :jump-back-if-zero)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (unless (accumulator-contains-zero-p interpreter)
      (jump-to-destination interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :jump-forward-if-not-zero)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (unless (accumulator-contains-zero-p interpreter)
      (jump-to-destination interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :jump-back-if-not-zero)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (when (accumulator-contains-zero-p interpreter)
      (jump-to-destination interpreter))
    (values)))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Executes the TriTape program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-TriTape (code)
  "Interprets the piece of TriTape source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine which treats any non-zero input equivalent to one (1),
;; thus printing an infinite tally of twos (2) if such is committed.
(interpret-TriTape ",=[.].")

;;; -------------------------------------------------------

;; Truth-machine which treats any non-zero input as one (1), printing
;; even for the number two (2) the digit one (1).
;; 
;; If provided with a non-zero input, increments the same repeatedly
;; until it reaches the zero (0) state, subsequently increments a final
;; time to one (1), employs the epiphenomenal peculiarity of the
;; sinistral tape pointer translation which, for the first cell, copies
;; the accumulator into this first cell, and prints the cell according
;; to one's acquaintance.
(interpret-TriTape ",=[[^]^<.].")

;;; -------------------------------------------------------

;; Repeating numeric cat program which terminates on a user input equal
;; to zero (0).
(interpret-TriTape ",=[.,=]")

;;; -------------------------------------------------------

;; Print five times the digit one (1).
(interpret-TriTape
  "^<v
   >>>>>
   {^+.<v=}")
