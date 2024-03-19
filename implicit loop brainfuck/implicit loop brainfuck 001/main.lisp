;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "implicit loop brainfuck", invented by the Esolang user
;; "Transoptimal" and presented on January 1st, 2023, entalented with
;; such proprium as to modulate and extend Urban Mueller's "brainfuck"
;; by jump-based control structures that refer to the immediately
;; succeeding sink point, rather than the compeer delineated by a
;; matching nesting level.
;; 
;; 
;; Concept
;; =======
;; A descendent from brainfuck's concepts, implicit loop brainfuck
;; deploys an instruction set compact of eleven operations, each such
;; identifier by an aefauld symbol, and operating on a bilaterally
;; unbounded tape of octet-valued cells.
;; 
;; == IMPLICIT LOOP BRAINFUCK EMPLOYS A KENSPECKLE JUMPING CONCEPT ==
;; Appropriating most of its stock-father's operations, which comprises
;; the cell state arithmetics, cell pointer management, and input/output
;; facilities, a counterdistinguishing signum wones in its discrepancy
;; in the control flow management, the same refers to the veridical
;; next jump destination, in lieu of that defined by the correct nesting
;; level, for the instruction pointer's relocation.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTES ==
;; In a mode identical to brainfuck, the memory's componency builds upon
;; a bilaterally infinite tape of unsigned-byte valued cells, responding
;; to transgression of its valid integer range of [0, 255] with a
;; wrapping around along its marches. A cell pointer designates at any
;; instant in the program the currently active unit, the sole entity
;; amenable to perquisitions and alterations. Instructions exist to
;; translate the pointer along the tape.
;; 
;; == PROGRAMS OPERATE IN AN IMPLICIT PERPETUAL LOOP ==
;; Ligated into a strong lealty to its agnomination, an implicit loop
;; brainfuck program operates in a tatic iteration, naturally perpetual
;; in its cycles, relocating the instruction pointer (IP), ensuing from
;; the code's patration, to the sinistral laterality for an iterum
;; procession.
;; 
;; An aefauld warklume's dation permits this samsara's desinence, the
;; "#" operation, its competence the immediate program conclusion.
;; 
;; 
;; Instructions
;; ============
;; implicit loop brainfuck's instruction set accounts for an undecimal
;; cardinality's governance in its competences, desuming a preponderance
;; of sextuple members from its brainfuck entheus, while the quintuple
;; remnants, appertaining to the control flow conduction, either
;; experiences a modulation from the extant provenance, with a treble's
;; novelty.
;; 
;; == OVERVIEW ==
;; A cursory ilk of gnarity's dation with respect to the language's
;; operative competences shall be the following apercu's dever:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one. Upon its upper
;;           | bourne's transgression, which is imposed by the value
;;           | 255, the cell state relapses to the lower extremum of
;;           | zero (0).
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's instruction set.
;;   ..................................................................
;;   -       | Decrements the current cell value by one. Upon its lower
;;           | bourne's transgression, which is imposed by the value
;;           | zero (0), the cell state wraps around to the upper
;;           | extremum of 255.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's instruction set.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's instruction set.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's instruction set.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's instruction set.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's instruction set.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the next --- not the
;;           | necessarily matching --- "]" token, contingently
;;           | wrapping around at the program's dextral bourne and
;;           | continuing the search at its inchoation, if the "]"
;;           | token precedes this "[" instruction's position.
;;           | If the current cell value does not contain zero (0),
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | Note that, given this command's diorism in behavior, one
;;           | "]" can serve as the jump destination of multiple "["
;;           | instructions.
;;           |---------------------------------------------------------
;;           | If no matching "]" point could be found, an error of the
;;           | type "MissingJumpDestinationError" is signaled.
;;           |---------------------------------------------------------
;;           | This operation diverges from brainfuck's notion in that
;;           | the stock-father jumps to the "]" instruction on a
;;           | matching nesting level, and always expects its
;;           | destination to be strictly right of the "[" point.
;;   ..................................................................
;;   ]       | Does not contribute any effect, except in serving as a
;;           | jump destination for the immediately prevenient "["
;;           | instruction.
;;           |---------------------------------------------------------
;;           | This operation diverges from brainfuck's notion in that
;;           | it does not entail the potential of the program flow
;;           | redirection.
;;   ..................................................................
;;   {       | If the current cell value does not equal (0), moves the
;;           | instruction pointer (IP) forward to the next --- not the
;;           | necessarily matching --- "}" token, contingently
;;           | wrapping around at the program's dextral bourne and
;;           | continuing the search at its inchoation, if the "}"
;;           | token precedes this "{" instruction's position.
;;           | If the current cell value contains zero (0), proceeds as
;;           | usual.
;;           |---------------------------------------------------------
;;           | Note that, given this command's diorism in behavior, one
;;           | "}" can serve as the jump destination of multiple "{"
;;           | instructions.
;;           |---------------------------------------------------------
;;           | If no matching "}" point could be found, an error of the
;;           | type "MissingJumpDestinationError" is signaled.
;;           |---------------------------------------------------------
;;           | This operation is forinsecal to brainfuck.
;;   ..................................................................
;;   }       | Does not contribute any effect, except in serving as a
;;           | jump destination for the immediately prevenient "{"
;;           | instruction.
;;           |---------------------------------------------------------
;;           | This operation is forinsecal to brainfuck.
;;   ..................................................................
;;   #       | Terminates the program immediately.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-18
;; 
;; Sources:
;;   [esolang2024implicitloopbrainfuck]
;;   The Esolang contributors, "implicit loop brainfuck",
;;     January 12th, 2024
;;   URL: "https://esolangs.org/wiki/Implicit_loop_brainfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&key (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates a tally of zero or more entries, each siccan jumelle an
   affiliation of a key and a value, the former assumes the KEY-TYPE,
   while the latter complies to the VALUE-TYPE, and for both holds the
   default of the comprehensive ``T''."
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

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   implicit loop brainfuck operations."
  '(member
    ;; Original brainfuck instructions:
    :increment-current-cell
    :decrement-current-cell
    :move-cell-pointer-left
    :move-cell-pointer-right
    :input-character
    :output-character
    ;; Adscititious implicit loop brainfuck instructions:
    :jump-if-zero-source
    :jump-if-zero-destination
    :jump-if-not-zero-source
    :jump-if-not-zero-destination
    :halt-execution))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' associates the recognized implicit loop
   brainfuck command identifiers with the representative instructions,
   harnessing for such endeavor a hash table's services, the keys
   therein imposing the identifier characters, and the values the
   ``instruction'' tantamounts."
  '(hash-table-of :key-type character :value-type instruction))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an implicit loop brainfuck as a
   one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping betwixt jump start points
   and the subsequent destinations, both represented positions into the
   respective implicit loop brainfuck, realized in a hash table which
   associates ``fixnum'' start points with ``fixnum'' destinations."
  '(hash-table-of :key-type fixnum :value-type fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, and thus a commorant of the closed integeral interval
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines a sparse vector of unsigned byte-valued
   cells, infinite in its extent and amenable to signed integer indices,
   and realized as a hash table whose keys contribute the integer cell
   indices, associated with ``octet'' cell states."
  '(hash-table-of :key-type integer :value-type octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism of which includes, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Implicit-Loop-Brainfuck-Error (error)
  ()
  (:documentation
    "The ``Implicit-Loop-Brainfuck-Error'' condition type establishes a
     command basis for all erroneous transpiration communication
     facilities operating in relation to an implicit loop brainfuck
     program."))

;;; -------------------------------------------------------

(define-condition Missing-Jump-Destination-Error
  (Implicit-Loop-Brainfuck-Error)
  ((source-position
    :initarg       :source-position
    :initform      (error "Missing source position.")
    :reader        missing-jump-destination-error-source-position
    :type          fixnum
    :documentation "The zero-indexed location of the jump source.")
   (source-command
    :initarg       :source-command
    :initform      (error "Missing source instruction.")
    :reader        missing-jump-destination-error-source-command
    :type          instruction
    :documentation "The jump source instruction.")
   (destination-command
    :initarg       :destination-command
    :initform      (error "Missing destination command.")
    :reader        missing-jump-destination-error-destination-command
    :type          instruction
    :documentation "The missing jump destination command."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Jump-Destination-Error condition))
      (declare (type destination                    stream))
      (format stream "Missing destination point \"~a\" for the ~
                      jump source instruction \"~a\" at position ~d."
        (missing-jump-destination-error-destination-command condition)
        (missing-jump-destination-error-source-command      condition)
        (missing-jump-destination-error-source-position    condition))))
  (:documentation
    "The ``Missing-Jump-Destination-Error'' condition type serves in the
     communication of an anomalous situation instigated by the attempt
     to search for or relocate to a jump destination from a source point
     in an implicit loop brainfuck program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'eql)
  "Associates the recognized command tokens with representative
   instruction objects.")

;;; -------------------------------------------------------

(flet ((register-identifier (token instruction)
        "Associates the command TOKEN with the INSTRUCTION in the
         +IDENTIFIERS+ table, superseding any contingent entry with the
         former as its key, and returns no value."
        (declare (type character   token))
        (declare (type instruction instruction))
        (setf (gethash token +IDENTIFIERS+) instruction)
        (values)))
  (register-identifier #\+ :increment-current-cell)
  (register-identifier #\- :decrement-current-cell)
  (register-identifier #\> :move-cell-pointer-right)
  (register-identifier #\< :move-cell-pointer-left)
  (register-identifier #\, :input-character)
  (register-identifier #\. :output-character)
  (register-identifier #\[ :jump-if-zero-source)
  (register-identifier #\] :jump-if-zero-destination)
  (register-identifier #\{ :jump-if-not-zero-source)
  (register-identifier #\} :jump-if-not-zero-destination)
  (register-identifier #\# :halt-execution)
  (values))

;;; -------------------------------------------------------

(defun instruction-token-p (candidate)
  "Determines whether the CANDIDATE represents an implicit loop
   brainfuck instruction, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (nth-value 1
        (gethash candidate +IDENTIFIERS+))))))

;;; -------------------------------------------------------

(defun parse-instruction (token)
  "Returns the instruction representation affiliated with the TOKEN, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type character token))
  (the instruction
    (or (and (instruction-token-p token)
             (gethash token +IDENTIFIERS+))
        (error "No instruction token: \"~c\"." token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of implicit loop brainfuck source CODE its
   instructions and returns a one-dimensional simple array
   representation thereof."
  (declare (type string code))
  (the program
    (coerce
      (loop
        for token of-type character across code
        when (instruction-token-p token)
          collect (parse-instruction token))
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-jump-destination (program
                                start
                                source-instruction
                                destination-instruction)
  "Proceeding from the START position into the implicit loop brainfuck
   PROGRAM, seeks the jump DESTINATION-INSTRUCTION, matching the probed
   SOURCE-INSTRUCTION, contingently wrapping around at the dextral
   bourne, and returns its location in the PROGRAM; or, if the
   desideratum eluded its discovery, signals an error of the type
   ``Missing-Jump-Destination-Error''."
  (declare (type program     program))
  (declare (type fixnum      start))
  (declare (type instruction destination-instruction))
  (let ((number-of-commands (length program)))
    (declare (type fixnum number-of-commands))
    (the fixnum
      (loop
        repeat number-of-commands
        for position
          of-type fixnum
          =       start
          then    (mod (1+ position) number-of-commands)
        for instruction
          of-type instruction
          =       (aref program position)
        when (eq instruction destination-instruction) do
          (return position)
        finally
          (error 'Missing-Jump-Destination-Error
            :source-position     position
            :source-command      source-instruction
            :destination-command destination-instruction)))))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table source destination)
  "Associates the jump SOURCE with the DESTINATION in the JUMP-TABLE in
   a unilateral mode and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     source))
  (declare (type fixnum     destination))
  (setf (gethash source jump-table) destination)
  (values))

;;; -------------------------------------------------------

(defun calculate-jump-table (program)
  "Supputates and returns for the implicit loop brainfuck PROGRAM a jump
   table, the same connects the jump source instructions to the nearest
   following destination operations via their positions in the PROGRAM,
   contingently wrapping around the dextral march in their probing."
  (declare (type program program))
  (let ((jump-table (make-hash-table :test #'eql)))
    (declare (type jump-table jump-table))
    (loop
      for instruction of-type instruction across program
      and position    of-type fixnum      from   0 by 1
      if (eq instruction :jump-if-zero-source) do
        (connect-jump-points jump-table position
          (locate-jump-destination program position
            :jump-if-zero-source
            :jump-if-zero-destination))
      else if (eq instruction :jump-if-not-zero-source) do
        (connect-jump-points jump-table position
          (locate-jump-destination program position
            :jump-if-not-zero-source
            :jump-if-not-zero-destination)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table source)
  "Returns the jump destination associated with the SOURCE position in
   the JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     source))
  (the fixnum
    (or (gethash source jump-table)
        (error "No jump destination associated with the position ~d."
          source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class serves in the representation of an infinite
   dispansion of unsigned byte-valued cells, operated upon by a mobile
   cell pointer."
  (tape    (make-hash-table :test #'eql)
           :type      tape
           :read-only T)
  (pointer 0
           :type      integer
           :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementaton of interpreter.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
    (program &aux (jump-table (calculate-jump-table program)))))
  "The ``Interpreter'' class is apportioned the dever of accompassing
   actual effect to an implicit loop brainfuck program."
  (program    (error "Missing program.")
              :type      program
              :read-only T)
  (jump-table (error "Missing jump table.")
              :type      jump-table
              :read-only T)
  (ip         0
              :type      fixnum
              :read-only NIL)
  (memory     (make-memory)
              :type      Memory
              :read-only T)
  (halted-p   NIL
              :type      boolean
              :read-only NIL))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program, contingently wrapping around if transcending
   the dextral bourne, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (mod
      (1+ (interpreter-ip interpreter))
      (length (interpreter-program interpreter))))
  (values))

;;; -------------------------------------------------------

(defun jump-to-destination (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the next
   jump destination and returns no value."
  (declare (type Interpreter interpreter))
  (setf
    (interpreter-ip interpreter)
    (get-jump-destination
      (interpreter-jump-table interpreter)
      (interpreter-ip         interpreter)))
  (values))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction at the instruction pointer's (IP) location
   in the INTERPRETER's program."
  (declare (type Interpreter interpreter))
  (the instruction
    (aref
      (interpreter-program interpreter)
      (interpreter-ip      interpreter))))

;;; -------------------------------------------------------

(defun current-cell-value (interpreter)
  "Returns the unsigned byte valued stored in the INTERPRETER memory's
   current cell."
  (declare (type Interpreter interpreter))
  (the octet
    (gethash
      (memory-pointer (interpreter-memory interpreter))
      (memory-tape    (interpreter-memory interpreter))
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value interpreter)
  "Stores the NEW-VALUE in the INTERPRETER memory's current cell,
   contingently preceded by a wrapping around of its state in order to
   respect the unsigned byte range of [0, 255], and returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (setf
    (gethash
      (memory-pointer (interpreter-memory interpreter))
      (memory-tape    (interpreter-memory interpreter))
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (interpreter)
  "Determines whether the INTERPRETER memory's current cell contains the
   value zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (zerop (current-cell-value interpreter))))))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (interpreter)
  "Translates the INTERPRETER memory's cell pointer one step to the
   right and returns no value."
  (declare (type Interpreter interpreter))
  (incf (memory-pointer (interpreter-memory interpreter)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (interpreter)
  "Translates the INTERPRETER memory's cell pointer one step to the left
   and returns no value."
  (declare (type Interpreter interpreter))
  (decf (memory-pointer (interpreter-memory interpreter)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :increment-current-cell)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (incf (current-cell-value interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :decrement-current-cell)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (decf (current-cell-value interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :move-cell-pointer-right)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (move-cell-pointer-right interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :move-cell-pointer-left)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (move-cell-pointer-left interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :input-character)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (format T "~&>> ")
  (finish-output)
  (setf
    (current-cell-value interpreter)
    (char-code (read-char)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :output-character)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (write-char
    (code-char
      (current-cell-value interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :jump-if-zero-source)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (when (current-cell-contains-zero-p interpreter)
    (jump-to-destination interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :jump-if-zero-destination)))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :jump-if-not-zero-source)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (unless (current-cell-contains-zero-p interpreter)
    (jump-to-destination interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :jump-if-not-zero-destination)))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction (eql :halt-execution)))
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (setf (interpreter-halted-p interpreter) T)
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the implicit loop brainfuck program maintained by the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-halted-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-implicit-loop-brainfuck (code)
  "Interprets the piece of implicit loop brainfuck source CODE and
   returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on an input of the
;; "null character".
(interpret-implicit-loop-brainfuck ",{#}.")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-implicit-loop-brainfuck
  "{,.
   ------------------------------------------------
   {#}
   ++++++++++++++++++++++++++++++++++++++++++++++++.
   ------------------------------------------------")

;;; -------------------------------------------------------

;; Query for a character and counts down from its ASCII code to
;; inclusive zero (0), printing each corresponding character.
(interpret-implicit-loop-brainfuck "{,}.-{#")
