;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "TACC", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 13th, 2023, the kenspeckle proprium of which wones
;; in its deployment of both an infinite tape of non-negative integers
;; and an accumulator for a scalar element from the same spectrum, their
;; coefficiency accompasses the parturition of its programs' operations,
;; their compass ensconces basic arithmetics, warklumes for character
;; input and output, conditional execution, and a label-based goto
;; control flow concept.
;; 
;; 
;; Concept
;; =======
;; The TACC programming language constitutes a specimen whose haecceity
;; proceeds from its dioristic twain of memory components: an infinite,
;; one-indexed cells of non-negative integers, and a scalar salvatory
;; for the same object type, the accumulator.
;; 
;; == TACC: [T]APE + [ACC]UMULATOR? ==
;; The acquisition of the language's agnomination, "TACC", most probably
;; registered its provenance in its data management's twissel, the
;; *T*ape and the *ACC*umulator, their champarty the vouchsafement of
;; the programs' effectivity.
;; 
;; == THE TAPE: AN INFINITE DISPANSION OF INTEGERS ==
;; That program memory constituent endowed with the most conspicle mete
;; in mickleness issues from the tape's contribution, itself a
;; unilaterally infinite expanse of cells, commencing its enumeration
;; from the first unit at the index one (1), but bournless towards the
;; upper march.
;; 
;; Every cell's capacity amounts to a non-negative integer object
;; greater than or equal to zero (0), at its inchoation assuming this
;; minimum extremum as the initial state.
;; 
;; At any instant a designated marker, the "cell pointer" (CP), selects
;; the currently active cell, the sole tape instance entalented with an
;; amenability to indagations and modulations. The cell pointer is
;; capacitated to admit to relocations by certain operations.
;; 
;; == THE ACCUMULATOR: A NON-NEGATIVE INTEGER SCALAR ==
;; A parhedral element of castaldy, the accumulator furnishes itself a
;; datum desumed from the tape's cell range, that is, a non-negative
;; integer of unbounded magnitude.
;; 
;; Empight on the initial value of zero (0) upon the program's
;; commencement, any attempt to reduce the accumulator alow the bottom
;; bourne will conclude in its reversal to the incipient state.
;; 
;; == TAPE AND ACCUMULATOR: COEFFICIENCY IN COLLABORATION ==
;; The champarty of the tape and the accumulator serves in the
;; amplification of the TACC programming language's competences.
;; 
;; == COMMANDS: ONE CHARACTER + OPTIONAL ARGUMENT ==
;; Every operation in TACC is nevened by an aefauld character, the same
;; may be, in some cases, succeeded by a singular argument.
;; 
;; The effective competences' amplectation is laid around the memory
;; management, the intercourse of its tape and accumulator, basic
;; arithmetics, input/output conduits, conditional skipping, as well as
;; a label-based goto mechanism.
;; 
;; 
;; Instructions
;; ============
;; The TACC programming language's instruction set tallies a membership
;; of eleven specimens, such appropriated to the tape management,
;; rudimentary arithmetics applied to the same or the accumulator, their
;; intercourse, input and output facilities, as well as a treble of
;; control flow mechanisms.
;; 
;; == OVERVIEW ==
;; An apercu's adhibition shall imbue the interested party with a
;; foundational mete of nortelry anenst TACC's operative features.
;; 
;; Please heed that placeholder sections are emphasized via a catena of
;; underlining asterisks ("*"), reserved for their supersession by
;; actual TACC code in a program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   : index | Relocates the cell pointer (CP) to the position
;;     ***** | designated by the {index}.
;;           |---------------------------------------------------------
;;           | {index} must be an integer number greater than or equal
;;           | to one (1).
;;   ..................................................................
;;   >       | Relocates the cell pointer (CP) to the position equal to
;;           | accumulator's value.
;;   ..................................................................
;;   <       | Copies the value of the tape cell designated by the cell
;;           | pointer (CP) to the accumulator.
;;   ..................................................................
;;   ^       | Copies the accumulator value to the tape cell designated
;;           | by the cell pointer (CP).
;;   ..................................................................
;;   + value | Increments the accumulator by the {value}.
;;     ***** |---------------------------------------------------------
;;           | {value} must be an integer number greater than or equal
;;           | to zero (0).
;;   ..................................................................
;;   - value | Decrements the accumulator by the {value}. If the new
;;     ***** | accumulator state would be negative, it is instead set
;;           | to zero (0).
;;           |---------------------------------------------------------
;;           | {value} must be an integer number greater than or equal
;;           | to zero (0).
;;   ..................................................................
;;   "       | Prints to the standard output the character whose ASCII
;;           | code corresponds to the accumulator's value.
;;   ..................................................................
;;   ;       | Queries the standard input for a character and stores
;;           | its ASCII code in the accumulator.
;;   ..................................................................
;;   ! guard | If the accumulator value does not equal the {guard},
;;     ***** | skips the next command; otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | {value} must be an integer number greater than or equal
;;           | to zero (0).
;;   ..................................................................
;;   [ label | Declares a label identified by the name {label} and
;;     ***** | associated with its position in the program for later
;;           | referral.
;;           |---------------------------------------------------------
;;           | {label} must be an identifier compact of one or more
;;           | Latin letters or decimal digits.
;;           |---------------------------------------------------------
;;           | Upon a cognominal label's existence, an error of the
;;           | type "DuplicateLabelError" is issued.
;;   ..................................................................
;;   ] label | Relocates the instruction pointer (IP) to the position
;;     ***** | associated with the label of the {label} name.
;;           |---------------------------------------------------------
;;           | {label} must be an identifier compact of one or more
;;           | Latin letters or decimal digits.
;;           |---------------------------------------------------------
;;           | Upon the requested label's lacuna, an error of the type
;;           | "UnknownLabelError" is issued.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-12-17
;; 
;; Sources:
;;   [esolang2023TACC]
;;   The Esolang contributors, "TACC", May 13th, 2023
;;   URL: "https://esolangs.org/wiki/TACC"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of predicate operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-entry-satisfies-p (hash-table predicate)
  "Determines whether every entry in the HASH-TABLE satisfies the
   PREDICATE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   The PREDICATE specifies a dyadic function whose inputs, the probed
   key and its affiliated value in this exact order, ought to produce
   a generalized boolean output, signifying with a non-``NIL'' result
   their eligibility, or via ``NIL'' their failure to fulfil their
   dever; the signature, as a corollary, amounts to:
     lambda (key value) => generalized-boolean"
  (declare (type hash-table         hash-table))
  (declare (type (function (* *) *) predicate))
  (the boolean
    (not (null
      (loop
        for    key of-type T being the hash-keys in hash-table
        using  (hash-value value)
        always (funcall predicate key value))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), but not restricted along its upper march,
   in corollary being an occupant of the integral range [0, +infinity}."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype index ()
  "The ``index'' type defines a cell index as a positive integer value
   not impounded by an upper bourne, and thus a commorant of the
   integral space [0, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' enumerates the recognized variation on TACC
   commands."
  '(member
    :set-cell-pointer-to-value          ;; :
    :set-cell-pointer-to-accumulator    ;; >
    
    :set-accumulator-to-current-cell    ;; <
    :set-current-cell-to-accumulator    ;; ^
    
    :increment-accumulator              ;; +
    :decrement-accumulator              ;; -
    
    :skip-command                       ;; !
    
    :output-character                   ;; "
    :input-character                    ;; ;
    
    :declare-label                      ;; [
    :go-to-label))                      ;; ]

;;; -------------------------------------------------------

(deftype command-argument ()
  "The ``command-argument'' type defines that species of objects
   admissible to their eomployment in a TACC command as an operand."
  '(or null non-negative-integer string))

;;; -------------------------------------------------------

(deftype argument-type ()
  "The ``argument-type'' type enumerates the possible species of
   arguments requisite for a command."
  '(member :none :integer :string))

;;; -------------------------------------------------------

(deftype tacc-program ()
  "The ``tacc-program'' type defines an executable TACC program as a
   vector composed of zero or more ``Command'' instances."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a value of the VALUE-TYPE, both defaulting to the generic ``*''
   sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (every-entry-satisfies-p
              (the hash-table candidate)
              #'(lambda (key value)
                  (declare (type T key))
                  (declare (type T value))
                  (and (typep key   key-type)
                       (typep value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   ilk includes, among other specimens, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition TACC-Error (error)
  ()
  (:documentation
    "The ``TACC-Error'' condition type establishes a common foundry for
     all errors bespoke to purposes of a TACC program's lexical
     analyzation, parsing, or interpretation."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (TACC-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        duplicate-label-error-offending-name
    :type          string
    :documentation "The label name whose repeated declaration has
                    instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Error condition))
      (declare (type destination           stream))
      (format stream "The label ~s has already been declared before."
        (duplicate-label-error-offending-name condition))))
  (:documentation
    "The ``Duplicate-Label-Error'' condition type's dedication relocates
     its to the particular encheson of a label declaration whose
     identifier has already been declared in the same program's previent
     position."))

;;; -------------------------------------------------------

(define-condition Unknown-Label-Error (TACC-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        unknown-label-error-offending-name
    :type          string
    :documentation "The label name whose lacuna during a goto request
                    has instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Unknown-Label-Error condition))
      (declare (type destination         stream))
      (format stream "The label ~s could not be found."
        (unknown-label-error-offending-name condition))))
  (:documentation
    "The ``Unknown-Label-Error'' condition type's onus comprehends the
     apprizal about the attempt to jump to a label whose name has not
     been defined in the program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value (generalized-boolean)
  "Returns for the GENERALIZED-BOOLEAN datum a purely ``boolean''
   object, answering to a non-``NIL'' input with ``T'', otherwise
   responding with ``NIL''."
  (declare (type T generalized-boolean))
  (the boolean
    (not (null generalized-boolean))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE subsumes into the species of
   whitespaces, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=))))

;;; -------------------------------------------------------

(defun label-character-p (candidate)
  "Determines whether the CANDIDATE permits its admission into a label
   name, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (alphanumericp candidate))))

;;; -------------------------------------------------------

(defun parse-command-type (token)
  "Returns the command type corresponding to the TOKEN, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type character token))
  (the command-type
    (case token
      (#\:       :set-cell-pointer-to-value)
      (#\>       :set-cell-pointer-to-accumulator)
      (#\<       :set-accumulator-to-current-cell)
      (#\^       :set-current-cell-to-accumulator)
      (#\+       :increment-accumulator)
      (#\-       :decrement-accumulator)
      (#\!       :skip-command)
      (#\"       :output-character)
      (#\;       :input-character)
      (#\[       :declare-label)
      (#\]       :go-to-label)
      (otherwise (error "No command token: \"~c\"." token)))))

;;; -------------------------------------------------------

(defun get-requisite-argument-type (command-type)
  "Returns for the COMMAND-TYPE the requisite type of argument for its
   patration."
  (declare (type command-type command-type))
  (the argument-type
    (case command-type
      (:set-cell-pointer-to-value       :integer)
      (:set-cell-pointer-to-accumulator :none)
      (:set-accumulator-to-current-cell :none)
      (:set-current-cell-to-accumulator :none)
      (:increment-accumulator           :integer)
      (:decrement-accumulator           :integer)
      (:skip-command                    :integer)
      (:output-character                :none)
      (:input-character                 :none)
      (:declare-label                   :string)
      (:go-to-label                     :string)
      (otherwise (error "Invalid command type: ~s." command-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (type &optional (argument NIL))))
  "The ``Command'' class accommodates a generic encapsulation of a TACC
   Language facility, compact of its discriminating type and an optional
   argument."
  (type     (error "Missing command type.")
            :type      command-type
            :read-only T)
  (argument NIL
            :type      command-argument
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-end (source start predicate)
  "Commencing from the START position into the SOURCE, returns the
   location of the first character which does not match the PREDICATE,
   or, upon its failure, responds with the SOURCE's length."
  (declare (type string                   source))
  (declare (type fixnum                   start))
  (declare (type (function (character) *) predicate))
  (the fixnum
    (or (position-if-not predicate source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces, and returns the position into
   the SOURCE of the first non-whitespace character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (locate-end source start #'whitespace-character-p)))

;;; -------------------------------------------------------

(defun read-integer (source start)
  "Proceeding from the START position into the SOURCE, reads an unsigned
   decimal integer literal, and returns two values:
     (1) The detected and parsed non-negative integer object.
     (2) The position into the SOURCE immediately succeeding the segment
         entailing the consumed integer content."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values non-negative-integer fixnum)
    (parse-integer source :start start :end
      (locate-end source start #'digit-char-p))))

;;; -------------------------------------------------------

(defun validate-label-name-length (probed-label-name)
  "Determines whether the PROBED-LABEL-NAME's is valid, that is, the
   same entails at least one character, returning on confirmation the
   PROBED-LABEL-NAME itself, otherwise signals an error of an
   unspecified type."
  (declare (type string probed-label-name))
  (the string
    (or (and (plusp (length probed-label-name))
             probed-label-name)
        (error "The label name is empty."))))

;;; -------------------------------------------------------

(defun read-label-name (source start)
  "Proceeding from the START position into the SOURCE, reads a label
   name, and returns two values:
     (1) The detected label name as a string.
     (2) The position into the SOURCE immediately succeeding the
         consumed label name segment."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (let ((end (locate-end source start #'label-character-p)))
      (declare (type fixnum end))
      (values
        (validate-label-name-length
          (subseq source start end))
        end))))

;;; -------------------------------------------------------

(defun read-command-type (source start)
  "Proceeding from the START position into the SOURCE, reads a command
   identifier and returns two values:
     (1) The ``command-type'' corresponding to the command name.
     (2) The position into the source immediately succeeding the
         consumed command name."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values command-type fixnum)
    (values
      (parse-command-type
        (char source start))
      (1+ start))))

;;; -------------------------------------------------------

(defun read-command (source start)
  "Proceeding from the START position into the SOURCE, reads a TACC
   operation and returns two values:
     (1) An appropriate ``Command'' representation of the consumed
         command.
     (2) The position in the SOURCE succeeding the consumed command,
         contingently skipping subsequent whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((command-type     NIL)
        (command-argument NIL)
        (position         start))
    (declare (type fixnum position))
    ;; Skip whitespaces and read the COMMAND-TYPE.
    (multiple-value-setq (command-type position)
      (read-command-type source
        (skip-whitespaces source position)))
    ;; Set the COMMAND-ARGUMENT, if necessary.
    (case (get-requisite-argument-type command-type)
      (:none
        NIL)
      (:integer
        (multiple-value-setq (command-argument position)
          (read-integer source
            (skip-whitespaces source position))))
      (:string
        (multiple-value-setq (command-argument position)
          (read-label-name source
            (skip-whitespaces source position))))
      (otherwise
        (error "Invalid requisite argument type: ~s."
          (get-requisite-argument-type command-type))))
    (the (values Command fixnum)
      (values
        (make-command command-type command-argument)
        (skip-whitespaces source position)))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of TACC SOURCE code and returns a vector of its
   commands."
  (declare (type string source))
  (let ((position (skip-whitespaces source 0)))
    (declare (type fixnum position))
    (flet ((consume-command (command end-position)
            "Returns the COMMAND, while concomitantly updating the
             POSITION cursor to the END-POSITION, contingently skipping
             subsequent whitespaces."
            (declare (type Command command))
            (declare (type fixnum  end-position))
            (the Command
              (prog1 command
                (setf position
                  (skip-whitespaces source end-position))))))
      (the tacc-program
        (coerce
          (loop while (< position (length source)) collect
            (multiple-value-call #'consume-command
              (read-command source
                (skip-whitespaces source position))))
          '(simple-array Command (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((labels
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string fixnum)
    :documentation "Maps each recognized label name to its position in
                    the TACC program."))
  (:documentation
    "The ``Label-Table'' class assumes the castaldy of labels, their
     definitions encompassing the vinculum betwixt an identifier and its
     position in a TACC program."))

;;; -------------------------------------------------------

(defun define-label (labels name position)
  "Registers the label designated by the NAME with the POSITION in the
   LABELS table and returns no value."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (declare (type fixnum      position))
  (setf
    (gethash name
      (slot-value labels 'labels))
    position)
  (values))

;;; -------------------------------------------------------

(defun contains-label-p (labels name)
  "Determines whether the LABELS table entails a label amenable to the
   NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (get-boolean-value
    (nth-value 1
      (gethash name
        (slot-value labels 'labels)))))

;;; -------------------------------------------------------

(defun get-label-position (labels name)
  "Returns the position in the TACC program bearing the label NAME as
   registered at the LABELS table, or signals an error of the type
   ``Unknown-Label-Error'' upon its disrespondency."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (the fixnum
    (or (gethash name
          (slot-value labels 'labels))
        (error 'Unknown-Label-Error :offending-name name))))

;;; -------------------------------------------------------

(defun build-label-table (program)
  "Supputates and returns for the TACC PROGRAM the label table and
   returns the same."
  (declare (type tacc-program program))
  (let ((labels (make-instance 'Label-Table)))
    (declare (type Label-Table labels))
    (loop
      for command  of-type Command across program
      for position of-type fixnum  from   0 by 1
      when (eq (command-type command) :declare-label) do
        (let ((label-name (command-argument command)))
          (declare (type string label-name))
          (if (contains-label-p labels label-name)
            (error 'Duplicate-Label-Error :offending-name label-name)
            (define-label labels label-name position))))
    (the Label-Table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape tape.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of index non-negative-integer)
    :documentation "A sparse vector of integer-valued cells, amenable to
                    positive integer indices.")
   (pointer
    :initform      1
    :accessor      cell-pointer
    :type          index
    :documentation "The cell pointer, which selects at any instant the
                    currently active cell by adminiculum of its index,
                    the key into the CELLS table."))
  (:documentation
    "The ``Tape'' class serves as an encapsulation of the program
     tape in the form of a sparse vector, realized via hash table
     whose positive integer keys map to signed integer cell values."))

;;; -------------------------------------------------------

(defun current-cell (tape)
  "Returns the number stored in the TAPE cell at its cell pointer."
  (declare (type Tape tape))
  (the non-negative-integer
    (gethash
      (slot-value tape 'pointer)
      (slot-value tape 'cells)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value tape)
  "Stores the NEW-VALUE in the TAPE cell at the cell pointer and
   returns no value."
  (declare (type non-negative-integer new-value))
  (declare (type Tape                 tape))
  (setf
    (gethash
      (slot-value tape 'pointer)
      (slot-value tape 'cells)
      0)
    new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing TACC program.")
    :reader        get-program
    :type          tacc-program
    :documentation "The TACC Program to execute.")
   (instruction-pointer
    :initform      0
    :accessor      instruction-pointer
    :type          fixnum
    :documentation "The instruction pointer (IP) location, which
                    designates the currently active command in the
                    PROGRAM.")
   (labels
    :initform      (make-instance 'Label-Table)
    :reader        get-labels
    :type          Label-Table
    :documentation "Associates the recognized label names with their
                    zero-based positions into the PROGRAM.")
   (tape
    :initform      (make-instance 'Tape)
    :accessor      get-tape
    :type          Tape
    :documentation "The memory tape, a vector of infinite extent along
                    the positive axis, indexed starting with one (1),
                    where each cell embraces a scalar integer object.")
   (accumulator
    :initform      0
    :accessor      accumulator
    :type          non-negative-integer
    :documentation "The integer-valued accumulator."))
  (:documentation
    "The ``Interpreter'' class attends to the application of causata to
     an executable TACC program's commands."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'labels)
    (build-label-table
      (get-program interpreter)))
  (values))

;;; -------------------------------------------------------

(defun get-program-size (interpreter)
  "Returns the number of commands constituting the INTERPRETER's
   program."
  (declare (type Interpreter interpreter))
  (the non-negative-integer
    (length
      (get-program interpreter))))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in its program, if possible, returns no value."
  (declare (type Interpreter interpreter))
  (setf
    (instruction-pointer interpreter)
    (min
      (1+ (instruction-pointer interpreter))
      (get-program-size interpreter)))
  (values))

;;; -------------------------------------------------------

(defun go-to-label (interpreter label-name)
  "Relocates the INTERPRETER's instruction pointer (IP) to the position
   associated with the LABEL-NAME and returns no value."
  (declare (type Interpreter interpreter))
  (setf
    (instruction-pointer interpreter)
    (get-label-position
      (get-labels interpreter)
      label-name))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the program maintained by the INTERPRETER is
   completed, which means that its desinent command has been processed,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value
      (>= (instruction-pointer interpreter)
          (get-program-size    interpreter)))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command selected by the INTERPRETER's instruction pointer
   (IP)."
  (declare (type Interpreter interpreter))
  (the Command
    (aref
      (get-program         interpreter)
      (instruction-pointer interpreter))))

;;; -------------------------------------------------------

(defgeneric dispatch-command (interpreter command-type command)
  (:documentation
    "Processes the COMMAND, identified and dispatched by its COMMAND, in
     the INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defun process-command (interpreter command)
  "Processes the COMMAND in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (dispatch-command interpreter
    (command-type command)
    command)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :set-cell-pointer-to-value))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (setf
    (cell-pointer
      (get-tape interpreter))
    (command-argument command))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :set-cell-pointer-to-accumulator))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (declare (ignore            command))
  (setf
    (cell-pointer
      (get-tape interpreter))
    (accumulator interpreter))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :set-accumulator-to-current-cell))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (declare (ignore            command))
  (setf
    (accumulator interpreter)
    (current-cell
      (get-tape interpreter)))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :set-current-cell-to-accumulator))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (declare (ignore            command))
  (setf
    (current-cell
      (get-tape interpreter))
    (accumulator interpreter))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :increment-accumulator))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (incf
    (accumulator interpreter)
    (command-argument command))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :decrement-accumulator))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (setf (accumulator interpreter)
    (max 0
      (- (accumulator interpreter)
         (command-argument command))))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :skip-command))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (when (/= (accumulator interpreter)
            (command-argument command))
    (advance-ip interpreter))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :output-character))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (declare (ignore            command))
  (format T "~c"
    (code-char
      (accumulator interpreter)))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :input-character))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (declare (ignore            command))
  (format T "~&>> ")
  (finish-output)
  (setf
    (accumulator interpreter)
    (char-code
      (read-char)))
  (clear-input)
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :declare-label))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (declare (ignore            command))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command
    ((interpreter  Interpreter)
     (command-type (eql :go-to-label))
     (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-type command-type))
  (declare (ignore            command-type))
  (declare (type Command      command))
  (go-to-label interpreter
    (command-argument command))
  (values))

;;; -------------------------------------------------------

(defun execute-interpreter (interpreter)
  "Executes the program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-TACC (code)
  "Interprets the piece of TACC source CODE and returns no value."
  (declare (type string code))
  (execute-interpreter
    (make-instance 'Interpreter :program
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-TACC
  "[a
   ;
   \"
   ]a")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-TACC
  "
  ;
  [print
  \"
  !49
  ]print
  ")

;;; -------------------------------------------------------

;; Print all ASCII character in the code range [0, 255].
(interpret-TACC
  "[repeat
   \"
   +1
   !256
   ]terminate
   ]repeat
   [terminate")

;;; -------------------------------------------------------

;; Print "Hello, World!" using jump-based iteration and the gremial
;; coefficiency partaken of by the tape and the accumulator.
;; 
;; Tape layout:
;;   tape[1...13] <- "Hello, World!"
;;   tape[14]     <- current index into tape[1...13]
;; 
(interpret-TACC
  "
  :1 +72 ^
  :2 +29 ^
  :3 +7  ^
  :4     ^
  :5 +3  ^
  :6 -67 ^
  :7 -12 ^
  :8 +55 ^
  :9 +24 ^
  :10 +3 ^
  :11 -6 ^
  :12 -8 ^
  :13 -67 ^

  -32
  :14
  ^

  [repeat
  >
  <
  \"

  :14
  <
  +1
  ^

  !14
  ]end
  ]repeat
  [end
  ")
