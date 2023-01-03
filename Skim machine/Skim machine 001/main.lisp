;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Skim machine", presented by the Esolang user "A" in the
;; year 2019, and serving as a substitution of the Minsky machine by
;; operating on non-negative integer-valued accumulators, or registers,
;; which can be incremented or decremented, engaged in coefficiency with
;; a conditional goto instruction based on their value.
;; 
;; 
;; Concept
;; =======
;; The Skim machine programming language provides a reformulated
;; equivalent of the Minsky machine, operating on a contingency of
;; infinite accumulators, or registers, each storing a single
;; non-negative integer. Very rudimentary already by the circumference
;; of its stock-father's cleronomy, the language permits only a gradual
;; incrementing or decrementing of its registers, as well as a
;; conditional goto or jump facility. These properties, however, serve
;; to elevate the Skim machine to a plenipotentiary state with the
;; unassuming, yet puissant provenance of its heredity.
;; 
;; == ACCUMULATORS: REGISTERS OF NON-NEGATIVE INTEGERS ==
;; The program memory's constituents are enumerated by a contingently
;; infinite tally of registers, known as "accumulators", each such
;; identified unambiguously by a name string, compact of alphanumeric
;; characters or the underscore ("_"), with the unit serving as a
;; salvatory to a scalar non-negative integer, initialized with the zero
;; (0) value.
;; 
;; An accumulator responds to gradual incrementing and decrementing,
;; concomitantly serving in its state of zero (0) as a goto operation's
;; instigator, transpiring upon a decrement attempt applied to a
;; zero-valued unit. This kenspeckle abortive response to the register's
;; reduction obviates a descent into the negative space.
;; 
;; == A SKIM MACHINE'S FACULTIES: INCREASE, REDUCE, OR JUMP ==
;; The Skim machine's indulgence in any effective activity is pinioned
;; by a curtailment into two alternatives: incrementing the accumulator
;; value by one or decrementing it by an equinumerant amount. A
;; particular deviation occurs in the second facility upon a collision
;; with the special case of a zero-valued accumulator: In lieu of the
;; reduction, an instruction pointer relocation to a specified line is
;; imposed.
;; 
;; 
;; Syntax
;; ======
;; Skim machine programs are stated in a linewise fashion, with each
;; non-empty row accommodating tenancy to a single instruction, either
;; of the "INC" or "JZDEC" variant.
;; 
;; == INSTRUCTIONS ==
;; An instruction always occupies a single line of its own, preceded
;; with optional spaces, but identified by the command type "INC" or
;; "JZDEC". The mandatory first argument, being the accumulator name, is
;; separated via one or more spaces, following which the space
;; characters are rendered optional constituents. The second argument,
;; if necessary, represents the target line for a goto operation, and is
;; segregated by a comma from its previous peer.
;; 
;; == ACCUMULATOR NAMES ==
;; The accumulator name's diorism proceeds by means of one or more
;; alphanumeric characters or underscores "_".
;; 
;; == LINE NUMBERS ==
;; Line numbers are stated as signed decimal integers.
;; 
;; == EMPTY LINES ==
;; Lines containing no character, or exclusively spaces, are homologated
;; at any position. Their insignificance extends into the bailiwick of
;; line numbering, where their existence does not contribute to the
;; tally.
;; 
;; == SPACES ==
;; The incorporation of spaces betwixt the instruction type and the
;; first operand constitutes an object of obligation. At any other
;; installment, these characters' participation is neglected.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation applies to
;; the language:
;; 
;;   program            := [ linebreaks ]
;;                      ,  [ programLine ]
;;                      ,  { linebreaks , programLine }
;;                      ,  [ linebreaks ]
;;                      ;
;;   programLine        := emptyLine | commandLine ;
;;   commandLine        := optionalSpaces
;;                      ,  ( incInstruction | jzdecInstruction )
;;                      ,  optionalSpaces
;;                      ;
;;   emptyLine          := optionalSpaces ;
;;   incInstruction     := "INC" , mandatorySpaces , accumulatorName ;
;;   jzdecInstruction   := "JZDEC"
;;                      ,  mandatorySpaces
;;                      ,  accumulatorName
;;                      ,  argumentSeparator
;;                      ,  integer
;;                      ;
;;   accumulatorName    := nameCharacter, { nameCharacter } ;
;;   nameCharacter      := letter | digit | "_" ;
;;   letter             := "a" | ... | "z" | "A" | ... | "Z" ;
;;   integer            := [ "+" | "-" ] , digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   linebreaks         := linebreak , { linebreak } ;
;;   linebreak          := "\n" ;
;;   argumentSeparator  := optionalSpaces , "," , optionalSpaces ;
;;   optionalSpaces     := { space } ;
;;   mandatorySpaces    := space , { space } ;
;;   space              := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; A single twain of operations exhausts the Skim machine instruction
;; set, with "INC" incrementing a specified accumulator, whereas "JZDEC"
;; encompasses a twifaced contribution: either decrementing a positive
;; accumulator, or changing to another line for a zero-valued entity.
;; The set's parvipotent appearance, tectly embracing the Minsky
;; machine's faculties, in corollary deceives the veridical potential.
;; 
;; == OVERVIEW ==
;; The following apercu shall be steadable for a cursory acquaintance
;; with the Skim machine instructions:
;; 
;;   ------------------------------------------------------------------
;;   Command             | Effect
;;   --------------------+---------------------------------------------
;;   INC   {acc}         | Increments the accumulator designated by the
;;                       | name {acc} by one, and lets the instruction
;;                       | pointer proceed onward.
;;   ..................................................................
;;   JZDEC {acc}, {line} | If the value of the accumulator specified by
;;                       | the name {acc} equals zero (0), the
;;                       | instruction pointer is relocated to the
;;                       | zero-based line index {line}, with empty
;;                       | lines not contributing to the numbering.
;;                       | Otherwise, the accumulator {acc} is
;;                       | decremented by one, and the instruction
;;                       | pointer simply proceeds onward.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-01-01
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Skim_machine"
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
  "The ``command'' type enumerates the recognized instruction types."
  '(member :INC :JZDEC))

;;; -------------------------------------------------------

(deftype accumulator-name ()
  "The ``accumulator-name'' type defines an object eligible as an
   accumulator's identifier in terms of a string."
  'string)

;;; -------------------------------------------------------

(deftype jump-target ()
  "The ``jump-target'' type defines an object eligible as a reference to
   a destination line in a \"JZDEC\" instruction in terms of either the
   ``NIL'' value or a signed integer, with the former pertaining to a
   variant for the \"INC\" operation that does not rely on this
   attribute."
  '(or null integer))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type defines a Skim machine instruction in the
   form of a list, either composed of two or three elements, with the
   first constituting a ``command'' identification of the instruction
   type, whereas the second mandatory item relates to an accumulator
   name, whereas the optional third attribute references a jump target."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (>= (length (the list candidate)) 2)
            (destructuring-bind (first-element
                                 &optional (second-element NIL)
                                           (third-element  NIL))
                (the list candidate)
              (declare (type T first-element))
              (declare (type T second-element))
              (declare (type T third-element))
              (and
                (typep first-element  'command)
                (typep second-element 'accumulator-name)
                (typep third-element  'jump-target))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype skim-machine-program ()
  "The ``skim-machine-program'' defines a Skim machine program as a
   vector of zero or more instructions."
  '(vector instruction))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines integer objects greater
   than or equal to zero, but unbounded towards the upper laterality,
   paravaunt useful as accumulator values."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype accumulator-set ()
  "The ``accumulator-set'' type defines a set of accumulators in the
   form of hash table, associating the ``accumulator-name'' identifiers
   with the respective ``non-negative-integer'' values, thus permitting
   a sparse representation."
  '(hash-table-of accumulator-name non-negative-integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Checks whether the CANDIDATE represents a space character, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Checks whether the CANDIDATE represents a word constituent, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (find candidate "-+" :test #'char=))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Scanner".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Scanner ()
  ((source
    :initarg       :source
    :initform      NIL
    :accessor      scanner-source
    :type          (or null string)
    :documentation "The source code line to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current location into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current LOCATION into the
                    SOURCE."))
  (:documentation
    "The ``Scanner'' class serves as a specialized tokenizer which
     extracts from a single string line the optated objects, including
     command identifiers, signed integer numbers, commas, and spaces."))

;;; -------------------------------------------------------

(defun make-scanner ()
  "Creates and returns a new ``Scanner'' without a source."
  (the Scanner (make-instance 'Scanner)))

;;; -------------------------------------------------------

(defun scanner-set-source (scanner new-source)
  "Registers the NEW-SOURCE at the SCANNER, resets the SCANNER's state,
   and returns it."
  (declare (type Scanner scanner))
  (declare (type string  new-source))
  (with-slots (source position character) scanner
    (declare (type (or null string)    source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf source   new-source)
    (setf position 0)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun scanner-advance (scanner)
  "Moves the SCANNER's position cursor to the next character, if
   possible, updates the internal state, and returns the modified
   SCANNER."
  (declare (type Scanner scanner))
  (with-slots (source position character) scanner
    (declare (type (or null string)    source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun scanner-skip-spaces (scanner)
  "Starting at the current position into the SCANNER' source, skips a
   sequence of zero or more adjacent spaces, and returns the modified
   SCANNER."
  (declare (type Scanner scanner))
  (with-slots (character) scanner
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (scanner-advance scanner)))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun scanner-end-of-line-p (scanner)
  "Checks whether the SCANNER's source has been exhausted, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Scanner scanner))
  (the boolean
    (not (null
      (or (null (slot-value scanner 'source))
          (null (slot-value scanner 'character)))))))

;;; -------------------------------------------------------

(defun scanner-read-word (scanner)
  "Starting at the current position into the SCANNER's source, reads a
   word as a coherent unit demarcated to its dextral side by a non-word
   character, and returns the same as a string."
  (declare (type Scanner scanner))
  (with-slots (character) scanner
    (declare (type (or null character) character))
    (the string
      (with-output-to-string (word)
        (declare (type string-stream word))
        (loop while (and character (word-character-p character)) do
          (write-char character word)
          (scanner-advance scanner))))))

;;; -------------------------------------------------------

(defun scanner-read-identifier (scanner)
  "Starting at the current position into the SCANNER's source, extracts
   and returns an instruction type as a ``command'' object.
   ---
   If no command identifier could be recognized, an error of an
   unspecified type is signaled."
  (declare (type Scanner scanner))
  (let ((word (scanner-read-word scanner)))
    (declare (type string word))
    (the command
      (cond
        ((string= word "INC")   :INC)
        ((string= word "JZDEC") :JZDEC)
        (T (error "Invalid identifier: ~s." word))))))

;;; -------------------------------------------------------

(defun scanner-read-accumulator-name (scanner)
  "Starting at the current position into the SCANNER's source, extracts
   and returns a string representing an accumulator identifier."
  (declare (type Scanner scanner))
  (the string
    (scanner-read-word scanner)))

;;; -------------------------------------------------------

(defun scanner-read-number (scanner)
  "Starting at the current position into the SCANNER's source, extracts
   and returns a potentially signed integer number."
  (declare (type Scanner scanner))
  (the integer
    (parse-integer
      (scanner-read-word scanner))))

;;; -------------------------------------------------------

(defun scanner-expect-comma (scanner)
  "Expects the character at the current position into the SCANNER's
   source to constitute a comma (\",\"), on confirmation consuming the
   same and returning the modified SCANNER, otherwise signaling an error
   of an unspecified type."
  (declare (type Scanner scanner))
  (with-slots (character) scanner
    (declare (type (or null character) character))
    (if (and character (char= character #\,))
      (scanner-advance scanner)
      (error "Expected a comma, but encountered ~s." character)))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun scanner-expect-end-of-line (scanner)
  "Checks whether the SCANNER's source, starting at its current
   position, contains either no content or exclusively spaces, on
   confirmation relocating the position cursor to the end of the source
   and returning the modified SCANNER, otherwise signaling an error of
   an unspecified type."
  (declare (type Scanner scanner))
  (with-slots (character position) scanner
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (loop while character do
      (if (space-character-p character)
        (scanner-advance scanner)
        (error "Expected end of line, but encountered ~s at ~
                position ~d."
          character position))))
  (the Scanner scanner))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction (type accumulator
                         &optional (target NIL))
  "Creates and returns a new ``instruction'' belonging to the TYPE,
   with the mandatory ACCUMULATOR identifier and an optional TARGET for
   an instruction capable of redirecting the instruction pointer."
  (declare (type command          type))
  (declare (type accumulator-name accumulator))
  (declare (type jump-target      target))
  (the instruction (list type accumulator target)))

;;; -------------------------------------------------------

(defun instruction-type (instruction)
  "Returns the INSTRUCTION type."
  (declare (type instruction instruction))
  (the command (first instruction)))

;;; -------------------------------------------------------

(defun instruction-accumulator (instruction)
  "Returns the identifier of the INSTRUCTION's accumulator reference."
  (declare (type instruction instruction))
  (the accumulator-name (second instruction)))

;;; -------------------------------------------------------

(defun instruction-target (instruction)
  "Returns the target line associated with the INSTRUCTION, if defined
   for it; otherwise, signals an error of an unspecified type."
  (declare (type instruction instruction))
  (the jump-target (third instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-INC-instruction (scanner command)
  "Parses the accumulator identifier of an \"INC\"-type instruction by
   utilizing the SCANNER and the already detected ``:INC'' command
   identifier, and returns an ``instruction'' representation thereof."
  (declare (type Scanner scanner))
  (declare (type command command))
  (let ((accumulator NIL))
    (declare (type (or null accumulator-name) accumulator))
    (scanner-skip-spaces scanner)
    (setf accumulator (scanner-read-accumulator-name scanner))
    (scanner-expect-end-of-line scanner)
    (the instruction (make-instruction command accumulator))))

;;; -------------------------------------------------------

(defun parse-JZDEC-instruction (scanner command)
  "Parses the arguments of a \"JZDEC\"-type instruction by utilizing the
   SCANNER and the already detected ``:JZDEC'' command identifier, and
   returns an ``instruction'' representation thereof."
  (declare (type Scanner scanner))
  (declare (type command command))
  (let ((accumulator NIL)
        (target      NIL))
    (declare (type (or null accumulator-name) accumulator))
    (declare (type jump-target                target))
    (scanner-skip-spaces scanner)
    (setf accumulator (scanner-read-accumulator-name scanner))
    (scanner-skip-spaces  scanner)
    (scanner-expect-comma scanner)
    (scanner-skip-spaces  scanner)
    (setf target (scanner-read-number scanner))
    (scanner-expect-end-of-line scanner)
    (the instruction (make-instruction command accumulator target))))

;;; -------------------------------------------------------

(defun parse-instructions (code)
  "Parses the piece of Skim machine CODE and returns a one-dimensional
   simple array of its instructions."
  (declare (type string code))
  (let ((instructions NIL)
        (scanner      (make-scanner)))
    (declare (type (list-of instruction) instructions))
    (declare (type Scanner               scanner))
    (with-input-from-string (code-stream code)
      (declare (type string-stream code-stream))
      (loop
        for line
          of-type (or null string)
          =       (read-line code-stream NIL NIL)
        while line
        do
          (scanner-set-source  scanner line)
          ;; Skip potential leading spaces on the line.
          (scanner-skip-spaces scanner)
          ;; If the line was not empty, search for an instruction.
          (unless (scanner-end-of-line-p scanner)
            (let ((command (scanner-read-identifier scanner)))
              (declare (type command command))
              (case command
                (:INC
                  (push
                    (parse-INC-instruction scanner command)
                    instructions))
                (:JZDEC
                  (push
                    (parse-JZDEC-instruction scanner command)
                    instructions))
                (otherwise
                  (error "Invalid command name: ~s." command)))))))
    (the (simple-array instruction (*))
      (coerce
        (nreverse instructions)
        '(simple-array instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          skim-machine-program
    :documentation "The Skim machine instructions to execute.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP), designating the
                    currently processed item among the INSTRUCTIONS.")
   (accumulators
    :initarg       :accumulators
    :initform      (make-hash-table :test #'equal)
    :type          accumulator-set
    :documentation "Maintains the accumulators, with each hash table
                    key representing the string identifier, associated
                    with the non-negative integer value."))
  (:documentation
    "The ``Interpreter'' class applies itself to the induction of actual
     effect into a sequence of Skim machine instructions."))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' which operates on the
   Skim machine INSTRUCTIONS."
  (declare (type skim-machine-program instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-advance-ip (interpreter)
  "Moves the INTERPRETER's instruction pointer to the next instruction,
   if possible, and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip) interpreter
    (declare (type skim-machine-program instructions))
    (declare (type fixnum               ip))
    (when (< ip (length instructions))
      (incf ip)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-move-ip-to (interpreter new-position)
  "Relocates the INTERPRETER's instruction pointer to the NEW-POSITION
   and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-position))
  (with-slots (instructions ip) interpreter
    (declare (type skim-machine-program instructions))
    (declare (type fixnum               ip))
    (setf ip
      (cond
        ((minusp new-position)
          0)
        ((>= new-position (length instructions))
          (length instructions))
        (T
          new-position))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-current-instruction (interpreter)
  "Returns the instruction under the INTERPRETER's instruction pointer,
   or ``NIL'' if having progressed beyond the available operations."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip) interpreter
    (declare (type skim-machine-program instructions))
    (declare (type fixnum               ip))
    (the (or null instruction)
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip)))))

;;; -------------------------------------------------------

(defun interpreter-ensure-accumulator (interpreter accumulator)
  "Ascertains the existence of an accumulator with the identifier
   ACCUMULATOR in the INTERPRETER by registering it with the default
   value of zero upon its absence, returning in any case the
   INTERPRETER."
  (declare (type Interpreter      interpreter))
  (declare (type accumulator-name accumulator))
  (with-slots (accumulators) interpreter
    (declare (type accumulator-set accumulators))
    (unless (nth-value 1 (gethash accumulator accumulators))
      (setf (gethash accumulator accumulators) 0)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-increment-accumulator (interpreter accumulator)
  "Increments the value of the ACCUMULATOR registered at the INTERPRETER
   by one and returns the modified INTERPRETER."
  (declare (type Interpreter      interpreter))
  (declare (type accumulator-name accumulator))
  (incf
    (gethash accumulator
      (slot-value interpreter 'accumulators) 0))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-decrement-accumulator (interpreter accumulator)
  "Decrements the value of the ACCUMULATOR registered at the INTERPRETER
   by one and returns the modified INTERPRETER.
   ---
   An error of an unspecified type is signaled if the ACCUMULATOR value
   would become negative by applying this operation."
  (declare (type Interpreter      interpreter))
  (declare (type accumulator-name accumulator))
  (with-slots (accumulators) interpreter
    (declare (type accumulator-set accumulators))
    (if (plusp (gethash accumulator accumulators 0))
      (decf (gethash accumulator accumulators 0))
      (error "Cannot increment the accumulator ~s, ~
              as its value of ~d is less than or equal to zero."
        accumulator (gethash accumulator accumulators 0))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-accumulator-zero-p (interpreter accumulator)
  "Checks whether the ACCUMULATOR maintained by the INTERPRETER contains
   a value of zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter      interpreter))
  (declare (type accumulator-name accumulator))
  (interpreter-ensure-accumulator interpreter accumulator)
  (the boolean
    (not (null
      (zerop
        (gethash accumulator
          (slot-value interpreter 'accumulators) 0))))))

;;; -------------------------------------------------------

(defun interpreter-accumulator-plus-p (interpreter accumulator)
  "Checks whether the ACCUMULATOR maintained by the INTERPRETER contains
   a positive number, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter      interpreter))
  (declare (type accumulator-name accumulator))
  (interpreter-ensure-accumulator interpreter accumulator)
  (the boolean
    (not (null
      (plusp
        (gethash accumulator
          (slot-value interpreter 'accumulators) 0))))))

;;; -------------------------------------------------------

(defun interpreter-accumulator-value (interpreter accumulator)
  "Returns the value of the ACCUMULATOR governed by the INTERPRETER."
  (declare (type Interpreter      interpreter))
  (declare (type accumulator-name accumulator))
  (interpreter-ensure-accumulator interpreter accumulator)
  (the non-negative-integer
    (gethash accumulator
      (slot-value interpreter 'accumulators) 0)))

;;; -------------------------------------------------------

(defun interpreter-print-accumulators (interpreter)
  "Prints the accumulators maintained by INTERPRETER to the standard
   output, conforming to no particular order, and returns the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (maphash
    #'(lambda (accumulator-name accumulator-value)
        (declare (type accumulator-name     accumulator-name))
        (declare (type non-negative-integer accumulator-value))
        (format T "~&~a = ~d" accumulator-name accumulator-value)
        (values))
    (slot-value interpreter 'accumulators))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the INTERPRETER's instructions, printing at the end of the
   program its accumulators, and returns no value."
  (declare (type Interpreter interpreter))
  (loop
    for instruction
      of-type (or null instruction)
      =       (interpreter-current-instruction interpreter)
    while instruction
    do
      (case (instruction-type instruction)
        (:INC
          (interpreter-increment-accumulator interpreter
            (instruction-accumulator instruction))
          (interpreter-advance-ip interpreter))
        
        (:JZDEC
          (let ((accumulator (instruction-accumulator instruction)))
            (declare (type accumulator-name accumulator))
            
            (cond
              ;; Accumulator value equals zero?
              ;; => Jump to target line (= second argument).
              ((interpreter-accumulator-zero-p interpreter accumulator)
                (interpreter-move-ip-to interpreter
                  (instruction-target instruction)))
              
              ;; Accumulator value is greater than zero?
              ;; => Decrement accumulator by one.
              ((interpreter-accumulator-plus-p interpreter accumulator)
                (interpreter-decrement-accumulator interpreter
                  accumulator)
                (interpreter-advance-ip interpreter))
              
              ;; Accumulator value is negative?
              ;; => Invalid state.
              (T
                (error "Invalid value for accumulator ~d: ~d."
                  accumulator
                  (interpreter-accumulator-value interpreter
                    accumulator))))))
        
        (otherwise
          (error "Unrecognized instruction: ~s at position ~d."
            instruction (slot-value interpreter 'ip)))))
  
  (interpreter-print-accumulators interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpret-Skim-machine (code)
  "Interprets the piece of Skim machine CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parse-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add the numbers 2 and 4, the former stored in the "augend"
;; accumulator, the latter in the "addend", and increment the "augend"
;; to the sum of 6.
;; 
;; Concept:
;; 
;;   ------------------------------------------------------------------
;;   Line # | Instruction      | Purpose 
;;   -------+------------------+---------------------------------------
;;   0, 1   | INC augend       | Sets the AUGEND register to 2.
;;   ..................................................................
;;   2--5   | INC addend       | Sets the ADDEND register to 4.
;;   ..................................................................
;;   6      | JZDEC skip, 8    | Prevents the supernumerary first
;;          |                  | augmentation of the AUGEND register,
;;          |                  | which would consequentially amount to
;;          |                  | an erroneous sum of 7 for 2 + 4.
;;   ..................................................................
;;   7      | INC augend       | The actual addition step which applies
;;          |                  | repeatedly to increase the AUGEND to
;;          |                  | 6 (= 2 + 4), while decreasing the
;;          |                  | ADDEND to zero.
;;   ..................................................................
;;   8      | JZDEC addend, 10 | Ascertains that the addition process
;;          |                  | is halted for an exhausted ADDEND of
;;          |                  | zero by skipping the next line (#9)
;;          |                  | and terminating the program.
;;   ..................................................................
;;   9      | JZDEC return, 7  | Repeated invokes the addition line
;;          |                  | (#7) by jumping using the always
;;          |                  | zero-valued RETURN register. The
;;          |                  | preceding line (#8) realizes the
;;          |                  | addition process' termination based
;;          |                  | upon the ADDEND's value.
;;   ------------------------------------------------------------------
(interpret-Skim-machine
  "
  INC augend
  INC augend
  
  INC addend
  INC addend
  INC addend
  INC addend
  
  JZDEC skip, 8
  INC augend
  
  JZDEC addend, 10
  JZDEC return, 7
  ")
