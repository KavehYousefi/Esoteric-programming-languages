;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "NABC", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 17th, 2023, the diorism of which resides in its
;; instructions' representation via the tallies of the participating
;; "a", "b", and "c" symbols, encoding in this format also the operation
;; arguments, while invested the pursuit of an integer stack and a
;; scalar integral accumulator's manipulation.
;; 
;; 
;; Concept
;; =======
;; The NABC programming language employs an encoding of its instructions
;; in idiographic tallies of the letters "a", "b", and "c", utilized in
;; the manipulation of both a scalar integer accumulator and a stack
;; providing a manifold of the same numeric base.
;; 
;; == PROGRAMS: LINES OF "a", "b", "c" WEFTAGES ==
;; An NABC program's composition ensues from a sequence of zero or more
;; lines, each such comprehending an arbitrary tally of instructions,
;; the same entail both the identification and all contingent arguments
;; in the account of the partaking "a", "b", and "c" symbols, with the
;; "d" entity in a latreutical agency for the operations' demarcation
;; among others.
;; 
;; == THE ACCUMULATOR: A SINGLE INTEGER ==
;; The less intricate moiety among the program memory's twifold
;; componency, the accumulator serves a register whose capacity
;; homologates a singular object only, this being a signed integer of no
;; natural bournes along any axis.
;; 
;; == THE STACK: MANY INTEGERS ==
;; That memory parcel enjoyment the paravaunt gravity among the twissel
;; manifests in the stack, a contingently infinitely capacitated
;; last-in first-out salvatory for signed integer elements commorant in
;; any polarity and mickleness.
;; 
;; == STACK AND ACCUMULATOR: CHAMPARTY AS POTENTIAL ==
;; The coefficiency of the scalar accumulator and the complex stack by
;; their data intercourse accrues further amplification in the NABC
;; language's competences.
;; 
;; 
;; Syntax
;; ======
;; A syntactical conspectuity's acquisition limns an NABC program's
;; guise as a catena of lines, each such composed of zero or more
;; instructions, their componency realized by combinations of the
;; Latin minuscules "a", "b", and "c"; with the sepiment "d" as a
;; requisite intersticial avail for an operation twain's segregation.
;; 
;; == INSTRUCTIONS ==
;; Any instruction's woning is defined in terms of a line, contingently
;; shared with its neighbor by a separating "d" token. The identity and
;; the arguments of an operation are both encoded in the number of "a",
;; "b", and "c" letters partaking of its composition, with their
;; distribution a thing of neglect.
;; 
;; Any other character except for the triad of minuscules and the
;; spacing entities is inflicted with the gravity of interdiction.
;; 
;; == WHITESPACES ==
;; Spaces and horizontal tabs enjoy a tolerance's adhibition, whereas
;; linebreaks thole a peisant role in the agency of each two code lines'
;; division.
;; 
;; == COMMENTS ==
;; The current language iteration's contingency for comments registers
;; a lacuna.
;; 
;; == GRAMMAR ==
;; A formulation compliant with the Extended Backus-Naur Form (ENBF)
;; stipulations shall administer a more formal tier to the language's
;; donet:
;; 
;;   program     := { line } ;
;;   line        := [ instruction , { "d" , [ instruction ] } ] ;
;;   instruction := { "a" } , { "b" } , { "c" } ;
;; 
;; 
;; Instructions
;; ============
;; NABC's instruction set is governed by a cardinality of 15 members,
;; the gamut of their dever extending across the accumulator and stack
;; management, their intercourse, basic arithmetics, input and output
;; communications, as well as several control flow mechanisms.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be a cursory mete of gnarity's
;; adhibition concerning the language's operative contingency:
;; 
;;   ------------------------------------------------------------------
;;   No. a | No. b | No. c | Effect
;;   ------+-------+-------+-------------------------------------------
;;     0   |   0   |   0   | Resets the accumulator to the default
;;         |       |       | state of zero (0).
;;   ..................................................................
;;     1   |   2   |   x   | Increments the accumulator by {x}.
;;   ..................................................................
;;     2   |   1   |   x   | Decrements the accumulator by {x}.
;;   ..................................................................
;;     5   |   4   |   1   | Pops the top stack element and increments
;;         |       |       | the accumulator by the same.
;;   ..................................................................
;;     5   |   3   |   2   | Pops the top stack element and decrements
;;         |       |       | the accumulator by the same.
;;   ..................................................................
;;     3   |   1   |   0   | Pushes the accumulator value unto the
;;         |       |       | stack.
;;   ..................................................................
;;     0   |   2   |   x   | Pushes the number {x} unto the stack.
;;   ..................................................................
;;     2   |   4   |   0   | Exchanges the positions of the two top
;;         |       |       | stack elements.
;;   ..................................................................
;;     4   |   2   |   0   | Reverses the stack elements' order.
;;   ..................................................................
;;     5   |   5   |   0   | Duplicates the top stack element.
;;   ..................................................................
;;     3   |   0   |   1   | Queries the standard input for a character
;;         |       |       | and pushes its ASCII code unto the stack.
;;   ..................................................................
;;     4   |   0   |   0   | Pops the top stack element and prints the
;;         |       |       | character whose ASCII code matches this
;;         |       |       | numeric value to the standard output.
;;   ..................................................................
;;     3   |   4   |   3   | Pops the top stack element, "a", and the
;;         |       |       | new top stack element, "b"; if "a" does
;;         |       |       | not equal "b", skips the next instruction;
;;         |       |       | otherwise proceeds as usual.
;;   ..................................................................
;;     9   |   x   |   y   | Pops the top stack element; if it equals
;;         |       |       | the value {x}, relocates the instruction
;;         |       |       | pointer (IP) to the instruction at the
;;         |       |       | zero-based index {y}; otherwise proceeds
;;         |       |       | as usual.
;;   ..................................................................
;;     0   |   3   |   x   | Relocates the instruction pointer (IP) to
;;         |       |       | the first instruction on the line amenable
;;         |       |       | to the zero-based index {x}.
;;   ------------------------------------------------------------------
;; 
;; A warklume for facilitation in the discussion and deployment of the
;; operative instruments, for each of the 15 instructions exists a
;; mnemonic:
;; 
;;   ------------------------------------------------------------------
;;   No. a | No. b | No. c | Mnemonic
;;   ------+-------+-------+-------------------------------------------
;;     0   |   0   |   0   | set
;;   ..................................................................
;;     1   |   2   |   x   | inc x
;;   ..................................................................
;;     2   |   1   |   x   | dec x
;;   ..................................................................
;;     5   |   4   |   1   | add
;;   ..................................................................
;;     5   |   3   |   2   | sub
;;   ..................................................................
;;     3   |   1   |   0   | psa
;;   ..................................................................
;;     0   |   2   |   x   | psh x
;;   ..................................................................
;;     2   |   4   |   0   | swp
;;   ..................................................................
;;     4   |   2   |   0   | rev
;;   ..................................................................
;;     5   |   5   |   0   | dup
;;   ..................................................................
;;     3   |   0   |   1   | inp
;;   ..................................................................
;;     4   |   0   |   0   | out
;;   ..................................................................
;;     3   |   4   |   3   | neq
;;   ..................................................................
;;     9   |   x   |   y   | peq x y
;;   ..................................................................
;;     0   |   3   |   x   | jmp x
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation's development has been exercised in the
;; programming language Common Lisp, most conspicable in its investments
;; imparted with a simple pattern matching facility for the NABC
;; identifier patterns' recognition, and their subsequent transformation
;; into instruction objects.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-05-09
;; 
;; Sources:
;;   [esolang2023NABC]
;;   The Esolang contributors, "NABC", May 21st, 2023
;;   URL: "https://esolangs.org/wiki/NABC"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, everichon among these subsumes into the ELEMENT-TYPE, for
   the same holds the comprehensive default of ``T''."
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

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an assocation list, or
   alist, composed of zero or more entries, each key of which conforms
   to the KEY-TYPE and answers to a value of the VALUE-TYPE, both
   defaulting to the comprehensive ``T''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral object greater
   than or equal to zero (0), but unbridled by any upper march, and thus
   a commorant of the interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype tally ()
  "The ``tally'' type defines the expected cardinality of a letter in a
   pattern as either a concrete non-negative integer or the sentinel
   keyword symbol \"x\", the same avers the homologation of any
   account's admission."
  '(or non-negative-integer
       (eql :x)))

;;; -------------------------------------------------------

(deftype operand ()
  "The ``operand'' type defines a instruction operand as an optional
   entity, either assuming a non-negative integral form or the ``NIL''
   sentinel as an intimation of its absence."
  '(or null non-negative-integer))

;;; -------------------------------------------------------

(deftype opcode ()
  "The ``opcode'' type enumerates the recognized instruction types."
  '(member
    :inc
    :dec
    :psh
    :inp
    :psa
    :swp
    :peq
    :rev
    :dup
    :add
    :sub
    :set
    :neq
    :jmp
    :out))

;;; -------------------------------------------------------

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines a one-dimensional simple
   array composed of zero or more ``Instruction'' objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype line-vector ()
  "The ``line-vector'' type defines a one-dimensional simple array
   composed of zero or more ``Code-Line'' objects."
  '(simple-array Code-Line (*)))

;;; -------------------------------------------------------

(deftype program-stack ()
  "The ``program-stack'' type defines an NABC program's stack via an
   ordered list of bourneless signed integer objects."
  '(list-of integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean representation of the OBJECT, returning for a
   non-``NIL'' input the ``boolean'' ``T'' value, otherwise, for an
   OBJECT that conflates with ``NIL'', the ``NIL'' constant."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction
    (type &optional (first-operand NIL) (second-operand NIL))))
  "The ``Instruction'' class bailiwick comprises the encapsulation of an
   NABC operation, amplecting in this compass a maximum of two optional
   arguments."
  (type           (error "Missing instruction type.")
                  :type      opcode
                  :read-only T)
  (first-operand  NIL
                  :type      operand
                  :read-only T)
  (second-operand NIL
                  :type      operand
                  :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction vector operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction-vector (instructions)
  "Creates and returns an ``instruction-vector'' comprehending the
   communicated INSTRUCTIONS in their specified order."
  (declare (type (list-of Instruction) instructions))
  (the instruction-vector
    (coerce instructions
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Code-Line".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Code-Line
  "The ``Code-Line'' class' vindication proceeds from the castaldy of
   one or more NABC instructions whose shared commorancy accounts for a
   single line in the program."
  (instructions               (error "Missing instructions.")
                              :type      instruction-vector
                              :read-only T)
  (concludes-with-separator-p (error "Missing conclusion flag.")
                              :type      boolean
                              :read-only T))

;;; -------------------------------------------------------

(defun valid-code-line-index-p (line index)
  "Determines whether the INDEX refers to a valid location inside of the
   code LINE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Code-Line line))
  (declare (type fixnum    index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (code-line-instructions line)
        index))))

;;; -------------------------------------------------------

(defun get-code-line-instruction (line index)
  "Returns the instruction located at the INDEX into the CODE line, or
   signals an error of an unspecified type upon the subscript's
   infraction of the admissible bournes."
  (declare (type Code-Line line))
  (declare (type fixnum    index))
  (the Instruction
    (aref
      (code-line-instructions line)
      index)))

;;; -------------------------------------------------------

(defun get-code-line-length (line)
  "Returns the number of instruction having received a commorancy on
   this code LINE."
  (declare (type Code-Line line))
  (the fixnum
    (length
      (code-line-instructions line))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code line vector operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-line-vector (lines)
  "Creates and returns a ``line-vector'' comprehending the communicated
   LINES in their specified order."
  (declare (type (list-of Code-Line) lines))
  (the line-vector
    (coerce lines
      '(simple-array Code-Line (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((lines
    :initarg       :lines
    :initform      (error "Missing lines.")
    :reader        get-lines
    :type          line-vector
    :documentation "Comprehends the lines of instructions.")
   (line-number
    :initform      0
    :accessor      line-number
    :type          fixnum
    :documentation "The index of the current line in the LINES vector.")
   (ip
    :initform      0
    :accessor      ip
    :type          fixnum
    :documentation "The index of the current instruction in the current
                    line among the LINES, the latter is designated by
                    the LINE-NUMBER."))
  (:documentation
    "The ``Program'' class attends to the castaldy of a parsed NABC
     program, including in its avails the code lines and an instruction
     pointer (IP) whose responsibility comprises the traversal of the
     former component's operations."))

;;; -------------------------------------------------------

(defun make-program (lines)
  "Creates and returns a fresh NABC ``Program'' dedicated to the code
   LINES' evaluation."
  (declare (type line-vector lines))
  (the Program
    (make-instance 'Program :lines lines)))

;;; -------------------------------------------------------

(defun validate-line-continuations (program)
  "Determines whether the PROGRAM's code lines bear betwixt each twissel
   a separator (\"d\"), returning on confirmation the PROGRAM itself;
   otherwise signals an error of an unspecified type."
  (declare (type Program program))
  (loop
    for current-line
      of-type (or null Code-Line)
      across  (slot-value program 'lines)
    for line-index
      of-type fixnum
      from    0
      by      1
    for last-line-p
      of-type boolean
      =       (get-boolean-value-of
                (>= line-index
                    (1- (length (slot-value program 'lines)))))
    when (and (not last-line-p)
              (not (code-line-concludes-with-separator-p current-line)))
      do
        (error "Missing separator betwixt statement line at index ~d ~
                and the subsequent row."
          line-index))
  (the Program program))

;;; -------------------------------------------------------

(defun get-current-line (program)
  "Returns the NABC PROGRAM's current line, or ``NIL'' line counter has
   processed beyond the PROGRAM's bournes."
  (declare (type Program program))
  (with-slots (lines line-number) program
    (declare (type line-vector lines))
    (declare (type fixnum      line-number))
    (the (or null Code-Line)
      (when (array-in-bounds-p lines line-number)
        (aref lines line-number)))))

;;; -------------------------------------------------------

(defun current-line-exhausted-p (program)
  "Determines whether the PROGRAM's instruction pointer (IP) has
   advanced beyond the current line's desinent instruction, which
   renders the line exhausted, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (let ((current-line (get-current-line program)))
    (declare (type (or null Code-Line) current-line))
    (the boolean
      (get-boolean-value-of
        (or (null current-line)
            (not (valid-code-line-index-p current-line
                   (ip program))))))))

;;; -------------------------------------------------------

(defun program-exhausted-p (program)
  "Determines whether the PROGRAM is exhausted, which is satisfied if
   the instruction pointer (IP) has traversed beyond the desinent
   instruction of the ultimate code line, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (the boolean
    (null (get-current-line program))))

;;; -------------------------------------------------------

(defun advance-program (program)
  "Advances the PROGRAM's instruction pointer (IP) to the next position
   in its operation sequence, contingently progressing to the subsequent
   line if necessary, and returns no value."
  (declare (type Program program))
  (incf (ip program))
  (when (current-line-exhausted-p program)
    (incf (line-number program))
    (setf (ip program) 0))
  (values))

;;; -------------------------------------------------------

(defun find-line-for-instruction (program new-instruction-index)
  "Locates the zero-based index of the code line inside of the NABC
   PROGRAM ensconcing the NEW-INSTRUCTION-INDEX, and returns two values:
     (1) The zero-based index of the code line holding the instruction
         designated by the zero-based NEW-INSTRUCTION-INDEX, or, upon
         its transgression of the PROGRAM's bournes, responds with the
         tally of code lines in the same.
     (2) The zero-based index of the instruction in the line located
         in the first return value (see (1) aboon) tantamount to the
         absolute position targeted by the zero-based
         NEW-INSTRUCTION-INDEX, or, upon its failure, the value
         zero (0)."
  (declare (type Program program))
  (declare (type fixnum  new-instruction-index))
  (the (values fixnum fixnum)
    (loop
      for current-line-number
        of-type fixnum
        from    0
        by      1
      for current-instruction-index
        of-type fixnum
        =       0
        then    (+ current-instruction-index current-line-length)
      for  current-line
        of-type Code-Line
        across  (get-lines program)
      for current-line-length
        of-type fixnum
        =       (get-code-line-length current-line)
      when (< new-instruction-index
              (+ current-instruction-index current-line-length)) do
        (return
          (values current-line-number
            (- new-instruction-index current-instruction-index)))
      finally
        (return
          (values
            (length (get-lines program))
            0)))))

;;; -------------------------------------------------------

(defun jump-to-instruction (program new-instruction-index)
  "Relocates the PROGRAM's instruction pointer (IP) to the
   NEW-INSTRUCTION-INDEX and returns no value."
  (declare (type Program program))
  (declare (type fixnum  new-instruction-index))
  (setf
    (values
      (line-number program)
      (ip          program))
    (find-line-for-instruction program new-instruction-index))
  (values))

;;; -------------------------------------------------------

(defun jump-to-line (program new-line-number)
  "Relocates the NABC PROGRAM's line counter to the inchoation of the
   line designated by the zero-indexed NEW-LINE-NUMBER and returns no
   value."
  (declare (type Program program))
  (declare (type fixnum  new-line-number))
  (setf (line-number program) new-line-number)
  (setf (ip          program) 0)
  (values))

;;; -------------------------------------------------------

(defun get-current-instruction (program)
  "Returns the NABC PROGRAM's currently processed instruction."
  (declare (type Program program))
  (the Instruction
    (get-code-line-instruction
      (get-current-line program)
      (ip program))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Identifier".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Identifier
  (:constructor make-identifier (number-of-as
                                 number-of-bs
                                 number-of-cs)))
  "The ``Identifier'' class serves in the encapsulation of a NABC
   instruction components, its compass intrines the tallies of \"a\",
   \"b\", and \"c\" letters partaking of a collaborating aggregate."
  (number-of-as (error "Missing number of a's.")
                :type      non-negative-integer
                :read-only T)
  (number-of-bs (error "Missing number of b's.")
                :type      non-negative-integer
                :read-only T)
  (number-of-cs (error "Missing number of c's.")
                :type      non-negative-integer
                :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Pattern".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Pattern
  (:constructor make-pattern (number-of-as number-of-bs number-of-cs)))
  "The ``Pattern'' class encapsulates an NABC instruction's general
   pattern, its diorism including the number of \"a\", \"b\", and \"c\"
   characters, which may be generalized to an arbitary tally."
  (number-of-as (error "Missing number of a's.")
                :type      tally
                :read-only T)
  (number-of-bs (error "Missing number of b's.")
                :type      tally
                :read-only T)
  (number-of-cs (error "Missing number of c's.")
                :type      tally
                :read-only T))

;;; -------------------------------------------------------

(defun matches-parameter-p (parameter candidate)
  "Determines whether the CANDIDATE number matches the PARAMETER,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type tally                parameter))
  (declare (type non-negative-integer candidate))
  (the boolean
    (get-boolean-value-of
      (or (and (symbolp parameter)
               (eq parameter :x))
          (= parameter candidate)))))

;;; -------------------------------------------------------

(defun matches-pattern-p (candidate pattern)
  "Determines whether the CANDIDATE identifier matches the PATTERN's
   conformation, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Identifier candidate))
  (declare (type Pattern    pattern))
  (the boolean
    (get-boolean-value-of
      (and
        (matches-parameter-p
          (pattern-number-of-as    pattern)
          (identifier-number-of-as candidate))
        (matches-parameter-p
          (pattern-number-of-bs    pattern)
          (identifier-number-of-bs candidate))
        (matches-parameter-p
          (pattern-number-of-cs    pattern)
          (identifier-number-of-cs candidate))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Production".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Production
  (:constructor make-production (generator)))
  "The ``Production'' class applies itself to the dever of producing
   from an NABC operation identifier an actual instruction
   representation."
  (generator (error "Missing generator.")
             :type      (function (Identifier) Instruction)
             :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of production rules.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of Pattern Production) +PRODUCTIONS+))

;;; -------------------------------------------------------

(defparameter +PRODUCTIONS+ NIL
  "Maintains the production rules by mapping the recognized instruction
   patterns to production objects dedicated to the generation of
   ``Instruction''s from identifiers matching the respective
   forbisens.
   ---
   An ``Identifier'', when probed in an affirmative trial against one
   of the patterns, established in the +PRODUCTIONS+ keys, elicits the
   associated value, a ``Production'' object, the same, when applied to
   the aforementioned identifier, produces an ``Instruction''
   representation of the thus communicated \"a\", \"b\", and \"c\"
   account configurations.")

;;; -------------------------------------------------------

(defmacro define-production-rule ((pattern-a pattern-b pattern-c)
                                  identifier-variable
                                  &body body)
  "Defines a production rule by associating the pattern, generated from
   the PATTERN-A as the \"a\" element's representation, the PATTERN-B
   for \"b\", and PATTERN-C for \"c\", with a production object whose
   callback function's aefauld argument is norned via the
   IDENTIFIER-VARIABLE, expecting the desinent BODY form's primary value
   to produce an ``Instruction'' representation of the matched
   identifier's components."
  `(setf +PRODUCTIONS+
     (acons
       (make-pattern ,pattern-a ,pattern-b ,pattern-c)
       (make-production
         #'(lambda (,identifier-variable)
             (declare (ignorable ,identifier-variable))
             ,@body))
       +PRODUCTIONS+)))

;;; -------------------------------------------------------

(define-production-rule (1 2 :x) identifier
  (make-instruction :inc
    (identifier-number-of-cs identifier)))

;;; -------------------------------------------------------

(define-production-rule (2 1 :x) identifier
  (make-instruction :dec
    (identifier-number-of-cs identifier)))

;;; -------------------------------------------------------

(define-production-rule (0 2 :x) identifier
  (make-instruction :psh
    (identifier-number-of-cs identifier)))

;;; -------------------------------------------------------

(define-production-rule (3 0 1) identifier
  (make-instruction :inp))

;;; -------------------------------------------------------

(define-production-rule (3 1 0) identifier
  (make-instruction :psa))

;;; -------------------------------------------------------

(define-production-rule (2 4 0) identifier
  (make-instruction :swp))

;;; -------------------------------------------------------

(define-production-rule (9 :x :x) identifier
  (make-instruction :peq
    (identifier-number-of-bs identifier)
    (identifier-number-of-cs identifier)))

;;; -------------------------------------------------------

(define-production-rule (4 2 0) identifier
  (make-instruction :rev))

;;; -------------------------------------------------------

(define-production-rule (5 5 0) identifier
  (make-instruction :dup))

;;; -------------------------------------------------------

(define-production-rule (5 4 1) identifier
  (make-instruction :add))

;;; -------------------------------------------------------

(define-production-rule (5 3 2) identifier
  (make-instruction :sub))

;;; -------------------------------------------------------

(define-production-rule (0 0 0) identifier
  (make-instruction :set))

;;; -------------------------------------------------------

(define-production-rule (3 4 3) identifier
  (make-instruction :neq))

;;; -------------------------------------------------------

(define-production-rule (0 3 :x) identifier
  (make-instruction :jmp
    (identifier-number-of-cs identifier)))

;;; -------------------------------------------------------

(define-production-rule (4 0 0) identifier
  (make-instruction :out))

;;; -------------------------------------------------------

(defun find-production (identifier)
  "Returns for the NABC IDENTIFIER a production, the capacitation
   apportioned to whom permits the generation of an instruction from the
   input.
   ---
   Upon a failure to locate a covenable production, an error of an
   unspecified type is signaled."
  (declare (type Identifier identifier))
  (the (or null Production)
    (or (cdr (assoc identifier +PRODUCTIONS+ :test #'matches-pattern-p))
        (error "Cannot detect a production for the identifier ~a."
          identifier))))

;;; -------------------------------------------------------

(defun parse-instruction (identifier)
  "Parses the IDENTIFIER and returns an ``Instruction'' representation
   therefrom, or, upon its disrespondency, signals an error of an
   unspecified type."
  (declare (type Identifier identifier))
  (the Instruction
    (funcall
      (production-generator
        (find-production identifier))
      identifier)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun parameter-character-p (candidate)
  "Determines whether the CANDIDATE represents an NABC instruction
   parameter constituent, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\a)
          (char= candidate #\b)
          (char= candidate #\c)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent admissible
   for its occurrence in an NABC instruction, which amplects both the
   effective elements and space entities, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (or (parameter-character-p candidate)
        (space-character-p     candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-position-p (source position)
  "Determines whether the POSITION represents a valid index into the
   SOURCE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p source position))))

;;; -------------------------------------------------------

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position into the
   SOURCE immediately succeeding the omitted section."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (space-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun expect-separator (source position)
  "Determines whether the character at the POSITION into the SOURCE
   constitutes the instruction separator \"d\", returning on
   confirmation the position in the SOURCE immediately succeeding its
   attested location; otherwise signals an error of an unspecified
   type."
  (declare (type string source))
  (declare (type fixnum position))
  (symbol-macrolet ((character-at-position
                      (the character
                        (char source position))))
    (declare (type character character-at-position))
    (the fixnum
      (cond
        ((not (valid-position-p source position))
          (error "Expected the separator \"d\" at the position ~d, ~
                  but encountered the source exhausted."
            position))
        ((char/= character-at-position #\d)
          (error "Expected the separator \"d\" at the position ~d, ~
                  but encountered the character \"~c\"."
            position
            character-at-position))
        (T
          (1+ position))))))

;;; -------------------------------------------------------

(defun locate-end-of-identifier (source start)
  "Proceeding from the START position into the SOURCE, returns the
   location immediately succeeding the next instruction separator, the
   minuscule \"d\", or, upon its absence, responds with the SOURCE's
   length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position #\d source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun count-character (source start end subject)
  "Proceeding from the START position and ceasing immediately before the
   END location into the SOURCE, tallies the number of consecutive
   SUBJECT instances and returns the non-negative account of the
   SUBJECT's occurrencies in the limited range."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type fixnum    end))
  (declare (type character subject))
  (the non-negative-integer
    (count subject source :start start :end end :test #'char=)))

;;; -------------------------------------------------------

(defun validate-identifier (source start end)
  "Determines whether the NABC identifier, perquired in the range
   commencing from the inclusive START to the exclusive END position in
   the SOURCE, adheres to all validity stipulation, which impose the
   presence of operative symbols or spaces only, returning on
   confirmation no value, otherwise signaling an error of an unspecified
   type."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type fixnum end))
  (when (position-if-not #'identifier-character-p source
          :start start :end end)
    (error "Invalid NABC identifier: ~s."
      (subseq source start end)))
  (values))

;;; -------------------------------------------------------

(defun read-identifier (source start)
  "Proceeding from the START position into the SOURCE, extracts an NABC
   identifier and returns two values:
     (1) An ``Identifier'' representation of the detected identifier.
     (2) The position into the SOURCE immediately succeeding the
         segment occupied by the extracted identifier."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (locate-end-of-identifier source start)))
    (declare (type fixnum end))
    (validate-identifier source start end)
    (the (values Identifier fixnum)
      (values
        (make-identifier
          (count-character source start end #\a)
          (count-character source start end #\b)
          (count-character source start end #\c))
        end))))

;;; -------------------------------------------------------

(defun trim-spaces (source)
  "Removes a fresh string obtained by the expungement of spaces from
   both of the SOURCE's lateralities."
  (declare (type string source))
  (the string
    (string-trim '(#\Space #\Tab) source)))

;;; -------------------------------------------------------

(defun blank-string-p (source)
  "Determines whether the SOURCE represents a blank line, such either
   bears no content at all, or enumerates a composition of space
   characters only, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (every #'space-character-p source))))

;;; -------------------------------------------------------

(defun string-ends-in-separator-p (source)
  "Determines whether the SOURCE, expected to be trimmed at its both
   lateralities, concludes with the \"d\" separator, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (and (plusp (length source))
           (char= (char source (1- (length source))) #\d)))))

;;; -------------------------------------------------------

(defun read-code-line (source)
  "Extracts the NABC instructions from the SOURCE and returns a fresh
   ``Code-Line'' object encompassing these."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((collect-instruction (next-identifier new-position)
            "Parses the NEXT-IDENTIFIER and returns its ``Instruction''
             representation, while concomitantly updating the POSITION
             cursor to the NEW-POSITION, while contingently skipping
             accolent trailing spaces."
            (declare (type Identifier next-identifier))
            (declare (type fixnum     new-position))
            (the Instruction
              (prog1
                (parse-instruction next-identifier)
                (setf position
                  (skip-spaces source new-position))))))
      (the Code-Line
        (make-code-line
          :instructions
            (make-instruction-vector
              (loop
                collect
                  (multiple-value-call #'collect-instruction
                    (read-identifier source position))
                if (< position (length source)) do
                  (setf position
                    (skip-spaces source
                      (expect-separator source position)))
                  (when (>= position (length source))
                    (loop-finish))
                else do
                  (loop-finish)))
          :concludes-with-separator-p
            (string-ends-in-separator-p source))))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Parses the NABC source CODE and returns a fresh ``Program'' model of
   its extracted code lines."
  (declare (type string code))
  (with-input-from-string (input-stream code)
    (declare (type string-stream input-stream))
    (the Program
      (make-program
        (make-line-vector
          (loop
            for input-line
              of-type (or null string)
              =       (read-line input-stream NIL NIL)
            while input-line
            unless (blank-string-p input-line)
              collect
                (read-code-line
                  (trim-spaces input-line))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program navigations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Navigation)
  "The ``Navigation'' interface establishes the shared foundry for all
   concrete classes imparted that onus to manage an NABC program's
   instruction pointer (IP) in terms of relocation ensuing from an
   operation's conclusion.")

;;; -------------------------------------------------------

(defgeneric navigate-program (navigation program)
  (:documentation
    "Relocates the PROGRAM's instruction pointer (IP) in concord with
     the NAVIGATION's scheme and returns no value."))

;;; -------------------------------------------------------

(defstruct (Next-Instruction-Navigation
  (:include Navigation))
  "The ``Next-Instruction-Navigation'' class steers an NABC program's
   instruction pointer (IP) by its plain progression to the subsequent
   instruction.")

;;; -------------------------------------------------------

(defmethod navigate-program ((navigation Next-Instruction-Navigation)
                             (program    Program))
  (declare (type Next-Instruction-Navigation navigation))
  (declare (ignore                           navigation))
  (declare (type Program                     program))
  (advance-program program)
  (values))

;;; -------------------------------------------------------

(defstruct (Jump-Instruction-Navigation
  (:include     Navigation)
  (:constructor make-jump-instruction-navigation (destination)))
  "The ``Jump-Instruction-Navigation'' class steers an NABC program's
   instruction pointer (IP) by its relocation to a specified instruction
   index."
  (destination (error "Missing destination instruction.")
               :type      fixnum
               :read-only T))

;;; -------------------------------------------------------

(defmethod navigate-program ((navigation Jump-Instruction-Navigation)
                             (program    Program))
  (declare (type Jump-Instruction-Navigation navigation))
  (declare (type Program                     program))
  (jump-to-instruction program
    (jump-instruction-navigation-destination navigation))
  (values))

;;; -------------------------------------------------------

(defstruct (Jump-Line-Navigation
  (:include     Navigation)
  (:constructor make-jump-line-navigation (destination)))
  "The ``Jump-Line-Navigation'' class steers an NABC program's
   instruction pointer (IP) by its relocation to the start, that is, the
   incipient instruction, of a specified code line."
  (destination (error "Missing destination line.")
               :type      fixnum
               :read-only T))

;;; -------------------------------------------------------

(defmethod navigate-program ((navigation Jump-Line-Navigation)
                             (program    Program))
  (declare (type Jump-Line-Navigation navigation))
  (declare (type Program              program))
  (jump-to-line program
    (jump-line-navigation-destination navigation))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Next-Instruction-Navigation
               +NEXT-INSTRUCTION-NAVIGATION+))

;;; -------------------------------------------------------

(defparameter +NEXT-INSTRUCTION-NAVIGATION+
  (make-next-instruction-navigation)
  "The default program navigation, which simply advances the instruction
   pointer (IP) to the subsequent operation.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          Program
    :documentation "The NABC program to evaluate.")
   (navigation
    :initform      +NEXT-INSTRUCTION-NAVIGATION+
    :accessor      program-navigation
    :type          Navigation
    :documentation "The entity responsible for the next occasion's
                    manipulation of the PROGRAM's instruction pointer
                    (IP).")
   (program-stack
    :initform      NIL
    :accessor      program-stack
    :type          program-stack
    :documentation "The program stack.")
   (accumulator
    :initform      0
    :accessor      accumulator
    :type          integer
    :documentation "The integer-valued scalar accumulator."))
  (:documentation
    "The ``Interpreter'' class applies itself to the provision of a
     context for the evaluation of an NABC program."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' nuncupated to the NABC
   PROGRAM's evaluation."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-interpreter (interpreter)
  "Relocates the instruction pointer (IP) of the NABC program consigned
   to the INTERPRETER's castaldy by the currently selected navigation
   scheme, resets the same to the standard subsequent instruction
   selector for a contingent future execution, and returns no value."
  (declare (type Interpreter interpreter))
  (navigate-program
    (program-navigation interpreter)
    (get-program        interpreter))
  (setf (program-navigation interpreter) +NEXT-INSTRUCTION-NAVIGATION+)
  (values))

;;; -------------------------------------------------------

(defun push-on-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-value))
  (push new-value (program-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (interpreter)
  "Pops and returns the top element from the INTERPRETER's stack, or
   signals an error of an unspecified type upon its vacancy."
  (declare (type Interpreter interpreter))
  (the integer
    (or (pop (program-stack interpreter))
        (error "Cannot pop from an empty stack."))))

;;; -------------------------------------------------------

(defun swap-stack-top (interpreter)
  "Exchanges the positions of the top and second to top element in the
   stack governed by the INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (cond
    ((null (program-stack interpreter))
      (error "Cannot swap the top elements of an empty stack."))
    ((= (length (program-stack interpreter)))
      (error "Cannot swap the top elements of a singleton stack."))
    (T
      (rotatef
        (first  (program-stack interpreter))
        (second (program-stack interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun reverse-stack (interpreter)
  "Reverses the program stack governed by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (setf (program-stack interpreter)
    (nreverse
      (program-stack interpreter)))
  (values))

;;; -------------------------------------------------------

(defun duplicate-stack-top (interpreter)
  "Duplicates the element at the INTERPRETER stack's top and returns no
   value."
  (declare (type Interpreter interpreter))
  (if (program-stack interpreter)
    (push
      (first (program-stack interpreter))
      (program-stack interpreter))
    (error "Cannot duplicate the top element of an empty stack."))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-instruction (interpreter opcode instruction)
  (:documentation
    "Evaluates the INSTRUCTION, dispatched on its OPCODE, in the
     INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defun process-instruction (interpreter instruction)
  "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (dispatch-instruction interpreter
    (instruction-type instruction)
    instruction)
  (values))

;;; -------------------------------------------------------

(defmacro define-instruction-dispatch
    (opcode (interpreter-variable instruction-variable)
     &body body)
  "Defines an implementation of the generic function
   ``dispatch-instruction'', empighting as the first formal parameter
   the agnomination communicated by the INTERPRETER-VARIABLE, as the
   second an automatically generated name which generalizes on an
   ``eql''-conformation with the OPCODE, as the third that of the
   INSTRUCTION-VARIABLE, and utilizes the BODY forms, finally returning
   in the thus generated method no value.
   ---
   If the first BODY form embraces a string object, the same is
   construed as the method implementation's documentation string and
   reappropriated for this purpose."
  (let ((opcode-variable (gensym)))
    (declare (type symbol opcode-variable))
    `(defmethod dispatch-instruction
         ((,interpreter-variable Interpreter)
          (,opcode-variable      (eql ,opcode))
          (,instruction-variable Instruction))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type opcode      ,opcode-variable))
       (declare (ignore           ,opcode-variable))
       (declare (type Instruction ,instruction-variable))
       (declare (ignorable        ,instruction-variable))
       ,@body)))

;;; -------------------------------------------------------

(define-instruction-dispatch :inc (interpreter instruction)
  (incf (accumulator interpreter)
    (instruction-first-operand instruction)))

;;; -------------------------------------------------------

(define-instruction-dispatch :dec (interpreter instruction)
  (decf (accumulator interpreter)
    (instruction-first-operand instruction)))

;;; -------------------------------------------------------

(define-instruction-dispatch :psh (interpreter instruction)
  (push-on-stack interpreter
    (instruction-first-operand instruction)))

;;; -------------------------------------------------------

(define-instruction-dispatch :inp (interpreter instruction)
  (format T "~&>> ")
  (finish-output)
  (push-on-stack interpreter
    (char-code
      (read-char NIL NIL #\Null)))
  (clear-input))

;;; -------------------------------------------------------

(define-instruction-dispatch :psa (interpreter instruction)
  (push-on-stack interpreter
    (accumulator interpreter)))

;;; -------------------------------------------------------

(define-instruction-dispatch :swp (interpreter instruction)
  (swap-stack-top interpreter))

;;; -------------------------------------------------------

(define-instruction-dispatch :peq (interpreter instruction)
  (when (= (pop-from-stack interpreter)
           (instruction-first-operand instruction))
    (setf (program-navigation interpreter)
      (make-jump-instruction-navigation
        (instruction-second-operand instruction)))))

;;; -------------------------------------------------------

(define-instruction-dispatch :rev (interpreter instruction)
  (reverse-stack interpreter))

;;; -------------------------------------------------------

(define-instruction-dispatch :dup (interpreter instruction)
  (duplicate-stack-top interpreter))

;;; -------------------------------------------------------

(define-instruction-dispatch :add (interpreter instruction)
  (incf
    (accumulator interpreter)
    (pop-from-stack interpreter)))

;;; -------------------------------------------------------

(define-instruction-dispatch :sub (interpreter instruction)
  (decf
    (accumulator interpreter)
    (pop-from-stack interpreter)))

;;; -------------------------------------------------------

(define-instruction-dispatch :set (interpreter instruction)
  (setf (accumulator interpreter) 0))

;;; -------------------------------------------------------

(define-instruction-dispatch :neq (interpreter instruction)
  (when (= (pop-from-stack interpreter)
           (pop-from-stack interpreter))
    (advance-interpreter interpreter)))

;;; -------------------------------------------------------

(define-instruction-dispatch :jmp (interpreter instruction)
  (setf (program-navigation interpreter)
    (make-jump-line-navigation
      (instruction-first-operand instruction))))

;;; -------------------------------------------------------

(define-instruction-dispatch :out (interpreter instruction)
  (write-char
    (code-char
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the NABC program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program) interpreter
    (declare (type Program program))
    (loop until (program-exhausted-p program) do
      (process-instruction interpreter
        (get-current-instruction program))
      (advance-interpreter interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-NABC (code)
  "Interprets the piece of NABC source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (validate-line-continuations
        (parse-program code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perpetually repeating cat program.
(interpret-NABC "aaacdaaaad
                 bbb")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Concept:
;;   ----------------------------------------------------
;;   Line no. | Mnemonic | Stack content
;;   ---------+----------+-------------------------------
;;    0       | psh 49   | [top> 49]
;;    1       | inp      | [top> input, 49]
;;    2       | dup      | [top> input, input, 49]
;;    3       | dup      | [top> input, input, input, 49]
;;    4       | rev      | [top> 49, input, input, input]
;;    5       | neq      | [top> input, input]
;;    6       | jmp 10   | [top> input, input]
;;    7       | out      | [top> input]
;;    8       | dup      | [top> input, input]
;;    9       | jmp 7    | [top> input, input]
;;   10       | out      | [top>]
;;   ----------------------------------------------------
(interpret-NABC
  "bbcccccccccccccccccccccccccccccccccccccccccccccccccd
   aaacd
   aaaaabbbbbd
   aaaaabbbbbd
   aaaabbd
   aaabbbbcccd
   bbbccccccccccd
   aaaad
   aaaaabbbbbd
   bbbcccccccd
   aaaa")

;;; -------------------------------------------------------

;; Print a catena of ten (10) asterisks ("*") in one row:
;; 
;; Concept:
;;   -------------------------------------------------------
;;   Line no. | Mnemonic | Stack content | Accumulator state
;;   ---------+----------+---------------+------------------
;;   0        | inc 10   | [top>]        | 10
;;   1        | psa      | [top> 10]     | 10
;;   2        | peq 0 9  | [top> acc]    | 10 through 0
;;   3        | psh 42   | [top> 42]     | 10 through 0
;;   4        | out      | [top>]        | 10 through 0
;;   5        | psh 1    | [top> 1]      | 10 through 0
;;   6        | sub      | [top>]        | 10 through 0
;;   7        | psa      | [top> acc]    | 10 through 0
;;   8        | jmp 2    | [top> acc]    | 10 through 0
;;   -------------------------------------------------------
(interpret-NABC
  "abbccccccccccd
   aaabd
   aaaaaaaaacccccccccd
   bbccccccccccccccccccccccccccccccccccccccccccd
   aaaad
   bbcd
   aaaaabbbccd
   aaabd
   bbbccd")
