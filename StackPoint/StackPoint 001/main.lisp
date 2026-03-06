;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "StackPoint", invented by the Esolang user "ChuckEsoteric08"
;; and presented on August 25th, 2022, its diorism's patefaction that
;; of a champarty's engagement atwixen a stack of unbounded, signed
;; integer numbers and a bilaterally infinite tape whose cells are
;; subjected to the same capacity, both filsting as the warklumes of a
;; language defined by basic arithmetics, input and output facilities,
;; and a label-based navigation mechanism.
;; 
;; 
;; Concept
;; =======
;; The StackPoint programming language is founded upon a very potent
;; memory constitution, thilk bewrays its competences in a coefficiency
;; of a stack and a bilaterally infinite tape, both aspects of the
;; salvatory's entirety comprehending integer values of any sign and
;; mickleness and their elements.
;; 
;; == THE MEMORY: A TWISSEL OF STACK AND TAPE ==
;; The memory constitutes a jumelle entailing a stack of integer numbers
;; disencumbered from any sign and mickleness' impositions, and a
;; bilaterally infinite tape whose cells siclike subscribe to this mete
;; of capacity.
;; 
;; == THE STACK AMPLECTS AT ITS INCHOACY A SINGLE ZERO ELEMENT ==
;; At the instant of the program's inchoacy, an aefauld zero (0) element
;; represents the stack's original incolant. Attempts at the indagation
;; of or removal from this salvatory, when empight in a status limned by
;; vacancy, instigate an "EmptyStackError" inroad.
;; 
;; == THE TAPE COMPREHENDS AN ABNUMERABLE ACCOMPT OF INTEGERS ==
;; The tape, a bilaterally infinite dispansion of cells, assigns to
;; each such unit, capacitated to comprehend signed integer number with
;; mear in the ascent or descent, the default value of zero (0).
;; 
;; A kenspeckle aspect of its conception, each cell's amenability is
;; realized in a signed integer number as the identifying subscript.
;; 
;; 
;; Instructions
;; ============
;; StackPoint's instruction set enumerates 14 members, their bailiwicks
;; distributed across rudiments of arithmetics, stack management,
;; the champarty atwixen the latter and the tape, input and output
;; intercourse, and a label-based jump mechanism.
;; 
;; == OVERVIEW ==
;; A summary concerning the available commands shall be produced in the
;; following table.
;; 
;; Please heed that the succedaneous segments are underlined employing
;; asterisks ("*"), and intended for their substitution by actual
;; StackPoint code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Increments the top stack element by an amount of
;;           | one (1).
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   <       | Decrements the top stack element by an amount of
;;           | one (1).
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   *       | Pushes the value zero (0) onto the stack.
;;   ..................................................................
;;   #       | Pops the stack's top element.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   "       | Duplicates the top stack element.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   ~       | Swaps the two top stack elements' positions.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation's execution, the illicit access
;;           | attempt will instigate an error of the type
;;           | "EmptyStackError".
;;   ..................................................................
;;   &       | Reverses the order of the stack's elements.
;;   ..................................................................
;;   +       | Increments the tape cell amenable to the index specified
;;           | by the top stack element by an amount of one (1).
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   -       | Decrements the tape cell amenable to the index specified
;;           | by the top stack element by an amount of one (1).
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a character and
;;           | stores its ASCII code the tape cell amenable to the
;;           | index specified by the top stack element.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code concurs with the
;;           | tape cell amenable to the index specified by the top
;;           | stack element.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   name:   | Declares a label amenable to the {name}.
;;   %%%%    |---------------------------------------------------------
;;           | {name} must be a catena comprising one or more
;;           | characters from the following set:
;;           |   - The minuscular Latin letters, "a" through "z".
;;           |   - The decimal digits,           "0" through "9".
;;           |---------------------------------------------------------
;;           | If a label with the {name} has been already defined in
;;           | a prevenient position, an error of the type
;;           | "DuplicateLabelError" is signaled.
;;   ..................................................................
;;   name^   | If the top stack element equals zero (0), relocates the
;;   %%%%    | instruction pointer (IP) to the position containing the
;;           | declaration of the label with the {name}; otherwise
;;           | produces no effect.
;;           |---------------------------------------------------------
;;           | {name} must be a catena comprising one or more
;;           | characters from the following set:
;;           |   - The minuscular Latin letters, "a" through "z".
;;           |   - The decimal digits,           "0" through "9".
;;           |---------------------------------------------------------
;;           | If no label with the {name} could be located, an error
;;           | of the type "NoSuchLabelError" is signaled.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   name@   | If the top stack element does not equal zero (0),
;;   %%%%    | relocates the instruction pointer (IP) to the position
;;           | containing the declaration of the label with the name
;;           | {name}; otherwise produces no effect.
;;           |---------------------------------------------------------
;;           | {name} must be a catena comprising one or more
;;           | characters from the following set:
;;           |   - The minuscular Latin letters, "a" through "z".
;;           |   - The decimal digits,           "0" through "9".
;;           |---------------------------------------------------------
;;           | If no label with the {name} could be located, an error
;;           | of the type "NoSuchLabelError" is signaled.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's patefaction constitutes an effort in the
;; programming language Common Lisp, the realization's firmament a
;; transcription of the instructions ensconced in the source code string
;; into conable class representations, ere their actual execution.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-03-03
;; 
;; Sources:
;;   [esolang:2025:StackPoint]
;;   The Esolang contributors, "StackPoint", November 28th, 2025
;;   URL: "https://esolangs.org/wiki/StackPoint"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, in which each key assumes the KEY-TYPE and affiliates
   with a value subsuming into the VALUE-TYPE, both being governed by
   the generic sentinel ``*'' in the default case."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and (typep current-key   key-type)
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a linked list, comprehending zero or
   more elements of the ELEMENT-TYPE, the same is governed by the
   generic sentinel ``*'' in the default case."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable StackPoint program as a
   one-dimensional simple array comprised of ``Command'' objects."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype label-table ()
  "The ``label-table'' type defines a registry for labels, mapping the
   unique names to the zero-based positions of the corresponding
   ``Declare-Label-Command'' instances in the entailing program, and
   realized as a hash table whose keys assume simple base strings,
   being affiliated with ``fixnum'' indices."
  '(hash-table-of simple-base-string fixnum))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines the StackPoint memory's stack component as
   a list-based last in, first out storage unit, admitting merely
   signed integer numbers."
  '(list-of integer))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines the StackPoint memory's tape component as
   a bilaterally infinite catena of signed integer numbers, amenable to
   indices of the same composition, and manifested in a hash table whose
   keys represent the cell indices, affiliating with the cell states in
   the values, both of the ``integer'' species."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface furnishes a substratum upon which all
   classes in the agency of a StackPoint command's representation shall
   be edified.")

;;; -------------------------------------------------------

(defstruct (Increment-Stack-Command
  (:include Command))
  "The ``Increment-Stack-Command'' class models the behest for the
   stack top element's incrementation.")

;;; -------------------------------------------------------

(defstruct (Decrement-Stack-Command
  (:include Command))
  "The ``Decrement-Stack-Command'' class models the behest for the
   stack top element's decrementation.")

;;; -------------------------------------------------------

(defstruct (Push-Zero-Command
  (:include Command))
  "The ``Push-Zero-Command'' class models the behest for the insertion
   of a zero (0) value at the stack's top position.")

;;; -------------------------------------------------------

(defstruct (Pop-Command
  (:include Command))
  "The ``Pop-Command'' class models the behest for removal of discarding
   of the stack's top element.")

;;; -------------------------------------------------------

(defstruct (Duplicate-Command
  (:include Command))
  "The ``Duplicate-Command'' class models the behest for the stack's top
   element's duplication.")

;;; -------------------------------------------------------

(defstruct (Swap-Command
  (:include Command))
  "The ``Swap-Command'' class models the behest for the stack's two top
   elements' exchange in position.")

;;; -------------------------------------------------------

(defstruct (Reverse-Command
  (:include Command))
  "The ``Reverse-Command'' class models the behest for the stack
   elements' reversal in their positions.")

;;; -------------------------------------------------------

(defstruct (Increment-Cell-Command
  (:include Command))
  "The ``Increment-Cell-Command'' class models the behest for a specific
   cell's incrementation.")

;;; -------------------------------------------------------

(defstruct (Decrement-Cell-Command
  (:include Command))
  "The ``Decrement-Cell-Command'' class models the behest for a specific
   cell's decrementation.")

;;; -------------------------------------------------------

(defstruct (Input-Command
  (:include Command))
  "The ``Input-Command'' class models the behest for a character
   input and its transfer into a specific cell.")

;;; -------------------------------------------------------

(defstruct (Output-Command
  (:include Command))
  "The ``Output-Command'' class models the behest for an output of a
   specific cell in a character form.")

;;; -------------------------------------------------------

(defstruct (Label-Command
  (:include Command))
  "The ``Label-Command'' abstract class, as a specialization of the
   ``Command'' structure, furnishes a common foundry for all classes
   intended to represent StackPoint commands dependent on a label name's
   specification."
  (name (error "No label name has been communicated.")
        :type      simple-base-string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Declare-Label-Command
  (:include Label-Command))
  "The ``Declare-Label-Command'' class models the behest for a label
   definition.")

;;; -------------------------------------------------------

(defstruct (Jump-If-Zero-Command
  (:include Label-Command))
  "The ``Jump-If-Zero-Command'' class models the behest for a relocation
   to a label in the case of a specific cell's equality to zero (0).")

;;; -------------------------------------------------------

(defstruct (Jump-If-Not-Zero-Command
  (:include Label-Command))
  "The ``Jump-If-Not-Zero-Command'' class models the behest for a
   relocation to a label in the case of a specific cell's inequality to
   zero (0).")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition StackPoint-Error (simple-error)
  ()
  (:documentation
    "The ``StackPoint-Error'' condition type serves as the firmament to
     all conditions partaking in any stage of a StackPoint program's
     reception, evaluation, or execution."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (StackPoint-Error)
  ()
  (:default-initargs
    :format-control
      "A label with the name ~s has already been defined.")
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the
     communication of an anomalous situation involving the attempted
     declaration of a label by a name already appropriated by a
     prevenient association."))

;;; -------------------------------------------------------

(define-condition No-Such-Label-Error (StackPoint-Error)
  ()
  (:default-initargs
    :format-control "No label with the name ~s could be located.")
  (:documentation
    "The ``No-Such-Label-Error'' condition type serves in the
     communication of an anomalous situation involving the illicit
     navigation to a label via a name not defined in the program."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (StackPoint-Error)
  ()
  (:default-initargs
    :format-control "You cannot peek into or pop from an empty stack.")
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the
    communication of an anomalous situation involving the illicit
    attempt to peek into or pop from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Interprets the OBJECT in its facette as a \"generalized boolean\"
   and returns an actual Boolean equivalent, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, produces ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (find candidate '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun label-name-character-p (candidate)
  "Determines whether the CANDIDATE represents an admissible constituent
   for a label name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (or (lower-case-p candidate)
          (digit-char-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Converts the SOURCE into a simple string, either returning a fresh
   instance of this specialized string type, or, upon its present
   conformance to the desired species, responds with the unmodified
   SOURCE itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun convert-into-a-simple-base-string (source)
  "Converts the SOURCE into a simple base string, either returning a
   fresh instance of this specialized string type, or, upon its present
   conformance to the desired species, responds with the unmodified
   SOURCE itself."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-an-empty-program ()
  "Creates and returns a fresh and empty ``program''."
  (the program
    (coerce NIL '(simple-array Command (*)))))

;;; -------------------------------------------------------

(defun make-a-program-from (commands)
  "Creates and returns a fresh ``program'' comprehending the COMMANDS."
  (declare (type (list-of Command) commands))
  (the program
    (coerce commands '(simple-array Command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexer.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source*))
(declaim (type fixnum        *current-position*))
(declaim (type character     *current-character*))
(declaim (type boolean       *source-is-exhausted-p*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The piece of StackPoint source code to evaluate.")

(defparameter *current-position* 0
  "The current position into the ``*SOURCE*'' string.")

(define-symbol-macro *current-character*
  (the character
    (schar *source* *current-position*)))

(define-symbol-macro *source-is-exhausted-p*
  (the boolean
    (not (array-in-bounds-p *source* *current-position*))))

;;; -------------------------------------------------------

(defun change-the-source-to (new-source)
  "Changes the ``*SOURCE*'' to the NEW-SOURCE, updates the pertinent
   state variables, and returns no value."
  (declare (type string new-source))
  (psetf
    *source*           (convert-into-a-simple-string new-source)
    *current-position* 0)
  (values))

;;; -------------------------------------------------------

(defun current-character-equals-p (expected-character)
  "Determines whether ``*CURRENT-CHARACTER*'' into the ``*SOURCE*''
   equals the EXPECTED-CHARACTER, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character expected-character))
  (the boolean
    (convert-into-a-boolean-value
      (char= *current-character* expected-character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces ()
  "Proceeding from the ``*CURRENT-POSITION*'' into the ``*SOURCE*'',
   skips a sequence comprehending zero or more attiguous whitespaces
   and returns no value."
  (setf *current-position*
    (or (position-if-not #'whitespace-character-p *source*
          :start *current-position*)
        (length *source*)))
  (values))

;;; -------------------------------------------------------

(defun read-a-label-name ()
  "Proceeding from the ``*CURRENT-POSITION*'' into the ``*SOURCE*'',
   consumes a label name and returns a fresh simple string
   representation of its content."
  (the simple-base-string
    (convert-into-a-simple-base-string
      (with-output-to-string (name)
        (declare (type string-stream name))
        (loop
          while
            (and (not *source-is-exhausted-p*)
                 (label-name-character-p *current-character*))
          do
            (write-char *current-character* name)
            (incf *current-position*))))))

;;; -------------------------------------------------------

(defun parse-a-label-command ()
  "Proceeding from the ``*CURRENT-POSITION*'' into the ``*SOURCE*'',
   parses a command involving a label name and returns a connable
   representation thereof."
  (the Label-Command
    (let ((label-name (read-a-label-name)))
      (declare (type simple-string label-name))
      (if *source-is-exhausted-p*
        (error "A command involving a label name has been left ~
                incomplete, with the source exhausted, at the ~
                position ~d."
          *current-position*)
        (case *current-character*
          (#\:
            (incf *current-position*)
            (make-declare-label-command :name label-name))
          (#\^
            (incf *current-position*)
            (make-jump-if-zero-command :name label-name))
          (#\@
            (incf *current-position*)
            (make-jump-if-not-zero-command :name label-name))
          (otherwise
            (error "The character \"~c\", located at the position ~d, ~
                    cannot be used as a suffix for a label-based ~
                    command."
              *current-character* *current-position*)))))))

;;; -------------------------------------------------------

(defun parse-the-next-command ()
  "Proceeding from the ``*CURRENT-POSITION*'' into the ``*SOURCE*'',
   extracts the nearest following behest and returns a connable
   ``Command'' representation thereof; or, upon the ``*SOURCE*'''s
   exhaustion, responds with ``NIL''."
  (the (or null Command)
    (cond
      (*source-is-exhausted-p*
        NIL)
      ((whitespace-character-p *current-character*)
        (skip-whitespaces)
        (parse-the-next-command))
      ((current-character-equals-p #\>)
        (prog1
          (make-increment-stack-command)
          (incf *current-position*)))
      ((current-character-equals-p #\<)
        (prog1
          (make-decrement-stack-command)
          (incf *current-position*)))
      ((current-character-equals-p #\*)
        (prog1
          (make-push-zero-command)
          (incf *current-position*)))
      ((current-character-equals-p #\#)
        (prog1
          (make-pop-command)
          (incf *current-position*)))
      ((current-character-equals-p #\")
        (prog1
          (make-duplicate-command)
          (incf *current-position*)))
      ((current-character-equals-p #\~)
        (prog1
          (make-swap-command)
          (incf *current-position*)))
      ((current-character-equals-p #\+)
        (prog1
          (make-increment-cell-command)
          (incf *current-position*)))
      ((current-character-equals-p #\-)
        (prog1
          (make-decrement-cell-command)
          (incf *current-position*)))
      ((current-character-equals-p #\,)
        (prog1
          (make-input-command)
          (incf *current-position*)))
      ((current-character-equals-p #\.)
        (prog1
          (make-output-command)
          (incf *current-position*)))
      ((label-name-character-p *current-character*)
        (parse-a-label-command))
      (T
        (error "The character \"~c\", located at the position ~d, ~
                cannot be affiliated with any recognized command."
          *current-character* *current-position*)))))

;;; -------------------------------------------------------

(defun parse-the-program ()
  "Parses the ``*SOURCE*'' and returns a ``program'' representation
   encompassing its commands in a one-dimensional simple array."
  (the program
    (make-a-program-from
      (loop
        for next-command
          of-type (or null Command)
          =       (parse-the-next-command)
        while next-command
          collect next-command))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the label management operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-label-table ()
  "Creates and returns an initially empty ``label-table''."
  (the label-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun register-the-label (labels name position)
  "Associates the label NAME with the zero-based POSITION in the LABELS
   table and returns no value.
   ---
   If an entry amenable to the NAME is already present among the LABELS,
   an error of the type ``Duplicate-Label-Error'' is signaled."
  (declare (type label-table        labels))
  (declare (type simple-base-string name))
  (declare (type fixnum             position))
  (if (gethash name labels)
    (error 'Duplicate-Label-Error :format-arguments (list name))
    (setf (gethash name labels) position))
  (values))

;;; -------------------------------------------------------

(defun collect-the-labels (program)
  "Creates and returns a fresh ``label-table'' comprehending the label
   definitions, as name-position combinations, entailed in the PROGRAM."
  (declare (type program program))
  (let ((labels (prepare-an-empty-label-table)))
    (declare (type label-table labels))
    (dotimes (current-position (length program))
      (declare (type fixnum current-position))
      (let ((current-command (aref program current-position)))
        (declare (type Command current-command))
        (when (declare-label-command-p current-command)
          (register-the-label labels
            (label-command-name current-command)
            current-position))))
    (the label-table labels)))

;;; -------------------------------------------------------

(defun locate-the-label (labels name)
  "Returns the zero-based position of the label amenable to the NAME as
   registered in the LABELS table; or signals an error of the type
   ``No-Such-Label-Error'' upon its disrespondency."
  (declare (type label-table        labels))
  (declare (type simple-base-string name))
  (the fixnum
    (or (gethash name labels)
        (error 'No-Such-Label-Error :format-arguments (list name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the interpreter variables.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type program     *program*))
(declaim (type fixnum      *ip*))
(declaim (type label-table *labels*))
(declaim (type Command     *current-command*))
(declaim (type boolean     *program-has-halted-P*))
(declaim (type stack       *stack*))
(declaim (type tape        *tape*))

;;; -------------------------------------------------------

(defparameter *program*
  (make-an-empty-program)
  "The StackPoint command sequence to execute.")

(defparameter *ip*
  0
  "The instruction pointer's (IP) current zero-based position into the
   ``*PROGRAM*''.")

(defparameter *labels*
  (prepare-an-empty-label-table)
  "The collated label name-position pairs from the ``*PROGRAM*''.")

(define-symbol-macro *current-command*
  (the Command
    (aref *program* *ip*)))

(define-symbol-macro *program-has-halted-p*
  (the boolean
    (not (array-in-bounds-p *program* *ip*))))

(defparameter *stack*
  (list 0)
  "The memory's stack component as a signed integer-valued stack,
   initially comprehending a single zero (0) element.")

(defparameter *tape*
  (make-hash-table :test #'eql)
  "The memory's tape component as a bilaterally infinite catena of
   signed integer-valued cells.")

;;; -------------------------------------------------------

(defun specify-a-new-stackpoint-program (new-program)
  "Changes the interpreter's ``*PROGRAM*'' to the NEW-PROGRAM, restores
   its state variables to their default configurations, and returns no
   value."
  (declare (type program new-program))
  (psetf
    *program* new-program
    *ip*      0
    *labels*  (collect-the-labels new-program)
    *stack*   (list 0)
    *tape*    (make-hash-table  :test #'eql))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory stack operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun determine-whether-the-stack-contains-elements ()
  "Determines whether the ``*STACK*'' comprehends at least one element,
   returning on confirmation no value; otherwise, in the case of an
   empty stack, signals an error of the type ``Empty-Stack-Error''."
  (unless *stack*
    (error 'Empty-Stack-Error))
  (values))

;;; -------------------------------------------------------

(defun push-onto-the-stack (new-element)
  "Inserts the NEW-ELEMENT at the ``*STACK*'''s top position and returns
   no value."
  (declare (type integer new-element))
  (push new-element *stack*)
  (values))

;;; -------------------------------------------------------

(defun pop-from-the-stack ()
  "Removes and returns from the ``*STACK*'' the top element.
   ---
   If the ``*STACK*'' is empty at the instant of this operation's
   invocation, an error of the type ``Empty-Stack-Error'' is signaled."
  (determine-whether-the-stack-contains-elements)
  (the integer
    (pop *stack*)))

;;; -------------------------------------------------------

(defun peek-into-the-stack ()
  "Returns without removing from the ``*STACK*'' the top element.
   ---
   If the ``*STACK*'' is empty at the instant of this operation's
   invocation, an error of the type ``Empty-Stack-Error'' is signaled."
  (determine-whether-the-stack-contains-elements)
  (the integer
    (first *stack*)))

;;; -------------------------------------------------------

(defun top-stack-element ()
  "Returns without removing from the ``*STACK*'' the top element.
   ---
   If the ``*STACK*'' is empty at the instant of this operation's
   invocation, an error of the type ``Empty-Stack-Error'' is signaled."
  (determine-whether-the-stack-contains-elements)
  (the integer
    (first *stack*)))

;;; -------------------------------------------------------

(defun (setf top-stack-element) (new-value)
  "Replaces the top element in the ``*STACK*'' by the NEW-VALUE and
   returns no value.
   ---
   If the ``*STACK*'' is empty at the instant of this operation's
   invocation, an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type integer new-value))
  (determine-whether-the-stack-contains-elements)
  (setf (first *stack*) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tape-cell-value-at (index)
  "Returns the value of the ``*TAPE*'' cell amenable to the INDEX."
  (declare (type integer index))
  (the integer
    (gethash index *tape* 0)))

;;; -------------------------------------------------------

(defun (setf tape-cell-value-at) (new-value index)
  "Stores the NEW-VALUE in the ``*TAPE*'' cell amenable to the INDEX
   and returns no value."
  (declare (type integer new-value))
  (declare (type integer index))
  (setf (gethash index *tape* 0) new-value)
  (values))

;;; -------------------------------------------------------

(defun current-tape-cell-value ()
  "Returns the value of the ``*TAPE*'' cell indexed by the ``*STACK*'''s
   top element."
  (the integer
    (tape-cell-value-at
      (peek-into-the-stack))))

;;; -------------------------------------------------------

(defun (setf current-tape-cell-value) (new-value)
  "Replaces the value of the ``*TAPE*'' cell indexed by the
   ``*STACK*'''s top element to the NEW-VALUE and returns no value."
  (declare (type integer new-value))
  (setf
    (tape-cell-value-at
      (peek-into-the-stack))
    new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-the-command (command)
  (:documentation
    "Evaluates the COMMAND in the interpreter's current context and
     returns no value.")
  
  (:method ((command Increment-Stack-Command))
    (declare (type Increment-Stack-Command command))
    (declare (ignore                       command))
    (incf (top-stack-element))
    (values))
  
  (:method ((command Decrement-Stack-Command))
    (declare (type Decrement-Stack-Command command))
    (declare (ignore                       command))
    (decf (top-stack-element))
    (values))
  
  (:method ((command Push-Zero-Command))
    (declare (type Push-Zero-Command command))
    (declare (ignore                 command))
    (push-onto-the-stack 0)
    (values))
  
  (:method ((command Pop-Command))
    (declare (type Pop-Command command))
    (declare (ignore           command))
    (pop-from-the-stack)
    (values))
  
  (:method ((command Duplicate-Command))
    (declare (type Duplicate-Command command))
    (declare (ignore                 command))
    (push-onto-the-stack
      (peek-into-the-stack))
    (values))
  
  (:method ((command Swap-Command))
    (declare (type Swap-Command command))
    (declare (ignore            command))
    (let ((top-element        (pop-from-the-stack))
          (next-lower-element (pop-from-the-stack)))
      (declare (type integer top-element))
      (declare (type integer next-lower-element))
      (push-onto-the-stack top-element)
      (push-onto-the-stack next-lower-element))
    (values))
  
  (:method ((command Reverse-Command))
    (declare (type Reverse-Command command))
    (declare (ignore               command))
    (setf *stack*
      (nreverse *stack*))
    (values))
  
  (:method ((command Increment-Cell-Command))
    (declare (type Increment-Cell-Command command))
    (declare (ignore                      command))
    (incf (current-tape-cell-value))
    (values))
  
  (:method ((command Decrement-Cell-Command))
    (declare (type Decrement-Cell-Command command))
    (declare (ignore                      command))
    (decf (current-tape-cell-value))
    (values))
  
  (:method ((command Input-Command))
    (declare (type Input-Command command))
    (declare (ignore             command))
    (format T "~&>> ")
    (finish-output)
    (setf (current-tape-cell-value)
      (char-code
        (read-char NIL NIL #\Null)))
    (clear-input)
    (values))
  
  (:method ((command Output-Command))
    (declare (type Output-Command command))
    (declare (ignore              command))
    (format T "~c"
      (code-char
        (current-tape-cell-value)))
    (finish-output)
    (values))
  
  (:method ((command Declare-Label-Command))
    (declare (type Declare-Label-Command command))
    (declare (ignore                     command))
    (values))
  
  (:method ((command Jump-If-Zero-Command))
    (declare (type Jump-If-Zero-Command command))
    (when (zerop (current-tape-cell-value))
      (setf *ip*
        (locate-the-label *labels*
          (jump-if-zero-command-name command))))
    (values))
  
  (:method ((command Jump-If-Not-Zero-Command))
    (declare (type Jump-If-Not-Zero-Command command))
    (unless (zerop (current-tape-cell-value))
      (setf *ip*
        (locate-the-label *labels*
          (jump-if-not-zero-command-name command))))
    (values)))

;;; -------------------------------------------------------

(defun process-the-current-command ()
  "Processes the ``*CURRENT-COMMAND*'' and returns no value."
  (process-the-command *current-command*)
  (values))

;;; -------------------------------------------------------

(defun execute-the-stackpoint-program ()
  "Executes the ``*PROGRAM*'' and returns no value."
  (loop until *program-has-halted-p* do
    (process-the-current-command)
    (incf *ip*))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-stackpoint-code (code)
  "Interprets the piece of StackPoint source CODE and returns no value."
  (declare (type string code))
  (change-the-source-to code)
  (specify-a-new-stackpoint-program
    (parse-the-program))
  (execute-the-stackpoint-program)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-the-stackpoint-code
  "repeat:
   ,
   halt^
   .
   repeat@
   halt:")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-stackpoint-code
  "++++++++++++++++++++++++++++++++++++++++++++++++
   >+++++++++++++++++++++++++++++++++++++++++++++++++
   >,------------------------------------------------
   
   zeroinput^
   
   <
   oneinput:
   .
   oneinput@
   
   zeroinput:
   <<.")
