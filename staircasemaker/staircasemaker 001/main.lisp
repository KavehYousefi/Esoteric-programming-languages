;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "staircasemaker", invented by the Esolang user "Threesodas"
;; and presented on December 14th, 2021, to whom the diorism is
;; apportioned of its programs' designment to mimic a descending
;; staircase when perused in a sinistrodextral airt, ostending as its
;; paravaunt competence the display of text.
;; 
;; 
;; Concept
;; =======
;; The staircasemaker programming language, also adhibited the secondary
;; agnomination of "laddermaker++", as conflating in some vista with
;; a derivation from the esoteric compernage "laddermaker", replicates
;; as its dioristic expression a staircase, the steps represented by
;; a single character on each program line, while, as a parhedral
;; component, operating on a rather parvipotent tape of string-valued
;; cells.
;; 
;; == PROGRAMS LIMN A DESCENDING STAIRCASE ==
;; Administered the service of one's conspectuity, a program in this
;; language commits to the mimicry of a sequence of stairs, descending
;; concinnously in sinistrodextral procession.
;; 
;; With enhanced lealty to the forbisen's imposition, commencing from
;; the first line's indentation, which might entail an arbitrary tally
;; of spaces, each subsequent line ought to expose exactly a single
;; additional space on its sinistral side. Programs straying from this
;; weftage are inflicted with an unrecoverable error.
;; 
;; == EACH LINE ENTAILS ONE INSTRUCTION OR CHARACTER LITERAL ==
;; A successor to its indentation, a line's patration ensues from an
;; aefauld character, designating the stair's step at the horizontal
;; spatiality that the row procures.
;; 
;; With whitespaces' infliction by interdiction, no further constraint
;; intrudes into the symbol's choice. An undecimal subset therefrom
;; enjoys a dedicated role as instruction identifier, concomitant to the
;; remaining repertoire's construe as literal characters admitted to
;; contingent output upon collision.
;; 
;; The disqualification of spaces from the step's outline, however, does
;; not install their banishment from the capacity of their display, as
;; the instruction "@" is nuncupated to the output of a single space.
;; 
;; == SEVERAL MODES GOVERN THE PROGRAM FLOW ==
;; A staircasemaker program's execution is accommodated the commorancy
;; of several distinct phases, appertaining to the general actions and
;; the printing respondency.
;; 
;; The program's incipiency mandates the communication of its alacrity
;; by mediation of the "#" command, as well as its ultimate closure via
;; the token "=". No prevenient action may precede the former, nor does
;; a successor behest may trail the latter.
;; 
;; A particular printing mode permits the recognition of literals and
;; special output instructions, introduced by the "%" token and
;; concluded by adminiculum of a "&" symbol.
;; 
;; == THE PARAVAUNT SALVATORY: AN STRING TAPE ==
;; The paravaunt warklume of staircasemaker's data castaldy resides in
;; its tape, a bilaterally infinite dispansion of cells, everichon among
;; which entalented with such a capacity as to allocate an aefauld
;; string of bourneless mickleness, in its inchoate state attending to
;; an empty string.
;; 
;; A cell pointer, empight at a program's inchoation at the first cell,
;; assumes the latreutical agency of selecting any instant the currently
;; active unit, this status being a requisitum to the subject's
;; amenability to perquisitions and modulations.
;; 
;; A secondary capacitation appertains to the cell pointer's mobility,
;; expressed in the contingency for gradual translations along both
;; axes of the tape in its pursuit to harness the cell catena's
;; entirety.
;; 
;; == AN ADMINICULUM FOR USER INPUT RECEPTION ==
;; A paravail salvatory's involvement enjoys its entelechy in the user
;; input storage, representing a line of string recipient, which is
;; subsequently stored for a potential transfer unto the tape.
;; 
;; 
;; Instructions
;; ============
;; staircasemaker's operative competences tally a membership of eleven,
;; their bailiwicks the program mode's governance, as well as input and
;; output facilities.
;; 
;; Any character not amplected by the official instruction set and not
;; being a space is either printed in its ipsissima verba fashion, if
;; the encounter transpires in the printing mode, or simply experiences
;; neglect outside of such a disiplay context.
;; 
;; == OVERVIEW ==
;; The following tabulation's onus shall be a communication of
;; acquaintance with the instructions in a compendious fashion:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   #       | Starts the program.
;;           |---------------------------------------------------------
;;           | This must constitute the first instruction of a
;;           | non-empty program.
;;           |---------------------------------------------------------
;;           | If this instruction occurs more than once in a program,
;;           | an error is signaled.
;;   ..................................................................
;;   =       | Terminates the program.
;;           |---------------------------------------------------------
;;           | This must constitute the desinent instruction of a
;;           | non-empty program.
;;           |---------------------------------------------------------
;;           | If any operation follows this instruction, an error is
;;           | signaled.
;;   ..................................................................
;;   %       | Commences the printing mode.
;;           |---------------------------------------------------------
;;           | If this instruction is encountered while in the printing
;;           | mode, an error is signaled.
;;   ..................................................................
;;   &       | Terminates the printing mode.
;;           |---------------------------------------------------------
;;           | If this instruction is encountered while not in the
;;           | printing mode, an error is signaled.
;;   ..................................................................
;;   $       | Queries the standard input for a line of characters and
;;           | stores the thus received string in a provisional
;;           | salvatory.
;;           |---------------------------------------------------------
;;           | If this instruction is encountered while in the printing
;;           | mode, an error is signaled.
;;   ..................................................................
;;   @       | Prints a single space character to the standard output.
;;           |---------------------------------------------------------
;;           | If encountered outside of the printing mode, this
;;           | operation is deprieved of any efficacy.
;;   ..................................................................
;;   ?       | Escapes the next character, that is, construes the same
;;           | as a literal character, even if normally affiliated with
;;           | an operative causatum.
;;           |---------------------------------------------------------
;;           | This instruction may be appropriated as an adminiculum
;;           | for the printing of content which in the standard case
;;           | would instead instigate an operational action.
;;   ..................................................................
;;   ~       | Prints the user input commorant in the current tape cell
;;           | to the standard output conduit. If none such has been
;;           | committed yet, no output is issued.
;;           |---------------------------------------------------------
;;           | If encountered outside of the printing mode, this
;;           | operation is deprieved of any efficacy.
;;   ..................................................................
;;   !       | Stores the most recently acquired user input in the
;;           | currently selected tape cell. If none such has been
;;           | committed yet, no causatum is accompassed.
;;           |---------------------------------------------------------
;;           | If this instruction is encountered while in the printing
;;           | mode, an error is signaled.
;;   ..................................................................
;;   >       | Translates the tape's cell pointer one step to the
;;           | right.
;;           |---------------------------------------------------------
;;           | If this instruction is encountered while in the printing
;;           | mode, an error is signaled.
;;   ..................................................................
;;   <       | Translates the tape's cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | If this instruction is encountered while in the printing
;;           | mode, an error is signaled.
;;   ------------------------------------------------------------------
;; 
;; == THE ADMISSIBLE INSTRUCTIONS DEPEND UPON THE PROGRAM STATE ==
;; An instruction's homologation at a specific instant during a
;; staircasemaker program's execution registers a vinculum to the
;; concomitant execution state, the same may be subsumed into a
;; quintuple of specimens:
;; 
;;   ------------------------------------------------------------------
;;   State       | Description
;;   ------------+-----------------------------------------------------
;;   Initialized | The program has just instigated; no operation,
;;               | except for the program start command "#", may be
;;               | adduced.
;;   ..................................................................
;;   Started     | The program has started via a "#" command.
;;   ..................................................................
;;   Terminated  | The program has terminated via a "=" command. No
;;               | further action may be requested.
;;   ..................................................................
;;   Printing    | The program is amenable to printing via the "%"
;;               | command. The next encounter of a "&" will translate
;;               | the context into the "Started" state.
;;   ..................................................................
;;   Error       | An erroneous transition has occurred. This specimen
;;               | does not with veridical puissance represent a state
;;               | in the program, but rather an ultimity at its
;;               | coercive abortion.
;;   ------------------------------------------------------------------
;; 
;; Furnished with this parasceve in gnarity, a state diagram shall be
;; delivered to one's conspectuity. Please heed the incipial state's
;; tiver in the guise of the "(O)" pseudostate, being a counterpoise to
;; the terminating equivalency in "(X)".
;; 
;;                                   @, literal
;;                                      _____                  (X)
;;                                     /     \                  ^
;;                                     |     |                  |
;;                                     |     V                  |
;;          +-------------+    #     +---------+    =     +------------+
;;   (O)--->| Initialized |--------->| Started |--------->| Terminated |
;;          +-------------+          +---------+          +------------+
;;                 |                /   |    ^                  |
;;            not #|        _______/   %|    |                  |
;;                 |       /   #        |    |&                 |
;;                 V      V             V    |                  |
;;          +-------------+   %, =   +----------+               |
;;   (X)<---|    Error    |<---------| Printing |               |
;;          +-------------+    #     +----------+               |
;;                 ^                   ^      |                 |
;;                 |                   |      |                 |
;;                 |                   \______/                 |
;;                 |                  @, literals               |
;;                 |                                            |
;;                 \____________________________________________/
;;                                 any character
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its ludibund and eath nature, the staircasemaker protolog
;; is inflicted with a few inroads of ambiguity, a subset thereof shall
;; render the following treatise's cynosure.
;; 
;; == WHICH PROVENANCE DOES THE INPUT COMMAND "~" QUERY? ==
;; The principles governing the user input's handling partake of a
;; rather convolute species, its distribution rendered across a trinity
;; of stages, scilicet, the response's obtention and integration in a
;; provisional salvatory, its explicit storage in the current tape cell,
;; and its reverberation on demand.
;; 
;; The instruction "~" is apportioned that desinent dever appertaining
;; to the display onus; however, the original standard description
;; merely mentions its printing, not its provenance. This treatise
;; does neither implicate the temporary nor the permanent memorization
;; avenue.
;; 
;; It has been adjudged, forecause the tape's desuetude in any other
;; aspect renders it a paragon of mateotechny and geason inquisition,
;; to impute that the "~" operation elicits its content from the current
;; tape cell.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation experiences its reification in the
;; programming language Common Lisp, the entirety of its efficacy
;; rendered by multiple tiers of processing.
;; 
;; == STATEMENTS REPRESENT PARSED CODE LINES ==
;; The separate staircasemaker lines, supplied in the form of one string
;; and segregated into the requisite horizontal tmemata, are replicated
;; in their indentation and character content by "Statement" entities,
;; which themselves establish as an ordered aggregate the program to
;; consign to actual evaluation.
;; 
;; == A STATE MACHINE GOVERNS THE INSTRUCTION VALIDATION ==
;; The instructions in their homologation and efficacy, as imbued with
;; moderate explication in the "Instruction" tmema's subordinate parcel
;; "THE ADMISSIBLE INSTRUCTIONS DEPEND UPON THE PROGRAM STATE", which
;; please consult upon necessity, transmit their potentials to entelech
;; by mediation of the current program state.
;; 
;; The involvement of intricacies in this conditional segment of the
;; interpretation process is relayed to the services of a dedicated
;; state machine, deploying as the transitions betwixt two states the
;; respective "Statement" instance types.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-08-15
;; 
;; Sources:
;;   [esolang2022staircasemaker]
;;   The Esolang contributors, "staircasemaker", January 21st, 2022
;;   URL: "https://esolangs.org/wiki/Staircasemaker"
;;   
;;   [kyashif2019practstatemachines]
;;   Denis Kyashif, "A Practical Guide to State Machines",
;;     November 20th, 2019
;;   URL: "https://deniskyashif.com/2019/11/20/
;;         a-practical-guide-to-state-machines/"
;;   Notes:
;;     - Describes state machines in a pellucid manner.
;;     - Adduces exemplary code in the programming language C#.
;;     - The fundamental ideation:
;;       o The transition function, "ChangeState", accepts two
;;         arguments:
;;         (a) The current state.
;;         (b) The current input.
;;       o The function returns one result:
;;         (a) The new state.
;;       o The signature, in corollary, resolves to:
;;           ChangeState (currentState : State, currentInput : Input)
;;           => State
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination constitutes a furnishment
   by the TYPE-NAME and whose formal parameters are appropriated in a
   verbatim fashion from the LAMBDA-LIST, the probed object being
   stevened through the CANDIDATE-NAME, its docimasy the bailiwick of
   the BODY forms, these admitted access to the same, with the desinent
   form's primary value providing the adjudgment of the candidate's
   eligibility by returning a generalized boolean value of \"true\" for
   its confirmation, otherwise responding with ``NIL''.
   ---
   If the first BODY form resolves to a string, the same is construed
   as a documentation string to the newly defined type and hence is
   extracted for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral object greater
   than or equal to zero (0), but bourneless towards the upper extremum,
   and, as a corollary, a commorant of the range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(define-bespoke-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the
   same defaults to the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type   T)
                                        (value-type T))
  "The ``association-list-of'' type defines an associaton list, or
   alist, as a list compact of zero or more entries, each member of
   which a key-value cons, the first moiety of which attends to the
   key, this being of the KEY-TYPE, the second compartment ensconcing
   the value of the VALUE-TYPE, for both holds the default of the
   comprehensive ``T''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(define-bespoke-type hash-table-of (candidate
                                    &optional (key-type   T)
                                              (value-type T))
  "The ``hash-table-of'' type defines a hash table whose conformation
   admits zero or more entries, everichon among which defined by a key
   of the KEY-TYPE and a value complying with the VALUE-TYPE, for both
   holds the default of the comprehensive ``T''."
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
             (typep value value-type)))))

;;; -------------------------------------------------------

(deftype statement-type ()
  "The ``statement-type'' type enumerates the recognized variation on
   staircasemaker line types, incorporating therein both the instruction
   specimens and the literal character sentinel."
  '(member
    :begin-program
    :end-program
    :create-whitespace
    :begin-printing
    :end-printing
    :query-user-input
    :print-user-input
    :store-user-input
    :navigate-to-right-cell
    :navigate-to-left-cell
    :escape-next-character
    :literal-character))

;;; -------------------------------------------------------

(deftype instruction-table ()
  "The ``instruction-table'' type defines a mapping betwixt the
   staircasemaker instruction tokens and their represented operative
   statement types, its incarnation established in an association list,
   or alist, the keys of which assume characters, while the
   affiliation's answering moeity is accommodated by ``statement-type''
   objects."
  '(association-list-of character statement-type))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable staircasemaker program as
   a vector of zero or more ``Statement'' members."
  '(simple-array Statement (*)))

;;; -------------------------------------------------------

(deftype execution-state ()
  "The ``execution-state'' type enumerates the recognized states
   admissible for a staircasemaker program's execution."
  '(member
    :initialized
    :program-started
    :program-terminated
    :printing))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its role as a generalized boolean entity,
   transcripts this perspective into a veridical Boolean dichotomy, and
   returns for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for OBJECT representing ``NIL'', responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Statement".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Statement
  "The ``Statement'' class serves in the encapsulation of a parsed
   staircasemaker line, intrining its indentation, the nature of its
   content, and a representative token."
  (indentation (error "Missing indentation.")
               :type      non-negative-integer
               :read-only T)
  (type        (error "Missing statement type.")
               :type      statement-type
               :read-only T)
  (token       (error "Missing statement token.")
               :type      character
               :read-only T))

;;; -------------------------------------------------------

(defun statement-of-type-p (statement expected-type)
  "Determines whether the STATEMENT type concurs with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Statement      statement))
  (declare (type statement-type expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (statement-type statement) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (char= candidate #\Space))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction table.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type instruction-table +INSTRUCTIONS+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTIONS+
  '((#\# . :begin-program)
    (#\= . :end-program)
    (#\@ . :create-whitespace)
    (#\% . :begin-printing)
    (#\& . :end-printing)
    (#\$ . :query-user-input)
    (#\~ . :print-user-input)
    (#\! . :store-user-input)
    (#\> . :navigate-to-right-cell)
    (#\< . :navigate-to-left-cell)
    (#\? . :escape-next-character))
  "Associates the recognized instruction token with representative
   objects.")

;;; -------------------------------------------------------

(defun get-instruction-type (identifier)
  "Returns for instruction associated with the IDENTIFIER, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type character identifier))
  (the statement-type
    (or (cdr (assoc identifier +INSTRUCTIONS+ :test #'eq))
        (error "No instruction identifier: \"~c\"." identifier))))

;;; -------------------------------------------------------

(defun instruction-character-p (candidate)
  "Determines whether the CANDIDATE represents an instruction name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (assoc candidate +INSTRUCTIONS+ :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program handling operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (statements)
  "Creates and returns a fresh staircasemaker ``program'' by desuming
   its membership from the STATEMENTS list."
  (declare (type (list-of Statement) statements))
  (the program
    (make-array
      (length statements)
      :element-type     'Statement
      :initial-contents statements
      :adjustable       NIL
      :fill-pointer     NIL)))

;;; -------------------------------------------------------

(defun empty-program-p (program)
  "Determines whether the staircasemaker PROGRAM is destitute of any
   statements, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type program program))
  (the boolean
    (get-boolean-value-of
      (zerop (length program)))))

;;; -------------------------------------------------------

(defun get-program-statement-at (program index)
  "Returns the staircasemaker statement located at the zero-based INDEX
   in the PROGRAM."
  (declare (type program program))
  (declare (type fixnum  index))
  (the Statement
    (aref program index)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-non-space-character (source start)
  "Proceeding from the START position into the SOURCE, returns the
   position of the first non-space character, or, upon its
   disrespondency, responds with the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position #\Space source :start start :test #'char/=)
        (length source))))

;;; -------------------------------------------------------

(defun read-indentation (line)
  "Commencing the LINE's inchoation, tallies the number of accolent
   spaces, the same conflates with the position into the LINE of the
   first non-space character, and returns two identical values:
     (1) The number of abutting spaces at the LINE's inchoation.
     (2) The position into the LINE of the first non-space character,
         or, upon such location's absence, the LINE's length."
  (declare (type string line))
  (let ((indentation (locate-non-space-character line 0)))
    (declare (type fixnum indentation))
    (the (values non-negative-integer fixnum)
      (values indentation indentation))))

;;; -------------------------------------------------------

(defun expect-end-of-line (line start)
  "Proceeding from the START position into the LINE, expects the same
   to admit no further content, with spaces as the aefauld exemption,
   returning on confirmation no value; otherwise signals an error of an
   unspecified type."
  (declare (type string line))
  (declare (type fixnum start))
  (let ((non-space-position (locate-non-space-character line start)))
    (declare (type fixnum non-space-position))
    (unless (>= non-space-position (length line))
      (error "Expected an empty expanse commencing from the ~
              position ~d, but encountered the character \"~c\" at ~
              the index ~d."
        start (char line non-space-position) non-space-position))))

;;; -------------------------------------------------------

(defun parse-code-line (line)
  "Parses the staircasemaker program LINE and returns a ``Statement''
   representation of its content."
  (declare (type string line))
  (let ((current-position 0)
        (indentation      0))
    (declare (type fixnum        current-position))
    (declare (type (integer 0 *) indentation))
    (multiple-value-setq (indentation current-position)
      (read-indentation line))
    (symbol-macrolet ((current-character
                        (the character
                          (char line current-position))))
      (declare (type character current-character))
      (the Statement
        (cond
          ((>= current-position (length line))
            (error "Expected a literal character or an instruction at ~
                    position ~d, but found the line exhausted."
              current-position))
          ((space-character-p current-character)
            (error "Expected a literal character or an instruction ~
                    identifier at the position ~d, but encountered ~
                    a space."
              current-position))
          ((instruction-character-p current-character)
            (prog1
              (make-statement
                :indentation indentation
                :type        (get-instruction-type current-character)
                :token       current-character)
              (expect-end-of-line line
                (1+ current-position))))
          (T
            (prog1
              (make-statement
                :indentation indentation
                :type        :literal-character
                :token       current-character)
              (expect-end-of-line line
                (1+ current-position)))))))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Parses the piece of staircasemaker source CODE and returns a
   ``program'' representation of its statements."
  (declare (type string code))
  (with-input-from-string (code-stream code)
    (declare (type string-stream code-stream))
    (let ((expected-indentation 0))
      (declare (type fixnum expected-indentation))
      (flet ((validate-statement (statement first-line-p)
              "Determines whether the STATEMENT's indentation conflates
               with the EXPECTED-INDENTATION, taking into consideration
               whether FIRST-LINE-P ascertains the program to reside in
               the incipient program line, on confirmation updating the
               EXPECTED-INDENTATION and returning the STATEMENT;
               otherwise signals an error of an unspecified type."
              (declare (type Statement statement))
              (declare (type boolean   first-line-p))
              (cond
                (first-line-p
                  (setf expected-indentation
                    (1+ (statement-indentation statement))))
                ((= (statement-indentation statement)
                    expected-indentation)
                  (incf expected-indentation))
                (T
                  (error "Expected an indentation of ~d spaces, but ~
                          the statement entails ~d."
                    expected-indentation
                    (statement-indentation statement))))
              (the Statement statement)))
        (the program
          (make-program
            (loop
              for first-line-p
                of-type boolean
                =       T
                then    NIL
              for current-line
                of-type (or null string)
                =       (read-line code-stream NIL NIL)
              while current-line collect
                (validate-statement
                  (parse-code-line current-line)
                  first-line-p))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer string)
    :documentation "The tape cells as a sparse vector of string-valued
                    cells, amenable to signed integer indices in the
                    form of the hash table keys.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, responsible for selecting at any
                    instant the currently active cell by the castaldy of
                    its index, that is, its key in the CELLS table."))
  (:documentation
    "The ``Tape'' class applies itself to the representation of a
     staircasemaker program tape, its componency that of an infinite
     tally along both axes, accommodating a woning to string-valued
     cells, and operated upon by a cell pointer, the same acquires the
     dever of the currently active unit's selection."))

;;; -------------------------------------------------------

(defun make-empty-tape ()
  "Creates and returns an initially vacant ``Tape'', everichon of its
   cells initialized to the default value of zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun get-current-cell-value (tape)
  "Returns the string value stored in the current TAPE cell."
  (declare (type Tape tape))
  (the string
    (gethash
      (slot-value tape 'pointer)
      (slot-value tape 'cells)
      "")))

;;; -------------------------------------------------------

(defun set-current-cell-value (tape new-value)
  "Stores the NEW-VALUE in the TAPE's current cell and returns no
   value."
  (declare (type Tape   tape))
  (declare (type string new-value))
  (setf (gethash
          (slot-value tape 'pointer)
          (slot-value tape 'cells))
        new-value)
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          program
    :documentation "The vector of statements to process.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP), designating the
                    current index into the PROGRAM.")
   (execution-state
    :initform      :initialized
    :type          execution-state
    :documentation "The state inwith which the program currently
                    resides.")
   (tape
    :initform      (make-empty-tape)
    :type          Tape
    :documentation "The program memory as a tape of string cells.")
   (user-input
    :initform      ""
    :type          string
    :documentation "The most recently acquired user input."))
  (:documentation
    "The ``Interpreter'' class establishes an entity entalented with
     the capacitation to administer actual efficacy to a staircasemaker
     program furnished in the mold of statements."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' whose dedication assigns
   it to the staircasemaker PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``program'' to the local
   symbol macro ``$program'', its ``ip'' to ``$ip'', the
   ``execution-state'' to ``$execution-state'', the ``tape'' to
   ``$tape'', and ``user-input'' to ``$user-input'', executes the BODY
   forms, granting adit to these definitions, and returns the desinent
   form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($program
             (the program
               (slot-value ,evaluated-interpreter 'program)))
            ($ip
             (the fixnum
               (slot-value ,evaluated-interpreter 'ip)))
            ($execution-state
             (the keyword
               (slot-value ,evaluated-interpreter 'execution-state)))
            ($tape
             (the Tape
               (slot-value ,evaluated-interpreter 'tape)))
            ($user-input
             (the string
               (slot-value ,evaluated-interpreter 'user-input))))
         (declare (type program $program))
         (declare (ignorable    $program))
         (declare (type fixnum  $ip))
         (declare (ignorable    $ip))
         (declare (type keyword $execution-state))
         (declare (ignorable    $execution-state))
         (declare (type Tape    $tape))
         (declare (ignorable    $tape))
         (declare (type string  $user-input))
         (declare (ignorable    $user-input))
         ,@body))))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the staircasemaker program consigned to the
   INTERPRETER's castaldy is exhausted, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (the boolean
      (get-boolean-value-of
        (>= $ip (length $program))))))

;;; -------------------------------------------------------

(defun get-current-statement (interpreter)
  "Returns the currently active statement in the INTERPRETER's
   staircasemaker program."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (the Statement
      (if (program-exhausted-p interpreter)
        (error "The program is exhausted at the IP position ~d." $ip)
        (aref $program $ip)))))

;;; -------------------------------------------------------

(defgeneric change-state (current-state
                          input
                          current-statement
                          interpreter)
  (:documentation
    "Executes a specific action during the transition from the
     CURRENT-STATE by mediation of the INPUT statement type, the same
     is desumed from the CURRENT-STATEMENT, executed in the
     INTERPRETER's context, and returns no value."))

;;; -------------------------------------------------------

(defmacro define-state-transition ((current-state input) &body body)
  "Defines an implementation of the generic function ``change-state'',
   its first formal parameter nevened automatically, while dispatching
   on the CURRENT-STATE, the second agnomination chosen similiter, here
   dispatching on the statement type INPUT, the third parameter name
   is norned in a fixed manner as $current-statement, the fourth
   $interpreter, evaluates the BODY forms, and returns no value."
  (let ((current-state-name (gensym))
        (input-name         (gensym)))
    (declare (type symbol current-state-name))
    (declare (type symbol input-name))
    `(defmethod change-state ((,current-state-name (eql ,current-state))
                              (,input-name         (eql ,input))
                              ($current-statement  Statement)
                              ($interpreter        Interpreter))
       (declare (type keyword        ,current-state-name))
       (declare (ignore              ,current-state-name))
       (declare (type statement-type ,input-name))
       (declare (ignore              ,input-name))
       (declare (type Statement      $current-statement))
       (declare (ignorable           $current-statement))
       (declare (type Interpreter    $interpreter))
       (declare (ignorable           $interpreter))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-state-transition (:initialized :begin-program)
  (with-interpreter ($interpreter)
    (setf $execution-state :program-started)))

;;; -------------------------------------------------------

(define-state-transition (:initialized :end-program)
  (error "Cannot stop the program if not already started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :create-whitespace)
  (error "Cannot create whitespaces if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :begin-printing)
  (error "Cannot begin printing if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :end-printing)
  (error "Cannot end printing if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :query-user-input)
  (error "Cannot query for user input if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :print-user-input)
  (error "Cannot print user input if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :store-user-input)
  (error "Cannot store user input if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :navigate-to-right-cell)
  (error "Cannot navigate to right cell if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :navigate-to-left-cell)
  (error "Cannot navigate to left cell if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :escape-next-character)
  (error "Cannot escape next character if the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:initialized :literal-character)
  (error "Cannot print literal character the program has not already ~
          started."))

;;; -------------------------------------------------------

(define-state-transition (:program-started :begin-program)
  (error "The program has already started."))

;;; -------------------------------------------------------

(define-state-transition (:program-started :end-program)
  (with-interpreter ($interpreter)
    (setf $execution-state :program-terminated)))

;;; -------------------------------------------------------

(define-state-transition (:program-started :create-whitespace))

;;; -------------------------------------------------------

(define-state-transition (:program-started :begin-printing)
  (with-interpreter ($interpreter)
    (setf $execution-state :printing)))

;;; -------------------------------------------------------

(define-state-transition (:program-started :end-printing)
  (error "Cannot end printing if not in the printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:program-started :query-user-input)
  (finish-output)
  (with-interpreter ($interpreter)
    (setf $user-input
      (read-line NIL NIL "")))
  (clear-input))

;;; -------------------------------------------------------

(define-state-transition (:program-started :print-user-input)
  (error "Cannot print user input if not in the printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:program-started :store-user-input)
  (with-interpreter ($interpreter)
    (set-current-cell-value $tape $user-input)))

;;; -------------------------------------------------------

(define-state-transition (:program-started :navigate-to-right-cell)
  (with-interpreter ($interpreter)
    (move-cell-pointer-right $tape)))

;;; -------------------------------------------------------

(define-state-transition (:program-started :navigate-to-left-cell)
  (with-interpreter ($interpreter)
    (move-cell-pointer-left $tape)))

;;; -------------------------------------------------------

(define-state-transition (:program-started :escape-next-character)
  (with-interpreter ($interpreter)
    (incf $ip)))

;;; -------------------------------------------------------

(define-state-transition (:program-started :literal-character))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :begin-program)
  (error "The program has already started."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :end-program)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :create-whitespace))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :begin-printing)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :end-printing)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :query-user-input)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :print-user-input)
  (error "Cannot print user input if not in the printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :store-user-input)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :navigate-to-right-cell)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :navigate-to-left-cell)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :escape-next-character)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:program-terminated :literal-character)
  (error "Cannot perform any action if the program is terminated."))

;;; -------------------------------------------------------

(define-state-transition (:printing :begin-program)
  (error "The program has already started."))

;;; -------------------------------------------------------

(define-state-transition (:printing :end-program)
  (error "Cannot terminate the program while in printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:printing :create-whitespace)
  (format T " "))

;;; -------------------------------------------------------

(define-state-transition (:printing :begin-printing)
  (error "Already in printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:printing :end-printing)
  (with-interpreter ($interpreter)
    (setf $execution-state :program-started)))

;;; -------------------------------------------------------

(define-state-transition (:printing :query-user-input)
  (error "Cannot query for user input while in printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:printing :print-user-input)
  (with-interpreter ($interpreter)
    (format T "~a"
      (get-current-cell-value $tape))))

;;; -------------------------------------------------------

(define-state-transition (:printing :store-user-input)
  (error "Cannot store user input while in printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:printing :navigate-to-right-cell)
  (error "Cannot move cell pointer right while in printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:printing :navigate-to-left-cell)
  (error "Cannot move cell pointer left while in printing mode."))

;;; -------------------------------------------------------

(define-state-transition (:printing :escape-next-character)
  (with-interpreter ($interpreter)
    (incf $ip)
    (if (program-exhausted-p $interpreter)
      (error "Missing symbol to escape.")
      (format T "~c"
        (statement-token
          (get-current-statement $interpreter))))))

;;; -------------------------------------------------------

(define-state-transition (:printing :literal-character)
  (format T "~c"
    (statement-token $current-statement)))

;;; -------------------------------------------------------

(defun process-current-statement (interpreter)
  "Evaluates the INTERPRETER's current statement and returns no value."
  (declare (type Interpreter interpreter))
  (let ((current-statement (get-current-statement interpreter)))
    (declare (type Statement current-statement))
    (change-state
      (slot-value interpreter 'execution-state)
      (statement-type current-statement)
      current-statement
      interpreter))
  (values))

;;; -------------------------------------------------------

(defun validate-terminal-program-state (interpreter)
  "Determines whether the staircasemaker program consigned to the
   INTERPRETER's castaldy contemporaneously maintains its woning in the
   terminal execution state ``:program-terminated'', on confirmation
   returning no value; otherwise an error of an unspecified type is
   signaled.
   ---
   This operation's intention appertains to a concluding assessment at
   the program's termination in order to ascertain that a desinent
   program halting instruction (\"=\") is empight at the ultimity's
   location."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (unless (eq $execution-state :program-terminated)
      (error "Missing concluding program termination (\"=\").")))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Executes the staircasemaker program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (unless (empty-program-p $program)
      (loop until (program-exhausted-p interpreter) do
        (process-current-statement interpreter)
        (incf $ip))
      (validate-terminal-program-state interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-staircasemaker (code)
  "Interprets the piece of staircasemaker source CODE and returns no
   value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello world!".
(interpret-staircasemaker
  "#
 %
  H
   e
    l
     l
      o
       @
        w
         o
          r
           l
            d
             ?
              !
               &
                =")

;;; -------------------------------------------------------

;; Query the user for their name and repeat that input.
(interpret-staircasemaker
  "#
 %
  W
   h
    a
     t
      '
       s
        @
         y
          o
           u
            r
             @
              n
               a
                m
                 e
                  ?
                   ?
                    @
                     &
                      $
                       !
                        %
                         Y
                          o
                           u
                            r
                             @
                              n
                               a
                                m
                                 e
                                  @
                                   i
                                    s
                                     @
                                      ~
                                       .
                                        &
                                         =")

;;; -------------------------------------------------------

;; Query separately for the first and the last name, store these in two
;; dedicated tape cells, and print the twissel in a single sentence.
(interpret-staircasemaker "#
 %
  F
   i
    r
     s
      t
       @
        n
         a
          m
           e
            :
             @
              &
               $
                !
                 >
                  %
                   L
                    a
                     s
                      t
                       @
                        n
                         a
                          m
                           e
                            :
                             @
                              &
                               $
                                !
                                 <
                                  %
                                   H
                                    e
                                     l
                                      l
                                       o
                                        ,
                                         @
                                          ~
                                           @
                                            &
                                             >
                                              %
                                               ~
                                                .
                                                 &
                                                  =")
