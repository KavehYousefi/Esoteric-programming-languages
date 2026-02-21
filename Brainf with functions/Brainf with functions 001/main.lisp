;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brainf with functions", invented by the Esolang user
;; "Cinnamony" and presented on June 23rd, 2023, the diorism of which
;; wones in its extension of Urban Mueller's "brainfuck" by an
;; instruction twain from whose champarty ensues the polymechany of a
;; function definition mechanism, capacitated to affiliate code tmemata
;; with a single-character identifier for their subsequent referral.
;; 
;; 
;; Concept
;; =======
;; The "Brainf with functions" programming language constitutes an
;; extension's adhibition to the brainfuck specimen, the capacitations
;; of which are valorized by the ability to define functions, amenable
;; to one-character identifiers, whose statements may be applied by
;; future invocations. 
;; 
;; == FUNCTION NAMES ARE SINGLE-CHARACTER IDENTIFIERS ==
;; A function name's agnomination constitutes an aefauld character's
;; dation, empight in prevenience to the instigating ":" symbol, whence
;; the body statements ensue.
;; 
;; The homologation of the stevening's choice does not exclude any
;; character except for the decimal accompt occupied by the language's
;; autochthonous keywords, scilicet:
;; 
;;   ----------------------------------------------
;;   Reserved symbol | Purpose
;;   ----------------+-----------------------------
;;   >               | Move the cell pointer right
;;   ..............................................
;;   <               | Move the cell pointer left
;;   ..............................................
;;   +               | Increment the current cell
;;   ..............................................
;;   -               | Decrement the current cell
;;   ..............................................
;;   .               | Ouput the current cell
;;   ..............................................
;;   ,               | Input into the current cell
;;   ..............................................
;;   [               | Jump forward
;;   ..............................................
;;   ]               | Jump back
;;   ..............................................
;;   :               | Start the function body
;;   ..............................................
;;   ;               | Conclude the function body
;;   ----------------------------------------------
;; 
;; == FUNCTIONS DEFINITIONS ARE ADMITTED AT RUNTIME ==
;; The function definitions' evaluation, inwith whose terminology is
;; amplected the identifying name's association with the body's
;; statements, avaunts as a sequela of the instruction pointer's (IP)
;; progression, rather than as a compile time's epiphonemnon: The
;; function is parsed and memorized during the run-time evaluation of
;; the "Brainf with functions" code; any prevenience in the associated
;; name remains in carency of incited causata.
;; 
;; As a forbisen adduced, in the course of the code tmema
;; 
;;   F +, F:[-]; F
;;   |    |      |
;;  (1)  (2)    (3)
;; 
;; The symbol "F" at the position (1) does not peract the activation of
;; the function "F", defined at the latter location (2), treating the
;; same as a commentary content. The instance "F" at the position (3),
;; however, succeeding the function registration at (2), recognizes the
;; functional object, executing, as a consectary, the incorporated
;; operations.
;; 
;; == FUNCTION STATEMENTS OUGHT TO FORM CLOSED UNITS ==
;; Statements enhalsed in a function body's context intend to establish
;; an independent aggregate from the surrounding program; this nomothesy
;; appertains, in particular, to the jump point matching principle.
;; 
;; For example, the following "[" and "]" instructions, with the former
;; a function definition's component, and the latter an outwith
;; operation, will inflict an abortive error to the program:
;; 
;;   F:+[; ]
;;      ^  ^
;;      |  |
;;      +--+--- These jump points are both regarded as unterminated.
;; 
;; == FUNCTION REDEFINITIONS ARE ADMINISTERED TOLERANCE ==
;; A function name may be subjected to a redefinition at any instant and
;; tally, with the association appropriating the freshest recency that
;; of the contemporaneous validity's obtainal.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of the language recipiency does not elude brainfuck's
;; architecture, appropriating in an ipsissima verba fashion a
;; bilaterally bourneless dispansion of unsigned byte-valued cells.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; 
;; Instructions
;; ============
;; "Brainf with functions"'s instruction set represents a derivation
;; from brainfuck's octuple cleronomy, eked by a twissel of members as
;; an additament pursuing the telos of function definitions'
;; capacitation.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be ostended in a requisite mete
;; of gnarity's adhibition anent the language's operative warklumes.
;; 
;; Please heed the demarcation of succedaneous tmemata by a catena
;; whose plasmature ensues from asterisks ("*"), intended for their
;; supersession by actual "Brainf with functions" code in the ultimate
;; program.
;; 
;; Please also heed the coalescence of the instruction jumelle "[" and
;; "]" into a singular command.
;; 
;;   ------------------------------------------------------------------
;;   Command          | Effect
;;   -----------------+------------------------------------------------
;;   >                | Translates the cell pointer one step to the
;;                    | right.
;;   ..................................................................
;;   <                | Translates the cell pointer one step to the
;;                    | left.
;;   ..................................................................
;;   +                | Increments the current cell value by a
;;                    | mountance of one (1). Upon the upper bourne's
;;                    | transgression, the cell state wraps around from
;;                    | 255 to zero (0).
;;   ..................................................................
;;   -                | Decrements the current cell value by a
;;                    | mountance of one (1). Upon the lower bourne's
;;                    | transgression, the cell state wraps around from
;;                    | zero (0) to 255.
;;   ..................................................................
;;   .                | Prints the character whose ASCII code conflates
;;                    | with the current cell value to the standard
;;                    | output conduit.
;;   ..................................................................
;;   ,                | Queries the standard input conduit for a
;;                    | character and stores its ASCII code in the
;;                    | current cell.
;;   ..................................................................
;;   [statements]     | While the current cell value equals zero (0),
;;    **********      | executes the {statements}.
;;                    |------------------------------------------------
;;                    | {statements} must constitute an ordered
;;                    | sequence comprised of zero or more
;;                    | instructions.
;;   ..................................................................
;;   name:statements; | Defines a new function identified by the {NAME}
;;   **** **********  | and comprehending the {statements} to execute
;;                    | upon its invocation.
;;                    |------------------------------------------------
;;                    | {name} must constitute exactly one character
;;                    | not conflating with the decimal reserved
;;                    | keyword identifiers "+", "-", ">", "<", ".", 
;;                    | ",", ":", and ";".
;;                    |------------------------------------------------
;;                    | {statements} must constitute an ordered
;;                    | sequence comprised of zero or more
;;                    | instructions.
;;                    |------------------------------------------------
;;                    | If no {name} precedes the ":" token, an error
;;                    | of the type "MissingFunctionNameError" is
;;                    | signaled.
;;                    |------------------------------------------------
;;                    | If the {name} concurs with one of the ten
;;                    | reserved language keywords, an error of the
;;                    | type "InvalidFunctionNameError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation accompts for a manifestation in
;; the Common Lisp programming language, its patration two strata's
;; champarty; imprimis, a scannerless parsing of the source code string
;; into an abstract syntax tree (AST); further, and desitive, this
;; node hierarchy's peragration in pursuit of its evaluation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-02-19
;; 
;; Sources:
;;   [esolang:2023:Brainf with functions]
;;   The Esolang contributors, "Brainf with functions", June 23rd, 2023
;;   URL: "https://esolangs.org/wiki/Brainf_with_functions"
;;   
;;   [goodrich:2006:datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 120--127 describe the doubly linked list.
;;       o The page 120 presents an implementation of a doubly linked
;;         list node in Java, norned "DNode".
;;       o The pages 125--127 present an implementation of a doubly
;;         linked list in Java.
;;     
;;     - The pages 213--216 describe the double-ended queue, or deque,
;;       abstract data type (ADT).
;;       o The pages 215--216 present a partial implementation in Java
;;         utilizing a doubly linked list.
;;     
;;     - The pages 231-241 describe the node list abstract data type
;;       (ADT).
;;       o This data type utilizes the notion of "positions" in order
;;         to furnish an abstraction of nodes for its elements' access.
;;       o The pages 234--235 describe an interface for the node list
;;         ADT, nevened "PositionList".
;;       o The page 235 mentions the equivalency of the node list
;;         operations and the deque counterparts.
;;       o The pages 236--241 present an implementation of the node list
;;         ADT via a doubly linked list, the product being yclept the
;;         "NodePositionList".
;;   
;;   [goodrich:2014:datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 132--137 describe the concept and an implementation
;;       of the doubly linked list in the Java programming language.
;;       o The pages 135--137 furnish the implementation.
;;     
;;     - The pages 248--251 describe the concept and implementation of
;;       the double-ended queue, or deque, abstract data type (ADT).
;;       o The pages 250--251 describe the implementation of a deque via
;;         a doubly linked list.
;;     
;;     - The pages 276--280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-bespoke-type
    (type-name (&rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination constitutes the NAME's
   dation, and which acts as a pernor to the LAMBDA-LIST's ipsissima
   verba specifications as its personal formal parameters, concomitantly
   assigning the probed object to the fixed local variable
   ``$candidate'', evaluates the BODY forms, and construes the desinent
   form's primary return value as the docimasy's adjudgment, a
   \"generalized boolean\" truth value of \"true\" peracting a
   successful compatibility's assessment's signification, while a
   \"false\" response concludes in the candidate's rejection.
   ---
   The first BODY form, in the case of its resolution to a string
   object, is adhibited the role of a documentation string to the type
   definition, being, as a corollary, reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda ($candidate)
               (declare (type T    $candidate))
               (declare (ignorable $candidate))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-bespoke-type list-of (&optional (element-type '*))
  "The ``list-of'' type defines a linked list whose componency is
   edified upon zero or more elements' participation, each such member
   subsumed into the ELEMENT-TYPE, for thilk governs the default
   configuration of the generic sentinel ``*''."
  (and
    (listp $candidate)
    (loop
      for    current-element of-type T in (the list $candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(define-a-bespoke-type hash-table-of (&optional (key-type   '*)
                                                (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each key of such adhering to the
   KEY-TYPE and associating with a value of the VALUE-TYPE, for both is
   imposed the generic sentinel ``*'' as the default state."
  (and
    (hash-table-p $candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table $candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list of abstract syntax
   tree (AST) nodes, enumerating a membership of zero (0) or more
   elements, and realized as a linked list of ``AST-Node'' objects."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value edified from an
   ogdoad of attiguous bits, thus constituting an incolant of the closed
   integer interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the abstract syntax tree (AST) nodes.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface serves in the establishment of a common
   foundry entreparted by all classes pursuing the representation of
   \"Brainf with functions\" language facilities in an abstract syntax
   tree (AST) node's guise.")

;;; -------------------------------------------------------

(defstruct (Identifier-Node
  (:include     AST-Node)
  (:constructor make-an-identifier-node (name)))
  "The ``Identifier-Node'' class applies itself to the representation of
   a single-character token in an abstract syntax tree (AST) node's
   plasmature."
  (name (error "No identifier name has been communicated.")
        :type      character
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include     AST-Node)
  (:constructor make-a-block-node (statements)))
  "The ``Block-Node'' class applies itself to the representation of a,
   contingently empty, ordered list of abstract syntax tree (AST) nodes
   in a nodular plasmature."
  (statements (error "No statements have been communicated for the ~
                      block node.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Iterance-Node
  (:include     AST-Node)
  (:constructor make-an-iterance-node (statements)))
  "The ``Iterance-Node'' class applies itself to the representation of
   a loop construct in an abstract syntax tree (AST) node's plasmature."
  (statements (error "No iterance body has been communicated.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Function-Definition-Node
  (:include     AST-Node)
  (:constructor make-a-function-definition-node (name statements)))
  "The ``Function-Definition-Node'' class applies itself to the
   representation of a function definition in an abstract syntax tree
   (AST) node's plasmature."
  (name       (error "No function definition name has been ~
                      communicated.")
              :type      character
              :read-only T)
  (statements (error "No function definition node body has been ~
                      communicated.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include     AST-Node)
  (:constructor make-a-program-node (statements)))
  "The ``Program-Node'' class applies itself to the representation of a
   parsed \"Brainf with functions\" program in an abstract syntax tree
   (AST) node's plasmature."
  (statements (error "No statements have been communicated for the ~
                      program node.")
              :type      Block-Node
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Brainf-With-Functions-Error (error)
  ()
  (:documentation
    "The ``Brainf-With-Functions-Error'' condition type furnishes a
     common firmament intended for all condition types in a pursuit of
     representing an anomalous situation during a
     \"Brainf with functions\" program's analyzation, parsing,
     evaluation, or execution."))

;;; -------------------------------------------------------

(define-condition Missing-Function-Name-Error
  (Brainf-With-Functions-Error)
  ((position
    :initarg       :position
    :initform      0
    :type          fixnum
    :reader        missing-function-name-error-position
    :documentation "The zero-based index into the \"Brainf with
                    functions\" source code string at which this
                    anomaly has been instigated, and which may serve
                    as an optional vouchsafement of ponibility for
                    reporting operations."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Function-Name-Error condition))
      (declare (type stream                      stream))
      (format stream
        "A function definition, introduced by the character \":\" ~
         and in carency of a prevenient identifier has been detected ~
         at the position ~d."
        (missing-function-name-error-position condition))
      (values)))
  (:documentation
    "The ``Missing-Function-Name-Error'' condition type serves in the
     apprizal about an anomalous situation emerging from the attempt to
     define a function without a preveniently established name."))

;;; -------------------------------------------------------

(define-condition Invalid-Function-Name-Error
  (Brainf-With-Functions-Error)
  ((name
    :initarg       :name
    :initform      (error "No offending name has been committed.")
    :reader        invalid-function-name-error-name
    :type          character
    :documentation "The offending function name, whose fixated vinculum
                    with an autochthonous command identifier has
                    incurred this anomalous circumstance."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Function-Name-Error condition))
      (declare (type stream                      stream))
      (format stream "The name \"~c\" cannot be assigned to a bespoke ~
                      function, as it designates an autochthonous ~
                      command name."
        (invalid-function-name-error-name condition))
      (values)))
  (:documentation
    "The ``Invalid-Function-Name-Error'' condition serves in the
     apprizal of an anomalous situation whose etiology wones in the
     attempt to define a bespoke function with a name reserved by the
     native \"Brainf with functions\" operations."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean truth value from the same, returning
   for a non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autochthonous-identifier-p (candidate)
  "Determines whether the CANDIDATE represents a reserved identifier,
   such enjoys a fixed parcery of significance by which the same is
   eloigned from the contingency of a reappropriation for a bespoke
   function name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (find candidate "><+-,.[]:;" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexical analyzer.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source*))
(declaim (type fixnum        *current-position*))
(declaim (type character     *current-character*))
(declaim (type boolean       *source-is-exhausted-p*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The piece of \"Brainf with functions\" source code to parse.")

(defparameter *source-length*
  (length *source*)
  "The tally of characters participating in the ``*SOURCE*'' string.")

(defparameter *current-position* 0
  "The contemporaneous zero-based index into the ``*SOURCE*'' string.")

(define-symbol-macro *current-character*
  (the character
    (schar *source* *current-position*)))

(define-symbol-macro *source-is-exhausted-p*
  (the boolean
    (not (array-in-bounds-p *source* *current-position*))))

;;; -------------------------------------------------------

(defun set-the-source-to (new-source)
  "Sets the ``*SOURCE*'' string to parse to the NEW-SOURCE, resets all
   state variables entalented with pertinence for the expected gestion,
   and returns no value."
  (declare (type string new-source))
  (psetf
    *source*           (coerce new-source 'simple-string)
    *current-position* 0)
  (setf
    *source-length*    (length *source*))
  (values))

;;; -------------------------------------------------------

(defun advance-to-the-next-character ()
  "Advances the ``*SOURCE*'' string's position cursor,
   ``*CURRENT-POSITION*'', to the next character in its source, if
   possible, and returns no value."
  (setf *current-position*
    (min
      (1+ *current-position*)
      *source-length*))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-next-command ()
  "Parses the next command from the underlying ``*SOURCE*'' string and
   returns a connable ``AST-Node'' representation thereof; or, upon its
   source's exhaustion, responds with the sentinel ``NIL''."
  (the (or null AST-Node)
    (unless *source-is-exhausted-p*
      (case *current-character*
        ;; Function definition instigator ":" detected?
        (#\:
          (error 'Missing-Function-Name-Error
            :position *current-position*))
        
        ;; Unmatched function definition terminator ";" detected?
        (#\;
          (error "A function definition terminator, signified by the ~
                  character \"~c\" has been detected at the position ~
                  ~d."
            *current-character* *current-position*))
        
        ;; Loop instigator "[" detected?
        (#\[
          (advance-to-the-next-character)
          
          (make-an-iterance-node
            (make-a-block-node
              (loop
                if *source-is-exhausted-p* do
                  (error "The source is exhausted without a loop's ~
                          closure at the position ~d."
                    *current-position*)
                else if (char= *current-character* #\]) do
                  (advance-to-the-next-character)
                  (loop-finish)
                else collect
                  (parse-the-next-command)))))
        
        ;; Unmatched loop terminator "]" detected?
        (#\]
          (error "An unmatched \"~c\" has been encountered at the
                  position ~d."
            *current-character* *current-position*))
        
        ;; Simple identifier or function definition follow?
        (otherwise
          (let ((identifier *current-character*))
            (declare (type character identifier))
            (advance-to-the-next-character)
            
            (cond
              ((and (not   *source-is-exhausted-p*)
                    (char= *current-character* #\:))
                (advance-to-the-next-character)
                
                (make-a-function-definition-node identifier
                  (make-a-block-node
                    (loop
                      if *source-is-exhausted-p* do
                        (error "The source is exhausted without a ~
                                function's closure at the position ~d."
                          *current-position*)
                      else if (char= *current-character* #\;) do
                        (advance-to-the-next-character)
                        (loop-finish)
                      else collect
                        (parse-the-next-command)))))
              (T
                (make-an-identifier-node identifier)))))))))

;;; -------------------------------------------------------

(defun parse-the-program ()
  "Parses a \"Brainf with functions\" program and returns a
   ``Program-Node'' representation of its ensconced statements."
  (the Program-Node
    (make-a-program-node
      (make-a-block-node
        (loop
          for next-command
            of-type (or null AST-Node)
            =       (parse-the-next-command)
          while next-command
            collect next-command)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the function table.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Function-Table ()
  ((definitions
    :initform      (make-hash-table :test #'eql)
    :accessor      function-table-definitions
    :type          (hash-table-of character Function-Definition-Node)
    :documentation "Maps the identifying function name, this resolving
                    to an aefauld character, to the representative
                    ``Function-Definition-Node'', whence its diorism
                    ensues."))
  (:documentation
    "The ``Function-Table'' class is apportioned the onus of the bespoke
     function definitions' castaldy by associating the identifying name
     symbols with the originating ``Function-Definition-Node''
     instances, the statements of which serves in the ensconcement of
     the function body."))

;;; -------------------------------------------------------

(defun make-an-empty-function-table ()
  "Creates and returns an initially empty ``Function-Table''."
  (the Function-Table
    (make-instance 'Function-Table)))

;;; -------------------------------------------------------

(defun register-the-function (table name tree)
  "Registers the function definition's abstract syntax TREE (AST) node
   with the NAME at the function TABLE and returns no value.
   ---
   Upon the presence of an entry with the NAME in the TABLE, the extant
   diorism is expunged in favor of the newly created alligation.
   ---
   If the NAME designates a built-in \"Brainf with functions\" command
   identifier, an error of the type ``Invalid-Function-Nmae-Error'' is
   signaled."
  (declare (type Function-Table           table))
  (declare (type character                name))
  (declare (type Function-Definition-Node tree))
  (if (autochthonous-identifier-p name)
    (error 'Invalid-Function-Name-Error :name name)
    (setf (gethash name (function-table-definitions table))
          tree))
  (values))

;;; -------------------------------------------------------

(defun function-with-the-name-exists-p (table name)
  "Determines whether a function definition amenable to the NAME exits
   in the function TABLE, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Function-Table table))
  (declare (type character      name))
  (the boolean
    (nth-value 1
      (gethash name
        (function-table-definitions table)))))

;;; -------------------------------------------------------

(defun look-up-the-function (table name)
  "Returns the ``Function-Definition-Node'' affiliated with the NAME in
   the function TABLE, or, upon its disrespondency, produces the ``NIL''
   sentinel as an absence marker."
  (declare (type Function-Table table))
  (declare (type character      name))
  (the (or null Function-Definition-Node)
    (nth-value 0
      (gethash name
        (function-table-definitions table)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape cell.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-a-new-cell (&optional (predecessor NIL)
                                           (successor   NIL))))
  "The ``Cell'' class implements a cell into the memory ``Tape'',
   realized as doubly linked list node; as a corollary comprehending
   the unsigned byte-valued datum, as well as a jumelle of optional
   pointers to the predecessor and successor cells."
  (value       0
               :type      octet
               :read-only NIL)
  (predecessor NIL
               :type      (or null Cell)
               :read-only NIL)
  (successor   NIL
               :type      (or null Cell)
               :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((header
    :initform      (make-a-new-cell NIL NIL)
    :type          Cell
    :documentation "The front node into the tape's doubly linked list,
                    serving as a sentinel, always inclavated at the
                    leftmost position in order to facilitate
                    insertions.")
   (trailer
    :initform      (make-a-new-cell NIL NIL)
    :type          Cell
    :documentation "The desinent node into the tape's doubly linked
                    list, serving as a sentinel, always inclavated at
                    the rightmost position in order to facilitate
                    insertions.")
   (pointer
    :initform      (make-a-new-cell NIL NIL)
    :type          Cell
    :documentation "A reference to the current selected cell.
                    ---
                    The cell pointer, albeit motile in its capacitation,
                    will never assume neither the HEADER nor the
                    TRAILER sentinels."))
  (:documentation
    "The ``Tape'' class furnishes an implementation of a bilaterally
     infinite dispansion of unsigned byte-valued cells, operated upon by
     a mobile cell pointer which at any instant designates the currently
     active entity, the sole cell endowed with amenability to
     perquisitions and modulations.
     ---
     This tape implementation's substratum is accommodated by a doubly
     linked list, along both lateralities involving sentinel nodes."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tape Tape) &key)
  "Establishes the vincula atwixen the inchoate node, represented by the
   TAPE's pointer, the header and the trailer sentinels, and returns no
   value."
  (declare (type Tape tape))
  (with-slots (header trailer pointer) tape
    (declare (type Cell header))
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (psetf
      (cell-successor   header)  pointer
      (cell-predecessor pointer) header
      (cell-successor   pointer) trailer
      (cell-predecessor trailer) pointer))
  (values))

;;; -------------------------------------------------------

(defun make-a-pristine-tape ()
  "Creates and returns a fresh ``Tape'' whose state of inchoacy
   incorporates an aefauld, zero-valued cell."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun insert-a-cell-atwixen (predecessor successor)
  "Inserts a fresh cell atwixen the PREDECESOR and SUCCESOR node, its
   state's configuration compliant with the default plasmature, and
   returns the thus yielded cell."
  (declare (type Cell predecessor))
  (declare (type Cell successor))
  (the Cell
    (let ((new-cell (make-a-new-cell predecessor successor)))
      (declare (type Cell new-cell))
      (psetf
        (cell-successor   predecessor) new-cell
        (cell-predecessor successor)   new-cell)
      new-cell)))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Relocates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (with-slots (header pointer) tape
    (declare (type Cell header))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-predecessor pointer) header)
        (insert-a-cell-atwixen header pointer)
        (cell-predecessor pointer))))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Relocates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (with-slots (trailer pointer) tape
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-successor pointer) trailer)
        (insert-a-cell-atwixen pointer trailer)
        (cell-successor pointer))))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (cell-value
      (slot-value tape 'pointer))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping into the valid unsigned byte
   range of [0, 255], and returns no value."
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf (cell-value pointer)
      (mod new-value 256)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "No program node has bee communicated.")
    :type          Program-Node
    :documentation "The root of the abstract syntax tree (AST)
                    representing the \"Brainf with functions\" program
                    to execute.")
   (functions
    :initform      (make-an-empty-function-table)
    :type          Function-Table
    :documentation "Maps the bespoke function definition's names to
                    the representative ``Function-Definition-Node''s.")
   (tape
    :initform      (make-a-pristine-tape)
    :type          Tape
    :documentation "THe program memory as a bilaterally infinite
                    mountenance of unsigned byte-valued cells."))
  (:documentation
    "The ``Interpreter'' class serves in the accoutrement of an entity
     responsible for the accompassing of actual efficacy to a
     \"Brainf with functions\" program concredited in the plasmature of
     an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-an-interpreter-for (tree)
  "Creates and returns a fresh ``Interpreter'' concredited with the
   execution of the \"Brainf with functions\" program communicated in
   the abstract syntax TREE's (AST) plasmature."
  (declare (type Program-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defgeneric visit-the-node (interpreter node)
  (:documentation
    "Sojourns the abstract syntax tree (AST) NODE, utilizing the
     INTERPRETER as the visitor, and returns no value."))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (visit-the-node interpreter
    (program-node-statements node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Block-Node))
  (declare (type Interpreter interpreter))
  (declare (type Block-Node  node))
  (dolist (current-child-node (block-node-statements node))
    (declare (type AST-Node current-child-node))
    (visit-the-node interpreter current-child-node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Function-Definition-Node))
  (declare (type Interpreter              interpreter))
  (declare (type Function-Definition-Node node))
  (with-slots (functions) interpreter
    (declare (type Function-Table functions))
    (register-the-function functions
      (function-definition-node-name node)
      node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Identifier-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Identifier-Node node))
  
  (with-slots (tape functions) interpreter
    (declare (type Tape           tape))
    (declare (ignorable           tape))
    (declare (type Function-Table functions))
    (declare (ignorable           functions))
    
    (let ((name (identifier-node-name node)))
      (declare (type character name))
      (case name
        (#\<
          (move-the-cell-pointer-left tape))
        
        (#\>
          (move-the-cell-pointer-right tape))
        
        (#\+
          (incf (current-cell-value tape)))
        
        (#\-
          (decf (current-cell-value tape)))
        
        (#\.
          (format *query-io* "~c"
            (code-char
              (current-cell-value tape)))
          (finish-output *query-io*))
        
        (#\,
          (format *query-io* "~&>> ")
          (finish-output *query-io*)
          (setf (current-cell-value tape)
            (char-code
              (read-char *query-io* NIL #\Null)))
          (clear-input *query-io*))
        
        ;; Not a native instruction?
        ;; => Invoke custom function, if associated with the NAME.
        (otherwise
          (when (function-with-the-name-exists-p functions name)
            (visit-the-node interpreter
              (function-definition-node-statements
                (look-up-the-function functions name))))))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Iterance-Node))
  (declare (type Interpreter   interpreter))
  (declare (type Iterance-Node node))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (loop until (zerop (current-cell-value tape)) do
      (visit-the-node interpreter
        (iterance-node-statements node))))
  (values))

;;; -------------------------------------------------------

(defun start-the-interpreter (interpreter)
  "Executes the \"Brainf with functions\" program concredited to the
   INTERPRETER's castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (visit-the-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-brainf-with-functions (code)
  "Interprets the piece of \"Brainf with functions\" source CODE and
   returns no value."
  (declare (type string code))
  (set-the-source-to code)
  (start-the-interpreter
    (make-an-interpreter-for
      (parse-the-program)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-brainf-with-functions "n:,.;+[n]")

;;; -------------------------------------------------------

;; Print the message "HIWORLD".
(interpret-brainf-with-functions
  "o:++++++++;
   c:[<+>-]<;
   f:+++++++[>o++<-]>c;
   f>f>f>f>f>f>f<<<<<<++.>+++.>oo+.>o+.>o++++.>++++++.>--.")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; FUNCTION DEFINITIONS
;; ====================
;; A quadruple contingency of functions serve to filst in a more
;; compendious code designment's chevisance.
;; 
;; Please heed their agnomination's listing in the sinistral column,
;; designated by "F", for "function", in the following tabulation.
;; 
;;   ------------------------------------------------------------------
;;   F | Causatum
;;   --+---------------------------------------------------------------
;;   A | Increments the current cell by a mountance of eight (8).
;;   ..................................................................
;;   B | Increments the current cell by a mountance of eight (8).
;;   ..................................................................
;;   R | Resets the current cell to zero (0).
;;   ..................................................................
;;   C | Stores the value 48 (ASCII code of the character "0") in the
;;     | current cell, while reducing its dextral neighbor to a zero
;;     | (0) state.
;;   ------------------------------------------------------------------
;; 
;; PSEUDOCODE
;; ==========
;; The gestion in accordance to whom the truth-machine peracts shall be
;; a pseudocode formulation's pernor.
;; 
;; Please heed that each line in the actual program correlates to a
;; segregated compound in the pseudocode, the latter's prevenience is
;; accommodated with the equiparated program line as a parasceuastic
;; commentary.
;; 
;;   { A:++++++++; }
;;   Define the function "A"
;;   
;;   { B:++++++; }
;;   Define the function "B"
;;   
;;   { R:[-]; }
;;   Define the function "C"
;;   
;;   { C:R>RA[<B>-]<; }
;;   Define the function "D"
;;   
;;   { C }
;;   memory[0] <- 48
;;   
;;   { >, }
;;   memory[1] <- user input ("0" (= 48) or "1" (= 49))
;;   
;;   { <[>-<-]> }
;;   reduce user input cell memory[1] to either 0 or 1
;;   
;;   { [>C+.R<] }
;;   while memory[1] != 0
;;     change to cell memory[2]   { print cell }
;;     memory[2] <- 49            { ASCII code of character "1" }
;;     print memory[2]
;;     change to cell memory[1]   { user input and loop guard cell }
;;   end while
;;   
;;   { C. }
;;   memory[1] <- 48              { ASCII code of character "0" }
;;   print memory[1]
(interpret-brainf-with-functions
  "A:++++++++;
   B:++++++;
   R:[-];
   C:R>RA[<B>-]<;
   
   C
   >,
   <[>-<-]>
   [>C+.R<]
   C.")

;;; -------------------------------------------------------

;; Truth-machine which naits a conditional function redefinition.
;; 
;; FUNCTION DEFINITIONS
;; ====================
;; A quadruple contingency of functions serve to filst in a more
;; compendious code designment's chevisance.
;; 
;; Please heed their agnomination's listing in the sinistral column,
;; designated by "F", for "function", in the following tabulation.
;; 
;;   ------------------------------------------------------------------
;;   F | Causatum
;;   --+---------------------------------------------------------------
;;   A | Increments the current cell by a mountance of eight (8).
;;   ..................................................................
;;   B | Increments the current cell by a mountance of eight (8).
;;   ..................................................................
;;   R | Resets the current cell to zero (0).
;;   ..................................................................
;;   C | Stores the value 48 (ASCII code of the character "0") in the
;;     | current cell, while reducing its dextral neighbor to a zero
;;     | (0) state.
;;   ..................................................................
;;   P | Conditionally defined function, which either prints "0" once,
;;     | "1" perpetually.
;;   ------------------------------------------------------------------
;; 
;; PSEUDOCODE
;; ==========
;; The gestion in accordance to whom the truth-machine peracts shall be
;; a pseudocode formulation's pernor.
;; 
;; Please heed that each line in the actual program correlates to a
;; segregated compound in the pseudocode, the latter's prevenience is
;; accommodated with the equiparated program line as a parasceuastic
;; commentary.
;; 
;;   { A:++++++++; }
;;   Define the function "A"
;;   
;;   { B:++++++; }
;;   Define the function "B"
;;   
;;   { R:[-]; }
;;   Define the function "C"
;;   
;;   { C:R>RA[<B>-]<; }
;;   Define the function "D"
;;   
;;   { C }
;;   memory[0] <- 48
;;   
;;   { >, }
;;   memory[1] <- user input ("0" (= 48) or "1" (= 49))
;;   
;;   { <[>-<-]> }
;;   reduce user input cell memory[1] to either 0 or 1
;;   
;;   { P:C.; }
;;   define a function "P" as default one-time "0" printer
;;   
;;   { [P:+[>C+.R<];-] }
;;   if memory[1] = 0 then
;;     redefine the function "P" as an olamic "1" printer, with a
;;       functionality governed by an owelty to the following tmema:
;;         while memory[1] != 0
;;           change to cell memory[2]   { print cell }
;;           memory[2] <- 49            { ASCII code of character "1" }
;;           print memory[2]
;;           change to cell memory[1]   { user input & loop guard cell }
;;         end while
;;   end if
;;   
;;   { P }
;;   invoke the function "P", which either prints "0" once or "1"
;;     perpetually
(interpret-brainf-with-functions
  "A:++++++++;
   B:++++++;
   R:[-];
   C:R>RA[<B>-]<;
   
   C
   >,
   <[>-<-]>
   P:C.;
   [P:+[>C+.R<];-]
   P")
