;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deadbird", invented by the Esolang user "ChuckEsoteric08"
;; and presented on December 29th, 2024, its proprium's commorancy an
;; augmentation of Jonathan Todd Skinner's "Deadfish" with respect to
;; the available arithmetics, input and output communication formats,
;; and, paravaunt and most peisant in its implications, the contingency
;; for bespoke function definitions, while operating on a quadruple
;; componency in registers in lieu of the entheus' aefauld accumulator.
;; 
;; 
;; Concept
;; =======
;; The Deadbird programming language constitutes a derivation and
;; extension of Deadfish, the supererogations' incarnations auctive
;; adhibitions to the extant arithmetics and input/output commerce
;; facilities, while introducing, a fortiori, a warklume for the
;; definition of bespoke functions; while concomitantly extending the
;; memory accumulator's accompt from its singularity to a quadruple
;; mountenance.
;; 
;; == DEADBIRD PROGRAM OPERATE IN A PERPETUAL CYCLE ==
;; A sience the kenspeckle Deadfish nomothesy, Deadbird's appropriation
;; does not encumber itself with the wite of its dioristic execution
;; model's renegation; pursing the selfsame olamic designment in the
;; gestion:
;; 
;;   (1) PROGRAM ACQUISITION:
;;       In its incipiency, the interpreter directs a request at the
;;       standard input for a sequence comprehending zero or more
;;       characters, the preposition to whom manifests in the prompt
;;       message
;;       
;;         >>
;;       
;;       The character sequence whose gendrure represents the query's
;;       response serves as the quesited program string to evaluate.
;;   
;;   (2) PROGRAM EXECUTION:
;;       The instructions participating in the received program are
;;       subjected to their evaluation and their allied epiphenomena's
;;       immediate execution.
;;       
;;       Unrecognized symbol, such thole an eloignment from the native
;;       instruction set and the advenient function definitions, serve
;;       in a newline output's issuance each.
;;   
;;   (3) REPETITION:
;;       A sequela obtained from the prevenient execution, the secle's
;;       iterum application emerges, returning to the step -> (1) as
;;       the inchoacy's reduplication.
;; 
;; == UNRECOGNIZED CHARACTERS INSTIGATE A NEWLINE OUTPUT ==
;; Any symbol to whom attendance no causatum's bailiwick, either from
;; the native instruction set, or the, its ponibility the encounter's
;; instant, a bespoke function agnomination, may be supputated a nexible
;; fact, will serve in a newline character's issuance to the standard
;; output conduit, rather than the actual abortive error whose
;; participation would install many cotidian programming language's
;; respondency.
;; 
;; == THE MEMORY: A RINGULAR QUADRUPLE OF SIGNED INTEGERS ==
;; A further point of nimiety as counterdistinguished from its
;; stock-father, Deadbird's memory ostends a componency ayond the
;; original aefauld accumulator for a quadruple contingency inwith this
;; subject, apposted in a ring, whose translation from the desitive
;; member instigates a return to the incipiency.
;; 
;; Maugre its enhaused puissance in quantity, the sere accumulators'
;; capacity nor deportment wist of a divergence from the autochthonous
;; nomothesia: A register's compass, ordained at its inchoacy to resolve
;; to the default state of zero (0), admits a scalar signed integer
;; datum, whose magnitude experiences no natural mears' tholance.
;; However, upon an owelty's attest with either the value -1 or 256, a
;; restoration is administered to the state to its original zero (0)
;; accompt.
;; 
;; 
;; Instructions
;; ============
;; Deadbird's instruction set limns a supererogation's application upon
;; the tesseratomy obtained from its Deadfish's cleronomy, its gendrure
;; a decimal mountenance thilk, besides the traditional and
;; concomitantly valorized arithmetics and input/output facilities,
;; attests its superiority in the accoutrements airted towards the
;; conditional skipping of instructions, as well as a potent bespoke
;; function definition construct.
;; 
;; Ensuing from an ejusdem generis conception, any identifier to whom
;; neither an autochthonous affiliation with a Deadbird operation, nor,
;; at the encheson's instant, a custom function identification may be
;; adhibited, is construed as an object of forinsecal constitution,
;; its sequela a single newline's issuance on the standard output
;; conduit.
;; 
;; == OVERVIEW ==
;; The alow tabulation's purpose shall be delineated by a requisite
;; mete of gnarity's vouchsafement concerning the language's operative
;; warklumes.
;; 
;; Please heed the demarcation adhibited to succedaneous tmemata by
;; adminiculum of a catena compact of asterisks ("*"), their presence
;; is intended for a supersession by actual Deadbird code in the
;; ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ==================================================================
;;   ARITHMETICS AND ASSIGNMENT
;;   ------------------------------------------------------------------
;;   i                 | Increments the current accumulator's value by
;;                     | a moutance of one (1). If the new state
;;                     | amounts to -1 or 256, the accumulator relapses
;;                     | to its inchoate value of zero (0).
;;   ..................................................................
;;   d                 | Decrements the current accumulator's value by
;;                     | a moutance of one (1). If the new state
;;                     | amounts to -1 or 256, the accumulator relapses
;;                     | to its inchoate value of zero (0).
;;   ..................................................................
;;   s                 | Squares the current accumulator's value; that
;;                     | is, sets thilk to the product of its current
;;                     | value multiplied by itself. If the new state
;;                     | amounts to -1 or 256, the accumulator relapses
;;                     | to its inchoate value of zero (0).
;;   ..................................................................
;;   z                 | Resets the current accumulator's value to its
;;                     | inchoate state of zero (0).
;;   ==================================================================
;;   INPUT AND OUTPUT
;;   ------------------------------------------------------------------
;;   o                 | Prints the current accumulator's value in its
;;                     | verbatim numeric form to the standard output
;;                     | conduit, the displayed message being
;;                     | circumfused by a newline character on each of
;;                     | the both lateralities.
;;   ..................................................................
;;   c                 | Prints the character whose ASCII code conflate
;;                     | with the current accumulator's value to the
;;                     | standard output conduit, the displayed message
;;                     | being amplected by no adscititious content.
;;   ..................................................................
;;   g                 | Queries the standard input conduit for a
;;                     | character and stores its ASCII code in the
;;                     | current cell.
;;   ==================================================================
;;   MEMORY MANAGEMENT
;;   ------------------------------------------------------------------
;;   n                 | Changes to the next accumulator. Upon the
;;                     | desinent one's departure, the pointer wraps
;;                     | around to the first specimen.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   f                 | If the current accumulator value equals zero
;;                     | (0), skips the next instruction; otherwise
;;                     | accompasses no causatum.
;;   ..................................................................
;;   (name/statements) | Defines a new function amenable to the {name},
;;    **** **********  | {name}, the body of which is comprised of the
;;                     | {statements}.
;;                     |-----------------------------------------------
;;                     | {name} must constitute a single character,
;;                     | the homologation of whose choice is merely
;;                     | impounded by the disqualification of the
;;                     | autochthonous Deadbird instruction
;;                     | identifiers; which lay their amplection
;;                     | around the duodecimal accompt:
;;                     |   "c", "d", "f", "g", "i", "n",
;;                     |   "o", "s", "z", "(", ")", "/".
;;                     |-----------------------------------------------
;;                     | If the {name} has already been associated with
;;                     | a function definition, the extant entry will
;;                     | be tacitly superseded by the new vinculum.
;;                     |-----------------------------------------------
;;                     | {statements} must represent an ordered
;;                     | sequence entailing zero or more instructions.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes a project in the
;; programming language Common Lisp, each program string's evaluation
;; limining a bipartite complex:
;; 
;;   (1) ASSEMBLAGE OF ABSTRACT SYNTAX TREE (AST):
;;       Each instruction, depending upon its actual haecceity, either
;;       an aefauld character in vallidom, or an intricacy of
;;       composition, is administered a transformation into a conable
;;       abstract syntax tree (AST) node, inwith whose capture the
;;       fundament information wones, and whose highest echolon always
;;       accommodates a program node's woning.
;;   
;;   (2) ABSTRACT SYNTAX TREE (AST) EVALUATION:
;;       Empight in the Deadbird interpreter's context, the abstract
;;       syntax tree experiences a traversal, commencing from its
;;       top program node's locality, with a concomitant evaluation's
;;       application actuated on the respective node in order to
;;       accompass the expected epiphenomena.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-02-23
;; 
;; Sources:
;;   [esolang:2024:Deadbird]
;;   The Esolang contributors, "Deadbird", December 29th, 2024
;;   URL: "https://esolangs.org/wiki/Deadbird"
;;   
;;   [stackoverflow:2013:q16678371]
;;   The Stack Overflow contributors, "Circular list in Common Lisp",
;;     May 21st, 2013
;;   URL: "https://stackoverflow.com/questions/16678371/
;;         circular-list-in-common-lisp"
;;   Notes:
;;     - Discusses circular lists.
;;     - Demonstrates the creation of such lists.
;;     - Presents code for gradually iterating over a circular list.
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the abstract syntax tree nodes.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface serves in a common foundry's
   vouchsafement for all classes whose telos mandates their incarnation
   of Deadbird behests in an abstract syntax tree (AST) node's
   plasmature.")

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include AST-Node))
  "The ``Block-Node'' class serves in the ensconcement of an arbitrary
   moutenance of Deadbird instructions, each such represented by a
   dedicated node, in an abstract syntax tree (AST) node's plasmature."
  (statements (error "No statement list has been communicated.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Arithmetic-Node
  (:include AST-Node))
  "The ``Arithmetic-Node'' class furnishes a unary arithmetic
   operation's encapsulation in an abstract syntax tree (AST) node's
   plasmature, intended for the unified representation of the Deadbird
   increment (\"i\"), decrement (\"d\"), and squaring (\"s\") behests."
  (operator (error "No unary operator has been communicated.")
            :type      (function (integer) integer)
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Node
  (:include AST-Node))
  "The ``Output-Node'' class establishes an abstract syntax tree (AST)
   node encapsulation of a print behest, the issuance's actual format a
   dever delegated to the internal printer callback function."
  (printer (error "No accumulator printer function has been ~
                   communicated.")
            :type      (function (integer) (values))
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Select-Next-Accumulator-Node
  (:include AST-Node))
  "The ``Select-Next-Accumulator-Node'' class accommodates the request
   for the next memory accumulator's selection, its incarnation that of
   an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include AST-Node))
  "The ``Input-Node'' class constitutes the parcery's pernor which
   involves the request for a user input in a character format,
   ostended in an abstract syntax tree's (AST) node's guise.")

;;; -------------------------------------------------------

(defstruct (Reset-Node
  (:include AST-Node))
  "The  ``Reset-Node'' class furnishes an abstract syntax tree (AST)
   node formulation of a request for the current memory accumulator's
   restoration to its zero-valued inchoate state.")

;;; -------------------------------------------------------

(defstruct (Skip-Node
  (:include AST-Node))
  "The ``Skip-Node'' class is assigned the dever of a conditional
   instruction skipping behest's plasmature in an abstract syntax tree
   (AST) node.")

;;; -------------------------------------------------------

(defstruct (Function-Definition-Node
  (:include AST-Node))
  "The ``Function-Definition-Node'' class furnishes an abstract syntax
   tree (AST) node's incarnation of a function definition behest, its
   diorism a twifold componency enumerating the identifying and an
   ordered list of its body's statement."
  (name       (error "No function name has been communicated.")
              :type      character
              :read-only T)
  (statements (error "No statement list has been communicated.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Identifier-Node
  (:include AST-Node))
  "The ``Identifier-Node'' class appertains to an abstract syntax tree
   (AST) node plasmature's dation of a Procrustean symbol's
   encapsulation, the same does not engage in an alligation with one of
   the recognized built-in Deadbird instruction identifiers, and may or
   may not correspond to a bespoke function name."
  (character (error "No identifier character has been communicated.")
             :type      character
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include AST-Node))
  "The ``Program-Node'' class serves in the representation of a parsed
   Deadbird program in an abstract syntax tree's (AST) plasmature."
  (statements (error "No statement list has been communicated.")
              :type      Block-Node
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the accumulator print functions.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-as-a-number (accumulator-state)
  "Prints the ACCUMULATOR-STATE in its verbatim numeric format to the
   standard output conduit, preceded by a conditional line break as a
   prefision and an unconditional equivalent as an additament, and
   returns no value."
  (declare (type integer accumulator-state))
  (format T "~&~d~%" accumulator-state)
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defun print-as-a-character (accumulator-state)
  "Prints the character whose ASCII code conflates with the
   ACCUMULATOR-STATE to the standard output conduit, destitute of any
   prefixion or additament, and returns no value."
  (declare (type integer accumulator-state))
  (format T "~c"
    (code-char accumulator-state))
  (finish-output)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun standard-instruction-name-p (candidate)
  "Determines whether the CANDIDATE designates an autochthonous
   Deadbird instruction identifier, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "cdfginosz()/" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the scanner.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Scanner) AST-Node) parse-a-command))

;;; -------------------------------------------------------

(defclass Scanner ()
  ((source
    :initarg       :source
    :initform      ""
    :type          simple-string
    :documentation "The current program string to evaluate.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The contemporaneous zero-based index into the
                    SOURCE string.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the contemporaneous POSITION into
                    the SOURCE."))
  (:documentation
    "The ``Scanner'' class accoutres a warklume nuncupated to the
     lexical analyzation of a piece of Deadbird code in order for its
     characters' orderly extraction."))

;;; -------------------------------------------------------

(defmacro with-the-scanner ((scanner) &body body)
  "Evaluates the SCANNER, binds its slot ``source'' to the local symbol
   macro ``$source'', ``position'' to ``$position'', and its
   ``character'' to ``$character'', evaluates the BODY forms, and
   returns the desinent form's results."
  (let ((evaluated-scanner (gensym)))
    (declare (type symbol evaluated-scanner))
    `(let ((,evaluated-scanner ,scanner))
       (declare (type Scanner ,evaluated-scanner)
                (ignorable    ,evaluated-scanner))
       (with-slots (($source    source)
                    ($position  position)
                    ($character character))
           ,evaluated-scanner
         (declare (type simple-string       $source)
                  (ignorable                $source))
         (declare (type fixnum              $position)
                  (ignorable                $position))
         (declare (type (or null character) $character)
                  (ignorable                $character))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((scanner Scanner) &key)
  "Initializes the SCANNER's current character as a sequela to its
   source and position information, and returns no value."
  (declare (type Scanner scanner))
  (with-the-scanner (scanner)
    (setf $character
      (when (array-in-bounds-p $source $position)
        (schar $source $position))))
  (values))

;;; -------------------------------------------------------

(defun prepare-a-pristine-scanner ()
  "Creates and returns a fresh ``Scanner'' whose source at this point
   of inchoacy represents an empty string."
  (the Scanner
    (make-instance 'Scanner)))

;;; -------------------------------------------------------

(defun prepare-a-scanner-for (new-source)
  "Creates and returns a fresh ``Scanner'' whose incipial source
   constitutes an appropriation from the NEW-SOURCE."
  (declare (type string new-source))
  (the Scanner
    (make-instance 'Scanner :source
      (coerce new-source 'simple-string))))

;;; -------------------------------------------------------

(defun change-the-scanner-source (scanner new-source)
  "Changes the SCANNER's source to the NEW-SOURCE, resets its state as
   a parasceve to the expected evaluation stage, and returns no value."
  (declare (type Scanner scanner))
  (declare (type string  new-source))
  (with-the-scanner (scanner)
    (psetf
      $source   (coerce new-source 'simple-string)
      $position 0)
    (setf $character
      (when (array-in-bounds-p $source $position)
        (schar $source $position))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-the-next-character (scanner)
  "Advances the SCANNER's position cursor to the next character in its
   underlying source, if possible, and returns no value."
  (declare (type Scanner scanner))
  (the (or null character)
    (with-the-scanner (scanner)
      (prog1 $character
        (setf $position
          (min
            (1+     $position)
            (length $source)))
        (setf $character
          (when (array-in-bounds-p $source $position)
            (schar $source $position)))))))

;;; -------------------------------------------------------

(defun expect-the-character (scanner expected-character)
  "Determines whether the SCANNER's current character establishes an
   owelty with the EXPECTED-CHARACTER, on confirmation advancing its
   position cursor ayond the successfully equiparated position, while
   returning no value; otherwise, an error of an unspecified type is
   signaled."
  (declare (type Scanner   scanner))
  (declare (type character expected-character))
  (with-the-scanner (scanner)
    (cond
      ((null $character)
        (error "The character \"~c\" was expected at the position ~d, ~
                but the source is already exhausted."
          expected-character $position))
      ((char/= $character expected-character)
        (error "The character \"~c\" was expected at the position ~d, ~
                but \"~c\" was encountered."
          expected-character $position $character))
      (T
        (advance-to-the-next-character scanner))))
  (values))

;;; -------------------------------------------------------

(defun parse-a-command-list (scanner)
  "Parses a catena enumerating zero or more Deadbird instructions,
   naiting the SCANNER's services for this telos, and returns a fresh
   ``Block-Node'' ensconcement of the extracted operations."
  (declare (type Scanner scanner))
  (the Block-Node
    (make-block-node :statements
      (with-the-scanner (scanner)
        (loop while (and $character (char/= $character #\))) collect
          (parse-a-command scanner))))))

;;; -------------------------------------------------------

(defun parse-a-function-definition (scanner)
  "Parses a function definition instruction by adminiculum of the
   SCANNER's services and returns a ``Function-Definition-Node''
   representation thereof."
  (declare (type Scanner scanner))
  (the Function-Definition-Node
    (with-the-scanner (scanner)
      (advance-to-the-next-character scanner)
      (cond
        ((null $character)
          (error "The function definition, commencing at the ~
                  position ~d, is incomplete."
            $position))
        ((standard-instruction-name-p $character)
          (error "The character \"~c\", empight at the position ~d, ~
                  represents a reserved symbol, and thus cannot be ~
                  redefined for a bespoke function."
            $character $position))
        (T
          (make-function-definition-node
            :name
              (prog1
                $character
                (advance-to-the-next-character scanner)
                (expect-the-character          scanner #\/))
            :statements
              (prog1
                (parse-a-command-list scanner)
                (expect-the-character scanner #\)))))))))

;;; -------------------------------------------------------

(defun parse-a-command (scanner)
  "Parses an aefauld Deadbird instruction naiting the SCANNER's services
   and returns a connable ``AST-Node'' representation thereof."
  (declare (type Scanner scanner))
  (the AST-Node
    (with-the-scanner (scanner)
      (case $character
        ((NIL)
          (error "The source is exhausted; thus no command could be ~
                  parsed at the position ~d."
            $position))
        
        (#\i
          (advance-to-the-next-character scanner)
          (make-arithmetic-node :operator #'1+))
        
        (#\d
          (advance-to-the-next-character scanner)
          (make-arithmetic-node :operator #'1-))
        
        (#\s
          (advance-to-the-next-character scanner)
          (make-arithmetic-node :operator
            #'(lambda (accumulator-value)
                (declare (type integer accumulator-value))
                (the (integer 0 *)
                  (* accumulator-value accumulator-value)))))
        
        (#\o
          (advance-to-the-next-character scanner)
          (make-output-node :printer #'print-as-a-number))
        
        (#\n
          (advance-to-the-next-character scanner)
          (make-select-next-accumulator-node))
        
        (#\c
          (advance-to-the-next-character scanner)
          (make-output-node :printer #'print-as-a-character))
        
        (#\g
          (advance-to-the-next-character scanner)
          (make-input-node))
        
        (#\z
          (advance-to-the-next-character scanner)
          (make-reset-node))
        
        (#\f
          (advance-to-the-next-character scanner)
          (make-skip-node))
        
        (#\(
          (parse-a-function-definition scanner))
        
        (#\)
          (error "The \")\" character at the position ~d does not ~
                  affiliate with any preveniently instigated function ~
                  definition behest."
            $position))
        
        (otherwise
          (prog1
            (make-identifier-node :character $character)
            (advance-to-the-next-character scanner)))))))

;;; -------------------------------------------------------

(defun parse-the-program (scanner)
  "Parses the Deadbird program concredited to the SCANNER's castaldy
   and returns a conable ``Program-Noe'' representation of the
   extracted instructions."
  (declare (type Scanner scanner))
  (the Program-Node
    (with-the-scanner (scanner)
      (make-program-node :statements
        (prog1
          (parse-a-command-list scanner)
          (when $character
            (error "The program contains unexpected content ~
                    commencing from the position ~d."
              $position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (integer Memory) (values))
                (setf current-accumulator-value)))

;;; -------------------------------------------------------

(defclass Memory ()
  ((accumulators
    :initform      (list 0 0 0 0)
    :type          list
    :documentation "A circular list comprehending four integer-valued
                    accumulators."))
  (:documentation
    "The ``Memory'' class encapsulates the Deadbird program memory's
     quadruple accumulators into an aefauld register bank, founded upon
     a circular list whose foremost cons cell at any instant designates
     the currently active unit."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((memory Memory) &rest initargs)
  "Connects the MEMORY's accumulator list's desinent cons cell with its
   incipient one, thus engendering a circular list from this adunation,
   and returns no value."
  (declare (type Memory memory))
  (declare (type list   initargs)
           (ignore      initargs))
  (with-slots (accumulators) memory
    (declare (type list accumulators))
    (setf (cdr (last accumulators)) accumulators))
  (values))

;;; -------------------------------------------------------

(defun prepare-a-pristine-memory ()
  "Creates and returns a fresh ``Memory'' whose accumulators all assume
   the default state of zero (0)."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-accumulator-value (memory)
  "Returns the current MEMORY accumulator's value."
  (declare (type Memory memory))
  (the integer
    (with-slots (accumulators) memory
      (declare (type list accumulators))
      (first accumulators))))

;;; -------------------------------------------------------

(defun (setf current-accumulator-value) (new-value memory)
  "Stores the NEW-VALUE in the current MEMORY accumulator and returns
   no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (accumulators) memory
    (declare (type list accumulators))
    (setf (first accumulators) new-value))
  ;; Normalize the current accumulator if it contains -1 or 256.
  (when (or (= (current-accumulator-value memory) -1)
            (= (current-accumulator-value memory) 256))
    (setf (current-accumulator-value memory) 0))
  (values))

;;; -------------------------------------------------------

(defun reset-the-current-accumulator (memory)
  "Resets the current MEMORY accumulator to its inchoate state of zero
   (0) and returns no value."
  (declare (type Memory memory))
  (setf (current-accumulator-value memory) 0)
  (values))

;;; -------------------------------------------------------

(defun select-the-next-accumulator (memory)
  "Selects the next accumulator from the MEMORY as the currently active
   one and returns no value."
  (declare (type Memory memory))
  (with-slots (accumulators) memory
    (declare (type list accumulators))
    (setf accumulators
      (rest accumulators)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory memory))
  (declare (type stream stream))
  (with-slots (accumulators) memory
    (declare (type list accumulators))
    (loop
      repeat 4
      for   current-accumulator-value of-type integer in accumulators
      do    (format stream " ~d" current-accumulator-value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Deadbird interpreter.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((memory
    :initform      (prepare-a-pristine-memory)
    :accessor      program-memory
    :type          Memory
    :documentation "A circular list of integer-valued registers.")
   (skips-next-instruction-p
    :initform      NIL
    :accessor      skips-the-next-instruction-p
    :type          boolean
    :documentation "A Boolean flag which determines whether the next
                    instruction shall be skipped.")
   (function-definitions
    :initform      (make-hash-table :test #'eql)
    :accessor      function-definitions
    :type          (hash-table-of character Function-Definition-Node)
    :documentation "Concredited with the bespoke function definitions'
                    castaldy by the alligation of their single-character
                    names with the representative
                    ``Function-Definition-Node'' objects."))
  (:documentation
    "The ``Interpreter'' class furnishes an entity the wike's parcery
     which obliges its adhibition of actual efficacy to Deadbird
     programs incarnated in an abstract syntax tree's (AST)
     plasmature."))

;;; -------------------------------------------------------

(defun make-an-interpreter ()
  "Creates and returns a fresh ``Interpreter'' in its pristine estate."
  (the Interpreter
    (make-instance 'Interpreter)))

;;; -------------------------------------------------------

(defun register-the-function (interpreter node)
  "Associates a new function definition, its identifying name desumed
   from the function definition NODE, with the NODE itself, stores the
   vinculum in the INTERPRETER, and returns no value.
   ---
   Any already present affiliation with the extracted name will be
   tacitly superseded."
  (declare (type Interpreter              interpreter))
  (declare (type Function-Definition-Node node))
  (setf
    (gethash
      (function-definition-node-name node)
      (function-definitions          interpreter))
    node)
  (values))

;;; -------------------------------------------------------

(defun look-up-the-function-for (interpreter name)
  "Returns the ``Function-Definition-Node'' registered at the
   INTERPRETER under the NAME, or responds with ``NIL'' upon its
   disrespondency."
  (declare (type Interpreter interpreter))
  (declare (type character   name))
  (the (or null Function-Definition-Node)
    (gethash name
      (function-definitions interpreter))))

;;; -------------------------------------------------------

(defmacro with-regard-to-the-skip-flag ((interpreter) &body body)
  "Interprets the INTERPRETER, determines whether the INTERPRETER's
   instruction skipping flag is activatd, in which circumstance a
   resetting to the inchoate unset status occasions, succeeded by the
   delivery of no value, without any further causata; otherwise executes
   the BODY forms, and returns the desinent form's results."
  `(with-slots (skips-next-instruction-p) ,interpreter
     (declare (type boolean skips-next-instruction-p))
     (cond
       (skips-next-instruction-p
         (setf skips-next-instruction-p NIL)
         (values))
       (T
         ,@body))))

;;; -------------------------------------------------------

(defgeneric visit-the-node (interpreter node)
  (:documentation
    "Evaluates the NODE in the INTERPRETER's context and returns no
     value.")
  
  (:method ((interpreter Interpreter)
            (node        Program-Node))
    (declare (type Interpreter  interpreter))
    (declare (type Program-Node node))
    (visit-the-node interpreter
      (program-node-statements node))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Block-Node))
    (declare (type Interpreter interpreter))
    (declare (type Block-Node  node))
    (dolist (current-statement (block-node-statements node))
      (declare (type AST-Node current-statement))
      (visit-the-node interpreter current-statement))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Arithmetic-Node))
    (declare (type Interpreter     interpreter))
    (declare (type Arithmetic-Node node))
    (with-regard-to-the-skip-flag (interpreter)
      (setf
        (current-accumulator-value
          (program-memory interpreter))
        (funcall
          (arithmetic-node-operator node)
          (current-accumulator-value
            (program-memory interpreter)))))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Select-Next-Accumulator-Node))
    (declare (type Interpreter                  interpreter))
    (declare (type Select-Next-Accumulator-Node node))
    (with-regard-to-the-skip-flag (interpreter)
      (select-the-next-accumulator
        (program-memory interpreter)))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Output-Node))
    (declare (type Interpreter interpreter))
    (declare (type Output-Node node))
    (with-regard-to-the-skip-flag (interpreter)
      (funcall
        (output-node-printer node)
        (current-accumulator-value
          (program-memory interpreter))))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Input-Node))
    (declare (type Interpreter interpreter))
    (declare (type Input-Node  node)
             (ignore           node))
    (with-regard-to-the-skip-flag (interpreter)
      (format T "~&Please enter a character: ")
      (finish-output)
      (setf
        (current-accumulator-value
          (program-memory interpreter))
        (char-code
          (read-char NIL NIL #\Null)))
      (clear-input))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Reset-Node))
    (declare (type Interpreter interpreter))
    (declare (type Reset-Node  node)
             (ignore           node))
    (with-regard-to-the-skip-flag (interpreter)
      (reset-the-current-accumulator
        (program-memory interpreter)))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Skip-Node))
    (declare (type Interpreter interpreter))
    (declare (type Skip-Node   node)
             (ignore           node))
    (with-regard-to-the-skip-flag (interpreter)
      (when (zerop
              (current-accumulator-value
                (program-memory interpreter)))
        (setf (skips-the-next-instruction-p interpreter) T)))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Function-Definition-Node))
    (declare (type Interpreter              interpreter))
    (declare (type Function-Definition-Node node))
    (with-regard-to-the-skip-flag (interpreter)
      (register-the-function interpreter node))
    (values))
  
  (:method ((interpreter Interpreter)
            (node        Identifier-Node))
    (declare (type Interpreter     interpreter))
    (declare (type Identifier-Node node))
    (with-regard-to-the-skip-flag (interpreter)
      (let ((function-node
              (look-up-the-function-for interpreter
                (identifier-node-character node))))
        (declare (type (or null Function-Definition-Node) function-node))
        (cond
          (function-node
            (visit-the-node interpreter
              (function-definition-node-statements function-node)))
          (T
            (format T "~%")
            (finish-output)))))
    (values)))

;;; -------------------------------------------------------

(defun interpret-the-program (interpreter tree)
  "Executes the Deadbird program concredited to the INTERPRETER in the
   abstract syntax TREE's (AST) plasmature and returns no value."
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node tree))
  (visit-the-node interpreter tree)
  (values))

;;; -------------------------------------------------------

(defun interpret-the-deadbird-code (interpreter scanner)
  "Executes the Deadbird code consigned to the SCANNER's castaldy in
   the INTERPRETER's context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Scanner     scanner))
  (interpret-the-program interpreter
    (parse-the-program scanner))
  (values))

;;; -------------------------------------------------------

(defun query-for-deadbird-code ()
  "Queries the standard input conduit for a line of Deadbird code and
   returns the same as a fresh string comprehending zero or more
   characters."
  (format T "~&>> ")
  (finish-output)
  (the string
    (prog1
      (read-line NIL NIL "")
      (clear-input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the actual interpretation operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-a-single-deadbird-program (code)
  "Executes the piece of Deadbird source CODE, subsequently halts the
   program, and returns no value."
  (declare (type string code))
  (interpret-the-deadbird-code
    (make-an-interpreter)
    (prepare-a-scanner-for code))
  (values))

;;; -------------------------------------------------------

(defun launch-the-deadbird-interpreter
    (&optional (initial-code "" initial-code-supplied))
  "Starts the Deadbird interpreter, optionally naiting the INITIAL-CODE
   as the incipial program, and perpetuates its operation in an olamic
   secle, returning upon an abortion no value."
  (declare (type string initial-code))
  (declare (type T      initial-code-supplied))
  (let ((interpreter (make-an-interpreter))
        (scanner     (prepare-a-pristine-scanner)))
    (declare (type Interpreter interpreter))
    (declare (type Scanner     scanner))
    (loop
      for current-code
        of-type (or null string)
        =       (if initial-code-supplied
                  initial-code
                  (query-for-deadbird-code))
        then    (query-for-deadbird-code)
      while current-code do
        (change-the-scanner-source   scanner     current-code)
        (interpret-the-deadbird-code interpreter scanner)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, world!" to the standard output conduit.
(execute-a-single-deadbird-program
  "iiisdsiiiiiiiiciiiiiiiiiiiiiiiiiiiiiiiiiiiiiciiiiiiicciiicdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddcddddddddddddcdddddddddddddddddddddsddcddddddddciiicddddddcddddddddcdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddc")

;;; -------------------------------------------------------

;; A counter which counts up from inclusive one (1) to 256, ere
;; wrapping around in order to pursue its olamic aspirations.
;; 
;; Please heed that an overflow on the stack's transpiration constitutes
;; a tenable sequela from the nimiety in recursive invocations.
(execute-a-single-deadbird-program "(U/ioU)U")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Please heed that an overflow on the stack's transpiration constitutes
;; a tenable sequela from the nimiety in recursive invocations.
(execute-a-single-deadbird-program
  "(0/o)(1/o1)gddddddddddddddddddddddddddddddddddddddddddddddddf10")

;;; -------------------------------------------------------

(launch-the-deadbird-interpreter)
