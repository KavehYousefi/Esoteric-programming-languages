;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "AnnoyStack", invented by the Esolang user "Jdsj llop llvm"
;; and presented on September 25th, 2020, the entelechy of whose
;; haecceity wones in programs explicitly operating in a iterating
;; context, either perpetually or bound to a tally in accordance with
;; one's optation, engaged in perquisitions and modulations of an
;; infinite tape of characters and a stack of such entities whose the
;; champarty governs the language's efficacy.
;; 
;; 
;; Concept
;; =======
;; The AnnoyStack programming language operates on a twissel of memory
;; components, the first being a tape of character-valued cells,
;; infinite in the dextral dispansion, its compernage manifesting in
;; a stack of characters, these data salvatories constituting the
;; cynosure of programs which always execute in a explicitly stated
;; loop, selected by one's deliberation either in the guise of a
;; finite or an infinite iterance account.
;; 
;; == THE LOOP: A FRAMEWORK ==
;; Any program's inchoation and conclusion ought to be delimited by
;; a loop, the commencement specified either in the form of a finite
;; iteration obeying the forbisen
;; 
;;   repeat {numberOfRepetitions}
;; 
;; where {numberOfRepetitions} has to resolve to a non-negative integer
;; literal greater than or equal to zero (0); or the infinite
;; counterpart
;; 
;;   forever
;; 
;; The program's desinent token is required to state the iteration's
;; closure via
;; 
;;   end
;; 
;; Neither of these three compartments may be adduced at any other
;; location in the program.
;; 
;; Offered as a parlecue, a program's two lateralities, endowed with
;; zero or more commands as {instructions}, either produce
;; 
;;   repeat {numberOfTimes}
;;   {instructions}
;;   end
;; 
;; or
;; 
;;   forever
;;   {instruction}
;;   end
;; 
;; == THE MEMORY: CHARACTERS IN AN INFINITE TAPE AND A STACK ==
;; The program memory's componency enumerates a twissel membership:
;; imprimis, a tape composed of cells each consigned to an aefauld
;; ASCII character's castaldy, the data structure being operated upon
;; by a mobile head that at any instant designates the currently active
;; unit among this series of salvatories, infinite in their dextral
;; expansion; secondly, and paravail in its echolon, a stack also
;; endowed with character elements in its capacity.
;; 
;; 
;; Syntax
;; ======
;; An AnnoyStack program, invested with a syntactical species of
;; conspectuity, subscribes to a fixated moiety and a dynamic part,
;; the former mandates the ensconcement of the operative body in a
;; loop context, introduced either via an infinite "forever" keyword,
;; or a deterministic tally succeeding as an unsigned integer literal
;; the "repeat" clause; both parasceves are terminated via an "end"
;; sentinel, which also imposes the entire program's closure.
;; 
;; The instructions themselves abstain from any operand's involvement,
;; each twain segregated by one or more whitespaces.
;; 
;; == GRAMMAR ==
;; The language's donet shall be endowed with superior formality by
;; employing an Extended Backus-Naur Form (EBNF) description:
;; 
;;   program       := loopHeader , instructions , "end" ;
;;   loopHeader    := "forever" | ( "repeat" , naturalNumber ) ;
;;   instructions  := { instruction } ;
;;   instruction   := "inp" | "lft" | "outp" | "pop" | "push" | "radd" ;
;;   naturalNumber := digit , { digit } ;
;;   digit         := "0" | "1" | "2" | "3" | "4"
;;                 |  "5" | "6" | "7" | "8" | "9"
;;                 ;
;; 
;; 
;; Instructions
;; ============
;; In addition to the parasceuastic loop start commands, "forever" and
;; "repeat", as well at the "end" sentinel, AnnoyStack's instruction set
;; enumerates a sextuple membership of operative warklumes.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be a cursory mete of gnarity's
;; adhibition anent the language's competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   lft     | If the tape head does not reside on the leftmost cell,
;;           | translates the same one step to the left; otherwise
;;           | accompasses no further causatum.
;;   ..................................................................
;;   radd    | Translates the tape head one cell to the right and
;;           | increments the new cell's value by one (1), that is,
;;           | advances to the character with next higher ASCII code.
;;           | Upon its upper march's transgression, which is empight
;;           | on 255, the state relapses to the minimum of zero (0).
;;   ..................................................................
;;   push    | Pushes the value of the currently selected cell unto the
;;           | stack without altering the tape.
;;   ..................................................................
;;   pop     | Pops the stack's top element and stores the same in the
;;           | current cell.
;;           |---------------------------------------------------------
;;           | An error of an unspecified type transpires if the stack
;;           | is empty at the instant of this operation's invocation.
;;   ..................................................................
;;   inp     | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   outp    | Prints the character stored in the current cell to the
;;           | standard output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization proceeds in the programming language
;; Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-08-05
;; 
;; Sources:
;;   [esolang2024AnnoyStack]
;;   The Esolang contributors, "AnnoyStack", May 3rd, 2024
;;   URL: "https://esolangs.org/wiki/AnnoyStack"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction'' type enumerates the recognized variation on
   ``AnnoyStack'' operation identifiers."
  '(member :end :forever :inp :lft :outp :pop :push :radd :repeat))

;;; -------------------------------------------------------

(deftype instruction-list ()
  "The ``instruction-list'' type defines an ordered list of zero or more
   AnnoyStack ``Instruction'' objects."
  '(list-of Instruction))

;;; -------------------------------------------------------

(deftype natural-number ()
  "The ``natural-number'' type defines a non-negative integer number
   greater than or equal to zero, but without any natural bourne along
   the upper extremum."
  '(integer 0 *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Adduces a representation of the OBJECT as a Boolean truth value,
   derived from its construe as a \"generalized boolean\", returning
   for a non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for
   the OBJECT being ``NIL'', responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, including
   in its diorism's circumference the space, horizontal tab, and newline
   entities, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun get-next-character (current-character)
  "Returns the character succeeding the CURRENT-CHARACTER, contingently
   wrapping around at the ASCII code range's upper bourne of 255."
  (declare (type character current-character))
  (the character
    (code-char
      (mod (1+ (char-code current-character))
           256))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "string-case" macro.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-case-p (case-key)
  "Determines whether the CASE-KEY designates a default case, that is,
   partakes of a membership in the set of symbols ``otherwise'' and
   ``T'', returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type (or string symbol) case-key))
  (the boolean
    (get-boolean-value-of
      (and (symbolp case-key)
           (or (eq case-key 'otherwise)
               (eq case-key 'T))))))

;;; -------------------------------------------------------

(defmacro string-case (key-form &rest clauses)
  "Conditionally executes at most one of the CLAUSES in response to its
   key's equality with the KEY-FORM by evaluating the latter and probing
   the CLAUSES, expected in their conformation to impose a list of one
   or more characters, the first being the string-valued key, the
   subsequent a series of zero or more consequent actions, and returns
   the matching clause's desinent action form's results.
   ---
   The KEY-FORM must resolve to a string object.
   ---
   Each clause among the CLAUSES must be a list of one or more elements
   complying to the following forbisen:
   
     (clause-key clause-action-1 ... clause-action-n)
   
   where the CLAUSE-KEY must resolve to a string object, while the
   CLAUSE-ACTION-1 through CLAUSE-ACTION-N specify the forms to evaluate
   upon the clause's eligibility.
   ---
   A particular specimen might be introduced in the form of a default
   clause, signified via either the symbol ``otherwise'' or ``T'' in
   the clause key place, which, upon any prevenient trial's failure,
   will be actuated.
   ---
   An example shall be adduced for this macro's exercise:
   
     (let ((your-favorite-animal (read-line)))
       (declare (type string your-favorite-animal))
       
       (string-case your-favorite-animal
         (\"bunny\" (print 'hop-hop))
         (\"bird\"  (print 'craw-craw))
         (\"frog\"  (print 'croak-croak))
         (otherwise (print 'hello?))))"
  (let ((test-key (gensym)))
    (declare (type symbol test-key))
    `(let ((,test-key ,key-form))
       (declare (type string ,test-key))
       (cond
         ,@(loop for clause of-type list in clauses collect
             (destructuring-bind (clause-key &rest clause-body) clause
               (declare (type (or string symbol) clause-key))
               (declare (type T                  clause-body))
               (if (default-case-p clause-key)
                 `(T
                    ,@clause-body)
                 `((string= ,clause-key ,test-key)
                    ,@clause-body))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of vector operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-character-vector ()
  "Creates and returns a dynamically extending vector of characters,
   initially comprehending an aefauld zero-valued item."
  (the (vector character *)
    (make-array 1
      :element-type    'character
      :initial-element #\Null
      :adjustable      T
      :fill-pointer    T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (argument NIL))))
  "The ``Instruction'' class serves in the encapsulation of the data
   requisite for an AnnoyStack operation's replication, enumerating in
   its componency the mandatory operator and an optional non-negative
   integer operand."
  (type     (error "Missing instruction type.")
            :type      instruction-type
            :read-only T)
  (argument NIL
            :type      (or null natural-number)
            :read-only T))

;;; -------------------------------------------------------

(defun instruction-of-type-p (instruction expected-type)
  "Determines whether the INSTRUCTION conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Instruction      instruction))
  (declare (type instruction-type expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (instruction-type instruction)
          expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexical analyzer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-next-word (source start)
  "Proceeding from the START position into the SOURCE, searches
   dextrally for the closest word's inchoation, upon affirmation
   returning its first character's location in the SOURCE; otherwise
   responds with the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, and expecting to
   be resident inside of a token's bournes, seeks the dextral end of the
   currently occupied word, returning the location in the SOURCE
   immediately succeeding the word's desinent character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun get-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates the
   next word and returns two values:
     (1) The thus detected word as a string.
     (2) The position into the SOURCE immediately succeeding the
         retrieved word's desinent character."
  (declare (type string source))
  (declare (type fixnum start))
  (let* ((start-of-word (locate-next-word   source start))
         (end-of-word   (locate-end-of-word source start-of-word)))
    (declare (type fixnum start-of-word))
    (declare (type fixnum end-of-word))
    (the (values string fixnum)
      (values
        (subseq source start-of-word end-of-word)
        end-of-word))))

;;; -------------------------------------------------------

(defun read-number (source start)
  "Proceeding from the START position into the SOURCE, reads an unsigned
   integer number and returns two values:
     (1) The parsed non-negative integer number.
     (2) The position into the SOURCE immediately succeeding the
         consumed number's occupied parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (next-word new-position)
      (get-next-word source start)
    (declare (type string next-word))
    (declare (type fixnum new-position))
    (the (values natural-number fixnum)
      (values
        (parse-integer next-word)
        new-position))))

;;; -------------------------------------------------------

(defun read-next-instruction (source start)
  "Proceed from the START position into the SOURCE, reads the next
   command and either returns two values:
     (1) If a command could be detected, an ``Instruction''
         representation thereof; otherwise the ``NIL'' value.
     (2) If a command could be detected, the position into the SOURCE
         immediately succeeding the occupied parcel; otherwise, the
         length of the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (next-word new-position)
      (get-next-word source start)
    (declare (type string next-word))
    (declare (type fixnum new-position))
    (the (values (or null Instruction) fixnum)
      (string-case next-word
        (""
          (values NIL new-position))
        
        ("forever"
          (values (make-instruction :forever) new-position))
        
        ("repeat"
          (let ((repetition-count 0))
            (declare (type natural-number repetition-count))
            (multiple-value-setq (repetition-count new-position)
              (read-number source new-position))
            (values
              (make-instruction :repeat repetition-count)
              new-position)))
        
        ("end"
          (values (make-instruction :end) new-position))
        
        ("inp"
          (values (make-instruction :inp) new-position))
        
        ("lft"
          (values (make-instruction :lft) new-position))
        
        ("outp"
          (values (make-instruction :outp) new-position))
        
        ("pop"
          (values (make-instruction :pop) new-position))
        
        ("push"
          (values (make-instruction :push) new-position))
        
        ("radd"
          (values (make-instruction :radd) new-position))
        
        (otherwise
          (error "No instruction identifier: ~s." next-word))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (source)
  "Extracts from the piece of AnnoyStack SOURCE code the incorporated
   operations and returns a list of ``Instruction'' encapsulations
   thereof."
  (declare (type string source))
  (let ((instructions     NIL)
        (position         0)
        (next-instruction NIL))
    (declare (type instruction-list      instructions))
    (declare (type fixnum                position))
    (declare (type (or null Instruction) next-instruction))
    (loop do
      (multiple-value-setq (next-instruction position)
        (read-next-instruction source position))
      (if next-instruction
        (push next-instruction instructions)
        (loop-finish)))
    (the instruction-list
      (nreverse instructions))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction list validator operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-if-non-empty-instructions (instructions)
  "Determines whether the INSTRUCTIONS entail at least one instruction,
   returning on confirmation no value; otherwise signals an error of an
   unspecified type."
  (declare (type instruction-list instructions))
  (unless instructions
    (error "The AnnoyStack instruction list is empty."))
  (values))

;;; -------------------------------------------------------

(defun check-if-starts-with-loop (instructions)
  "Determines whether the INSTRUCTIONS commence with a \"forever\" or
   \"repeat\" instruction, returning on confirmation no value; otherwise
   signals an error of an unspecified type."
  (declare (type instruction-list instructions))
  (let ((first-instruction (first instructions)))
    (declare (type Instruction first-instruction))
    (unless (or (instruction-of-type-p first-instruction :forever)
                (instruction-of-type-p first-instruction :repeat))
      (error "The instructions do not start with a loop instruction, ~
              but instead with ~a."
        first-instruction)))
  (values))

;;; -------------------------------------------------------

(defun check-if-terminates-with-end-instruction (instructions)
  "Determines whether the INSTRUCTIONS terminate in an \"end\"
   instruction, returning on confirmation no value; otherwise signals
   an error of an unspecified type."
  (declare (type instruction-list instructions))
  (let ((last-instruction (first (last instructions))))
    (declare (type Instruction last-instruction))
    (unless (instruction-of-type-p last-instruction :end)
      (error "The instructions do not terminate in an \"end\" ~
              instruction, but rather in ~a."
        last-instruction)))
  (values))

;;; -------------------------------------------------------

(defun check-if-loop-bournes-are-valid (instructions)
  "Determines whether the tally of loop starts and ends in the
   INSTRUCTIONS complies with the expectations, returning on
   confirmation no value, otherwise signals an error of an unspecified
   type."
  (declare (type instruction-list instructions))
  (loop
    for current-instruction of-type Instruction in instructions
    
    if (or (instruction-of-type-p current-instruction :forever)
           (instruction-of-type-p current-instruction :repeat))
      count 1
      into  number-of-loop-starts
    else if (instruction-of-type-p current-instruction :end)
      count 1
      into  number-of-loop-ends
    
    finally
      (cond
        ((/= number-of-loop-starts 1)
          (error "Invalid number of loop starts: ~d."
            number-of-loop-starts))
        ((/= number-of-loop-ends 1)
          (error "Invalid number of loop ends: ~d."
            number-of-loop-ends))
        (T
          NIL)))
  (values))

;;; -------------------------------------------------------

(defun validate-instructions (instructions)
  "Determines whether the INSTRUCTIONS conform with AnnoyStack's
   stipulations, returning on confirmation the INSTRUCTIONS list itself,
   otherwise signals an error of an unspecified type."
  (declare (type instruction-list instructions))
  (check-if-non-empty-instructions          instructions)
  (check-if-starts-with-loop                instructions)
  (check-if-terminates-with-end-instruction instructions)
  (check-if-loop-bournes-are-valid          instructions)
  (the instruction-list instructions))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((loop-configuration
    :initarg       :loop-configuration
    :initform      (error "Missing loop configuration.")
    :type          Instruction
    :documentation "The ``Instruction'' object encapsulating the
                    iterance principle.")
   (instructions
    :initarg       :instructions
    :initform      (error "Missing instructions.")
    :reader        get-program-instructions
    :type          (list-of instruction-type)
    :documentation "A list of zero or more instructions, everichon's
                    representation by adminiculum of an
                    ``instruction-type''."))
  (:documentation
    "The ``Program'' class serves in the encapsulation of an AnnoyStack
     program, its composition involving the iterance principle in
     champarty with zero or more instructions."))

;;; -------------------------------------------------------

(defun make-program (instructions)
  "Creates and returns a fresh ``Program'' derived from the list of
   INSTRUCTIONS."
  (declare (type instruction-list instructions))
  (the Program
    (make-instance 'Program
      :loop-configuration
        (pop instructions)
      :instructions
        (mapcar #'instruction-type
          (butlast instructions)))))

;;; -------------------------------------------------------

(defun program-repeats-perpetually-p (program)
  "Determines whether the PROGRAM's iterance principle is founded upon
   a mode of perpetuation, that is, a \"forever\" loop, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (the boolean
    (instruction-of-type-p
      (slot-value program 'loop-configuration)
      :forever)))

;;; -------------------------------------------------------

(defun get-number-of-repetitions (program)
  "Returns the tally of repetitions allotted to the PROGRAM's iterance
   construct, in the case of the same being desumed from the \"repeat\"
   species, otherwise responds with ``NIL''."
  (declare (type Program program))
  (the (or null natural-number)
    (when (instruction-of-type-p
            (slot-value program 'loop-configuration)
            :repeat)
      (instruction-argument
        (slot-value program 'loop-configuration)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-dynamic-character-vector)
    :type          '(vector character *)
    :documentation "A dynamically expanding vector of characters.")
   (head
    :initform      0
    :type          fixnum
    :documentation "The current index into the CELLS vector."))
  (:documentation
    "The ``Tape'' class models one moeity among AnnoyStack's memory
     composition, this being the tape, a linear series of
     character-valued cells, bourneless in their dextral dispansion,
     while establishing a head's cynosure, the same acquires the dever
     of the currently active member's selection."))

;;; -------------------------------------------------------

(defun move-tape-head-left (tape)
  "Translates the TAPE's head one cell sinistrally, if not empighted on
   the first cell already, and returns no value."
  (declare (type Tape tape))
  (when (>= (slot-value tape 'head) 1)
    (decf (slot-value tape 'head)))
  (values))

;;; -------------------------------------------------------

(defun move-tape-head-right (tape)
  "Translates the TAPE's head one cell dextrally, and returns no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'head))
  (when (>= (slot-value tape 'head)
            (fill-pointer (slot-value tape 'cells)))
    (vector-push-extend #\Null
      (slot-value tape 'cells)))
  (values))

;;; -------------------------------------------------------

(defun current-tape-cell-value (tape)
  "Returns the character stored in the current TAPE cell."
  (declare (type Tape tape))
  (the character
    (aref (slot-value tape 'cells)
      (slot-value tape 'head))))

;;; -------------------------------------------------------

(defun (setf current-tape-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE and returns no value."
  (declare (type character new-value))
  (declare (type Tape      tape))
  (setf (aref (slot-value tape 'cells)
          (slot-value tape 'head))
    new-value)
  (values))

;;; -------------------------------------------------------

(defun increment-current-tape-cell (tape)
  "Increments the TAPE's current cell, contingently wrapping its state
   around upon the upper bourne's transcendence, and returns no value."
  (declare (type Tape tape))
  (setf (current-tape-cell-value tape)
    (get-next-character
      (current-tape-cell-value tape)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Stack".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack ()
  ((elements
    :initform      NIL
    :type          (list-of character)
    :documentation "The characters stored in this stack as elements of
                    a list."))
  (:documentation
    "The ``Stack'' class accoutres the manifestation of AnnoyStack's
     eponymous stack data structure, a last in, first out ilk of
     salvatory, such is entalented with the capacitation for an
     arbitrary tally of ASCII characters' castaldy."))

;;; -------------------------------------------------------

(defun push-unto-stack (stack new-element)
  "Inserts the NEW-ELEMENT at the STACK's top position and returns no
   value."
  (declare (type Stack     stack))
  (declare (type character new-element))
  (push new-element
    (slot-value stack 'elements))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (stack)
  "Removes and returns the STACK's top element, or signals an error of
   an unspecified type upon its vacancy."
  (declare (type Stack stack))
  (the character
    (or (pop (slot-value stack 'elements))
        (error "Cannot pop from an empty stack."))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing AnnoyStack program.")
    :type          Program
    :documentation "The AnnoyStack program to execute.")
   (tape
    :initform      (make-instance 'Tape)
    :type          Tape
    :documentation "A dextrally infinite sequence of character-valued
                    cells, the currently active member governed by the
                    head's efforts.")
   (stack
    :initform      (make-instance 'Stack)
    :type          Stack
    :documentation "A stack of character."))
  (:documentation
    "The ``Interpreter'' class furnishes a context for the execution of
     a parsed AnnoyStack program, administering actual operative value
     to the static instruction list."))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction (eql :inp)))
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction))
  (declare (ignore                instruction))
  (format T "~&>> ")
  (finish-output)
  (setf (current-tape-cell-value (slot-value interpreter 'tape))
        (read-char NIL NIL #\Null))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction (eql :lft)))
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction))
  (declare (ignore                instruction))
  (move-tape-head-left
    (slot-value interpreter 'tape))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction (eql :outp)))
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction))
  (declare (ignore                instruction))
  (format T "~c"
    (current-tape-cell-value
      (slot-value interpreter 'tape)))
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction (eql :radd)))
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction))
  (declare (ignore                instruction))
  (move-tape-head-right
    (slot-value interpreter 'tape))
  (increment-current-tape-cell
    (slot-value interpreter 'tape))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction (eql :pop)))
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction))
  (declare (ignore                instruction))
  (setf (current-tape-cell-value
          (slot-value interpreter 'tape))
    (pop-from-stack
      (slot-value interpreter 'stack)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction (eql :push)))
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction))
  (declare (ignore                instruction))
  (push-unto-stack
    (slot-value interpreter 'stack)
    (current-tape-cell-value
      (slot-value interpreter 'tape)))
  (values))

;;; -------------------------------------------------------

(defun execute-program-instructions (interpreter)
  "Executes the instructions governed by the INTERPRETER's program and
   returns no value."
  (declare (type Interpreter interpreter))
  (dolist (instruction
            (get-program-instructions
              (slot-value interpreter 'program)))
    (declare (type instruction-type instruction))
    (process-instruction interpreter instruction))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the AnnoyStack program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program) interpreter
    (declare (type Program program))
    (if (program-repeats-perpetually-p program)
      (loop do
        (execute-program-instructions interpreter))
      (loop repeat (get-number-of-repetitions program) do
        (execute-program-instructions interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpret-AnnoyStack (code)
  "Interprets the piece of AnnoyStack source CODE and returns no value."
  (declare (type string))
  (interpret-program
    (make-instance 'Interpreter :program
      (make-program
        (validate-instructions
          (extract-instructions code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-AnnoyStack
  "forever
   inp
   outp
   end")

;;; -------------------------------------------------------

;; Perpetually advance dextrally along the tape without effectively
;; alterating the same.
(interpret-AnnoyStack
  "forever
   push
   radd
   pop
   end")

;;; -------------------------------------------------------

;; Print the ASCII characters with the codes in the range [0, 255].
(interpret-AnnoyStack
  "repeat 256
   outp
   radd
   push
   lft
   pop
   end")
