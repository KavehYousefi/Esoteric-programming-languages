;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "LargeFish", invented by the Esolang user "ChuckEsoteric08"
;; and presented on December 20th, 2023, the haecceity of which derives
;; from the language "BigFish", conceived by the user "Yes", and, in its
;; ultimity, from Jonathan Todd Skinner's "Deadfish", administering a
;; conspicable amplification to the latter's very restricted competences
;; by the admission of assignments and a single iterance construct,
;; while operating on a multitude of non-negative integer registers in
;; lieu of an aefauld signed accumulator.
;; 
;; 
;; Concept
;; =======
;; The LargeFish programming language's capacitation lays its amplex
;; around several instruction intended for the manipulation and
;; output of unsigned integer-valued registers, each such a scalar
;; salvatory amenable to a deliberately chosen name, thus infinite in
;; their occurrency.
;; 
;; == THE PROGRAM MEMORY: AN INFINITE CONTINGENCY OF REGISTERS ==
;; The LargeFish memory's componency enumerates a theoretically infinite
;; tally of registers, amenable to agnominations chosen from the
;; developer's own deliberation, with each unit's capacity restricted to
;; a scalar non-negative integer number manumitted from any upper
;; bourne.
;; 
;; All registers at their inchoation acquire the default state of zero
;; (0), and, upon inquisition ere their value's explicit alteration,
;; respond with selfsame content.
;; 
;; 
;; Syntax
;; ======
;; A LargeFish program, observed from a syntactical vista, comprises
;; a sequence of statements compact of an instruction identifier and
;; one or more arguments, the former assumes in most, but not all, cases
;; an aefauld symbol, while the latter intrines unsigned literal integer
;; numbers, bare register names, as well as requests for their values.
;; 
;; == GRAMMAR ==
;; An amplification in the donat exposition's stringency shall be the
;; following Extended Backus-Naur Form (EBNF) formulation's
;; contribution:
;; 
;;   program          := statementList ;
;;   statementList    := { command } ;
;;   command          := increment
;;                    |  decrement
;;                    |  square
;;                    |  output
;;                    |  set
;;                    |  loop
;;                    ;
;;   increment        := "i" , registerName ;
;;   decrement        := "d" , registerName ;
;;   square           := "s" , registerName ;
;;   output           := "o" , registerName ;
;;   set              := "e" , registerName , expression ;
;;   loop             := "loop"
;;                    ,  expression
;;                    ,  statementList
;;                    ,  "endLoop"
;;                    ;
;;   
;;   expression       := integerLiteral | registerAccess ;
;;   
;;   registerAccess   := "*" , registerName ;
;;   registerName     := registerNameChar , { registerNameChar } ;
;;   registerNameChar := character - ( whitespace | "*" ) ;
;;   integerLiteral   := digit , { digit } ;
;;   
;;   digit            := "0" | "1" | "2" | "3" | "4"
;;                    |  "5" | "6" | "7" | "8" | "9"
;;                    ;
;;   whitespace       := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; LargeFish's instruction set's patration ensues from a sextuple
;; membership, the coeffiency partaken by the same covers the aspects
;; of basic arithmetics, output issuance, and a counting iteration
;; construct.
;; 
;; == INSTRUCTION ARGUMENTS EMBRACE LITERALS AND REGISTER ACCESS ==
;; A triad of contingency governs the admissibility of operation
;; arguments:
;; 
;;   ------------------------------------------------------------------
;;   Argument type   | Effect
;;   ----------------+-------------------------------------------------
;;   Literal integer | An unsigned integer literal greater than or
;;                   | equal to zero (0), unbridled along the positive
;;                   | airt's progression.
;;                   |-------------------------------------------------
;;                   | Two forbisens shall be adduced:
;;                   |  0
;;                   |  51
;;   ..................................................................
;;   Register name   | An agnomination concinnous for its usance as a
;;                   | register name, commorant in a diorism that
;;                   | merely excludes whitespaces and the asterisk
;;                   | ("*"), and that ultimately designates a register
;;                   | manipulate according to some principle.
;;                   |-------------------------------------------------
;;                   | Two forbisens shall be adduced:
;;                   |  my_register
;;                   |  9
;;   ..................................................................
;;   Register value  | An asterisk ("*") succeeded by a register name,
;;                   | which communicates the optation of the register
;;                   | value's obtention, akin to a variable value
;;                   | perquisition.
;;                   |-------------------------------------------------
;;                   | Two forbisens shall be adduced:
;;                   |  *my_register
;;                   |  *9
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; An apercu's dever shall be the elucidation of LargeFish's operative
;; warklumes.
;; 
;; Please heed that succedaneous segments are emphasized via a catena
;; of asterisks ("*") alow, the parcels being intended for their
;; supersession by actual LargeFish code fragments in the program's
;; ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command         | Effect
;;   ----------------+-------------------------------------------------
;;   i register      | Increments the {register} by one (1).
;;     ********      |-------------------------------------------------
;;                   | {register} must be a admissive register name.
;;   ..................................................................
;;   d register      | Decrements the {register} by one (1) if the
;;     ********      | same is greater than zero; otherwise
;;                   | accompasses no causatum.
;;                   |-------------------------------------------------
;;                   | {register} must be a admissive register name.
;;   ..................................................................
;;   s register      | Squares the {register}'s value by multiplying
;;     ********      | the same by itself.
;;                   |-------------------------------------------------
;;                   | {register} must be a admissive register name.
;;   ..................................................................
;;   e target source | Assigns the {source} value to the {target}
;;     ****** ****** | register.
;;                   |-------------------------------------------------
;;                   | {target} must be a admissive register name.
;;                   |-------------------------------------------------
;;                   | {source} must either be a unsigned literal
;;                   | integer number or a register reference.
;;   ..................................................................
;;   o register      | Prints the numeric value stored in the
;;                   | {register} in its verbatim form to the standard
;;                   | output.
;;                   |-------------------------------------------------
;;                   | {register} must be a admissive register name.
;;   ..................................................................
;;   loop guard      | Repeats the {statements} either the {guard}
;;        *****      | tally of times, if the same amounts to an
;;     statements    | unsigned integer literal, or, upon the
;;     **********    | specification of a register access, until the
;;   endloop         | loop cycle counter's value has transcended the
;;                   | {guard} register's state.
;;                   |-------------------------------------------------
;;                   | {guard} must either be a unsigned literal
;;                   | integer number or a register reference.
;;                   |-------------------------------------------------
;;                   | In a pseudocode diction, the following principle
;;                   | applies:
;;                   | 
;;                   |   if {guard} is an integer number thend
;;                   |     repeat {guard} times do
;;                   |       execute {statements}
;;                   |     end repeat
;;                   |   else if {guard} is a register reference
;;                   |     let cycle <- 1
;;                   |     
;;                   |     while (cycle <= registerValue({guard})) do
;;                   |       execute statements
;;                   |       cycle <- cycle + 1
;;                   |     end while
;;                   |   else
;;                   |     error: Invalid {guard}.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been realized in the programming
;; language Common Lisp, proceeding in concord with the traditional
;; approach that intrines the lexical analyzation tier for the token
;; production from a code provenance, its assemblage into an abstract
;; syntax tree (AST) hierarchy by a recursive descent parser's
;; adminiculum, and the ultimate accompassing of efficacy by mediation
;; of a dedicated interpreter instance.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-06-15
;; 
;; Sources:
;;   [esolang2023LargeFish]
;;   The Esolang contributors, "LargeFish", December 21st, 2023
;;   URL: "https://esolangs.org/wiki/LargeFish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a new derived type the agnomination assigned to whom is
   desumed from the TYPE-NAME, siclike to its formal parameters verbatim
   appropriation from the LAMBDA-LIST, and whose cynosure of the
   docimasy is norned by the CANDIDATE-VARIABLE, being evaluated in the
   BODY, the covenableness being distilled from the desinent form's
   primary result, with a non-``NIL'' response being tantamount to an
   affirmation, otherwise attesting, with ``NIL'', its incompatibility.
   ---
   The first BODY form, if representing a string object, will experience
   a construe as the derived type's documentation string, and will, as
   an ultimity, be reappropriated for this purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-custom-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which complies with the ELEMENT-TYPE, default of which
   is imposed by the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-custom-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each key of which conforms to the
   KEY-TYPE and answers to a value of the VALUE-TYPE, both being
   affiliated with the default of the comprehensive ``T''."
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

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized variation
   on unary operators applicable in the arithmetic realm."
  '(member :increment :decrement :square))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered sequence of zero or more
   ``AST-Node'' objects."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), but unimpeded along the upper bourne, and
   thus being a commorant of the integral range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype register-table ()
  "The ``register-table'' type defines a mapping betwixt register names
   and their non-negative integer values, realized as a hash table that
   affiliates the string identifiers with their content."
  '(hash-table-of string non-negative-integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Produces a veridical Boolean representation of the OBJECT by applying
   a construe of the same as a \"generalized boolean\" entity, returning
   a ``boolean'' value of ``T'' for a non-``NIL'' input, otherwise
   responding with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Token" class.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type characters value))
  (:constructor make-eof-token (&aux (type       :eof)
                                     (characters "")
                                     (value      NIL))))
  "The ``Token'' class serves in a significant object's encapsulation,
   extracted from a piece of LargeFish source code under indagation, and
   delineated by jumelle of a categorizing type and its detailing
   value."
  (type       (error "Missing token type.")
              :type      keyword
              :read-only T)
  (characters (error "Missing token characters.")
              :type      string
              :read-only T)
  (value      (error "Missing token value.")
              :type      T
              :read-only T))

;;; -------------------------------------------------------

(defun token-of-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Affiliates the recognized identifiers with representative tokens.")

;;; -------------------------------------------------------

(flet ((register-identifier (identifier token-type)
        "Associates the IDENTIFIER with its token representation, built
         from the TOKEN-TYPE and the IDENTIFIER as both its characters
         and value, in the global +IDENTIFIERS+ table and returns no
         value.
         ---
         Any extant entry amenable to the IDENTIFIER as its key will be
         supplanted in a tacit manner."
        (declare (type string  identifier))
        (declare (type keyword token-type))
        (setf (gethash identifier +IDENTIFIERS+)
          (make-token token-type identifier identifier))
        (values)))
  (register-identifier "d"       :decrement)
  (register-identifier "e"       :assignment)
  (register-identifier "i"       :increment)
  (register-identifier "o"       :output)
  (register-identifier "s"       :square)
  (register-identifier "loop"    :loop)
  (register-identifier "endloop" :endloop)
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns for the IDENTIFIER a connable token representation, this
   either being a LargeFish keyword name encapsulation or, if none
   corresponds to the input, an ``:identifier'' token."
  (declare (type string identifier))
  (the Token
    (or (gethash identifier +IDENTIFIERS+)
        (make-token :identifier identifier identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   encompassing in its diorism the space, horizontal tab, and newline
   entities, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for
   a LargeFish identifier name, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and (not (whitespace-character-p candidate))
           (graphic-char-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Lexer" class.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of LargeFish source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class provides an entity entalented with the
     capacitation for a piece of LargeFish source code's transcription
     into a token stream."))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slot ``source'' to the local symbol
   macro ``$source'', ``position'' to ``$position'', and ``character''
   to ``$character'', evaluates the BODY forms, and returns the desinent
   form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           (($source
             (the string
               (slot-value ,evaluated-lexer 'source)))
            ($position
             (the fixnum
               (slot-value ,evaluated-lexer 'position)))
            ($character
             (the (or null character)
               (slot-value ,evaluated-lexer 'character))))
         (declare (type string              $source))
         (declare (ignorable                $source))
         (declare (type fixnum              $position))
         (declare (ignorable                $position))
         (declare (type (or null character) $character))
         (declare (ignorable                $character))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Requests LEXER source's first character, stores the same in the
   LEXER, and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf $character
      (when (array-in-bounds-p $source $position)
        (char $source $position))))
  (values))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Returns the LEXER's current character, while concomitantly advances
   its position cursor to the next location, if possible."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the (or null character)
      (prog1 $character
        (setf $character
          (when (array-in-bounds-p $source (1+ $position))
            (char $source
              (incf $position))))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and $character (whitespace-character-p $character)) do
      (advance-lexer lexer)))
  (values))

;;; -------------------------------------------------------

(defun read-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a word and returns a string representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the string
      (with-output-to-string (word)
        (declare (type string-stream word))
        (loop
          while (and $character (identifier-character-p $character))
          do    (write-char (advance-lexer lexer) word))))))

;;; -------------------------------------------------------

(defun read-number-literal (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned integer literal and returns a ``:number'' representation
   thereof."
  (declare (type Lexer lexer))
  (let ((content (read-word lexer)))
    (declare (type string content))
    (the Token
      (make-token :number content
        (parse-integer content)))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an identifier and returns a covenable token representation thereof."
  (declare (type Lexer lexer))
  (let ((content (read-word lexer)))
    (declare (type string content))
    (the Token
      (get-identifier-token content))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end of file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null $character)
          (make-eof-token))
        
        ((whitespace-character-p $character)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        
        ((digit-char-p $character)
          (read-number-literal lexer))
        
        ((char= $character #\*)
          (prog1
            (make-token :asterisk
              (string $character)
              $character)
            (advance-lexer lexer)))
        
        ((identifier-character-p $character)
          (read-identifier lexer))
        
        (T
          (error "Unexpected character \"~c\" at position ~d."
            $character $position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface establishes a substrate for those classes
   pursuing the representation of LargeFish facilities in the form of
   abstract syntax tree (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include AST-Node))
  "The ``Block-Node'' class is apportioned the dever of an ordered
   sequence of zero or more LargeFish statement's castaldy, everichon
   among these an encapsulation inwith an abstract syntax tree (AST)
   node itself."
  (statements (error "Missing block node statements.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Expression-Node
  (:include AST-Node))
  "The ``Expression-Node'' interface establishes a specialization of an
   abstract syntax tree (AST) node, employed in the agency of a value's
   generation.")

;;; -------------------------------------------------------

(defstruct (Number-Node
  (:include Expression-Node))
  "The ``Number-Node'' class establishes an abstract syntax tree (AST)
   node encapsulation of an integer literal encountered during a
   LargeFish program's parsing."
  (value (error "Missing number node value.")
         :type      non-negative-integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Reference-Node
  (:include Expression-Node))
  "The ``Reference-Node'' class communicates the participation of a
   register as an indirect member of an operation, mediated by its
   value."
  (name (error "Missing register name.")
        :type      string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Register-Node
  (:include Expression-Node))
  "The ``Register-Node'' class communicates the participation of a
   register as an immediate member of an operation."
  (name (error "Missing register name.")
        :type      string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Assignment-Node
  (:include AST-Node))
  "The ``Assignment-Node'' class serves in the encapsulation of a
   LargeFish assignment betwixt a recipient register and an expression
   as its desideratum, manifesting in the form of an abstract syntax
   tree (AST) node."
  (target (error "Missing assignment node target.")
          :type      Register-Node
          :read-only T)
  (source (error "Missing assignment node source.")
          :type      Expression-Node
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Loop-Node
  (:include AST-Node))
  "The ``Loop-Node'' class serves in a LargeFish \"loop\" invocation's
   encapsulation, amplecting in its diorism's compass the continuation
   predicate subject, as well as an ordered sequence of statements
   comprising its body."
  (guard      (error "Missing loop node guard.")
              :type      Expression-Node
              :read-only T)
  (statements (error "Missing loop node statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Node
  (:include AST-Node))
  "The ``Output-Node'' class applies itself to the representation of a
   LargeFish print behest in the guise of an abstract syntax tree (AST)
   node."
  (register (error "Missing output node register.")
            :type      Register-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include AST-Node))
  "The ``Program-Node'' class establishes the abstract syntax tree (AST)
   structure's root node in representation of a complete LargeFish
   program."
  (statements (error "Missing program node statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Operation-Node
  (:include AST-Node))
  "The ``Unary-Operation-Node'' class applies itself to the ensconcement
   of a unary operation involving a target register in the guise of an
   abstract syntax tree (AST) node."
  (operator (error "Missing unary operator.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing unary operand.")
            :type      Register-Node
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Parser" class.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) Block-Node) parse-statement-list))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for parser.")
    :type          Lexer
    :documentation "The entity constituting the token purveyance's
                    provenance.")
   (current-token
    :initform      (make-eof-token)
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class' onus encompasses the assemblage of an
     abstract syntax tree (AST) representation of a piece of LargeFish
     source code distributed via a token stream."))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slot ``lexer'' to the local symbol
   macro ``$lexer'' and its ``current-token'' to ``$current-token'',
   evaluates the BODY forms, and returns the desinent form's results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (declare (ignorable   ,evaluated-parser))
       (symbol-macrolet
           (($lexer
             (the Lexer
               (slot-value ,evaluated-parser 'lexer)))
            ($current-token
             (the Token
               (slot-value ,evaluated-parser 'current-token))))
         (declare (type Lexer $lexer))
         (declare (ignorable  $lexer))
         (declare (type Token $current-token))
         (declare (ignorable  $current-token))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Queries the incipient token from the PARSER's lexer, stores the same
   in the PARSER, and returns no value."
  (declare (type Parser parser))
  (with-parser (parser)
    (setf $current-token
      (get-next-token $lexer)))
  (values))

;;; -------------------------------------------------------

(defun consume-token (parser)
  "Returns the PARSER's current token, while concomitantly querying the
   next one from the underlying lexer and storing the same in the
   PARSER."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Token
      (prog1 $current-token
        (setf $current-token
          (get-next-token $lexer))))))

;;; -------------------------------------------------------

(defun expect-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation the thus probed token,
   while concomitantly querying the next from the internally managed
   lexer and storing it in the PARSER; otherwise signals an error of an
   unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-parser (parser)
    (the Token
      (if (token-of-type-p $current-token expected-token-type)
        (prog1 $current-token
          (setf $current-token
            (get-next-token $lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type $current-token)))))

;;; -------------------------------------------------------

(defun parse-identifier (parser)
  "Parses a register name employing the PARSER's services and returns a
   string representation."
  (declare (type Parser parser))
  (with-parser (parser)
    (the string
      (if (or (token-of-type-p $current-token :eof)
              (token-of-type-p $current-token :asterisk))
        (error "The token ~s does not designate a valid identifier."
          $current-token)
        (token-characters
          (consume-token parser))))))

;;; -------------------------------------------------------

(defun parse-register-name (parser)
  "Parses a register name employing the PARSER's services and returns a
   ``Register-Node'' representation."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Register-Node
      (if (token-of-type-p $current-token :identifier)
        (make-register-node :name
          (parse-identifier parser))
        (error "The token ~s does not introduce a register name."
          $current-token)))))

;;; -------------------------------------------------------

(defun parse-expression (parser)
  "Parses an expression employing the PARSER's services and returns
   either a ``Number-Node'' or ``Reference-Node'' representation of the
   thus assembled abstract syntax tree (AST) fragment."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Expression-Node
      (case (token-type $current-token)
        (:number
          (make-number-node :value
            (token-value
              (expect-token parser :number))))
        (:asterisk
          (expect-token parser :asterisk)
          (make-reference-node :name
            (parse-identifier parser)))
        (otherwise
          (error "The token ~s does not introduce an expression."
            $current-token))))))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses a LargeFish statement in the PARSER's context and returns a
   conable ``AST-Node'' encapsulation thereof, or, if no statement
   ensues, returns the ``NIL'' value."
  (declare (type Parser parser))
  (with-parser (parser)
    (the (or null AST-Node)
      (case (token-type $current-token)
        ;; Command "d", "i", or "s".
        ((:decrement :increment :square)
          (let ((operator (token-type (consume-token parser))))
            (declare (type unary-operator operator))
            (make-unary-operation-node
              :operator operator
              :operand  (parse-register-name parser))))
        ;; Command "e".
        (:assignment
          (expect-token parser :assignment)
          (make-assignment-node
            :target (parse-register-name parser)
            :source (parse-expression    parser)))
        ;; Command "o".
        (:output
          (expect-token parser :output)
          (make-output-node :register
            (parse-register-name parser)))
        ;; Command "loop".
        (:loop
          (expect-token parser :loop)
          (make-loop-node
            :guard
              (parse-expression parser)
            :statements
              (prog1
                (parse-statement-list parser)
                (expect-token         parser :endloop))))
        (otherwise
          NIL)))))

;;; -------------------------------------------------------

(defun parse-statement-list (parser)
  "Parses an ordered sequence of zero or more LargeFish statements in
   the PARSER's context and returns a ``Block-Node'' encapsulation
   thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Block-Node
      (make-block-node :statements
        (loop
          for statement
            of-type (or null AST-Node)
            =       (parse-statement parser)
          while statement
            collect statement)))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Assembles a LargeFish program in the PARSER's context and returns a
   ``Program-Node'' representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Program-Node
      (prog1
        (make-program-node :statements
          (parse-statement-list parser))
        (expect-token parser :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Memory" class.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((registers
    :initform      (make-hash-table :test #'equal)
    :type          register-table
    :documentation "A sparse mapping betwixt register names and their
                    associated values."))
  (:documentation
    "The ``Memory'' class furnishes an infinite expanse of registers,
     each such a salvatory for a scalar unsigned integer."))

;;; -------------------------------------------------------

(defun register-value (memory name)
  "Returns the non-negative content of the register amenable to the
   NAME in the MEMORY."
  (declare (type Memory memory))
  (declare (type string name))
  (the non-negative-integer
    (gethash name (slot-value memory 'registers) 0)))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value memory name)
  "Stores the NEW-VALUE in the register amenable to the NAME in the
   MEMORY and returns no value."
  (declare (type non-negative-integer new-value))
  (declare (type Memory               memory))
  (declare (type string               name))
  (setf (gethash name (slot-value memory 'registers) 0) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Reference" class.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Reference
  (:constructor make-reference (name)))
  "The ``Reference'' class serves in the encapsulation of a register
   reference as a vehicle of its value's obtention."
  (name (error "Missing reference name.")
        :type      string
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Interpreter" class.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing program tree.")
    :type          Program-Node
    :documentation "The LargeFish program specified as an abstract
                    syntax tree (AST).")
   (memory
    :initform      (make-instance 'Memory)
    :type          Memory
    :documentation "The LargeFish program memory as an infinite array of
                    registers."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of accompassing
     veridical efficacy to a LargeFish program supplied in the mold of
     an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``tree'' to the local
   symbol macro ``$tree'' and ``memory'' to ``$memory'', evaluates the
   BODY forms, and returns the desinent form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($tree
             (the Program-Node
               (slot-value ,evaluated-interpreter 'tree)))
            ($memory
             (the Memory
               (slot-value ,evaluated-interpreter 'memory))))
         (declare (type Program-Node $tree))
         (declare (ignorable         $tree))
         (declare (type Memory       $memory))
         (declare (ignorable         $memory))
         ,@body))))

;;; -------------------------------------------------------

(defgeneric resolve-value (interpreter object)
  (:documentation
    "Returns the OBJECT's value in the INTERPRETER's context.")
  
  (:method ((interpreter Interpreter) (object integer))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type integer     object))
    (the integer object))
  
  (:method ((interpreter Interpreter) (object Reference))
    (declare (type Interpreter interpreter))
    (declare (type Reference   object))
    (the non-negative-integer
      (with-interpreter (interpreter)
        (register-value $memory
          (reference-name object))))))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Evaluates the NODE in the INTERPRETER's context and returns a value
     covenable for this combination."))

;;; -------------------------------------------------------

(defmacro define-node-visitor ((node-class) &body body)
  "Defines an implementation of the generic function ``visit-node'',
   employing as the first formal parameter the agnomination
   ``$interpreter'', specializing on the class ``Interpreter'', and
   for the second the name ``$name'', specializing on the NODE-CLASS,
   evaluates the BODY forms, and returns the desinent form's results."
  `(defmethod visit-node (($interpreter Interpreter)
                          ($node        ,node-class))
     (declare (type Interpreter $interpreter))
     (declare (ignorable        $interpreter))
     (declare (type ,node-class $node))
     (declare (ignorable        $node))
     ,@body))

;;; -------------------------------------------------------

(define-node-visitor (Program-Node)
  (visit-node $interpreter
    (program-node-statements $node))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Assignment-Node)
  (with-interpreter ($interpreter)
    (setf
      (register-value $memory
        (visit-node $interpreter
          (assignment-node-target $node)))
      (resolve-value $interpreter
        (visit-node $interpreter
          (assignment-node-source $node)))))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Block-Node)
  (dolist (statement (block-node-statements $node))
    (declare (type AST-Node statement))
    (visit-node $interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Loop-Node)
  (let ((guard
          (visit-node $interpreter
            (loop-node-guard $node))))
    (declare (type (or non-negative-integer Reference) guard))
    (typecase guard
      (non-negative-integer
        (loop repeat guard do
          (visit-node $interpreter
            (loop-node-statements $node))))
      (Reference
        (loop
          for cycle of-type (integer 1 *) from 1 by 1
          until (> cycle (resolve-value $interpreter guard)) do
            (visit-node $interpreter
              (loop-node-statements $node))))
      (otherwise
        (error "Invalid loop node guard: ~s." guard))))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Number-Node)
  (the non-negative-integer
    (number-node-value $node)))

;;; -------------------------------------------------------

(define-node-visitor (Reference-Node)
  (the Reference
    (make-reference
      (reference-node-name $node))))

;;; -------------------------------------------------------

(define-node-visitor (Register-Node)
  (the string
    (register-node-name $node)))

;;; -------------------------------------------------------

(define-node-visitor (Output-Node)
  (with-interpreter ($interpreter)
    (format T "~&~d"
      (register-value $memory
        (visit-node $interpreter
          (output-node-register $node)))))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Unary-Operation-Node)
  (let ((register-name
          (visit-node $interpreter
            (unary-operation-node-operand $node))))
    (declare (type string register-name))
    (declare (ignorable   register-name))
    (with-interpreter ($interpreter)
      (case (unary-operation-node-operator $node)
        (:decrement
          (when (plusp (register-value $memory register-name))
            (decf (register-value $memory register-name))))
        (:increment
          (incf (register-value $memory register-name)))
        (:square
          (setf (register-value $memory register-name)
            (* (register-value $memory register-name)
               (register-value $memory register-name))))
        (otherwise
          (error "Invalid unary operator: ~s."
            (unary-operation-node-operator $node))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the LargeFish program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (visit-node interpreter $tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-LargeFish (code)
  "Interprets the piece of LargeFish source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-instance 'Interpreter :tree
      (parse-program
        (make-instance 'Parser :lexer
          (make-instance 'Lexer :source code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Count from one (1) up to inclusive five (5).
(interpret-LargeFish
  "e counter               1
   e number_of_repetitions 5
   
   loop *number_of_repetitions
     o counter
     i counter
   endloop")

;;; -------------------------------------------------------

;; Ascertain that the register "x" equals zero (0).
(interpret-LargeFish
  "e test *x
   loop *test
     e test 1
   endloop")
