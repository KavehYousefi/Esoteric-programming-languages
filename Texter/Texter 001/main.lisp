;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Texter", invented by the Esolang user "BestCoder" and
;; presented on March 9th, 2024, the proprium of whose existency is
;; patefied in the exclusive attrectation of strings.
;; 
;; 
;; Concept
;; =======
;; Entertaining its edification upon a firmament of string manipulation,
;; the Texter programming language's compass enumerates such avails as
;; to homologate the reversal, concatenation, selective removal, as well
;; as conditional substitution adhibited upon character sequences.
;; 
;; == TEXTER: A STRING MANIPULATION LANGUAGE ==
;; A consectary's gendrure from the language's devotion to character
;; catenae's processing, Texter's warklumes lays its amplection around
;; several polymechanies concerning the modification of strings, its
;; competences, however, in any other bailiwick tholing an eloignment
;; from their entelechies' enjoyment.
;; 
;; == TEXTER THARFS NO MEMORY ==
;; Ensuing from its purpose in strings' attrectation, Texter itself
;; neither demurs at a memory architecture's carency, nor incorporates
;; thilk adminiculum's services among its paraphernalia.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical adhibition of one's conspection, the constitution
;; assigned to a Texter program limns an ordered sequence of zero or
;; more lines, each twissel's sepiment a newline entity, the horizontal
;; dispansions each an optional expression's dimense.
;; 
;; == A TEXT PROGRAM: A CATENA OF LINES ==
;; A Texter program's firmament is edified upon the notion of statements
;; as lines, each such at most one expression's commorancy, wisting of
;; a newline character as the compernage's merist.
;; 
;; == STATEMENTS ARE REPRESENTED BY EXPRESSIONS ==
;; Texter's plasmature concerning a statement's designment derives from
;; the principle of expressions, their delegates either literal strings,
;; endowed with no particular demarcation, grouping parentheses, unary,
;; as well as binary operations.
;; 
;; == THE BACKSLASH ESCAPES A CHARACTER ==
;; A dioristic set of characters' ipsissima verba participation in a
;; literal string is encumbered with an interdiction, ensuing from an
;; alligation into an operative wike's nomothesia. Such content,
;; natheless, does not thole a disqualification of enker rigor, the
;; warklume to its alternative construe as a string's constituent a
;; prevenient backslash character, "\", the succeeding symbol always
;; retains its verbatim conformation.
;; 
;; == GRAMMAR ==
;; A more formalized mete of the language's donet's expression shall be
;; the following Extended Backus-Naur Form (EBNF) treatise's cynosure:
;; 
;;   program        := { innerLine } , [ desinentLine ] ;
;;   innerLine      := [ expression ] , newline ;
;;   desinentLine   := [ expression ] ;
;;   
;;   expression      := independentExpr | binaryOperation ;
;;   binaryOperation := expression , binaryOperator , expression ;
;;   binaryOperator  := "*" | "^" | "/" | "=" | "-" | "<" | ">" ;
;;   unaryOperation  := reverseExpr ;
;;   reverseExpr     := "!" , independentExpr ;
;;   group           := "(" , expression , ")" ;
;;   independentExpr := literal | group | reverseExpr ;
;;   
;;   literal        := literalChar , { literalChar } ;
;;   literalChar    := nonCommandChar | escapedChar ;
;;   escapedChar    := "\" , character ;
;;   nonCommandChar := character - commandChar ;
;;   commandChar    := "!" | "*" | "^" | "/" | "="
;;                  |  "-" | "<" | ">" | "(" | ")"
;;                  ;
;;   newline        := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Texter's operative warklumes enumerates an ennead in contingency,
;; their purposes the exclusive bailiwick of string manipulations.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be realized in a requisite mete
;; of gnarity's communication anent the language's operative avails.
;; 
;; Please heed that the characters "a" and "b" designate succedaneous
;; symbols, their positions intended for a substitution by a Texter
;; expression in the program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ! a     | Reverses {a} and returns the result.
;;   ..................................................................
;;   a * b   | Appends {b} to {a} and returns the result.
;;   ..................................................................
;;   a ^ b   | Reverses {a}, appends {b} to the same, and returns the
;;           | result.
;;   ..................................................................
;;   a / b   | Removes all instances of {b} from {a} and returns the
;;           | result.
;;   ..................................................................
;;   a = b   | If {a} and {b} are equal, returns {a}; otherwise returns
;;           | "_".
;;   ..................................................................
;;   a - b   | If {a} and {b} are equal, returns "_"; otherwise returns
;;           | {a}.
;;   ..................................................................
;;   a > b   | If {a} equals "_", returns {b}; otherwise returns {a}.
;;   ..................................................................
;;   a < b   | If {a} equals "_", returns {a}; otherwise returns {b}.
;;   ..................................................................
;;   ( a )   | Groups the expression {a} and returns the result.
;;   ------------------------------------------------------------------
;; 
;; == CONDITIONAL SUBSTITUTION: AN EPEXEGETICAL TREATISE ==
;; Vindicated by the rather convolute haecceity commorant in the
;; tesseratomy of conditional substitution operands, scilicet, the
;; symbols "=", "-", ">", and "<", an epexegesis' vouchsafement has
;; been supputated a sensible additament to the instructions' treatise.
;; 
;; Ere the conditional operators' detailed elucidation, a proem whose
;; subject appertains to the deployed symbols shall be a tabulation's
;; cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Symbol | Signification
;;   -------+----------------------------------------------------------
;;   a      | The value of the expression equals the left operand.
;;   ..................................................................
;;   b      | The value of the expression equals the right operand.
;;   ..................................................................
;;   "_"    | The value of the expression equals the literal character
;;          | "_".
;;   ..................................................................
;;   *      | The value of the expression may be any content. If
;;          | empight on a line preceded by any other definition, the
;;          | respective contents are excluded from this liberty.
;;   ------------------------------------------------------------------
;; 
;; Proceeding from the aboon explications, the following nomothesia's
;; governail is extended over the four conditional retention operators:
;; 
;;   ==================================
;;   Retain if equal
;;   ----------------------------------
;;   Operator |  a  |  b  | Result
;;   ---------+-----+-----+------------
;;   =        |  b  |  a  |  a
;;            |-----+-----+------------
;;            |  *  |  *  | "_"
;;   ----------------------------------
;;   
;;   ==================================
;;   Retain if not equal
;;   ----------------------------------
;;   Operator |  a  |  b  | Result
;;   ---------+-----+-----+------------
;;   -        |  b  |  a  | "_"
;;            |-----+-----+------------
;;            |  *  |  *  |  a
;;   ----------------------------------
;;   
;;   ==================================
;;   Retain if not blank
;;   ----------------------------------
;;   Operator |  a  |  b  | Result
;;   ---------+-----+-----+------------
;;   >        | "_" |  *  |  b
;;            |-----+-----+------------
;;            |  *  |  *  |  a
;;   ----------------------------------
;;   
;;   ==================================
;;   Retain if blank
;;   ----------------------------------
;;   Operator |  a  |  b  | Result
;;   ---------+-----+-----+------------
;;   <        | "_" |  *  |  a (= "_")
;;            |-----+-----+------------
;;            |  *  |  *  |  b
;;   ----------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This Texter interpreter's implementation constitutes an effort
;; peracted in the programming language Common Lisp, its patefaction a
;; trisulc componency's champarty; imprimis, commencing in the Texter
;; source code string's tokens' extraction by per procurationem of a
;; dedicated lexer; succeeded by these components' assemblage through a
;; parser's labor into a hierarchical abstract syntax tree (AST)
;; reformulation of the communicated program; and experiencing its
;; desition in the interpreter entity's adhibition on this tree
;; structure in order to accompass actual efficacy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-04-15
;; 
;; Sources:
;;   [esolang:2024:Texter]
;;   The Esolang contributors, "Texter", March 14th, 2024
;;   URL: "https://esolangs.org/wiki/Texter"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*) (size '*))
  "The ``list-of'' type defines a list whose componency is edified upon
   a collation of zero or more elements, each member adhering to the
   ELEMENT-TYPE, the accompt, if specified, imposed to tally a SIZE
   cardinality."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq size '*)
              (= (length (the list candidate))
                 size))
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype token-type ()
  "The ``token-type'' type enumerates the recognized variation on token
   categories."
  '(member
    :backslash
    :concatenate
    :ecphoneme
    :eof
    :left-parenthesis
    :newline
    :remove
    :retain-if-blank
    :retain-if-equal
    :retain-if-not-blank
    :retain-if-not-equal
    :reverse
    :reverse-and-concatenate
    :right-parenthesis
    :word))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized variation on
   unary operators partaking in the Texter programming language"
  '(eql :reverse))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variation on
   binary operators partaking in the Texter programming language."
  '(member
    :concatenate
    :remove
    :reverse-and-concatenate
    :retain-if-equal
    :retain-if-not-equal
    :retain-if-not-blank
    :retain-if-blank))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list comprehending zero or
   more ``AST-Node'' instances."
  '(list-of AST-Node *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-as-a-boolean-value (object)
  "Administers a conspection of the OBJECT in its agency as a
   \"generalized boolean\" signifier and produces a veridicous Boolean
   paregal thereof, responding for a non-``NIL'' input with a
   ``boolean'' value of ``T''; otherwise, for a ``NIL'' OBJECT,
   responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE; either by a
   fresh instance's production of the demanded type, upon the SOURCE's
   deviation from this special species, or, upon its compatibility with
   the same, by returning the unaltered SOURCE itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun convert-into-a-simple-base-string (source)
  "Returns a simple base string representation of the SOURCE; either by
   a fresh instance's production of the demanded type, upon the SOURCE's
   deviation from this special species, or, upon its compatibility with
   the same, by returning the unaltered SOURCE itself."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))

;;; -------------------------------------------------------

(defun concatenate-the-strings (left-string right-string)
  "Creates and returns a fresh simple string obtained via an appendage
   of the RIGHT-STRING to the LEFT-STRING."
  (declare (type simple-string left-string))
  (declare (type simple-string right-string))
  (the simple-string
    (concatenate 'simple-string left-string right-string)))

;;; -------------------------------------------------------

(defun locate-the-substring-in (source desideratum &key (start 0))
  "Commencing at the inclusive START position into the SOURCE, seeks the
   entire DESIDERATUM and returns two values:
     (1) If the DESIDERATUM could be detected in the SOURCE, the
         former's inclusive start index into the latter; otherwise the
         length of the SOURCE.
     (2) If the DESIDERATUM could be detected in the SOURCE, the
         former's exclusive end index into the latter; otherwise the
         length of the SOURCE."
  (declare (type simple-string source))
  (declare (type simple-string desideratum))
  (declare (type fixnum        start))
  (the (values fixnum fixnum)
    (let ((source-length
            (length source))
          (desideratum-start
            (search desideratum source :start2 start :test #'char=)))
      (declare (type fixnum           source-length))
      (declare (type (or null fixnum) desideratum-start))
      (if desideratum-start
        (values
          desideratum-start
          (+ desideratum-start (length desideratum)))
        (values
          source-length
          source-length)))))

;;; -------------------------------------------------------

(defun remove-all-instances-of-the-substring-from (source delendum)
  "Returns a fresh simple string obtained by the expungement of all
   instances of the DELENDUM from the SOURCE."
  (declare (type simple-string source))
  (declare (type simple-string delendum))
  (the simple-string
    (convert-into-a-simple-string
      (with-output-to-string (new-content)
        (declare (type string-stream new-content))
        (loop
          with source-length  of-type fixnum = (length source)
          with copy-start     of-type fixnum = 0
          with delendum-start of-type fixnum = 0
          with delendum-end   of-type fixnum = 0
          do
            (multiple-value-setq (delendum-start delendum-end)
              (locate-the-substring-in source delendum
                :start delendum-end))
            (write-string source new-content
              :start copy-start
              :end   delendum-start)
            (setf copy-start delendum-end)
          while (< delendum-start source-length))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 11) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (convert-into-a-simple-base-string "*/!\\^=-><()")
  "Enumerates the recognized command names as constituents of a
   simple string.")

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE designates Texter command name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (find candidate +IDENTIFIERS+ :test #'char=))))

;;; -------------------------------------------------------

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a character utible as a
   linebreak signification, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (<= 10 (char-code candidate) 13))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the token.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-a-token      (type value))
  (:constructor make-an-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class is apportioned the wike of a cohesive unit's
   establishment inwith whose bournes are combined the pieces of
   information requisite for a significant object's delineations, the
   same is extracted from an analyzed piece of Texter source code, and
   which alligates a categorizing type with a detailing value."
  (type  (error "No token type has been communicated.")
         :type      token-type
         :read-only T)
  (value (error "No token value has been communicated.")
         :type      T
         :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-the-type-p (candidate expected-type)
  "Determines whether the CANDIDATE token ostends a type that matches
   the EXPECTED-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token      candidate))
  (declare (type token-type expected-type))
  (the boolean
    (interpret-as-a-boolean-value
      (eq (token-type candidate) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global token constants.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of token-type 7) +BINARY-OPERATORS+))

;;; -------------------------------------------------------

(defparameter +BINARY-OPERATORS+
  '(:concatenate
    :remove
    :reverse-and-concatenate
    :retain-if-equal
    :retain-if-not-equal
    :retain-if-not-blank
    :retain-if-blank)
  "Enumerates the recognized variation on binary operator identifiers
   in their symbolic format.")

;;; -------------------------------------------------------

(defun binary-operator-token-p (candidate)
  "Determines whether the CANDIDATE represents a token dedicated to the
   ensconcement of a binary operator, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (find (token-type candidate) +BINARY-OPERATORS+ :test #'eq))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the global token constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Token +EOF-TOKEN+))

;;; -------------------------------------------------------

(defparameter +EOF-TOKEN+
  (make-an-eof-token)
  "Represents the singleton end-of-file (EOF) token.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexical analyzer (lexer).              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "No source has been specified.")
    :type          simple-string
    :documentation "The piece of Texter source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The zero-based index of the currently selected
                    character into the SOURCE string.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE string."))
  (:documentation
    "The ``Lexer'' class is assigned the wike of a piece of Texter
     source code string's segregation into its tokens, twissel compounds
     of a categorizing type and a detailing datum, intended for a
     subsequent assemblage into an abstract syntax tree (AST) per
     procurationem of a parser."))

;;; -------------------------------------------------------

(defun update-the-current-character (lexer)
  "Adjusts the LEXER's current character as a perclose of its position
   cursor's contemporaneous state and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type simple-string       source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (schar source position))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initializes the LEXER's current character to that located under its
   contemporaneous position cursor into its source and returns no
   value."
  (declare (type Lexer lexer))
  (update-the-current-character lexer)
  (values))

;;; -------------------------------------------------------

(defun make-a-lexer-for (source)
  "Creates and returns a fresh ``Lexer'' dedicated to the Texter SOURCE
   string's evaluation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer
      :source (convert-into-a-simple-string source))))

;;; -------------------------------------------------------

(defun advance-to-the-next-character (lexer)
  "Advances the LEXER's position cursor to the next character in its
   underlying source and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position) lexer
    (declare (type simple-string source))
    (declare (type fixnum        position))
    (setf position
      (min
        (1+ position)
        (length source))))
  (update-the-current-character lexer)
  (values))

;;; -------------------------------------------------------

(defun more-characters-follow-p (lexer)
  "Determines whether at least one character succeeds the index occupied
   by the LEXER's current position cursor location, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (with-slots (source position) lexer
      (declare (type simple-string source))
      (declare (type fixnum        position))
      (interpret-as-a-boolean-value
        (array-in-bounds-p source
          (1+ position))))))

;;; -------------------------------------------------------

(defun peek-the-next-character (lexer)
  "Returns the character immediately succeeding the index occupied by
   LEXER's position cursor; or, upon the cursor's state being empight on
   the desinent location, responds with the ``NIL'' sentinel."
  (declare (type Lexer lexer))
  (the (or null character)
    (when (more-characters-follow-p lexer)
      (with-slots (source position) lexer
        (declare (type simple-string source))
        (declare (type fixnum        position))
        (schar source (1+ position))))))

;;; -------------------------------------------------------

(defun read-an-escaped-character (lexer destination)
  "Expecting the LEXER's position cursor to reside on a backslash
   character (\"\\\"), signifying the potential for escaped content,
   interprets such an escape sequence, prints the represented content
   to the DESTINATION stream, and returns no value."
  (declare (type Lexer         lexer))
  (declare (type string-stream destination))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (let ((next-character (peek-the-next-character lexer)))
      (declare (type (or null character) next-character))
      (cond
        (next-character
          (write-char next-character destination)
          (advance-to-the-next-character lexer)
          (advance-to-the-next-character lexer))
        (T
          (error "The escape sequence, commencing at the position ~d, ~
                  has not been terminated."
            position)))))
  (values))

;;; -------------------------------------------------------

(defun read-a-symbol (lexer token-type)
  "Returns a fresh token whose gendure limns a combination of the
   LEXER's current character with the specified TOKEN-TYPE, while
   concomitantly advancing to the next position in the LEXER's source."
  (declare (type Lexer      lexer))
  (declare (type token-type token-type))
  (the Token
    (with-slots (character) lexer
      (declare (type (or null character) character))
      (prog1
        (make-a-token token-type character)
        (advance-to-the-next-character lexer)))))

;;; -------------------------------------------------------

(defun read-a-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a word enumerating zero or more characters, and returns a conable
   ``:word'' token representation of the thus obtained content."
  (declare (type Lexer lexer))
  (the Token
    (with-slots (character) lexer
      (declare (type (or null character) character))
      (make-a-token :word
        (convert-into-a-simple-string
          (with-output-to-string (content)
            (declare (type string-stream content))
            (loop do
              (cond
                ((null character)
                  (loop-finish))
                ((newline-character-p character)
                  (loop-finish))
                ((char= character #\\)
                  (read-an-escaped-character lexer content))
                ((identifier-character-p character)
                  (loop-finish))
                (T
                  (write-char character content)
                  (advance-to-the-next-character lexer))))))))))

;;; -------------------------------------------------------

(defun request-the-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, each request is answered with a shared
   ``:eof'' token, identical to the global constant ``+EOF-TOKEN+''."
  (declare (type Lexer lexer))
  (the Token
    (with-slots (character) lexer
      (declare (type (or null character) character))
      (cond
        ((null character)
          +EOF-TOKEN+)
        ((newline-character-p character)
          (read-a-symbol lexer :newline))
        ((char= character #\*)
          (read-a-symbol lexer :concatenate))
        ((char= character #\/)
          (read-a-symbol lexer :remove))
        ((char= character #\!)
          (read-a-symbol lexer :ecphoneme))
        ((char= character #\^)
          (read-a-symbol lexer :reverse-and-concatenate))
        ((char= character #\=)
          (read-a-symbol lexer :retain-if-equal))
        ((char= character #\-)
          (read-a-symbol lexer :retain-if-not-equal))
        ((char= character #\>)
          (read-a-symbol lexer :retain-if-not-blank))
        ((char= character #\<)
          (read-a-symbol lexer :retain-if-blank))
        ((char= character #\()
          (read-a-symbol lexer :left-parenthesis))
        ((char= character #\))
          (read-a-symbol lexer :right-parenthesis))
        (T
          (read-a-word lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the abstract syntax tree (AST) nodes.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AST-Node)
  "The ``AST-Node'' interface serves in a firmament's establishment,
   entreparted by classes pursuing a Texter language facility's
   replication in an abstract syntax tree's (AST) guise.")

;;; -------------------------------------------------------

(defstruct (Binary-Node
  (:include     AST-Node)
  (:constructor make-a-binary-node (left-operand
                                    operator
                                    right-operand)))
  "The ``Binary-Node'' class serves in the ensconcement of a binary
   operation in an abstract syntax tree (AST) node's guise, its diorism
   a trisulk componency of the operand twissel and the operator."
  (left-operand  (error "No left operand has been committed.")
                 :type      AST-Node
                 :read-only T)
  (operator      (error "No operator has been committed.")
                 :type      binary-operator
                 :read-only T)
  (right-operand (error "No right operand has been committed.")
                 :type      AST-Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Group-Node
  (:include     AST-Node)
  (:constructor make-a-group-node (expression)))
  "The ``Group-Node'' class applies itself an expression's ensconcement
   in a cohesive unit."
  (expression (error "No expression has been committed.")
              :type      AST-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Literal-Node
  (:include     AST-Node)
  (:constructor make-a-literal-node (value)))
  "The ``Literal-Node'' class is apportioned the dever involving a
   word literal's ensconcement in an abstract syntax tree (AST) node's
   guise."
  (value (error "No value has been committed.")
         :type      simple-string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (NOP-Node
  (:include     AST-Node)
  (:constructor make-a-nop-node ()))
  "The ``NOP-Node'' class patefies a non-operative abstract syntax tree
   (AST) node's plasmature, its telos a proxy's furnishment for blank
   statements.")

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include     AST-Node)
  (:constructor make-a-program-node (statements)))
  "The ``Program-Node'' class applies itself to a root's furnishment for
   a parsed Texter program's abstract syntax tree (AST) representation,
   its diorism's a product of zero or more statement sub-trees."
  (statements (error "No statements have been committed.")
              :type      (list-of AST-Node)
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Node
  (:include     AST-Node)
  (:constructor make-a-unary-node (operator operand)))
  "The ``Unary-Node'' class serves in the ensconcement of a unary
   operation in an abstract syntax tree (AST) node's guise, its diorism
   a twyfold componency of the operator and the operand."
  (operator (error "No operator has been committed.")
            :type      unary-operator
            :read-only T)
  (operand  (error "No operand has been committed.")
            :type      AST-Node
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global AST node constants.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type NOP-Node +NOP-NODE+))

;;; -------------------------------------------------------

(defparameter +NOP-NODE+
  (make-a-nop-node)
  "The globally effective singleton instance of the ``NOP-Node''
   class, representative of a non-operative statement.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) AST-Node) parse-an-expression))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "No source for the token provision has been ~
                           furnished.")
    :type          Lexer
    :documentation "The token provision provenance.")
   (current-token
    :initform      +EOF-TOKEN+
    :type          Token
    :documentation "The most recently token acquisition from the
                    LEXER."))
  (:documentation
    "The ``Parser'' class establishes the entity concredited with the
     assemblage of a Texter program's abstract syntax tree (AST)
     plasmature from a sequence of tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Requests the next token from the PARSER's underlying lexer, stores
   the same in the PARSER, and returns no value."
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token
      (request-the-next-token lexer)))
  (values))

;;; -------------------------------------------------------

(defun make-a-parser (lexer)
  "Creates and returns a fresh ``Parser'' whose token obtainal's
   provenance is realized in the LEXER."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun consume-the-current-token (parser)
  "Returns the PARSER's current token, while concomitantly loading the
   next one from the underlying lexer and replacing the just delivered
   instance by its successor."
  (declare (type Parser parser))
  (the Token
    (with-slots (lexer current-token) parser
      (declare (type Lexer lexer))
      (declare (type Token current-token))
      (prog1 current-token
        (setf current-token
          (request-the-next-token lexer))))))

;;; -------------------------------------------------------

(defun expect-a-token-of-the-type (parser expected-type)
  "Determines whether the PARSER's current token complies with the
   EXPECTED-TYPE, returning on confirmation the same, while
   concomitantly querying and storing the subsequent token from the
   underlying lexer; otherwise, an error of an unspecified type is
   signaled."
  (declare (type Parser     parser))
  (declare (type token-type expected-type))
  (the Token
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (if (token-is-of-the-type-p current-token expected-type)
        (consume-the-current-token parser)
        (error "A token of the type ~s was expected, which conflicts ~
                with the actual token ~s."
          expected-type current-token)))))

;;; -------------------------------------------------------

(defun parse-an-independent-expression (parser)
  "Parses an independent expression, the species of which does not
   include in its haecceity the requisite for a sinistral or dextral
   operand, naiting the PARSER's services and returns a covenable
   ``AST-Node'' encapsulation of its content."
  (declare (type Parser parser))
  (the AST-Node
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (case (token-type current-token)
        (:word
          (make-a-literal-node
            (token-value
              (consume-the-current-token parser))))
        (:ecphoneme
          (consume-the-current-token parser)
          (make-a-unary-node :reverse
            (parse-an-independent-expression parser)))
        (:left-parenthesis
          (consume-the-current-token parser)
          (prog1
            (make-a-group-node (parse-an-expression parser))
            (expect-a-token-of-the-type parser :right-parenthesis)))
        (otherwise
          (error "The token ~s does not designate an expression."
            current-token))))))

;;; -------------------------------------------------------

(defun parse-an-expression (parser)
  "Parses an expression naiting the PARSER's services and returns a
   connable ``AST-Node'' representation of the thus yielded plasmature."
  (declare (type Parser parser))
  (the AST-Node
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (let ((node (parse-an-independent-expression parser)))
        (declare (type AST-Node node))
        (loop while (binary-operator-token-p current-token) do
          (let ((operator
                  (token-type
                    (consume-the-current-token parser))))
            (declare (type token-type operator))
            (setf node
              (make-a-binary-node node operator
                (parse-an-expression parser)))))
        (the AST-Node node)))))

;;; -------------------------------------------------------

(defun parse-the-program (parser)
  "Assembles the tokens concredited to the PARSER's castaldy into a
   Text program and returns a connable ``Program-Node'' representation
   thereof."
  (declare (type Parser parser))
  (the Program-Node
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (make-a-program-node
        (loop collect
          (case (token-type current-token)
            (:eof
              (loop-finish))
            (:newline
              (prog1 +NOP-NODE+
                (consume-the-current-token parser)))
            (otherwise
              (prog1
                (parse-an-expression parser)
                (unless (token-is-of-the-type-p current-token :eof)
                  (expect-a-token-of-the-type parser :newline))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the unary operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-the-unary-operator (operator operand)
  (:documentation
    "Applies the unary OPERATOR to its aefauld OPERAND and returns a
     simple string establishing the result.
     ---
     The yielded string object's identity does not thole any impounding
     nomothesia: A fresh instance metes a conceivable gendrure, as much
     as a reference to the OPERAND.
     ---
     The OPERAND will not be subjected to a destructive modification.")
  
  (:method ((operator (eql :reverse)) (operand string))
    (declare (type unary-operator operator))
    (declare (ignore              operator))
    (declare (type string         operand))
    (the simple-string
      (convert-into-a-simple-string
        (reverse operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 1) +BLANK-STRING+))

;;; -------------------------------------------------------

(defparameter +BLANK-STRING+
  (convert-into-a-simple-base-string "_")
  "Represents the underline character, \"_\", as a singleton string.")

;;; -------------------------------------------------------

(defgeneric apply-the-binary-operator (operator
                                       left-operand
                                       right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and the
     RIGHT-OPERAND in this exact order and returns a simple string as
     the result's establishment.
     ---
     The yielded string object's identity does not thole any impounding
     nomothesia: A fresh instance metes a conceivable gendrure, as much
     as a reference to the LEFT-OPERAND or the RIGHT-OPERAND.
     ---
     Neither the LEFT-OPERAND nor the RIGHT-OPERAND will be subjected to
     a destructive modification."))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :concatenate))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (concatenate-the-strings left-operand right-operand)))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :remove))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (remove-all-instances-of-the-substring-from
      left-operand
      right-operand)))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :reverse-and-concatenate))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (concatenate-the-strings
      (reverse left-operand)
      right-operand)))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :retain-if-equal))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (if (string= left-operand right-operand)
      left-operand
      +BLANK-STRING+)))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :retain-if-not-equal))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (if (string/= left-operand right-operand)
      left-operand
      +BLANK-STRING+)))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :retain-if-not-blank))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (if (string= left-operand +BLANK-STRING+)
      right-operand
      left-operand)))

;;; -------------------------------------------------------

(defmethod apply-the-binary-operator
    ((operator      (eql :retain-if-blank))
     (left-operand  string)
     (right-operand string))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type string          left-operand))
  (declare (type string          right-operand))
  (the simple-string
    (if (string= left-operand +BLANK-STRING+)
      left-operand
      right-operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "No program tree has been communicated.")
    :type          Program-Node
    :documentation "The abstract syntax tree's (AST) root node as a
                    parsed Texter program's representation."))
  (:documentation
    "The ``Interpreter'' class limns that dever's pernor to whom the
     execution of a Texter program, concredited in an abstract syntax
     tree's (AST) guise, constitutes the parcery."))

;;; -------------------------------------------------------

(defun make-an-interpreter-for (tree)
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   execution of abstract syntax TREE's (AST) representation of a Texter
   program."
  (declare (type Program-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defgeneric visit-the-node (interpreter node)
  (:documentation
    "Processes the abstract syntax tree (AST) NODE in the INTERPRETER's
     context and returns a connable value for this combination."))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Binary-Node))
  (declare (type Interpreter interpreter))
  (declare (type Binary-Node node))
  (the simple-string
    (apply-the-binary-operator
      (binary-node-operator node)
      (visit-the-node interpreter
        (binary-node-left-operand node))
      (visit-the-node interpreter
        (binary-node-right-operand node)))))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Group-Node))
  (declare (type Interpreter interpreter))
  (declare (type Group-Node  node))
  (the simple-string
    (visit-the-node interpreter
      (group-node-expression node))))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Literal-Node))
  (declare (type Interpreter  interpreter)
           (ignore            interpreter))
  (declare (type Literal-Node node))
  (the simple-string
    (literal-node-value node)))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        NOP-Node))
  (declare (type Interpreter interpreter)
           (ignore           interpreter))
  (declare (type NOP-Node    node)
           (ignore           node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (dolist (current-statement (program-node-statements node))
    (declare (type AST-Node current-statement))
    (print
      (visit-the-node interpreter current-statement)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-the-node ((interpreter Interpreter)
                           (node        Unary-Node))
  (declare (type Interpreter interpreter))
  (declare (type Unary-Node  node))
  (the simple-string
    (apply-the-unary-operator
      (unary-node-operator node)
      (visit-the-node interpreter
        (unary-node-operand node)))))

;;; -------------------------------------------------------

(defun execute-the-interpreter (interpreter)
  "Executes the Texter program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (visit-the-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-texter-code (code)
  "Interprets the piece of Texter source CODE and returns no value."
  (declare (type string code))
  (execute-the-interpreter
  (make-an-interpreter-for
    (parse-the-program
      (make-a-parser
        (make-a-lexer-for code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate the string "Hello, World!" through concatenation.
(interpret-the-texter-code "Hello, *World\\!")

;;; -------------------------------------------------------

;; Generate the string "Hello, World!" through reversal.
(interpret-the-texter-code "!\\!dlroW ,olleH")

;;; -------------------------------------------------------

;; Generate the string "Hello, World!" through reversal and
;; concatenation.
(interpret-the-texter-code "olleH^, World\\!")

;;; -------------------------------------------------------

;; Generate the string "Hello, World!" through conditional substitution
;; of an underscore ("_") by a space (" ").
(interpret-the-texter-code "Hello,*_> *World\\!")

;;; -------------------------------------------------------

;; Remove all instances of "XYZ" and thus generate the string "abcd".
(interpret-the-texter-code "aXYZbcdXYZ/XYZ")

;;; -------------------------------------------------------

;; Set the content to "_".
(interpret-the-texter-code "tuna-tuna")

;;; -------------------------------------------------------

;; Demonstrate the effect of grouping by generating the following
;; trisulc of strings:
;;   (1) "ottoto"
;;   (2) "otto"
;;   (3) "_".
(interpret-the-texter-code
  "ot*(to-ot)*to
ot*to-ot*to
(ot*to)-(ot*to)")
