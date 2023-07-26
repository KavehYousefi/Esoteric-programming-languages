;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Setlang", invented by the Esolang user "None1" and
;; presented in the year 2023, the composition of which relates to the
;; mimicry of the Python language's data structure symbols, pursuing to
;; commit input, output, and perform infinite loops.
;; 
;; 
;; Concept
;; =======
;; In its syntactical perspective, the Setlang programming language
;; emulates the symbols amplected by the Python programming language in
;; its various data structure designators, including parentheses, "("
;; and ")", bracket pairs of "[" and "]", and jumelles of braces, "{"
;; and "}", in its prosecution of input, output, and iterative
;; facilities.
;; 
;; 
;; Architecture
;; ============
;; Its governance by austerity regarding the persistent castaldy of data
;; serves in any requisitum's expulsion for such commerce.
;; 
;; 
;; Data Types
;; ==========
;; Setlang's competence does not protrude beyond the textual vale,
;; enumerating only strings and single characters as tokens of
;; deliberation.
;; 
;; 
;; Syntax
;; ======
;; The preponderance of Setlang's commands are delineated by a dedicated
;; pairing of grouping symbols, whose perimeter encompasses parentheses,
;; brackets, and braces. The ensconced arguments may either be commands
;; themselves, literal strings in double quotes, or single characters,
;; given their natural disambiguation.
;; 
;; == CHARACTER AND STRING LITERALS ==
;; The language's discrimination betwixt string literals as content
;; ensconced in double quotation marks and literal characters as single
;; entities destitute of further designations conflates into a single
;; textual concept.
;; 
;; A foible in their equiparation issues from the bare characters'
;; susceptibility to ambivalency in construe; namely, the menace of
;; confounding a character literal for a command token precludes a
;; certain set of symbols from its admission to the cause, and
;; subsequently impairs the attainment of equipollence for unadorned
;; character sequences.
;; 
;; == WHITESPACES ==
;; Whitespaces, an aggregate of spaces, tabs, and newline characters,
;; are admitted to liberality in their distribution.
;; 
;; == COMMENTS ==
;; No provision for comments are offered in the current specification
;; rendition.
;; 
;; == GRAMMAR ==
;; A formulation of the Setlang syntax in the Extended Backus-Naur Form
;; (EBNF) shall now follow:
;; 
;;   program          := statementList ;
;;   
;;   statementList    := { statement } ;
;;   statement        := input
;;                    |  concatenation
;;                    |  stringOutput
;;                    |  loop
;;                    ;
;;   expression       := literalString
;;                    |  literalCharacter
;;                    |  input
;;                    |  concatenation
;;                    ;
;;   input            := "{}" ;
;;   concatenation    := "{" , expression , { "," , expression } , "}" ;
;;   stringOutput     := "(" , expression , ")" ;
;;   loop             := "[" , statementList , "]" ;
;;   
;;   literalString    := '"' , { character - '"' } , '"' ;
;;   literalCharacter := character - ( commandCharacter | whitspace ) ;
;;   specialCharacter := '"' | "{" | "(" | "[" | "," ;
;;   whitespace       := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Setlang's instruction set registers a quintuple coeffiency for the
;; sake of input, output, and infinite iteration.
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity regarding the language's operations shall
;; now be adduced.
;; 
;; Please note that the placeholder portions of a command, intended to
;; be substituted by a valid piece of Setlang code, are underlined with
;; asterisks ("*"). Any other constituent must be stated verbatim.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   {}             | Queries the user for a single character and
;;                  | returns the same as a string.
;;                  |--------------------------------------------------
;;                  | This operation, constituting an expression, can
;;                  | be utilized in any place admissive to strings.
;;   ..................................................................
;;   {e1,e2,...,eN} | Concatenates the expressions {e1} through {eN}
;;    ** ** *** **  | into a single string, destitute of any
;;                  | adscititous sepiment, and returns the same.
;;                  |--------------------------------------------------
;;                  | {e1} through {eN} must be a sequence of one or
;;                  | more expressions, each either being
;;                  |   - a literal string
;;                  |   - a non-command literal character
;;                  |   - a string resulting from an expression.
;;                  |--------------------------------------------------
;;                  | Any two accolent elements e[i] and e[j] must be
;;                  | segregated by exactly one comma (",").
;;                  |--------------------------------------------------
;;                  | This operation, constituting an expression, can
;;                  | be utilized in any place admissive to strings.
;;   ..................................................................
;;   (argument)     | Prints the {argument} to the standard output and
;;    ********      | returns no value.
;;                  |--------------------------------------------------
;;                  | The {argument} must be an expression, that is,
;;                  | either:
;;                  |   - a literal string
;;                  |   - a non-command literal character
;;                  |   - a string resulting from an expression.
;;   ..................................................................
;;                  | Prints the message "Nope." to the standard ouput.
;;                  |--------------------------------------------------
;;                  | This command can only be recognized if the
;;                  | is blank or empty.
;;   ..................................................................
;;   [statements]   | Repeatedly executes the {statements} in an
;;    **********    | infinite loop.
;;                  |--------------------------------------------------
;;                  | The {statements} must be a sequence of zero or
;;                  | more statements.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The curtailed nature of the Setlang protolog --- contingently an
;; epiphenomenon of its recency --- inflicts the language with several
;; exposition to ambiguity, whence a subset shall be issued below.
;; 
;; == HOW ARE CHARACTER LITERALS COMMUNICATED? ==
;; The standard describes the eligiblity of characters as an adjunct of
;; traditional, double-quoted strings for text specifications. It is,
;; however, not mentioned which symbols are admitted, and how, if at
;; all, their delimitation from the surroundings shall proceed.
;; 
;; It has been chosen to permit single characters only in contexts where
;; delimited strings of arbitrary extent are homologated, and thus
;; restrain the contingencies for ambiguities. Further, tokens
;; introducing commands, as well as whitespaces as aesthetical
;; instruments, are interdicted from participating in this raw form.
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been accompassed in the programming language
;; Common Lisp.
;; 
;; The entirety of the program relates of a tripartite endeavour, its
;; componency the production of the ordered stages
;; 
;;   (1) Lexical analyzation ...
;;       ... which evaluates a string containing the Setlang source code
;;       in order to generate a series of tokens that describe the
;;       pertinent objects in the same.
;;   (2) Parsing ...
;;       ... whence, by assembling the received tokens, an abstract
;;       syntax tree (AST) node hierarchy is developed that models the
;;       Setlang program by its encapsulated components.
;;   (3) Interpretation ...
;;       ... as the traversal and processing of the AST subtrees while
;;       prosecuting the application of actual effect to its facilities.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-23
;; 
;; Sources:
;;   [esolang2023Setlang]
;;   The Esolang contributors, "Setlang", 2023
;;   URL: "https://esolangs.org/wiki/Setlang"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the comprehensive ``T''."
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

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   compact of zero or more entries, each indicator, or key, of which
   conforms to the INDICATOR-TYPE and associates with a value of the
   VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and (typep indicator indicator-type)
                     (typep value     value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype token-type ()
  "The ``token-type'' type enumerates the recognized variants of token
   categories."
  '(member
    :eof
    :left-brace
    :right-brace
    :left-bracket
    :right-bracket
    :left-parenthesis
    :right-parenthesis
    :comma
    :string
    :character))

;;; -------------------------------------------------------

(deftype node-type ()
  "The ``node-type'' type enumerates the recognized variants of node
   categories."
  '(member
    :program
    :input
    :concatenate
    :output-string
    :output-nope
    :loop
    :string-literal))

;;; -------------------------------------------------------

(deftype attribute-list ()
  "The ``attribute-list'' type defines a dictionary of attribute
   name-value twains for a ``Node'', manifesting as a property list, the
   indicators of which assume symbol keywords, and affiliate with any
   object of the type ``T''."
  '(property-list-of keyword T))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   embracing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token ()
  ((type
    :initarg       :type
    :initform      (error "Missing token type.")
    :reader        get-token-type
    :type          token-type
    :documentation "The categorizing type.")
   (value
    :initarg       :value
    :initform      (error "Missing token value.")
    :reader        get-token-value
    :type          T
    :documentation "The particular value."))
  (:documentation
    "The ``Token'' class represents a significant object extracted from
     a piece of Setlang source code, and delineated by a twain of a
     categorizing type and a detailing value."))

;;; -------------------------------------------------------

(defun make-token (type value)
  "Creates and returns a new ``Token'' categorized via its TYPE and
   detailed by its VALUE."
  (declare (type token-type type))
  (declare (type T          value))
  (the Token
    (make-instance 'Token :type type :value value)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token      token))
  (declare (type token-type expected-type))
  (the boolean
    (not (null
      (eq (get-token-type token) expected-type)))))

;;; -------------------------------------------------------

(defmethod print-object ((token Token) stream)
  (declare (type Token       token))
  (declare (type destination stream))
  (format stream "(Token type=~s value=~s)"
    (get-token-type  token)
    (get-token-value token)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :reader        get-lexer-source
    :type          string
    :documentation "The piece of Setlang source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :accessor      lexer-position
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :accessor      lexer-character
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class applies itself to the lexical analyzation of a
     piece of Setlang source code by extracting its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initalizes the LEXER's current character to the first position in its
   source, if possible, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p
            (get-lexer-source lexer)
            (lexer-position lexer))
      (char
        (get-lexer-source lexer)
        (lexer-position lexer))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' whose competence appertains to
   the evaluation of the Setlang SOURCE string."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Returns the LEXER's current character, ere advancing to the next
   position in its source, if possible."
  (declare (type Lexer lexer))
  (the (or null character)
    (prog1
      (lexer-character lexer)
      (setf (lexer-character lexer)
        (when (array-in-bounds-p
                (get-lexer-source lexer)
                (1+ (lexer-position lexer)))
          (char
            (get-lexer-source lexer)
            (incf (lexer-position lexer))))))))

;;; -------------------------------------------------------

(defun read-string (lexer)
  "Proceeding from the LEXER's current position into its source, reads
   a string ensconced in double quotation marks and returns a
   ``:string'' token representation thereof."
  (declare (type Lexer lexer))
  (advance-lexer lexer)
  (the Token
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop do
          (case (lexer-character lexer)
            ((NIL)
              (error "Unterminated string at position ~d."
                (lexer-position lexer)))
            (#\"
              (advance-lexer lexer)
              (loop-finish))
            (otherwise
              (write-char (advance-lexer lexer) content))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Obtains the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (case (lexer-character lexer)
      ((NIL)
        (make-token :eof NIL))
      ((#\Space #\Tab #\Newline)
        (advance-lexer  lexer)
        (get-next-token lexer))
      (#\{
        (make-token :left-brace
          (advance-lexer lexer)))
      (#\}
        (make-token :right-brace
          (advance-lexer lexer)))
      (#\[
        (make-token :left-bracket
          (advance-lexer lexer)))
      (#\]
        (make-token :right-bracket
          (advance-lexer lexer)))
      (#\(
        (make-token :left-parenthesis
          (advance-lexer lexer)))
      (#\)
        (make-token :right-parenthesis
          (advance-lexer lexer)))
      (#\,
        (make-token :comma
          (advance-lexer lexer)))
      (#\"
        (read-string lexer))
      (otherwise
        (make-token :character
          (advance-lexer lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of AST node.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      (error "Missing node type.")
    :reader        get-node-type
    :type          node-type
    :documentation "The node's categorizing type.")
   (attributes
    :initarg       :attributes
    :initform      NIL
    :accessor      get-attributes
    :type          attribute-list
    :documentation "A property list containing the attribute name-value
                    pairs."))
  (:documentation
    "The ``Node'' class models an abstract syntax tree (AST) node or
     subtree, compact of a categorizing type and a property list
     containing zero or more attributes."))

;;; -------------------------------------------------------

(defun make-node (type &rest attributes)
  "Creates and returns a new ``Node'', categorized by its TYPE and
   described by the ATTRIBUTES, the same must be provided in the form of
   a variadic property list, that is, a sequence of keywords and
   arbitrary attributes in this order, where each twain of successive
   elements specifies an attribute name-value pair."
  (declare (type node-type      type))
  (declare (type attribute-list attributes))
  (the Node
    (make-instance 'Node :type type :attributes attributes)))

;;; -------------------------------------------------------

(defun get-attribute (node attribute-name)
  "Returns the value of the attribute registered with the ATTRIBUTE-NAME
   at the NODE, or signals an error of an unspecified type upon its
   absence."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (let ((attribute-value
          (getf (get-attributes node) attribute-name :none)))
    (declare (type T attribute-value))
    (the T
      (if (eq attribute-value :none)
        (error "Invalid node attribute name: ~s." attribute-name)
        attribute-value))))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "(Node type=~s attributes=~s"
    (get-node-type  node)
    (get-attributes node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) node-list) parse-statements))
(declaim (ftype (function (Parser) Node)      parse-expression))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :reader        get-lexer
    :type          Lexer
    :documentation "The lexer responsible for the provision of tokens,
                    the most recent acquisition of which is contained in
                    the CURRENT-TOKEN slot.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :accessor      current-token
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class' onus entails the assemblage of an abstract
     syntax tree (AST) representation from a series of tokens supplied
     by a lexer."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Initializes the PARSER's current token by querying its internally
   managed lexer and returns the modified PARSER."
  (declare (type Parser parser))
  (setf (current-token parser)
    (get-next-token
      (get-lexer parser)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' whose tokens are purveyed by the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun expect-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly querying its successor from the PARSER's
   internally managed lexer and replacing the current token by the same;
   aliter an error of an unspecified type is signaled."
  (declare (type Parser     parser))
  (declare (type token-type expected-token-type))
  (the Token
    (if (token-type-p (current-token parser) expected-token-type)
      (prog1
        (current-token parser)
        (setf (current-token parser)
          (get-next-token
            (get-lexer parser))))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (current-token parser)))))

;;; -------------------------------------------------------

(defun parse-concatenation (parser)
  "Parses a concatenation or input command using the PARSER and returns
   either a ``:concatenation'' or ``:input'' node representation
   thereof."
  (declare (type Parser parser))
  (expect-token parser :left-brace)
  (the Node
    (case (get-token-type (current-token parser))
      (:right-brace
        (expect-token parser :right-brace)
        (make-node :input))
      (otherwise
        (let ((strings NIL))
          (declare (type node-list strings))
          (loop
            initially
              (unless (token-type-p (current-token parser) :comma)
                (push (parse-expression parser) strings))
            while
              (token-type-p (current-token parser) :comma)
            do
              (expect-token parser :comma)
              (push (parse-expression parser) strings))
          (expect-token parser :right-brace)
          (make-node :concatenate
            :components (nreverse strings)))))))

;;; -------------------------------------------------------

(defun parse-character-literal (parser)
  "Parses a character literal using the PARSER and returns a
   ``string-literal'' representation thereof."
  (declare (type Parser parser))
  (the Node
    (make-node :string-literal
      :value
        (string
          (get-token-value
            (expect-token parser :character))))))

;;; -------------------------------------------------------

(defun parse-string-literal (parser)
  "Parses a string literal using the PARSER and returns a
   ``:string-literal'' representation thereof."
  (declare (type Parser parser))
  (the Node
    (make-node :string-literal
      :value
        (get-token-value
          (expect-token parser :string)))))

;;; -------------------------------------------------------

(defun parse-expression (parser)
  "Parses an expression using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (the Node
    (case (get-token-type (current-token parser))
      (:character
        (parse-character-literal parser))
      (:string
        (parse-string-literal parser))
      (:left-brace
        (parse-concatenation parser))
      (otherwise
        (error "No expression token: ~s."
          (current-token parser))))))

;;; -------------------------------------------------------

(defun parse-string-output (parser)
  "Parses a string output command using the PARSER and returns a
   ``:output-string'' node representation thereof."
  (declare (type Parser parser))
  (expect-token parser :left-parenthesis)
  (the Node
    (prog1
      (make-node :output-string
        :argument (parse-expression parser))
      (expect-token parser :right-parenthesis))))

;;; -------------------------------------------------------

(defun parse-loop (parser)
  "Parses an infinite loop using the PARSER and returns a ``:loop'' node
   representation thereof."
  (declare (type Parser parser))
  (expect-token parser :left-bracket)
  (the Node
    (prog1
      (make-node :loop :statements (parse-statements parser))
      (expect-token parser :right-bracket))))

;;; -------------------------------------------------------

(defun parse-nope-output (parser)
  "Parses a \"Nope.\" output command using the PARSER and returns a
   ``:output-nope'' node representation thereof."
  (declare (type Parser parser))
  (expect-token parser :eof)
  (the Node
    (make-node :output-nope)))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses a statement using the PARSER and returns a node representation
   thereof."
  (declare (type Parser parser))
  (the Node
    (case (get-token-type (current-token parser))
      (:left-brace
        (parse-concatenation parser))
      (:left-parenthesis
        (parse-string-output parser))
      (:left-bracket
        (parse-loop parser))
      (otherwise
        (error "Invalid statement token: ~s."
          (current-token parser))))))

;;; -------------------------------------------------------

(defun statement-token-p (token)
  "Determines whether the TOKEN introduces a statement, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (get-token-type token)
        '(:string :left-brace :left-parenthesis :left-bracket)
        :test #'eq)))))

;;; -------------------------------------------------------

(defun parse-statements (parser)
  "Parses a sequence of zero or more adjacent statements using the
   PARSER and returns a list containing these in their specified order."
  (declare (type Parser parser))
  (the node-list
    (loop
      while   (statement-token-p (current-token parser))
      collect (parse-statement parser))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a program using the PARSER and returns a ``:program'' node
   representation thereof."
  (declare (type Parser parser))
  (the Node
    (make-node :program
      :statements
        (if (token-type-p (current-token parser) :eof)
          (list (make-node :output-nope))
          (parse-statements parser)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree.")
    :reader        get-program-tree
    :type          Node
    :documentation "The abstract syntax tree (AST) representation of the
                    Setlang program to evaluate."))
  (:documentation
    "The ``Interpreter'' class serves to accompass actual effect to a
     Setlang program communicated in the form of an abstract syntax tree
     (AST)."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a new ``Interpreter'' which evaluates the
   abstract syntax TREE (AST) representation of a Setlang program."
  (declare (type Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE, dispatched on its NODE-TYPE, in the
     INTERPRETER's context, and returns a value appropriate for this
     combination."))

;;; -------------------------------------------------------

(defun visit-node (interpreter node)
  "Processes the NODE in the INTERPRETER's context by dispatching on the
   NODE's type and subsequently invoking the eligible ``dispatch-node''
   generic function implementation, returning its respective value."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T
    (dispatch-node interpreter
      (get-node-type node)
      node)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :program))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  "Processes a NODE of the ``:program'' NODE-TYPE in the INTERPRETER's
   context by evaluating its statement nodes and returns no value."
  (dolist (statement (get-attribute node :statements))
    (declare (type Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :string-literal))
                          (node        Node))
  "Processes the NODE of the ``:string-literal'' NODE-TYPE by returning,
   without the INTERPRETER's adminiculum, the NODE's string value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the string
    (get-attribute node :value)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :input))
                          (node        Node))
  "Queries the user for a line of input, ignoring the INTERPRETER,
   NODE-TYPE and NODE, and returns the response as a string."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (declare (ignore           node))
  (format T "~&>> ")
  (the string
    (prog1
      (string (read-char *standard-input* NIL #\Null))
      (clear-input))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :concatenate))
                          (node        Node))
  "Processes the NODE of the NODE-TYPE in the INTERPRETER's context by
   concatenating its arguments into a single string, which is
   subsequently returned."
  (declare (type Interpreter interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the string
    (with-output-to-string (joined-string)
      (declare (type string-stream joined-string))
      (dolist (component (get-attribute node :components))
        (declare (type Node component))
        (format joined-string "~a"
          (visit-node interpreter component))))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :output-string))
                          (node        Node))
  "Processes the NODE of the NODE-TYPE in the INTERPRETER's context by
   printing its argument to the standard output and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (format T "~&~a"
    (visit-node interpreter
      (get-attribute node :argument)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :output-nope))
                          (node        Node))
  "Processes the NODE of the NODE-TYPE without the INTERPRETER's
   contribution by printing the message \"Nope.\" to the standard
   output and returns no value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (declare (ignore           node))
  (format T "~&Nope.")
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :loop))
                          (node        Node))
  "Processes the NODE of the NODE-TYPE in the INTERPRETER's context by
   infinitely repeating the NODE's statements, theoretically returning
   no value."
  (declare (type Interpreter interpreter))
  (declare (type node-type   node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (loop do
    (dolist (statement (get-attribute node :statements))
      (declare (type Node statement))
      (visit-node interpreter statement)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the abstract syntax tree (AST) maintained by the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (get-program-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Setlang (code)
  "Interprets the piece of Setlang source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World!".
(interpret-Setlang "(\"Hello World!\")")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Setlang "[({})]")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language "Nope.".
(interpret-Setlang "")

;;; -------------------------------------------------------

;; Query the user for a single character of their name, here designated
;; as {NAME}, and output the message
;;   Hello, {NAME}! How are you?
(interpret-Setlang "( { \"Hello, \", {}, !, \" How are you?\" } )")

;;; -------------------------------------------------------

;; Output the bare character "H" as a string.
(interpret-Setlang "(H)")
