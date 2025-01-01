;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Do-if", invented by the Esolang user "Martsadas" and
;; presented on August 28th, 2021, its kenspeckle proprium as well as
;; agnomination accounting in their gendrure for the twissel of
;; conditional "do ... if" and iterative "do ... while" mechanisms,
;; operating in champarty with variables and input/output facilities
;; in order to capacitate the perquisition and modulation of integer
;; and string objects.
;; 
;; 
;; Concept
;; =======
;; The Do-if programming language's cynosure is airted at the twain of
;; conditional "do ... if" construct and the iterance faciility
;; "do ... while", while consigning its integer and string data to the
;; castaldy of variables.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical exercise of the conspecuity, a Do-if program
;; tallies an ordered sequence of zero or more statements, the species'
;; conformation amplecting basic statement designs, such mimicking
;; function invocations, and composite specimens commorant in the
;; conditional and iterative realms.
;; 
;; == INSTRUCTIONS ==
;; The instruction department subsumes into a variety of appearances:
;; 
;;   (1) ATOMAR STATEMENTS
;;       These specimens emulate a fundamental statement, comprehending
;;       as the aefauld member the "let" specimen.
;;   
;;   (2) FUNCTIONS
;;       A membership of this category expresses its structure in a
;;       functional guise, succeeding the identifier by a jumelle of
;;       braces, inwith which may wone zero or more arguments. The
;;       expression "INPN {}" and the statement "PRINT {...}" exhausts
;;       the type.
;;   
;;   (3) COMPOUND STATMENTS
;;       Composite statements are defined by an amplectation of zero or
;;       more statements or expressions in a common block, enumerating
;;       in this case the "do ... if" and "do ... while" elements.
;; 
;; == EXPRESSIONS ==
;; The contingency for explicitly stated expressions bifurcates into
;; the integer and string types.
;; 
;; == INTEGER EXPRESSIONS ==
;; The integer diorism admits a signed or unsigned decimal form,
;; without impositions on the numeric value's mickleness.
;; 
;; == STRING EXPRESSIONS ==
;; A twifold variation applies to the designment of strings, permitting
;; both such ensconced in apostrophes ("'...'") and an alternative
;; betwixt double quotes ('"..."').
;; 
;; The embraced content may comprehend any element desumed from the
;; underlying character repertoire, including a particular set of
;; compounds serving as succedanea: the escape sequences.
;; 
;; == ESCAPE SEQUENCES ==
;; Escape sequences homologate the introduction of special characters
;; into a string in a convenient form, their parasceuastic component
;; realized in a prefixion by a single backslash ("\") symbol.
;; 
;; The following escape sequences entertain their accommodation in the
;; Do-if programming language:
;; 
;;   ------------------------------------------------------------------
;;   Escape sequence | Interpretation
;;   ----------------+-------------------------------------------------
;;   \n              | Substituted by a single newline character.
;;   ..................................................................
;;   \t              | Substituted by a horizontal tab.
;;   ..................................................................
;;   \\              | Substituted by a single backslash ("\").
;;   ------------------------------------------------------------------
;; 
;; Any character succeeding a backslash and being absent from the aboon
;; listing is retained verbatim, with the prevenient backslash's
;; expungement.
;; 
;; == WHITESPACES ==
;; The requisitum of whitespaces, whose diorism amplects the space,
;; horizontal tab, and newline entities, represents the foundation for
;; the single language tokens' discrimination; at any other occasion,
;; however, siccan presence enjoys mere tolerance without epiphenomenal
;; puissance.
;; 
;; == COMMENTS ==
;; No contingency for the inclusion of comments partakes of this
;; language rendition's dation.
;; 
;; == GRAMMAR ==
;; The language's donet shall enjoy a enhanced mete of stringency in
;; its treatment by an Extended Backus-Naur Form (EBNF) formulation:
;; 
;;   program            := statementList ;
;;   statementList      := { statement } ;
;;   statement          := letStatement
;;                      |  printStatement
;;                      |  doIfStatement
;;                      |  doWhileStatement
;;                      ;
;;   letStatement       := "let" , variableName , "=" , expression ;
;;   printStatement     := "PRINT" , "{" , printArguments , "}" ;
;;   printArguments     := [ expression , { "," , expression } ] ;
;;   doIfStatement      := doStatements , "if"    , expression ;
;;   doWhileStatement   := doStatements , "while" , expression ;
;;   doStatements       := "do" , statementList ;
;;   
;;   expression         := integerLiteral
;;                      |  stringLiteral
;;                      |  variableName
;;                      |  binaryExpression
;;                      |  relationExpression
;;                      |  input
;;                      ;
;;   
;;   input              := "INPN" , "{" , "}" ;
;;   
;;   binaryExpression   := expression , binaryOperator , expression ;
;;   binaryOperator     := "+" | "-" | "*" | "/" ;
;;   
;;   relationExpression := expression , relationOperator , expression;
;;   relationOperator   := "=" | "<" | ">" ;
;;   
;;   stringLiteral      := doubleQuotedString | singleQuotedString ;
;;   doubleQuotedString := '"'
;;                      ,  { ( character - '"' ) | escapeSequence }
;;                      ,  '"'
;;                      ;
;;   singleQuotedString := "'"
;;                      ,  { ( character - "'" ) | escapeSequence }
;;                      ,  "'"
;;                      ;
;;   escapeSequence     := "\" , character ;
;;   
;;   integerLiteral     := [ "+" | "-" ] , digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;; 
;; 
;; Instructions
;; ============
;; Do-if's instruction set tallies a quintuple componency, in whose
;; compass are admitted such members as to capacitate the declaration
;; and assignment of variables, the request of input and issuance of
;; output, as well as a conditional execution and a while-based iterance
;; construct.
;; 
;; == OVERVIEW ==
;; The following apercu shall be apply itself to a basic nortelry's
;; adhibition concerning the Do-if language's operative competences.
;; 
;; Please that the underlining of succedaneous parcels by adminuculum
;; of catenas of asterisks ("*"), the demarcated locations are intended
;; to be supersesed by actual Do-if tmemata code in the ultimate
;; program.
;; 
;;   ------------------------------------------------------------------
;;   Command             | Effect
;;   --------------------+---------------------------------------------
;;   let varName = value | If a variable amenable to the {varName}
;;       *******   ***** | exists, supersedes its content by the
;;                       | {value}, otherwise declare such variable ere
;;                       | the assignment.
;;   ..................................................................
;;   do                  | Executes the {statements} while the
;;     statements        | {antecedent} is satisfied; otherwise
;;     **********        | produces no causta.
;;   while antecedent    |---------------------------------------------
;;      **********       | The {statements} must be sequence of zero or
;;                       | more statements.
;;                       |---------------------------------------------
;;                       | The {antecedent} must be an expression.
;;                       | A Boolean truth value of "false" designates
;;                       | a failure; any other reponse communicates
;;                       | an affirmation.
;;   ..................................................................
;;   do                  | Executes the {statements} if the
;;     statements        | {antecedent} is satisfied; otherwise
;;     **********        | produces no causata.
;;   if antecedent       |---------------------------------------------
;;      **********       | The {statements} must be sequence of zero or
;;                       | more statements.
;;                       |---------------------------------------------
;;                       | The {antecedent} must be an expression.
;;                       | A Boolean truth value of "false" designates
;;                       | a failure; any other reponse communicates
;;                       | an affirmation.
;;   ..................................................................
;;   PRINT {arguments}   | Prints the {arguments} to the standard
;;          *********    | output, no sepiment governing their
;;                       | intermedes, and no implicit linebreak
;;                       | empight on its desinence.
;;                       |---------------------------------------------
;;                       | The {arguments} must be a sequence of zero
;;                       | or more expressions, each twissel segregated
;;                       | by an aefauld comma (","). Strings, if
;;                       | partaking of this arrangement, may
;;                       | comprehend escape sequences.
;;   ..................................................................
;;   INPN {}             | Queries the standard input for a signed or
;;                       | unsigned integer number and returns thilk.
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS ==
;; The involvement of expressions in the Do-if programming languages
;; acquires manifold apparitions:
;; 
;;   (1) INTEGER LITERALS
;;       Integer numbers may be inserted into the program in a literal
;;       form as signed or unsigned decimal numbers, disencumbered from
;;       any impositions of its mickleness.
;;   
;;   (2) STRING LITERALS
;;       The specification of string literals proceeds by means of zero
;;       or more characters, their ensconcement either reified in a
;;       jumelle of apostrophes ("'") or double quotes ('"').
;;   
;;   (3) INTEGER INPUT
;;       The inquisition for user input manifests in the instruction
;;       "INPN {}", thilk queries the standard input conduit for a
;;       signed or unsigned decimal integer number, returning the
;;       response verbatim.
;;   
;;   (4) COMPOUND EXPRESSIONS
;;       Complex expressions are elicited by an edification from two
;;       or more atomar or composite specimens, desumed either from the
;;       arithmetic or lexicographic vales, concluding in a response
;;       originating from the same provenance, or subsuming into the
;;       Boolean category.
;; 
;; == UNARY OPERATORS ==
;; A twissel of unary operations experience their admission to the
;; arithmetic realm:
;; 
;;   ------------------------------------------------------------------
;;   Unary operator | Operand | Result
;;   ---------------+---------+----------------------------------------
;;   +              | boolean | invalid
;;                  |--------------------------------------------------
;;                  | integer | identity
;;                  |--------------------------------------------------
;;                  | string  | invalid
;;   ..................................................................
;;   -              | boolean | invalid
;;                  |--------------------------------------------------
;;                  | integer | negation
;;                  |--------------------------------------------------
;;                  | string  | invalid
;;   ------------------------------------------------------------------
;; 
;; == BINARY OPERATORS ==
;; A quadruple membership exhausts the binary operators' dation, the
;; diorisms appertaining exclusively to the arithmetic and lexicographic
;; bailiwicks:
;; 
;;   ------------------------------------------------------------------
;;   Binary   | Left    | Right   | Result
;;   operator | operand | operand | 
;;   ---------+---------+---------+------------------------------------
;;   +        | boolean | boolean | invalid
;;            |--------------------------------------------------------
;;            | boolean | integer | invalid
;;            |--------------------------------------------------------
;;            | boolean | string  | invalid
;;            |--------------------------------------------------------
;;            | integer | integer | addition
;;            |--------------------------------------------------------
;;            | integer | string  | string concatenation
;;            |--------------------------------------------------------
;;            | string  | string  | string concatenation
;;   ..................................................................
;;   -        | boolean | boolean | invalid
;;            |--------------------------------------------------------
;;            | boolean | integer | invalid
;;            |--------------------------------------------------------
;;            | boolean | string  | invalid
;;            |--------------------------------------------------------
;;            | integer | integer | subtraction
;;            |--------------------------------------------------------
;;            | integer | string  | invalid
;;            |--------------------------------------------------------
;;            | string  | string  | invalid
;;   ..................................................................
;;   *        | boolean | boolean | invalid
;;            |--------------------------------------------------------
;;            | boolean | integer | invalid
;;            |--------------------------------------------------------
;;            | boolean | string  | invalid
;;            |--------------------------------------------------------
;;            | integer | integer | multiplication
;;            |--------------------------------------------------------
;;            | integer | string  | invalid
;;            |--------------------------------------------------------
;;            | string  | string  | invalid
;;   ..................................................................
;;   /        | boolean | boolean | invalid
;;            |--------------------------------------------------------
;;            | boolean | integer | invalid
;;            |--------------------------------------------------------
;;            | boolean | string  | invalid
;;            |--------------------------------------------------------
;;            | integer | integer | integer division
;;            |--------------------------------------------------------
;;            | integer | string  | invalid
;;            |--------------------------------------------------------
;;            | string  | string  | invalid
;;   ------------------------------------------------------------------
;; 
;; == RELATIONAL OPERATORS ==
;; A treble contingency of relational operators exists, their conduct
;; with the available data types shall be the following section's
;; cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Relational | Left    | Right   | Result
;;   operator   | operand | operand | 
;;   -----------+---------+---------+----------------------------------
;;   <          | boolean | boolean | invalid
;;              |------------------------------------------------------
;;              | boolean | integer | invalid
;;              |------------------------------------------------------
;;              | boolean | string  | invalid
;;              |------------------------------------------------------
;;              | integer | integer | less than (arithmetic)
;;              |------------------------------------------------------
;;              | integer | string  | always false (contradiction)
;;              |------------------------------------------------------
;;              | string  | string  | less than (lexicographic)
;;   ..................................................................
;;   =          | boolean | boolean | invalid
;;              |------------------------------------------------------
;;              | boolean | integer | invalid
;;              |------------------------------------------------------
;;              | boolean | string  | invalid
;;              |------------------------------------------------------
;;              | integer | integer | equality (arithmetic)
;;              |------------------------------------------------------
;;              | integer | string  | always false (contradiction)
;;              |------------------------------------------------------
;;              | string  | string  | equality (lexicographic)
;;   ..................................................................
;;   >          | boolean | boolean | invalid
;;              |------------------------------------------------------
;;              | boolean | integer | invalid
;;              |------------------------------------------------------
;;              | boolean | string  | invalid
;;              |------------------------------------------------------
;;              | integer | integer | greater than (arithmetic)
;;              |------------------------------------------------------
;;              | integer | string  | always false (contradiction)
;;              |------------------------------------------------------
;;              | string  | string  | greater than (lexicographic)
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp utilizes a recursive
;; descent parser, extended and complemented by a Pratt parser component
;; for the processing of expressions.
;; 
;; == INTERPRETATION CONSTITUTES A TRIPLE STAGE PROCESS ==
;; The complete interpretation is defined in terms of a graduated
;; processing, separable into three chief tiers:
;; 
;;   (1) A lexer generates from the Do-if program string a sequence
;;       of tokens.
;;   (2) The parser queries these tokens and assembles an abstract
;;       syntax tree (AST), the nodes of which represent the language
;;       facilities.
;;   (3) The interpreter traverses the AST and embues it with effect.
;; 
;; == EXPRESSIONS ARE ASSEMBLED VIA PRATT PARSING ==
;; The parser combines aspects of recursive descent and Pratt's
;; solution, with the former apprehending the general process, aided by
;; the latter for the assemblage of expressions. The Pratt component's
;; conventions and notions are partially derived from Denis
;; Lantsman [lantsman2018prattparsers].
;; 
;; 
;; Appendices
;; ==========
;; Ensuing from the documentation's main subjects, a certain repertoire
;; appertaining to topics harboring significance to a more remote mete
;; remain. This orra material, maugre its paravail status, shall be the
;; following sections' bailiwick.
;; 
;; == APPENDIX A: OPERATOR BINDING POWERS AND ASSOCIATIVITIES ==
;; This implementation's reliance upon the Pratt parser and its innate
;; components, the binding power and associativity, rede a compendious
;; tendance's adhibition upon the subject.
;; 
;; A listing of the available operators, their binding powers, and
;; associativity configurations shall be ostended in the following
;; tabular illustration:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Binding power | Associativity | Apostil
;;   ---------+---------------+---------------+------------------------
;;   (...)    | 0             | none          | Effectively endowed
;;            |               |               | with the highest
;;            |               |               | binding power.
;;   ..................................................................
;;   =        | 20            | right-to-left | Assignment.
;;   ..................................................................
;;   =        | 90            | left-to-right | Equality.
;;   ..................................................................
;;   <        | 100           | left-to-right | 
;;   ..................................................................
;;   >        | 100           | left-to-right | 
;;   ..................................................................
;;   +        | 130           | left-to-right | 
;;   ..................................................................
;;   -        | 130           | left-to-right | 
;;   ..................................................................
;;   *        | 140           | left-to-right | 
;;   ..................................................................
;;   /        | 140           | left-to-right | 
;;   ..................................................................
;;   - (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   + (sign) | 170           | right-to-left | 
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-12-26
;; 
;; Sources:
;;   [cppreference2023c++operprec]
;;   The cppreference contributors, "C++ Operator Precedence",
;;     September 10th, 2023
;;   URL: "https://en.cppreference.com/w/cpp/language/
;;         operator_precedence"
;;   Notes:
;;     - Lists the C++ operators in conjunction with their precedence
;;       and associativity attributes.
;;   
;;   [crockford2007topdownopprec]
;;   Douglas Crockford, "Top Down Operator Precedence", 2007-02-21
;;   URL: "http://crockford.com/javascript/tdop/tdop.html"
;;   Notes:
;;     - Describes a Pratt parser implementing a subset of JavaScript,
;;       yclept "Simplified JavaScript".
;;     - Introduces the concept of "statement denotation" ("std") for
;;       incorporating statements into the Pratt parser's homogeneous
;;       expression system.
;;   
;;   [crockford2010tdopparse.js]
;;   Douglas Crockford, "parse.js", 2010-06-26
;;   URL: "http://crockford.com/javascript/tdop/parse.js"
;;   Notes:
;;     - Presents the source code for a Pratt parser implementing a
;;       subset of JavaScript, yclept "Simplified JavaScript".
;;     - The main page, supplying elucidations, can be found under
;;       -> "https://crockford.com/javascript/tdop/tdop.html".
;;   
;;   [esolang2023Do-if]
;;   The Esolang contributors, "Do-if", October 8th, 2023
;;   URL: "https://esolangs.org/wiki/Do-if"
;;   
;;   [grand1997javalangref]
;;   Mark Grand,
;;     "Java Language Reference", 2nd Edition July 1997,
;;     "Chapter 4.14 Order of Operations",
;;     1997
;;   URL: "http://web.deu.edu.tr/doc/oreily/java/langref/ch04_14.htm"
;;   Notes:
;;     - Describes and lists the order of operations established in the
;;       Java programming language.
;;   
;;   [kumar2016javaprecassoc]
;;   Krishan Kumar, "Java Operators: Precedence and Associativity", 2016
;;   URL: "https://cs-fundamentals.com/java-programming/
;;         java-operators-precedence-and-associativity"
;;   Notes:
;;     - Lists the operator precedences and associativities specified
;;       for the Java programming language.
;;   
;;   [lantsman2018prattparsers]
;;   Denis Lantsman, "How Desmos uses Pratt Parsers", 2018
;;   URL: "https://engineering.desmos.com/articles/pratt-parser/"
;;   Notes:
;;     - Provides a pellucid explanation of the Pratt parser concept.
;;   
;;   [pratt1973top]
;;   Vaughan R. Pratt, "Top Down Operator Precedence", 1973
;;   URL: "https://daesan.com/wp-content/uploads/2018/05/
;;         top_down_operator_precedence.pdf"
;;   
;;   [pythonswfoundation2023operprec]
;;   Python Software Foundation,
;;     "The Python Language Reference",
;;     section 6.17, "Operator precedence",
;;     September 14th, 2023
;;   URL: "https://docs.python.org/3/reference/
;;         expressions.html#operator-precedence"
;;   Notes:
;;     - Lists the operators in Python in conjunction with their
;;       precedences.
;;   
;;   [sedgewick2022operatorprecedence]
;;   Robert Sedgewick, Kevin Wayne,
;;     "Appendix A: Operator Precedence in Java", 2022
;;   URL: "https://introcs.cs.princeton.edu/java/11precedence/"
;;   Notes:
;;     - Operator precedence in Java.
;;   
;;   [stackoverflow2011q2811319]
;;   The Stack Overflow contributors,
;;     "What is the difference between >>> and >> operators in Java?",
;;     2011
;;   URL: "https://stackoverflow.com/questions/2811319/
;;         difference-between-and"
;;   Notes:
;;     - Describes the ">>>" operator in Java, an unsigned right shift.
;;   
;;   [williams2022onrecursivedescent]
;;   URL: "https://chidiwilliams.com/post/
;;         on-recursive-descent-and-pratt-parsing/"
;;   Notes:
;;     - Introduces a "ParseRule" class and a "Precedence" enumerated
;;       type which in conjunction bundle the Pratt parser concepts.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a VALUE-TYPE, for both is reserved the generic sentinel ``*'' as
   a default."
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
  "The ``list-of'' type defines a list enumerating zero or more
   elements, each member among these complying to the ELEMENT-TYPE, its
   default prescribed as the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (and (symbolp element-type) (eq element-type '*))
              (loop
                for    current-element of-type T in (the list candidate)
                always (typep current-element element-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized identifiers
   for unary operations desumed from the arithmetic realm."
  '(member :plus :minus))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized identifiers
   for binary operations desumed from the arithmetic and relational
   realms."
  '(member
     :plus
     :minus
     :times
     :divided
     :less-than
     :equal-to
     :greater-than))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list comprehending zero or more
   abstract syntax tree (AST) nodes, each complying to the type
   ``AST-Node''."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the contingency for an
   operator's associativity configuration, utible in the resolution of
   conflicts betwixt participants of tantamount puissance."
  '(member :none :left-to-right :right-to-left))

;;; -------------------------------------------------------

(deftype nud-processor ()
  "The ``nud-processor'' type defines a functional object entalented
   with such a capacitation as to produce from a token stream and its
   most recently acquired and consumed \"null denotation\" (nud) token
   an abstract syntax tree (AST) node; as a consectary, complying with
   this signature:
     function (tokens    : Token-Stream,
               nud-token : Token)
              => AST-Node"
  '(function (Token-Stream Token) AST-Node))

;;; -------------------------------------------------------

(deftype led-processor ()
  "The ``led-processor'' type defines a functional object entalented
   with such a capacitation as to prodce from a token stream, its most
   recently acquired and consumed \"left denotation\" (led) token, and
   the already processed left expression, available as an abstract
   syntax tree (AST) node, another abstract syntax tree (AST) node; as
   a consectary, a compliance with the following signature is imposed:
     function (tokens    : Token-Stream,
               led-token : Token,
               left-node : AST-Node)
              => AST-Node"
  '(function (Token-Stream Token AST-Node) AST-Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which enlists, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its role as a generalized boolean entity,
   producing a veridical Boolean tantamount of thilk, and returns for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise responds
   with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from a piece of Do-if source code."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-type-p (token expected-type)
  "Determines whether the TOKEN complies with the EXPECTED-TOKEN-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifiers.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the Do-if language keywords with representative tokens.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token-type token-value)
        "Associates the identifier NAME with a fresh token assembled
         from the TOKEN-TYPE and the TOKEN-VALUE in the +IDENTIFIERS+
         table and returns no value."
        (declare (type string  name))
        (declare (type keyword token-type))
        (declare (type T       token-value))
        (setf (gethash name +IDENTIFIERS+)
              (make-token token-type token-value))
        (values)))
  (register-identifier "do"    :do    "do")
  (register-identifier "if"    :if    "if")
  (register-identifier "INPN"  :inpn  "INPN")
  (register-identifier "let"   :let   "let")
  (register-identifier "PRINT" :print "PRINT")
  (register-identifier "while" :while "while")
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token associated with the IDENTIFIER, or signals an error
   of an unspecified type upon its disrespondency."
  (declare (type string identifier))
  (the Token
    (or (gethash identifier +IDENTIFIERS+)
        (error "No identifier name: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   in its diorism amplecting the space, horizontal tab, and newline
   specimens, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Newline)
          (char= candidate #\Return)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for
   an identifier, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (alphanumericp candidate)
          (char=         candidate #\_)))))

;;; -------------------------------------------------------

(defun variable-name-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for
   a variable name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (alphanumericp candidate)
          (char=         candidate #\_)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable name operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-variable-name (name)
  "Determines whether the NAME represents a valid variable identifier,
   returning on confirmation thilk in an ipsissima verba fashion;
   otherwise signals an error of an unspecified type."
  (declare (type string name))
  (the string
    (or (and (plusp (length name)) name)
        (error "Invalid variable name: ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source for lexer.")
    :reader        lexer-source
    :type          string
    :documentation "The piece of Do-if source code to analyze.")
   (position
    :initform      0
    :accessor      lexer-position
    :type          fixnum
    :documentation "The zero-based index of the current character into
                    the SOURCE.")
   (character
    :initform      NIL
    :accessor      lexer-character
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class is invested with the dever of extracting from
     a piece of Do-if source code the encompassed tokens."))

;;; -------------------------------------------------------

(defun update-current-character (lexer)
  "Updates the LEXER's internally managed character to the entity at the
   current position into its source and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Queries the first character from the LEXER's source, stores thilk in
   the LEXER, and returns no value."
  (declare (type Lexer lexer))
  (update-current-character lexer)
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a fresh ``Lexer'' whose cynosure manifests in the
   piece of Do-if SOURCE code."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun advance-to-next-character (lexer)
  "Advances the LEXER to the next position in its source and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position) lexer
    (declare (type string source))
    (declare (type fixnum position))
    (setf position
      (min (1+ position) (length source))))
  (update-current-character lexer)
  (values))

;;; -------------------------------------------------------

(defun write-current-character (lexer destination)
  "Writes the LEXER's current character to the DESTINATION and returns
   no value."
  (declare (type Lexer       lexer))
  (declare (type destination destination))
  (write-char (lexer-character lexer) destination)
  (values))

;;; -------------------------------------------------------

(defun expect-character (lexer expected-character)
  "Determines whether the LEXER's current character equals the
   EXPECTED-CHARACTER, on confirmation advancing to the next position
   into the LEXER's source, while returning no value; otherwise an error
   of an unspecified is signaled."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (cond
      ((null character)
        (error "Expected the character \"~c\" at position ~d, but ~
                encountered the source exhausted."
          expected-character position))
      ((char/= character expected-character)
        (error "Expected the character \"~c\" at position ~d, but ~
                encountered \"~c\"."
          expected-character position character))
      (T
        (advance-to-next-character lexer))))
  (values))

;;; -------------------------------------------------------

(defun read-escape-sequence (lexer destination)
  "Proceeding from the current position into the LEXER, and expected to
   be empight immediately on the introducing backslash (\"\\\"), reads
   an escape sequence, writes the decoded entity to the DESTINATION,
   and returns no value."
  (declare (type Lexer       lexer))
  (declare (type destination destination))
  (expect-character lexer #\\)
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (case character
      ((null character)
        (error "Unterminated escape sequence at position ~d." position))
      (#\n
        (write-char #\Newline destination)
        (advance-to-next-character lexer))
      (#\t
        (write-char #\Tab destination)
        (advance-to-next-character lexer))
      (otherwise
        (write-current-character   lexer destination)
        (advance-to-next-character lexer))))
  (values))

;;; -------------------------------------------------------

(defun read-string-literal (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a string literal and returns a ``:string'' token representation
   thereof."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (let ((quote-character character))
      (declare (type character quote-character))
      (advance-to-next-character lexer)
      (the Token
        (make-token :string
          (with-output-to-string (content)
            (declare (type string-stream content))
            (loop do
              (cond
                ((null character)
                  (error "Unterminated string literal at position ~d."
                    position))
                ((char= character quote-character)
                  (advance-to-next-character lexer)
                  (loop-finish))
                ((char= character #\\)
                  (read-escape-sequence lexer content))
                (T
                  (write-current-character   lexer content)
                  (advance-to-next-character lexer))))))))))

;;; -------------------------------------------------------

(defun read-integer-literal (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned integer literal and returns an ``:integer''
   representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :integer
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-current-character   lexer digits)
              (advance-to-next-character lexer))))))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER, reads a Do-if
   language keyword and returns a conable token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier-token
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (with-slots (character) lexer
          (declare (type (or null character) character))
          (loop
            while
              (and character
                   (identifier-character-p character))
            do
              (write-current-character   lexer identifier)
              (advance-to-next-character lexer)))))))

;;; -------------------------------------------------------

(defun read-variable-name (lexer)
  "Proceeding from the current position into the LEXER, reads a variable
   name and returns a ``:variable'' token representation thereof."
  (declare (type Lexer lexer))
  (expect-character lexer #\$)
  (the Token
    (make-token :variable
      (validate-variable-name
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (with-slots (character) lexer
            (declare (type (or null character) character))
            (loop
              while
                (and character
                     (variable-name-character-p character))
              do
                (write-current-character   lexer identifier)
                (advance-to-next-character lexer))))))))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Creates and returns a fresh token by conjoining the TOKEN-TYPE with
   the LEXER's current character, while concomitantly advances the
   latter to the next position in its source."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (with-slots (character) lexer
      (declare (type (or null character) character))
      (prog1
        (make-token token-type character)
        (advance-to-next-character lexer)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (advance-to-next-character lexer)))
  (values))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end of file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (with-slots (character position) lexer
      (declare (type (or null character) character))
      (declare (type fixnum              position))
      (cond
        ((null character)
          (make-eof-token))
        ((whitespace-character-p character)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        ((alpha-char-p character)
          (read-identifier lexer))
        ((digit-char-p character)
          (read-integer-literal lexer))
        ((or (char= character #\") (char= character #\'))
          (read-string-literal lexer))
        ((char= character #\$)
          (read-variable-name lexer))
        ((char= character #\=)
          (read-symbol lexer :equal-to))
        ((char= character #\<)
          (read-symbol lexer :less-than))
        ((char= character #\>)
          (read-symbol lexer :greater-than))
        ((char= character #\+)
          (read-symbol lexer :plus))
        ((char= character #\-)
          (read-symbol lexer :minus))
        ((char= character #\*)
          (read-symbol lexer :times))
        ((char= character #\/)
          (read-symbol lexer :divided))
        ((char= character #\,)
          (read-symbol lexer :comma))
        ((char= character #\{)
          (read-symbol lexer :left-brace))
        ((char= character #\})
          (read-symbol lexer :right-brace))
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Stream ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for token stream.")
    :reader        token-stream-lexer
    :type          Lexer
    :documentation "The token obtention's provenance.")
   (next-token
    :initform      (make-eof-token)
    :accessor      token-stream-next-token
    :documentation "The most recently queried token from the LEXER."))
  (:documentation
    "The ``Token-Stream'' class serves a twifaced purpose: imprimis,
     attending to the furnishment of an abstract obtention of tokens
     from the perquired piece of Do-if source code; secondly, augmenting
     the lexer's avails by the contingency of peeking the subsequent
     token without its immediate consumption."))

;;; -------------------------------------------------------

(defun load-next-token (tokens)
  "Returns the TOKENS stream's current token, while concomitantly
   loading successor from the underlying lexer and storing thilk in the
   TOKENS."
  (declare (type Token-Stream tokens))
  (with-slots (lexer next-token) tokens
    (declare (type Lexer lexer))
    (declare (type Token next-token))
    (the Token
      (prog1 next-token
        (setf next-token
          (get-next-token lexer))))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokens Token-Stream) &key)
  "Queries the first from the TOKENS stream's underlying lexer, stores
   thilk in the TOKENS, and returns no value."
  (declare (type Token-Stream tokens))
  (load-next-token tokens)
  (values))

;;; -------------------------------------------------------

(defun make-token-stream (lexer)
  "Creates and returns a fresh ``Token-Stream'' whose provenance for the
   token acquisition is realized in the LEXER."
  (declare (type Lexer lexer))
  (the Token-Stream
    (make-instance 'Token-Stream :lexer lexer)))

;;; -------------------------------------------------------

(defun consume-token (tokens)
  "Consumes and returns the next token from the TOKENS stream."
  (declare (type Token-Stream tokens))
  (the Token
    (load-next-token tokens)))

;;; -------------------------------------------------------

(defun peek-token (tokens)
  "Returns without digestion the next token from the TOKENS stream."
  (declare (type Token-Stream tokens))
  (the Token
    (token-stream-next-token tokens)))

;;; -------------------------------------------------------

(defun expect-token (tokens expected-token-type)
  "Determines whether the statement TOKENS stream's next token complies
   with the EXPECTED-TOKEN-TYPE, on confirmation consuming and returning
   the probed object; otherwise an error of an unspecified type is
   signaled."
  (declare (type Token-Stream tokens))
  (declare (type keyword      expected-token-type))
  (the Token
    (if (token-is-of-type-p (peek-token tokens) expected-token-type)
      (consume-token tokens)
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (peek-token tokens)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AST-Node)
  "The ``AST-Node'' interface attends to a common foundry's furnishment
   to all concrete classes entalented with the telos of a Do-if language
   facility's modeling.")

;;; -------------------------------------------------------

(defstruct (Expression-Node
  (:include AST-Node))
  "The ``Expression-Node'' interface serves as a specialized interface
   entreparted by all concrete classes dedicated to a Do-If language
   expression's modeling.")

;;; -------------------------------------------------------

(defstruct (Assignment-Node
  (:include     AST-Node)
  (:constructor make-assignment-node (target value)))
  (target (error "Missing assignment node target.")
          :type      string
          :read-only T)
  (value  (error "Missing assignment node value.")
          :type      Expression-Node
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include     AST-Node)
  (:constructor make-block-node (statements)))
  "The ``Block-Node'' class provides an encapsulation of zero or more
   Do-If statements in the guise of ``AST-Node'' instances."
  (statements (error "Missing block node statements.")
              :type      (list-of AST-Node)
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include     Expression-Node)
  (:constructor make-binary-operation-node (operator
                                            left-operand
                                            right-operand)))
  "The ``Binary-Operation-Node'' class furnishes an encapsulation of a
   binary operation in an abstract syntax tree's (AST) guise."
  (operator      (error "Missing operator for binary operation node.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand for binary operation ~
                         node.")
                 :type      Expression-Node
                 :read-only T)
  (right-operand (error "Missing right operand for binary operation ~
                         node.")
                 :type      Expression-Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Do-If-Node
  (:include     AST-Node)
  (:constructor make-do-if-node (antecedent statements)))
  "The ``Do-If-Node'' class furnishes an encapsulation of a
   \"do ... if\" conditional statement in an abstract syntax tree's
   (AST) guise."
  (antecedent (error "Missing do-if node antecedent.")
              :type      Expression-Node
              :read-only T)
  (statements (error "Missing do-if node statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Do-While-Node
  (:include     AST-Node)
  (:constructor make-do-while-node (antecedent statements)))
  "The ``Do-While-Node'' class furnishes an encapsulation of a
   \"do ... while\" conditional statement in an abstract syntax tree's
   (AST) guise."
  (antecedent (error "Missing do-while node antecedent.")
              :type      Expression-Node
              :read-only T)
  (statements (error "Missing do-while node statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include     Expression-Node)
  (:constructor make-input-node ()))
  "The ``Input-Node'' class serves in the encapsulation of a numeric
   input request behest in an abstract syntax tree's (AST) guise.")

;;; -------------------------------------------------------

(defstruct (Integer-Literal-Node
  (:include     Expression-Node)
  (:constructor make-integer-literal-node (value)))
  "The ``Integer-Literal-Node'' class serves in the ensconcement of an
   integral literal in an abstract syntax tree's (AST) guise."
  (value (error "Missing value for integer literal node.")
         :type      (integer 0 *)
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Print-Node
  (:include     AST-Node)
  (:constructor make-print-node (arguments)))
  "The ``Print-Node'' class furnishes an abstract syntax tree (AST)
   representation of an output behest, amplecting in its diorism a list
   of the expression nodes to evaluate and display."
  (arguments (error "Missing print node arguments.")
             :type      (list-of Expression-Node)
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include     AST-Node)
  (:constructor make-program-node (statements)))
  "The ``Program-Node'' class serves in the encapsulation of a parsed
   Do-if program in the form of an abstract syntax tree (AST) nodes,
   these comprising a block node's membership."
  (statements (error "Missing program node statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (String-Literal-Node
  (:include     Expression-Node)
  (:constructor make-string-literal-node (value)))
  "The ``String-Literal-Node'' class serves in the ensconcement of a
   string literal in an abstract syntax tree's (AST) guise."
  (value (error "Missing value for string literal node.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Operation-Node
  (:include     Expression-Node)
  (:constructor make-unary-operation-node (operator operand)))
  "The ``Unary-Operation-Node'' class furnishes an encapsulation of a
   unary operation in an abstract syntax tree's (AST) guise."
  (operator (error "Missing operator for unary operation node.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing left node for unary operation node.")
            :type      Expression-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Node
  (:include     Expression-Node)
  (:constructor make-variable-node (name)))
  "The ``Variable-Node'' class serves in the encapsulation of a variable
   name in an abstract syntax tree (AST) context."
  (name (error "Missing variable node name.")
        :type      string
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Precedence".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence (binding-power associativity))
  (:constructor make-neutral-precedence (&aux (binding-power 0)
                                              (associativity :none))))
  "The ``Precedence'' class serves in the ensconcement of a operator's
   precedence information, the perimeter of which includes the numeric
   binding power as a warklume of resolving confrontations betwixt
   discrepant operators, and the associativity for the case of an
   equipollence's governance in the agonists' contributions."
  (binding-power (error "Missing precedence binding power.")
                 :type      integer
                 :read-only T)
  (associativity (error "Missing precedence associativity.")
                 :type      associativity
                 :read-only T))

;;; -------------------------------------------------------

(defun get-effective-binding-power (precedence)
  "Returns the PRECEDENCE's effective binding power, its derivation's
   provenance the fundamental binding power modulated by the influence
   of the associativity."
  (declare (type Precedence precedence))
  (the integer
    (case (precedence-associativity precedence)
      ((:none :left-to-right)
        (precedence-binding-power precedence))
      (:right-to-left
        (1- (precedence-binding-power precedence)))
      (otherwise
        (error "Invalid associativity: ~s."
          (precedence-associativity precedence))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Parselet".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parselet)
  "The ``Parselet'' abstract class furnishes a substratum entreparted by
   all concrete classes intended to provide parselet functionalities."
  (precedence (make-neutral-precedence)
              :type      Precedence
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Nud-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Nud-Parselet
  (:include     Parselet)
  (:constructor make-nud-parselet
    (processor
     &optional (precedence (make-neutral-precedence)))))
  "The ``Nud-Parselet'' class applies itself to the bailiwick of a nud
   token's transformation into an abstract syntax tree (AST) node
   representation."
  (processor (error "Missing nud processor.")
             :type      nud-processor
             :read-only T))

;;; -------------------------------------------------------

(defun apply-nud-parselet (parselet tokens nud-token)
  "Parses the NUD-TOKEN employing the PARSELET, the TOKENS stream acting
   as a contingency for further tokens' obtention, and returns an
   connable ``AST-Node'' representation of the result."
  (declare (type Nud-Parselet parselet))
  (declare (type Token-Stream tokens))
  (declare (type Token        nud-token))
  (the AST-Node
    (funcall
      (nud-parselet-processor parselet)
      tokens
      nud-token)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Led-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Led-Parselet
  (:include     Parselet)
  (:constructor make-led-parselet (processor precedence)))
  "The ``Led-Parselet'' class applies itself to the bailiwick of a led
   token's transformation into an abstract syntax tree (AST) node
   representation."
  (processor (error "Missing led processor.")
             :type      led-processor
             :read-only T))

;;; -------------------------------------------------------

(defun apply-led-parselet (parselet tokens led-token left-node)
  "Parses the LED-TOKEN employing the PARSELET, the TOKENS stream acting
   as a contingency for further tokens' obtention, while the LEFT-NODE
   contributes the already extant sinistral operand, and returns a
   connable ``AST-Node'' representation of the result."
  (declare (type Led-Parselet parselet))
  (declare (type Token-Stream tokens))
  (declare (type Token        led-token))
  (declare (type AST-Node     left-node))
  (the AST-Node
    (funcall
      (led-parselet-processor parselet)
      tokens
      led-token
      left-node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Pratt-Parser".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Pratt-Parser ()
  ((nud-parselets
    :initform      (make-hash-table :test #'eq)
    :accessor      nud-parselets
    :type          (hash-table-of keyword Nud-Parselet)
    :documentation "Associates the recognized nud token types with
                    representative parselets.")
   (led-parselets
    :initform      (make-hash-table :test #'eq)
    :reader        led-parselets
    :type          (hash-table-of keyword Led-Parselet)
    :documentation "Associates the recognized led token types with
                    representative parselets."))
  (:documentation
    "The ``Pratt-Parser'' class' bailiwick constitutes the competence
     involving the expression parsing, in which wike's compass wones the
     castaldy of the recognized nud and led token types, allotting to
     everichon among these a parselet responsible for its transformation
     into an abstract syntax tree (AST) node."))

;;; -------------------------------------------------------

(defun make-pratt-parser ()
  "Creates and returns a fresh ``Pratt-Parser'', initially destitute of
   any nud or led parselet components."
  (the Pratt-Parser
    (make-instance 'Pratt-parser)))

;;; -------------------------------------------------------

(defun register-nud-parselet (parser token-type parselet)
  "Affiliates the TOKEN-TYPE with the nud PARSELET in the Pratt PARSER
   and returns no value."
  (declare (type Pratt-Parser parser))
  (declare (type keyword      token-type))
  (declare (type Nud-Parselet parselet))
  (setf (gethash token-type (nud-parselets parser))
        parselet)
  (values))

;;; -------------------------------------------------------

(defun nud-token-p (parser token)
  "Determines whether the TOKEN represents a nud token as the
   consequence of the Pratt PARSER's administered docimasy, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token)
          (nud-parselets parser))))))

;;; -------------------------------------------------------

(defun get-nud-parselet (parser token)
  "Returns for the TOKEN, expected to constitute a nud specimen, the
   affiliated parselet."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the Nud-Parselet
    (or (gethash (token-type token)
          (nud-parselets parser))
        (error "The token ~s does not represent a nud specimen."
          token))))

;;; -------------------------------------------------------

(defun get-nud-precedence (parser token)
  "Returns for the TOKEN, expected to constitute a nud specimen, the
   affiliated precedence."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the Precedence
    (parselet-precedence
      (get-nud-parselet parser token))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (parser token)
  "Returns for the TOKEN, expected to constitute a nud specimen, the
   affiliated effective binding power."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the integer
    (get-effective-binding-power
      (get-nud-precedence parser token))))

;;; -------------------------------------------------------

(defun parse-nud-token (parser tokens token)
  "Parses the nud TOKEN in concord with the Pratt Parser's accommodated
   parselet, employing the TOKENS stream for contingent further data's
   obtention, and returns an ``AST-Node'' representation of the parsing
   result."
  (declare (type Pratt-Parser parser))
  (declare (type Token-Stream tokens))
  (declare (type Token        token))
  (the AST-Node
    (apply-nud-parselet
      (get-nud-parselet parser token)
      tokens token)))

;;; -------------------------------------------------------

(defun register-led-parselet (parser token-type parselet)
  "Affiliates the TOKEN-TYPE with the led PARSELET in the Pratt PARSER's
   context and returns no value."
  (declare (type Pratt-Parser parser))
  (declare (type keyword      token-type))
  (declare (type Led-Parselet parselet))
  (setf (gethash token-type (led-parselets parser))
        parselet)
  (values))

;;; -------------------------------------------------------

(defun led-token-p (parser token)
  "Determines whether the TOKEN represents a led specimen in concord
   with the Pratt PARSER's definitions, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token)
          (led-parselets parser))))))

;;; -------------------------------------------------------

(defun get-led-parselet (parser token)
  "Expecting the TOKEN to represent a led specimen, returns the parselet
   affiliated with thilk in the Pratt PARSER's context."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the Led-Parselet
    (or (gethash (token-type token)
          (led-parselets parser))
        (error "The token ~s does not represent a led token." token))))

;;; -------------------------------------------------------

(defun get-led-precedence (parser token)
  "Expecting the TOKEN to represent a led specimen, returns the
   precedence affiliated with thilk in the Pratt PARSER's context."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the Precedence
    (parselet-precedence
      (get-led-parselet parser token))))

;;; -------------------------------------------------------

(defun get-led-binding-power (parser token)
  "Expecting the TOKEN to represent a led specimen, returns the
   effective binding power affiliated with thilk in the Pratt PARSER's
   context."
  (declare (type Pratt-Parser parser))
  (declare (type Token        token))
  (the integer
    (get-effective-binding-power
      (get-led-precedence parser token))))

;;; -------------------------------------------------------

(defun parse-led-token (parser tokens token left-node)
  "Parses the led TOKEN in concord with the Pratt PARSER's accommodated
   parselet, employing the already extant LEFT-NODE as the sinistral
   operand, while providing the TOKENS stream as a warklume for
   contingent further tokens' obtention, and returns an ``AST-Node''
   representation of the parsing result."
  (declare (type Pratt-Parser parser))
  (declare (type Token-Stream tokens))
  (declare (type Token        token))
  (declare (type AST-Node     left-node))
  (the AST-Node
    (apply-led-parselet
      (get-led-parselet parser token)
      tokens token left-node)))

;;; -------------------------------------------------------

(defun token-can-bind-expression-p (parser
                                    candidate-token
                                    competing-binding-power)
  "Determines whether the CANDIDATE-TOKEN may bind an expression if
   confronted with the COMPETING-BINDING-POWER, which shall be
   supputated as tenable if (1) the CANDIDATE-TOKEN represents a led
   token as defined in the Pratt PARSER's context, and (2) this led
   token's effective binding power strictly exceeds the
   COMPETIG-BINDING-POWER, representative of a competitor in this agon
   to acquire the desiderated expression, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Pratt-Parser parser))
  (declare (type Token        candidate-token))
  (declare (type integer      competing-binding-power))
  (the boolean
    (get-boolean-value-of
      (and (led-token-p parser candidate-token)
           (> (get-led-binding-power parser candidate-token)
              competing-binding-power)))))

;;; -------------------------------------------------------

(defun parse-expression (parser tokens current-binding-power)
  "Parses the expression in the Pratt PARSER's context, the invoking
   entity being representing by its effective CURRENT-BINDING-POWER,
   while deploying the TOKENS stream for further tokens' obtention, and
   returns an ``AST-Node'' representation of the parsing result."
  (declare (type Pratt-Parser parser))
  (declare (type Token-Stream tokens))
  (declare (type integer      current-binding-power))
  (let ((left-node
          (parse-nud-token parser tokens
            (consume-token tokens))))
    (declare (type AST-Node left-node))
    (loop
      for next-token of-type Token = (peek-token tokens)
      if (token-can-bind-expression-p
           parser
           next-token
           current-binding-power) do
        (consume-token tokens)
        (setf left-node
          (parse-led-token parser tokens next-token left-node))
      else do
        (loop-finish)
      end)
    (the AST-Node left-node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of convenience parselet operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-binary-operation-parselet (parser
                                           operator
                                           binding-power
                                           associativity)
  "Creates a fresh led parselet, endowed with the BINDING-POWER and the
   ASSOCIATIVITY as the components of its inherent precedence, the
   parselet producing upon its application a ``Binary-Operation-Node''
   whose operation type is desumed from the OPERATOR specification."
  (declare (type Pratt-Parser    parser))
  (declare (type binary-operator operator))
  (declare (type integer         binding-power))
  (declare (type associativity   associativity))
  (the Binary-Operation-Node
    (register-led-parselet parser operator
      (make-led-parselet
        #'(lambda (tokens token left-node)
            (declare (type Token-Stream tokens))
            (declare (type Token        token))
            (declare (type AST-Node     left-node))
            (the Binary-Operation-node
              (make-binary-operation-node operator left-node
                (parse-expression parser tokens
                  (get-led-binding-power parser token)))))
        (make-precedence binding-power associativity)))))

;;; -------------------------------------------------------

(defun register-standard-parselets (parser)
  "Registers the standard nud and led parselets at the Pratt PARSER and
   returns the modified PARSER."
  (declare (type Pratt-parser parser))
  
  (register-nud-parselet parser :inpn
    (make-nud-parselet
      #'(lambda (tokens token)
          (declare (type Token-Stream tokens))
          (declare (type Token        token))
          (declare (ignore            token))
          (the Input-Node
            (prog1
              (make-input-node)
              (expect-token tokens :left-brace)
              (expect-token tokens :right-brace))))))
  
  (register-nud-parselet parser :integer
    (make-nud-parselet
      #'(lambda (tokens token)
          (declare (type Token-Stream tokens))
          (declare (ignore            tokens))
          (declare (type Token        token))
          (the Integer-Literal-Node
            (make-integer-literal-node
              (token-value token))))))
  
  (register-nud-parselet parser :minus
    (make-nud-parselet
      #'(lambda (tokens token)
          (declare (type Token-Stream tokens))
          (declare (type Token        token))
          (the Unary-Operation-Node
            (make-unary-operation-node :minus
              (parse-expression parser tokens
                (get-nud-binding-power parser token)))))
      (make-precedence 170 :right-to-left)))
  
  (register-nud-parselet parser :plus
    (make-nud-parselet
      #'(lambda (tokens token)
          (declare (type Token-Stream tokens))
          (declare (type Token        token))
          (the Unary-Operation-Node
            (make-unary-operation-node :plus
              (parse-expression parser tokens
                (get-nud-binding-power parser token)))))
      (make-precedence 170 :right-to-left)))
  
  (register-nud-parselet parser :string
    (make-nud-parselet
      #'(lambda (tokens token)
          (declare (type Token-Stream tokens))
          (declare (ignore            tokens))
          (declare (type Token        token))
          (the String-Literal-Node
            (make-string-literal-node
              (token-value token))))))
  
  (register-nud-parselet parser :variable
    (make-nud-parselet
      #'(lambda (tokens token)
          (declare (type Token-Stream tokens))
          (declare (ignore            tokens))
          (declare (type Token        token))
          (the Variable-Node
            (make-variable-node
              (token-value token))))))
  
  (register-binary-operation-parselet parser :less-than
                                             100
                                             :left-to-right)
  (register-binary-operation-parselet parser :equal-to
                                             100
                                             :left-to-right)
  (register-binary-operation-parselet parser :greater-than
                                             100
                                             :left-to-right)
  (register-binary-operation-parselet parser :plus
                                             130
                                             :left-to-right)
  (register-binary-operation-parselet parser :minus
                                             130
                                             :left-to-right)
  (register-binary-operation-parselet parser :times
                                             140
                                             :left-to-right)
  (register-binary-operation-parselet parser :divided
                                             140
                                             :left-to-right)
  
  (the Pratt-Parser parser))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Statement-Parser".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Statement-Parser) Block-Node)
                parse-statement-block))

;;; -------------------------------------------------------

(defclass Statement-Parser ()
  ((token-provider
    :initarg       :token-provider
    :initform      (error "Missing token provider for statement ~
                           parser.")
    :reader        get-token-provider
    :type          Token-Stream
    :documentation "")
   (expression-parser
    :initarg       :expression-parser
    :initform      (error "Missing expression parser for statement ~
                           parser.")
    :reader        get-expression-parser
    :type          Pratt-Parser
    :documentation "The Pratt parser responsible for the parsing of
                    expressions."))
  (:documentation
    "The ``Statement-Parser'' class establishes the main parsing entity,
     its dever the assemblage of a Do-if program's abstract syntax tree
     (AST) representation, for which enterprise the contributions of a
     token stream, as the tokens' provenance, and a Pratt expression
     parser, for the production of expression nodes, partake of a
     peisant coefficient agency."))

;;; -------------------------------------------------------

(defun make-statement-parser (expression-parser token-provider)
  "Creates and returns a fresh ``Statement-Parser'' whose token
   acquisition constitutes the TOKEN-PROVIDER's bailiwick, while the
   assemblage of expressions derives from the EXPRESSION-PARSER's
   investments."
  (declare (type Token-Stream token-provider))
  (the Statement-Parser
    (make-instance 'Statement-Parser
      :expression-parser expression-parser
      :token-provider    token-provider)))

;;; -------------------------------------------------------

(defun get-current-token (parser)
  "Returns the statement PARSER's current token."
  (declare (type Statement-Parser parser))
  (the Token
    (peek-token
      (get-token-provider parser))))

;;; -------------------------------------------------------

(defun current-token-type (parser)
  "Returns the type of the PARSER's current token."
  (declare (type Statement-Parser parser))
  (the keyword
    (token-type
      (get-current-token parser))))

;;; -------------------------------------------------------

(defun current-token-is-of-type-p (parser expected-type)
  "Determines whether the PARSR's current token complies with the
   EXPECTED-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Statement-Parser parser))
  (declare (type keyword          expected-type))
  (the boolean
    (token-is-of-type-p
      (get-current-token parser)
      expected-type)))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the statement PARSER's current token complies with
   the EXPECTED-TOKEN-TYPE, on confirmation returning the probed object,
   while concomitantly advancing to the next one in the underlying token
   stream; otherwise an error of an unspecified type is signaled."
  (declare (type Statement-Parser parser))
  (declare (type keyword          expected-token-type))
  (the Token
    (expect-token
      (get-token-provider parser)
      expected-token-type)))

;;; -------------------------------------------------------

(defun apply-expression-parser (parser current-binding-power)
  "Parses an expression by adminiculum of the Pratt parser commorant in
   the statement PARSER, employing the CURRENT-BINDING-POWER as its
   invocating puissance, and returns an ``AST-Node'' representation of
   the result."
  (declare (type Statement-Parser parser))
  (declare (type integer          current-binding-power))
  (the AST-Node
    (parse-expression
      (get-expression-parser parser)
      (get-token-provider    parser)
      current-binding-power)))

;;; -------------------------------------------------------

(defun expression-follows-p (parser)
  "Determines whether the PARSER's current token introduces an
   expression, ascertained if a nud token presents itself, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Statement-Parser parser))
  (the boolean
    (nud-token-p
      (get-expression-parser parser)
      (get-current-token     parser))))

;;; -------------------------------------------------------

(defun parse-expresson-list (parser)
  "Parses a sequence of zero or more expressions, each twissel
   demarcated by an aefauld comma (\",\"), and returns a list of
   expression nodes."
  (declare (type Statement-Parser parser))
  (the (list-of Expression-Node)
    (when (expression-follows-p parser)
      (let ((arguments NIL))
        (declare (type (list-of Expression-Node) arguments))
        (loop
          initially
            (push (apply-expression-parser parser 0) arguments)
          while (current-token-is-of-type-p parser :comma) do
            (eat-token parser :comma)
            (push (apply-expression-parser parser 0) arguments)
          finally
            (return
              (nreverse arguments)))))))

;;; -------------------------------------------------------

(defun parse-let-statement (parser)
  "Parses a \"let\" statement employing the PARSER's services and
   returns an ``Assignment-Node'' representation thereof."
  (declare (type Statement-Parser parser))
  (eat-token parser :let)
  (the Assignment-Node
    (make-assignment-node
      (token-value
        (eat-token parser :variable))
      (progn
        (eat-token parser :equal-to)
        (apply-expression-parser parser 0)))))

;;; -------------------------------------------------------

(defun parse-do-statement (parser)
  "Parses a \"do ... if\" or \"do ... while\" statement employing the
   PARSER's services and returns a ``Do-if-Node'' or ``Do-While-Node''
   representation thereof."
  (declare (type Statement-Parser parser))
  (eat-token parser :do)
  (let ((statements (parse-statement-block parser)))
    (declare (type Block-Node statements))
    (the (or Do-If-Node Do-While-Node)
      (case (current-token-type parser)
        (:if
          (eat-token parser :if)
          (make-do-if-node
            (apply-expression-parser parser 0)
            statements))
        (:while
          (eat-token parser :while)
          (make-do-while-node
            (apply-expression-parser parser 0)
            statements))
        (otherwise
          (error "Expected \"if\" or \"while\" in the \"do\" ~
                  statement, but encountered the token ~s."
            (get-current-token parser)))))))

;;; -------------------------------------------------------

(defun parse-print-statement (parser)
  "Parses a \"PRINT\" statement employing the PARSER's services and
   returns a ``Print-Node'' representation thereof."
  (declare (type Statement-Parser parser))
  (eat-token parser :print)
  (eat-token parser :left-brace)
  (the Print-Node
    (prog1
      (make-print-node
        (parse-expresson-list parser))
      (eat-token parser :right-brace))))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses a statement utilizing the PARSER, if possible, and returns an
  ``AST-Node'' upon success; otherwise responds with the ``NIL'' value."
  (declare (type Statement-Parser parser))
  (the (or null AST-Node)
    (case (current-token-type parser)
      (:let
        (parse-let-statement parser))
      (:do
        (parse-do-statement parser))
      (:print
        (parse-print-statement parser))
      (otherwise
        (when (expression-follows-p parser)
          (apply-expression-parser parser 0))))))

;;; -------------------------------------------------------

(defun parse-statement-block (parser)
  "Parses an ordered sequence of zero or more Do-if statements and
   returns a ``Block-Node'' encapsulation thereof."
  (declare (type Statement-Parser parser))
  (the Block-Node
    (make-block-node
      (loop
        for current-statement
          of-type (or null AST-Node)
          =       (parse-statement parser)
        while current-statement
          collect current-statement))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a Do-if program utilizing the PARSER's services and returns a
   ``Program-Node'' representation of the assembled statements."
  (declare (type Statement-Parser parser))
  (the Program-Node
    (make-program-node
      (prog1
        (parse-statement-block parser)
        (eat-token parser :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Do-if object classes.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Do-If-Object)
  "Th ``Do-If-Object'' interface establishes a common foundry for all
   classes in a pursuit to represent a Do-if data type.")

;;; -------------------------------------------------------

(defstruct (Do-If-Boolean
  (:include     Do-If-Object)
  (:constructor make-do-if-boolean (value)))
  "The ``Do-If-Boolean'' class furnishes an encapsulation of a Boolean
   truth value in a form conable with a Do-if program."
  (value (error "Missing Boolean truth value.")
         :type      boolean
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Do-If-Integer
  (:include     Do-If-Object)
  (:constructor make-do-if-integer (value)))
  "The ``Do-If-Integer'' class furnishes an encapsulation of a signed
   or unsigned integer datum in a form conable with a Do-if program."
  (value (error "Missing integer value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Do-If-String
  (:include     Do-If-Object)
  (:constructor make-do-if-string (value)))
  "The ``Do-If-String'' class furnishes an encapsulation of a string in
   a form conable with a Do-if program."
  (value (error "Missing string value.")
         :type      string
         :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global ``Do-If-Object'' constants.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Do-If-Boolean +DO-IF-BOOLEAN-FALSE+))
(declaim (type Do-If-Boolean +DO-IF-BOOLEAN-TRUE+))

;;; -------------------------------------------------------

(defparameter +DO-IF-BOOLEAN-FALSE+
  (make-do-if-boolean NIL)
  "A ``Do-If-Boolean'' object representation of the Boolean \"false\"
   value.")

(defparameter +DO-IF-BOOLEAN-TRUE+
  (make-do-if-boolean T)
  "A ``Do-If-Boolean'' object representation of the Boolean \"true\"
   value.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Do-if object operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-do-if-object-value (object)
  (:documentation
    "Returns the value maintained by the Do-if OBJECT.")
  
  (:method ((truth-value Do-If-Boolean))
    (declare (type Do-If-Boolean truth-value))
    (the boolean
      (do-if-boolean-value truth-value)))
  
  (:method ((number Do-If-Integer))
    (declare (type Do-If-Integer number))
    (the integer
      (do-if-integer-value number)))
  
  (:method ((string Do-If-String))
    (declare (type Do-If-String string))
    (the string
      (do-if-string-value string))))

;;; -------------------------------------------------------

(defgeneric do-if-object-is-true-p (object)
  (:documentation
    "Determines whether the Do-if OBJECT is tantamount to a Boolean
     truth value of \"true\", returning on confirmation a ``boolean''
     value of ``T'', otherwise ``NIL''.")
  
  (:method ((truth-value Do-If-Boolean))
    (declare (type Do-If-Boolean truth-value))
    (the boolean
      (do-if-boolean-value truth-value)))
  
  (:method ((number Do-If-Integer))
    (declare (type Do-If-Integer number))
    (declare (ignore             number))
    (the boolean T))
  
  (:method ((string Do-If-String))
    (declare (type Do-If-String string))
    (declare (ignore            string))
    (the boolean T)))

;;; -------------------------------------------------------

(defgeneric write-do-if-object-to (object destination)
  (:documentation
    "Writes an aesthetically pleasing representation of the OBJECT to
     the DESTINATION and returns no value.")
  
  (:method ((object      Do-If-Boolean)
            (destination T))
    (declare (type Do-If-Boolean object))
    (declare (type destination   destination))
    (format destination "~:[false~;true~]"
      (do-if-boolean-value object))
    (values))
  
  (:method ((object      Do-If-Integer)
            (destination T))
    (declare (type Do-If-Integer object))
    (declare (type destination   destination))
    (format destination "~d"
      (do-if-integer-value object))
    (values))
  
  (:method ((object      Do-If-String)
            (destination T))
    (declare (type Do-If-String object))
    (declare (type destination  destination))
    (format destination "~a"
      (do-if-string-value object))
    (values)))

;;; -------------------------------------------------------

(defun get-do-if-boolean-value-of (object)
  "Returns a ``Do-If-Boolean'' tantaount to the OBJECT when construed
   as a \"generalized boolean\", returning an affirmative response for
   a non-``NIL'' input, otherwise produces ``NIL''."
  (declare (type T object))
  (the Do-If-Boolean
    (or (and object +DO-IF-BOOLEAN-TRUE+)
        +DO-IF-BOOLEAN-FALSE+)))

;;; -------------------------------------------------------

(defun apply-to-do-if-objects (callback first-object second-object)
  "Invokes the CALLBACK function on the FIRST-OBJECT and the
   SECOND-OBJECT and returns the result obtained whence."
  (declare (type (function (Do-If-Object Do-If-Object) *) callback))
  (declare (type Do-If-Object                             first-object))
  (declare (type Do-If-Object                            second-object))
  (the T
    (funcall callback first-object second-object)))

;;; -------------------------------------------------------

(defun apply-to-do-if-object-values (callback
                                     first-object
                                     second-object)
  "Invokes the CALLBACK function on the FIRST-OBJECT and the
   SECOND-OBJECT and returns the result obtained whence."
  (declare (type (function (Do-If-Object Do-If-Object) *) callback))
  (declare (type Do-If-Object                             first-object))
  (declare (type Do-If-Object                            second-object))
  (the T
    (funcall callback
      (get-do-if-object-value first-object)
      (get-do-if-object-value second-object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-do-if-objects (first-object second-object)
  "Concatenates the ``Do-If-Object'' instances FIRST-OBJECT and
   SECOND-OBJECT into a unified ``Do-If-String'', which is subsequently
   returned."
  (declare (type Do-If-Object first-object))
  (declare (type Do-If-Object second-object))
  (the Do-If-String
    (make-do-if-string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (write-do-if-object-to first-object  content)
        (write-do-if-object-to second-object content)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-unary-operator (operator operand)
  (:documentation
    "Applies the unary OPERATOR to the OPERAND and returns a result
     covenable with this combination."))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :plus))
                                 (operand  Do-If-integer))
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type Do-If-integer  operand))
  (the Do-If-Integer
    (make-do-if-integer
      (do-if-integer-value operand))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :minus))
                                 (operand  Do-If-Integer))
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type Do-If-integer  operand))
  (the Do-If-Integer
    (make-do-if-integer
      (- (do-if-integer-value operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and the
     RIGHT-OPERAND in this exact order and returns a result covenable
     with this combination."))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-Integer
    (make-do-if-integer
      (apply-to-do-if-object-values #'+ left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-String))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-String    right-operand))
  (the Do-If-String
    (concatenate-do-if-objects left-operand right-operand)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  Do-If-String)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-String    left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-String
    (concatenate-do-if-objects left-operand right-operand)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  Do-If-String)
                                  (right-operand Do-If-String))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-String    left-operand))
  (declare (type Do-If-String    right-operand))
  (the Do-If-String
    (concatenate-do-if-objects left-operand right-operand)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :minus))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-Integer
    (make-do-if-integer
      (apply-to-do-if-object-values #'- left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :times))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-Integer
    (make-do-if-integer
      (apply-to-do-if-object-values #'* left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :divided))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-Integer
    (make-do-if-integer
      (nth-value 0
        (apply-to-do-if-object-values
          #'round left-operand right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-than))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-Boolean
    (get-do-if-boolean-value-of
      (apply-to-do-if-object-values #'< left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-than))
                                  (left-operand  Do-If-Object)
                                  (right-operand Do-If-Object))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Object    left-operand))
  (declare (ignore               left-operand))
  (declare (type Do-If-Object    right-operand))
  (declare (ignore               right-operand))
  (the Do-If-Boolean +DO-IF-BOOLEAN-FALSE+))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-than))
                                  (left-operand  Do-If-String)
                                  (right-operand Do-If-String))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-String    left-operand))
  (declare (type Do-If-String    right-operand))
  (the Do-If-Boolean
    (get-do-if-boolean-value-of
      (apply-to-do-if-object-values
        #'string< left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal-to))
                                  (left-operand  Do-If-Boolean)
                                  (right-operand Do-If-Boolean))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Boolean   left-operand))
  (declare (type Do-If-Boolean   right-operand))
  (the Do-If-boolean
    (get-do-if-boolean-value-of
      (apply-to-do-if-object-values #'eq left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal-to))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-boolean
    (get-do-if-boolean-value-of
      (apply-to-do-if-object-values #'= left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal-to))
                                  (left-operand  Do-If-Object)
                                  (right-operand Do-If-Object))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Object    left-operand))
  (declare (ignore               left-operand))
  (declare (type Do-If-Object    right-operand))
  (declare (ignore               right-operand))
  (the Do-If-Boolean +DO-IF-BOOLEAN-FALSE+))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal-to))
                                  (left-operand  Do-If-String)
                                  (right-operand Do-If-String))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-String    left-operand))
  (declare (type Do-If-String    right-operand))
  (the Do-If-Boolean
    (get-do-if-boolean-value-of
      (apply-to-do-if-object-values
        #'string= left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :greater-than))
                                  (left-operand  Do-If-Integer)
                                  (right-operand Do-If-Integer))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Integer   left-operand))
  (declare (type Do-If-Integer   right-operand))
  (the Do-If-Boolean
    (get-do-if-boolean-value-of
      (apply-to-do-if-object-values #'> left-operand right-operand))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :greater-than))
                                  (left-operand  Do-If-Object)
                                  (right-operand Do-If-Object))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-Object    left-operand))
  (declare (ignore               left-operand))
  (declare (type Do-If-Object    right-operand))
  (declare (ignore               right-operand))
  (the Do-If-Boolean +DO-IF-BOOLEAN-FALSE+))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :greater-than))
                                  (left-operand  Do-If-String)
                                  (right-operand Do-If-String))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type Do-If-String    left-operand))
  (declare (type Do-If-String    right-operand))
  (the boolean
    (get-boolean-value-of
      (apply-to-do-if-object-values
        #'string> left-operand right-operand))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Reference".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Reference
  (:constructor make-reference (name)))
  "The ``Reference'' class serves as an envelope wrapping a variable
   name following a discrimating aspect from a string literal."
  (name (error "Missing reference name.")
        :type      string
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree (AST) for ~
                           the interpreter.")
    :reader        interpreter-tree
    :type          Program-Node
    :documentation "The parsed Do-if program in a tree form.")
   (variables
    :initform      (make-hash-table :test #'equal)
    :reader        interpreter-variables
    :type          (hash-table-of string  Do-If-Object)
    :documentation "Assigns the declared variable names encapsulations
                    of integer numbers or strings."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of administering
     actual efficacy to a Do-if program communicated in the guise of an
     abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a fresh ``Interpreter'' dedicated to evaluation
   of a Do-if program's abstract syntax TREE (AST) representation."
  (declare (type Program-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun variable-is-defined-p (interpreter name)
  "Determines whether a variable amenable to the NAME exists in the
   INTERPRETER, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash name
          (interpreter-variables interpreter))))))

;;; -------------------------------------------------------

(defun get-variable-value (interpreter name)
  "Returns the value maintained in the variable amenable to the NAME
   in the INTERPRETER, or signals an error of an unspecified type upon
   its disrespondency."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the Do-If-Object
    (if (variable-is-defined-p interpreter name)
      (nth-value 0
        (gethash name
          (interpreter-variables interpreter)))
      (error "The variable ~s is not yet defined." name))))

;;; -------------------------------------------------------

(defun set-variable-value (interpreter name new-value)
  "If a variable amenable to the NAME exists in the INTERPRETER,
   supersedes its content with the NEW-VALUE, otherwise creates a new
   entry conjoining the NAME and NEW-VALUE, in any case returning no
   value."
  (declare (type Interpreter  interpreter))
  (declare (type string       name))
  (declare (type Do-If-Object new-value))
  (setf (gethash name (interpreter-variables interpreter) 0)
        new-value)
  (values))

;;; -------------------------------------------------------

(defgeneric resolve-value (interpreter object)
  (:documentation
    "Returns the OBJECT's value in the INTERPRETER's context and returns
     the ``Do-If-Object'' instance connable for this combination.")
  
  (:method ((interpreter Interpreter)
            (truth-value Do-If-Boolean))
    (declare (type Interpreter   interpreter))
    (declare (ignore             interpreter))
    (declare (type Do-If-Boolean truth-value))
    (the Do-If-Boolean truth-value))
  
  (:method ((interpreter Interpreter)
            (number      Do-If-Integer))
    (declare (type Interpreter   interpreter))
    (declare (ignore             interpreter))
    (declare (type Do-If-Integer number))
    (the Do-If-Integer number))
  
  (:method ((interpreter Interpreter)
            (reference   Reference))
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (the Do-If-Object
      (resolve-value interpreter
        (get-variable-value interpreter
          (reference-name reference)))))
  
  (:method ((interpreter Interpreter)
            (string      Do-If-String))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type Do-If-String string))
    (the Do-If-String string)))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Evaluates the abstract syntax tree (AST) NODE in the INTERPRETER's
     context and returns a result covenable for this combination."))

;;; -------------------------------------------------------

(defun resolve-node-value (interpreter node)
  "Visits the NODE in the INTERPRETER's context, resolves its value,
   and returns thilk."
  (declare (type Interpreter interpreter))
  (declare (type AST-Node    node))
  (the Do-If-Object
    (resolve-value interpreter
      (visit-node interpreter node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (visit-node interpreter
    (program-node-statements node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Assignment-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Assignment-Node node))
  (set-variable-value interpreter
    (assignment-node-target node)
    (resolve-node-value interpreter
      (assignment-node-value node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Binary-Operation-Node))
  (declare (type Interpreter           interpreter))
  (declare (type Binary-Operation-Node node))
  (the Do-If-Object
    (apply-binary-operator
      (binary-operation-node-operator node)
      (resolve-node-value interpreter
        (binary-operation-node-left-operand node))
      (resolve-node-value interpreter
        (binary-operation-node-right-operand node)))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Block-Node))
  (declare (type Interpreter interpreter))
  (declare (type Block-Node  node))
  (dolist (current-statement (block-node-statements node))
    (declare (type AST-Node current-statement))
    (visit-node interpreter current-statement))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Do-If-Node))
  (declare (type Interpreter interpreter))
  (declare (type Do-If-Node  node))
  (when (do-if-object-is-true-p
          (resolve-node-value interpreter
            (do-if-node-antecedent node)))
    (visit-node interpreter
      (do-if-node-statements node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Do-While-Node))
  (declare (type Interpreter   interpreter))
  (declare (type Do-While-Node node))
  (loop
    while
      (do-if-object-is-true-p
        (resolve-node-value interpreter
          (do-while-node-antecedent node)))
    do
      (visit-node interpreter
        (do-while-node-statements node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Input-Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Input-Node  node))
  (declare (ignore           node))
  (finish-output)
  (the Do-If-Integer
    (make-do-if-integer
      (prog1
        (parse-integer
          (read-line NIL NIL "-1"))
        (clear-input)))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Integer-Literal-Node))
  (declare (type Interpreter          interpreter))
  (declare (ignore                    interpreter))
  (declare (type Integer-Literal-Node node))
  (the Do-If-Integer
    (make-do-if-integer
      (integer-literal-node-value node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Print-Node)) 
  (declare (type Interpreter interpreter))
  (declare (type Print-Node  node))
  (dolist (current-argument (print-node-arguments node))
    (declare (type Expression-Node current-argument))
    (write-do-if-object-to
      (resolve-node-value interpreter current-argument)
      T))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        String-Literal-Node))
  (declare (type Interpreter         interpreter))
  (declare (type String-Literal-Node node))
  (the Do-If-String
    (make-do-if-string
      (string-literal-node-value node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Unary-Operation-Node))
  (declare (type Interpreter          interpreter))
  (declare (type Unary-Operation-Node node))
  (the Do-If-Object
    (apply-unary-operator
      (unary-operation-node-operator node)
      (resolve-node-value interpreter
        (unary-operation-node-operand node)))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Variable-Node))
  (declare (type Interpreter   interpreter))
  (declare (type Variable-Node node))
  (the Reference
    (make-reference
      (variable-node-name node))))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Do-if program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Do-if (code)
  "Interprets the piece of Do-if source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program
        (make-statement-parser
          (register-standard-parselets
            (make-pratt-parser))
          (make-token-stream
            (make-lexer code))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!", succeeded by a linebreak.
(interpret-Do-if "PRINT {'Hello, World!\\n'}")

;;; -------------------------------------------------------

;; Print "Hello world" several times with a counter increasing from
;; inclusive zero (0) to nine (9).
(interpret-Do-if
  "let $X = 0
   do
     PRINT {'Hello world ', $X, '!\\n'}
     let $X = $X + 1
   while $X < 10")

;;; -------------------------------------------------------

;; Compute factorial of user input.
(interpret-Do-if
  "let $N = INPN {}
   let $X = $N
   let $R = 1
   do
     let $R = $R * $X
     let $X = $X - 1
   while $X > 1
   PRINT {$N, '! = ', $R}")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-Do-if
  "PRINT {INPN {}}")

;;; -------------------------------------------------------

;; Repeating numeric cat program which terminates on a user input of
;; zero (0).
(interpret-Do-if
  "let $userInput = 1
   do
     PRINT {'>> '}
     let $userInput = INPN {}
     PRINT {$userInput, '\\n'}
   while $userInput > 0")

;;; -------------------------------------------------------

;; Demonstrate the printing of multifarious expressions.
(interpret-Do-if
  "let $X = -6
   PRINT {'Hello', INPN {}, $X}")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Do-if
  "PRINT {'Please input 0 or 1: '}
   let $input = INPN {}
   PRINT {$input}
   do
     PRINT {$input}
   while $input = 1")

;;; -------------------------------------------------------

;; Looping counter which counts up from one (1) to ten (10).
(interpret-Do-if
  "
  let $rowNumber     = 0
  let $columnCounter = 0
  
  do
    let $columnCounter = 1
    do
      PRINT {'*'}
      let $columnCounter = $columnCounter + 1
    while $columnCounter < $rowNumber + 1
    
    PRINT {'\\n'}
    
    let $rowNumber = $rowNumber + 1
  while $rowNumber < 11
  ")
