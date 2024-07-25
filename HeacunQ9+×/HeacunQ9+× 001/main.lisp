;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "HeacunQ9+×", invented by the Esolang user "AmNow" and
;; presented on May 1st, 2022, this, being a derivative of "HQ9+" by
;; Cliff L. Biffle, augments the ludibund entheus by certain behoovable
;; facilities, siccan dation amplects the input and output conduits,
;; the capacitation for arithmetic expressions, and the definition of
;; functions as latreutic cohorts to the aefauld integer-valued
;; accumulator.
;; 
;; 
;; Concept
;; =======
;; HeacunQ9+×'s fundament is edified a twifaced ideation, the first
;; being the perquisition and modulation of a singular integer-valued
;; register, the "accumulator", the complementing moeity's entelechy
;; such as to furnish an arbitrarily chosen repertoire of output
;; messages, in its perimeter, among others, "Hello, World!" and a
;; quine.
;; 
;; == "HEACUNQ9+×": THE STEVENING BEWRAYS THE SYNTAX ==
;; The HeacunQ9+× language's agnomination constitutes an establishment
;; proceeding from its decimal operative identifiers.
;; 
;; == IDENTIFIERS: CASE-INSENSITIVE SINGLE SYMBOLS ==
;; Each operation identifier's employment is defined in a mode
;; neglecting the discrepancy betwixt minuscular and majuscular forms,
;; and registers an aefauld symbol for an instruction's recognition.
;; 
;; == PROGRAM MEMORY: A SINGLE INTEGER-VALUED REGISTER ==
;; The entirety of the language's program memory is realized in a
;; scalar register norned the "accumulator", its capacity being meted
;; in a signed integer whose bournes wist of no cumbrance along any of
;; the laterality's twain.
;; 
;; 
;; Instructions
;; ============
;; A decimal account of avails offers itself to the programmer's
;; operative optations, in its compass embraced such to manipulate the
;; accumulator by basic arithmetic means, input and output in form of
;; numeric or character-based communication, the printing of certain
;; fixated texts, as well as the definition and either conditional or
;; unconditional execution of functions.
;; 
;; The HeacunQ9+× language's mathematics entails a small set of unary
;; and binary operations, their coefficiency together with integral
;; literal numbers capacitates the formation of expressions in the
;; requisite localities.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be a cursory mete of gnarity's
;; impartation anent the available instructions.
;; 
;; Its derivation from the HQ9+ introduces the stock-father's diorism
;; of case-insensitivity in the context of any content.
;; 
;; Please heed the designation of succedaneous segments by adminiculum
;; of a catena of asterisks ("*"), such are intended for their
;; supersession in the ultimate HeacunQ9+× program by valid code
;; fragments.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   +              | Increments the accumulator by one (1).
;;   ..................................................................
;;   C expression C | Evaluates the arithmetic {expression} and stores
;;     **********   | the result in the accumulator.
;;                  |--------------------------------------------------
;;                  | The {expression} must be a mathematical formula
;;                  | obeying the imposed stipulations, which please
;;                  | consult alow under the section "EXPRESSIONS".
;;   ..................................................................
;;   U              | If the accumulator state equals -1, queries the
;;                  | standard input for an integer number and stores
;;                  | the same in the accumulator; otherwise, for a
;;                  | value not equal to -1, prints the accumulator
;;                  | state in its ipsissima verba form to the standard
;;                  | output.
;;   ..................................................................
;;   N              | If the accumulator state equals -1, queries the
;;                  | standard input for a character and stores its
;;                  | ASCII code in the accumulator; otherwise, for a
;;                  | value not equal to -1, prints the character whose
;;                  | ASCII code matches the standard output.
;;   ..................................................................
;;   H              | Prints the message "Hello, World!" to the
;;                  | standard output.
;;   ..................................................................
;;   9              | Prints the lyrics of the song
;;                  | "99 Bottles of Beer" to the standard output.
;;   ..................................................................
;;   Q              | Prints the program's source code to the standard
;;                  | output.
;;   ..................................................................
;;   A statements A | Defines a function whose body is composed of
;;     **********   | the {statements} and sets the same as the global
;;                  | function, superseding any extant definition.
;;                  |--------------------------------------------------
;;                  | {statements} must be a sequence of zero or more
;;                  | HeacunQ9+× instructions.
;;   ..................................................................
;;   E              | Executes the most recently defined function, or,
;;                  | if none such exists, accompasses no effect.
;;   ..................................................................
;;   ×              | If the accumulator state does not equal zero (0),
;;                  | otherwise executes the most recently defined
;;                  | function, or, if none such exists, accompasses no
;;                  | effect.
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS ==
;; The language's expression department is exhausted already by signed
;; integer literals and two sets of arithmetic operations.
;; 
;; This avail on operations bifurcates in a triad of unary specimens and
;; five binary members, the former expects an aefauld operand, while the
;; latter imposes a twissel's participation.
;; 
;; The unary operations class intrines the following membership, with
;; placeholder sections being amplected by braces ("{...}"):
;; 
;;   ------------------------------------------------------------------
;;   Unary operator   | Effect
;;   -----------------+------------------------------------------------
;;   +{expression}    | Returns the {expression} unaltered.
;;   ..................................................................
;;   -{expression}    | Negates the {expression}'s sign.
;;   ..................................................................
;;   ( {expression} ) | Groups an {expression}, this ensconcement being
;;                    | an obviation of external influences, and
;;                    | returns the evaluation result.
;;   ------------------------------------------------------------------
;; 
;; A set of quintuple cardinality governs the binary operations, iterum
;; administering a jumelle of braces, "{" and "}", for the purpose of
;; succedaneous parcels' limning:
;; 
;;   ------------------------------------------------------------------
;;   Binary operator  | Effect
;;   -----------------+------------------------------------------------
;;   {left} + {right} | Adds the {right} expression to the {left} one.
;;   ..................................................................
;;   {left} - {right} | Subtracts the {right} expression from the
;;                    | {left} one.
;;   ..................................................................
;;   {left} * {right} | Multiplies the {left} expression by the {right}
;;                    | one.
;;   ..................................................................
;;   {left} / {right} | Divides the {left} expression by the {right}
;;                    | one and rouds the quotient to the nearest
;;                    | integral value.
;;   ..................................................................
;;   {left} % {right} | Divides the {left} expression by the {right}
;;                    | one and returns the division's remainder.
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
;;   (1) A lexer generates from the Var=Bar program string a sequence of
;;       tokens.
;;   (2) The parser queries these tokens and assembles an abstract
;;       syntax tree (AST), the nodes of which represent the language
;;       facilities.
;;   (3) The interpreter traverses the AST and embues it with effect.
;; 
;; == EXPRESSIONS ARE ASSEMBLED VIA PRATT PARSING ==
;; The parser combines aspects of recursive descent and Pratt's
;; solution, with the former apprehending the general process, aided by
;; the latter for the assemblage of expressions. The Pratt component's
;; conventions and notions are derived from Denis
;; Lantsman [lantsman2018prattparsers].
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
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
;;   +        | 130           | left-to-right | 
;;   ..................................................................
;;   -        | 130           | left-to-right | 
;;   ..................................................................
;;   *        | 140           | left-to-right | 
;;   ..................................................................
;;   /        | 140           | left-to-right | 
;;   ..................................................................
;;   %        | 140           | left-to-right | 
;;   ..................................................................
;;   - (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   + (sign) | 170           | right-to-left | 
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-07-20
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
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
;;   [esolang2023HeacunQ9+×]
;;   The Esolang contributors, "HeacunQ9+×", October 10th, 2023
;;   URL: "https://esolangs.org/wiki/HeacunQ9%2B%C3%97"
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
;; -- Implementation of operations on types.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type founded upon the ``deftype'' infrastructure
   in conjunction with the ``satisfies'' type specifier, the new
   species' agnomination being obtained from the TYPE-NAME, its formal
   parameters an ipsissima verba appropriation of the LAMBDA-LIST, while
   the subject to the docimasy involves via the CANDIDATE-VARIABLE name,
   evaluating the BODY forms, the desinent form of which in its primary
   value is intended to communicate the convenableness judgment, a
   \"generalized boolean\" value of \"true\" confirming its eligiblity,
   while a \"false\" refutes the same.
   ---
   The first BODY form, if resolving to a string object, will be
   administered the construe as a documentation string to the derived
   type and will be reappropriated for the same purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
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

(define-bespoke-type hash-table-of (candidate
                                    &optional (key-type   T)
                                              (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key among which complies with the KEY-TYPE and
   answers to a value of the VALUE-TYPE, for both holding the default
   of the comprehensive ``T''."
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

(define-bespoke-type property-list-of (candidate &optional (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   compact of zero or more entries, each indicator, or key. of which
   constitutes a keyword symbol and answers to a value of the
   VALUE-TYPE, for the latter holds the default of the comprehensive
   ``T''."
  (and
    (listp candidate)
    (evenp (length (the list candidate)))
    (loop
      for (indicator value)
        of-type (T T)
        on      (the list candidate)
        by      #'cddr
      always
        (and (keywordp indicator)
             (typep    value value-type)))))

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-map'' type defines a collection of attributes
   consigned to an ``AST-Node'''s castaldy, mapping the keyword-valued
   attribute name to an object of any type, being realized as a property
   list, or plist."
  '(property-list-of T))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized variations on
   associativity policies betwixt two operands of equipollence in
   binding power."
  '(member :left-to-right :right-to-left :none))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (integer Token-Stream) AST-Node)
                parse-expression))
(declaim (ftype (function (Token-Stream) AST-Node) parse-function-body))
(declaim (ftype (function (Token) boolean) nud-token-p))
(declaim (ftype (function (Token) boolean) led-token-p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a veridical Boolean representation of the OBJECT in its
   construe as a \"generalized boolean\", returning for a non-``NIL''
   input a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines the CANDIDATE's membership among the whitespace entities,
   a diorism whose amplectation exhausts the space, horizontal tab, as
   well as newline characters, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline)))))

;;; -------------------------------------------------------

(defun significant-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a symbol whose
   occurrence is apportioned an effective role in a HeacunQ9+× program,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (find candidate "HEACUNQ+×-*/%()" :test #'char-equal)
          (digit-char-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class' dever is realized in its encapsulation of a
   significant object extracted during the lexical analyzation stage
   from a piee of HeacunQ9+× source code."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-of-type-p (token expected-type)
  "Determines whether the TOKEN complies with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexical analyzer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class establishes a lexical analyzer, or scanner, the
   competence posseded by whom resolves to the detection and extraction
   of significant objects from a piece of HeacunQ9+× source code
   consigned to its perquisition, pursuing their subsequent provision
   in the form of token instances."
  (source    (error "Missing lexer source.")
             :type      string
             :read-only T)
  (position  (error "Missing lexer position.")
             :type      fixnum
             :read-only NIL)
  (character (error "Missing lexer character.")
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defun advance-to-next-character (lexer)
  "Advances to the next character in the LEXER's source, if possible,
   and returns no value."
  (declare (type Lexer lexer))
  (the (or null character)
    (prog1
      (lexer-character lexer)
      (setf (lexer-character lexer)
        (when (array-in-bounds-p (lexer-source lexer)
                (1+ (lexer-position lexer)))
          (char (lexer-source lexer)
            (incf (lexer-position lexer))))))))

;;; -------------------------------------------------------

(defun move-lexer-to (lexer new-position)
  "Relocates the LEXER's position cursor to the NEW-POSITION and returns
   no value."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (setf (lexer-position lexer) new-position)
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (lexer-source lexer)
            (lexer-position lexer))
      (char (lexer-source lexer)
        (lexer-position lexer))))
  (values))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Proceeding from the current position into the LEXER's source,
   consumes the present character, produces a token which combines the
   specified TOKEN-TYPE with this character into a compound, and
   returns the same."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (make-token token-type
      (prog1
        (lexer-character           lexer)
        (advance-to-next-character lexer)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (loop
    while (and (lexer-character lexer)
               (whitespace-character-p
                 (lexer-character lexer)))
    do    (advance-to-next-character lexer))
  (values))

;;; -------------------------------------------------------

(defun skip-negligible-content (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more non-operative characters and returns no
   value."
  (declare (type Lexer lexer))
  (loop
    while (and (lexer-character lexer)
               (not (significant-character-p
                      (lexer-character lexer))))
    do    (advance-to-next-character lexer))
  (values))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        (make-eof-token))
      ((whitespace-character-p (lexer-character lexer))
        (skip-whitespaces lexer)
        (get-next-token   lexer))
      ((digit-char-p (lexer-character lexer))
        (prog1
          (make-token :digit
            (digit-char-p
              (lexer-character lexer)))
          (advance-to-next-character lexer)))
      ((char-equal (lexer-character lexer) #\A)
        (read-symbol lexer :a))
      ((char-equal (lexer-character lexer) #\C)
        (read-symbol lexer :c))
      ((char-equal (lexer-character lexer) #\E)
        (read-symbol lexer :e))
      ((char-equal (lexer-character lexer) #\H)
        (read-symbol lexer :h))
      ((char-equal (lexer-character lexer) #\N)
        (read-symbol lexer :n))
      ((char-equal (lexer-character lexer) #\Q)
        (read-symbol lexer :q))
      ((char-equal (lexer-character lexer) #\U)
        (read-symbol lexer :u))
      ((char= (lexer-character lexer) #\+)
        (read-symbol lexer :plus))
      ((char= (lexer-character lexer) #\-)
        (read-symbol lexer :minus))
      ((char= (lexer-character lexer) #\*)
        (read-symbol lexer :times))
      ((char= (lexer-character lexer) #\/)
        (read-symbol lexer :divide))
      ((char= (lexer-character lexer) #\%)
        (read-symbol lexer :remainder))
      ((char= (lexer-character lexer) #\×)
        (read-symbol lexer :×))
      ((char= (lexer-character lexer) #\()
        (read-symbol lexer :left-parenthesis))
      ((char= (lexer-character lexer) #\))
        (read-symbol lexer :right-parenthesis))
      (T
        (skip-negligible-content lexer)
        (get-next-token          lexer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token stream.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token-Stream
  (:constructor make-token-stream
    (lexer
     &aux (next-token (get-next-token lexer)))))
  "The ``Token-Stream'' class serves in the furnishment of an abstract,
   and yet concomitantly more potent tier superimposed upon a lexer,
   and nuncupated to the token provision's duty."
  (lexer      (error "Missing lexer.")
              :type      Lexer
              :read-only T)
  (next-token (error "Missing next token.")
              :type      Token
              :read-only NIL))

;;; -------------------------------------------------------

(defun peek-token (tokens)
  "Returns without removing the next token from the TOKENS stream."
  (declare (type Token-Stream tokens))
  (the Token
    (token-stream-next-token tokens)))

;;; -------------------------------------------------------

(defun consume-token (tokens)
  "Removes and returns the next token from the TOKENS stream."
  (declare (type Token-Stream tokens))
  (the Token
    (prog1
      (token-stream-next-token tokens)
      (setf (token-stream-next-token tokens)
        (get-next-token
          (token-stream-lexer tokens))))))

;;; -------------------------------------------------------

(defun expect-token (tokens expected-token-type)
  "Determines whether the next token in the stream of TOKENS conforms to
   the EXPECTED-TOKEN-TYPE, on confirmation consuming and returning the
   probed object, otherwise signaling an error of an unspecified type."
  (declare (type Token-Stream tokens))
  (declare (type keyword      expected-token-type))
  (let ((next-token (peek-token tokens)))
    (declare (type Token next-token))
    (the Token
      (or (and (token-of-type-p next-token expected-token-type)
               (consume-token tokens))
          (error "Expected a token of the type ~a, but encountered ~a."
            expected-token-type next-token)))))

;;; -------------------------------------------------------

(defun expression-token-p (token)
  "Determines whether the TOKEN represents a constituent for an
   expression, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (or (nud-token-p token)
          (led-token-p token)))))

;;; -------------------------------------------------------

(defun skip-to-expression-token (tokens)
  "Proceeding from its current token, skips the content in the stream
   of TOKENS until either an expression token, the expression terminator
   \"c\" or \"C\", or the end-of-file sentinel (``:eof'') is detected,
   and returns the TOKENS stream."
  (declare (type Token-Stream tokens))
  (loop
    for next-token of-type Token = (peek-token tokens)
    until (or (expression-token-p next-token)
              (token-of-type-p    next-token :c)
              (token-of-type-p    next-token :eof))
      do (consume-token tokens))
  (the Token-Stream tokens))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) node.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AST-Node
  (:constructor make-ast-node (type &rest attributes)))
  "The ``AST-Node'' class implements an abstract syntax tree (AST) node
   pursuing a generic approach, the type of which determines the node's
   species, while a table of attributes accoutres the subtree's
   diorism."
  (type       (error "Missing AST node type.")
              :type      keyword
              :read-only T)
  (attributes NIL
              :type      attribute-map
              :read-only T))

;;; -------------------------------------------------------

(defun get-ast-node-attribute (node attribute-name)
  "Returns the attribute value associated with the ATTRIBUTE-NAME in the
   abstract syntax tree (AST) NODE, or responds with ``NIL'' upon its
   disrespondency."
  (declare (type AST-Node node))
  (declare (type keyword  attribute-name))
  (the T
    (getf (ast-node-attributes node) attribute-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of precedence.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence
    (binding-power associativity
     &aux (effective-binding-power
            (case associativity
              ((:none :left-to-right)
                binding-power)
              (:right-to-left
                (1- binding-power))
              (otherwise
                (error "Invalid associativity: ~a." associativity)))))))
  "The ``Precedence'' class serves in the encapsulation of precedence
   information, composed of the twissel enumerating the numeric binding
   power and the associativity."
  (binding-power           (error "Missing binding power.")
                           :type      integer
                           :read-only T)
  (associativity           (error "Missing associativity.")
                           :type      associativity
                           :read-only T)
  (effective-binding-power (error "Missing effective binding power.")
                           :type      integer
                           :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of parselet interface.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ()
  (:documentation
    "The ``Parselet'' interface furnishes the top echolon of the
     hierarchy whose compass engulfs all variations on nud and led
     parselets."))

;;; -------------------------------------------------------

(defgeneric get-parselet-binding-power (parselet)
  (:documentation
    "Returns the PARSELET's effective binding power as a signed integer
     number, or signals an error of an unspecified type upon the
     absence of such property from the indagated unit."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of nud parselet interface.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nud-Parselet (Parselet)
  ()
  (:documentation
    "The ``Nud-Parselet'' interface establishes the substratum for all
     classes pursing the accoutrement of nud, or initial, tokens'
     parsing."))

;;; -------------------------------------------------------

(defgeneric apply-nud-parselet (parselet nud-token tokens)
  (:documentation
    "Parses the NUD-TOKEN in the PARSELET's context, employing the
     stream of TOKENS for the contingency of further tokens' obtention,
     and returns an ``AST-Node'' representation of the parsed
     expression."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of number parselet.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Number-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Number-Parselet'' class implements a nud parselet nuncupated
     to the evaluation of an integral literal."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Number-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Number-Parselet parselet))
  (declare (ignore               parselet))
  (declare (type Token           nud-token))
  (declare (type Token-Stream    tokens))
  (the AST-Node
    (make-ast-node :number :value
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (format digits "~d"
            (token-value nud-token))
          (loop while (token-of-type-p (peek-token tokens) :digit) do
            (format digits "~d"
              (token-value
                (consume-token tokens)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of sign parselet.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Sign-Parselet (Nud-Parselet)
  ((precedence
    :initarg       :precedence
    :initform      (error "Missing sign parselet precedence.")
    :type          Precedence
    :documentation "The sign's precedence."))
  (:documentation
    "The ``Sign-Parselet'' class serves in the parsing of an expression
     preceded by a positive or negative sign."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Sign-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Sign-Parselet parselet))
  (declare (type Token         nud-token))
  (declare (type Token-Stream  tokens))
  (the AST-Node
    (case (token-type nud-token)
      ((:plus :minus)
        (make-ast-node :unary-operation
          :operator (token-type nud-token)
          :operand
            (parse-expression
              (get-parselet-binding-power parselet)
              tokens)))
      (otherwise
        (error "Invalid arithmetic sign in ~a." nud-token)))))

;;; -------------------------------------------------------

(defmethod get-parselet-binding-power ((parselet Sign-Parselet))
  (declare (type Sign-Parselet parselet))
  (the integer
    (precedence-effective-binding-power
      (slot-value parselet 'precedence))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of group parselet.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Group-Parselet (Nud-Parselet)
  ((precedence
    :initform      (make-precedence 0 :none)
    :type          Precedence
    :documentation "The expression grouping's precedence."))
  (:documentation
    "The ``Group-Parselet'' implements a nud token processor responsible
     for the grouping of arithmetic expressions."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Group-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Group-Parselet parselet))
  (declare (type Token          nud-token))
  (declare (type Token-Stream   tokens))
  (the AST-Node
    (if (token-of-type-p nud-token :left-parenthesis)
      (error "Expected a left parenthesis, but encountered ~s."
        nud-token)
      (make-ast-node :group :expression
        (parse-expression 0 tokens)))))

;;; -------------------------------------------------------

(defmethod get-parselet-binding-power ((parselet Group-Parselet))
  (declare (type Group-Parselet parselet))
  (the integer
    (precedence-effective-binding-power
      (slot-value parselet 'precedence))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of led parselet interface.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Led-Parselet ()
  ()
  (:documentation
    "The ``Led-Parselet'' interface establishes the substratum for all
     classes pursing the accoutrement of led, or consequent, tokens'
     parsing."))

;;; -------------------------------------------------------

(defgeneric apply-led-parselet (parselet
                                left-expression
                                led-token
                                tokens)
  (:documentation
    "Parses the LED-TOKEN in the PARSELET's context, the LEFT-EXPRESSION
     being the contribution of the preveniently parsed sinistral
     operand, employing the stream of TOKENS for the contingency of
     further tokens' obtention, and returns an ``AST-Node''
     representation of the parsed expression."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operation parselet.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-Operation-Parselet (Led-Parselet)
  ((precedence
    :initarg       :precedence
    :initform      (error "Missing binary parselet precedence.")
    :type          Precedence
    :documentation "The binary operator's precedence."))
  (:documentation
    "The ``Binary-Operation-Parselet'' class applies itself to the
     evaluation of a led token cognate with a binary operation from the
     arithmetic realm."))

;;; -------------------------------------------------------

(defmethod apply-led-parselet
    ((parselet        Binary-Operation-Parselet)
     (left-expression AST-Node)
     (led-token       Token)
     (tokens          Token-Stream))
  (declare (type Binary-Operation-Parselet parselet))
  (declare (type AST-Node                  left-expression))
  (declare (type Token                     led-token))
  (declare (type Token-Stream              tokens))
  (the AST-Node
    (make-ast-node :binary-operation
      :operator     (token-type led-token)
      :left-operand left-expression
      :right-operand
        (parse-expression
          (get-parselet-binding-power parselet)
          tokens))))

;;; -------------------------------------------------------

(defmethod get-parselet-binding-power
    ((parselet Binary-Operation-Parselet))
  (declare (type Binary-Operation-Parselet parselet))
  (the integer
    (precedence-effective-binding-power
      (slot-value parselet 'precedence))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet registries.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Nud-Parselet) +NUD-PARSELETS+))
(declaim (type (hash-table-of keyword Led-Parselet) +LED-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +NUD-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized nud token types with parselets dedicated
   to their parsing.")

(defparameter +LED-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized led token types with parselets dedicated
   to their parsing.")

;;; -------------------------------------------------------

(defun nud-token-p (token)
  "Determines whether the TOKEN represents a nud token, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token) +NUD-PARSELETS+)))))

;;; -------------------------------------------------------

(defun get-nud-parselet (token)
  "Returns the parselet responsible for the nud TOKEN's parsing, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the Nud-Parselet
    (or (gethash (token-type token) +NUD-PARSELETS+)
        (error "No nud token: ~a." token))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (token)
  "Returns the binding power associated with the nud TOKEN, or signals
   an error of an unspecified upon its disrespondency."
  (declare (type Token token))
  (the integer
    (get-parselet-binding-power
      (get-nud-parselet token))))

;;; -------------------------------------------------------

(defun parse-nud-token (token tokens)
  "Parses the nud TOKEN utilizing the stream of TOKENS for the
   contingency of further data's acquisition and returns the thus
   yielded expression's value."
  (declare (type Token        token))
  (declare (type Token-Stream tokens))
  (the AST-Node
    (apply-nud-parselet
      (get-nud-parselet token) token tokens)))

;;; -------------------------------------------------------

(defun register-nud-token (token-type parselet)
  "Associates the nud TOKEN-TYPE with the PARSELET in the
   +NUD-PARSELETS+ table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Nud-Parselet parselet))
  (setf (gethash token-type +NUD-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defun led-token-p (token)
  "Determines whether the TOKEN represents a led token, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token) +LED-PARSELETS+)))))

;;; -------------------------------------------------------

(defun get-led-parselet (token)
  "Returns the parselet responsible for the led TOKEN's parsing, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the Led-Parselet
    (or (gethash (token-type token) +LED-PARSELETS+)
        (error "No led token: ~a." token))))

;;; -------------------------------------------------------

(defun get-led-binding-power (token)
  "Returns the binding power associated with the led TOKEN, or signals
   an error of an unspecified upon its disrespondency."
  (declare (type Token token))
  (the integer
    (get-parselet-binding-power
      (get-led-parselet token))))

;;; -------------------------------------------------------

(defun parse-led-token (left-expression token tokens)
  "Parses the led TOKEN, preceded by the LEFT-EXPRESSION as the first
   operand, and utilizing the stream of TOKENS for the contingency of
   further data's acquisition and returns the thus yielded expression's
   value."
  (declare (type AST-Node     left-expression))
  (declare (type Token        token))
  (declare (type Token-Stream tokens))
  (the AST-Node
    (apply-led-parselet
      (get-led-parselet token) left-expression token tokens)))

;;; -------------------------------------------------------

(defun register-led-token (token-type parselet)
  "Associates the led TOKEN-TYPE with the PARSELET in the
   +LED-PARSELETS+ table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Led-Parselet parselet))
  (setf (gethash token-type +LED-PARSELETS+) parselet)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-nud-token :digit
  (make-instance 'Number-Parselet))

;;; -------------------------------------------------------

(register-nud-token :plus
  (make-instance 'Sign-Parselet
    :precedence (make-precedence 170 :right-to-left)))

;;; -------------------------------------------------------

(register-nud-token :minus
  (make-instance 'Sign-Parselet
    :precedence (make-precedence 170 :right-to-left)))

;;; -------------------------------------------------------

(register-nud-token :left-parenthesis
  (make-instance 'Group-Parselet))

;;; -------------------------------------------------------

(register-led-token :plus
  (make-instance 'Binary-Operation-Parselet
    :precedence (make-precedence 130 :left-to-right)))

;;; -------------------------------------------------------

(register-led-token :minus
  (make-instance 'Binary-Operation-Parselet
    :precedence (make-precedence 130 :left-to-right)))

;;; -------------------------------------------------------

(register-led-token :times
  (make-instance 'Binary-Operation-Parselet
    :precedence (make-precedence 140 :left-to-right)))

;;; -------------------------------------------------------

(register-led-token :divide
  (make-instance 'Binary-Operation-Parselet
    :precedence (make-precedence 140 :left-to-right)))

;;; -------------------------------------------------------

(register-led-token :remainder
  (make-instance 'Binary-Operation-Parselet
    :precedence (make-precedence 140 :left-to-right)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Pratt parsing routine.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (current-binding-power tokens)
  "Parses an expression whose invoking instance is represented by the
   CURRENT-BINDING-POWER, employing the stream of TOKENS for contingent
   subsequent token objects' obtention, and returns the evaluated
   expression's value."
  (declare (type integer      current-binding-power))
  (declare (type Token-Stream tokens))
  (let ((result
          (parse-nud-token
            (consume-token tokens)
            (skip-to-expression-token tokens))))
    (declare (type AST-Node result))
    (loop
      for next-token
        of-type Token
        =       (peek-token
                  (skip-to-expression-token tokens))
      if (not (led-token-p next-token)) do
        (loop-finish)
      else if (<= (get-led-binding-power next-token)
                  current-binding-power) do
        (loop-finish)
      else do
        (consume-token tokens)
        (setf result
          (parse-led-token result next-token tokens)))
    (the AST-Node result)))

;;; -------------------------------------------------------

(defun parse-statement (tokens)
  "Parses a HeacunQ9+× statement from the stream of TOKENS and returns
   a connable ``AST-Node'' representation thereof."
  (declare (type Token-Stream tokens))
  (the AST-Node
    (let ((next-token (consume-token tokens)))
      (declare (type Token next-token))
      (case (token-type next-token)
        (:h
          (make-ast-node :print-hello-world))
        (:e
          (make-ast-node :execute-function))
        (:a
          (make-ast-node :define-function :statements
            (parse-function-body tokens)))
        (:c
          (prog1
            (make-ast-node :set-accumulator :value
              (parse-expression 0 tokens))
            (expect-token     tokens :c)))
        (:u
          (make-ast-node :input-or-output-number))
        (:n
          (make-ast-node :input-or-output-character))
        (:q
          (make-ast-node :quine :source-code
            (token-value next-token)))
        (:digit
          (if (= (token-value next-token) 9)
            (make-ast-node :print-99-bottles-of-beer)
            (make-ast-node :nop)))
        (:plus
          (make-ast-node :increment-accumulator))
        (:×
          (make-ast-node :execute-function-if-true))
        (otherwise
          (make-ast-node :nop))))))

;;; -------------------------------------------------------

(defun parse-function-body (tokens)
  "Expected to be located after the instigating \"A\" symbol, parses a
   function definition from the stream of TOKENS and returns a
   ``:block'' node representation of the assembled function body."
  (declare (type Token-Stream tokens))
  (the AST-Node
    (make-ast-node :block :statements
      (loop
        for next-token of-type Token = (peek-token tokens)
        until (token-of-type-p next-token :a)
          collect
            (case next-token
              (:eof
                (error "Unterminated function declaration."))
              (otherwise
                (parse-statement tokens)))
          into statements
        finally
          (expect-token tokens :a)
          (return statements)))))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Parses a HeacunQ9+× program utilizing the stream of TOKENS and
   returns a ``:program'' node representation thereof."
  (declare (type Token-Stream tokens))
  (the AST-Node
    (make-ast-node :program :statements
      (make-ast-node :block :statements
        (loop
          for next-token of-type Token = (peek-token tokens)
          until (token-of-type-p next-token :eof)
            collect (parse-statement tokens))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Interpreter AST-Node) *) visit-node))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing program tree.")
    :type          AST-Node
    :documentation "The HeacunQ9+× program as an abstract syntax tree
                    (AST).")
   (accumulator
    :initform      0
    :type          integer
    :documentation "The aefauld memory register.")
   (function
    :initform      NIL
    :type          (or null AST-Node)
    :documentation "The most recently defined function, stored, if
                    set at all, in the form of its ``:define-function''
                    node."))
  (:documentation
    "The ``Interpreter'' class is apportioned that onus to administer
     actual efficacy to a HeacunQ9+× program supplied as an abstract
     syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   processing of the abstract syntax TREE representation of a
   HeacunQ9+× program."
  (declare (type AST-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun execute-function (interpreter)
  "Executes the function consigned to the INTERPRETER's castaldy, if
   such is defined, in any case returning no value."
  (declare (type Interpreter interpreter))
  (with-slots (function) interpreter
    (declare (type (or null AST-Node) function))
    (when function
      (visit-node interpreter
        (get-ast-node-attribute function :statements))))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Evaluates the abstract syntax tree (AST) NODE, dispatched on its
     NODE-TYPE, in the INTERPRETER's context and returns a result
     covenable for this combination."))

;;; -------------------------------------------------------

(defun visit-node (interpreter node)
  "Evaluates the NODE, dispatching on its node type, in the
   INTERPRETER's context and returns a result covenable for this
   combination."
  (declare (type Interpreter interpreter))
  (declare (type AST-Node    node))
  (the T
    (dispatch-node interpreter
      (ast-node-type node)
      node)))

;;; -------------------------------------------------------

(defmacro define-node-dispatch
    (node-type (interpreter-variable node-variable)
     &body body)
  "Defines an implementation of the generic function ``dispatch-node'',
   utilizing the agnomination INTERPRETER-VARIABLE for its first formal
   parameter, assigns an automatically generated name for the second
   parameter, which dispatches on the NODE-TYPE, and employs the
   NODE-VARIABLE for its third input, evaluates the BODY forms, and
   returns the desinent form's results."
  (let ((node-type-variable (gensym)))
    (declare (type symbol node-type-variable))
    `(defmethod dispatch-node ((,interpreter-variable Interpreter)
                               (,node-type-variable   (eql ,node-type))
                               (,node-variable        AST-Node))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type keyword     ,node-type-variable))
       (declare (ignore           ,node-type-variable))
       (declare (type AST-Node    ,node-variable))
       (declare (ignorable        ,node-variable))
       ,@body)))

;;; -------------------------------------------------------

(define-node-dispatch :program (interpreter node)
  (visit-node interpreter
    (get-ast-node-attribute node :statements))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :block (interpreter node)
  (dolist (statement (get-ast-node-attribute node :statements))
    (declare (type AST-Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :increment-accumulator (interpreter node)
  (incf (slot-value interpreter 'accumulator))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :set-accumulator (interpreter node)
  (setf (slot-value interpreter 'accumulator)
    (visit-node interpreter
      (get-ast-node-attribute node :value)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :number (interpreter node)
  (the (integer 0 *)
    (get-ast-node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :unary-operation (interpreter node)
  (the integer
    (let ((operator (get-ast-node-attribute node :operator)))
      (declare (type keyword operator))
      (case operator
        (:plus
          (visit-node interpreter
            (get-ast-node-attribute node :operand)))
        (:minus
          (- (visit-node interpreter
               (get-ast-node-attribute node :operand))))
        (otherwise
          (error "Invalid unary operator: ~a." operator))))))

;;; -------------------------------------------------------

(define-node-dispatch :group (interpreter node)
  (the integer
    (visit-node interpreter
      (get-ast-node-attribute node :expression))))

;;; -------------------------------------------------------

(define-node-dispatch :binary-operation (interpreter node)
  (the integer
    (nth-value 0
      (funcall
        (case (get-ast-node-attribute node :operator)
          (:plus      #'+)
          (:minus     #'-)
          (:times     #'*)
          (:divide    #'round)
          (:remainder #'rem)
          (otherwise
            (error "Invalid binary operator: ~a."
              (get-ast-node-attribute node :operator))))
        (visit-node interpreter
          (get-ast-node-attribute node :left-operand))
        (visit-node interpreter
          (get-ast-node-attribute node :right-operand))))))

;;; -------------------------------------------------------

(define-node-dispatch :input-or-output-character (interpreter node)
  (with-slots (accumulator) interpreter
    (declare (type integer accumulator))
    (cond
      ((= accumulator -1)
        (format T "~&Please input a character: ")
        (finish-output)
        (setf accumulator
          (char-code
            (read-char NIL NIL #\Null)))
        (clear-input))
      (T
        (format T "~c"
          (code-char accumulator)))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :input-or-output-number (interpreter node)
  (with-slots (accumulator) interpreter
    (declare (type integer accumulator))
    (cond
      ((= accumulator -1)
        (format T "~&Please input a number: ")
        (finish-output)
        (setf accumulator
          (parse-integer
            (read-line NIL NIL 0)))
        (clear-input))
      (T
        (format T "~d " accumulator))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :print-hello-world (interpreter node)
  (format T "~&Hello, World!")
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :print-99-bottles-of-beer (interpreter node)
  (loop for number-of-bottles of-type (integer 0 99) from 99 downto 1 do
    (format T "~&~d bottle~:p of beer on the wall,~%~
               ~:*~d bottle~:p of beer.~%~
               Take one down pass it around,~%~
               ~:[~d~;No~] bottle~:p of beer on the wall.~2%"
      number-of-bottles
      (zerop (1- number-of-bottles))
      (1- number-of-bottles)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :quine (interpreter node)
  (format T "~&~a"
    (get-ast-node-attribute node :source-code))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :define-function (interpreter node)
  (setf (slot-value interpreter 'function) node)
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :execute-function (interpreter node)
  (execute-function interpreter)
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :execute-function-if-true (interpreter node)
  (unless (zerop (slot-value interpreter 'accumulator))
    (execute-function interpreter))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :nop (interpreter node)
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the program maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-HeacunQ9+× (code)
  "Interprets the piece of HeacunQ9+× source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-token-stream
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-HeacunQ9+× "C-1CUAUEA×U")

;;; -------------------------------------------------------

;; Repeating cat program.
(interpret-HeacunQ9+× "AC-1CNNEAE")

;;; -------------------------------------------------------

;; Quine.
(interpret-HeacunQ9+× "Q")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-HeacunQ9+× "H")

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-HeacunQ9+× "9")
