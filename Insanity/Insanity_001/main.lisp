;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Insanity", originated by the Esolang user
;; "PythonshellDebugwindow" in coefficacy with the user "A" in the year
;; 2020, and based upon variables which are initialized once and
;; subsequently manipulated in an implicitly operating infinite loop.
;; 
;; 
;; Concept
;; =======
;; The Insanity programming language's kenspeckle emblem resides in the
;; operation on variables, initialized once in the incipient program
;; line, and manipulated in an infinitely repeating fashion by the same
;; instructions in order to yield varying results.
;; 
;; == INSANITY: REPEATED ACTIONS, ALTERNATED RESPONSES ==
;; The agnomination "Insanity" is founded upon a statement attributed to
;; the scientist Albert Einstein, claiming that insanity incorporates
;; the dioristic haecceity that expects for a repetition of the same
;; actions a differentiation in the conclusion.
;; 
;; The Insanity programming language makes light of this declamation, as
;; its variables, once initialized, are subjected to the influences of
;; an infinite iteration's body, the same tacitly assigns each statement
;; line's expressions to the available placeholders, thus destructively
;; modifying their state.
;; 
;; A consectary of this procedure, whereas the code's phenotypical
;; component avers constancy, the epiphenoma, ensuing from modulations
;; in the variables' contents, bear the potentials to vary.
;; 
;; The ludibund elements resides in a contradiction of Einstein's claim
;; by repeating the same actions --- or program statements --- and
;; concomitantly yielding differentiated results --- stored in the
;; constantly modified variables.
;; 
;; == VARIABLES: LETTERS TO STORE NUMBERS ==
;; Data management in the language is realized by variables, each such a
;; salvatory to a scalar integer or floating-point number.
;; 
;; The variables' nevening follows a simple, fixed convention, involving
;; every identifier's restriction to a single Latin letter, covering the
;; members "a", "b", "c", to "z", and continuing into "A", "B", to "Z".
;; The table below shall educate about the identifiers and their
;; positions:
;; 
;;   ---------------------
;;   Index | Variable name
;;   ------+--------------
;;      1  |      a
;;   .....................
;;      2  |      b
;;   .....................
;;      3  |      c
;;   .....................
;;      4  |      d
;;   .....................
;;      5  |      e
;;   .....................
;;      6  |      f
;;   .....................
;;      7  |      g
;;   .....................
;;      8  |      h
;;   .....................
;;      9  |      i
;;   .....................
;;     10  |      j
;;   .....................
;;     11  |      k
;;   .....................
;;     12  |      l
;;   .....................
;;     13  |      m
;;   .....................
;;     14  |      n
;;   .....................
;;     15  |      o
;;   .....................
;;     16  |      p
;;   .....................
;;     17  |      q
;;   .....................
;;     18  |      r
;;   .....................
;;     19  |      s
;;   .....................
;;     20  |      t
;;   .....................
;;     21  |      u
;;   .....................
;;     22  |      v
;;   .....................
;;     23  |      w
;;   .....................
;;     24  |      x
;;   .....................
;;     25  |      y
;;   .....................
;;     26  |      z
;;   .....................
;;     27  |      A
;;   .....................
;;     28  |      B
;;   .....................
;;     29  |      C
;;   .....................
;;     30  |      D
;;   .....................
;;     31  |      E
;;   .....................
;;     32  |      F
;;   .....................
;;     33  |      G
;;   .....................
;;     34  |      H
;;   .....................
;;     35  |      I
;;   .....................
;;     36  |      J
;;   .....................
;;     37  |      K
;;   .....................
;;     38  |      L
;;   .....................
;;     39  |      M
;;   .....................
;;     40  |      N
;;   .....................
;;     41  |      O
;;   .....................
;;     42  |      P
;;   .....................
;;     43  |      Q
;;   .....................
;;     44  |      R
;;   .....................
;;     45  |      S
;;   .....................
;;     46  |      T
;;   .....................
;;     47  |      U
;;   .....................
;;     48  |      V
;;   .....................
;;     49  |      W
;;   .....................
;;     50  |      X
;;   .....................
;;     51  |      Y
;;   .....................
;;     52  |      Z
;;   ---------------------
;; 
;; This order constitutes a fact of significance as variables cannot be
;; assigned arbitrarily, but following their position in the definition
;; sequence.
;; 
;; == PROGRAMS ARE COMPOSED OF LINES ==
;; Every Insanity program is composed of zero or more lines, the
;; incipient member of which defines the variables' initial values.
;; The once executed initialization line precedes an arbitrary tally of
;; rows that shall be repeated in an infinite cycle, thus comprising the
;; implicit loop body.
;; 
;; == VARIABLES: INITIALIZE ONCE, RUN FOREVER ==
;; A non-empty program's inchoation is delineated by an initialization
;; line compact of zero or more numeric values or arithmetic expressions
;; resolving to the same. The variable identifiers' absence constitutes
;; a diorism of this section species, as no variables are defined yet;
;; their availability is, based upon the parallel binding principle,
;; granted in the aftermath of the line's perfected evaluation.
;; 
;; Demonstrated on a schematic forbisen, for an initial or loop body
;; line build from the expressions
;; 
;;   expression_1, expression_2, ..., expression_N
;; 
;; the variables are assigned as
;; 
;;   variable_1, variable_2, .., variable_N
;; 
;; where the identifier assignment proceeds in concord with the above
;; listed table, that is,
;; 
;;   variable_1  = "a"
;;   variable_2  = "b"
;;   ...
;;   variable_26 = "z"
;;   variable_27 = "A"
;;   ...
;;   variable_52 = "Z"
;; 
;; The assigned variables, if exhausted, thus constitute
;; 
;;   a, b, ..., z, A, B, ..., Z
;; 
;; Please note that, by mediation of the parallel binding, the
;; assignment of a variable variable_i does not immediately take effect,
;; such that succeeding expressions on the same line will operate on the
;; value adhibited by the previous modification.
;; 
;; == PARALLEL BINDING: VARIABLES ARE UPDATED AFTER A LINE ==
;; A line's variable assignments proceed by parallel binding; that is,
;; across a horizontal extent, a variable's manipulation exercises no
;; influence on its application in the preceding or succeeding
;; expressions; the variables retain their previous value during the
;; evaluation, until all expressions have been finalized. Ensuing from
;; this status, the actual variable updating transpires.
;; 
;; The parallel binding concept as invested in this context shall be a
;; pseudocode expression's cynosure, constrained to an aefauld line's
;; explication:
;; 
;;   let lineResults <- empty list
;;   
;;   { Evaluate the line expressions and collect their results. }
;;   for each expression expr[i] on the current line do
;;     let result[i] <- evaluated value of expr[i]
;;     append result[i] to lineResults
;;   end for
;;   
;;   { Assign the collected line results to the respective variables. }
;;   { Note the variable names' implicit ordering, from "a" to "Z".   }
;;   for each line result result[i] in lineResults do
;;     set the variable var[i] to result[i]
;;   end
;; 
;; A simple example's equiparation of the actually employed parallel
;; binding, serving to distinguish from the theoretical output of a
;; sequential alternative, is produced alow:
;; 
;;   ------------------------------------------------------------------
;;   Line   | Parallel binding (actual)     | Sequential bind. (theor.)
;;   -------+-------------------------------+--------------------------
;;   0, 1   | result[a] = 0                 | a = 0
;;          | result[b] = 1                 | b = 1
;;          |-------------------------------|
;;          | a = result[a] = 0             |
;;          | b = result[b] = 1             |
;;   ..................................................................
;;   b, a+b | result[a] = b = 1             | a = b = 1
;;          | result[b] = a + b = 0 + 1 = 1 | b = a + b = 1 + 1 = 2
;;          |-------------------------------|
;;          | a = result[a] = 1             |
;;          | b = result[b] = 1             |
;;   ------------------------------------------------------------------
;; 
;; The discriminating conclusions in the two principles shall entertain
;; their elucidation in a forbisen founded upon the original
;; specification's aefauld example, a Fibonacci sequence generator,
;; whose "a" variable values, in particular, limn a tantamount of the
;; expected results:
;; 
;;   ------------------------------------------------------------------
;;   Line   | Parallel binding              | Sequential binding
;;   -------+-------------------------------+--------------------------
;;   0, 1   | result[a] = 0                 | a = 0
;;          | result[b] = 1                 | b = 1
;;          |-------------------------------|
;;          | a = result[a] = 0             |
;;          | b = result[b] = 1             |
;;   ..................................................................
;;   b, a+b | result[a] = b = 1             | a = b = 1
;;          | result[b] = a + b = 0 + 1 = 1 | b = a + b = 1 + 1 = 2
;;          |-------------------------------|
;;          | a = result[a] = 1             |
;;          | b = result[b] = 1             |
;;   ..................................................................
;;   b, a+b | result[a] = b = 1             | a = b = 2
;;          | result[b] = a + b = 1 + 1 = 2 | b = a + b = 2 + 2 = 4
;;          |-------------------------------|
;;          | a = result[a] = 1             |
;;          | b = result[b] = 2             |
;;   ..................................................................
;;   b, a+b | result[a] = b = 2             | a = b = 4
;;          | result[b] = a + b = 1 + 2 = 3 | b = a + b = 4 + 4 = 8
;;          |-------------------------------|
;;          | a = result[a] = 2             |
;;          | b = result[b] = 3             |
;;   ..................................................................
;;   b, a+b | result[a] = b = 3             | a = b = 8
;;          | result[b] = a + b = 2 + 3 = 5 | b = a + b = 8 + 8 = 16
;;          |-------------------------------|
;;          | a = result[a] = 3             |
;;          | b = result[b] = 5             |
;;   ..................................................................
;;   b, a+b | result[a] = b = 5             | a = b = 16
;;          | result[b] = a + b = 3 + 5 = 8 | b = a + b = 16 + 16 = 32
;;          |-------------------------------|
;;          | a = result[a] = 5             |
;;          | b = result[b] = 8             |
;;   ..................................................................
;;   [...]
;;   ------------------------------------------------------------------
;; 
;; A patent aftercome, the sequential procedure would not yield the
;; recognized requisitum.
;; 
;; 
;; Architecture
;; ============
;; Apart from the urgency to maintain a bounded sequence of variables,
;; no further commodities are assigned to the architecture's ambit.
;; 
;; The variable registry conforms to an associative ilk, homologating
;; adit to any of the 52 variables, amenable to Latin minuscles and
;; majuscles as identifiers, each of which may either map to an integer
;; or floating-point number, or be designated as unbound, in the case of
;; an assignment's absence.
;; 
;; 
;; Data Types
;; ==========
;; The entirety of Insanity's data types exists in relation to integer
;; and floating-point numbers of any sign and magnitude, both expressed
;; as literals and maintained in variables, in any case permitted a
;; participation in arithmetic expressions.
;; 
;; 
;; Syntax
;; ======
;; An Insanity program's syntactical structure divides it into a series
;; of zero or more lines, any of which composed of comma-separated
;; expressions, with the first member representing the initialization
;; row, interdicting variables, whereas all subsequent statements
;; produce the implicit loop body that tolerates variable identifiers.
;; 
;; == PROGRAMS ARE STRUCTURED LINEWISE ==
;; The Insanity program ordonnance manifests in a division into zero or
;; more lines, the incipient of which, if extant, determines the
;; variables' initialization, succeeded by zero or more statement lines,
;; constituting the implicitly operating loop's body.
;; 
;; == THE INITIALIZATION LINE INITIALIZES THE VARIABLES ==
;; The initialization comprehends zero or more expressions, compact of
;; literal numbers and optional operators applying to the same, each two
;; of which are separated by a single comma (","). Variables, as yet
;; non-existent entities, may not be queried.
;; 
;; == THE STATEMENT LINES REASSIGN THE VARIABLES ==
;; Whereas the initalization line precedes the variables' existence, and
;; thus ought to abstain from their ordination, the statement lines,
;; equally composed of comma-separated items, may embrace in their
;; expressions variable identifiers as a supererogation accolent to the
;; acquainted numeric literals and general mathematical compounds.
;; 
;; == WHITESPACES ==
;; The interspersion of spaces, a term to embrace both the space
;; character " " and the horizontal tab, constitutes a dependency upon
;; the programmer's personal delectation.
;; 
;; Linebreaks deviate from this liberty in their appropriation as an
;; obliging sepiment betwixt the program lines.
;; 
;; == COMMENTS ==
;; No provision for comments is accommodated in this language iteration.
;; 
;; == GRAMMAR ==
;; A formulation of the syntax in the Extended Backus-Naur Form (EBNF)
;; avails in the language's delineation:
;; 
;;   program            := lineSeparator
;;                      ,  [ initalizationLine ,
;;                           { lineSeparator , loopBodyLine }
;;                         ]
;;                      ,  lineSeparator
;;                      ;
;;   lineSeparator      := { emptyLine , linebreak } ;
;;   emptyLine          := spaces ;
;;   initializationLine := [ number , { "," , number } ] ;
;;   loopBodyLine       := [ expression , { "," , expression } ] ;
;;   expression         := number
;;                      |  variable
;;                      |  expression , operator , expression
;;                      |  "(" , expression , ")"
;;                      ;
;;   operator           := "+" | "-" | "*" | "/" ;
;;   variable           := "a" | "b" | ... | "z" | "A" | "B" | ... | "Z" ;
;;   number             := [ "+" | "-" ] , digits , [ "." , digits ] ;
;;   digits             := digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   linebreak          := "\n" ;
;;   spaces             := { space } ;
;;   space              := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; The ambitus of Insanity's instruction set solely tallies the
;; warklumes for basic arithmetic operations, namely, addition,
;; subtraction, multiplication, and division.
;; 
;; == OVERVIEW ==
;; The following building blocks for expressions exist, employing brace
;; jumelles in the mold "{" and "}" to denote placeholder segments.
;; 
;;   ------------------------------------------------------------------
;;   Expression       | Description
;;   ------------------------------------------------------------------
;;   {number}         | A signed integer or floating-point literal.
;;   ..................................................................
;;   {variable}       | The single-letter identifier of a variable
;;                    | whose numeric value shall be obtained.
;;                    | If not assigned yet, an error of an unspecified
;;                    | type is issued.
;;   ..................................................................
;;   {left} + {right} | Adds the right operand to the left operand and
;;                    | returns the sum.
;;   ..................................................................
;;   {left} - {right} | Subtracts the right operand from the left
;;                    | operand and returns the difference.
;;   ..................................................................
;;   {left} * {right} | Multiplies the left operand by the right
;;                    | operand and returns the product.
;;   ..................................................................
;;   {left} / {right} | Divides the left operand by the right operand
;;                    | and returns the quotient.
;;   ------------------------------------------------------------------
;; 
;; Please note that expressions can be grouped by amplectation via a
;; jumelle of opening and closing parentheses, "(" and ")".
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
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-05-26
;; 
;; Sources:
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
;;   [esolang2020Insanity]
;;   The Esolang contributors, "Insanity", 2020
;;   URL: "https://esolangs.org/wiki/Insanity"
;;   
;;   [grand1997javalangref]
;;   Mark Grand, "Java Language Reference", 2nd Edition July 1997,
;;               "Chapter 4.14 Order of Operations"
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
;;   [sedgewick2022operatorprecedence]
;;   Robert Sedgewick, Kevin Wayne,
;;     "Appendix A: Operator Precedence in Java", 2022
;;   URL: "https://introcs.cs.princeton.edu/java/11precedence/"
;;   Notes:
;;     - Operator precedence in Java.
;;   
;;   [stackoverflow2011q2811319]
;;   The Stack Overflow contributors, "What is the difference between
;;     >>> and >> operators in Java?", 2011
;;   URL: "https://stackoverflow.com/questions/2811319/
;;         difference-between-and"
;;   Notes:
;;     - Describes the ">>>" operator in Java an unsigned right shift.
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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
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
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``asssociativity'' type enumerates the recognized variants of
   operator associativity, serving to differentiate priorities betwixt
   operators competing for operands while entalented with an
   equipollence in their binding power."
  '(member :left :right :none))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more abstract syntax
   tree (AST) nodes."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   embracing, without the claim of exhaustion, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates the type and value of a significant
   portion of analyzed Insanity source code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space character ---
   either a space or a horizontal tab ---, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Insanity source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class applies itself to the lexical analyzation of a
     piece of Insanity source code into its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which analyzes the SOURCE."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next character in its
   source, updates its state, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Consumes the current character from the LEXER's source and returns a
   new token associating the TOKEN-TYPE with this datum."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token token-type
        (prog1 character
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-read-variable (lexer)
  "Consumes the current character from the LEXER's source and returns a
   ``:variable'' token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :variable
        (prog1 character
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Commencing at the current position into the LEXER's source, consumes
   an integer or floating-point number and returns a ``:number'' token
   representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :number
        (read-from-string
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (flet ((read-digits ()
                    "Reads a sequence of zero or more digits from the
                     LEXER's source, writes these to the DIGITS stream,
                     and returns no value."
                    (loop
                      while (and character (digit-char-p character))
                      do
                        (write-char character digits)
                        (lexer-advance lexer))
                    (values)))
              (read-digits)
              (when (and character (char= character #\.))
                (write-char character digits)
                (lexer-advance lexer)
                (read-digits)))))))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Commencing at the current position into the LEXER's source, skips a
   sequence of zero or more adjacent spaces and returns the modified
   LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, each request is answered with a fresh
   end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (lexer-skip-spaces    lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\Newline)
          (lexer-read-symbol lexer :newline))
        
        ((char= character #\+)
          (lexer-read-symbol lexer :plus))
        
        ((char= character #\-)
          (lexer-read-symbol lexer :minus))
        
        ((char= character #\*)
          (lexer-read-symbol lexer :asterisk))
        
        ((char= character #\/)
          (lexer-read-symbol lexer :slash))
        
        ((char= character #\,)
          (lexer-read-symbol lexer :comma))
        
        ((char= character #\()
          (lexer-read-symbol lexer :left-parenthesis))
        
        ((char= character #\))
          (lexer-read-symbol lexer :right-parenthesis))
        
        ((alpha-char-p character)
          (lexer-read-variable lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        (T
          (error "Unrecogized character \"~c\" at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Stream ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing token stream lexer.")
    :type          Lexer
    :documentation "The lexer whose tokens shall be queried.")
   (next-token
    :initarg       :next-token
    :initform      (make-token :eof NIL)
    :documentation "The token which shall be peeked or consumed next."))
  (:documentation
    "The ``Token-Stream'' class provides an entity whose competence
     permits it to peek and remove a specific lexer's tokens."))

;;; -------------------------------------------------------

(defun make-token-stream (lexer)
  "Creates and returns a new ``Token-Stream'' which provides access to
   the LEXER."
  (declare (type Lexer lexer))
  (the Token-Stream
    (make-instance 'Token-Stream :lexer lexer)))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((token-stream Token-Stream) &key)
  (declare (type Token-Stream token-stream))
  (with-slots (lexer next-token) token-stream
    (declare (type Lexer lexer))
    (declare (type Token next-token))
    (setf next-token
      (lexer-get-next-token lexer)))
  (the Token-Stream token-stream))

;;; -------------------------------------------------------

(defun token-stream-peek (token-stream)
  "Returns without removing the next token from the TOKEN-STREAM's
   underlying lexer."
  (declare (type Token-Stream token-stream))
  (the Token
    (slot-value token-stream 'next-token)))

;;; -------------------------------------------------------

(defun token-stream-consume (token-stream)
  "Removes and returns the next token from the TOKEN-STREAM's underlying
   lexer."
  (declare (type Token-Stream token-stream))
  (with-slots (lexer next-token) token-stream
    (declare (type Lexer lexer))
    (declare (type Token next-token))
    (the Token
      (prog1 next-token
        (setf next-token
          (lexer-get-next-token lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      (error "Missing node type.")
    :reader        node-type
    :type          keyword
    :documentation "The categorizing type of this node.")
   (attributes
    :initarg       :attributes
    :initform      (make-hash-table :test #'eql)
    :accessor      node-attributes
    :type          (hash-table-of keyword T)
    :documentation "Associates the node property names with the
                    respective values."))
  (:documentation
    "The ``Node'' class encapsulates the information requisite for the
     representation of a particular object or facility in an Insanity
     program as an abstract syntax tree (AST) node."))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' categorized by the TYPE and
   endowed with the INITIAL-ATTRIBUTES."
  (declare (type keyword     type))
  (declare (type (list-of T) initial-attributes))
  (let ((new-node (make-instance 'Node :type type)))
    (declare (type Node new-node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (node-attributes new-node))
              attribute-value))
    (the Node new-node)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the attribute value corresponding to the ATTRIBUTE-NAME in
   the NODE, or signals an error of an unspecified type upon its
   absence."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (multiple-value-bind (attribute-value contains-attribute-p)
      (gethash attribute-name (node-attributes node))
    (declare (type T attribute-value))
    (declare (type T contains-attribute-p))
    (the T
      (if contains-attribute-p
        attribute-value
        (error "No node attribute with the name ~s found."
          attribute-name)))))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "(Node type=~s" (node-type node))
  (maphash
    #'(lambda (attribute-name attribute-value)
        (declare (type keyword attribute-name))
        (declare (type T       attribute-value))
        (format stream ", ~a=~s" attribute-name attribute-value))
    (node-attributes node))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet classes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ()
  (:documentation
    "The ``Parselet'' interface supplies a common foundation for all
     ilks of the parselet concept's reification."))

;;; -------------------------------------------------------

(defclass Abstract-Parselet (Parselet)
  ((binding-power
    :initarg       :binding-power
    :initform      0
    :accessor      parselet-binding-power
    :type          integer
    :documentation "The initial (nud) or consequent (led) token's
                    binding power, determining the token's priority
                    during an operand's attempted appropriation.")
   (associativity
    :initarg       :associativity
    :initform      :none
    :accessor      parselet-associativity
    :type          associativity
    :documentation "The initial (nud) or consequent (led) token's
                    associativity, determining the token's priority
                    during an operand's attempted appropriation when in
                    contest with an equipollent operator."))
  (:documentation
    "The ``Abstract-Parselet'' class establishes a base ensconcing those
     piece of information appertaining to all or a preponderance of the
     variegated parselet variants."))

;;; -------------------------------------------------------

(defclass Initial-Parselet (Abstract-Parselet)
  ((callback
    :initarg       :callback
    :initform      (error "Missing initial parselet callback.")
    :accessor      initial-parselet-callback
    :type          (function (Token Token-Stream) Node)
    :documentation "The parsing function which, provided with the
                    initial or nud token to parse and a token stream for
                    a potential dextral operand's generation, responds
                    with a node representation of the initial or nud
                    token."))
  (:documentation
    "The ``Initial-Parselet'' class represents an initial or nud token's
     parsing capability, compact of the ubiquitous binding power and
     associativity properties, and in conjugation with a callback
     function responsible for the actual parsing routine."))

;;; -------------------------------------------------------

(defclass Consequent-Parselet (Abstract-Parselet)
  ((callback
    :initarg       :callback
    :initform      (error "Missing consequent parselet callback.")
    :accessor      consequent-parselet-callback
    :type          (function (Token Token-Stream Node) Node)
    :documentation "The parsing function which, provided with the
                    consequent or led token to parse, a token stream for
                    a potential dextral operand's generation, and the
                    preceding left operand, having already been
                    converted from its token form into a node, responds
                    with a node representation of the consequent or led
                    token."))
  (:documentation
    "The ``Consequent-Parselet'' class represents a consequent or led
     token's parsing capability, compact of the uniquitous binding power
     and associativity properties, and in conjunction with a callback
     function responsible for the actual parsing routine."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Initial-Parselet)
         *initial-tokens*))
(declaim (type (hash-table-of keyword Consequent-Parselet)
         *consequent-tokens*))

;;; -------------------------------------------------------

(defparameter *initial-tokens*
  (make-hash-table :test #'eq)
  "Associates the recognized initial or nud token types with parselets
   responsible for their evaluation.")

(defparameter *consequent-tokens* (make-hash-table :test #'eq)
  "Associates the recognized consequent or led token types with
   parselets responsible for their evaluation.")

;;; -------------------------------------------------------

(defun initial-token-p (token)
  "Determines whether the TOKEN represents an initial or nud token,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) *initial-tokens*))))))

;;; -------------------------------------------------------

(defun get-initial-binding-power (token)
  "Returns the binding power associated with the initial or nud TOKEN,
   or signals an error of an unspecified type if none such is specified
   for the same."
  (declare (type Token token))
  (the integer
    (parselet-binding-power
      (gethash (token-type token) *initial-tokens*))))

;;; -------------------------------------------------------

(defun get-initial-associativity (token)
  "Returns the associativity for the initial or nud TOKEN, employed in
   situations of its competition with an operator token of equal binding
   power, or signals an error of an unspecified type if none such is
   specified for the same."
  (declare (type Token token))
  (the associativity
    (parselet-associativity
      (gethash (token-type token) *initial-tokens*))))

;;; -------------------------------------------------------

(defun get-effective-initial-binding-power (token)
  "Returns the binding power associated with the initial or nud TOKEN,
   taking its associativity into account, or signals an error of an
   unspecified type if none such is specified for the same."
  (declare (type Token token))
  (let ((associativity (get-initial-associativity token)))
    (declare (type associativity associativity))
    (the integer
      (case associativity
        (:left
          (get-initial-binding-power token))
        (:right
          (1- (get-initial-binding-power token)))
        (:none
          (get-initial-binding-power token))
        (otherwise
          (error "Invalid associativity for the initial token ~s."
            token))))))

;;; -------------------------------------------------------

(defun parse-initial-token (token tokens)
  "Parses the initial or nud TOKEN, employing the token stream TOKENS
   for contingent provision of additional data, and returns a node
   representing the parsing result.
   ---
   An error of an unspecified type is signaled if the TOKEN does not
   constitute an initial or nud token."
  (declare (type Token        token))
  (declare (type Token-Stream tokens))
  (the Node
    (funcall
      (initial-parselet-callback
        (gethash (token-type token) *initial-tokens*))
      token tokens)))

;;; -------------------------------------------------------

(defun consequent-token-p (token)
  "Determines whether the TOKEN represents an consequent or led token,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) *consequent-tokens*))))))

;;; -------------------------------------------------------

(defun get-consequent-binding-power (token)
  "Returns the binding power associated with the consequent or led
   TOKEN, or signals an error of an unspecified type if none such is
   specified for the same."
  (declare (type Token token))
  (the integer
    (parselet-binding-power
      (gethash (token-type token) *consequent-tokens*))))

;;; -------------------------------------------------------

(defun get-consequent-associativity (token)
  "Returns the associativity for the consequent or led TOKEN, employed
   in situations of its competition with an operator token of equal
   binding power, or signals an error of an unspecified type if none
   such is specified for the same."
  (declare (type Token token))
  (the associativity
    (parselet-associativity
      (gethash (token-type token) *consequent-tokens*))))

;;; -------------------------------------------------------

(defun get-effective-consequent-binding-power (token)
  "Returns the binding power associated with the consequent or led
   TOKEN, taking its associativity into account, or signals an error of
   an unspecified type if none such is specified for the same."
  (declare (type Token token))
  (let ((associativity (get-consequent-associativity token)))
    (declare (type associativity associativity))
    (the integer
      (case associativity
        (:left
          (get-consequent-binding-power token))
        (:right
          (1- (get-consequent-binding-power token)))
        (:none
          (get-consequent-binding-power token))
        (otherwise
          (error "Invalid associativity for the consequent token ~s."
            token))))))

;;; -------------------------------------------------------

(defun parse-consequent-token (token tokens left-node)
  "Parses the consequent or led TOKEN, employing the token stream TOKENS
   for contingent provision of additional data and the LEFT-NODE as the
   sinistral operand, and returns a node representing the parsing
   result.
   ---
   An error of an unspecified type is signaled if the TOKEN does not
   constitute a consequent or led token"
  (declare (type Token        token))
  (declare (type Token-Stream tokens))
  (declare (type Node         left-node))
  (the Node
    (funcall
      (consequent-parselet-callback
        (gethash (token-type token) *consequent-tokens*))
      token tokens left-node)))

;;; -------------------------------------------------------

(defun register-initial-token (token-type initial-parselet)
  "Registers the TOKEN-TYPE as an initial or nud token at the
   *INITIAL-TOKENS* and associates it with the INITIAL-PARSELET,
   returning no value.
   ---
   Any extant entry bearing the TOKEN-TYPE as its key will be tacitly
   supplanted."
  (declare (type keyword          token-type))
  (declare (type Initial-Parselet initial-parselet))
  (setf (gethash token-type *initial-tokens*) initial-parselet)
  (values))

;;; -------------------------------------------------------

(defun register-consequent-token (token-type consequent-parselet)
  "Registers the TOKEN-TYPE as a consequent or led token at the
   *CONSEQUENT-TOKENS* and associates it with the CONSEQUENT-PARSELET,
   returning no value.
   ---
   Any extant entry bearing the TOKEN-TYPE as its key will be tacitly
   supplanted."
  (declare (type keyword             token-type))
  (declare (type Consequent-Parselet consequent-parselet))
  (setf (gethash token-type *consequent-tokens*) consequent-parselet)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression parser.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (tokens current-binding-power)
  "Parses an expression involving the data provided by TOKENS token
   stream, utilizing the invoker's CURRENT-BINDING-POWER, and returns a
   node representation of the thus assembled output."
  (declare (type Token-Stream tokens))
  (declare (type integer      current-binding-power))
  
  (let ((initial-token (token-stream-consume tokens))
        (left-node     NIL))
    (declare (type Token          initial-token))
    (declare (type (or null Node) left-node))
    
    (unless (initial-token-p initial-token)
      (error "The token ~s does not represent an initial-token."
        initial-token))
    
    (setf left-node (parse-initial-token initial-token tokens))
    
    (loop for next-token of-type Token = (token-stream-peek tokens) do
      (cond
        ((token-type-p next-token :eof)
          (loop-finish))
        
        ((not (consequent-token-p next-token))
          (loop-finish))
        
        ((<= (get-consequent-binding-power next-token)
             current-binding-power)
          (loop-finish))
        
        (T
          (token-stream-consume tokens)
          (setf left-node
            (parse-consequent-token next-token tokens left-node)))))
    
    (the Node left-node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of top-level parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun end-of-line-token-p (token)
  "Determines whether the TOKEN represents a newline or end-of-file
   (EOF), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token) '(:newline :eof) :test #'eq)))))

;;; -------------------------------------------------------

(defun skip-newlines (tokens)
  "Commencing at the current token in the TOKENS stream, skips a
   sequence of zero or more consecutive ``:newline'' tokens and returns
   the modified TOKENS."
  (declare (type Token-Stream tokens))
  (loop while (token-type-p (token-stream-peek tokens) :newline) do
    (token-stream-consume tokens))
  (the Token-Stream tokens))

;;; -------------------------------------------------------

(defun parse-line (tokens)
  "Parses the TOKENS token stream to assemble a program line and returns
   a ``:line'' node representation thereof."
  (declare (type Token-Stream tokens))
  (let ((expressions NIL)
        (next-token  (token-stream-peek tokens)))
    (declare (type node-list expressions))
    (declare (type Token     next-token))
    
    (unless (end-of-line-token-p next-token)
      (push (parse-expression tokens 0) expressions)
      
      (loop do
        (setf next-token (token-stream-peek tokens))
        
        (case (token-type next-token)
          ((:eof :newline)
            (loop-finish))
          (:comma
            (token-stream-consume tokens)
            (push (parse-expression tokens 0) expressions))
          (otherwise
            (error "Unexpected token ~s." next-token)))))
    
    (the Node
      (make-node :line :elements (nreverse expressions)))))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Assembles from the TOKENS token stream an abstract syntax tree (AST)
   representation of an Insanity program and returns the root
   ``:program'' node."
  (declare (type Token-Stream tokens))
  (let ((lines NIL))
    (declare (type (list-of Node) lines))
    (loop do
      (skip-newlines tokens)
      (if (token-type-p (token-stream-peek tokens) :eof)
        (loop-finish)
        (push (parse-line tokens) lines)))
    (setf lines (nreverse lines))
    (the Node
      (make-node :program
        :initializations (first lines)
        :body            (rest  lines)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-initial-token :number
  (make-instance 'Initial-Parselet
    :callback
      #'(lambda (initial-token tokens)
          (declare (type Token        initial-token))
          (declare (type Token-Stream tokens))
          (declare (ignore            tokens))
          (the Node
            (make-node :number :value (token-value initial-token))))))

;;; -------------------------------------------------------

(register-initial-token :variable
  (make-instance 'Initial-Parselet
    :callback
      #'(lambda (initial-token tokens)
          (declare (type Token        initial-token))
          (declare (type Token-Stream tokens))
          (declare (ignore            tokens))
          (the Node
            (make-node :variable :name (token-value initial-token))))))

;;; -------------------------------------------------------

(register-initial-token :plus
  (make-instance 'Initial-Parselet
    :binding-power 30
    :associativity :right
    :callback
      #'(lambda (initial-token tokens)
          (declare (type Token        initial-token))
          (declare (type Token-Stream tokens))
          (the Node
            (make-node :unary-operation
              :operator :plus
              :operand
                (parse-expression tokens
                  (get-effective-initial-binding-power
                    initial-token)))))))

;;; -------------------------------------------------------

(register-initial-token :minus
  (make-instance 'Initial-Parselet
    :binding-power 30
    :associativity :right
    :callback
      #'(lambda (initial-token tokens)
          (declare (type Token        initial-token))
          (declare (type Token-Stream tokens))
          (the Node
            (make-node :unary-operation
              :operator :minus
              :operand
                (parse-expression tokens
                  (get-effective-initial-binding-power
                    initial-token)))))))

;;; -------------------------------------------------------

(register-initial-token :left-parenthesis
  (make-instance 'Initial-Parselet
    :binding-power 0
    :associativity :none
    :callback
      #'(lambda (initial-token tokens)
          (declare (type Token        initial-token))
          (declare (type Token-Stream tokens))
          (the Node
            (prog1
              (make-node :group :expression
                (parse-expression tokens
                  (get-effective-initial-binding-power initial-token)))
              (let ((current-token (token-stream-peek tokens)))
                (declare (type Token current-token))
                (if (token-type-p current-token :right-parenthesis)
                  (token-stream-consume tokens)
                  (error "Expected a closing parenthesis, but ~
                          encountered the token ~s."
                    current-token))))))))

;;; -------------------------------------------------------

(register-consequent-token :plus
  (make-instance 'Consequent-Parselet
    :binding-power 10
    :associativity :left
    :callback
      #'(lambda (consequent-token tokens left-node)
          (declare (type Token        consequent-token))
          (declare (type Token-Stream tokens))
          (declare (type Node         left-node))
          (the Node
            (make-node :binary-operation
              :operator      :addition
              :left-operand  left-node
              :right-operand
                (parse-expression tokens
                  (get-effective-consequent-binding-power
                    consequent-token)))))))

;;; -------------------------------------------------------

(register-consequent-token :minus
  (make-instance 'Consequent-Parselet
    :binding-power 10
    :associativity :left
    :callback
      #'(lambda (consequent-token tokens left-node)
          (declare (type Token        consequent-token))
          (declare (type Token-Stream tokens))
          (declare (type Node         left-node))
          (the Node
            (make-node :binary-operation
              :operator      :subtraction
              :left-operand  left-node
              :right-operand
                (parse-expression tokens
                  (get-effective-consequent-binding-power
                    consequent-token)))))))

;;; -------------------------------------------------------

(register-consequent-token :asterisk
  (make-instance 'Consequent-Parselet
    :binding-power 20
    :associativity :left
    :callback
      #'(lambda (consequent-token tokens left-node)
          (declare (type Token        consequent-token))
          (declare (type Token-Stream tokens))
          (declare (type Node         left-node))
          (the Node
            (make-node :binary-operation
              :operator      :multiplication
              :left-operand  left-node
              :right-operand
                (parse-expression tokens
                  (get-effective-consequent-binding-power
                    consequent-token)))))))

;;; -------------------------------------------------------

(register-consequent-token :slash
  (make-instance 'Consequent-Parselet
    :binding-power 20
    :associativity :left
    :callback
      #'(lambda (consequent-token tokens left-node)
          (declare (type Token        consequent-token))
          (declare (type Token-Stream tokens))
          (declare (type Node         left-node))
          (the Node
            (make-node :binary-operation
              :operator      :division
              :left-operand  left-node
              :right-operand
                (parse-expression tokens
                  (get-effective-consequent-binding-power
                    consequent-token)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Variable-Registry".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 52) +VARIABLE-NAMES+))

;;; -------------------------------------------------------

(defparameter +VARIABLE-NAMES+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Lists the possible variable names in the correct order.")

;;; -------------------------------------------------------

(defclass Variable-Registry ()
  ((variables
    :initarg       :variables
    :initform      (make-hash-table :test #'eql)
    :accessor      variable-registry-variables
    :type          (hash-table-of character real)
    :documentation "Associates the assigned variable names with their
                    numeric values."))
  (:documentation
    "The ``Variable-Registry'' class applies itself with the castaldy
     of variables, associating with the available set of names the
     assigned objects."))

;;; -------------------------------------------------------

(defun make-variable-registry ()
  "Creates and returns an initially empty ``Variable-Registry''."
  (the Variable-Registry
    (make-instance 'Variable-Registry)))

;;; -------------------------------------------------------

(defun variable-registry-value (registry name)
  "Returns the value assigned to the variable identified by the NAME in
   the variable REGISTRY, or signals an error of an unspecified type
   upon its disrespondency."
  (declare (type Variable-Registry registry))
  (declare (type character         name))
  (the real
    (or (gethash name (slot-value registry 'variables))
        (error "Uninitialized variable: \"~a\"." name))))

;;; -------------------------------------------------------

(defun (setf variable-registry-value) (new-value registry name)
  "Assigns to the variable with the NAME in the variable REGISTRY the
   NEW-VALUE and returns the modified REGISTRY."
  (declare (type T                 new-value))
  (declare (type Variable-Registry registry))
  (declare (type character         name))
  (setf (gethash name (slot-value registry 'variables)) new-value)
  (the Variable-Registry registry))

;;; -------------------------------------------------------

(defun variable-registry-contains-p (registry name)
  "Determines whether the variable identified by the NAME has been
   assigned in the variable REGISTRY, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Variable-Registry registry))
  (declare (type character         name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash name
          (slot-value registry 'variables)))))))

;;; -------------------------------------------------------

(defun variable-registry-assign-values (registry variable-values)
  "Assigns the VARIABLE-VALUES to the variable REGISTRY's variables in
   their natural order and returns the modified REGISTRY."
  (declare (type Variable-Registry registry))
  (declare (type (list-of real)    variable-values))
  (loop
    for variable-value of-type real      in     variable-values
    and variable-name  of-type character across +VARIABLE-NAMES+
    do  (setf (variable-registry-value registry variable-name)
              variable-value))
  (the Variable-Registry registry))

;;; -------------------------------------------------------

(defun variable-registry-for-each (registry callback)
  "Iterates the assigned variables of the variable REGISTRY in their
   natural order, invokes for each of these the CALLBACK function with
   the current variable name as the sole argument, ignoring the result,
   and returns no value."
  (declare (type Variable-Registry registry))
  (declare (type (function (character *) *) callback))
  (loop for variable-name of-type character across +VARIABLE-NAMES+ do
    (when (variable-registry-contains-p registry variable-name)
      (funcall callback variable-name
        (variable-registry-value registry variable-name))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree.")
    :type          Node
    :documentation "The abstract syntax tree (AST) representation of the
                    Insanity program to evaluate.")
   (variables
    :initarg       :variable
    :initform      (make-variable-registry)
    :type          Variable-Registry
    :documentation "Associates the declared variables with their
                    values in a table.")
   (print-variables-p
    :initarg       :print-variables-p
    :initform      T
    :type          boolean
    :documentation "Determines whether, at the end of each iteration
                    cycle or betwixt the end of the initialization line
                    and the first iteration cycle, the assigned
                    variables shall be printed to the standard output.")
   (cycle-delay
    :initarg       :cycle-delay
    :initform      0.5
    :type          real
    :documentation "The number of seconds to wait betwixt to iteration
                    cycles or betwixt the end of the initialization
                    line and the first iteration cycle."))
  (:documentation
    "The ``Interpreter'' class evaluates an abstract syntax tree (AST)
     representation of an Insanity program."))

;;; -------------------------------------------------------

(defun make-interpreter (tree
                         &key (print-variables-p T)
                              (cycle-delay       0.5))
  "Creates and returns a new ``Interpreter'' which evaluates the
   abstract syntax TREE (AST)."
  (declare (type Node tree))
  (the Interpreter
    (make-instance 'Interpreter
      :tree              tree
      :print-variables-p print-variables-p
      :cycle-delay       cycle-delay)))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Evaluates the NODE, dispatched on its NODE-TYPE, in the
     INTERPRETER's context, and returns a value appropriate for this
     combination."))

;;; -------------------------------------------------------

(defun interpreter-visit-node (interpreter node)
  "Evaluates the NODE in the INTERPRETER's context by dispatching on the
   NODE type pursuing to invoke the eligible
   ``interpreter-dispatch-node'' method, and returns the value yielded
   by the same."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the (or null real)
    (interpreter-dispatch-node interpreter
      (node-type node) node)))

;;; -------------------------------------------------------

(defun interpreter-print-variables (interpreter)
  "Prints the variables assigned by the INTERPRETER's variable registry
   in their natural order and returns no value."
  (declare (type Interpreter interpreter))
  (when (slot-value interpreter 'print-variables-p)
    (variable-registry-for-each
      (slot-value interpreter 'variables)
      #'(lambda (variable-name variable-value)
          (declare (type character variable-name))
          (declare (type T         variable-value))
          (format T "~&~s => ~s"
            variable-name variable-value)))
    (terpri)
    (terpri))
  (values))

;;; -------------------------------------------------------

(defun interpreter-delay-iteration (interpreter)
  "Temporarily stalls the INTERPRETER for the number of seconds
   specified by the INTERPRETER's \"cycle delay\" and returns no value."
  (declare (type Interpreter interpreter))
  (sleep (slot-value interpreter 'cycle-delay))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :program))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((initializations (node-attribute node :initializations))
        (body            (node-attribute node :body)))
    (declare (type (or null Node) initializations))
    (declare (type (list-of Node) body))
    
    (variable-registry-assign-values
      (slot-value interpreter 'variables)
      (loop
        for initial-expression
        of-type Node
        in      (node-attribute initializations :elements)
        collect
          (interpreter-visit-node interpreter initial-expression)))
    
    (interpreter-print-variables interpreter)
    
    ;; Repeatedly execute the implicit loop body.
    (loop do
      (loop for line of-type Node in body do
        (interpreter-visit-node      interpreter line)
        (interpreter-print-variables interpreter)
        (interpreter-delay-iteration interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :variable))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real
    (variable-registry-value
      (slot-value interpreter 'variables)
      (node-attribute node :name))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :number))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real
    (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :line))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((new-variable-values
          (loop
            for expression
              of-type Node
              in      (node-attribute node :elements)
            collect
              (interpreter-visit-node interpreter expression))))
    (declare (type (list-of real) new-variable-values))
    ;; Update the variables.
    (variable-registry-assign-values
      (slot-value interpreter 'variables)
      new-variable-values))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :unary-operation))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real
    (case (node-attribute node :operator)
      (:plus
        (interpreter-visit-node interpreter
          (node-attribute node :operand)))
      (:minus
        (- (interpreter-visit-node interpreter
             (node-attribute node :operand))))
      (otherwise
        (error "Invalid unary operator: ~s."
          (node-attribute node :operator))))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :binary-operation))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real
    (funcall
      (case (node-attribute node :operator)
        (:addition       #'+)
        (:subtraction    #'-)
        (:multiplication #'*)
        (:division       #'/)
        (otherwise       (error "Invalid binary operator: ~s."
                           (node-attribute node :operator))))
      (interpreter-visit-node interpreter
        (node-attribute node :left-operand))
      (interpreter-visit-node interpreter
        (node-attribute node :right-operand)))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :group))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real
    (interpreter-visit-node interpreter
      (node-attribute node :expression))))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the Insanity program stored in the INTERPRETER as an
   abstract syntax tree (AST) and returns no value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-Insanity (code &key (print-variables-p T)
                                     (cycle-delay       0.5))
  "Interprets the piece of Insanity source CODE, configured to
   potentially print the variable values after each iteration if
   PRINT-VARIABLES-P resolves to ``T'', and delaying each two cycles by
   the CYCLE-DELAY in seconds, and returns no value."
  (declare (type string  code))
  (declare (type boolean print-variables-p))
  (declare (type real    cycle-delay))
  (interpreter-interpret
    (make-interpreter
      (parse-program
        (make-token-stream
          (make-lexer code)))
      :print-variables-p print-variables-p
      :cycle-delay       cycle-delay))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Counter which counts up from one (1) to infinity.
(interpret-Insanity
  "1
   a+1")

;;; -------------------------------------------------------

;; Demonstrates arithmetics and grouping by producing the result 14 in
;; the variable "a".
(interpret-Insanity
  "2 * (3 + 4)")

;;; -------------------------------------------------------

;; This program swaps the values of the variables "a" and "b"
;; repeatedly, starting with
;;   a = 1
;;   b = 2.
(interpret-Insanity
  "1, 2
   b, a")

;;; -------------------------------------------------------

;; Fibonacci sequence generator which stores the results in the variable
;; "a".
;; The results of "a thus comprise
;;   0, 1, 1, 2, 3, 5, 8, ...
(interpret-Insanity
  "0, 1
   b, a+b")

;;; -------------------------------------------------------

;; Factorial generator which stores the result n! in variable "a", while
;; the variable "b" holds the next multiplicand n+1.
;; The results of "a" thus comprise
;;   1, 2, 6, 24, 120, 720, ...
(interpret-Insanity
  "1, 2
   a * b, b + 1")
