;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "JustWords", invented by the Esolang user "AnotherUser05"
;; and presented on February 8th, 2024, the dioristic attribute of which
;; wones in its code's diction, expressed exclusively via the tokens'
;; spellings, in lieu of the acquainted symbolic variety inherent to
;; linguistics.
;; 
;; 
;; Concept
;; =======
;; The JustWords programming language's foundry is realized in the
;; notion of a vocabulary's componency from Latin letter only which
;; reflects the desiderated tokens' spelled form.
;; 
;; 
;; Architecture
;; ============
;; The simplicity commorant in the language expels any contingency as
;; well as necessity for sophistication in the architectural segment,
;; tharfing no storage component.
;; 
;; 
;; Data Type
;; =========
;; An exemplary of equipoise to the architecture, JustWords type system
;; admits a more varigated, yet manageable circumference, its
;; circumference's amplectation extending around both unsigned
;; non-negative integer numbers and arbitrary strings.
;; 
;; 
;; Syntax
;; ======
;; JustWords's syntaxis pursues an exclusive and stalwart application of
;; spelled dictions as expressive tokens, with any two commands'
;; segregation proceeding by mediation of one or more whitespaces.
;; 
;; 
;; Instructions
;; ============
;; The JustWords instruction set enumerates a tally of thirteen
;; participants, augmented by the contingency for the definition of
;; unsigned integral literals, and entailing basic arithmetics, logical
;; operations, one conditional, two iterative, and an input/output
;; facility.
;; 
;; == OVERVIEW ==
;; The following apercu shall be a warklume for a cursory acquaintance
;; with the language's operative elements' administration.
;; 
;; Please heed that succedaneous segments are ensconced in a twissel of
;; braces, "{...}", and intended for their supersession by actual
;; JustWords code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command               | Effect
;;   ----------------------+-------------------------------------------
;;   {digits}              | Installs a sequence of one or more decimal
;;                         | {digits} which, in the order of their
;;                         | specification, form a single unsigned
;;                         | integer number, where any member is
;;                         | desumed from the following set:
;;                         |   --------------------------
;;                         |   Identifier | Numeric value
;;                         |   -----------+--------------
;;                         |   Zero       | 0
;;                         |   ..........................
;;                         |   One        | 1
;;                         |   ..........................
;;                         |   Two        | 2
;;                         |   ..........................
;;                         |   Three      | 3
;;                         |   ..........................
;;                         |   Four       | 4
;;                         |   ..........................
;;                         |   Five       | 5
;;                         |   ..........................
;;                         |   Six        | 6
;;                         |   ..........................
;;                         |   Seven      | 7
;;                         |   ..........................
;;                         |   Eight      | 8
;;                         |   ..........................
;;                         |   Nine       | 9
;;                         |   --------------------------
;;   ..................................................................
;;   Print{argument}       | Prints the {argument} to the standard
;;                         | output.
;;                         |-------------------------------------------
;;                         | The {argument} may be any expression.
;;   ..................................................................
;;   Input                 | Queries the standard input for a signed or
;;                         | unsigned integer number or an arbitrary
;;                         | string and returns the same.
;;   ..................................................................
;;   {left}Plus{right}     | Adds the {right} number to the {left}
;;                         | number and returns the sum.
;;   ..................................................................
;;   {left}Minus{right}    | Subtracts the {right} number from the
;;                         | {left} number and returns the difference.
;;   ..................................................................
;;   {left}Multiply{right} | Multiplies the {left} number by the
;;                         | {right} and returns the product.
;;   ..................................................................
;;   {left}Divide{right}   | Divides the {left} number by the {right}
;;                         | number, rounds to the nearest integral
;;                         | value, and returns the quotient.
;;   ..................................................................
;;   {left}Equals{right}   | Returns a Boolean truth value of "true"
;;                         | if the {left} value equals the {right}
;;                         | value, otherwise returns "false".
;;   ..................................................................
;;   {left}Less{right}     | Returns a Boolean truth value of "true"
;;                         | if the {left} value is less than the
;;                         | {right} value, otherwise returns "false".
;;   ..................................................................
;;   {left}Greater{right}  | Returns a Boolean truth value of "true"
;;                         | if the {left} value is greater than the
;;                         | {right} value, otherwise returns "false".
;;   ..................................................................
;;   If{condition}Then     | Determines whether the {condition} is
;;     {statements}        | satisfied, on confirmation executing the
;;   End                   | {statements} in their given order;
;;                         | otherwise no causatum applies.
;;   ..................................................................
;;   If{condition}Then     | Determines whether the {condition} is
;;     {thenStatements}    | satisfied, on confirmation executing the
;;   Else                  | {thenStatements} in their given order;
;;     {elseStatements}    | otherwise instigates the {elseStatements}.
;;   End                   | 
;;   ..................................................................
;;   Forever               | Perpetually repeats the {statements}.
;;     {statements}        | 
;;   End                   | 
;;   ..................................................................
;;   Loop{repetitions}     | Repeats the {statements} a tally of times
;;     {statements}        | equal to the {repetitions} count.
;;   End                   | 
;;   ..................................................................
;;   Stop                  | Immediately terminates the program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp utilizes a recursive
;; descent parser, extended and complemented by a Pratt parser component
;; for the processing of expressions, the species of which derives from
;; Douglas Crockford's [crockford2007topdownopprec] augmentation of the
;; basic principles to incorporate statements.
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
;; 
;; Appendices
;; ==========
;; A series of topics' value resides in the crepuscle betwixt peisant
;; contribution and parhedral intelligence, to whom, as an ultimity of
;; this assessment, shall be nuncupated the following appendix sections.
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
;;   -----------------------------------------
;;   Operator | Binding power | Associativity
;;   ---------+---------------+---------------
;;   Equals   |  90           | left-to-right
;;   .........................................
;;   Less     | 100           | left-to-right
;;   .........................................
;;   Greater  | 100           | left-to-right
;;   .........................................
;;   Plus     | 130           | left-to-right
;;   .........................................
;;   Minus    | 130           | left-to-right 
;;   .........................................
;;   Multiply | 140           | left-to-right
;;   .........................................
;;   Divide   | 140           | left-to-right
;;   -----------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-27
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
;;   [esolang2024JustWords]
;;   The Esolang contributors, "JustWords", February 20th, 2024
;;   URL: "https://esolangs.org/wiki/JustWords"
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type definition macros.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type employing the ``deftype'' infrastructure in
   conjunction with the ``satisfies'' type specifier, the agnomination
   of which is obtained from the TYPE-NAME, the formal parameters from
   the LAMBDA-LIST, and the predicate function's sole input, the probed
   object, being nevend through the CANDIDATE-VARIABLE, evaluating the
   BODY forms with access to the same, and construing a non-``NIL''
   return value in the desinent form as the ascertainment of the probed
   object's eligibility, while the docimasy fails for the ``NIL''
   response.
   ---
   If the first BODY form constitutes a string, the same interpreted as
   the type specifier's documentation string and reappropriated for this
   purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
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

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   T)
                                                 (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each such identified by a key
   conformant with the KEY-TYPE and a value subscribing to the
   VALUE-TYPE, for both holds the default of ``T''."
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

(deftype identifier-table ()
  "The ``identifier-table'' defines a mapping of recognized words to
   representative tokens, realized in a hash table whose keys
   accommodate a commorancy to the words' string forms, while the
   associated values establish the ``Token'' equivalents."
  '(hash-table-of simple-string Token))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), but bourneless along the upper axis."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized variation on
   binding powers for unary and binary operators, utile, in particular,
   in the context of Pratt parsing."
  '(member :none :left-to-right :right-to-left))

;;; -------------------------------------------------------

(deftype nud-processor ()
  "The ``nud-processor'' type defines a nud parselet's operative
   component as a function of two arguments, these being the nud token
   to processor and the token stream whose services may be requested in
   order to obtain a contingent dextral expression, returning an
   abstract syntax tree (AST) node representation of the thus parsed
   nud expression.
   ---
   As a corollary, the function complies to the signature:
     lambda (nud-token token-stream) => ast-node"
  '(function (Token Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype led-processor ()
  "The ``led-processor'' type defines a led parselet's operative
   component as a function of three arguments, these being the left
   expression already supputated, the led token to process, and the
   token stream whose services may be requested in order to obtain a
   contingent dextral expression, returning an abstract syntax tree
   (AST) node representation of the thus parsed led expression.
   ---
   As a consectary, the function complies to the signature:
     lambda (left-expression nud-token token-stream) => ast-node"
  '(function (AST-Node Token Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype std-processor ()
  "The ``std-processor'' type defines a std parselet's operative
   component as a function of two arguments, these being the std token
   to evaluate and the token stream whose services may be appropriated
   for subsequent tokens' obtention, the function result constituting an
   abstract syntax tree (AST) node.
   ---
   As a consectary, the function complies to the signature:
     lambda (std-token token-stream) => ast-node"
  '(function (Token Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variants on
   binary operations."
  '(member
    :plus
    :minus
    :multiply
    :divide
    :equals
    :greater
    :less))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list whose componency enumerates a
   tally of zero or more elements conformant with the ELEMENT-TYPE, for
   which holds the comprehensive default of ``T''."
  (and
    (listp candidate)
    (loop for element of-type T in (the list candidate) always
      (typep element element-type))))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list of zero or more
   abstract syntax tree (AST) nodes, each such of the class ``AST-Node''
   or one of its subclasses."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which amplects, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a stringently Boolean tantamount of the OBJECT's
   \"generalized boolean\" status, that is, either ``T'' if the OBJECT
   is not ``NIL'', otherwise ``NIL''.
   ---
   The inferior significance usually apportioned to the stricture of the
   traditional Boolean dichotomoy in Common Lisp may yet be harnessed in
   order to elevate the code's expressive potentials."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL)))
  (:copier      NIL))
  "The ``Token'' class appropriates the dever of a significant object's
   encapsulation, the same originates from a piece of JustWords source
   code's analyzation."
  (type  (error "Missing type.")  :type keyword :read-only T)
  (value (error "Missing value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifiers.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized words with token representations.")

;;; -------------------------------------------------------

(flet ((register-word (identifier token)
        "Affiliates the word IDENTIFIER with the TOKEN representation
         in the +IDENTIFIERS+ table, contingently superseding any
         extant entry for the former's key, and returns no value."
        (declare (type simple-string identifier))
        (declare (type Token         token))
        (setf (gethash identifier +IDENTIFIERS+) token)
        (values)))
  
  ;; Numerals.
  (register-word "Zero"     (make-token :number   0))
  (register-word "One"      (make-token :number   1))
  (register-word "Two"      (make-token :number   2))
  (register-word "Three"    (make-token :number   3))
  (register-word "Four"     (make-token :number   4))
  (register-word "Five"     (make-token :number   5))
  (register-word "Six"      (make-token :number   6))
  (register-word "Seven"    (make-token :number   7))
  (register-word "Eight"    (make-token :number   8))
  (register-word "Nine"     (make-token :number   9))
  
  ;; Input and output.
  (register-word "Print"    (make-token :print    "Print"))
  (register-word "Input"    (make-token :input    "Input"))
  
  ;; Arithmetic operators.
  (register-word "Plus"     (make-token :plus     "Plus"))
  (register-word "Minus"    (make-token :minus    "Minus"))
  (register-word "Multiply" (make-token :multiply "Multiply"))
  (register-word "Divide"   (make-token :divide   "Divide"))
  
  ;; Relational operators.
  (register-word "Equals"   (make-token :equals   "Equals"))
  (register-word "Greater"  (make-token :greater  "Greater"))
  (register-word "Less"     (make-token :less     "Less"))
  
  ;; Control flow tokens.
  (register-word "If"       (make-token :if       "If"))
  (register-word "Then"     (make-token :then     "Then"))
  (register-word "Else"     (make-token :else     "Else"))
  (register-word "Loop"     (make-token :loop     "Loop"))
  (register-word "Forever"  (make-token :forever  "Forever"))
  (register-word "End"      (make-token :end      "End"))
  (register-word "Stop"     (make-token :stop     "Stop"))
  
  (values))

;;; -------------------------------------------------------

(defun parse-identifier (identifier)
  "Returns the token representation of the IDENTIFIER, if such exists,
   otherwise produces an ``:identifier'' token comprehending the
   IDENTIFIER."
  (declare (type string identifier))
  (the Token
    (or (gethash identifier +IDENTIFIERS+)
        (make-token :identifier identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun majuscule-p (candidate)
  "Determines whether the CANDIDATE represents a majuscular Latin
   letter, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and (alpha-char-p candidate)
           (upper-case-p candidate)))))

;;; -------------------------------------------------------

(defun minuscule-p (candidate)
  "Determines whether the CANDIDATE represents a minuscular Latin
   letter, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and (alpha-char-p candidate)
           (lower-case-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-character-sequence (input)
  "Evaluates the INPUT and returns either an integer object
   representation or, if ineligible for such purpose, the INPUT itself
   in its ipsissima verba string form."
  (declare (type string input))
  (the (or integer string)
    (handler-case
      (parse-integer input)
      (error () input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class applies itself to a piece of JustWords source
   code's lexical analyzation, the ultimity of which is nuncupated to
   the extract and delivery of the recognized tokens."
  (source    (error "Missing source.")
             :type      string
             :read-only T)
  (position  0
             :type      fixnum
             :read-only NIL)
  (character NIL
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Imparts a commodity for a convenient adit to the LEXER's slots by
   evaluating the same, and binding its ``source'' slot to the local
   symbol macro ``$source'', the ``position'' to ``$position'', and the
   ``character'' to ``$character'', executes the BODY forms, and returns
   the desinent form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           (($source
              (the string
                (lexer-source ,evaluated-lexer)))
            ($position
              (the fixnum
                (lexer-position ,evaluated-lexer)))
            ($character
              (the (or null character)
                (lexer-character ,evaluated-lexer))))
         (declare (type string              $source))
         (declare (ignorable                $source))
         (declare (type fixnum              $position))
         (declare (ignorable                $position))
         (declare (type (or null character) $character))
         (declare (ignorable                $character))
         ,@body))))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Returns the LEXER's current character, while concomitantly advancing
   to the next position in its source, if possible.
   ---
   If the LEXER's source is exhausted prevenient to this operation, the
   ``NIL'' value is returned."
  (declare (type Lexer lexer))
  (the (or null character)
    (with-lexer (lexer)
      (prog1 $character
        (setf $character
          (when (array-in-bounds-p $source (1+ $position))
            (char $source
              (incf $position))))))))

;;; -------------------------------------------------------

(defun expect-majuscule (lexer)
  "Determines whether the LEXER's current character constitutes a
   majuscular Latin letter, returning on confirmation the same, while
   concomitantly advancing to the next character in its source;
   otherwise signals an error of an unspecified type."
  (declare (type Lexer lexer))
  (the character
    (with-lexer (lexer)
      (cond
        ((null $character)
          (error "Expected a majuscule at position ~d, ~
                  but found the source exhausted."
            $position))
        ((majuscule-p $character)
          (advance-lexer lexer))
        (T
          (error "Expected a majuscule at position ~d, ~
                  but encountered the character \"~c\"."
            $position $character))))))

;;; -------------------------------------------------------

(defun read-word (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes a capitalized word and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (the Token
    (parse-identifier
      (with-output-to-string (word)
        (declare (type string-stream word))
        (write-char (expect-majuscule lexer) word)
        (with-lexer (lexer)
          (loop while (and $character (minuscule-p $character)) do
            (write-char (advance-lexer lexer) word)))))))

;;; -------------------------------------------------------

(defun read-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes a sequence of zero or more whitspaces and returns a token
   representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :whitespaces
      (with-output-to-string (whitespaces)
        (declare (type string-stream whitespaces))
        (with-lexer (lexer)
          (loop
            while (and $character (whitespace-character-p $character))
            do    (write-char (advance-lexer lexer) whitespaces)))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhasution, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (cond
        ((null $character)
          (make-eof-token))
        ((whitespace-character-p $character)
          (read-whitespaces lexer))
        ((majuscule-p $character)
          (read-word lexer))
        ((minuscule-p $character)
          (error "Invalid character \"~c\" at position ~d: ~
                  Identifiers must not commence with minuscules."
            $character $position))
        (T
          (error "Invalid character \"~c\" at position ~d."
            $character $position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of AST nodes.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface encapsulates the concept of an abstract
   syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include AST-Node))
  "The ``Block-Node'' class encapsulates an ordered list of zero or more
   abstract syntax tree (AST) child nodes which form a logical
   compound."
  (statements (error "Missing statements.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Print-Node
  (:include AST-Node))
  "The ``Print-Node'' class encapsulates a print operation in the form
   of an abstract syntax tree (AST) node."
  (argument (error "Missing print argument.")
            :type      AST-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (String-Node
  (:include AST-Node))
  "The ``String-Node'' class encapsulates a string literal in the form
   of an abstract syntax tree (AST) node."
  (value (error "Missing string value.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Integer-Node
  (:include AST-Node))
  "The ``Integer-Node'' class encapsulates a string literal in the form
   of an abstract syntax tree (AST) node."
  (value (error "Missing integer value.")
         :type      non-negative-integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include AST-Node))
  "The ``Input-Node'' class encapsulates an input operation in the form
   of an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include AST-Node))
  "The ``Binary-Operation-Node'' class encapsulates a binary operation,
   either of the arithmetic or logical species, in the form of an
   abstract syntax tree (AST) node."
  (operator      (error "Missing binary operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      AST-Node
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      AST-Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (If-Node
  (:include AST-Node))
  "The ``If-Node'' class encapsulates the notion of a conditional
   \"If\" construct in the form of an abstract syntax tree (AST) node."
  (antecedent (error "Missing antecedent.")
              :type      AST-Node
              :read-only T)
  (then-body  (error "Missing then body.")
              :type      Block-Node
              :read-only T)
  (else-body  (error "Missing else body.")
              :type      (or null Block-Node)
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Counting-Loop-Node
  (:include AST-Node))
  "The ``Counting-Loop'' class encapsulates the notion of a counting
   iteration \"Loop\" whose cycles are predefined by adminiculum of a
   numeric iterance tally."
  (repetitions (error "Missing repetitions.")
               :type      AST-Node
               :read-only T)
  (body        (error "Missing body.")
               :type      Block-Node
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Infinite-Loop-Node
  (:include AST-Node))
  "The ``Infinite-Loop'' class encapsulates the notion of a perpetually
   operating \"Forever\" iterance construct."
  (body (error "Missing body.")
        :type      Block-Node
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Stop-Node
  (:include AST-Node))
  "The ``Stop-Node'' class encapsulates the \"Stop\" instruction as a
   behest airted towards a JustWords program's immediate termination.")

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include AST-Node))
  "The ``Program-Node'' class encapsulates a JustWords program in the
   form of an abstract syntax tree (AST), furnishing in this entity the
   root node."
  (statements (error "Missing statements.")
              :type      Block-Node
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token stream.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token-Stream
  (:constructor make-token-stream
    (lexer &aux (current-token (get-next-token lexer)))))
  "The ``Token-Stream'' class appropriates the dever of providing access
   to a lexer's token in an abstract fashion."
  (lexer         (error "Missing lexer.")
                 :type      Lexer
                 :read-only T)
  (current-token (make-eof-token)
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(defun peek-token (token-stream)
  "Returns without its consumption the next token from the
   TOKEN-STREAM."
  (declare (type Token-Stream token-stream))
  (the Token
    (token-stream-current-token token-stream)))

;;; -------------------------------------------------------

(defun consume-token (token-stream)
  "Consumes and returns the next token from the TOKEN-STREAM."
  (declare (type Token-Stream token-stream))
  (the Token
    (prog1
      (token-stream-current-token token-stream)
      (setf (token-stream-current-token token-stream)
        (get-next-token
          (token-stream-lexer token-stream))))))

;;; -------------------------------------------------------

(defun token-follows-p (token-stream expected-token-type)
  "Determines whether the next token in the TOKEN-STREAM conforms to the
   TOKEN-TYPE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token-stream token-stream))
  (declare (type keyword      expected-token-type))
  (the boolean
    (token-type-p (peek-token token-stream) expected-token-type)))

;; --------------------------------------------------------

(defun expect-token (token-stream expected-token-type)
  "Determines whether the next token in the TOKEN-STREAM conforms to the
   TOKEN-TYPE, on confirmation consuming and returning the probed
   object, otherwise signaling an error of an unspecified type."
  (declare (type Token-stream token-stream))
  (declare (type keyword      expected-token-type))
  (the Token
    (if (token-type-p (peek-token token-stream) expected-token-type)
      (consume-token token-stream)
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (peek-token token-stream)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of precedence.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence         (binding-power associativity))
  (:constructor make-neutral-precedence (&aux (binding-power 0)
                                              (associativity :none))))
  "The ``Precedence'' class' bailiwick embraces the encapsulation and
   coefficiency of a numeric binding power and an associativity rule,
   their champarty being the claviger to the Pratt parser's triage
   algorithm."
  (binding-power (error "Missing binding power.")
                 :type      integer
                 :read-only T)
  (associativity (error "Missing associativity.")
                 :type      associativity
                 :read-only T))

;;; -------------------------------------------------------

(defun get-effective-binding-power (precedence)
  "Returns the PRECEDENCE's effective binding power, the same derives
   from the actual binding power in dependence upon the involved
   associativity."
  (declare (type Precedence precedence))
  (the integer
    (case (precedence-associativity precedence)
      ((:none :left-to-right)
        (precedence-binding-power precedence))
      (:right-to-left
        (1- (precedence-binding-power precedence)))
      (otherwise
        (error "Unrecognized associativity: ~s."
          (precedence-associativity precedence))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Parselet".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Parselet
  "The ``Parselet'' interface establishes a common foundry for all
   classes nuncupated to the provision of a parselet's manifestation.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Nud-Parselet".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Nud-Parselet
  (:include Parselet))
  "The ``Nud-Parselet'' class encapsulates the notion of a
   \"null denotation\", or \"nud\", parselet, the same relies on no
   left expression, imposing as a unified requisite chiefly the
   currently processed token, but might, in some cases, appropriate a
   dextral operand for its purposes."
  (precedence (make-neutral-precedence)
              :type      Precedence
              :read-only T)
  (processor  (error "Missing nud parselet processor.")
              :type      nud-processor
              :read-only T))

;;; -------------------------------------------------------

(defun apply-nud-parselet (parselet token-stream nud-token)
  "Applies the nud PARSELET to the NUD-TOKEN, granted adit to the
   TOKEN-STREAM in order to preserve the contingency for a dextral
   expression's obtention, and returns an abstract syntax tree (AST)
   node representation of the thus yielded expression."
  (declare (type Nud-Parselet parselet))
  (declare (type Token-Stream token-stream))
  (declare (type Token        nud-token))
  (the AST-Node
    (funcall
      (nud-parselet-processor parselet)
      nud-token
      token-stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Led-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Led-Parselet
  (:include Parselet))
  "The ``Led-Parselet'' class encapsulates the notion of a
   \"left denotation\", or \"led\", parselet, the same depends for its
   operative conclusion upon at least the extant sinistral expression
   and its currently processed token, in some cases extending the
   requisitum to a dextral operand."
  (precedence (error "Missing led parselet precedence.")
              :type      Precedence
              :read-only T)
  (processor  (error "Missing led parselet processor.")
              :type      led-processor
              :read-only T))

;;; -------------------------------------------------------

(defun apply-led-parselet (parselet token-stream left-node led-token)
  "Applies the led PARSELET to the LED-TOKEN, incorporating the
   LEFT-NODE as the sinistral operand, and granting adit to the
   TOKEN-STREAM in order to preserve the contingecy for the dextral
   expression's obtention, and returns an abstract syntax tree (AST)
   node representation of the thus yielded expression."
  (declare (type Led-Parselet parselet))
  (declare (type Token-Stream token-stream))
  (declare (type AST-Node     left-node))
  (declare (type Token        led-token))
  (the AST-Node
    (funcall
      (led-parselet-processor parselet)
      left-node
      led-token
      token-stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Std-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Std-Parselet
  (:include Parselet))
  "The ``Std-Parselet'' class implements a \"statement denotation\", or,
   abbreviated, \"std\" parselet, nuncupated to the parsing of a
   statement token and the subsequent generation of an abstract syntax
   tree (AST) encapsulation of its requisite information."
  (processor (error "Missing std processor.")
             :type      std-processor
             :read-only T))

;;; -------------------------------------------------------

(defun apply-std-parselet (parselet token-stream std-token)
  "Processes the STD-TOKEN in the std PARSELET's context, accommodating
   for the contingency of subsequent tokens' involvement the
   TOKEN-STREAM, and returns an abstract syntax tree (AST) node
   representation of the thus produced parse result."
  (declare (type Std-Parselet parselet))
  (declare (type Token-Stream token-stream))
  (declare (type Token        std-token))
  (the AST-Node
    (funcall
      (std-parselet-processor parselet)
      std-token
      token-stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of parselet registry.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim
  (ftype (function (Token-Stream integer) AST-Node) parse-expression)
  (ftype (function (Token)                integer)  get-led-binding-power))

;;; -------------------------------------------------------

(declaim
  (type (hash-table-of keyword Nud-Parselet) +NUD-PARSELETS+)
  (type (hash-table-of keyword Led-Parselet) +LED-PARSELETS+)
  (type (hash-table-of keyword Std-Parselet) +STD-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +NUD-PARSELETS+
  (make-hash-table :test #'eq)
  "Maps the nud token types to the nud parselets nuncupated to their
   parsing.")

(defparameter +LED-PARSELETS+
  (make-hash-table :test #'eq)
  "Maps the led token types to the led parselets nuncupated to their
   parsing.")

(defparameter +STD-PARSELETS+
  (make-hash-table :test #'eq)
  "Maps the std token types to the std parselets nuncupated to their
   parsing.")

;;; -------------------------------------------------------

(defun register-nud-parselet (token-type parselet)
  "Associates the nud PARSELET with the TOKEN-TYPE in the
   +NUD-PARSELETS+ table, superseding an extant entry with the
   TOKEN-TYPE as its key, and returns no value."
  (declare (type keyword      token-type))
  (declare (type Nud-Parselet parselet))
  (setf (gethash token-type +NUD-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defmacro define-nud-parselet
    (token-type
     (token-variable token-stream-variable)
     (&body body)
     &optional (precedence (make-neutral-precedence)))
  "Offers a commodity for the registration of a nud parselet by
   generating a ``Nud-Parselet'' instance whose processor's lambda list
   accepts the TOKE-VARIABLE and TOKEN-STREAM-VARIABLE for its formal
   parameter names, being implemented using the BODY forms, the desinent
   form's primary value must return an ``AST-Node'' object, while the
   binding power and associativity are specified via the PRECEDENCE,
   registers this parselet as a nud token of the TOKEN-TYPE, and returns
   no value."
  `(register-nud-parselet ,token-type
     (make-nud-parselet
       :processor
         #'(lambda (,token-variable ,token-stream-variable)
             (declare (type Token        ,token-variable))
             (declare (ignorable         ,token-variable))
             (declare (type Token-Stream ,token-stream-variable))
             (declare (ignorable         ,token-stream-variable))
             (the AST-Node
               (progn ,@body)))
       :precedence ,precedence)))

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
  "Returns the nud parselet associated with the nud TOKEN."
  (declare (type Token token))
  (the Nud-Parselet
    (or
      (nth-value 0
        (gethash (token-type token) +NUD-PARSELETS+))
      (error "No nud token: ~s." token))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (token)
  "Returns the binding power associated with the nud TOKEN."
  (declare (type Token token))
  (the integer
    (get-effective-binding-power
      (nud-parselet-precedence
        (get-nud-parselet token)))))

;;; -------------------------------------------------------

(defun parse-nud-token (token token-stream)
  "Parses the nud TOKEN employing the TOKEN-STREAM for contingent
   subsequent token apprehension and returns an abstract syntax tree
   (AST) node representation of the thus produced expression."
  (declare (type Token        token))
  (declare (type Token-Stream token-stream))
  (the AST-Node
    (apply-nud-parselet
      (get-nud-parselet token)
      token-stream
      token)))

;;; -------------------------------------------------------

(defun register-led-parselet (token-type parselet)
  "Associates the led PARSELET with the TOKEN-TYPE in the
   +LED-PARSELETS+ table, superseding an extant entry with the
   TOKEN-TYPE as its key, and returns no value."
  (declare (type keyword      token-type))
  (declare (type led-Parselet parselet))
  (setf (gethash token-type +LED-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defmacro define-led-parselet
    (token-type
     (left-expression-variable token-variable token-stream-variable)
     (&body body)
     precedence)
  "Furnishes a commodity for the eath creation and registration of a led
   parselet by generating an instance of the ``Led-Parselet'' whose
   processor function accepts as its three formal parameters, in this
   exact order, the LEFT-EXPRESSION-VARIABLE, TOKEN-VARIABLE, and
   TOKEN-STREAM-VARIABLE, the effects being implemented by the BODY
   forms, encapsulated in an implicit ``progn'' form, with the desinent
   BODY form's primary value expected to return an ``AST-Node'' object,
   registers this parselet with the TOKEN-TYPE in the +LED-PARSELETS+
   table, and returns no value."
  `(register-led-parselet ,token-type
     (make-led-parselet
       :processor
         #'(lambda (,left-expression-variable
                    ,token-variable
                    ,token-stream-variable)
             (declare (type AST-Node     ,left-expression-variable))
             (declare (ignorable         ,left-expression-variable))
             (declare (type Token        ,token-variable))
             (declare (ignorable         ,token-variable))
             (declare (type Token-Stream ,token-stream-variable))
             (declare (ignorable         ,token-stream-variable))
             (the AST-Node
               (progn ,@body)))
       :precedence ,precedence)))

;;; -------------------------------------------------------

(defun register-binary-operation (token-type
                                  binding-power
                                  associativity)
  "Furnishes a more convenient avenue for the registration of a binary
   operation as a specialized led parselet species, the association of
   which ensues from the TOKEN-TYPE, the same concomitantly defines the
   generated ``Binary-Operation-Node'''s binary operator, consuming the
   dextral operand via the PRECEDENCE, which is specified by mediation
   of the BINDING-POWER and ASSOCIATIVITY information, and returns no
   value."
  (define-led-parselet token-type (left-node token token-stream)
    ((make-binary-operation-node
       :operator      token-type
       :left-operand  left-node
       :right-operand (parse-expression token-stream
                        (get-led-binding-power token))))
    (make-precedence binding-power associativity)))

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
  "Returns the led parselet associated with the led TOKEN."
  (declare (type Token token))
  (the Led-Parselet
    (or
      (nth-value 0
        (gethash (token-type token) +LED-PARSELETS+))
      (error "No led token: ~s." token))))

;;; -------------------------------------------------------

(defun get-led-binding-power (token)
  "Returns the binding power associated with the led TOKEN."
  (declare (type Token token))
  (the integer
    (get-effective-binding-power
      (led-parselet-precedence
        (get-led-parselet token)))))

;;; -------------------------------------------------------

(defun parse-led-token (token token-stream left-node)
  "Parses the led TOKEN employing the LEFT-NODE as the sinistral operand
   and the TOKEN-STREAM for contingent subsequent token apprehension and
   returns an abstract syntax tree (AST) node representation of the thus
   produced expression."
  (declare (type Token        token))
  (declare (type Token-Stream token-stream))
  (declare (type AST-Node     left-node))
  (the AST-Node
    (apply-led-parselet
      (get-led-parselet token)
      token-stream
      left-node
      token)))

;;; -------------------------------------------------------

(defun register-std-parselet (token-type parselet)
  "Registers the std PARSELET with the TOKEN-TYPE at the +STD-PARSELETS+
   table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Std-Parselet parselet))
  (setf (gethash token-type +STD-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defmacro define-std-parselet (token-type
                               (token-variable token-stream-variable)
                               &body body)
  "Furnishes a commodity for an eath definition of an std parselet and
   its registration under the TOKEN-TYPE by generating a new
   ``Std-Parselet'' instance with the processor function's formal
   arguments derived, in this order, from the TOKEN-VARIABLE and the
   TOKEN-STREAM-VARIABLE names, the implementation being realized via
   the BODY forms, ensconced in an implicit ``progn'', the desinent
   form's primary value expected to produce an ``AST-Node'' instance,
   registers this parselet at the +STD-PARSELETS+ table, and returns no
   value."
  `(register-std-parselet ,token-type
     (make-std-parselet :processor
       #'(lambda (,token-variable ,token-stream-variable)
           (declare (type Token        ,token-variable))
           (declare (ignorable         ,token-variable))
           (declare (type Token-Stream ,token-stream-variable))
           (declare (ignorable         ,token-stream-variable))
           (the AST-Node
             (progn ,@body))))))

;;; -------------------------------------------------------

(defun std-token-p (token)
  "Determines whether the TOKEN represents a \"statement denotation\",
   or \"std\", token, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token) +STD-PARSELETS+)))))

;;; -------------------------------------------------------

(defun get-std-parselet (token)
  "Returns the std parselet associated with the TOKEN in the
   +STD-PARSELETS+ table, or signals an error of an unspecified type
   upon its disrespondency."
  (declare (type Token token))
  (the Std-Parselet
    (or
      (nth-value 0
        (gethash (token-type token) +STD-PARSELETS+))
      (error "No std token: ~s." token))))

;;; -------------------------------------------------------

(defun parse-std-token (token token-stream)
  "Parses the std TOKEN employing the TOKEN-STREAM for contingent
   subsequent token apprehension and returns an abstract syntax tree
   (AST) node representation of the thus produced statement."
  (declare (type Token-Stream token-stream))
  (the AST-Node
    (apply-std-parselet
      (get-std-parselet token)
      token-stream
      token)))

;;; -------------------------------------------------------

(defun parse-expression (token-stream current-binding-power)
  "Parses an expression in concord with the Pratt parsing notion by
   operating upon the TOKEN-STREAM, the caller being represented by its
   CURRENT-BINDING-POWER, and returns an abstract syntax tree (AST) node
   representation of the thus generated expression."
  (declare (type Token-Stream token-stream))
  (declare (type integer      current-binding-power))
  
  (let ((nud-token (consume-token token-stream))
        (left-node NIL))
    (declare (type Token              nud-token))
    (declare (type (or null AST-Node) left-node))
    
    (if (nud-token-p nud-token)
      (setf left-node
        (parse-nud-token nud-token token-stream))
      (error "The token ~s is no nud token." nud-token))
    
    (loop for next-token of-type Token = (peek-token token-stream) do
      (cond
        ((token-type-p next-token :eof)
          (loop-finish))
        ((not (led-token-p next-token))
          (loop-finish))
        ((<= (get-led-binding-power next-token)
             current-binding-power)
          (loop-finish))
        (T
          (consume-token token-stream)
          (setf left-node
            (parse-led-token next-token token-stream left-node)))))
    
    (the AST-Node left-node)))

;;; -------------------------------------------------------

(defun parse-statement (token-stream)
  "Parses a statement utilizing the TOKEN-STREAM, with the next token
   therein expected to be of an eligible type."
  (declare (type Token-Stream token-stream))
  (the AST-Node
    (parse-std-token
      (consume-token token-stream)
      token-stream)))

;;; -------------------------------------------------------

(defun parse-block-node (token-stream)
  "Proceeding from the current state of the TOKEN-STREAM, parses a
   sequence amplecting zero or more statements and returns a
   ``Block-Node'' representation thereof."
  (declare (type Token-Stream token-stream))
  (symbol-macrolet ((current-token (peek-token token-stream)))
    (declare (type Token current-token))
    (the Block-Node
      (make-block-node :statements
        (loop
          with done-p of-type boolean = NIL
          until done-p
            when (token-follows-p token-stream :whitespaces) do
              (consume-token token-stream)
            
            if (std-token-p current-token) collect
              (prog1
                (parse-statement token-stream)
                (setf done-p
                  (not (token-follows-p token-stream :whitespaces))))
            else if (led-token-p current-token) collect
              (prog1
                (parse-nud-token current-token token-stream)
                (setf done-p
                  (not (token-follows-p token-stream :whitespaces))))
            else do
              (setf done-p T))))))

;;; -------------------------------------------------------

(defun collect-literal-content (token-stream destination)
  "Proceeding from the current state of the TOKEN-STREAM, consumes a
   sequence of zero or more non-keyword tokens, prints these to the
   DESTINATION, and returns no value."
  (declare (type Token-Stream token-stream))
  (declare (type destination  destination))
  (loop
    while
      (or (token-follows-p token-stream :identifier)
          (token-follows-p token-stream :number))
    do
      (format destination "~a"
        (token-value
          (consume-token token-stream))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-nud-parselet :number (token token-stream)
  ((let ((parsed-content
          (parse-character-sequence
            (with-output-to-string (content)
              (declare (type string-stream content))
              (format content "~d" (token-value token))
              (collect-literal-content token-stream content)))))
    (declare (type (or integer string) parsed-content))
    (typecase parsed-content
      (integer   (make-integer-node :value parsed-content))
      (string    (make-string-node  :value parsed-content))
      (otherwise (error "Invalid literal object: ~s."
                   parsed-content))))))

;;; -------------------------------------------------------

(define-nud-parselet :input (token token-stream)
  ((make-input-node)))

;;; -------------------------------------------------------

(define-nud-parselet :identifier (token token-stream)
  ((make-string-node :value
    (with-output-to-string (content)
      (declare (type string-stream content))
      (format content "~a" (token-value token))
      (collect-literal-content token-stream content)))))

;;; -------------------------------------------------------

(register-binary-operation :equals    90 :left-to-right)
(register-binary-operation :less     100 :left-to-right)
(register-binary-operation :greater  100 :left-to-right)
(register-binary-operation :plus     130 :left-to-right)
(register-binary-operation :minus    130 :left-to-right)
(register-binary-operation :multiply 140 :left-to-right)
(register-binary-operation :divide   140 :left-to-right)

;;; -------------------------------------------------------

(define-std-parselet :print (token token-stream)
  (make-print-node :argument
    (parse-expression token-stream 0)))

;;; -------------------------------------------------------

(define-std-parselet :if (token token-stream)
  (let ((antecedent (parse-expression token-stream 0))
        (then-body  NIL)
        (else-body  NIL))
    (declare (type AST-Node           antecedent))
    (declare (type (or null AST-Node) then-body))
    (declare (type (or null AST-Node) else-body))
    
    (expect-token token-stream :then)
    (expect-token token-stream :whitespaces)
    
    (setf then-body
      (parse-block-node token-stream))
    
    (when (token-follows-p token-stream :else)
      (expect-token token-stream :else)
      (expect-token token-stream :whitespaces)
      (setf else-body
        (parse-block-node token-stream)))
    
    (expect-token token-stream :end)
    
    (the If-Node
      (make-if-node
        :antecedent antecedent
        :then-body  then-body
        :else-body  else-body))))

;;; -------------------------------------------------------

(define-std-parselet :loop (token token-stream)
  (let ((repetitions (parse-expression token-stream 0)))
    (declare (type AST-Node repetitions))
    (expect-token token-stream :whitespaces)
    (the Counting-Loop-Node
      (make-counting-loop-node
        :repetitions repetitions
        :body        (prog1
                       (parse-block-node token-stream)
                       (expect-token     token-stream :end))))))

;;; -------------------------------------------------------

(define-std-parselet :forever (token token-stream)
  (expect-token token-stream :whitespaces)
  (the Infinite-Loop-Node
    (make-infinite-loop-node :body
      (prog1
        (parse-block-node token-stream)
        (expect-token     token-stream :end)))))

;;; -------------------------------------------------------

(define-std-parselet :stop (token token-stream)
  (the Stop-Node
    (make-stop-node)))

;;; -------------------------------------------------------

(defun parse-program (token-stream)
  "Parses a JustWords program, distributed across the tokens in the
   TOKEN-STREAM, and returns a ``Program-Node'' representation thereof."
  (declare (type Token-Stream token-stream))
  (the Program-Node
    (make-program-node :statements
      (prog1
        (parse-block-node token-stream)
        (expect-token     token-stream :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of JWObjects.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (JWObject)
  "The ``JWObject'' interface accoutres a common foundry for all objects
   pursuing the representation of data types in accordance with and
   delineated by the JustWords specification's marches.")

;;; -------------------------------------------------------

(defstruct (JWBoolean
  (:include     JWObject)
  (:constructor make-jwboolean (value)))
  "The ``JWBoolean'' class encapsulates the notion of a Boolean value in
   the context of the JustWords language specification."
  (value (error "Missing Boolean value.")
         :type      boolean
         :read-only T))

;;; -------------------------------------------------------

(defstruct (JWInteger
  (:include     JWObject)
  (:constructor make-jwinteger (value)))
  "The ``JWInteger'' class encapsulates the notion of an integer number
   in the context of the JustWords language specification."
  (value (error "Missing integer value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (JWString
  (:include     JWObject)
  (:constructor make-jwstring (value)))
  "The ``JWString'' class encapsulates the notion of a string object in
   the context of the JustWords language specification."
  (value (error "Missing string value.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defgeneric get-jwobject-value (jwobject)
  (:documentation
    "Returns the value stored in the JWOBJECT.")
  
  (:method ((jwboolean JWBoolean))
    (declare (type JWBoolean jwboolean))
    (the boolean (jwboolean-value jwboolean)))
  
  (:method ((jwinteger JWInteger))
    (declare (type JWInteger jwinteger))
    (the integer (jwinteger-value jwinteger)))
  
  (:method ((jwstring JWString))
    (declare (type JWString jwstring))
    (the string (jwstring-value jwstring))))

;;; -------------------------------------------------------

(defgeneric jwobject-true-p (jwobject)
  (:documentation
    "Determines whether the JWOBJECT resolves to a Boolean truth value,
     returning on confirmation the respective value, otherwise signals
     an error of an unspecified type.")
  
  (:method ((jwboolean JWBoolean))
    (declare (type JWBoolean jwboolean))
    (the boolean
      (get-jwobject-value jwboolean)))
  
  (:method ((jwinteger JWInteger))
    (declare (type JWInteger jwinteger))
    (error "The integer object ~s cannot be utilized as ~
            Boolean truth values."
      jwinteger))
  
  (:method ((jwstring JWString))
    (declare (type JWString jwstring))
    (error "The string object ~s cannot be utilized as a ~
            Boolean truth value."
      jwstring)))

;;; -------------------------------------------------------

(defgeneric print-jwobject (jwobject destination)
  (:documentation
    "Prints the JWOBJECT to the DESTINATION and returns no value.")
  
  (:method ((jwboolean JWBoolean) (destination T))
    (declare (type JWBoolean   jwboolean))
    (declare (type destination destination))
    (format destination "~:[false~;true~]"
      (get-jwobject-value jwboolean))
    (values))
  
  (:method ((jwinteger JWInteger) (destination T))
    (declare (type JWInteger   jwinteger))
    (declare (type destination destination))
    (format destination "~d"
      (get-jwobject-value jwinteger))
    (values))
  
  (:method ((jwstring JWString) (destination T))
    (declare (type JWString    jwstring))
    (declare (type destination destination))
    (format destination "~a"
      (get-jwobject-value jwstring))
    (values)))

;;; -------------------------------------------------------

(defmethod print-object ((jwobject JWObject) (stream T))
  (declare (type JWObject    jwobject))
  (declare (type destination stream))
  (print-jwobject jwobject stream)
  (values))

;;; -------------------------------------------------------

(defun parse-jwobject (input)
  "Evaluates the INPUT and returns a covenable ``JWObject''
   representation of its analyzed content."
  (declare (type string input))
  (the JWObject
    (handler-case
      (make-jwinteger
        (parse-integer input))
      (error ()
        (make-jwstring input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Stop-Condition (condition)
  ()
  (:documentation
    "The ``Stop-Condition'' serves to signal an interrupt akin to an
     optative expressing the desire to terminate the program
     immediately."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and the
     RIGHT-OPERAND in this exact order and returns a result conable for
     this combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operation
    (operator (left-operand-class right-operand-class result-type)
     &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operator'', employing as the first argument, which
   ``eql''-dispatches on the OPERATOR type, an automatically norned
   identifier, establishing as the second operand a binding with the
   agnomination ``$left-operand'' and the type LEFT-OPERAND-CLASS, as
   the third input a ``$right-operand'' specialized on the
   RIGHT-OPERAND-CLASS, evaluting the BODY forms in an implicitly
   ensconcing ``progn'' form, and returning the desinent BODY form's
   results, the same ought to be compatible with the RESULT-TYPE for
   which a ``the'' special form invocation applies.
   ---
   If the first BODY form represents a string, the same is construed as
   the method's documentation string and reappropriated exclusively for
   this purpose."
  (let ((operator-variable (gensym)))
    (declare (type symbol operator-variable))
    `(defmethod apply-binary-operator
         ((,operator-variable (eql ,operator))
          ($left-operand      ,left-operand-class)
          ($right-operand     ,right-operand-class))
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Defines the binary operation ~s for a left ~
                       operand of the type ~s and a right operand ~
                       being subsumed into the species ~s, and returns ~
                       a value of the type ~s."
            operator left-operand-class right-operand-class
            result-type))
       (declare (type binary-operator      ,operator-variable))
       (declare (ignore                    ,operator-variable))
       (declare (type ,left-operand-class  $left-operand))
       (declare (ignorable                 $left-operand))
       (declare (type ,right-operand-class $right-operand))
       (declare (ignorable                 $right-operand))
       (the ,result-type
         (progn ,@body)))))

;;; -------------------------------------------------------

(define-binary-operation :plus (JWInteger JWInteger JWInteger)
  "Returns the sum of the $LEFT-OPERAND incremented by the
   $RIGHT-OPERAND."
  (make-jwinteger
    (+ (get-jwobject-value $left-operand)
       (get-jwobject-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :minus (JWInteger JWInteger JWInteger)
  "Returns the difference yielded by subtracting the $RIGHT-OPERAND from
   the $LEFT-OPERAND."
  (make-jwinteger
    (- (get-jwobject-value $left-operand)
       (get-jwobject-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :multiply (JWInteger JWInteger JWInteger)
  "Returns the product yielded by multiplying the $LEFT-OPERAND by the
   $RIGHT-OPERAND."
  (make-jwinteger
    (* (get-jwobject-value $left-operand)
       (get-jwobject-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :divide (JWInteger JWInteger JWInteger)
  "Returns the integer quotient of the $LEFT-OPERAND's division by the
   $RIGHT-OPERAND with subsequent rounding towards the nearest integral
   point."
  (make-jwinteger
    (nth-value 0
      (round (get-jwobject-value $left-operand)
             (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :equals (JWBoolean JWBoolean JWBoolean)
  "Determines whether the $LEFT-OPERAND represents the paregal Boolean
   truth value as the $RIGHT-OPERAND, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (eq (get-jwobject-value $left-operand)
          (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :equals (JWInteger JWInteger JWBoolean)
  "Determines whether the $LEFT-OPERAND equals the $RIGHT-OPERAND,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (= (get-jwobject-value $left-operand)
         (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :equals (JWString JWString JWBoolean)
  "Determines whether the $LEFT-OPERAND equals the $RIGHT-OPERAND,
   according to the lexicographic equiparation principle, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (string= (get-jwobject-value $left-operand)
               (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :equals (T T JWBoolean)
  "Refutes the equality of the $LEFT-OPERAND and the $RIGHT-OPERAND by
   returning a ``boolean'' value of ``NIL''."
  (make-jwboolean NIL))

;;; -------------------------------------------------------

(define-binary-operation :less (JWInteger JWInteger JWBoolean)
  "Determines whether the $LEFT-OPERAND is less than the $RIGHT-OPERAND,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (< (get-jwobject-value $left-operand)
         (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :less (JWString JWString JWBoolean)
  "Determines whether the $LEFT-OPERAND is less than the $RIGHT-OPERAND,
   according to the lexicographic equiparation principle, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (string< (get-jwobject-value $left-operand)
               (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :greater (JWInteger JWInteger JWBoolean)
  "Determines whether the $LEFT-OPERAND is greater than the
   $RIGHT-OPERAND, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (> (get-jwobject-value $left-operand)
         (get-jwobject-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :greater (JWString JWString JWBoolean)
  "Determines whether the $LEFT-OPERAND is greater than the
   $RIGHT-OPERAND, according to the lexicographic equiparation
   principle, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (make-jwboolean
    (get-boolean-value-of
      (string> (get-jwobject-value $left-operand)
               (get-jwobject-value $right-operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree (AST).")
    :reader        get-tree
    :type          Program-Node
    :documentation "The JustWords program's abstract syntax tree (AST)
                    mold, intended for evaluation."))
  (:documentation
    "The ``Interpreter'' class establishes that entity nuncupated to the
     dever of accompassing operative valor to a JustWords program
     formulated as an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a new ``Interpreter'' which operates on the
   JustWords program's abstract syntax TREE (AST) representation."
  (declare (type Program-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Visits the abstract syntax tree (AST) NODE in the INTERPRETER's
     context and returns a value connable for this combination."))

;;; -------------------------------------------------------

(defmacro define-node-dispatch (node-class
                                (&optional
                                  (interpreter-variable '$interpreter)
                                  (node-variable        '$node))
                                &body body)
  "Defines an implementation of the generic function ``visit-node'',
   assembled utilizing the INTERPRETER-VARIABLE as an agnomination for
   the first formal parameter and the NODE-VARIABLE supplying the
   second, the latter dispatching on type conformance with the
   specified NODE-CLASS, the effects being contributed by the BODY
   forms, the desinent among which is responds with its return values.
   ---
   If not supplied, the INTERPRETER-VARIABLE defaults to the symbol
   ``$interpreter'', while the NODE-VARIABLE assumes ``$node''.
   ---
   If the BODY form's first element constitutes a string, the same is
   construed as a documentation string and reappropriated for this
   purpose."
  `(defmethod visit-node ((,interpreter-variable Interpreter)
                          (,node-variable        ,node-class))
     ,(if (stringp (first body))
        (pop body)
        (format NIL "Visits the ~s instance, stevened ~s, in the
                     context of the interpreter ~s and returns a ~
                     covenable result."
          node-class node-variable interpreter-variable))
     (declare (type Interpreter ,interpreter-variable))
     (declare (ignorable        ,interpreter-variable))
     (declare (type ,node-class ,node-variable))
     (declare (ignorable        ,node-variable))
     ,@body))

;;; -------------------------------------------------------

(define-node-dispatch Program-Node (interpreter node)
  "Executes the program $NODE's statements in their specified order and
   returns no value."
  (handler-case
    (visit-node interpreter
      (program-node-statements node))
    (Stop-Condition () NIL))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch Block-Node (interpreter node)
  "Executes the block $NODE's statements in their specified order and
   returns no value."
  (dolist (statement (block-node-statements node))
    (declare (type AST-Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch Print-Node ()
  "Evaluates the print $NODE's argument in the $INTERPRETER's context,
   prints the same to the standard output, and returns no value."
  (format T "~&~a"
    (visit-node $interpreter
      (print-node-argument $node)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch Input-Node ()
  "Queries the standard input for a line of input and returns a
   ``JWObject'' encapsulation of its evaluated content."
  (format T "~&>> ")
  (finish-output)
  (prog1
    (parse-jwobject
      (read-line NIL NIL ""))
    (clear-input)))

;;; -------------------------------------------------------

(define-node-dispatch If-Node ()
  "Perquires the \"If\" $NODE's antecedent, upon its fulfilment
   executing the \"Then\" statements, otherwise, if specified, the
   \"Else\" counterparts, and returns no value."
  (let ((antecedent (if-node-antecedent $node))
        (then-body  (if-node-then-body  $node))
        (else-body  (if-node-else-body  $node)))
    (declare (type AST-Node           antecedent))
    (declare (type AST-Node           then-body))
    (declare (ignorable               then-body))
    (declare (type (or null AST-Node) else-body))
    (declare (ignorable               else-body))
    (cond
      ((jwobject-true-p (visit-node $interpreter antecedent))
        (visit-node $interpreter then-body))
      (else-body
        (visit-node $interpreter else-body))
      (T
        NIL)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch Counting-Loop-Node ()
  "Executes the counting loop $NODE's statements a tally of cycles
   tantamount to the encapsulates specification and returns no value."
  (let ((repetitions
          (visit-node $interpreter
            (counting-loop-node-repetitions $node))))
    (declare (type JWInteger repetitions))
    (loop repeat (get-jwobject-value repetitions) do
      (visit-node $interpreter
        (counting-loop-node-body $node))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch Infinite-Loop-Node ()
  "Executes the infinite loop $NODE's statements perpetually and returns
   no value."
  (loop do
    (visit-node $interpreter
      (infinite-loop-node-body $node)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch Binary-Operation-Node ()
  "Applies the binary operation $NODE's operator to the left and the
   right operand in this exact order and returns the thus produced
   Boolean or integral value."
  (the JWObject
    (apply-binary-operator
      (binary-operation-node-operator $node)
      (visit-node $interpreter
        (binary-operation-node-left-operand $node))
      (visit-node $interpreter
        (binary-operation-node-right-operand $node)))))

;;; -------------------------------------------------------

(define-node-dispatch Stop-Node ()
  "Signals a ``Stop-Condition'', ignoring both the $INTERPRETER and the
   $NODE."
  (signal 'Stop-Condition))

;;; -------------------------------------------------------

(define-node-dispatch String-Node ()
  "Returns the string value stored in the string $NODE."
  (the JWString
    (make-jwstring
      (string-node-value $node))))

;;; -------------------------------------------------------

(define-node-dispatch Integer-Node ()
  "Returns the non-negative integer value stored in the integer $NODE."
  (the JWInteger
    (make-jwinteger
      (integer-node-value $node))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Evaluates the abstract syntax tree (AST) representation of the parsed
   JustWords governed by the INTERPRETER's castaldy and returns no
   value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (get-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-JustWords (code)
  "Interprets the piece of JustWords source CODE and returns no value."
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

;; Print "HelloWorld".
(interpret-JustWords "PrintHelloWorld")

;;; -------------------------------------------------------

;; Infinite line-based cat program.
(interpret-JustWords
  "Forever
     PrintInput
   End")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; This solution queries the user twice.
(interpret-JustWords
  "IfInputEqualsZeroThen
     PrintZero
   Stop
   End
   IfInputEqualsOneThen
     Forever
       PrintOne
     End
   End")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; This solution queries the user once.
(interpret-JustWords
  "
  IfInputGreaterZeroThen
    Forever
      PrintOne
    End
  Else
    PrintZero
  End
  ")

;;; -------------------------------------------------------

;; Print the text "Ha" a tally of times tantamount to the user input.
(interpret-JustWords
  "LoopInput
     PrintHa
   End")

;;; -------------------------------------------------------

;; Fixed Repeating Output: Print the number one (1) a user-specified
;; tally of times, succeeded by an aefauld instance of zero (0).
(interpret-JustWords
  "LoopInput
     PrintOne
   End
   PrintZero")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-JustWords
  "Forever
   End")

;;; -------------------------------------------------------

;; Simulate a number guessing game which repeatedly queries for an
;; integral number in the closed interval [1, 20], until the correct
;; response, five (5), is supplied.
(interpret-JustWords
  "PrintPleaseGuessANumberBetwixtOneAndTwoZero
   Forever
     IfInputEqualsFiveThen
       PrintYouHaveGuessedTheNumberFiveCorrectly
       Stop
     Else
       PrintYourGuessWasWrong
       PrintPleaseTryAgain
     End
   End")

;;; -------------------------------------------------------

;; Query the user for an integer number, halve, round, and print the
;; value.
(interpret-JustWords
  "PrintPleaseSupplyANumberToHalve
   PrintInputDivideTwo")
