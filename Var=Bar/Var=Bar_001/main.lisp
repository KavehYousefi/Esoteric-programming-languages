;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Var=Bar", designed by the Esolang user "A" in the year
;; 2018, and based upon the assignment to variables as the exclusive
;; warklume for the accomplishment of operational effects.
;; 
;; 
;; Concept
;; =======
;; The Var=Bar programming language is based upon the mimicry of
;; variable assignments for the fulfilment of its various capabilities,
;; the perimeter of which embraces, besides the storage of literal
;; integers and strings, simple arithmetics and string concatenation,
;; input and output communication, and a while-based iteration
;; construct.
;; 
;; == INSTRUCTIONS ==
;; The most conspicuous diorism of this language resides in its
;; instructions. Each accommodated with a line of its own for a haft,
;; the pattern follows a Procrustean rule in the mimicry of a variable
;; assignment, irrespective of the intention:
;; 
;;   <variableName> = <command>
;; 
;; The sinistral moeity, bearing solely the variable name, may adduce
;; any name for the placeholder, including strings and integers. The
;; utility, designed above with <command>, determines the identifier's
;; actual role.
;; 
;; Separated from the variable by a single equality sign ("="), the
;; <command> portion may assume one of several forms, the content of
;; the same operates in champarty with the sinistral identifier in order
;; to eventuate an effect:
;; 
;;   - If the <command> equals "0UT", the <variable> value is printed.
;;   - If the <command> constitutes an arithmetic expression, including
;;     the plus ("+") or minus ("-") operator, the variable is assigned
;;     the thus yielded result.
;;   - If the <command> contains a condition in the form of a predicate,
;;     utilizing the relationships "less than" ("<"), "greater than"
;;     (">"), or "equal to" ("=="), the complete instruction designates
;;     the start of a while loop perpetuating while this condition
;;     holds, with the loop identified by the <variable> for later
;;     termination.
;;   - If the <command> equals the "STOP" keyword, the line demarcates
;;     the end of the while loop body whose name matches the <variable>.
;; 
;; == VARIABLES ==
;; A central aspect of Var=Bar programming revolves around the
;; manipulation of variables.
;; 
;; These placeholders, amenable to an identification by string or
;; integer designations, permit the storage of scalars from the same
;; set. Any position which homologates the insertion of literals also
;; tolerates variables, encompassing, in particular, arithmetic and
;; conditional contexts.
;; 
;; == SPECIAL NAMES ==
;; A set of language keywords, known as special names, provides the
;; means of input/output and iterations.
;; 
;; As with any construct in Var=Bar, the special names reside at the
;; dextral position of a statement --- either as singleton terms, or as
;; part of a longer expression.
;; 
;; This reserved aspect shall now be enumerated:
;; 
;;   ------------------------------------------------------------------
;;   Special name | Description
;;   -------------+----------------------------------------------------
;;   1N           | Queries the user for an integer or string input and
;;                | returns the same.
;;                | Can be incorporated into expressions or employed on
;;                | its own.
;;   ..................................................................
;;   0UT          | Prints the assigned variable's value.
;;                | Must be employed on its own.
;;   ..................................................................
;;   STOP         | Demarcates the end of a while loop's body.
;;                | Must be employed on its own.
;;   ------------------------------------------------------------------
;; 
;; 
;; Instructions
;; ============
;; Var=Bar represents its various facilities, whose compass possedes
;; input/output operations, addition, subtraction, and string
;; concatenation, as well as a while loop, following the exact same
;; pattern, distinguished in the intention by the assigned portion.
;; 
;; == OVERVIEW ==
;; The following apercu shall impart a cursory nortelry concerning the
;; language's operative department:
;; 
;;   ------------------------------------------------------------------
;;   Pattern             | Effect
;;   --------------------+---------------------------------------------
;;   {x} = 0UT           | Prints to the standard output to the value
;;                       | of the variable with the name {x}.
;;   ..................................................................
;;   {x} = {arithmetics} | Computes the numeric or string expression
;;                       | {arithmetics} and stores the result in the
;;                       | variable with the name {x}.
;;   ..................................................................
;;   {x} = {condition}   | Starts a loop identified by the name {x},
;;                       | which executes its body statements while the
;;                       | {condition} is satisfied.
;;                       | The body starts with the line immediately
;;                       | succeeding this one, and terminates with the
;;                       | instruction
;;                       |   {x} = STOP
;;                       | whose name {x} concurs with the established
;;                       | {x}; which please see below.
;;   ..................................................................
;;   {x} = STOP          | Demarcates the end of the while loop
;;                       | established with the name {x} using
;;                       |   {x} = {condition}
;;   ------------------------------------------------------------------
;; 
;; == ARITHMETICS ==
;; Var=Bar involves a very restricted set of arithmetic capabilities,
;; applicable both to integers and, in a lesser extent, strings, by
;; adminiculum of merely a simple twain of operations: the addition and
;; subtraction.
;; 
;; Beside the literal statement of values and the indirection
;; accommodated through variables, the input command "1N" may be
;; inserted at any position compatible with the previous two options.
;; 
;; Merely two operators partake in this subject:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Description
;;   ---------+--------------------------------------------------------
;;   +        | Addition or string concatenation.
;;   ..................................................................
;;   -        | Subtraction.
;;   ------------------------------------------------------------------
;; 
;; The scant cardinality governing both the set of available data types
;; and operators homologates an exhaustive juxtaposition of the latter's
;; effect in response to combinations of the former, as shall now be
;; exhibited:
;; 
;;   ------------------------------------------------------------------
;;   Left operand | Operator | Right operand | Effect
;;   -------------+----------+---------------+-------------------------
;;   integer      |    +     | integer       | addition
;;   ..................................................................
;;   integer      |    +     | string        | string concatenation
;;   ..................................................................
;;   string       |    +     | integer       | string concatenation
;;   ..................................................................
;;   string       |    +     | string        | string concatenation
;;   ..................................................................
;;   integer      |    -     | integer       | subtraction
;;   ..................................................................
;;   integer      |    -     | string        | invalid
;;   ..................................................................
;;   string       |    -     | integer       | invalid
;;   ..................................................................
;;   string       |    -     | string        | invalid
;;   ------------------------------------------------------------------
;; 
;; == CONDITIONS ==
;; The second context pertinent for the investment of expressions
;; relates to conditions, which exclusively conjoin with the only
;; iteration structure in the language, the while loop.
;; 
;; A condition is designed as a tripartite entity, with the infix
;; operator, or relational operator, or simply relation, ensconced on
;; its two lateralities by an expression each, known as the left and the
;; right operand.
;; 
;; The expression may contain any sequence of items capable of yielding
;; an integer or string result, including literals, variables, and
;; arithmetic expressions, the latter of which please see in the above
;; subsection "ARITHMETICS".
;; 
;; Three members already exhaust the set of available comparison
;; operators:
;; 
;;   ------------------------------------------------------------------
;;   Relation | Description
;;   ---------+--------------------------------------------------------
;;   <        | less than
;;   ..................................................................
;;   >        | greater than
;;   ..................................................................
;;   ==       | equal to
;;   ------------------------------------------------------------------
;; 
;; The interrelations betwixt integers and strings implements a point
;; with significant ramifications; the same shall now be enumerated:
;; 
;;   ------------------------------------------------------------------
;;   Left operand | Relation | Right operand | Effect
;;   -------------+----------+---------------+-------------------------
;;   integer      |    <     | integer       | numeric less than
;;   ..................................................................
;;   integer      |    <     | string        | always false
;;   ..................................................................
;;   string       |    <     | integer       | always false
;;   ..................................................................
;;   string       |    <     | string        | case-sensitive less than
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;   integer      |    >     | integer       | numeric greater than
;;   ..................................................................
;;   integer      |    >     | string        | always false
;;   ..................................................................
;;   string       |    >     | integer       | always false
;;   ..................................................................
;;   string       |    >     | string        | case-sensitive greater
;;                |          |               | than
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;   integer      |    ==    | integer       | numeric equality
;;   ..................................................................
;;   integer      |    ==    | string        | always false
;;   ..................................................................
;;   string       |    ==    | integer       | always false
;;   ..................................................................
;;   string       |    ==    | string        | case-sensitive equality
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; A Var=Bar program's architectural framework is founded upon the
;; definition of variables only, with no other entity invested in the
;; data castaldy.
;; 
;; == VARIABLES RESIDE IN A REGISTRY ==
;; Variable names being admissive to strings as well as integers, their
;; affiliation replicates in the realm of their permitted content.
;; Maugre a specific format's destitution, the requirements extracted
;; from the original specification most sensibly intimate the services
;; of a mapping, for instance in the form of a hash table, which
;; conjoins with any variable identifier the associated value.
;; 
;; 
;; Data Types
;; ==========
;; The language is endowed with two data types: signed integers of any
;; magnitude and strings.
;; 
;; == INTEGERS ==
;; The numeric bailiwick of Var=Bar comprises integers of any sign and
;; magnitude, theoretically occupants of the range
;; [-infinity, +infinity].
;; 
;; == STRINGS ==
;; In Counterdistinguishment from a preponderance of programming
;; languages, Var=Bar strings are not demarcated or designated by any
;; particular formatting or pattern; instead, any identifier not
;; implicitly excluded by its subsumption into the special names or the
;; program's registered variable experiences an interpretation as a
;; literal character sequence object.
;; 
;; Strings an integers, in those contexts tentive of their separation,
;; are defined as a composition of one or more non-whitespace
;; characters, with at least one constituent not borrowed from the set
;; of decimal digits.
;; 
;; 
;; Syntax
;; ======
;; Var=Bar's linguistic diorism furnishes the commorant of a very
;; peculiar and homogenous haecceity, with each line reserved for at
;; most one statement, and each statement designed so as to mimic a
;; variable assignment, the dextral moeity of which establishes the
;; instruction's construe.
;; 
;; == INSTRUCTIONS ==
;; Var=Bar's dioristic guise does not appear anywhere in such a perfect
;; mete as in its instruction's forbisen. Every statement's woning
;; assumes a source code line of its own possession, without exemption
;; in concord with the simulation of an assignment to a variable:
;; 
;;   <variableName> = <operation>
;; 
;; The emblematic instruments are realized in the dextral compartment,
;; here designated by the <operation>, which in its form and content
;; entails the faculty of distinguishment.
;; 
;; == INTEGERS ==
;; The role of an integer number is appropriated by a sequence of one or
;; more decimal digits in immediate adjacency, optionally preceded by a
;; sign, "+" or "-".
;; 
;; == IDENTIFIERS ==
;; A kenspeckle attribute of its concoction, no explicit discrepancy
;; applies itself to be steadable in the segregation of special
;; identifiers, or language keywords, variable names, and strings.
;; 
;; Any sequence of characters not engaged in the affiliation with
;; whitespace or operators, and containing at least one non-digit
;; constituent automatically subsumes into the treble token nature.
;; Given the fact that variable names may appropriate integers, the
;; capacity of distinguishment is further adumbrated.
;; 
;; == COMMENTS ==
;; No provisions for comments are embraced among the language's dations.
;; 
;; == SPACES ==
;; The existence, distribution, and quantification of spaces, the
;; definition of which is compact of the space character and the
;; horizontal tab, elicits no significance and incites no demur.
;; 
;; == LINEBREAKS ==
;; Newline characters partake of an especially substantive role as
;; sepiments betwixt each two instruction lines. Vacant expanses may be
;; interspersed with magnimity.
;; 
;; == GRAMMAR ==
;; The language's donat is replicated in the following Extended
;; Backus-Naur Form (EBNF):
;; 
;;   program      := [ statement ] , { newline , [ statement ] } ;
;;   
;;   statement    := arithmetics
;;                |  print
;;                |  loopStart
;;                |  loopEnd
;;                ;
;;   arithmetics  := identifier , "=" ,
;;                   expression , { ( "+" | "-" ) , expression }
;;                ;
;;   print        := identifier , "=" , "0UT" ;
;;   loopStart    := identifier , "=" , condition ;
;;   loopEnd      := identifier , "=" , "STOP" ;
;;   condition    := expression , relation , expression ;
;;   relation     := "<" | ">" | "==" ;
;;   
;;   expression   := input
;;                |  number
;;                |  identifier
;;                ;
;;   input        := "1N" ;
;;   
;;   identifier   := nonSpace , { nonSpace } ;
;;   nonSpace     := character - ( space | newline ) ;
;;   space        := " " | "\t" ;
;;   newline      := "\n" ;
;;   number       := digit , { digit } ;
;;   digit        := "0" | "1" | "2" | "3" | "4"
;;                |  "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The original Var=Bar specification, despite stern efforts into its
;; descriptive and demonstrative aspects, perforce exhibits a few
;; lacunae; a select thereof shall be enumerated below:
;; 
;; == CAN SPECIAL VALUES BE APPROPRIATED AS VARIABLE NAMES? ==
;; The specification claims the admission of "special names", among
;; other tokens, as variable names, in conjunction with "special values"
;; for their contents.
;; 
;; A detailed treatise on the definition of "special names", however,
;; remains unrequited. It seems that "special values" appertains to the
;; keywords "1N", "0UT, and "STOP", whereas no separate specimens
;; conforming to the former category exist, begetting the question of
;; the terms' parity.
;; 
;; It has thus been adjudged to refrain from the permission to specify
;; these three special values as variable identifiers. Concretely, their
;; names may be mentioned for an assignment; yet, no means for such a
;; variable value's recovery is provided, as the statement of a thus
;; designated identifier always relays to its original effect.
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
;; Date:   2023-05-17
;; 
;; Sources:
;;   [crockford2007tdopparse.js]
;;   URL: "http://crockford.com/javascript/tdop/parse.js"
;;   Notes:
;;     - Presents the source code for a Pratt parser implementing a
;;       subset of JavaScript, yclept "Simplified JavaScript".
;;     - The main page, supplying elucidations, can be found under
;;       -> "https://crockford.com/javascript/tdop/tdop.html".
;;   
;;   [esolang2023Var=Bar]
;;   The Esolang contributors, "Var=Bar", 2023
;;   URL: "https://esolangs.org/wiki/Var%3DBar"
;;   
;;   [grand1997javalangref]
;;   Mark Grand, "Java Language Reference", 2nd Edition July 1997,
;;               "Chapter 4.14 Order of Operations"
;;   URL: "http://web.deu.edu.tr/doc/oreily/java/langref/ch04_14.htm"
;;   Notes:
;;     - Describes and lists the order of operations established in the
;;       Java programming language.
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list as a possible
   empty sequence of even-valued length, each indicator (key) of which
   conforms to the INDICATOR-TYPE, immediately following by an
   associated value of the VALUE-TYPE, both of which default to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (list object)
            (evenp (length (the list object)))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list object)
                by      #'cddr
              always
                (and (typep indicator indicator-type)
                     (typep value     value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  `(list-of Node))

;;; -------------------------------------------------------

(deftype relation ()
  "The ``relation'' type enumerates the recognized comparison relation
   operators."
  '(member :equal-to :greater-than :less-than))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized unary
   operators."
  '(member :plus :minus))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized binary
   operators."
  '(member :plus :minus))

;;; -------------------------------------------------------

(deftype vb-object ()
  "The ``vb-object'' type enumerates the recognized data types for the
   deployment in a Var=Bar program, concretely, integers and strings."
  '(or integer string))

;;; -------------------------------------------------------

(deftype variable-set ()
  "The ``variable-set'' type defines a set of variables a mapping of
   variable names to the associated ``vb-object'' values."
  '(hash-table-of string vb-object))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant portion obtained
   during the lexical analyzation of a piece of Var=Bar source code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Var=Bar source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class establishes a lexical analyzer, an agent
     suitable for the separation of a piece of Var=Bar source code into
     its tokens."))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'', and
   ``character'' to the eponymous symbol macros for general access,
   executes the BODY forms, and returns the last evaluated form's
   results.
   ---
   As an supererogative adminiculum, a local function ``advance'' is
   declared which upon its invocation moves the LEXER's position cursor
   to the next character without returning a value."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (with-slots (source position character) ,evaluated-lexer
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (flet ((advance ()
                  "Moves the LEXER's position cursor to the next
                   location in its source, if possible, updates the
                   current CHARACTER, and returns no value."
                  (setf character
                    (when (array-in-bounds-p source (1+ position))
                      (char source (incf position))))
                  (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which operates on the Var=Bar
   SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-move-to (lexer new-position)
  "Relocates the LEXER's position cursor to the NEW-POSITION, updates
   its internal state, and returns the modified LEXER."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (with-lexer (lexer)
    (setf position new-position)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-string-follows-p (lexer expected-string)
  "Starting at the current position into the LEXER's source, checks
   whether the subsequent characters replicate the EXPECTED-STRING, on
   confirmation moving the position cursor to the location inside of the
   LEXER's source immediately following the matching portion and
   returning a ``boolean'' value of ``T'', otherwise retaining the
   position at the instant of this function's invocation and returning
   ``NIL''."
  (declare (type Lexer  lexer))
  (declare (type string expected-string))
  (with-lexer (lexer)
    (let ((start-position position))
      (declare (type fixnum start-position))
      (the boolean
        (loop
          for expected-character
            of-type character
            across  expected-string
          do
            (cond
              ((null character)
                (lexer-move-to lexer start-position)
                (return NIL))
              ((char/= character expected-character)
                (lexer-move-to lexer start-position)
                (return NIL))
              (T
                (advance)))
          finally
            (return T))))))

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER represents a valid constituent of an
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not
      (member character '(#\Space #\Tab #\Newline #\+ #\- #\= #\< #\>)
        :test #'char=))))

;;; -------------------------------------------------------

(defun integer-string-p (string)
  "Checks whether the STRING represents a signed integer, on
   confirmation returning a token representation thereof, otherwise the
   ``NIL'' value."
  (declare (type string string))
  (the (or null Token)
    (nth-value 0
      (ignore-errors
        (make-token :number
          (parse-integer string))))))

;;; -------------------------------------------------------

(defun special-name-p (string)
  "Checks whether the STRING represents a Var=Bar special name, on
   confirmation returning a token representation thereof, otherwise the
   ``NIL'' value."
  (declare (type string string))
  (the (or null Token)
    (cond
      ((string= string "1N")
        (make-token :1N "1N"))
      ((string= string "0UT")
        (make-token :0UT "0UT"))
      ((string= string "STOP")
        (make-token :STOP "STOP")))))

;;; -------------------------------------------------------

(defun evaluate-identifier (string)
  "Returns a token representation of the STRING, either forming an
   integer, special name, or a plain identifier, in this exact order of
   priority."
  (declare (type string string))
  (the Token
    (or (integer-string-p string)
        (special-name-p   string)
        (make-token :identifier string))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current position into the LEXER's source, reads an
   identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (evaluate-identifier
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop
            while (and character (identifier-character-p character))
            do
              (write-char character identifier)
              (advance)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds with a fresh
   end-of-file (EOF) token to each request."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((member character '(#\Space #\Tab) :test #'char=)
          (advance)
          (lexer-get-next-token lexer))
        
        ((char= character #\Newline)
          (prog1
            (make-token :newline character)
            (advance)))
        
        ((identifier-character-p character)
          (lexer-read-identifier lexer))
        
        ((char= character #\+)
          (prog1
            (make-token :plus character)
            (advance)))
        
        ((char= character #\-)
          (prog1
            (make-token :minus character)
            (advance)))
        
        ((char= character #\>)
          (prog1
            (make-token :greater-than character)
            (advance)))
        
        ((char= character #\<)
          (prog1
            (make-token :less-than character)
            (advance)))
        
        ((lexer-string-follows-p lexer "==")
          (make-token :equal-to "=="))
        
        ((char= character #\=)
          (prog1
            (make-token :assign character)
            (advance)))
        
        (T
          (error "Invalid character ~c at position ~d." character
            position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class avails in the encapsulation of a Var=Bar language
   facility's attributes in the form of an abstract syntax tree (AST)
   leaf or subtree."
  (type
    (error "Missing node type.")
    :type keyword)
  (attributes
    (make-hash-table :test #'eql)
    :type (hash-table-of keyword T)))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' categorized by the TYPE, and
   optionally initialized with the INITIAL-ATTRIBUTES, expected as a
   flat list where each attribute name is followed by the associated
   value."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) initial-attributes))
  (let ((node (initialize-node type)))
    (declare (type Node node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (node-attributes node))
              attribute-value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the value associated in the NODE with the ATTRIBUTE-NAME, or
   signals an error of an unspecified type if no such affiliation can be
   ascertained."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (the T
    (or (gethash attribute-name (node-attributes node))
        (error "The node ~s does not contain an attribute named ~s."
          node attribute-name))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (new-value node attribute-name)
  "Associates the ATTRIBUTE-NAME with the NEW-VALUE in the NODE,
   contingently superseding an extant entry with the identifier, and
   returns the modified NODE."
  (declare (type T       new-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (node-attributes node)) new-value)
  (values))

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
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser integer)   Node)
                parser-parse-expression))
(declaim (ftype (function (Parser vb-object) node-list)
                parser-parse-loop))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for parser.")
    :type          Lexer
    :documentation "The lexer responsible for supplying the tokens.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class' onus comprehends the assemblage of an
     abstract syntax tree (AST) from a sequence of tokens, ready for the
     processing by an interpreter."))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and ``current-token''
   to eponymous symbol macros for general access, executes the BODY
   forms, and returns the last evaluated form's results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (with-slots (lexer current-token) ,evaluated-parser
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-parser (parser)
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which assembles its program from
   the LEXER's token stream."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))


;;====================================================================;;
;; Parser: Foundational capabilities section.                         ;;
;;====================================================================;;

(defun parser-consume (parser)
  "Returns the PARSER's current token, while requesting and storing the
   next one from the internally managed lexer."
  (declare (type Parser parser))
  (the Token
    (with-parser (parser)
      (prog1 current-token
        (setf current-token
          (lexer-get-next-token lexer))))))


;;====================================================================;;
;; Parser: Pratt parser section.                                      ;;
;;====================================================================;;

(defun get-consequent-binding-power (token)
  "Returns the numeric binding power associated with the TOKEN, or
   signals an error of an unspecified type if no such attribute is
   affiliated with the same."
  (declare (type Token token))
  (the integer
    (case (token-type token)
      ((:plus         :minus)               30)
      ((:greater-than :less-than :equal-to) 20)
      (otherwise
        (error "There is no consequent binding power defined for the ~
                token ~s."
          token)))))

;;; -------------------------------------------------------

(defun get-initial-binding-power (token)
  (declare (type Token token))
  (the integer
    (case (token-type token)
      ((:plus :minus) 50)
      (otherwise
        (error "There is no initial binding power defined for the ~
                token ~s."
          token)))))

;;; -------------------------------------------------------

(defun get-initial-parselet (token)
  "Returns the initial parselet for the TOKEN, or ``NIL'' in the absence
   of such an affiliation.
   ---
   The initial parselet, also known as operand or prefix operator
   parselet or \"nud\" (null denotation) parselet, serves to evaluate
   the TOKEN in order to produce a node representation thereof."
  (declare (type Token token))
  
  (the (or null function)
    (case (token-type token)
      (:number
        #'(lambda (parser initial-token)
            (declare (type Parser parser))
            (declare (type Token  initial-token))
            (declare (ignore      parser))
            (the Node
              (make-node :number
                :value (token-value initial-token)))))
      
      ((:plus :minus)
        #'(lambda (parser initial-token)
            (declare (type Parser parser))
            (declare (type Token  initial-token))
            (the Node
              (make-node :unary-operation
                :operator (token-type initial-token)
                :operand  (parser-parse-expression parser
                            (get-initial-binding-power initial-token))))))
      
      (:identifier
        #'(lambda (parser initial-token)
            (declare (type Parser parser))
            (declare (type Token  initial-token))
            (declare (ignore      parser))
            (the Node
              (make-node :identifier
                :value (token-value initial-token)))))
      
      (:1N
        #'(lambda (parser initial-token)
            (declare (type Parser parser))
            (declare (type Token  initial-token))
            (declare (ignore      parser))
            (declare (ignore      initial-token))
            (the Node
              (make-node :input))))
      
      (otherwise
        NIL))))

;;; -------------------------------------------------------

(defun consequent-token-p (token)
  "Checks whether the TOKEN represents a consequent token, or operator
   or \"led\" (left denotation), on confirmation returning a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token)
        '(:greater-than :less-than :equal-to
          :plus :minus)
        :test #'eq)))))

;;; -------------------------------------------------------

(defun get-consequent-parselet (token)
  "Returns the consequent parselet for the TOKEN, or ``NIL'' if none
   such exists.
   ---
   The consequent parselet, also known as an infix or postfix operator
   parselet or \"led\" (left denotation) parselet, serves to evaluate
   the TOKEN, and a preceding left node providing the left operand, in
   order to produce a node representation."
  (declare (type Token token))
  
  (the (or null function)
    (case (token-type token)
      ;; "+", "-"
      ((:plus :minus)
        #'(lambda (parser left-node consequent-token)
            (declare (type Parser parser))
            (declare (type Node   left-node))
            (declare (type Token  consequent-token))
            (the Node
              (make-node :binary-operation
                :operator (token-type consequent-token)
                :left     left-node
                :right    (parser-parse-expression parser
                            (get-consequent-binding-power
                              consequent-token))))))
      
      ;; ">", "<", "=="
      ((:greater-than :less-than :equal-to)
        #'(lambda (parser left-node consequent-token)
            (declare (type Parser parser))
            (declare (type Node   left-node))
            (declare (type Token  consequent-token))
            (the Node
              (make-node :condition
                :operator (token-type consequent-token)
                :left     left-node
                :right    (parser-parse-expression parser
                            (get-consequent-binding-power
                              consequent-token))))))
      
      (otherwise
        NIL))))

;;; -------------------------------------------------------

(defun parser-parse-expression (parser current-binding-power)
  "Starting with the PARSER's current token and the
   CURRENT-BINDING-POWER which either represents the preceding operator
   token's precedence or an arbitrarily chosen strength, parses a
   sequences one operands and operators using Pratt's concept, and
   returns a node representation of the thus obtained subtree."
  (declare (type Parser  parser))
  (declare (type integer current-binding-power))
  
  (with-parser (parser)
    (let ((initial-token    NIL)
          (initial-parselet NIL)
          (left-node        NIL))
      (declare (type (or null Token)    initial-token))
      (declare (type (or null function) initial-parselet))
      (declare (type (or null Node)     left-node))
      
      (setf initial-token current-token)
      (parser-consume parser)
      
      (setf initial-parselet (get-initial-parselet initial-token))
      
      (unless initial-parselet
        (error "No initial token: ~s." initial-token))
      
      (setf left-node
        (funcall initial-parselet parser initial-token))
      
      (loop do
        (cond
          ;; End of code reached.
          ((token-type-p current-token :eof)
            (loop-finish))
          
          ;; End of expression reached.
          ((token-type-p current-token :new-line)
            (loop-finish))
          
          ;; Next token is not an operator.
          ((not (consequent-token-p current-token))
            (loop-finish))
          
          ;; Next operator is too weak to claim the LEFT-NODE.
          ((<= (get-consequent-binding-power current-token)
               current-binding-power)
            (loop-finish))
          
          ;; Next operator binds the LEFT-NODE and returns its own node.
          (T
            (let ((consequent-token    current-token)
                  (consequent-parselet (get-consequent-parselet
                                         current-token)))
              (declare (type Token    consequent-token))
              (declare (type function consequent-parselet))
              
              (parser-consume parser)
              
              (setf left-node
                (funcall consequent-parselet parser
                                             left-node
                                             consequent-token))))))
      
      (the Node left-node))))


;;====================================================================;;
;; Parser: General capabilities section.                              ;;
;;====================================================================;;

(defun parser-expect-end-of-line (parser)
  "Checks whether the PARSER's current token represents either a
   linebreak (EOL) or the end of the file (EOF), on confirmation either
   consuming the current token and loading the next from the internally
   managed lexer, as in the first case, or simplying proceeding, as in
   the second situation, before returning the PARSER; otherwise, an
   error of an unspecified type is signaled."
  (declare (type Parser parser))
  (with-parser (parser)
    (case (token-type current-token)
      (:eof      NIL)
      (:newline  (parser-consume parser))
      (otherwise (error "Expected end-of-file or newline, ~
                         but encountered ~s."
                   current-token))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation querying and storing the next
   token from the internally managed lexer, while returing the just
   perquired and substituted token; otherwise, an error of an
   unspecified type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-parser (parser)
    (the Token
      (if (token-type-p current-token expected-token-type)
        (parser-consume parser)
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-variable-name (parser)
  "Parses the variable name located at the start of an instruction line
   using the PARSER, skips the following assignment symbol \"=\", and
   returns an integer or string representation of the former."
  (declare (type Parser parser))
  (with-parser (parser)
    (the vb-object
      (prog1
        (case (token-type current-token)
          (:identifier
            (token-value (parser-eat parser :identifier)))
          (:number
            (token-value (parser-eat parser :number)))
          (otherwise
            (error "Expected a variable name identifier or number, ~
                    but encountered the token ~s."
              current-token)))
        (parser-eat parser :assign)))))

;;; -------------------------------------------------------

(defun parser-parse-statement (parser)
  "Parses a Var=Bar program statement using the PARSER and returns a
   node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((variable (parser-parse-variable-name parser)))
      (declare (type vb-object variable))
      (prog1
        (case (token-type current-token)
          (:eof
            (error "Missing variable assignment value on end of file."))
          
          (:newline
            (error "Missing variable assignment value on end of line."))
          
          (:STOP
            (parser-eat parser :STOP)
            (make-node :loop-end :variable variable))
          
          (:0UT
            (parser-eat parser :0UT)
            (make-node :output :variable variable))
          
          (otherwise
            (let ((value-node (parser-parse-expression parser 0)))
              (declare (type Node value-node))
              
              (case (node-type value-node)
                (:number
                  (make-node :assignment
                    :kind     :numeric
                    :variable variable
                    :value    value-node))
                
                (:identifier
                  (make-node :assignment
                    :kind     :textual
                    :variable variable
                    :value    value-node))
                
                (:input
                  (make-node :assignment
                    :kind     :input
                    :variable variable
                    :value    value-node))
                
                (:unary-operation
                  (make-node :assignment
                    :kind     :arithmetic
                    :variable variable
                    :value    value-node))
                
                (:binary-operation
                  (make-node :assignment
                    :kind     :arithmetic
                    :variable variable
                    :value    value-node))
                
                (:condition
                  (make-node :loop
                    :variable  variable
                    :condition value-node
                    :body      (progn
                                 (parser-expect-end-of-line parser)
                                 (parser-parse-loop parser variable))))
                
                (otherwise
                  (error "Unexpected token and node combination: ~
                          ~s and ~s."
                    current-token value-node))))))
        
        (parser-expect-end-of-line parser)))))

;;; -------------------------------------------------------

(defun parser-parse-loop (parser loop-name)
  "Parses an iteration (loop) block's body statements and returns a list
   of nodes, each representing such a statement."
  (declare (type Parser    parser))
  (declare (type vb-object loop-name))
  (let ((statements NIL))
    (declare (type node-list statements))
    (with-parser (parser)
      (flet ((end-of-loop-p (statement)
              "Checks whether the STATEMENT node represents the
               processed Var=Bar loop's terminator, on confirmation
               returning a ``boolean'' value of ``T'', otherwise
               ``NIL''."
              (declare (type Node statement))
              (the boolean
                (not (null
                  (and
                    ;; Is this an end-of-loop statement, that is:
                    ;;   <variableName> = STOP
                    (eq (node-type statement)
                        :loop-end)
                    ;; Does the <variableName> portion equal the
                    ;; processed Var=Bar loop's name?
                    (equal (node-attribute statement :variable)
                           loop-name)))))))
        (loop do
          (case (token-type current-token)
            (:eof
              (error "Unterminated loop ~s." loop-name))
            (:newline
              (parser-consume parser))
            (otherwise
              (let ((statement (parser-parse-statement parser)))
                (declare (type Node statement))
                (cond
                  ((end-of-loop-p statement)
                    (loop-finish))
                  (T
                    (push statement statements)))))))))
    (the node-list (nreverse statements))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the tokens acquired by the PARSER and returns the root node of
   the abstract syntax tree (TREE) modeling the thus presented Var=Bar
   program."
  (declare (type Parser parser))
  (the Node
    (make-node :program :statements
      (with-parser (parser)
        (loop
          if (token-type-p current-token :eof) do
            (loop-finish)
          else if (token-type-p current-token :newline) do
            (parser-consume parser)
          else
            collect (parser-parse-statement parser))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of function "compare".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compare (relation left-operand right-operand)
  (:documentation
    "Applies the RELATION to the LEFT-OPERAND and RIGHT-OPERAND, and
     returns a ``boolean'' value of ``T'' signifying the RELATION's
     fulfilment, otherwise ``NIL''.")
  
  (:method ((relation      (eql :equal-to))
            (left-operand  integer)
            (right-operand integer))
    (declare (type relation relation))
    (declare (type integer  left-operand))
    (declare (type integer  right-operand))
    (declare (ignore        relation))
    (the boolean
      (not (null (= left-operand right-operand)))))
  
  (:method ((relation      (eql :equal-to))
            (left-operand  string)
            (right-operand string))
    (declare (type relation relation))
    (declare (type string   left-operand))
    (declare (type string   right-operand))
    (declare (ignore        relation))
    (the boolean
      (not (null (string= left-operand right-operand)))))
  
  (:method ((relation      (eql :equal-to))
            (left-operand  T)
            (right-operand T))
    (declare (type relation relation))
    (declare (type string   left-operand))
    (declare (type string   right-operand))
    (declare (ignore        relation))
    (declare (ignore        left-operand))
    (declare (ignore        right-operand))
    (the boolean NIL))
  
  (:method ((relation      (eql :greater-than))
            (left-operand  integer)
            (right-operand integer))
    (declare (type relation relation))
    (declare (type integer  left-operand))
    (declare (type integer  right-operand))
    (declare (ignore        relation))
    (the boolean
      (not (null (> left-operand right-operand)))))
  
  (:method ((relation      (eql :greater-than))
            (left-operand  string)
            (right-operand string))
    (declare (type relation relation))
    (declare (type string   left-operand))
    (declare (type string   right-operand))
    (declare (ignore        relation))
    (the boolean
      (not (null (string> left-operand right-operand)))))
  
  (:method ((relation      (eql :greater-than))
            (left-operand  T)
            (right-operand T))
    (declare (type relation relation))
    (declare (type string   left-operand))
    (declare (type string   right-operand))
    (declare (ignore        relation))
    (declare (ignore        left-operand))
    (declare (ignore        right-operand))
    (the boolean NIL))
  
  (:method ((relation      (eql :less-than))
            (left-operand  integer)
            (right-operand integer))
    (declare (type relation relation))
    (declare (type integer  left-operand))
    (declare (type integer  right-operand))
    (declare (ignore        relation))
    (the boolean
      (not (null (< left-operand right-operand)))))
  
  (:method ((relation      (eql :less-than))
            (left-operand  string)
            (right-operand string))
    (declare (type relation relation))
    (declare (type string   left-operand))
    (declare (type string   right-operand))
    (declare (ignore        relation))
    (the boolean
      (not (null (string< left-operand right-operand)))))
  
  (:method ((relation      (eql :less-than))
            (left-operand  T)
            (right-operand T))
    (declare (type relation relation))
    (declare (type string   left-operand))
    (declare (type string   right-operand))
    (declare (ignore        relation))
    (declare (ignore        left-operand))
    (declare (ignore        right-operand))
    (the boolean NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of generic function "compute".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compute (operator left-operand right-operand)
  (:documentation
    "Applies the OPERATOR to the LEFT-OPERAND and the RIGHT-OPERAND and
     returns either an integer or a string result.")
  
  (:method ((operator      (eql :plus))
            (left-operand  integer)
            (right-operand integer))
    (declare (type binary-operator operator))
    (declare (type integer         left-operand))
    (declare (type integer         right-operand))
    (declare (ignore               operator))
    (the integer
      (+ left-operand right-operand)))
  
  (:method ((operator      (eql :plus))
            (left-operand  integer)
            (right-operand string))
    (declare (type binary-operator operator))
    (declare (type integer         left-operand))
    (declare (type string          right-operand))
    (declare (ignore               operator))
    (the string
      (format NIL "~a~a" left-operand right-operand)))
  
  (:method ((operator      (eql :plus))
            (left-operand  string)
            (right-operand string))
    (declare (type binary-operator operator))
    (declare (type string          left-operand))
    (declare (type string          right-operand))
    (declare (ignore               operator))
    (the string
      (format NIL "~a~a" left-operand right-operand)))
  
  (:method ((operator      (eql :plus))
            (left-operand  string)
            (right-operand integer))
    (declare (type binary-operator operator))
    (declare (type string          left-operand))
    (declare (type integer         right-operand))
    (declare (ignore               operator))
    (the string
      (format NIL "~a~a" left-operand right-operand)))
  
  (:method ((operator      (eql :minus))
            (left-operand  integer)
            (right-operand integer))
    (declare (type binary-operator operator))
    (declare (type integer         left-operand))
    (declare (type integer         right-operand))
    (declare (ignore               operator))
    (the integer
      (- left-operand right-operand))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-input (input)
  "Parses the INPUT as an integer or the string, inclining towards the
   former type, but resorting in cases of untenability to the general
   nature of the latter."
  (declare (type string input))
  (the vb-object
    (handler-case
      (nth-value 0 (parse-integer input))
      (error () input))))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing tree for the interpreter.")
    :type          Node
    :documentation "The abstract syntax tree (AST) to process.")
   (variables
    :initarg       :variables
    :initform      (make-hash-table :test #'equal)
    :type          variable-set
    :documentation "Associates each registered variable name with an
                    integer or string value."))
  (:documentation
    "The ``Interpreter'' class applies itself to the traversal and
     evaluation of an abstract syntax tree's (AST) nodes, with the
     objective of inducing effect into the thus represented Var=Bar
     program."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a new ``Interpreter'' operating on the abstract
   syntax TREE."
  (declare (type Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun variable-value (interpreter variable-name)
  "Returns the value of the variable registered with the VARIABLE-NAME
   at the INTERPRETER, or the VARIABLE-NAME itself upon such a
   variable's absence."
  (declare (type Interpreter interpreter))
  (declare (type vb-object   variable-name))
  (the vb-object
    (gethash variable-name
      (slot-value interpreter 'variables)
      variable-name)))

;;; -------------------------------------------------------

(defun (setf variable-value) (new-value interpreter variable-name)
  "Changes the value of the variable registered with the VARIABLE-NAME
   at the INTERPRETER to the NEW-VALUE, or creates and registered such
   an association, and returns no value."
  (declare (type vb-object   new-value))
  (declare (type Interpreter interpreter))
  (declare (type vb-object   variable-name))
  (setf (gethash variable-name (slot-value interpreter 'variables))
        new-value)
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-node (visitor node node-type)
  (:documentation
    "Orders the VISITOR to process the NODE dispatched on by its
     NODE-TYPE, returning a value appropriate for the same."))

;;; -------------------------------------------------------

(defun visitor-visit-node (visitor node)
  "Invokes the ``dispatch-node'' implementation most fitten for the
   processing of the NODE's type, employing the VISITOR as the first
   parameter."
  (the T
    (dispatch-node visitor node (node-type node))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :program)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((statements (node-attribute node :statements)))
    (declare (type node-list statements))
    (dolist (statement statements)
      (declare (type Node statement))
      (visitor-visit-node interpreter statement)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :assignment)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((variable (node-attribute node :variable))
        (value    (node-attribute node :value)))
    (declare (type vb-object variable))
    (declare (type Node      value))
    (setf (variable-value interpreter variable)
      (visitor-visit-node interpreter value)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :number)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (the vb-object
    (variable-value interpreter
      (node-attribute node :value))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :identifier)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (the vb-object
    (variable-value interpreter
      (node-attribute node :value))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :input)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node))
  (declare (ignore           node-type))
  (format T "~&Please input an integer or a string: ")
  (let ((input (parse-input (read-line))))
    (declare (type vb-object input))
    (clear-input)
    (the vb-object input)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :output)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((target (node-attribute node :variable)))
    (declare (type string target))
    (format T "~a"
      (variable-value interpreter target)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :condition)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((left-operand  (node-attribute node :left))
        (right-operand (node-attribute node :right))
        (operator      (node-attribute node :operator)))
    (declare (type Node     left-operand))
    (declare (type Node     right-operand))
    (declare (type relation operator))
    (the boolean
      (compare operator
        (visitor-visit-node interpreter left-operand)
        (visitor-visit-node interpreter right-operand)))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :unary-operation)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((operator (node-attribute node :operator))
        (operand  (node-attribute node :operand)))
    (declare (type unary-operator operator))
    (declare (type Node           operand))
    (the vb-object
      (case operator
        (:plus
          (visitor-visit-node interpreter operand))
        (:minus
          (- (visitor-visit-node interpreter operand)))
        (otherwise
          (error "No unary operator: ~s." operator))))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :binary-operation)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((left-operand  (node-attribute node :left))
        (right-operand (node-attribute node :right))
        (operator      (node-attribute node :operator)))
    (declare (type Node            left-operand))
    (declare (type Node            right-operand))
    (declare (type binary-operator operator))
    (the vb-object
      (compute operator
        (visitor-visit-node interpreter left-operand)
        (visitor-visit-node interpreter right-operand)))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node        Node)
                          (node-type   (eql :loop)))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (let ((condition (node-attribute node :condition))
        (body      (node-attribute node :body)))
    (declare (type Node      condition))
    (declare (type node-list body))
    (loop while (visitor-visit-node interpreter condition) do
      (dolist (statement body)
        (declare (type Node statement))
        (visitor-visit-node interpreter statement))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree stored in the INTERPRETER and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (visitor-visit-node interpreter (slot-value interpreter 'tree))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Var=Bar (code)
  "Interprets the piece of Var=Bar source CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-Var=Bar "v=1N
                    v=0UT
                    x=v==1
                    v=0UT
                    x=STOP")

;;; -------------------------------------------------------

;; Loop which counts up from one (1) to a user-induced maximum.
(interpret-Var=Bar "separator=,
                    maximum=1N
                    currentValue=1
                    currentValue=0UT
                    myLoop=currentValue<maximum
                    currentValue=currentValue+1
                    separator=0UT
                    currentValue=0UT
                    myLoop=STOP")

;;; -------------------------------------------------------

;; Employ arithmetics by utilizing the addition ("+") operator.
(interpret-Var=Bar "x=4
                    J=x - 1
                    J=0UT")

;;; -------------------------------------------------------

;; Employ string concatenation ("+") in order to build the text "no".
(interpret-Var=Bar "f=n
                    p=o
                    s=f + p
                    s=0UT")

;;; -------------------------------------------------------

;; Demonstrate the ramification of using integers as variable names:
;; The variable "1" is associated with the numeric value 100, which
;; henceforth associates the occurrence of the integer 1 with the
;; variable's inquisition in lieu of the literal expression.
(interpret-Var=Bar "1=100
                    x=1 + 5
                    x=0UT")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Var=Bar "infiniteLoop = 1 == 1
                    input = 1N
                    input = 0UT
                    infiniteLoop = STOP")

;;; -------------------------------------------------------

;; Fibonacci sequence with sequence length determined by the user input.
(interpret-Var=Bar
  "separator           = ,
   numberOfGenerations = 1N
   generationCounter   = 0
   shallPrintF0        = numberOfGenerations
   shallPrintF1        = numberOfGenerations - 1
   
   F0                  = 0
   F1                  = 1
   
   iteration           = shallPrintF0 > 0
   F0                  = 0UT
   shallPrintF0        = 0
   numberOfGenerations = numberOfGenerations - 1
   iteration           = STOP
   
   iteration           = shallPrintF1 > 0
   separator           = 0UT
   F1                  = 0UT
   shallPrintF1        = 0
   numberOfGenerations = numberOfGenerations - 1
   iteration           = STOP
   
   iteration           = generationCounter < numberOfGenerations
   FN                  = F0 + F1
   separator           = 0UT
   FN                  = 0UT
   F0                  = F1
   F1                  = FN
   generationCounter   = generationCounter + 1
   iteration           = STOP
  ")
