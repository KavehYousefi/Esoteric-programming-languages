;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "V3i", invented by the Esolang user "BowlingPizzaBall" and
;; presented on February 10th, 2022, its haecceity's firmament edified
;; upon a treble of variables as the aefauld memory expressions, being
;; operated upon by instructions invested with the capacitation for
;; arithmetics, logical facilities, input and output, as well as
;; several warklumes for conditional and iterative execution.
;; 
;; 
;; Concept
;; =======
;; The V3i programming language is founded upon the manipulation of a
;; treble of variables, norned "x", "y", and "z", by adminiculum of
;; several instructions, this capacitation's compass amplecting the
;; operation upon signed integers and strings.
;; 
;; == "V3i": THREE VARIABLES FORM THE MEMORY ==
;; The V3i programming language's stevening bewrays its provenance,
;; issuing from the treble memory componency, as "Variable 3 Input".
;; 
;; == THE SALVATORY: A PRIAL OF VARIABLES ==
;; The availability of variables does not tally a more commodious
;; componency than a pair royal of fixed agnominations, namely, "x",
;; "y", and "z", admitting signed integers of any mickleness, as well
;; as strings whose dispansion wists of a tantamount liberty.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical conspection, a V3i program comprises an ordered
;; sequence of zero or more statements, everichon among this membership
;; introduced via an instruction name, succeeded by zero or more
;; arguments, and terminated by a semicolon (";").
;; 
;; == COMMENTS ==
;; The V3i programming language offers the contingency for comments in
;; the guise of sections whose inchoation as well as conclusion are
;; demarcated by a single hash sign ("#").
;; 
;; 
;; Instructions
;; ============
;; Tallying in the most stringent sense a sextuple componency, the
;; V3i instruction set comprehends warklumes for the definition of
;; variables, the printing of arbitrary content, and several conditional
;; as well as iterative constructs.
;; 
;; == OVERVIEW ==
;; The following shall contribute an apercu's dation concerning the
;; language's operative competences.
;; 
;; Please heed the underlining of succedaneous parcels by adminiculum
;; of a catena of asterisks ("*"), intended for the supersession by
;; actual V3i code fragments in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ------------------+-----------------------------------------------
;;   def name value    | Assigns the {value} to the variable amenable
;;       **** *****    | to the {name}.
;;                     |-----------------------------------------------
;;                     | The {name} must be one of the triplet of
;;                     | recognized variable names, scilicet:
;;                     |   x
;;                     |   y
;;                     |   z
;;                     |-----------------------------------------------
;;                     | The {value} must be an expression.
;;   ..................................................................
;;   print arguments   | Prints the {arguments} to the standard output
;;         *********   | and issues a concluding newline character.
;;                     |-----------------------------------------------
;;                     | The {arguments} must be a sequence of zero or
;;                     | more expressions.
;;                     |-----------------------------------------------
;;                     | No further sepiment is introduced into an
;;                     | argument twissel's intermede.
;;   ..................................................................
;;   if antecedent;    | If the {antecedent} is satisfied, executes the
;;      **********     | {thenBody} statement once; otherwise
;;     thenBody        | accompasses no causatum.
;;     ********        |-----------------------------------------------
;;                     | The {antecedent} must be an expression which
;;                     | yields an integral value.
;;                     |-----------------------------------------------
;;                     | The {thenBody} must comprise a sequence
;;                     | composed of zero or more statements.
;;                     |-----------------------------------------------
;;                     | The {thenBody} is either terminated by an
;;                     | "end;" token matching in its nesting level, a
;;                     | matching "else" keyword, or, alternatively, by
;;                     | the program's desinence.
;;   ..................................................................
;;   if antecedent;    | If the {antecedent} is satisfied, executes the
;;      **********     | {thenBody} statements once; otherwise
;;     thenBody        | evaluates the {elseBody} part.
;;     ********        |-----------------------------------------------
;;   else;             | The {antecedent} must be an expression which
;;     elseBody        | yields an integral value.
;;     ********        |-----------------------------------------------
;;                     | The {thenBody} must comprise a sequence
;;                     | composed of zero or more statements.
;;                     |-----------------------------------------------
;;                     | The {thenBody} is either terminated by an
;;                     | "end;" token matching in its nesting level or
;;                     | a matching "else" keyword.
;;                     |-----------------------------------------------
;;                     | The {thenBody} must comprise a sequence
;;                     | composed of zero or more statements.
;;                     |-----------------------------------------------
;;                     | The {elseBody} is either terminated by an
;;                     | "end;" token matching in its nesting level,
;;                     | or, alternatively, by the program's desinence.
;;                     |-----------------------------------------------
;;                     | The semicolon betwixt the "else" keyword and
;;                     | the {elseBody} constitutes an optional
;;                     | constituent if the first statement succeeding
;;                     | "else" represents "if".
;;   ..................................................................
;;   iloop; body       | Executes the {body} statements in a perpetual
;;          ****       | manner.
;;                     |-----------------------------------------------
;;                     | The {body} must comprise a sequence composed
;;                     | of zero or more statements.
;;                     |-----------------------------------------------
;;                     | The {body} is either terminated by an "end;"
;;                     | token matching in its nesting level, or,
;;                     | alternatively, by the program's desinence.
;;                     |-----------------------------------------------
;;                     | The semicolon betwixt the "iloop" keyword and
;;                     | the {body} constitutes an optional
;;                     | constituent.
;;   ..................................................................
;;   loop cycles; body | Executes the {body} statements a tally of
;;        ******  **** | times equal to the {cycles.
;;                     |-----------------------------------------------
;;                     | The {cycles} must be a non-negative integer
;;                     | integer number. If less than zero (0), the
;;                     | causatum conflates with a zero (0) account.
;;                     |-----------------------------------------------
;;                     | The {body} must comprise a sequence composed
;;                     | of zero or more statements.
;;                     |-----------------------------------------------
;;                     | The {body} is either terminated by an "end;"
;;                     | token matching in its nesting level, or,
;;                     | alternatively, by the program's desinence.
;;   ..................................................................
;;   end;              | Optional demarcation to terminate a block
;;                     | instruction.
;;                     |-----------------------------------------------
;;                     | The following listing defines the instructions
;;                     | amenable to such a terminating sequence:
;;                     |   if
;;                     |   else
;;                     |   iloop
;;                     |   loop
;;   ..................................................................
;;   wait delay        | Pauses the program for the {delay} number of
;;        *****        | milliseconds.
;;                     |-----------------------------------------------
;;                     | The {delay} must be a non-negative integer
;;                     | integer number. If less than zero (0), the
;;                     | causatum conflates with a zero (0) account.
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS ==
;; V3i deploys a rather extensive set of operations, desumed from the
;; categories of unary and binary forms, with a further procession of
;; this ramosity into arithmetic, logical, and string branches.
;; 
;; == SPECIES OF OPERANDS ==
;; The requisite compernage to the operators, operands enjoy a
;; capacitation to furcate into a quintuple variety:
;; 
;;   (1) LITERAL INTEGERS
;;       Such objects partake in the guise of signed or unsigned decimal
;;       integer numbers, bournless along both extrema.
;;       Boolean values subsume into a particular subset commorant in
;;       this definition, homologated merely to either assume a zero (0)
;;       sentinel, to designate a "false" response, or a one (1) for the
;;       "true" output.
;;   
;;   (2) LITERAL STRING
;;       Strings are ensconced in a jumelle of quotation marks, '"',
;;       enumerating zero or more characters.
;;   
;;   (3) VARIABLE IDENTIFIERS
;;       A triad of variables is vouchsafed an existency in the V3i
;;       programming language, everichon among these an aefauld Latin
;;       letter's agnomation, namely:
;;         x
;;         y
;;         z
;;   
;;   (4) EXPLICIT VARIABLE INQUISITIONS
;;       The keyword "var" capacitates the explicit request of a
;;       variable's content, thilk poses a paregal to the more
;;       compendious simple statement of the identifier.
;;   
;;   (5) COMPOUND EXPRESSIONS
;;       The designment of siccan constituents constitutes a unary,
;;       binary, or relational operation's ultimity, contingently
;;       nested in order to harness the language's potential for complex
;;       constructs.
;; 
;; == UNARY OPERATORS ==
;; A triplet of unary operators exhausts the respective department:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Causatum
;;   ---------+--------------------------------------------------------
;;   +        | An identity operator which returns its argument without
;;            | modification.
;;   ..................................................................
;;   -        | Inverts its argument's sign.
;;   ..................................................................
;;   sqrt     | Returns the integral square root of its argument.
;;   ------------------------------------------------------------------
;; 
;; == BINARY OPERATORS ==
;; A quintuple componency maintains is entalented with the governance
;; of the binary operations, their conduits in both the input and
;; result aspects restricted to signed integer numbers:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Causatum
;;   ---------+--------------------------------------------------------
;;   +        | Returns the sum obtained by incrementing the left
;;            | operand by the right one.
;;   ..................................................................
;;   -        | Returns the difference obtained by a subtraction of the
;;            | left operand by the right one.
;;   ..................................................................
;;   *        | Returns the production obtained by a multiplication of
;;            | the left operand by the right one.
;;   ..................................................................
;;   /        | Returns the integral quotient obtained by a division of
;;            | the left operand by the right one.
;;   ..................................................................
;;   %        | Returns the remainder obtained by a division of the
;;            | left operand by the right one.
;;   ------------------------------------------------------------------
;; 
;; == RELATIONAL OPERATORS ==
;; A prial of relational operators embues the language with a logical
;; faculty, producing from its integral or string operands a binary
;; truth value representation, which comports with the equiparation of
;; a one-valued (1) sentinel with a Boolean "true" response, and the
;; identification of the number zero (0) with "false", that is:
;; 
;;   ---------------------------------
;;   Binary value | Boolean tantamount
;;   -------------+-------------------
;;   0            | false
;;   .................................
;;   1            | true
;;   ---------------------------------
;; 
;; A generalized construe's purview appertains to Boolean logic's
;; extension to any integral number as well as string objects:
;; 
;;   (1) An integer number equal to zero (0) is tantamount to a Boolean
;;       "false" output.
;;   
;;   (2) Any non-zero integer number is tantamount to a Boolean "true"
;;       output.
;;   
;;   (3) Any string is tantamount to a Boolean "true" output.
;; 
;; The contingency for juxtaposition of integers and strings begets a
;; heterogeneity in the interpretation of relationship betwixt operands,
;; whence ensues a requisitum for their reconciliation. The coming
;; tabulation shall attend to these twissel options' dportment:
;; 
;;   ------------------------------------------------------------------
;;   Left operand | Right operand | Handling
;;   -------------+---------------+------------------------------------
;;   integer      | integer       | Both operands partake in their
;;                |               | original form, adhering to the
;;                |               | arithmetic principles.
;;   ..................................................................
;;   integer      | string        | The right operand is parsed as an
;;                |               | integer number; upon its successful
;;                |               | transformation, the integer
;;                |               | equiparation principles govern the
;;                |               | process. If not admissive to a
;;                |               | numeric construe, a Boolean "false"
;;                |               | emerges.
;;   ..................................................................
;;   string       | string        | Both operands partake in their
;;                |               | original form, adhering to the
;;                |               | lexicographic principles.
;;   ..................................................................
;;   string       | integer       | The left operand is parsed as an
;;                |               | integer number; upon its successful
;;                |               | transformation, the integer
;;                |               | equiparation principles govern the
;;                |               | process. If not admissive to a
;;                |               | numeric construe, a Boolean "false"
;;                |               | emerges.
;;   ------------------------------------------------------------------
;; 
;; Proceeding from these diorisms' establishment, the following
;; tabulation shall intrine the relational specimens:
;; 
;;   ------------------------------------------------------------------
;;   Relation | Result
;;   ---------+--------------------------------------------------------
;;   =        | Returns a value of one (1) if the left operand equals
;;            | the right operand; otherwise produces zero (0).
;;   ..................................................................
;;   <        | Returns a value of one (1) if the left operand is
;;            | strictly less than the right operand; otherwise
;;            | produces zero (0).
;;   ..................................................................
;;   >        | Returns a value of one (1) if the left operand is
;;            | strictly greater than the right operand; otherwise
;;            | produces zero (0).
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specialization
;; =============================
;; The yet inchoate nature of the V3i protolog --- magure the boons of
;; senescence at the temporal fixation of this documentation's
;; persistence --- serves to procure inroads for ambiguous construes.
;; As a consequence distilled from this fact, the following sections
;; shall attend to a few select subjects as their cynosure.
;; 
;; == ARE SEMICOLONS MANDATORY STATEMENT SEPARATORS? ==
;; The provision of examples, rather than to alleviate this cumbrance,
;; incurs one's conspectuity's application upon the subject of statement
;; terminators with a high mete of ambivalency: While a preponderance
;; among the code lines ostend a semicolon as a statement's suffix,
;; sometimes betwixt an instruction's constituents themselves, a,
;; ostentatiously whimsical, selection eschews their participation.
;; 
;; It has been adjudged to enforce semicolons as mandatory sepiments
;; to abide in their presence as a statement's desinence. This agency
;; also governs the discrimination of a block instruction and its
;; potential operand from the subsequent body.
;; 
;; Merely the following exemptions shall be delivered to one's personal
;; liberties:
;; 
;;   (1) ELSE IF
;;       The semicolon betwixt a conditional "else" branch and its
;;       conjoined "if" may be omitted in order to actuate a syntomy,
;;       producing for the nimiety incarnated in the stringent
;;         else; if
;;       the more compact
;;         else if
;;   
;;   (2) ILOOP
;;       The semicolon betwixt the "iloop" and its body statements
;;       remains optional.
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
;;   (1) A lexer generates from the V3i program string a sequence of
;;       tokens.
;;   (2) The parser queries these tokens and assembles an abstract
;;       syntax tree (AST), the nodes of which represent the language
;;       facilities.
;;   (3) The interpreter traverses the AST and embues it with effect.
;; 
;; == EXPRESSIONS ARE ASSEMBLED VIA PRATT PARSING ==
;; The parser combines aspects of recursive descent and Pratt's
;; solution, with the former apprehending the general process, aided by
;; the latter for the assemblage of expressions.
;; 
;; 
;; Appendices
;; ==========
;; A particular set of subjects being eloigned in their pertinence from
;; the implementation, yet induced into a ligation with a fortitude as
;; potent as to homologate their elucidations in this document, shall be
;; the coming sections' cynosure.
;; 
;; == APPENDIX A: PROJECT FILES ==
;; The compass edified by this project in its conceptual and physical
;; mickleness, and in the conformation of its ensuing file account as a
;; warklume pursuing the complexity's facilitated castaldy, serves in
;; the parturition of a reticulation among the distributed Common Lisp
;; code components.
;; 
;; A tabular exposition concerning the files and their contributions
;; shall become the following listing's dation:
;; 
;;   ------------------------------------------------------------------
;;   No.| File                         | Role
;;   ---+------------------------------+-------------------------------
;;   01 | types.lisp                   | Encompasses the type
;;      |                              | definitions referenced
;;      |                              | throughout the project.
;;   ..................................................................
;;   02 | logical-operations.lisp      | Bundles Boolean operations
;;      |                              | requisite to several other
;;      |                              | project files.
;;   ..................................................................
;;   03 | token.lisp                   | Defines the ``Token'' class as
;;      |                              | an encapsulation of a
;;      |                              | significant object extracted
;;      |                              | from a piece of V3i source
;;      |                              | code.
;;   ..................................................................
;;   04 | lexer.lisp                   | Establishes the lexer
;;      |                              | functionality, ordained to the
;;      |                              | extraction and supply of
;;      |                              | tokens from a piece of V3i
;;      |                              | source code.
;;   ..................................................................
;;   05 | token-stream.lisp            | Realizes the ``Token-Stream''
;;      |                              | class, a tier superimposed on
;;      |                              | the lexer in order to
;;      |                              | vouchsafe an eath access
;;      |                              | warklume.
;;   ..................................................................
;;   06 | abstract-syntax-tree.lisp    | Specifies the abstract syntax
;;      |                              | tree (AST) nodes which
;;      |                              | comprise the parsed V3i
;;      |                              | program's static model.
;;   ..................................................................
;;   07 | parselet.lisp                | Contributes the expression
;;      |                              | parser adhering to the Pratt
;;      |                              | parsing concepts, reifying in
;;      |                              | several parselet diorisms.
;;   ..................................................................
;;   08 | parser.lisp                  | Defines the main parser, thilk
;;      |                              | relies on personal as well as
;;      |                              | the Pratt expression parselet
;;      |                              | competences.
;;   ..................................................................
;;   09 | operators.lisp               | Implements the V3i unary and
;;      |                              | binary operator functions.
;;   ..................................................................
;;   10 | variables.lisp               | Realizes the castaldy of the
;;      |                              | three available variables.
;;   ..................................................................
;;   11 | reference.lisp               | Implements the ``Reference'',
;;      |                              | a class ensconcing a variable
;;      |                              | identifier in order to
;;      |                              | distinguish the same from a
;;      |                              | string literal.
;;   ..................................................................
;;   12 | input-output-operations.lisp | Provides operations for the
;;      |                              | reception and parsing of
;;      |                              | input, as well as the delaying
;;      |                              | of system interactions.
;;   ..................................................................
;;   13 | interpreter.lisp             | Comprehends the implementation
;;      |                              | of the ``Interpreter'' class
;;      |                              | and its appertaining
;;      |                              | functions.
;;   ..................................................................
;;   14 | tests.lisp                   | Implements the test cases and
;;      |                              | examples serving as a
;;      |                              | vindication of the
;;      |                              | interpreter's conformance with
;;      |                              | the V3i language standard.
;;   ..................................................................
;;   15 | main.lisp                    | Establishes the entry point
;;      |                              | into this application.
;;   ------------------------------------------------------------------
;; 
;; == APPENDIX B: OPERATOR BINDING POWERS AND ASSOCIATIVITIES ==
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
;;   =        | 10            | left-to-right | Designated equality.
;;   ..................................................................
;;   <        | 20            | left-to-right | 
;;   ..................................................................
;;   >        | 20            | left-to-right | 
;;   ..................................................................
;;   +        | 30            | left-to-right | 
;;   ..................................................................
;;   -        | 30            | left-to-right | 
;;   ..................................................................
;;   *        | 40            | left-to-right | 
;;   ..................................................................
;;   /        | 40            | left-to-right | 
;;   ..................................................................
;;   %        | 40            | left-to-right | 
;;   ..................................................................
;;   - (sign) | 50            | right-to-left | Negative sign.
;;   ..................................................................
;;   + (sign) | 50            | right-to-left | Positive sign.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-09-22
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
;;   [esolang2023V3i]
;;   The Esolang contributors, "V3i", December 16th, 2023
;;   URL: "https://esolangs.org/wiki/V3i"
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

(deftype file-source ()
  "The ``file-source'' type a provenance covenable for the obtention of
   a data file or its content."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of project directory.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "Specifies the directory which shall lend a commorancy to all Common
   Lisp source files appertaining to the project.
   ---
   Please substitute this global variable's content by the actual
   directory on your system which contains the V3i interpreter's source
   files.
   ---
   Several facilities are offered by the Common Lisp standard library
   for engaging in such an activity, enumerating, for instance:
     ------------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+------------------------------------------
     make-path-name   | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                 \"Users\"
                      |                 \"Kaveh\"
                      |                 \"V3i\"
                      |                 \"V3i_001\"))
     ............................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/V3i/V3i_001/\")
     ------------------------------------------------------------")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file import operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-source-file (source-file)
  "Loads the Common Lisp SOURCE-FILE, expecting its commorancy to be
   located under the +PROJECT-DIRECTORY+, and returns no value."
  (declare (type file-source source-file))
  (load (merge-pathnames +PROJECT-DIRECTORY+ source-file))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of source files.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-source-file "types.lisp")
(import-source-file "logical-operations.lisp")
(import-source-file "token.lisp")
(import-source-file "lexer.lisp")
(import-source-file "token-stream.lisp")
(import-source-file "abstract-syntax-tree.lisp")
(import-source-file "parselet.lisp")
(import-source-file "parser.lisp")
(import-source-file "operators.lisp")
(import-source-file "variables.lisp")
(import-source-file "reference.lisp")
(import-source-file "input-output-operations.lisp")
(import-source-file "interpreter.lisp")
(import-source-file "tests.lisp")
