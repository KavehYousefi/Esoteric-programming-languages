;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "{}s", invented by the Esolang user "AmNow" and presented on
;; June 23rd, 2021, the proprium of which appertains to a rather
;; dioristic selection of characters in the manipulation of its
;; variables, norned via integer numbers and capacitated to store the
;; selfsame realm's specimens.
;; 
;; 
;; Concept
;; =======
;; The {}s programming language, its pronunciation established with the
;; diction "sets", and its alternative agnomination, reserved for the
;; circumstances of a less potent homologated character repertoire,
;; expressed in "()s", edifies its foundry upon the a rather kenspeckle
;; aggregate of symbols in order to pursue its indulgence into the
;; perquisitions and manipulations of variables, their stevening an act
;; of integral identifiers and their composition the scalar congruency
;; of the same species.
;; 
;; == VARIABLES: INTEGER IDENTIFIERS FOR INTEGER VALUES ==
;; The castaldy of data is confined to the variables' bailiwick,
;; associating with the identifiers, these handles manifested in
;; unsigned integer numerals, values desumed from the selfsame species
;; realm.
;; 
;; == INPUT AND OUTPUT ARE ACTUATED BY DEDICATED VARIABLES ==
;; The input and output conduit's participation proceeds in a rather
;; adumbrated ilk of involvement by mediation of two dedicated
;; variables' ordainment.
;; 
;; An assignment of the zero-valued variable, "0", eventuates, as an
;; epiphenomenal action, the assumed expression's issuance onto the
;; standard output. This variable's perquisition, however, incorporates
;; no deviation from its peers' inquisition.
;; 
;; An input reception's elicitation, as the athwart communication airt,
;; is begotten by querying the value of the variable "1". A twifaced
;; exposition wones in this placeholder, as assignments to the same,
;; maugre their homologation, of course are deprived of the quotidian
;; response's capacitation; forecause the value stored in the "1"
;; variable may never be yielded by subordination to the commorancy of
;; the input behest.
;; 
;; The following tabular illustration's cynosure shall be the input and
;; output communication facilities' deployment limned in the guise of
;; forbisens:
;; 
;;   ------------------------------------------------------------------
;;   Use case       | Effect
;;   ---------------+--------------------------------------------------
;;   0 < expression | Prints the {expression} to the standard output,
;;       ^^^^^^^^^^ | while concomitantly storing it in the variable
;;                  | answering to the name "0".
;;                  |--------------------------------------------------
;;                  | The concrete format of the display depends upon
;;                  | the current input/output mode, which please
;;                  | consult alow.
;;   ..................................................................
;;   1              | Queries the standard input for a Unicode
;;                  | character or an integer number and returns the
;;                  | response, either as the character's code point or
;;                  | as the number's ipsissima verba form.
;;                  |--------------------------------------------------
;;                  | The concrete behavior of the input inquisition
;;                  | depends upon the current input/output mode, which
;;                  | please consult alow.
;;   ..................................................................
;;   variable < 1   | Queries the standard input for a Unicode
;;   ^^^^^^^^       | character or an integer number and stores the
;;                  | response, either as the character's code point or
;;                  | as the number's ipsissima verba form, in the
;;                  | {variable}.
;;                  |--------------------------------------------------
;;                  | The concrete behavior of the input inquisition
;;                  | depends upon the current input/output mode, which
;;                  | please consult alow.
;;   ------------------------------------------------------------------
;; 
;; == THE INPUT/OUTPUT MODE: CHOICE BETWIXT CHARACTERS AND NUMBERS ==
;; The quandary whose involvement appertains to the bifurcation into
;; character-based and numeric input and output operations in {}s has
;; been meliorated by the input/output mode, a flag whence is derived
;; the concrete case's system response.
;; 
;; This mode wists of a twissel of states, namely the character and the
;; numeric specimen, to the former of which is reserves the inchoation's
;; settlement. Upon the "#" instruction's invocation, the flag toggles
;; its status, as is extricable from the following tabulation:
;; 
;;   -------------------------------------
;;   Input/Output mode | Mode after switch
;;   ------------------+------------------
;;   character         | numeric
;;   .....................................
;;   numeric           | character
;;   -------------------------------------
;; 
;; Resident in the character mode, an input request incorporates the
;; standard input's imploration of a Unicode character's specification,
;; the integral code point of which is returned. During an output
;; behest's issuance, the numeric expression experiences its assessment
;; in the form of a Unicode code point, whence, as a consectary, the
;; allied character displays.
;; 
;; The numeric state, on the other hand, expects its optation for a
;; signed or unsigned integer's reception to be expressed in this exact
;; format, the same conflates with the returned object. Vice versa, the
;; printing operation merely write to the standard output the
;; expression's verbatim numeric design.
;; 
;; A parlecue nuncupated to these conduits' and modes' champarty shall
;; be the coming tabular illustration's dever:
;; 
;;   ------------------------------------------------------------------
;;   Action | I/O mode  | Result
;;   -------+-----------+----------------------------------------------
;;   Input  | Character | Queries the standard input for a character
;;          |           | and returns its Unicode code point.
;;          |..........................................................
;;          | Numeric   | Queries the standard input for a signed or
;;          |           | unsigned integer number and returns its
;;          |           | immediate form.
;;   ==================================================================
;;   Output | Character | Construes the numeric expression to print as
;;          |           | a Unicode code point and displays the
;;          |           | character associated with the same.
;;          |..........................................................
;;          | Numeric   | Directly prints the numeric expression's
;;          |           | immediate form.
;;   ------------------------------------------------------------------
;; 
;; 
;; Instructions
;; ============
;; In its most basic construe, {}s's instruction set accounts for a
;; quintuple membership, valorized via the contingency for unary and
;; binary expressions, which in their coefficiency operate upon
;; variables, actuate the input and output commerce, and proffer a
;; while-based iterance construct.
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity's adhibition in reference to the language's
;; operative capacitations shall be constitute this apercu's dation.
;; 
;; Please heed the succedaneous segments' designation via an underline
;; compact of carets ("^"), the same expect a supersession by actual
;; {}s code in the program's ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ------------------------+-----------------------------------------
;;   %                       | Represents the literal integer two (2).
;;   ..................................................................
;;   varName                 | References the variable amenable to the
;;   ^^^^^^^                 | {varName}.
;;                           |-----------------------------------------
;;                           | {varName} must be an unsigned integer
;;                           | number.
;;   ..................................................................
;;   varName < value         | Assigns the {value} to the variable
;;   ^^^^^^^   ^^^^^         | amenable to the identifier {varName}.
;;                           |-----------------------------------------
;;                           | {varName} must be an unsigned integer
;;                           | number.
;;   ..................................................................
;;   [subject, {statements}] | While the {subject} does not resolve to
;;    ^^^^^^^   ^^^^^^^^^^   | the value zero (0), repeatedly executes
;;                           | the {statements}.
;;                           |-----------------------------------------
;;                           | {statements} must be an ordered sequence
;;                           | encompassing zero or more statements or
;;                           | expressions.
;;   ..................................................................
;;   #                       | Switches betwixt the input/output mode
;;                           | betwixt the character-based and numeric
;;                           | form, commencing in the former.
;;                           |-----------------------------------------
;;                           | If commorant in the character-based
;;                           | input/output mode, the program queries
;;                           | for a Unicode character and answers with
;;                           | its code point, while responding to
;;                           | output requests via a numeric value's
;;                           | interpretation as a code point in order
;;                           | to display the affiliated character. An
;;                           | application in the numeric mode expects
;;                           | a signed or unsigned integer literal's
;;                           | provision by the standard input, and
;;                           | prints its verbatim form to the standard
;;                           | output conduit.
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS ==
;; A rather magnanimous degree of unary and binary operations from the
;; provenances of arithmetics, relations, and logic, tallies among the
;; {}s language's expression constituents.
;; 
;; Two arithmetic and one logical member intrine in the formation of
;; the unary competences, as extricable from the alow adduction:
;; 
;;   ------------------------------------------------------------------
;;   Unary operator | Effect
;;   ---------------+--------------------------------------------------
;;   + operand      | UNARY PLUS: A neutral operator which does not
;;     ^^^^^^^      | modulate the {operand} expression.
;;   ..................................................................
;;   - operand      | UNARY MINUS: Negates the {operand}'s signum.
;;     ^^^^^^^      | 
;;   ..................................................................
;;   ! operand      | LOGICAL NOT: Inverts the {operand} in its
;;     ^^^^^^^      | perspective as a Boolean truth value, producing
;;                  | for a non-zero {operand} the value zero (0);
;;                  | otherwise, for a zero (0) input, returns one (1).
;;   ------------------------------------------------------------------
;; 
;; Endowed with a paravaunt numeric capacity, the binary operations
;; abide in the expectancy of an amplified mete of significance's
;; assessment:
;; 
;;   ------------------------------------------------------------------
;;   Binary operator | Effect
;;   ----------------+-------------------------------------------------
;;   left + right    | ADDITION: Returns the sum of the {left} operand
;;   ^^^^   ^^^^^    | incremented by the {right} one.
;;   ..................................................................
;;   left - right    | SUBTRACTION: Returns the difference of the
;;   ^^^^   ^^^^^    | {left} operand decremented by the {right} one.
;;   ..................................................................
;;   left * right    | MULTIPLICATION: Returns the product of the
;;   ^^^^   ^^^^^    | {left} operand multiplied by the {right} one.
;;   ..................................................................
;;   left / right    | INTEGER DIVISION: Returns the quotion of the
;;   ^^^^   ^^^^^    | {left} operand divided by the {right} one and
;;                   | rounded up or down to the nearest integer.
;;   ==================================================================
;;   left = right    | EQUAL TO: Returns one (1) if the {left} operand
;;   ^^^^   ^^^^^    | equals the {right} operand, otherwise zero (0).
;;   ..................................................................
;;   left / right    | NOT EQUAL TO: Returns one (1) if the {left}
;;   ^^^^   ^^^^^    | operand does not equal the {right} operand,
;;                   | otherwise zero (0).
;;   ..................................................................
;;   left ~ right    | NOT EQUAL TO: Returns one (1) if the {left}
;;   ^^^^   ^^^^^    | operand is strictly less than the {right}
;;                   | operand, otherwise zero (0).
;;   ..................................................................
;;   left $ right    | NOT EQUAL TO: Returns one (1) if the {left}
;;   ^^^^   ^^^^^    | operand is strictly greater than the {right}
;;                   | operand, otherwise zero (0).
;;   ==================================================================
;;   left & right    | LOGICAL AND: Returns one (1) if both the {left}
;;   ^^^^   ^^^^^    | operand and the {right} operand are non-zero,
;;                   | otherwise responds with zero (0).
;;   ..................................................................
;;   left @ right    | LOGICAL OR: Returns one (1) if either the {left}
;;   ^^^^   ^^^^^    | operand or the {right} operand or both are
;;                   | non-zero, otherwise responds with zero (0).
;;   ..................................................................
;;   left | right    | LOGICAL XOR: Returns one (1) if either the
;;   ^^^^   ^^^^^    | {left} operand or the {right} operand, but not
;;                   | both, are non-zero, otherwise responds with
;;                   | zero (0).
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
;;   {...}    | 0             | none          | Effectively endowed
;;            |               |               | with the highest
;;            |               |               | binding power.
;;   ..................................................................
;;   <        | 0             | none          | A statement in this
;;            |               |               | language rather than
;;            |               |               | being an expression.
;;   ..................................................................
;;   @        | 40            | left-to-right | 
;;   ..................................................................
;;   &        | 50            | left-to-right | 
;;   ..................................................................
;;   |        | 70            | left-to-right | 
;;   ..................................................................
;;   =        | 90            | left-to-right | 
;;   ..................................................................
;;   \        | 90            | left-to-right | 
;;   ..................................................................
;;   ~        | 100           | left-to-right | 
;;   ..................................................................
;;   $        | 100           | left-to-right | 
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
;;   ..................................................................
;;   !        | 170           | right-to-left | 
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-05-15
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
;;   [esolang2021()s]
;;   The Esolang contributors, "()s", July 23rd, 2021
;;   URL: "https://esolangs.org/wiki/()s"
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
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Stream integer) AST-Node)
                parse-expression))

(declaim (ftype (function (Token-Stream) Block-Node)
                parse-statement-block))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean truth value as a representation of the OBJECT,
   deriving its response from the \"generalized boolean\"
   interpretation's basis for such by responding with a ``boolean''
   value of ``T'' for a non-``NIL'' input; otherwise, for a ``NIL''
   OBJECT, producing the ``NIL'' sentinel."
  (declare (type T object))
  (the boolean
    (not (null object))))

;;; -------------------------------------------------------

(defun get-integral-truth-value-of (object)
  "Returns a bit value tantamount to a Boolean truth value
   representation of the OBJECT, deriving its response from the
   \"generalized boolean\" interpretation's basis for such by responding
   with zero (0), the numeric equivalent of the Boolean \"true\", for a
   non-``NIL'' input; otherwise, for a ``NIL'' OBJECT, producing the
   value one (1), a paregal of the Boolean \"false\" sentinel."
  (declare (type T object))
  (the bit
    (or (and object 1)
        0)))

;;; -------------------------------------------------------

(defun boolean-xor (left-operand right-operand)
  "Supputates a Boolean truth value representing the exclusive OR (XOR)
   combination of the LEFT-OPERAND and the RIGHT-OPERAND, returning a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type boolean left-operand))
  (declare (type boolean right-operand))
  (the boolean
    (not (eq left-operand right-operand))))

;;; -------------------------------------------------------

(defun integral-xor (left-operand right-operand)
  "Supputates the logical exclusive OR (XOR) on the two integer numbers
   LEFT-OPERAND and RIGHT-OPERAND, returning one (1) if and only if
   either the LEFT-OPERAND or the RIGHT-OPERAND, but never both, amount
   to one (1), othrwise responds with zero (0)."
  (declare (type integer left-operand))
  (declare (type integer right-operand))
  (the (integer 0 1)
    (cond
      ((and (zerop left-operand)
            (not (zerop right-operand)))
        1)
      ((and (not (zerop left-operand))
            (zerop right-operand))
        1)
      (T
        0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a new derived type founded upon the ``deftype''
   infrastructure in champarty with the ``satisfies'' predicate, and
   stevened by the TYPE-NAME, while its formal parameters are
   appropriated in an ipsissima verba fashion from the LAMBDA-LIST, the
   implementation evaluating the BODY forms, granted adit to the subject
   of this docimasy by the CANDIDATE-VARIABLE's agnomination, and
   interpreting the desinent BODY form's primary value as the ultimate
   assessment in terms of a \"generalized boolean\", a non-``NIL''
   value's provision being construed as an averment of candidate's
   covenableness, whereas the ``NIL'' sentinel determines its
   refutation.
   ---
   The first BODY form, if representing a string object, is interpreted
   as the derived type's documentation string and, as a corollary,
   reappropriated for this purpose."
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

;;; -------------------------------------------------------

(defun every-element-is-of-type-p (probed-list element-type)
  "Determines whether every element in the PROBED-LIST conforms to the
   ELEMENT-TYPE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type list probed-list))
  (declare (type T    element-type))
  (the boolean
    (get-boolean-value-of
      (every
        #'(lambda (element)
            (declare (type T element))
            (typep element element-type))
        probed-list))))

;;; -------------------------------------------------------

(defmacro every-hash-table-entry-p
    ((key-variable value-variable) hash-table
     &body body)
  "Determines whether the HASH-TABLE satisfies the predicate imposed by
   the BODY forms, probing each entry by subjecting its key, committed
   as the KEY-VARIABLE, and the allied value, norned by the
   VALUE-VARIABLE, to the BODY forms, the desinent form of which must
   return a \"generalized boolean\" value of \"true\" for the respective
   key-value twain's covenableness, otherwise ``NIL'', with this
   operation returning for a constant and ubiquitous satisfaction a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (let ((evaluated-table (gensym)))
    (declare (type symbol evaluated-table))
    `(let ((,evaluated-table ,hash-table))
       (declare (type hash-table ,evaluated-table))
       (get-boolean-value-of
         (loop
           for ,key-variable
             of-type T
             being the hash-keys in ,evaluated-table
           using
             (hash-value ,value-variable)
           always
             ,@body)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, for both holds the default
   generic sentinel ``*''."
  (and
    (hash-table-p candidate)
    (every-hash-table-entry-p (key value)
        (the hash-table candidate)
      (and
        (or (eq key '*)
            (typep key key-type))
        (or (eq value '*)
            (typep value value-type))))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for
   which holds the default of the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (every-element-is-of-type-p candidate element-type))))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized variation on
   unary operators, irregardless of their subsumption itno the
   arithmetic or logical species."
  '(member :plus :minus :logical-not))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variation on
   binary operators, irregardless of their subsumption into the
   arithmetic, relational, or logical species."
  '(member
    :plus
    :minus
    :times
    :divide
    :equal-to
    :not-equal-to
    :greater-than
    :less-than
    :logical-and
    :logical-or
    :logical-xor))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list of abstract syntax
   tree (AST) nodes."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized variants of
   operator associativity properties, nuncupated to the resolution of
   precedence conflicts among members of equipollent binding power."
  '(member :none :left :right))

;;; -------------------------------------------------------

(deftype nud-processor ()
  "The ``nud-processor'' type defines a callback function dedicated to
   the parsing of a \"null denotation\" (nud) token, pursuing its
   transformation into an abstract syntax tree (AST) node, and realized
   by a function of two arguments, the first being the transformed nud
   token, the second a ``Token-Stream'' whence to obtain contingently
   required subsequent tokens, while the output ought to constitute an
   ``AST-Node'' representation of the thus processed nud token; the
   signature thus complying to:
     lambda (Token Token-Stream) => AST-Node"
  '(function (Token Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype led-processor ()
  "The ``led-processor'' type defines a callback function dedicated to
   the parsing of a \"left denotation\" (led) token, pursuing its
   transformation into an abstract syntax tree (AST) node, and realized
   by a function of three arguments, the first being the transformed led
   token, the second the already extant sinistral expression,
   communicated as an ``AST-Node'', the third a ``Token-Stream'' whence
   to obtain contingently required subsequent tokens, while the output
   ought to constitute an ``AST-Node'' representation of the thus
   processed led token; the signature thus complying to:
     lambda (Token AST-Node Token-Stream) => AST-Node"
  '(function (Token AST-Node Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype io-mode ()
  "The ``io-mode'' type enumerates the recognized options for the
   input and output modality's entelechy."
  '(member :character :numeric))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted during the lexical analyzation of a piece of {}s
   source code, modeled as a twissel of a categorizing type and a
   detailing value."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-type-p (probed-token expected-type)
  "Determines whether the PROBED-TOKEN's type conforms to the
   EXPECTED-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token   probed-token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type probed-token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   whose diorism' amplectation enumerates spaces, horizontal tabs, as
   well as the newline entity, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class applies itself to the lexical analyzation of a
   piece of {}s source code, pursuing the telos of its tokens'
   extraction."
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

(defmacro with-lexer
    ((lexer &optional (character-variable '$character)
                      (position-variable  '$position)
                      (source-variable    '$source))
     &body body)
  "Evaluates the LEXER, utilizing local symbols macros to bind its slot
   ``character'' to the CHARACTER-VARIABLE, the slot ``position'' to the
   POSITION-VARIABLE, and ``source'' to the SOURCE-VARIABLE, executes
   the BODY forms, and returns the desinent form's results.
   ---
   A parergon actuated to the slot bindings, a twain of local functions'
   establishment homologates a more comfortable perquisition and
   manipulation of the LEXER's state:
     ------------------------------------------------------------------
     Function                            | Effect
     ------------------------------------+-----------------------------
     (advance)                           | Advances the LEXER's
                                         | position cursor to the next
                                         | character in its source.
     ..................................................................
     (character-of-p expected-character) | Determines whether the
                                         | LEXER's current character
                                         | equals the
                                         | EXPECTED-CHARACTER.
     ------------------------------------------------------------------"
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           ((,character-variable
              (the (or null character)
                (lexer-character ,evaluated-lexer)))
            (,position-variable
              (the fixnum
                (lexer-position ,evaluated-lexer)))
            (,source-variable
              (the string
                (lexer-source ,evaluated-lexer))))
         (declare (type (or null character) ,character-variable))
         (declare (ignorable                ,character-variable))
         (declare (type fixnum              ,position-variable))
         (declare (ignorable                ,position-variable))
         (declare (type string              ,source-variable))
         (declare (ignorable                ,source-variable))
         (flet
             ((advance ()
               "Returns the LEXER's current character, while
                concomitantly advances its position cursor to the next
                location in its source."
               (the (or null character)
                 (prog1 ,character-variable
                   (setf ,character-variable
                     (when (array-in-bounds-p ,source-variable
                             (1+ ,position-variable))
                       (char ,source-variable
                         (incf ,position-variable)))))))
              
              (character-of-p (expected-character)
               "Determines whether the LEXER's currently selected
                character equals the EXPECTED-CHARACTER, returning on
                confirmation a ``boolean'' value of ``T'', otherwise
                ``NIL''."
               (declare (type character expected-character))
               (the boolean
                 (get-boolean-value-of
                   (and ,character-variable
                     (char= ,character-variable
                            expected-character))))))
           (declare (ftype (function () (or null character)) advance))
           (declare (ftype (function (character) boolean)
                    character-of-p))
           ,@body)))))

;;; -------------------------------------------------------

(defun read-symbol (lexer new-token-type)
  "Creates and returns a new token of the NEW-TOKEN-TYPE, allied with
   the LEXER's current character, while concomitantly advancing its
   position cursor to the next location in its source."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token new-token-type
        (advance)))))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an unsigned integer literal and returns a ``:number'' token
   representation of the obtained object."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and $character (digit-char-p $character)) do
              (write-char (advance) digits))))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns the
   contingently modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and $character (whitespace-character-p $character)) do
      (advance)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ;; Program boundary markers.
        ((null $character)
          (make-eof-token))
        
        ;; Neglected content.
        ((whitespace-character-p $character)
          (get-next-token
            (skip-whitespaces lexer)))
        
        ;; Grouping symbols.
        ((character-of-p #\{)
          (read-symbol lexer :left-brace))
        ((character-of-p #\})
          (read-symbol lexer :right-brace))
        ((character-of-p #\[)
          (read-symbol lexer :left-bracket))
        ((character-of-p #\])
          (read-symbol lexer :right-bracket))
        
        ;; Literals and variables.
        ((character-of-p #\%)
          (read-symbol lexer :percentage))
        ((digit-char-p $character)
          (read-number lexer))
        
        ;; Assignments.
        ((character-of-p #\<)
          (read-symbol lexer :assignment))
        
        ;; Separators.
        ((character-of-p #\,)
          (read-symbol lexer :comma))
        
        ;; Arithmetic operators.
        ((character-of-p #\+)
          (read-symbol lexer :plus))
        ((character-of-p #\-)
          (read-symbol lexer :minus))
        ((character-of-p #\*)
          (read-symbol lexer :times))
        ((character-of-p #\/)
          (read-symbol lexer :divide))
        
        ;; Relational operators.
        ((character-of-p #\=)
          (read-symbol lexer :equal-to))
        ((character-of-p #\\)
          (read-symbol lexer :not-equal-to))
        ((character-of-p #\~)
          (read-symbol lexer :less-than))
        ((character-of-p #\$)
          (read-symbol lexer :greater-than))
        
        ;; Logical operators.
        ((character-of-p #\&)
          (read-symbol lexer :logical-and))
        ((character-of-p #\@)
          (read-symbol lexer :logical-or))
        ((character-of-p #\|)
          (read-symbol lexer :logical-xor))
        ((character-of-p #\!)
          (read-symbol lexer :logical-not))
        
        ;; Input/Output operators.
        ((character-of-p #\#)
          (read-symbol lexer :hash-sign))
        
        ;; Invalid content.
        (T
          (error "Unexpected character \"~c\" at position ~d."
            $character $position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token-Stream
  (:constructor make-token-stream
    (lexer &aux (next-token (get-next-token lexer)))))
  "The ``Token-Stream'' class furnishes a commodity for the eath and
   competent access to a ``Lexer'' instance's tokens, encompassing in
   its capacitations the consumption as well as looking ahead options
   for such elements."
  (lexer      (error "Missing lexer.")
              :type      Lexer
              :read-only T)
  (next-token (error "Missing next token.")
              :type      Token
              :read-only NIL))

;;; -------------------------------------------------------

(defun consume-token (tokens)
  "Returns and removes the next token from the token stream TOKENS,
   while concomitantly advancing to the next element."
  (declare (type Token-Stream tokens))
  (the Token
    (prog1
      (token-stream-next-token tokens)
      (setf (token-stream-next-token tokens)
        (get-next-token
          (token-stream-lexer tokens))))))

;;; -------------------------------------------------------

(defun peek-token (tokens)
  "Returns without removing the next token from the token stream
   TOKENS."
  (declare (type Token-Stream tokens))
  (the Token
    (token-stream-next-token tokens)))

;;; -------------------------------------------------------

(defun expect-token (tokens expected-token-type)
  "Determines whether the next token in the TOKENS stream conforms to
   the EXPECTED-TOKEN-TYPE, returning on confirmation the probed token,
   while concomitantly consuming the same; otherwise signals an error of
   an unspecfied type."
  (declare (type Token-Stream tokens))
  (declare (type keyword      expected-token-type))
  (the Token
    (if (token-is-of-type-p (peek-token tokens) expected-token-type)
      (consume-token tokens)
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (peek-token tokens)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Precedence".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence         (binding-power associativity))
  (:constructor make-neutral-precedence (&aux (binding-power 0)
                                              (associativity :none))))
  "The ``Precedence'' class serves in the encapsulation of an operator's
   precedence, compact of the twissel of binding power and associativity
   which in coefficiency define the effective puissance."
  (binding-power (error "Missing binding power.")
                 :type      integer
                 :read-only T)
  (associativity (error "Missing associativity.")
                 :type      associativity
                 :read-only T))

;;; -------------------------------------------------------

(defun get-effective-binding-power (precedence)
  "Returns the PRECEDENCE's effective binding power, supputated as a
   variation on the basic power and its modulation via the associativity
   status."
  (declare (type Precedence precedence))
  (the integer
    (- (precedence-binding-power precedence)
      (case (precedence-associativity precedence)
        ((:none :left) 0)
        (:right        1)
        (otherwise
          (error "Invalid associativity: ~s."
            (precedence-associativity precedence)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface serves in the instalment of a common base
   for all concrete classes pursuing the representation of {}s language
   facilities in the guise of abstract syntax tree (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Expression-Node
  (:include AST-Node))
  "The ``Expression-Node'' interface extends the ``AST-Node'' interface
   in order to establish a common foundry for all classes pursuing the
   representation of {}s expressions in the mold of abstract syntax tree
   (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include Expression-Node))
  "The ``Binary-Operation-Node'' class applies itself to the
   encapsulation of a {}s binary operation in the form of an abstract
   syntax tree (AST) node, comprehending both the twissel of operands
   and the connecting operator."
  (operator      (error "Missing binary operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      Expression-Node
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      Expression-Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include AST-Node))
  "The ``Block-Node'' class serves in the encapsulation of an ordered
   list of abstract syntax tree (AST) nodes forming a logical and
   physical compound."
  (statements (error "Missing statements.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Group-Node
  (:include Expression-Node))
  "The ``Group-Node'' class envelopes in its amplectation an expression
   as a parenthesized compound."
  (expression (error "Missing grouped expression.")
              :type      Expression-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Literal-Node
  (:include Expression-Node))
  "The ``Literal-Node'' class encapsulates a literal object in an
   abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Output-Mode-Node
  (:include AST-Node))
  "The ``Output-Mode-Node'' class represents a behest to toggle the
   output mode as an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include AST-Node))
  "The ``Program-Node'' class serves in the ensconcement of a {}s
   program's entry point as the repesentative abstract syntax tree's
   (AST) root node."
  (statements (error "Missing program node statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Operation-Node
  (:include Expression-Node))
  "The ``Unary-Operation-Node'' class applies itself to the
   representation of a {}s unary operation in the guise of an abstract
   syntax tree (AST) node."
  (operator (error "Missing unary operator.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing operand.")
            :type      AST-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Node
  (:include Expression-Node))
  "The ``Variable-Node'' class encapsulates variable reference in an
   abstract syntax tree (AST) node."
  (identifier (error "Missing variable identifier.")
              :type      integer
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Assignment-Node
  (:include AST-Node))
  "The ``Assignment-node'' class appropriates the wike of a variable
   assignment's representation, embracing in its composition the target
   placeholder and its intended value."
  (target (error "Missing assignment target.")
          :type      Variable-Node
          :read-only T)
  (value  (error "Missing assignment value.")
          :type      Expression-Node
          :read-only T))

;;; -------------------------------------------------------

(defstruct (While-Loop-Node
  (:include AST-Node))
  "The ``While-Loop-Node'' class encapsulates a while loop in the form
   of an abstract syntax tree (AST) node, composed of the indagated
   subject and an ordered list of body statements."
  (subject    (error "Missing while loop subject.")
              :type      Expression-Node
              :read-only T)
  (statements (error "Missing while loop body.")
              :type      Block-Node
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interface "Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ()
  (:documentation
    "The ``Parselet'' interface establishes a common foundry for all
     classes intent on the implementation of parselets in the context of
     Pratt parsing, such proceeds from the definition of a small parser
     dedicated to a token's transformation into an abstract syntax tree
     (AST) node."))

;;; -------------------------------------------------------

(defgeneric get-precedence (parselet)
  (:documentation
    "Returns the precedence associated with the PARSELET, or signals an
     error of an unspecified type upon its disrespondency."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interface "Nud-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nud-Parselet (Parselet)
  ()
  (:documentation
    "The ``Nud-Parselet'' interface furnishes a foundry for all
     parselets appertaining to the processing of nud tokens."))

;;; -------------------------------------------------------

(defgeneric apply-nud-parselet (parselet nud-token tokens)
  (:documentation
    "Invokes the nud PARSELET in order to process the NUD-TOKEN,
     employing the token stream TOKENS for further tokens' contingent
     obentention, and returns an abstract syntax tree (AST) node
     representation of the transformation process."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Group-Parselet".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Group-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Group-Parselet'' class attends to the wike of parsing a
     grouped expression in order to accommodate a ``Group-Node''
     representation of the unified composite."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Group-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Group-Parselet parselet))
  (declare (ignore              parselet))
  (declare (type Token          nud-token))
  (declare (ignore              nud-token))
  (declare (type token-Stream   tokens))
  (the Group-Node
    (make-group-node :expression
      (prog1
        (parse-expression tokens 0)
        (expect-token     tokens :right-brace)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Literal-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Literal-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Literal-Parselet'' furnishes a parselet accommodated to the
     transformation of a literal object token into a
     ``Literal-Node''."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Literal-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Literal-Parselet parselet))
  (declare (ignore                parselet))
  (declare (type Token            nud-token))
  (declare (ignore                nud-token))
  (declare (type token-Stream     tokens))
  (declare (ignore                tokens))
  (the Literal-Node
    (make-literal-node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Variable-Parselet".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Variable-Parselet'' furnishes a parselet accommodated to the
     transformation of a variable token into a ``Variable-Node''."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Variable-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Variable-Parselet parselet))
  (declare (ignore                 parselet))
  (declare (type Token             nud-token))
  (declare (type token-Stream      tokens))
  (declare (ignore                 tokens))
  (the Variable-Node
    (make-variable-node :identifier
      (token-value nud-token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Unary-Operation-Parselet".          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Unary-Operation-Parselet (Nud-Parselet)
  ((operator
    :initarg       :operator
    :initform      (error "Missing unary operator.")
    :type          unary-operator
    :documentation "The unary operator.")
   (precedence
    :initarg       :precedence
    :initform      (error "Missing unary operator precedence.")
    :reader        get-precedence
    :type          Precedence
    :documentation "The unary operator's precedence."))
  (:documentation
    "The ``Unary-Operation-Parselet'' class applies itself to the wike
     of producing by a prefix operation token's transformation an
     abstract syntax tree (AST) node representation."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Unary-Operation-Parselet)
                               (nud-token Token)
                               (tokens    Token-Stream))
  (declare (type Unary-Operation-Parselet parselet))
  (declare (type Token                    nud-token))
  (declare (ignore                        nud-token))
  (declare (type token-Stream             tokens))
  (the Unary-Operation-Node
    (make-unary-operation-node
      :operator (slot-value parselet 'operator)
      :operand  (parse-expression tokens
                  (get-effective-binding-power
                    (get-precedence parselet))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Led-Parselet".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Led-Parselet (Parselet)
  ((precedence
    :initarg       :precedence
    :initform      (error "Missing led parselet precedence.")
    :reader        get-precedence
    :type          Precedence
    :documentation "The represented operator's precedence."))
  (:documentation
    "The ``Led-Parselet'' abstract class defines a substrate for all
     classes pursuing the transformation of led tokens into abstract
     syntax tree (AST) nodes, offering as the sole point of ligature
     a mandatory precedence."))

;; --------------------------------------------------------

(defgeneric apply-led-parselet (parselet led-token left-node tokens)
  (:documentation
    "Invokes the led PARSELET in order to process the LED-TOKEN,
     supplying the LEFT-NODE as the sinistral operand, and employing the
     token stream TOKENS for further tokens' contingent obentention,
     and returns an abstract syntax tree (AST) node representation of
     the transformation process."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Binary-Operation-Node".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-Operation-Parselet (Led-Parselet)
  ((operator
    :initarg       :operator
    :initform      (error "Missing binary operator.")
    :type          binary-operator
    :documentation "The binary operator."))
  (:documentation
    "The ``Binary-Operation-Parselet'' class implements a parselet to
     whom the onus is apportioned to transform a binary operator and
     the circumambient operand twissel into a
     ``Binary-Operation-Node''."))

;;; -------------------------------------------------------

(defmethod apply-led-parselet ((parselet  Binary-Operation-Parselet)
                               (led-token Token)
                               (left-node AST-Node)
                               (tokens    Token-Stream))
  (declare (type Binary-Operation-Parselet parselet))
  (declare (type Token                     led-token))
  (declare (ignore                         led-token))
  (declare (type AST-Node                  left-node))
  (declare (type Token-Stream              tokens))
  (the Binary-Operation-Node
    (make-binary-operation-node
      :operator      (slot-value parselet 'operator)
      :left-operand  left-node
      :right-operand (parse-expression tokens
                       (get-effective-binding-power
                         (get-precedence parselet))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of nud parselet registry.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Nud-Parselet) +NUD-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +NUD-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized nud token types with representative nud
   parselets.")

;;; -------------------------------------------------------

(defun register-nud-parselet (token-type parselet)
  "Associates the nud PARSELET with the TOKEN-TYPE and returns no
   value."
  (declare (type keyword      token-type))
  (declare (type nud-Parselet parselet))
  (setf (gethash token-type +nud-PARSELETS+) parselet)
  (values))

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
  "Returns the nud parselet associated with the TOKEN, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the nud-Parselet
    (or (gethash (token-type token) +nud-PARSELETS+)
        (error "No nud parselet is associated with the token ~s."
          token))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (token)
  "Returns the effective binding power associated with the nud TOKEN, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the integer
    (get-effective-binding-power
      (get-precedence
        (get-nud-parselet token)))))

;;; -------------------------------------------------------

(defun parse-nud-token (token tokens)
  "Parses the nud TOKEN utilizing the token stream TOKENS and returns an
   abstract syntax tree (AST) node representation of the result."
  (declare (type Token        token))
  (declare (type Token-Stream tokens))
  (the AST-Node
    (apply-nud-parselet
      (get-nud-parselet token)
      token
      tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of led parselet registry.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Led-Parselet) +LED-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +LED-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized led token types with representative led
   parselets.")

;;; -------------------------------------------------------

(defun register-led-parselet (token-type parselet)
  "Associates the led PARSELET with the TOKEN-TYPE and returns no
   value."
  (declare (type keyword      token-type))
  (declare (type led-Parselet parselet))
  (setf (gethash token-type +LED-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-binary-operation-parselet (token-type
                                           binding-power
                                           associativity)
  "Creates a fresh ``Binary-Operation-Parselet'' whose operator
   conflates with the TOKEN-TYPE and whose precedence is yielded from a
   derivation from the BINDING-POWER and ASSOCIATIVITY's coeffiency,
   associates the led parselet with the TOKEN-TYPE, and returns no
   value."
  (declare (type binary-operator token-type))
  (declare (type integer         binding-power))
  (declare (type associativity   associativity))
  (register-led-parselet token-type
    (make-instance 'Binary-Operation-Parselet
      :operator   token-type
      :precedence (make-precedence binding-power associativity)))
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
  "Returns the led parselet associated with the TOKEN, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the led-Parselet
    (or (gethash (token-type token) +LED-PARSELETS+)
        (error "No led parselet is associated with the token ~s."
          token))))

;;; -------------------------------------------------------

(defun get-led-binding-power (token)
  "Returns the effective binding power associated with the led TOKEN, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the integer
    (get-effective-binding-power
      (get-precedence
        (get-led-parselet token)))))

;;; -------------------------------------------------------

(defun parse-led-token (token left-node tokens)
  "Parses the led TOKEN utilizing the LEFT-NODE as its sinistral
   operand and the token stream TOKENS and returns an abstract syntax
   tree (AST) node representation of the result."
  (declare (type Token        token))
  (declare (type AST-Node     left-node))
  (declare (type Token-Stream tokens))
  (the AST-Node
    (apply-led-parselet
      (get-led-parselet token)
      token
      left-node
      tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Pratt expression parser.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (tokens current-binding-power)
  "Parses an expression from the available TOKENS, imposing the
   CURRENT-BINDING-POWER for the fortitude governing the operand
   cohension."
  (declare (type Token-Stream tokens))
  (declare (type integer      current-binding-power))
  
  (let ((left-node (parse-nud-token (consume-token tokens) tokens)))
    (declare (type AST-Node left-node))
    
    (loop
      for next-token of-type Token = (peek-token tokens)
      
      if (not (led-token-p next-token)) do
        (loop-finish)
      
      else if (<= (get-led-binding-power next-token)
                  current-binding-power) do
        (loop-finish)
      
      else do
        (consume-token tokens)
        (setf left-node
          (parse-led-token next-token left-node tokens)))
    
    (the AST-Node left-node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-nud-parselet :percentage
  (make-instance 'Literal-Parselet))

(register-nud-parselet :number
  (make-instance 'Variable-Parselet))

(register-nud-parselet :plus
  (make-instance 'Unary-Operation-Parselet
    :operator   :plus
    :precedence (make-precedence 170 :right)))

(register-nud-parselet :minus
  (make-instance 'Unary-Operation-Parselet
    :operator   :minus
    :precedence (make-precedence 170 :right)))

(register-nud-parselet :logical-not
  (make-instance 'Unary-Operation-Parselet
    :operator   :logical-not
    :precedence (make-precedence 170 :right)))

(register-nud-parselet :left-brace
  (make-instance 'Group-Parselet))

(register-binary-operation-parselet :logical-or    40 :left)
(register-binary-operation-parselet :logical-and   50 :left)
(register-binary-operation-parselet :logical-xor   70 :left)

(register-binary-operation-parselet :equal-to      90 :left)
(register-binary-operation-parselet :not-equal-to  90 :left)

(register-binary-operation-parselet :less-than    100 :left)
(register-binary-operation-parselet :greater-than 100 :left)

(register-binary-operation-parselet :plus         130 :left)
(register-binary-operation-parselet :minus        130 :left)
(register-binary-operation-parselet :times        140 :left)
(register-binary-operation-parselet :divide       140 :left)

;;; -------------------------------------------------------

(defun parse-while-loop (tokens)
  "Parses a \"while\" iterance construct from the TOKENS and returns a
   ``While-Loop-Node'' representation thereof."
  (declare (type Token-Stream tokens))
  (expect-token tokens :left-bracket)
  (let ((subject (parse-expression tokens 0)))
    (declare (type Expression-Node subject))
    (expect-token tokens :comma)
    (expect-token tokens :left-brace)
    (the While-Loop-Node
      (make-while-loop-node
        :subject    subject
        :statements
          (prog1
            (parse-statement-block tokens)
            (expect-token          tokens :right-brace)
            (expect-token          tokens :right-bracket))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of statement and program parsing operations.  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-statement (tokens)
  "Parses a single {}s statement based upon the TOKENS and returns a
   connable ``AST-Node'' representation of the detected facility."
  (declare (type Token-Stream tokens))
  (let ((probed-token (peek-token tokens)))
    (declare (type Token probed-token))
    (the (or null AST-Node)
      (case (token-type probed-token)
        (:left-bracket
          (parse-while-loop tokens))
        
        (:hash-sign
          (consume-token tokens)
          (make-output-mode-node))
        
        (otherwise
          (when (nud-token-p probed-token)
            (let ((expression (parse-expression tokens 0)))
              (declare (type Expression-Node expression))
              (cond
                ;; Expression followed by assignment?
                ((token-is-of-type-p (peek-token tokens) :assignment)
                  (consume-token tokens)
                  (let ((assigned-value (parse-expression tokens 0)))
                    (declare (type Expression-Node assigned-value))
                    (make-assignment-node
                      :target expression
                      :value  assigned-value)))
                ;; Stand-alone expression?
                (T
                  expression)))))))))

;;; -------------------------------------------------------

(defun parse-statement-list (tokens)
  "Parses and returns an ordered list composed of zero or more {}s
   statements from the TOKENS."
  (declare (type Token-Stream tokens))
  (the node-list
    (loop
      for statement
        of-type (or null AST-Node)
        =       (parse-statement tokens)
      while   statement
      collect statement)))

;;; -------------------------------------------------------

(defun parse-statement-block (tokens)
  "Parses and returns a on ordered list of zero or more {}s statements
   from the TOKENS and returns a ``Block-Node'' representation thereof."
  (declare (type Token-Stream tokens))
  (the Block-Node
    (make-block-node :statements
      (parse-statement-list tokens))))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Parses a {}s program by adminiculum of the TOKENS and returns a
   ``Program-Node'' representation of its statements."
  (declare (type Token-Stream tokens))
  (the Program-Node
    (prog1
      (make-program-node :statements
        (parse-statement-block tokens))
      (expect-token tokens :eof))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of {}s objects.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Sets-Object)
  "The ``Sets-Object'' interface serves in a common foundry's
   edification for concrete classes whose purpose's entelechy appertains
   to the representation of objects complying to specific species in a
   {}s program.")

;;; -------------------------------------------------------

(defstruct (Sets-Boolean
  (:include     Sets-Object)
  (:constructor make-sets-boolean (value)))
  "The ``Sets-Boolean'' class represents the notion of a Boolean truth
   value in the {}s programming language's context."
  (value (error "Missing value.") :type boolean :read-only T))

;;; -------------------------------------------------------

(defstruct (Sets-Number
  (:include     Sets-Object)
  (:constructor make-sets-number (value)))
  "The ``Sets-Number'' class represents the notion of a signed integer
   number in the {}s programming language's context."
  (value (error "Missing value.") :type integer :read-only T))

;;; -------------------------------------------------------

(defgeneric get-sets-object-value (sets-object)
  (:documentation
    "Returns the value from the SETS-OBJECT's ensconcement.")
  
  (:method ((sets-object Sets-Boolean))
    (declare (type Sets-Boolean sets-object))
    (the boolean
      (sets-boolean-value sets-object)))
  
  (:method ((sets-object Sets-Number))
    (declare (type Sets-Number sets-object))
    (the integer
      (sets-number-value sets-object))))

;;; -------------------------------------------------------

(defgeneric sets-object-is-true-p (sets-object)
  (:documentation
    "Determines whether the SETS-OBJECT represents a Boolean \"true\"
     value, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''.")
  
  (:method ((sets-object Sets-Boolean))
    (declare (type Sets-Boolean sets-object))
    (the boolean
      (sets-boolean-value sets-object)))
  
  (:method ((sets-object Sets-Number))
    (declare (type Sets-Number sets-object))
    (the boolean
      (not (zerop (sets-number-value sets-object))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Reference".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Reference
  (:constructor make-reference (target)))
  "The ``Reference'' class serves in the encapsulation of an object
   reference, in the common case communicated in the guise of a
   variable."
  (target (error "Missing reference target.")
          :type      integer
          :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Invokes the binary OPERATOR on the LEFT-OPERAND and the
     RIGHT-OPERAND in this exact order and yields a result convenable
     with this combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operation
    (operator (left-operand-type right-operand-type result-type)
     &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operator'', the first formal parameter of which is
   agnominated in an automatic fashion and dispatches via an
   ``eql''-specialization on the OPERATOR, the second argument is
   stevened ``$left-operand'' and specializes on the LEFT-OPERAND-TYPE,
   the desinent third input assumes the ``$right-operand'' stevening,
   with a concomitant acquisition of the RIGHT-OPERAND-TYPE for its
   specialization; the implementation subsequently appropriates the BODY
   forms, ensconced in an implicit ``progn'' form, and enveloped in a
   ``the'' special form that expresses the RESULT-TYPE as its output
   species."
  (let ((operator-variable (gensym)))
    (declare (type symbol operator-variable))
    `(defmethod apply-binary-operator
         ((,operator-variable (eql ,operator))
          ($left-operand      ,left-operand-type)
          ($right-operand     ,right-operand-type))
       (declare (type binary-operator     ,operator-variable))
       (declare (ignore                   ,operator-variable))
       (declare (type ,left-operand-type  $left-operand))
       (declare (ignorable                $left-operand))
       (declare (type ,right-operand-type $right-operand))
       (declare (ignorable                $right-operand))
       (the ,result-type
         (progn ,@body)))))

;;; -------------------------------------------------------

(define-binary-operation :divide (Sets-Number Sets-Number Sets-Number)
  (make-sets-number
    (round
      (get-sets-object-value $left-operand)
      (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :minus (Sets-Number Sets-Number Sets-Number)
  (make-sets-number
    (- (get-sets-object-value $left-operand)
       (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :plus (Sets-Number Sets-Number Sets-Number)
  (make-sets-number
    (+ (get-sets-object-value $left-operand)
       (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :times (Sets-Number Sets-Number Sets-Number)
  (make-sets-number
    (* (get-sets-object-value $left-operand)
       (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (Sets-Boolean
                                    Sets-Boolean
                                    Sets-Boolean)
  (make-sets-boolean
    (eq (get-sets-object-value $left-operand)
        (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (Sets-Number
                                    Sets-Number
                                    Sets-Number)
  (make-sets-number
    (get-integral-truth-value-of
      (= (get-sets-object-value $left-operand)
         (get-sets-object-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (Sets-Boolean
                                        Sets-Boolean
                                        Sets-Boolean)
  (make-sets-boolean
    (boolean-xor
      (get-sets-object-value $left-operand)
      (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (Sets-Number
                                        Sets-Number
                                        Sets-Number)
  (make-sets-number
    (get-integral-truth-value-of
      (/= (get-sets-object-value $left-operand)
          (get-sets-object-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (Sets-Number
                                     Sets-Number
                                     Sets-Number)
  (make-sets-number
    (get-integral-truth-value-of
      (< (get-sets-object-value $left-operand)
         (get-sets-object-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (Sets-Number
                                        Sets-Number
                                        Sets-Number)
  (make-sets-number
    (get-integral-truth-value-of
      (> (get-sets-object-value $left-operand)
         (get-sets-object-value $right-operand)))))

;;; -------------------------------------------------------

(define-binary-operation :logical-and (Sets-Boolean
                                       Sets-Boolean
                                       Sets-Boolean)
  (make-sets-boolean
    (and
      (get-sets-object-value $left-operand)
      (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :logical-and (Sets-Number
                                       Sets-Number
                                       Sets-Number)
  (make-sets-number
    (cond
      ((zerop (get-sets-object-value $left-operand))  0)
      ((zerop (get-sets-object-value $right-operand)) 0)
      (T                                              1))))

;;; -------------------------------------------------------

(define-binary-operation :logical-or (Sets-Boolean
                                      Sets-Boolean
                                      Sets-Boolean)
  (make-sets-boolean
    (or
      (get-sets-object-value $left-operand)
      (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :logical-or (Sets-Number
                                      Sets-Number
                                      Sets-Number)
  (make-sets-number
    (if (and (zerop (get-sets-object-value $left-operand))
             (zerop (get-sets-object-value $right-operand)))
      0
      1)))

;;; -------------------------------------------------------

(define-binary-operation :logical-xor (Sets-Boolean
                                       Sets-Boolean
                                       Sets-Boolean)
  (make-sets-boolean
    (boolean-xor
      (get-sets-object-value $left-operand)
      (get-sets-object-value $right-operand))))

;;; -------------------------------------------------------

(define-binary-operation :logical-xor (Sets-Number
                                       Sets-Number
                                       Sets-Number)
  (make-sets-number
    (integral-xor
      (get-sets-object-value $left-operand)
      (get-sets-object-value $right-operand))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-unary-operator (operator operand)
  (:documentation
    "Invokes the unary OPERATOR with the OPERAND as its aefauld argument
     and returns a result convenable for this combination."))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :logical-not))
                                 (operand  Sets-Boolean))
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type Sets-Boolean   operand))
  (the Sets-Boolean
    (make-sets-boolean
      (not (get-sets-object-value operand)))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :logical-not))
                                 (operand  Sets-Number))
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type Sets-Number    operand))
  (the Sets-Number
    (make-sets-number
      (if (zerop (get-sets-object-value operand))
        1
        0))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :minus))
                                 (operand  Sets-Number))
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type Sets-Number    operand))
  (the Sets-Number
    (make-sets-number
      (- (get-sets-object-value operand)))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :plus))
                                 (operand  Sets-Number))
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type Sets-Number    operand))
  (the Sets-Number
    (make-sets-number
      (get-sets-object-value operand))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing program tree.")
    :type          Program-Node
    :documentation "The abstract syntax tree (AST) representation of
                    the {}s program to evaluate.")
   (variables
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer Sets-Object)
    :documentation "Associates the assigned variables with their
                    respective values.")
   (io-mode
    :initform      :character
    :accessor      io-mode
    :type          io-mode
    :documentation "Determines whether input and output are committed
                    in a numeric or character form."))
  (:documentation
    "The ``Interpreter'' class is apportioned that onus to accompass
     actual efficacy to a {}s program supplied in the form of an
     abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a fresh ``Interpreter'' dedicated to the {}s
   program's abstract syntax TREE (AST) representation's execution."
  (declare (type Program-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun get-variable-value (interpreter name)
  "Returns the value of the variable registered with the NAME at the
   INTERPRETER, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type Interpreter interpreter))
  (declare (type integer     name))
  (the Sets-Object
    (or (gethash name (slot-value interpreter 'variables))
        (error "No variable with the name ~d found." name))))

;;; -------------------------------------------------------

(defun set-variable (interpreter name value)
  "Stores the VALUE in a variable designated by the NAME in the
   INTERPRETER, contingently superseding an already extant association
   amenable to this NAME, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     name))
  (declare (type Sets-Object value))
  (setf (gethash name (slot-value interpreter 'variables)) value)
  (values))

;;; -------------------------------------------------------

(defun switch-output-mode (interpreter)
  "Toggles the INTERPRETER's internally managed input/output mode to
   the obverse state and returns no value."
  (declare (type Interpreter interpreter))
  (setf (io-mode interpreter)
    (case (io-mode interpreter)
      (:character :numeric)
      (:numeric   :character)
      (otherwise
        (error "Invalid input/output mode to switch from: ~s."
          (io-mode interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun query-for-input (interpreter)
  "Queries the standard input for an object compatible with the
   INTERPRETER's contemporaneous input/output mode and returns a
   ``Sets-Number'' representation of the received response."
  (declare (type Interpreter interpreter))
  (the Sets-Number
    (case (io-mode interpreter)
      (:character
        (format T "~&Please input a character: ")
        (finish-output)
        (prog1
          (make-sets-number
            (char-code
              (read-char NIL NIL #\Null)))
          (clear-input)))
      (:numeric
        (format T "~&Please input a number: ")
        (finish-output)
        (prog1
          (make-sets-number
            (parse-integer
              (read-line NIL NIL 0)))
          (clear-input)))
      (otherwise
        (error "Invalid input mode: ~s."
          (io-mode interpreter))))))

;;; -------------------------------------------------------

(defgeneric resolve-object (interpreter object)
  (:documentation
    "Returns the OBJECT's value, encapsulated in a ``Sets-Object''
     subtype, in the INTERPRETER's context.")
  
  (:method ((interpreter Interpreter) (object Sets-Object))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type Sets-Object object))
    (the Sets-Object object))
  
  (:method ((interpreter Interpreter) (reference Reference))
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (the Sets-Number
      (if (= (reference-target reference) 1)
        (query-for-input interpreter)
        (get-variable-value interpreter
          (reference-target reference))))))

;;; -------------------------------------------------------

(defgeneric output-object (interpreter object)
  (:documentation
    "Prints a convenable representation of the OBJECT, construed in the
     INTERPRETER's context, to the standard output and returns no
     value.")
  
  (:method ((interpreter Interpreter)
            (object      Sets-Boolean))
    (declare (type Interpreter  interpreter))
    (declare (ignore            interpreter))
    (declare (type Sets-Boolean object))
    (format T "~:[false~;true~]"
      (get-sets-object-value object))
    (values))
  
  (:method ((interpreter Interpreter)
            (object      Sets-Number))
    (declare (type Interpreter interpreter))
    (declare (type Sets-Number object))
    (case (io-mode interpreter)
      (:character
        (format T "~c"
          (code-char
            (get-sets-object-value object))))
      (:numeric
        (format T " ~d "
          (get-sets-object-value object)))
      (otherwise
        (error "Invalid output mode: ~s."
          (io-mode interpreter))))
    (values))
  
  (:method ((interpreter Interpreter)
            (reference   Reference))
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (output-object interpreter
      (get-variable-value interpreter
        (reference-target reference)))
    (values)))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Processes the abstract syntax tree (AST) NODE in the INTERPRETER's
     context and returns a value convenable with this combination."))

;;; -------------------------------------------------------

(defmacro define-node-visitor ((node-class) &body body)
  "Defines an implementation of the generic function ``visit-node'', the
   first formal parameter of which is nevened ``$interpreter'', the
   second ``$node'', dispatching on the latter by adminiculum of the
   NODE-CLASS, the function's forms being composed of the BODY forms,
   with the desinent element's result constituting the return value or
   values."
  `(defmethod visit-node (($interpreter Interpreter)
                          ($node        ,node-class))
     (declare (type Interpreter $interpreter))
     (declare (ignorable        $interpreter))
     (declare (type ,node-class $node))
     (declare (ignorable        $node))
     ,@body))

;;; -------------------------------------------------------

(define-node-visitor (Assignment-Node)
  (let ((target (visit-node $interpreter
                  (assignment-node-target $node))))
    (declare (type (or Reference Sets-Object) target))
    (cond
      ;; TARGET is a ``Reference'' object?
      ;; => Assign the expression.
      ((reference-p target)
        (set-variable $interpreter
          (reference-target target)
          (resolve-object $interpreter
            (visit-node $interpreter
              (assignment-node-value $node))))
        
        ;; Print if value is assigned to the variable "0".
        (when (zerop (reference-target target))
          (output-object $interpreter
            (visit-node $interpreter
              (assignment-node-value $node)))))
      
      ;; TARGET is no ``Reference'' object?
      ;; => Cannot assign to such.
      (T
        (error "Cannot assign to the object ~s." target))))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Binary-Operation-Node)
  (the Sets-Object
    (apply-binary-operator
      (binary-operation-node-operator $node)
      (resolve-object $interpreter
        (visit-node $interpreter
          (binary-operation-node-left-operand $node)))
      (resolve-object $interpreter
        (visit-node $interpreter
          (binary-operation-node-right-operand $node))))))

;;; -------------------------------------------------------

(define-node-visitor (Block-Node)
  (dolist (statement (block-node-statements $node))
    (declare (type AST-Node statement))
    (visit-node $interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Group-Node)
  (the Sets-Object
    (visit-node $interpreter
      (group-node-expression $node))))

;;; -------------------------------------------------------

(define-node-visitor (Literal-Node)
  (the Sets-Number
    (make-sets-number 2)))

;;; -------------------------------------------------------

(define-node-visitor (Output-Mode-Node)
  (switch-output-mode $interpreter)
  (values))

;;; -------------------------------------------------------

(define-node-visitor (Program-Node)
  (visit-node $interpreter
    (program-node-statements $node)))

;;; -------------------------------------------------------

(define-node-visitor (Unary-Operation-Node)
  (the Sets-Object
    (apply-unary-operator
      (unary-operation-node-operator $node)
      (resolve-object $interpreter
        (visit-node $interpreter
          (unary-operation-node-operand $node))))))

;;; -------------------------------------------------------

(define-node-visitor (Variable-Node)
  (the Reference
    (make-reference
      (variable-node-identifier $node))))

;;; -------------------------------------------------------

(define-node-visitor (While-Loop-Node)
  (symbol-macrolet
      ((shall-continue-loop-p
        (sets-object-is-true-p
          (resolve-object $interpreter
            (visit-node $interpreter
              (while-loop-node-subject $node))))))
    (declare (type boolean shall-continue-loop-p))
    (loop while shall-continue-loop-p do
      (visit-node $interpreter
        (while-loop-node-statements $node))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the {}s program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-{}s (code)
  "Interprets the piece of {}s source CODE and returns no value."
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

;; One-time character-based cat program.
(interpret-{}s "2<1 0<2")

;;; -------------------------------------------------------

;; Repeating character-based cat program which terminates on a
;; "null character" input.
(interpret-{}s "2<1 [2, {0<2 2<1}]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-{}s "# 2<1 0<2 [2, {0<2}]")

;;; -------------------------------------------------------

;; Demonstrate addition by printing by the of 2+2, that is: 4.
(interpret-{}s "2<{%+%} # 0<2")

;;; -------------------------------------------------------

;; Looping counter which expects the tally of iterations to be supplied
;; by the user.
;; 
;; The following illustrates the concept, with V(n) being a variable
;; reference, where n constitutes an integer number designating the
;; variable's name:
;; 
;;   V(2) <- input  { Number of lines to print; equals repetitions.  }
;;   V(3) <- 1      { Tally of "*"s to print on current line.        }
;;   V(4) <- 0      { Tally of "*"s already printed on current line. }
;;   V(5) <- 42     { ASCII code of character "*".                   }
;;   V(6) <- 10     { ASCII code of newline character.               }
;;   
;;   while V(2) > 0 do
;;     V(4) <- 0
;;     
;;     while V(4) < V(3) do
;;       print "*"
;;       V(4) <- V(4) + 1
;;     end
;;     
;;     print newline
;;     
;;     V(2) <- V(2) - 1  { Decrement tally of remaining iterations.    }
;;     V(3) <- V(3) + 1  { Next line prints one "*" more than current. }
;;   end while
(interpret-{}s
  "
  #
  
  2 < 1
  3 < {% / %}
  4 < {% - %}
  5 < {{% * % * % * % * %} + {% * % * %} + %}
  6 < {{% + %  + {% / %}} * %}
  
  #
  
  [2 $ {% - %},
    {
      4 < {% - %}
      
      [4 ~ 3,
        {
          0 < 5
          4 < 4 + {% / %}
        }
      ]
      0 < 6
      
      2 < 2 - {% / %}
      3 < 3 + {% / %}
    }
  ]
  ")
