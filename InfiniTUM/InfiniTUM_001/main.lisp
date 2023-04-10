;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "InfiniTUM", presented by the Esolang user "iconmaster" in
;; the year 2011, conceived as a variation on the Turing machine,
;; yet distinguished by operating on an infinite loop which probes a
;; series of conditional blocks in order to indagate and modify the tape
;; head state, as well as the cell under the tape head.
;; 
;; 
;; Concept
;; =======
;; InfiniTUM's foundry relates to the abstract automaton known as the
;; Turing machine, whence it borrows the concepts of its haecceity;
;; natheless, deviating with conspicuous resourcefulness in certain
;; aspects.
;; 
;; == INFINITUM: AN "INFINITE TURING MACHINE" ==
;; The InfiniTUM programming language bewrays by the mode of its
;; nevening its afflation's provenance --- being an *infini*te *TU*ring
;; *M*achine, operating on an equally infinite tape, deploying a symbol
;; alphabet endowed with a similar extent in the integral space, and an
;; account of states subscribing to the same specification, the whole
;; program being ensconced in an infinite loop responsible for the
;; probing of a sequence of "if"-conditionals and their potential
;; execution.
;; 
;; == TURING MACHINE = STATE MACHINE + INFINITE TAPE + RULE SET ==
;; The Turing machine, an element of uttermost significance to computer
;; science, provides an illustrative diorism of a computer in abstract
;; terms, encompassing a state machine and its coefficiency with an
;; infinite tape, the deportment of which is regulated by a given rule
;; set.
;; 
;; == TURING MACHINE: A STATE MACHINE WITH A FINITE STATE ACCOUNT ==
;; One moeity of Turing's automaton employs a state machine, compact of
;; a finite tally of states, as such bound to the delinations applicable
;; to its commonly appropriated design, namely the inchoation in a start
;; state and the capability to translate into another one by mediation
;; of symbols along the conduits, nevened the transitions.
;; 
;; A certain set of states must be defined, known as the "final states",
;; whose transition into immediately halts the Turing machine.
;; 
;; == TURING MACHINE: AN INFINITE TAPE OF SYMBOLS ==
;; The second compartment, the tape, applies itself to the castaldy of
;; an infinite account of cells, linear in their ordonnance, and each
;; capable of storing a single symbol from a specified alphabet, or a
;; "blank" entity which aiblins most concinnously translates to a "null"
;; or "nil" value in modern computer language parlance.
;; 
;; == TURING MACHINE: THE TAPE CONTAINS INPUT AND OUTPUT ==
;; The tape cells are initialized with input symbols as an antecedent to
;; the program's executions, admitting their overwriting during the
;; actual run in act that relates to an output facility.
;; 
;; == TURING MACHINE: THE MOVABLE TAPE HEAD SELECTS THE ACTIVE CELL ==
;; The tape comprehends another feature: the active cell marker, clept
;; the "tape head", or simply "head". At any instant in the program, the
;; tape head designates the currently active cell from which to read a
;; symbol, and whose content consequently shall be replaced by a new
;; one. The tape head may engage during an execution cycle in exactly
;; one of the following reactions:
;; 
;;   (a) It moves one cell to the left.
;;   (b) It moves one cell to the right.
;;   (c) It does not move at all.
;; 
;; == TURING MACHINE: (STATE, SYMBOL) -> (STATE, SYMBOL, HEAD_MOVE) ==
;; In addition to the state machine and tape configuration, the
;; instigating agent must provide a rule set, such maps the valid
;; combinations of the current state machine state and current tape
;; symbol --- that under the tape head's location --- to the new state
;; to reach in the machine, the symbol by which to replace the current
;; cell's content, and the direction, if any, into which the tape head
;; shall move by one step following the successfully executed precedent
;; work.
;; 
;; Illustrated in an approximation of formality, a rule can be defined
;; as
;; 
;;   (currentState, currentSymbol) -> (newState, newSymbol, headMove)
;; 
;; == TURING MACHINE: READ, WRITE, MOVE, AND REPEAT ==
;; The machine subsequently operates in an iterative manner:
;; 
;;   (1) It indagates the current state machine state s[c].
;;   (2) It reads the symbol t[c] located under the tape's head.
;;   (3) Based upon the eligible rule from the rule set, the
;;       state-symbol tuple (s[c], t[c]) modifies the state machine,
;;       current cell's content, and tape head:
;;         (s[c], t[c]) -> (s[n], t[n], h)
;;       where
;;         s[c] --- the current state
;;         t[c] --- the symbol under the tape head
;;         s[n] --- the new state for the state machine
;;         t[n] --- the new symbol to write into the cell under the tape
;;                  head
;;         h    --- the direction to move the tape head into by one
;;                  step, that is, either "left", "right", or none.
;;   (4) If having arrived in one of the final states, which means
;;         s[n] is an element of F
;;       where
;;         F --- the set of final states,
;;       the program halts immediately.
;;       Otherwise, if the new state s[n] does not belong to the final
;;       states, the process repeats starting with the step (1).
;; 
;; == INFINITUM: TURING MACHINE + NEW RULES ==
;; The vinculum of the InfiniTUM language and its afflatus constitutes
;; a commorancy in a state machine's coefficiency with an infinite
;; tape --- maugre both constituents' adjustment to the novel warklume
;; of manipulation, deviating from the Turing machine's rule set in
;; being "if"-like conditions.
;; 
;; == INFINITUM: INFINITE INTEGER STATE MACHINE ==
;; An incipient emblem of adaptation, InfiniTUM employs an infinite
;; tally of states, congruent with, as manifest by, the signed integer
;; set's members. The states themselves are attributed to the tape head
;; in lieu of the traditionally state machine notion, an act with no
;; effectual consequence, but conceptually gravity.
;; 
;; == INFINITUM: A DIRECTION USURPS THE FINAL STATES'S COMPETENCE ==
;; From the rejection of particular states' diorism, in the traditional
;; Turing machine serving to establish with the final states a
;; transition into a program's abortion, ensues a requisitum to define
;; an alternative medium for this desideratum's communication. In lieu
;; of a dedicated state, a particular "direction" applies to this role,
;; nevened "X", assigned the onus of an immediate execution termination.
;; 
;; == INFINITUM: INFINITE INTEGER TAPE ==
;; Governing similiter, the tape lays its amplectation around an
;; infinite extent of cells, however, admitting as currency of input and
;; output only unbounded signed integers.
;; 
;; Concomitant to the tape head's appropriation from the abstract
;; automaton, the gradual translation is curtailed to the imperative of
;; motion, coercing a sinistral or dextral activity. The original option
;; involving a resting at the current location is ejected, superseded by
;; a special "X" direction, conveying the wish to stop the program, as
;; elucidated above.
;; 
;; == INFINITUM: NO TRADITIONAL RULES, MANY IF-STATEMENTS ==
;; Its haecceity's most kenspeckle parcel resides in the Turing machine
;; rule set definition's substitution by a sequence of zero or more
;; conditional blocks, akin to "if" statements, destitute, however, of
;; an "else" complement.
;; 
;; Such a conditional facility is delineated by four components:
;; 
;;   (1) A {condition} which determines whether the block, here
;;       compromising the steps (2) through (4), shall be executed at
;;       all.
;;   (2) A {stateExpression} setting the new state of the tape head.
;;   (3) A {valueExpression} designating the new value of the cell under
;;       the tape head.
;;   (4) The {direction} directive, either the airt to move the tape
;;       head into, or "X" as a signification of the program
;;       termination.
;; 
;; The general syntaxis amounts to:
;; 
;;   IF {condition}
;;     STATE {stateExpression}
;;     VALUE {valueExpression}
;;     MOVE  {direction}
;; 
;; The first three constituents, involving the steps (1) through (3),
;; are admitted access to the current tape head state in the form of the
;; variable yclept "s", as well as the current cell value as "v".
;; 
;; == INFINITUM: NO FINAL STATES, BUT AN INFINITE LOOP WITH AN "X" ==
;; Iterum deserting the stringent Turing machine diorism, an InfiniTUM
;; program's proceeding does not depend on the state machine's final
;; states. Programs in the esoteric language repeat in an infinite loop,
;; the cessation of which is instigated by a "MOVE" command invoked with
;; the special "X" parameter in lieu of a tape head translation airt.
;; 
;; 
;; Architecture
;; ============
;; InfiniTUM's construction founds upon two entities' champarty,
;; recognized as the objects of Turing machines' deliberation: a state
;; machine whose contingently boundless states are represented by
;; arbitrary integers, and a bilaterally infinite tape of integer-valued
;; cells, upon the same a head as the current unit's cursor operates.
;; 
;; == STATES ARE REPRESENTED BY INTEGERS ==
;; As counterdistinguished from the stringently mathematical diorism of
;; a Turing machine, a contingency for an infinite account of states is
;; accommodated in the InfiniTUM system. Each such participant's
;; definition proceeds from a single signed integer of any magnitude.
;; 
;; The initial state value resolves to one (1).
;; 
;; == THE TAPE IS COMPOSED OF AN INFINITE TALLY OF INTEGERS ==
;; Enhanced lealty to the formal automaton description can be attributed
;; to the tape in its infinite extent, its occupants being signed
;; integers cells without any impositions along both extremes, all
;; initialized to the default value zero (0).
;; 
;; A particular cursor, the "tape head", or simply "head", designates at
;; every instant the currently selected cell. A consequence of the
;; language's statements, this marker may be translated stepwise in the
;; sinistral or dextral airt along the tape.
;; 
;; 
;; Data Types
;; ==========
;; InfiniTUM's acquaintances among the data types enumerates integers,
;; unbounded in magnitude and sign, as its sole constituent.
;; 
;; 
;; Syntax
;; ======
;; InfiniTUM programs are constructed from an arbitrary tally of
;; statements, themselves four-lines conditional blocks, segregated by
;; linebreaks.
;; 
;; == STATEMENTS ==
;; A statement in InfiniTUM can be delineated as a block-like
;; composition enumerating four members:
;; 
;;   (a) the condition
;;   (b) the tape head state modifier
;;   (c) the tape head cell modifier
;;   (d) the tape head location or program flow modifier
;; 
;; Any such constituent is accommodated a line of its own and separated
;; by the adjacent participant via at least one newline.
;; 
;; An InfiniTUM program itself is compact of zero or more statements of
;; this ilk.
;; 
;; == NEWLINES ==
;; Linebreaks impose a requisitum to the arrangement of a statement,
;; with the three interstices betwixt each of the four components ---
;; condition, state, value, and move specifier --- dependent upon at
;; least one such sepiment. The quadruple constituents themselves may
;; not be intruded by newlines.
;; 
;; At any other instant linebreaks are equally homologated and ignored.
;; 
;; == SPACES ==
;; Spacing instruments, exhausted by a simple space as well as the
;; horizontal tab character, tally among the mandated separators between
;; tokens, but may be subjected to distribution and omission in concord
;; with one's own wonts.
;; 
;; == GRAMMAR ==
;; An expression of the InfiniTUM donat in the Extended Backus-Naur Form
;; (EBNF) shall be presented:
;; 
;;   program          := optionalNewlines
;;                    ,  [ statement
;;                    ,    { newline , optionalNewlines , statement } ]
;;                    ,  optionalNewlines
;;                    ;
;;   statement        := "IF"    , condition  , newline
;;                    ,  "STATE" , expression , newline
;;                    ,  "VALUE" , expression , newline
;;                    ,  "MOVE"  , direction
;;                    ;
;;   
;;   direction        := "<" | ">" | "X" ;
;;   
;;   condition        := conditionBase
;;                    |  conditionalExpr
;;                    ;
;;   conditionalExpr  := conditionBase
;;                    |  logicalNot
;;                    |  logicalAnd
;;                    |  logicalOr
;;                    ;
;;   logicalNot       := "!" , conditionalExpr ;
;;   logicalAnd       := conditionalExpr , "&&" , conditionalExpr ;
;;   logicalOr        := conditionalExpr , "||" , conditionalExpr ;
;;   conditionBase    := expression , relationOperator , expression ;
;;   relationOperator := "=" | "!" | "<" | "<=" | ">" | ">=" ;
;;   
;;   expression       := integer
;;                    |  variable
;;                    |  expression , binaryOperator , expression
;;                    ;
;;   binaryOperator   := "+" | "-" | "*" | "/" | "^" ;
;;   
;;   variable         := "s" | "v" ;
;;   integer          := [ "+" | "-" ] , digit , { digit } ;
;;   digit            := "0" | "1" | "2" | "3" | "4"
;;                    |  "5" | "6" | "7" | "8" | "9"
;;                    ;
;;   optionalNewlines := { newline } ;
;;   newline          := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; InfiniTUM programs offer a single instruction type only: a
;; conditional block, being a compound of four lines, employed in the
;; statement of the composition predicate, the tape head state modifier,
;; the current tape cell editor, and a tape head move or program flow
;; manipulation.
;; 
;; == A SINGLE INSTRUCTION EXISTS ==
;; Curtailed to its most abstract formulation, InfiniTUM admits merely a
;; single type of instruction, a conditional "if" block, however,
;; designed by four components' coefficiency:
;; 
;;   IF {condition}
;;     STATE {stateExpression}
;;     VALUE {valueExpression}
;;     MOVE  {direction}
;; 
;; The effect always amounts to the following:
;; 
;;   (1) The {condition} is probed; if confirmed, the succeeding three
;;       steps will be executed; otherwise the block is skipped.
;;   (2) The tape head state is set to the value of the
;;       {stateExpression}.
;;   (3) The value stored by the cell under the tape head is set to the
;;       {valueExpression} result.
;;   (4) If the {direction} equals "<", the tape head is moved one cell
;;       to the left; if matching ">", it translates one cell to the
;;       right. In the case of an "X" the program immediately halts.
;; 
;; A pseudocode expression tantamount to the just elucidated procedure
;; shall be adduced below:
;; 
;;   if the {condition} is satisfied then
;;     transition to state         {stateExpression}
;;     set cell under tape head to {valueExpression}
;;     
;;     if {direction} = ">"
;;       move the tape head one cell to the right
;;     else if {direction} = "<"
;;       move the tape head one cell to the left
;;     else if {direction} = "X"
;;       halt the program
;;     else
;;       error: "Invalid direction."
;;     end if
;;   end if
;; 
;; == A PROGRAM PROBES AND APPLIES INSTRUCTIONS IN AN INFINITE LOOP ==
;; An arbitrary tally of such conditional blocks may partake in a
;; program, administering significance to their specification's order.
;; 
;; An implicit iteration, infinite in its cycles, presides over the
;; program's perpetuation, upon each repetition probing in their
;; specified order the conditions. Upon confirmation, the block's body
;; is executed, applying the ensuing modifications. A mismatching
;; predicate denies the statement's participation in the current cycle.
;; 
;; As conformant to the aboon elucidations, a head move behest of "X"
;; constitutes the only point of incursion in order to escape the
;; implicit program loop, immediately terminating the execution.
;; 
;; == EXPRESSIONS MAY INCLUDE LITERALS AND PREDEFINED VARIABLES ==
;; A statement's constituents reliant upon a mathematical or logical
;; expression, namely
;; 
;;   - {condition}
;;   - {stateExpression}
;;   - {valueExpression}
;; 
;; may embrace, besides the operators, two types of terms: integral
;; literals and two predefined variables.
;; 
;; Signed integer literals assume the form of unbounded decimal numbers.
;; 
;; As a provision for the mandatory intelligence related to a Turing
;; machine's operational cycle, the current state and tape symbol ought
;; to be retrievable; to this end, two special variables exist, both of
;; the aforementioned integer species:
;; 
;;   ------------------------------------------------------------------
;;   Variable | Description
;;   ---------+--------------------------------------------------------
;;   s        | Contains the current tape head state.
;;   ..................................................................
;;   v        | Contains the value of the cell under the tape head,
;;            | that is, the current symbol.
;;   ------------------------------------------------------------------
;; 
;; == ARITHMETIC OPERATIONS ==
;; The following five binary operators are incorporated:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Syntax    | Effect
;;   ---------+-----------+--------------------------------------------
;;   +        | {a} + {b} | Addition: Returns the sum of {a} augmented
;;            |           | by {b}.
;;   ..................................................................
;;   -        | {a} - {b} | Subtraction: Returns the difference of {a}
;;            |           | reduced by {b}.
;;   ..................................................................
;;   *        | {a} * {b} | Multiplication: Returns the product of {a}
;;            |           | multiplied by {b}.
;;   ..................................................................
;;   /        | {a} / {b} | Integer division: Returns the quotient of
;;            |           | {a} divided by {b} and rounded up or down
;;            |           | to the nearest integer value.
;;   ..................................................................
;;   ^        | {a} ^ {b} | Exponentiation: Returns the result of {a}
;;            |           | raised to the power of {b}.
;;   ------------------------------------------------------------------
;; 
;; == RELATIONAL OPERATIONS ==
;; The following sextuple of relation operators exhausts the juxtaposing
;; facilities:
;; 
;;   ------------------------------------------------------------------
;;   Relation | Syntax     | Effect
;;   ---------+------------+-------------------------------------------
;;   =        | {a}  = {b} | Equal to: Satisfied if {a} equals {b}.
;;   ..................................................................
;;   !=       | {a} != {b} | Not equal to: Satisfied if {a} does not
;;            |            | equal {b}.
;;   ..................................................................
;;   <        | {a} <  {b} | Less than: Satisfied if {a} is strictly
;;            |            | smaller than {b}.
;;   ..................................................................
;;   <=       | {a} <= {b} | Less than or equal to: Satisfied if {a} is
;;            |            | smaller than {b} or equal to {b}.
;;   ..................................................................
;;   >        | {a} >  {b} | Greater than: Satisfied if {a} is strictly
;;            |            | greater than {b}.
;;   ..................................................................
;;   >=       | {a} >= {b} | Greater than or equal to: Satisfied if {a}
;;            |            | is greater than {b} or equal to {b}.
;;   ------------------------------------------------------------------
;; 
;; == LOGICAL OPERATIONS ==
;; Additionally, three logical operators are offered:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Syntax     | Effect
;;   ---------+------------+-------------------------------------------
;;   !        | ! {a}      | Logical NOT: Inverts the truth value of
;;            |            | the Boolean expression {a}.
;;   ..................................................................
;;   &&       | {a} && {b} | Logical AND: AND-combines the two Boolean
;;            |            | expressions {a} and {b}.
;;   ..................................................................
;;   ||       | {a} || {b} | Logical OR: OR-combines the two Boolean
;;            |            | expressions {a} and {b}.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A plain design dispels most of the susceptibility whose contingence
;; could manifest in InfiniTUM specification; natheless, a few specimens
;; with dubious quality may be raised, and shall be adduced in the
;; following sections.
;; 
;; == DO LINEBREAKS AND INDENTATIONS IMPOSE REQUISITES? ==
;; The syntactical presentation and the single example produced in the
;; protolog exhibit a particular format, with the four statement
;; constituents allocated on a line of their own, and the treble subset
;; succeeding the antecedent indented. It is not confided whether these
;; linebreaks and introductory space serve as requisite elements or not.
;; 
;; It has been adjudged to apportion the presence of at least one
;; linebreak in the interstices betwixt the statement segments, as well
;; as between successive statements, an imperative role, with an
;; integral perspective on the potentials of ambiguities to encroach the
;; parsing process.
;; 
;; The subject involving indentations has been reckoned a less severe
;; aspect and thus remains exposed to one's liberty.
;; 
;; 
;; Implementation
;; ==============
;; This program has been implemented in the Common Lisp programming
;; language, most conspicuously employing a Pratt parser [pratt1973top]
;; for the extraction of expressions with a special vista on operator
;; precedences.
;; 
;; The parser's entelech derives from the conventions and notions
;; perpetuated by Denis Lantsman [lantsman2018prattparsers], deviating,
;; however, in a functional variation, rather than the object-oriented
;; architecture introduced in the provenance.
;; 
;; Please note that the generous deployment of global variables
;; subscribes to an experimental effort and pursuit, and as thus should
;; not be equipensated in a mete approximating a cleaner solution.
;; 
;; The following precedence and associativity configurations, loosely
;; derived from [grand1997javalangref], have been adjudged:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Description              | Type   | Precedence | Assoc.
;;   ---------+--------------------------+--------+------------+-------
;;   +        | positive sign            | prefix |    100     | right
;;   ..................................................................
;;   -        | negative sign            | prefix |    100     | left
;;   ..................................................................
;;   ^        | power, exponentiation    | infix  |     90     | right
;;   ..................................................................
;;   *        | multiplication           | infix  |     80     | left
;;   ..................................................................
;;   /        | integer division         | infix  |     80     | left
;;   ..................................................................
;;   +        | addition                 | infix  |     70     | left
;;   ..................................................................
;;   -        | substraction             | infix  |     70     | left
;;   ..................................................................
;;   <        | less than                | infix  |     60     | left
;;   ..................................................................
;;   <=       | less than or equal to    | infix  |     60     | left
;;   ..................................................................
;;   >        | greater than             | infix  |     60     | left
;;   ..................................................................
;;   >=       | greater than or equal to | infix  |     60     | left
;;   ..................................................................
;;   =        | equal to                 | infix  |     50     | left
;;   ..................................................................
;;   !=       | not equal to             | infix  |     50     | left
;;   ..................................................................
;;   !        | logical NOT              | prefix |     30     | right
;;   ..................................................................
;;   ||       | logical OR               | infix  |     20     | left
;;   ..................................................................
;;   &&       | logical AND              | infix  |     10     | left
;;   ..................................................................
;;   (        | group                    | nilfix |      0     | none
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-04-10
;; 
;; Sources:
;;   [esolang2012infinitum]
;;   Esolang contributors, "InfiniTUM - Esolang", 2012
;;   URL: "https://esolangs.org/wiki/InfiniTUM"
;;   
;;   [lantsman2018prattparsers]
;;   Denis Lantsman, "How Desmos uses Pratt Parsers", 2018
;;   URL: "https://engineering.desmos.com/articles/pratt-parser/"
;;   
;;   [grand1997javalangref]
;;   Mark Grand, "Java Language Reference", 2nd Edition July 1997,
;;               "Chapter 4.14 Order of Operations"
;;   URL: "http://web.deu.edu.tr/doc/oreily/java/langref/ch04_14.htm"
;;   # Describes and lists the order of operations established in the
;;     Java programming language.
;;   
;;   [pratt1973top]
;;   Vaughan R. Pratt, "Top Down Operator Precedence", 1973
;;   URL: "https://daesan.com/wp-content/uploads/2018/05/
;;         top_down_operator_precedence.pdf"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype token ()
  "The ``token'' type defines a significant portion of an analyzed piece
   of InfiniTUM source as a cons, the sinistral compartment of which
   embraces the token type as a keyword, wheres the dextral moeity
   comprehends the token value."
  '(cons keyword T))

;;; -------------------------------------------------------

(deftype alist-of (&optional (key-type T) (value-type T))
  "The ``alist-of'' type defines an association list, or abbreviated
   alist, of zero or more entries, each key of which either amounts to
   the ``NIL'' value or a cons, the sinstral compartment of which holds
   a datum of the KEY-TYPE, representing the entry key, whereas the
   dextral section entails the entry value of the VALUE-TYPE, both
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
                  (or
                    (null element)
                    (and
                      (consp element)
                      (typep (car (the cons element)) key-type)
                      (typep (cdr (the cons element)) value-type))))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized variants of
   associativity for tokens in the role of initial or consequent
   tokens."
  '(member :left-associative :right-associative))

;;; -------------------------------------------------------

(deftype plist-of (&optional (value-type T))
  "The ``plist-of'' defines a property list, or abbreviated plist, of
   zero or more entries, each key of which, represented by a keyword
   symbol, is associated with a value of the VALUE-TYPE, defaulting to
   the comprehensive ``VALUE-TYPE''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for (even-element odd-element)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and (typep even-element 'keyword)
                     (typep odd-element  value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype node ()
  "The ``node'' type defines an abstract syntax tree (AST) node in terms
   of a property list of zero or more entries, the incipient key-value
   twain being construed as the subtree type, with the remaining members
   presenting the descriptive attributes."
  '(plist-of T))

;;; -------------------------------------------------------

(deftype initial-parselet ()
  "The ``initial-parselet'' type defines an initial parselet, or
   \"nud\", as a function embracing a token as its input and producing
   an abstract syntax tree (AST) node as response."
  '(function (token) node))

;;; -------------------------------------------------------

(deftype consequent-parselet ()
  "The ``consequent-parselet'' type defines a consequent parselet, or
   \"led\", as a function embracing the preceding abstract syntax tree
   (AST) node and a token as its inputs and producing another AST node
   as a response."
  '(function (node token) node))

;;; -------------------------------------------------------

(deftype move-direction ()
  "The ``move-direction'' type enumerates the recognized tape head move
   instructions."
  '(member :left :right :halt))

;;; -------------------------------------------------------

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

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``node''
   objects."
  '(list-of node))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype tape ()
  "The ``tape'' type defines an infinite tape of integer-valued cells,
   amenable to an index of the same type, and manifesting in the form of
   a hash table which maps integer keys to integer values."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token (type value)
  "Creates and returns a new token of the specified TYPE and VALUE."
  (declare (type keyword type))
  (declare (type T       value))
  (the token (cons type value)))

;;; -------------------------------------------------------

(defun token-type (token)
  "Returns the TOKEN type."
  (declare (type token token))
  (the keyword (car token)))

;;; -------------------------------------------------------

(defun token-value (token)
  "Returns the TOKEN value."
  (declare (type token token))
  (the T (cdr token)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of identifiers.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (alist-of string token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  `(("IF"    . ,(make-token :IF       "IF"))
    ("STATE" . ,(make-token :STATE    "STATE"))
    ("VALUE" . ,(make-token :VALUE    "VALUE"))
    ("MOVE"  . ,(make-token :MOVE     "MOVE"))
    ("s"     . ,(make-token :variable "s"))
    ("v"     . ,(make-token :variable "v"))
    ("X"     . ,(make-token :X        "X")))
  "Affiliates the recognized identifier names with representative
   tokens.")

;;; -------------------------------------------------------

(defun get-identifier-token (name)
  "Returns the token corresponding to the identifier NAME, or, if none
   such could be detected in the +IDENTIFIERS+ table, creates and
   returns a new identifier token comprehending the NAME."
  (declare (type string name))
  (the token
    (or (cdr (assoc name +IDENTIFIERS+ :test #'string=))
        (make-token :identifier name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or tab character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   `NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline, returning on
   confirmation a ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (char= candidate #\Newline)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent in an
   identifier name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alpha-char-p candidate)))))

;;; -------------------------------------------------------

(defun operator-character-p (candidate)
  "Determines whether the CANDIDATE represents an operator constituent,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "<>=!+-*/|&" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type string              *source*))
(declaim (type fixnum              *position*))
(declaim (type (or null character) *character*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The piece of InfiniTUM source code to analyze.")

(defparameter *position* 0
  "The current position into the *SOURCE*.")

(defparameter *character* NIL
  "The character at the current *POSITION* into the *SOURCE*.")

;;; -------------------------------------------------------

(defun initialize-lexer (new-source)
  "Sets the lexer to the NEW-SOURCE, resets its position and current
   character, and returns no value."
  (declare (type string new-source))
  (setf *source*    new-source)
  (setf *position*  0)
  (setf *character*
    (when (array-in-bounds-p *source* *position*)
      (char *source* *position*)))
  (values))

;;; -------------------------------------------------------

(defun lexer-advance ()
  "Returns the *CHARACTER* at the current *POSITION* into the *SOURCE*,
   and advances to the next character in the same, if possible."
  (the (or null character)
    (prog1 *character*
      (setf *character*
        (when (array-in-bounds-p *source* (1+ *position*))
          (char *source* (incf *position*)))))))

;;; -------------------------------------------------------

(defun lexer-move-to (new-position)
  "Relocates the *POSITION* cursor to the NEW-POSITION, updates the
   lexer's state, and returns no value."
  (declare (type fixnum new-position))
  (setf *position* new-position)
  (setf *character*
    (when (array-in-bounds-p *source* *position*)
      (char *source* *position*)))
  (values))

;;; -------------------------------------------------------

(defun lexer-string-follows-p (expected-string)
  "Probes whether, commencing at the current *POSITION*, the next
   *SOURCE* characters replicate the EXPECTED-STRING, on confirmation
   relocating the *POSITION* cursor immediately after the matching
   portion in the *SOURCE*, while returning a ``boolean'' value of
   ``T''; otherwise the *POSITION* cursor is not modified, and the
   ``NIL'' response is returned."
  (declare (type string expected-string))
  (let ((start-position *position*))
    (declare (type fixnum start-position))
    (the boolean
      (loop
        for expected-character of-type character across expected-string
        do
          (cond
            ((or (null *character*)
                 (char/= *character* expected-character))
              (lexer-move-to start-position)
              (return NIL))
            (T
              (lexer-advance)))
        finally
          (return T)))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces ()
  "Commencing at the current *POSITION* in the *SOURCE*, skips a
   sequence of zero or more adjacent spaces and returns no value."
  (loop while (and *character* (space-character-p *character*)) do
    (lexer-advance))
  (values))

;;; -------------------------------------------------------

(defun lexer-read-symbol (token-type)
  "Creates and returns a new token by associating the TOKEN-TYPE with
   the current *CHARACTER* as its value, while concomitantly advancing
   to the next position in the *SOURCE*."
  (declare (type keyword token-type))
  (the token
    (prog1
      (make-token token-type *character*)
      (lexer-advance))))

;;; -------------------------------------------------------

(defun lexer-read-number ()
  "Commencing at the current *POSITION* in the *SOURCE*, reads an
   unsigned integer number and returns a token representation thereof."
  (the token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (loop while (and *character* (digit-char-p *character*)) do
            (write-char (lexer-advance) digits)))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier ()
  "Commencing at the current *POSITION* in the *SOURCE*, reads an
   identifier and returns a token representation thereof."
  (the token
    (get-identifier-token
      (with-output-to-string (name)
        (declare (type string-stream name))
        (loop
          while (and *character* (identifier-character-p *character*))
          do    (write-char (lexer-advance) name))))))

;;; -------------------------------------------------------

(defun lexer-read-operator ()
  "Commencing at the current *POSITION* in the *SOURCE*, reads an
   operator and returns a token representation thereof."
  (the token
    (cond
      ((null *character*)
        (error "No operator character encountered at position ~d."
          *position*))
      
      ;; "="
      ((char= *character* #\=)
        (lexer-read-symbol :equal))
      
      ;; "!="
      ((lexer-string-follows-p "!=")
        (make-token :not-equal "!="))
      
      ;; "!"
      ((char= *character* #\!)
        (lexer-read-symbol :logical-not))
      
      ;; "<="
      ((lexer-string-follows-p "<=")
        (make-token :less-or-equal "<="))
      
      ;; "<"
      ((char= *character* #\<)
        (lexer-read-symbol :less-than))
      
      ;; ">="
      ((lexer-string-follows-p ">=")
        (make-token :greater-or-equal ">!"))
      
      ;; ">"
      ((char= *character* #\>)
        (lexer-read-symbol :greater-than))
      
      ;; "&&"
      ((lexer-string-follows-p "&&")
        (make-token :logical-and "&&"))
      
      ;; "||"
      ((lexer-string-follows-p "||")
        (make-token :logical-or "||"))
      
      ((char= *character* #\+)
        (lexer-read-symbol :plus))
      
      ((char= *character* #\-)
        (lexer-read-symbol :minus))
      
      ((char= *character* #\*)
        (lexer-read-symbol :times))
      
      ((char= *character* #\/)
        (lexer-read-symbol :divide))
      
      ((char= *character* #\^)
        (lexer-read-symbol :power))
      
      (T
        (error "Invalid operator \"~c\" at position ~d."
          *character* *position*)))))

;;; -------------------------------------------------------

(defun lexer-get-next-token ()
  "Returns the next token from the lexer.
   ---
   Upon the *SOURCE*'s exhaustion, each invocation returns a fresh
   end-of-file type."
  (the token
    (cond
      ((null *character*)
        (make-token :eof NIL))
      
      ((space-character-p *character*)
        (lexer-skip-spaces)
        (lexer-get-next-token))
      
      ((newline-character-p *character*)
        (lexer-read-symbol :newline))
      
      ((digit-char-p *character*)
        (lexer-read-number))
      
      ((identifier-character-p *character*)
        (lexer-read-identifier))
      
      ((operator-character-p *character*)
        (lexer-read-operator))
      
      ((char= *character* #\()
        (lexer-read-symbol :left-parenthesis))
      
      ((char= *character* #\))
        (lexer-read-symbol :right-parenthesis))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          *character* *position*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token stream.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Token *current-token*))

;;; -------------------------------------------------------

(defparameter *current-token* (make-token :eof NIL)
  "The token most recently obtained from the lexer for use in the token
   stream.")

;;; -------------------------------------------------------

(defun initialize-token-stream ()
  "Initializes or resets the token stream and returns no value."
  (setf *current-token* (lexer-get-next-token))
  (values))

;;; -------------------------------------------------------

(defun peek-token ()
  "Returns the next token from the lexer without returning it."
  (the token *current-token*))

;;; -------------------------------------------------------

(defun consume-token ()
  "Removes and returns the next token from the lexer."
  (the token
    (prog1 *current-token*
      (setf *current-token*
        (lexer-get-next-token)))))

;;; -------------------------------------------------------

(defun expect-token (expected-type)
  "Determines whether the lexer's current token conforms to the
   EXPECTED-TYPE, on confirmation consuming and returning this token,
   otherwise signaling an error of an unspecified type."
  (declare (type keyword expected-type))
  (the token
    (if (token-type-p *current-token* expected-type)
      (consume-token)
      (error "Expected a token of the type ~s, but encountered the ~
              token ~s."
        expected-type *current-token*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of AST node.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new node of the specified TYPE, optionally
   configured with the INITIAL-ATTRIBUTES, provided as a property list
   (plist) of zero or more entries in the form of attribute names on
   even-indexed positions, each such immediately followed by its
   associated value."
  (declare (type keyword      type))
  (declare (type (plist-of T) initial-attributes))
  (the node
    (append (list :type type) initial-attributes)))

;;; -------------------------------------------------------

(defun node-type (node)
  "Returns the NODE's categorizing type."
  (declare (type node node))
  (the keyword (getf node :type)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the value of the NODE attribute associated with the
   ATTRIBUTE-NAME, or ``NIL'' if none such can be detected."
  (declare (type node    node))
  (declare (type keyword attribute-name))
  (the T (getf node attribute-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselets.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (integer) node) parse-expression))

;;; -------------------------------------------------------

(declaim (type (alist-of keyword function)
               *initial-parselets*))
(declaim (type (alist-of keyword integer)
               *initial-binding-powers*))
(declaim (type (alist-of keyword associativity)
               *initial-associativities*))
(declaim (type (alist-of keyword function)
               *consequent-parselets*))
(declaim (type (alist-of keyword integer)
               *consequent-binding-powers*))
(declaim (type (alist-of keyword associativity)
               *consequent-associativities*))

;;; -------------------------------------------------------

(defparameter *initial-parselets* NIL
  "Associates token types representing initial tokens with functions
   implementing the ``initial-parselet'' type.")

(defparameter *initial-binding-powers* NIL
  "Associates token types representing initial tokensw with their
   integer-valued binding powers.")

(defparameter *initial-associativities* NIL
  "Associates token types representing initial tokens with their
   associativity settings.")

(defparameter *consequent-parselets* NIL
  "Associates token types representing consequent tokens with functions
   implementing the ``consequent-parselet'' type.")

(defparameter *consequent-binding-powers* NIL
  "Associates token types representing consequent tokens with their
   integer-valued binding powers.")

(defparameter *consequent-associativities* NIL
  "Associates token types representing consequent tokens with their
   associativity settings.")

;;; -------------------------------------------------------

(defun initial-token-p (token)
  "Determines whether the TOKEN constitutes an initial token, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token token))
  (the boolean
    (not (null
      (assoc (token-type token) *initial-parselets* :test #'eq)))))

;;; -------------------------------------------------------

(defun get-initial-binding-power (token)
  "Returns the TOKEN's initial binding power, or signals an error of an
   unspecified type upon its lacuna."
  (declare (type token token))
  (the integer
    (or
      (cdr (assoc (token-type token)
             *initial-binding-powers*
             :test #'eq))
      (error "No initial binding power defined for the token ~s."
        token))))

;;; -------------------------------------------------------

(defun get-initial-associativity (token)
  "Returns the initial TOKEN's associativity, or signals an error if the
   the same is not associated with such."
  (declare (type token token))
  (the associativity
    (or (cdr (assoc (token-type token)
               *initial-associativities*
               :test #'eq))
        (error "No initial associativity defined for the token ~s."
          token))))

;;; -------------------------------------------------------

(defun parse-initial-token (initial-token)
  "Parses the INITIAL-TOKEN by querying its initial parselet and
   applying the same to the token, and returns an abstract syntax tree
   (AST) node."
  (declare (type token initial-token))
  (the node
    (funcall
      (cdr (assoc (token-type initial-token)
                  *initial-parselets*
                  :test #'eq))
      initial-token)))

;;; -------------------------------------------------------

(defun consequent-token-p (token)
  "Determines whether the TOKEN constitutes a consequent token,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type token token))
  (the boolean
    (not (null
      (assoc (token-type token) *consequent-parselets* :test #'eq)))))

;;; -------------------------------------------------------

(defun get-consequent-binding-power (token)
  "Returns the TOKEN's consequent binding power, or signals an error of
   an unspecified type upon its lacuna."
  (declare (type token token))
  (the integer
    (or
      (cdr (assoc (token-type token)
             *consequent-binding-powers*
             :test #'eq))
      (error "No consequent binding power defined for the token ~s."
        token))))

;;; -------------------------------------------------------

(defun get-consequent-associativity (token)
  "Returns the consequent TOKEN's associativity, or signals an error if
   the the same is not associated with such."
  (declare (type token token))
  (the associativity
    (or (cdr (assoc (token-type token)
               *consequent-associativities*
               :test #'eq))
        (error "No consequent associativity defined for the token ~s."
          token))))

;;; -------------------------------------------------------

(defun parse-consequent-token (left-node consequent-token)
  "Parses the CONSEQUENT-TOKEN by querying its initial parselet and
   applying the same to the the LEFT-NODE and the CONSEQUENT-TOKEN, and
   returns an abstract syntax tree (AST) node."
  (declare (type node  left-node))
  (declare (type token consequent-token))
  (the node
    (funcall
      (cdr (assoc (token-type consequent-token)
             *consequent-parselets*
             :test #'eq))
      left-node
      consequent-token)))

;;; -------------------------------------------------------

(defun get-effective-binding-power (original-binding-power
                                    associativity)
  "Returns the effective binding power based upon the
   ORIGINAL-BINDING-POWER when taking the ASSOCIATIVITY into account."
  (declare (type integer       original-binding-power))
  (declare (type associativity associativity))
  (the integer
    (case associativity
      (:left-associative
        original-binding-power)
      (:right-associative
        (1- original-binding-power))
      (otherwise
        (error "Invalid associativity: ~s." associativity)))))

;;; -------------------------------------------------------

(defun register-initial-token
    (token-type parselet
     &optional (binding-power NIL binding-power-supplied-p)
               (associativity NIL associativity-supplied-p))
  "Registers the TOKEN-TYPE representing an initial token by associating
   the initial PARSELET with the same, optionally, if sensible for this
   indicator, embracing the BINDING-POWER and ASSOCIATIVITY, and returns
   no value."
  (declare (type keyword                 token-type))
  (declare (type initial-parselet        parselet))
  (declare (type (or null integer)       binding-power))
  (declare (ignorable                    binding-power))
  (declare (type T                       binding-power-supplied-p))
  (declare (type (or null associativity) associativity))
  (declare (ignorable                    associativity))
  (declare (type T                       associativity-supplied-p))
  
  (push (cons token-type parselet)
        *initial-parselets*)
  
  (when binding-power-supplied-p
    (push (cons token-type binding-power)
          *initial-binding-powers*))
  
  (when associativity-supplied-p
    (push (cons token-type associativity)
          *initial-associativities*))
  
  (values))

;;; -------------------------------------------------------

(defun register-unary-token (operator binding-power associativity)
  "Registers a prefix token representing the unary OPERATOR, vested with
   the BINDING-POWER and ASSOCIATIVITY, and returns no value."
  (declare (type keyword       operator))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-initial-token operator
    #'(lambda (token)
        (declare (type token token))
        (the node
          (make-node :unary
            :operator operator
            :operand  (parse-expression
                        (get-effective-binding-power
                          (get-initial-binding-power token)
                          (get-initial-associativity token))))))
    binding-power associativity)
  (values))

;;; -------------------------------------------------------

(defun register-consequent-token (token-type
                                  parselet
                                  binding-power
                                  associativity)
  "Registers the TOKEN-TYPE representing a consequent token by
   associating the consequent PARSELET, BINDING-POWER and ASSOCIATIVITY
   with the same, and returns no value."
  (declare (type keyword             token-type))
  (declare (type consequent-parselet parselet))
  (declare (type integer             binding-power))
  (declare (type associativity       associativity))
  (push (cons token-type parselet)      *consequent-parselets*)
  (push (cons token-type binding-power) *consequent-binding-powers*)
  (push (cons token-type associativity) *consequent-associativities*)
  (values))

;;; -------------------------------------------------------

(defun register-binary-token (operator binding-power associativity)
  "Registers a consequent token representing a binary OPERATOR, vested
   with the BINDING-POWER and ASSOCIATIVITY, and returns no value."
  (declare (type keyword       operator))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-consequent-token operator
    #'(lambda (left-node token)
        (declare (type node  left-node))
        (declare (type token token))
        (the node
          (make-node :binary
            :operator      operator
            :left-operand  left-node
            :right-operand
              (parse-expression
                (get-effective-binding-power
                  (get-consequent-binding-power token)
                  (get-consequent-associativity token))))))
    binding-power associativity)
  (values))

;;; -------------------------------------------------------

(defun register-relation-token (operator binding-power)
  "Registers the OPERATOR token type, as a particular instance of the
   consequent ilk, and vested with the BINDING-POWER, at the consequent
   token table and returns no value."
  (declare (type keyword operator))
  (declare (type integer binding-power))
  (register-consequent-token operator
    #'(lambda (left-node operator-token)
        (declare (type node  left-node))
        (declare (type token operator-token))
        (the node
          (make-node :relation
            :operator      operator
            :left-operand  left-node
            :right-operand
              (parse-expression
                (get-effective-binding-power
                  (get-consequent-binding-power operator-token)
                  (get-consequent-associativity operator-token))))))
    binding-power
    :left-associative)
  (values))

;;; -------------------------------------------------------

(defun parse-expression (current-binding-power)
  "Parses an expression using Pratt's principles, binding initial tokens
   with the CURRENT-BINDING-POWER, and returns a node representation of
   the received expression."
  (declare (type integer current-binding-power))
  
  (let ((initial-token (consume-token))
        (left-node     NIL))
    (declare (type token initial-token))
    (declare (type node  left-node))
    
    (if (initial-token-p initial-token)
      (setf left-node (parse-initial-token initial-token))
      (error "Not an initial token: ~s." initial-token))
    
    (loop for next-token of-type token = (peek-token) do
      (cond
        ;; No token follows?
        ((token-type-p next-token :eof)
          (loop-finish))
        
        ;; No consequent token follows?
        ((not (consequent-token-p next-token))
          (loop-finish))
        
        ;; Consequent token follows, but is too weak?
        ((<= (get-consequent-binding-power next-token)
             current-binding-power)
          (loop-finish))
        
        ;; Consequent token follows, and its binding power is superior?
        (T
          (consume-token)
          
          (setf left-node
            (parse-consequent-token left-node next-token)))))
    
    (the node left-node)))

;;; -------------------------------------------------------

(defun expect-newline ()
  "Determines whether the token stream's next token constitutes a
   newline, on confirmation consuming this token and the contingently
   abutting newline tokens, and returns no value; otherwise signals an
   error of an unspecified type."
  (expect-token :newline)
  (loop while (token-type-p (peek-token) :newline) do
    (consume-token))
  (values))

;;; -------------------------------------------------------

(defun skip-newlines ()
  "If the token stream's current token constitutes a newline, consumes
   it, as well as all contingetly abutting newline tokens, and returns
   no value."
  (loop while (token-type-p (peek-token) :newline) do
    (consume-token))
  (values))

;;; -------------------------------------------------------

(defun parse-direction ()
  "Parses a tape head move direction and returns a ``:move'' node
   representation thereof."
  (let ((direction-token (consume-token)))
    (declare (type token direction-token))
    (the node
      (make-node :move :direction
        (case (token-type direction-token)
          (:less-than    :left)
          (:greater-than :right)
          (:X            :halt)
          (otherwise
            (error "Invalid direction token: ~s."
              direction-token)))))))

;;; -------------------------------------------------------

(defun parse-statement ()
  "Parses a single statement and returns a ``:statement'' node
   representation thereof."
  (let ((condition        NIL)
        (state-expression NIL)
        (value-expression NIL)
        (move-direction   NIL))
    (declare (type (or null node) condition))
    (declare (type (or null node) state-expression))
    (declare (type (or null node) value-expression))
    (declare (type (or null node) move-direction))
    
    ;; Skip superfluous preceding newline tokens.
    (skip-newlines)
    
    (expect-token :IF)
    (setf condition (parse-expression 0))
    (expect-newline)
    
    (expect-token :STATE)
    (setf state-expression (parse-expression 0))
    (expect-newline)
    
    (expect-token :VALUE)
    (setf value-expression (parse-expression 0))
    (expect-newline)
    
    (expect-token :MOVE)
    (setf move-direction (parse-direction))
    
    (the node
      (make-node :statement
        :condition condition
        :state     state-expression
        :value     value-expression
        :move      move-direction))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Parses an InfiniTUM program, compact of zero or more statements, and
   returns a ``:program'' node representation thereof."
  (let ((statements NIL))
    (declare (type node-list statements))
    
    (skip-newlines)
    
    (loop until (token-type-p (peek-token) :eof) do
      (push (parse-statement) statements)
      
      (let ((next-token (peek-token)))
        (declare (type token next-token))
        (case (token-type next-token)
          (:eof
            (loop-finish))
          (:newline
            (expect-newline))
          (otherwise
            (error "Expected a newline or end of file, but encountered ~
                    the token ~s."
              next-token)))))
    
    (expect-token :eof)
    
    (the node
      (make-node :program
        :statements (nreverse statements)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Integer literal.
(register-initial-token :number
  #'(lambda (token)
      (declare (type token token))
      (the node
        (make-node :number :value (token-value token)))))

;;; -------------------------------------------------------

;; Variable.
(register-initial-token :variable
  #'(lambda (variable-token)
      (declare (type token variable-token))
      (the node
        (make-node :variable
          :name (token-value variable-token)))))

;;; -------------------------------------------------------

(register-unary-token :plus  100 :right-associative)
(register-unary-token :minus 100 :right-associative)

;;; -------------------------------------------------------

(register-initial-token :left-parenthesis
  #'(lambda (left-parenthesis-token)
      (declare (type token left-parenthesis-token))
      (declare (ignore     left-parenthesis-token))
      (the node
        (prog1
          (make-node :group :term (parse-expression 0))
          (expect-token :right-parenthesis))))
  0 :right-associative)

;;; -------------------------------------------------------

(register-binary-token :power  90 :right-associative)
(register-binary-token :times  80 :left-associative)
(register-binary-token :divide 80 :left-associative)
(register-binary-token :plus   70 :left-associative)
(register-binary-token :minus  70 :left-associative)

;;; -------------------------------------------------------

(register-relation-token :less-than        60)
(register-relation-token :less-or-equal    60)
(register-relation-token :greater-than     60)
(register-relation-token :greater-or-equal 60)
(register-relation-token :equal            50)
(register-relation-token :not-equal        50)

;;; -------------------------------------------------------

(register-unary-token  :logical-not 30 :right-associative)
(register-binary-token :logical-or  20 :left-associative)
(register-binary-token :logical-and 10 :left-associative)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logical-and (left-operand right-operand)
  "Returns the boolean value resulting from the logical AND-combination
   of the LEFT-OPERAND and the RIGHT-OPERAND."
  (declare (type boolean left-operand))
  (declare (type boolean right-operand))
  (the boolean (and left-operand right-operand)))

;;; -------------------------------------------------------

(defun logical-or (left-operand right-operand)
  "Returns the Boolean value resulting from the logical OR-combination
   of the LEFT-OPERAND and the RIGHT-OPERAND."
  (declare (type boolean left-operand))
  (declare (type boolean right-operand))
  (the boolean (or left-operand right-operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of operation callback table.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (alist-of keyword function) +OPERATOR-CALLBACKS+))

;;; -------------------------------------------------------

(defparameter +OPERATOR-CALLBACKS+
  `((:equal            . ,#'=)
    (:not-equal        . ,#'/=)
    (:less-than        . ,#'<)
    (:less-or-equal    . ,#'<=)
    (:greater-than     . ,#'>)
    (:greater-or-equal . ,#'>=)
    
    (:logical-or       . ,#'logical-or)
    (:logical-and      . ,#'logical-and)
    
    (:plus             . ,#'+)
    (:minus            . ,#'-)
    (:times            . ,#'*)
    (:divide           . ,#'round)
    (:power            . ,#'expt))
  "Associates the recognized operator and relation identifiers with
   callback functions whose competency permits their application with
   actual parameters.")

;;; -------------------------------------------------------

(defun get-operator-handler (operator)
  "Returns the function responsible for handling the OPERATOR, or
   signals an error of an unspecified type upon its absence."
  (declare (type keyword operator))
  (the function
    (or (cdr (assoc operator +OPERATOR-CALLBACKS+ :test #'eq))
        (error "Unrecognized operator: ~s." operator))))

;;; -------------------------------------------------------

(defun apply-operator (operator left-operand right-operand)
  "Applies the function associated with the OPERATOR to the LEFT-OPERAND
   and RIGHT-OPERAND and returns a value appropriate for this
   combination."
  (declare (type keyword operator))
  (declare (type T       left-operand))
  (declare (type T       right-operand))
  (the T
    (funcall
      (get-operator-handler operator)
      left-operand
      right-operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Program-Halt-Condition".        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Program-Halt-Condition (condition)
  ()
  (:documentation
    "The ``Program-Halt-Condition'' signals the behest to terminate an
     executing InfiniTUM program immediately."))

;;; -------------------------------------------------------

(defun halt-program ()
  "Signals a condition of the type ``Program-Halt-Condition''."
  (signal 'Program-Halt-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type tape    *tape*))
(declaim (type integer *tape-head*))
(declaim (type integer *tape-value*))
(declaim (type integer *state*))

;;; -------------------------------------------------------

(defparameter *tape* (make-hash-table :test #'eql)
  "An infinite tape composed of cells sufficiently potent to store an
   arbitrary integer datum, and implemented by a hash table's
   adminiculum so as to supply a sparse structure.")

(defparameter *tape-head* 0
  "The current tape head position as the index of the active *TAPE*
   cell.")

(define-symbol-macro *tape-value*
  (gethash *tape-head* *tape* 0))

(defparameter *state* 1
  "The current state.")

;;; -------------------------------------------------------

(defun initialize-interpreter ()
  "Initializes or resets the interpreter's state as a parasceve for a
   pending abstract syntax tree (AST) processing, and returns no value."
  (clrhash *tape*)
  (setf *tape-head* 0)
  (setf *state*     1)
  (values))

;;; -------------------------------------------------------

(defun check-statement-condition (condition)
  "Determines whether the CONDITION represents a Boolean object, on
   confirmation returning the CONDITION, otherwise signaling an error of
   an unspecified type."
  (declare (type T condition))
  (unless (typep condition 'boolean)
    (error "The condition does not represent a Boolean ~
            expression: ~s."
      condition))
  (the boolean condition))

;;; -------------------------------------------------------

(defun check-statement-state (state)
  "Determines whether the STATE represents an integer object, on
   confirmation returning the STATE, otherwise signaling an error of an
   unspecified type."
  (declare (type T state))
  (unless (integerp state)
    (error "The tape head state does not represent an integer: ~s."
      state))
  (the integer state))

;;; -------------------------------------------------------

(defun check-statement-value (value)
  "Determines whether the VALUE represents an integer object, on
   confirmation returning the VALUE, otherwise signaling an error of an
   unspecified type."
  (declare (type T value))
  (unless (integerp value)
    (error "The tape head value does not represent an integer: ~s."
      value))
  (the integer value))

;;; -------------------------------------------------------

(defun get-indexed-tape-elements ()
  "Returns the *TAPE* cells as an association list, or alist, compact of
   cell index-value conses, however, unsorted."
  (the (alist-of integer integer)
    (loop
      for     cell-index of-type integer being the hash-keys in *tape*
      using   (hash-value cell-value)
      collect (cons cell-index cell-value))))

;;; -------------------------------------------------------

(defun get-sorted-tape-elements ()
  "Returns the *TAPE* cells as an association list, or alist, compact of
   cell index-value conses, and sorted in ascending order of the
   indices."
  (the (alist-of integer integer)
    (sort (get-indexed-tape-elements) #'< :key #'car)))

;;; -------------------------------------------------------

(defun print-program-state ()
  "Prints to the standard output the program state, consisting of the
   Turing machine's state and its tape's content, with the cell under
   the tape head emphasized with a trailing asterisk (\"*\"), and
   returns no value."
  (format T "~&STATE")
  (format T "~&~4t~d" *state*)
  (format T "~&TAPE:")
  (flet ((cell-under-head-p (candidate-cell-index)
          "Determines whether the CANDIDATE-CELL-INDEX designates the
           position of the tape head, returning on confirmation a
           ``boolean'' value of ``T'', otherwise ``NIL''."
          (declare (type integer candidate-cell-index))
          (the boolean
            (not (null (= candidate-cell-index *tape-head*))))))
    (loop
      for (cell-index . cell-value)
        of-type (integer integer)
        in      (get-sorted-tape-elements)
      do
        (format T "~&~4t#~d = ~d~@[ *~]"
          cell-index cell-value
          (cell-under-head-p cell-index))))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-node (node-type node)
  (:documentation
    "Processes the NODE, dispatching on its NODE-TYPE, and returns a
     result appropriate for the same."))

;;; -------------------------------------------------------

(defun visit-node (node)
  "Visits the NODE using the interpreter and returns a value appropriate
   for the NODE's type."
  (declare (type node node))
  (the T (dispatch-node (node-type node) node)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :program))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (handler-case
    (loop do
      (dolist (statement (node-attribute node :statements))
        (declare (type node statement))
        (visit-node statement)))
    (Program-Halt-Condition ()
      NIL))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :statement))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (let ((condition-node (node-attribute node :condition))
        (state-node     (node-attribute node :state))
        (value-node     (node-attribute node :value))
        (move-node      (node-attribute node :move)))
    (declare (type node condition-node))
    (declare (type node state-node))
    (declare (type node value-node))
    (declare (type node move-node))
    (when (check-statement-condition (visit-node condition-node))
      (setf *state*
        (check-statement-state
          (visit-node state-node)))
      (setf *tape-value*
        (check-statement-value
          (visit-node value-node)))
      (visit-node move-node)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :move))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (let ((direction (node-attribute node :direction)))
    (declare (type move-direction direction))
    (case direction
      (:left  (decf *tape-head*))
      (:right (incf *tape-head*))
      (:halt  (halt-program))
      (otherwise
        (error "Unrecognized move direction: ~s." direction))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :number))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (the integer (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :variable))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (let ((variable-name (node-attribute node :name)))
    (declare (type string variable-name))
    (the integer
      (cond
        ((string= variable-name "s") *state*)
        ((string= variable-name "v") *tape-value*)
        (T (error "Unrecognized variable: ~s." variable-name))))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :unary))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (let ((operator (node-attribute node :operator))
        (operand  (node-attribute node :operand)))
    (declare (type keyword operator))
    (declare (type node    operand))
    (declare (ignorable    operand))
    (the (or number boolean)
      (case operator
        (:plus        (visit-node operand))
        (:minus       (- (visit-node operand)))
        (:logical-not (not (visit-node operand)))
        (otherwise
          (error "Invalid unary operator: ~s." operator))))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :binary))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (let ((operator      (node-attribute node :operator))
        (left-operand  (node-attribute node :left-operand))
        (right-operand (node-attribute node :right-operand)))
    (declare (type keyword operator))
    (declare (type node    left-operand))
    (declare (type node    right-operand))
    (the (or number boolean)
      (apply-operator operator
        (visit-node left-operand)
        (visit-node right-operand)))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :relation))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (let ((operator      (node-attribute node :operator))
        (left-operand  (node-attribute node :left-operand))
        (right-operand (node-attribute node :right-operand)))
    (declare (type keyword operator))
    (declare (type node    left-operand))
    (declare (type node    right-operand))
    (the boolean
      (not (null
        (apply-operator operator
          (visit-node left-operand)
          (visit-node right-operand)))))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :group))
                          (node      list))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (the T
    (visit-node
      (node-attribute node :term))))

;;; -------------------------------------------------------

(defun interpret-InfiniTUM (code)
  "Interprets the piece of InfiniTUM source CODE and returns no value.
   ---
   Ensuing from the interpreter's conclusion, the program state, its
   perimeter being delineated by the underlying Turing machine's last
   state and the significant tape cells, is printed to the standard
   output."
  (declare (type string code))
  (initialize-lexer code)
  (initialize-token-stream)
  (initialize-interpreter)
  (visit-node (parse-program))
  (print-program-state)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An infinite counter which counts up.
(interpret-InfiniTUM
  "IF s != 0
     STATE s+1
     VALUE s
     MOVE >")

;;; -------------------------------------------------------

;; Counter that fills the tape cells at the indices 0 to 8 with the
;; values from 2 to 10.
(interpret-InfiniTUM
  "IF s != 0
     STATE s + 1
     VALUE s
     MOVE  >
   
   IF s >= 10
     STATE s
     VALUE v
     MOVE  X")

;;; -------------------------------------------------------

;; A counter sicklike to the aboon, but setting elements at odd tape
;; indices to their negative value.
(interpret-InfiniTUM
  "IF s != 0
     STATE s + 1
     VALUE s
     MOVE  >
   
   IF (s = 2) || (s = 4) || (s = 6) || (s = 8) || (s = 10)
     STATE s + 1
     VALUE -s
     MOVE  >
   
   IF s >= 10
     STATE s
     VALUE v
     MOVE  X")
