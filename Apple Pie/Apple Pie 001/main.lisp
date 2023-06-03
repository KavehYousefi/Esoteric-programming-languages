;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Apple Pie", designed by the Esolang user "Cortex" and
;; presented in the year 2018, pursuing an intricate form's application
;; as a means of introducing complexity, in particular with its
;; ensconcement of commands in a jumelle of inchoating identifier and
;; terminating character, the latter determined by a rather complex
;; rule.
;; 
;; 
;; Concept
;; =======
;; The Apple Pie esoteric programming language prides itself with the
;; encumbrance of the programming efforts by a complex syntactical
;; design, incorporating kensback diorisms in each of its commands.
;; 
;; == ADDRESSES OF FORTUNE AND ECPHONEMES BRACE THE PROGRAM ==
;; An Apple Pie program's inchoation is realized in the phrase, aiblins
;; termed with imperfect insincerity,
;; 
;;   Good luck reading this lol u
;; 
;; With all code immediately proceeding from its statement, without the
;; inroads of superfluous spaces.
;; 
;; The code's termination must be incurred via an ecphonemes' treble:
;; 
;;   !!!
;; 
;; In its ultimity, the following pattern amplects any program:
;; 
;;   Good luck reading this lol u...!!!
;; 
;; == APPLE PIE COMMANDS: EVERY ONE A LANGUAGE OF ITS OWN ==
;; The idiopathic construction rules appertaining to every command
;; constitute a defining linguistic proprium, variating in the
;; homologation of parameters, or "inputs", as nevened by the protolog,
;; and their spatial allocation in the instruction.
;; 
;; == STATEMENTS ENDS IN THE LETTER 9 PLACES FURTHER IN THE ALPHABET ==
;; A unifying attribute of all of these quiddities resides in a
;; command's concluding emblem, which prescribes a space succeeded by
;; the letter nine (9) positions after the command identifier's own
;; desinent letter in the alphabet. For example, the iteration facility
;; 
;;   EepbeepQ
;;          *
;; 
;; bears the letter "Q" at its desinence. The successor in the alphabet
;; nine positions further constitutes "Z":
;; 
;;   -----------------------------------------------------------
;;   Letter in alphabet | Q | R | S | T | U | V | W | X | Y | Z
;;   -------------------+---+---+---+---+---+---+---+---+---+---
;;   Offset from "Q"    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;;   -----------------------------------------------------------
;; 
;; The pattern for the instruction's incipience and termination thus
;; produces
;; 
;;   EepbeepQ... Z
;;          *    |_ The letter "Z" follows nine positions after "Q".
;; 
;; If transcending the alphabet's terminal march, the tallying principle
;; wraps around, starting from "A" while supputating the offset. For
;; instance, in the hypothecial case of a command with a last letter of
;; "S", we obtain the closure as "B", via:
;; 
;;   -----------------------------------------------------------
;;   Letter in alphabet | S | T | U | V | W | X | Y | Z | A | B
;;   -------------------+---+---+---+---+---+---+---+---+---+---
;;   Offset from "S"    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;;   -----------------------------------------------------------
;; 
;; However, no command yet partakes of such a late desinent character.
;; 
;; Categorical desistence from this routine registers only in the two 
;; program extremity markers, "Good luck reading this lol u", the
;; proclamation of its incipiency, and "!!!" which indicates its
;; desinence's march.
;; 
;; From the scheme's imposition the following contraposition's geniture
;; results, in its consortion the apostille that the first twenty-six
;; entries relate to majuscles, whereas the desinent moeity is dedicated
;; to the minuscular letters.
;; 
;;   ------------------------------------------------------------------
;;   Starting character | Terminating character
;;   -------------------+----------------------------------------------
;;   A                  | J
;;   ..................................................................
;;   B                  | K
;;   ..................................................................
;;   C                  | L
;;   ..................................................................
;;   D                  | M
;;   ..................................................................
;;   E                  | N
;;   ..................................................................
;;   F                  | O
;;   ..................................................................
;;   G                  | P
;;   ..................................................................
;;   H                  | Q
;;   ..................................................................
;;   I                  | R
;;   ..................................................................
;;   J                  | S
;;   ..................................................................
;;   K                  | T
;;   ..................................................................
;;   L                  | U
;;   ..................................................................
;;   M                  | V
;;   ..................................................................
;;   N                  | W
;;   ..................................................................
;;   O                  | X
;;   ..................................................................
;;   P                  | Y
;;   ..................................................................
;;   Q                  | Z
;;   ..................................................................
;;   R                  | A
;;   ..................................................................
;;   S                  | B
;;   ..................................................................
;;   T                  | C
;;   ..................................................................
;;   U                  | D
;;   ..................................................................
;;   V                  | E
;;   ..................................................................
;;   W                  | F
;;   ..................................................................
;;   X                  | G
;;   ..................................................................
;;   Y                  | H
;;   ..................................................................
;;   Z                  | I
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;   a                  | j
;;   ..................................................................
;;   b                  | k
;;   ..................................................................
;;   c                  | l
;;   ..................................................................
;;   d                  | m
;;   ..................................................................
;;   e                  | n
;;   ..................................................................
;;   f                  | o
;;   ..................................................................
;;   g                  | p
;;   ..................................................................
;;   h                  | q
;;   ..................................................................
;;   i                  | r
;;   ..................................................................
;;   j                  | s
;;   ..................................................................
;;   k                  | t
;;   ..................................................................
;;   l                  | u
;;   ..................................................................
;;   m                  | v
;;   ..................................................................
;;   n                  | w
;;   ..................................................................
;;   o                  | x
;;   ..................................................................
;;   p                  | y
;;   ..................................................................
;;   q                  | z
;;   ..................................................................
;;   r                  | a
;;   ..................................................................
;;   s                  | b
;;   ..................................................................
;;   t                  | c
;;   ..................................................................
;;   u                  | d
;;   ..................................................................
;;   v                  | e
;;   ..................................................................
;;   w                  | f
;;   ..................................................................
;;   x                  | g
;;   ..................................................................
;;   y                  | h
;;   ..................................................................
;;   z                  | i
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS AS INPUTS ESCHEW THE CLOSURE ==
;; A prepotent cardinality among these commands abides this peculiar
;; closure in all aspects --- exempt from this remain only expressions
;; when participating as parameter values.
;; 
;; == VARIABLES STORE NUMBERS OR STRINGS ==
;; The singular data storage in a program is realized in its variables,
;; identified by single Latin letters, and admissive to numbers or
;; strings.
;; 
;; 
;; Architecture
;; ============
;; Its reliance upon variables as the aefauld currency of information
;; serves as a disencumbrance of Apple Pie from intricate memory
;; designs. Merely an implement for the variables' castaldy induces some
;; structural requirement.
;; 
;; The variable mapping introduces the requirement of such a
;; placeholder's perquisition and modification by its unambiguous
;; identifier's mediation. Commonly a requisitum of this ilk resorts to
;; an associative data structure, such as a dictionary. The absence of a
;; natural order among the indicators conduces a deploying of a
;; hash-based solution.
;; 
;; 
;; Data Types
;; ==========
;; Apple Pie's type system embraces a rather generous range in its
;; admission of signed integers and strings, the former of which must be
;; provided, in dependency upon the context, in decimal or ternary form.
;; 
;; == INTEGERS ==
;; Integers, naturally uninhibited in their magnitude, partake of a
;; quadruple pattern in the language, the incipient denoting the
;; differentiation betwixt signed and unsigned variants, as mandated by
;; the concrete operation; a second criterion, iterum along this line of
;; determination, the underlying number system may be decimal or
;; ternary.
;; 
;; The material of its sparse arithmetics bailiwick, the integers'
;; appropriation of a slightly prepotent echolon designates their choice
;; role in Apple Pie.
;; 
;; == STRINGS ==
;; A second participant in the type system, the strings' occupation of
;; an approximate equipollent significance assigns to these a paravaunt
;; currency in the output ambitus, permitting as constituents decimal
;; digits and Latin letters.
;; 
;; 
;; Syntax
;; ======
;; Apple Pie's alliciency relates to it being a commorant of a
;; syntactical realm in fantastic compositions, assigning to every
;; instruction a peculiar design and operative regulation.
;; 
;; == INSTRUCTIONS ==
;; Every command ostends a sui generis syntax. If its operations demand
;; it to be entalented with arguments, these are embedded into the
;; character sequence with immediacy, prohibiting any separating
;; entities along the intermede.
;; 
;; A command not in the role of an expression is terminated with the
;; letter nine positions further in the alphabet than its identifier's
;; desinent letter, wrapping around the repertoire upon necessity.
;; 
;; == PARAMETERS ==
;; The sportive, yet repressive haecceity of Apple Pie propagates into
;; its operation parameters, also agnominated "inputs", with the actual
;; formulation and species of specification a variable of its demanding
;; command.
;; 
;; == INTEGERS ==
;; Integers are either specified in the decimal or ternary number
;; system, depending upon the ensconcing operation, in the same mete as
;; their admission of a sign, "+" or "-", proclaims its reliance.
;; 
;; == STRINGS ==
;; The string definition's magnanimity encompasses merely the cyriologic
;; and numeric aspects, that is, Latin minuscles and majuscles in
;; consortion with decimal digits.
;; 
;; == VARIABLES ==
;; Variables are hechted using a single Latin majuscle or minuscle.
;; 
;; == SPACES ==
;; The insertion of spaces, which comprehends the space as well as the
;; horizontal tab, does not belong to the possessions of a programmer's
;; prerogatives. Merely betwixt a statement and its terminating
;; two-character sequence, the introducing member of which manifests in
;; the space itself, followed by the letter nine positions further in
;; the alphabet than the command's desinent one, does such an entity
;; experience tolerance. Any other case is inflicted with interdiction.
;; 
;; == NEWLINES ==
;; The newline character is attributed with a wike of its own, the
;; request of the Apple Pie program's source code, thus rendering the
;; whole program a quine. A corollary yielded by this assignment, a
;; newline, defrayed by its operational consequences, may be inserted
;; at any location receptive to an instruction.
;; 
;; == GRAMMAR ==
;; A formulation of the language's syntaxis according to the Extended
;; Backus-Naur Form (EBNF) shall serve in its elucidation:
;; 
;;   program            := statementList ;
;;   
;;   statementList      := { statement } ;
;;   
;;   statement          := print
;;                      |  comment
;;                      |  variableDefinition
;;                      |  loop
;;                      |  ( binaryOperation , space , "O" )
;;                      |  HQ9+interpreter
;;                      |  variableClearance
;;                      |  ( variableAccess , space , "O" )
;;                      |  quine
;;                      ;
;;   
;;   print              := "A" , printArgument , space , "J" ;
;;   printArgument      := word | variableAccess ;
;;   
;;   comment            := "B" , { nonSpaceCharacter } , space , "K" ;
;;   
;;   variableDefinition := "D" , variable , "D" , varAssignedValue
;;                      ,  space , "M"
;;                      ;
;;   varAssignedValue   := decimalNumber
;;                      |  variableAccess
;;                      |  binaryOperation
;;                      |  word
;;                      ;
;;   
;;   loop               := "EepbeepQ"
;;                      ,  loopRepetitions
;;                      ,  space , "Z"
;;                      ,  loopBody
;;                      ,  loopTerminator
;;                      ;
;;   loopRepetitions    := ternaryNumber
;;                      |  variableAccess
;;                      |  binaryOperation
;;                      ;
;;   loopBody           := statementList ;
;;   loopTerminator     := "C" , space , "L" ;
;;   
;;   binaryOperation    := "F" , binaryOperand
;;                      ,  "F" , binaryOperator
;;                      ,  "F" , binaryOperand
;;                      ;
;;   binaryOperand      := decimalNumber
;;                      |  variableAccess
;;                      |  binaryOperation
;;                      ;
;;   binaryOperator     := "+" | "-" | "*" | "/" | "^" ;
;;   
;;   HQ9+interpreter    := "G" , HQ9+program , space , "P" ;
;;   HQ9+program        := { HQ9+command } ;
;;   HQ9+command        := "H" | "h" | "Q" | "q" | "9" | "+" ;
;;   
;;   variableClearance  := "H44" , variable , "Q" ;
;;   
;;   variableAccess     := "$" , variable , "F" ;
;;   
;;   quine              := "\n" ;
;;   
;;   word               := nonSpaceCharacter , { nonSpaceCharacter } ;
;;   nonSpaceCharacter  := character - space ;
;;   space              := " " ;
;;   variable           := letter ;
;;   letter             := "a" | ... | "z" | "A" | ... | "Z" ;
;;   ternaryNumber      := optionalSign
;;                      ,  ternaryDigit , { ternaryDigit }
;;                      ;
;;   ternaryDigit       := "0" | "1" | "2" ;
;;   decimalNumber      := optionalSign
;;                      ,  decimalDigit , { decimalDigit }
;;                      ;
;;   decimalDigit       := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   optionalSign       := [ "+" | "-" ] ;
;; 
;; 
;; Instructions
;; ============
;; A duodecimal tally exhausts Apple Pie's instruction set, with its
;; members subsumable into output, arithmetics, variable castaldy, and
;; an kenspeckle HQ9+ programming language interpreter.
;; 
;; == OVERVIEW ==
;; An apercu shall administer the basic comprehension regarding the
;; language's operational circumference. Please note that the
;; "<newline>" instruction constitutes a placeholder for the verbatim
;; newline or linebreak character, not an actual token.
;; 
;;   ------------------------------------------------------------------
;;   Command                      | Description
;;   -----------------------------+------------------------------------
;;   Good luck reading this lol u | Starts the program.
;;                                | This command's presence constitues
;;                                | an obligation at the code's start
;;                                | and a proscription at any other
;;                                | location.
;;   ..................................................................
;;   A{argument}                  | Outputs the {argument}.
;;                                |------------------------------------
;;                                | If the {argument} is string, only
;;                                | the character preceding its first
;;                                | character in the alphabet is
;;                                | printed.
;;                                |------------------------------------
;;                                | If the {argument} is an unsigned,
;;                                | integer, the digit preceding its
;;                                | first digit is printed.
;;                                |------------------------------------
;;                                | If the {argument} is a variable
;;                                | accessor ("${variable}F"), its
;;                                | content is printed in reverse
;;                                | order.
;;   ..................................................................
;;   B{text}                      | Represents a comment and thus is
;;                                | ignored.
;;   ..................................................................
;;   C                            | Demarcates the matching loop's end,
;;                                | thus always being matched with an
;;                                | "EepbeepQ" command, which please
;;                                | see.
;;   ..................................................................
;;   D{variable}D{value}          | Defines or redefines a variable
;;                                | with the name {variable} and the
;;                                | specified {value}.
;;                                |------------------------------------
;;                                | The {variable} must be a one-letter
;;                                | variable name.
;;                                |------------------------------------
;;                                | The {value} is an arbitrary datum
;;                                | to assign to the variable.
;;                                |------------------------------------
;;                                | If the variable does not exist, it
;;                                | is created and initialized;
;;                                | otherwise it is reassigned to the
;;                                | {value}.
;;   ..................................................................
;;   EepbeepQ{repetitions}        | Designates the start of a loop
;;                                | which repeats its body a
;;                                | {repetitions} number of times.
;;                                |------------------------------------
;;                                | The {repetitions} must be an
;;                                | integer stated in the base 3.
;;                                |------------------------------------
;;                                | Every "EepbeepQ" loop must be
;;                                | matched with a closing "C" command.
;;                                |------------------------------------
;;                                | Betwixt this loop start and the
;;                                | closing "C" command, a sequence of
;;                                | zero or more commands may appear to
;;                                | form the loop body.
;;   ..................................................................
;;   F{left}F{operator}F{right}   | Applies a binary arithmetic and
;;                                | returns the result.
;;                                |------------------------------------
;;                                | The {left} operand must be a number
;;                                | in the base 10 or a variable
;;                                | accessor ("${variable}F").
;;                                |------------------------------------
;;                                | The {operator} must be one of the
;;                                | following symbols:
;;                                |  ----------------------------------
;;                                |  Operator | Effect
;;                                |  ---------+------------------------
;;                                |  +        | Addition
;;                                |  -        | Subtraction.
;;                                |  *        | Multiplication.
;;                                |  /        | Division
;;                                |  ^        | Exponentiation.
;;                                |------------------------------------
;;                                | The {right} operand must be a
;;                                | number in the base 10 or a variable
;;                                | accessor ("${variable}F").
;;   ..................................................................
;;   ${variable}F                 | If the {variable} designates the
;;                                | name of a variable, its value is
;;                                | returned. Otherwise, responds with
;;                                | the letter "L".
;;   ..................................................................
;;   G{HQ9+Commands}              | Interprets a sequence of zero or
;;                                | instructions {HQ9+Commands} of the
;;                                | programming language HQ9+
;;   ..................................................................
;;   H44{variable}                | If the {variable} designates the
;;                                | name of a variable, clears its
;;                                | value.
;;   ..................................................................
;;   !!!                          | Terminates the program.
;;   ..................................................................
;;   <newline>                    | Prints the program's source code,
;;                                | that is, renders the program a
;;                                | quine, after having deleted any
;;                                | other output previously issued.
;;   ------------------------------------------------------------------
;; 
;; == COMMAND "G" --- THE HQ9+ INTERPRETER ==
;; The "G" command expects a sequence of zero or more source code tokens
;; conformant with the esoteric programming language HQ9+, with a
;; special attribution to the space character as the instruction's
;; immediate termination.
;; 
;; HQ9+ constitutes a creation of Cliff L. Biffle, the geniture of which
;; reaches into the year 2001, and subsumes into the category of joke
;; languages, entalented with a competence whose profitability cannot be
;; assayed beyond a rather arbitrary choice for the telos of joviality.
;; 
;; A quadruple instruction set, exhausted already by the agnomination's
;; constituents, applies itself either to the printing of text, or a
;; incrementation of an accumulator perfectly assigned no further
;; entelechy. The command tokens, "H", "Q", "9", and "+", are assumed to
;; be case-insensitive.
;; 
;; An apercu directed at the HQ9+ language shall administer a cursory
;; nortelry:
;; 
;;   ------------------------------------------------------------------
;;   HQ9+ command | Description
;;   -------------+----------------------------------------------------
;;   H            | Prints the message "hello, world" to the standard
;;                | output.
;;   ..................................................................
;;   Q            | Prints the HQ9+ program's source code to the
;;                | standard output, that is, produces a quine.
;;   ..................................................................
;;   9            | Prints the lyrics of the song "99 Bottles of Beer"
;;                | to the standard output.
;;   ..................................................................
;;   +            | Increments the accumulator by one.
;;   ------------------------------------------------------------------
;; 
;; Please note that the HQ9+ interpreter will be freshly created for
;; each invocation of Apple Pie's "G" command, and destroyed in
;; immediate conclusion of the same operation's termination. A
;; consectary of this deportment, HQ9+ interpreter states from previous
;; calls do not influence future instances.
;; 
;; == STATEMENT CLOSURES ==
;; As encompassed in the "Concept" section's elucidations, each
;; statement must be closed with the letter nine positions after its
;; desinent character in the alphabet.
;; 
;; The below table's wike embraces the apposition of the command
;; identifiers and their expected concluding letters. Please note that
;; each responsive entry in its sinistral column is co-located with an
;; asterisk alow its pertinent letter for the same the termination
;; principle, extricable in the third and last file, holds.
;; 
;;   ------------------------------------------------------------------
;;   Command                      | Last letter | Terminating letter
;;   -----------------------------+-------------+----------------------
;;   Good luck reading this lol u | u           | (None.)
;;   ..................................................................
;;   A{argument}                  | A           | J
;;   *                            |             |
;;   ..................................................................
;;   B{text}                      | B           | K
;;   *                            |             |
;;   ..................................................................
;;   C                            | C           | L
;;   *                            |             |
;;   ..................................................................
;;   D{variable}D{value}          | D           | M
;;              *                 |             |
;;   ..................................................................
;;   EepbeepQ{repetitions}        | Q           | Z
;;          *                     |             |
;;   ..................................................................
;;   F{left}F{operator}F{right}   | F           | O
;;                     *          |             |
;;   ..................................................................
;;   ${variable}F                 | F           | O
;;              *                 |             |
;;   ..................................................................
;;   G{HQ9+Commands}              | G           | P
;;   *                            |             |
;;   ..................................................................
;;   H44{variable}                | H           | Q
;;   *                            |             |
;;   ..................................................................
;;   !!!                          | (None.)     | (None.)
;;   ..................................................................
;;   <newline>                    | (None.)     | (None.)
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The detrimental coefficacy of Apple Pie's syntactical peculiarities
;; and the original specification's brevity in explications encumbers
;; the language with several instances of acataleptic passages, a subset
;; of which shall be the following listing's cynosure.
;; 
;; == WHAT CHARACTERS MAY COMPRISE A STRING? ==
;; The Apple Pie protolog conerns itself solely with numbers and letters
;; for output operations, especially in relation with referring to the
;; preceding character; no mentioning of punctuation or other special
;; symbols transpires.
;; 
;; It has been adjudged that merely decimal digits and minuscular as
;; well as majuscular Latin letters may occur in strings.
;; 
;; == ARE SPACES TOLERATED? ==
;; While linebreaks are associated with quines, a comprensive treatise
;; concerning spaces as sepiments exceeds the specification, with the
;; mandatory instance preceding a command terminator character supplying
;; the aefauld exception.
;; 
;; It has been deemed that spaces shall not participate in a valid piece
;; of Apple Pie code apart from the aforementioned use case.
;; 
;; 
;; Implementation
;; ==============
;; This simple interpreter's realization proceeds in the programming
;; language Common Lisp, uniting, ensuing from the kenspeckle design of
;; Apple Pie, the lexer and parser stages into an approximate amalgam.
;; 
;; == LEXER AND PARSER OPERATE IN CLOSE ADUNATION ==
;; A catopter of its perplex syntactical regulations, the parser queries
;; the lexer's wikes for adjudging the fitten actions in the face of the
;; current lexical analyzation state, producing abstract syntax tree
;; (AST) nodes from this effort.
;; 
;; == NODE REPRESENTATIONS ==
;; The Procrustean administration of the ``Node'' class to the
;; multifarious Apple Pie commands begets the necessity of a
;; differentiation among the consequential aspects by adminiculum of
;; the categorizing node type and the detailing attributes.
;; 
;; The following table ostends these properties of the ``Node'' class
;; as adhibited to the language commands.
;; 
;;   ------------------------------------------------------------------
;;   Command                      | Node representation
;;   -----------------------------+------------------------------------
;;   Good luck reading this lol u | No node representation.
;;   ..................................................................
;;   A{argument}                  | Type
;;                                |   :output
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :argument
;;   ..................................................................
;;   B{text}                      | No node representation.
;;   ..................................................................
;;   D{variable}D{value}          | Type
;;                                |   :variable-definition
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :variable
;;                                |   :value
;;   ..................................................................
;;   EepbeepQ{repetitions}        | Type
;;                                |   :loop
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :repetitions
;;                                |   :body
;;   ..................................................................
;;   F{left}F{operator}F{right}   | Type
;;                                |   :binary-operation
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :left
;;                                |   :operator
;;                                |   :right
;;   ..................................................................
;;   ${variable}F                 | Type
;;                                |   :variable-access
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :variable
;;   ..................................................................
;;   G{HQ9+Commands}              | Type
;;                                |   :interpret-HQ9+
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :commands
;;   ..................................................................
;;   H44{variable}                | Type
;;                                |   :variable-clearance
;;                                |------------------------------------
;;                                | Attributes
;;                                |   :variable
;;   ..................................................................
;;   !!!                          | No node representation.
;;   ..................................................................
;;   <newline>                    | Type
;;                                |   :quine
;;                                | Attributes
;;                                |   :code
;;   ------------------------------------------------------------------
;; 
;; The ``:commands'' attribute of the "G{HQ9+Commands}" command's node
;; representation maintains a list of nodes itself, each identified by
;; the ``:HQ9+-command'' keyword, and associated with a mandatory
;; ``:command'' attribute as well as an optional ``:argument'' property.
;; 
;; The following table provides an exposition of these attributes per
;; any of the quadruple HQ9+ instructions' node models:
;; 
;;   ------------------------------------------------------------------
;;   HQ9+ command | :command attribute  | :argument attribute
;;   -------------+---------------------+------------------------------
;;   H            |                     |
;;   .............| :hello-world        | None.
;;   h            |                     |
;;   ..................................................................
;;   Q            |                     |
;;   .............| :quine              | Apple Pie source code.
;;   q            |                     |
;;   ..................................................................
;;   9            | :99-bottles-of-beer | None.
;;   ..................................................................
;;   +            | :increment          | None.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-06-04
;; 
;; Sources:
;;   [esolang2020ApplePie]
;;   The Esolang contributors, "Apple Pie", 2020
;;   URL: "https://esolangs.org/wiki/Apple_Pie"
;;   
;;   [esolang2023HQ9+]
;;   The Esolang contributors, "HQ9+", 2023
;;   URL: "https://esolangs.org/wiki/HQ9%2B"
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   compact of zero or more entries, represented by a simple list whose
   indicators (keys) are immediately succeeded by the associated value,
   the former being of the INDICATOR-TYPE, the latter assuming the
   VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, without the claim of exhaustion, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype radix ()
  "The ``radix'' type defines the valid bases for number systems."
  '(integer 2 36))

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-map'' defines a collection of node attributes,
   amenable to their value's inquisition by adminiculum of their names,
   and manifesting in the form of a hash table which associates the
   keyword symbol names with arbitrary objects as values."
  '(hash-table-of keyword T))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype operator ()
  "The ``operator'' type enumerates the recognized binary operators."
  '(member :plus :minus :times :division :power))

;;; -------------------------------------------------------

(deftype variable-map ()
  "The ``variable-map'' type defines a registry for the association of
   variable names with ``APVariable'' objects, implemented in the form
   of a hash table, mapping name strings to variables."
  '(hash-table-of string APVariable))

;;; -------------------------------------------------------

(deftype source ()
  "The ``source'' type defines a provenance for the obtentation of a
   piece of Apple Pie source code."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 26) +ALPHABET+))
(declaim (type fixnum             +ALPHABET-SIZE+))
(declaim (type fixnum             +TERMINATOR-OFFSET+))

;;; -------------------------------------------------------

(defparameter +ALPHABET+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The letters composing the alphabet.")

(defparameter +ALPHABET-SIZE+ (length +ALPHABET+)
  "The number of letters in the alphabet.")

(defparameter +TERMINATOR-OFFSET+ 9
  "The number of positions by which the terminating character differs
   from a command character in the alphabet.")

;;; -------------------------------------------------------

(defun get-letter (position)
  "Returns the letter associated with the zero-based POSITION in the
   alphabet, contingently wrapping this designator around in order to
   achieve a correspondence."
  (declare (type fixnum position))
  (the character
    (schar +ALPHABET+
      (mod position +ALPHABET-SIZE+))))

;;; -------------------------------------------------------

(defun get-position-in-alphabet (letter)
  "Returns the zero-based position of the LETTER in the alphabet, or
   signals an error of an unspecified type if the same does not
   constitute a member."
  (declare (type character letter))
  (the fixnum
    (or (position letter +ALPHABET+ :test #'char-equal)
        (error "The letter \"~c\" is foreign to the alphabet."
          letter))))

;;; -------------------------------------------------------

(defun assume-case (character-to-adjust character-to-mimic)
  "Returns the CHARACTER-TO-ADJUST converted into the case of the
   CHARACTER-TO-MIMIC."
  (declare (type character character-to-adjust))
  (declare (type character character-to-mimic))
  (the character
    (cond
      ((upper-case-p character-to-mimic)
        (char-upcase character-to-adjust))
      ((lower-case-p character-to-mimic)
        (char-downcase character-to-adjust))
      (T
        (error "Cannot determine the case of ~c."
          character-to-mimic)))))

;;; -------------------------------------------------------

(defun get-terminating-character (command-character)
  "Returns the character associated as a terminating entity with the
   command introduced by the COMMAND-CHARACTER, or signals an error of
   an unspecified type if the latter does not constitute a member of the
   alphabet."
  (declare (type character command-character))
  (the character
    (assume-case
      (get-letter
        (+ (get-position-in-alphabet command-character)
           +TERMINATOR-OFFSET+))
      command-character)))

;;; -------------------------------------------------------

(defun letter-p (candidate)
  "Checks whether the CANDIDATE represents a letter from the +ALPHABET+,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate +ALPHABET+ :test #'char-equal)))))

;;; -------------------------------------------------------

(defun get-previous-digit (digit)
  "Returns the digit immediately preceding the DIGIT, wrapping around if
   necessary."
  (declare (type character digit))
  (let ((digit-value (digit-char-p digit)))
    (declare (type (integer 0 9) digit-value))
    (the character
      (digit-char (mod (1- digit-value) 10)))))

;;; -------------------------------------------------------

(defun get-previous-letter (letter)
  "Returns the letter immediately preceding the LETTER in the alphabet."
  (declare (type character letter))
  (the character
    (assume-case
      (get-letter
        (mod (1- (get-position-in-alphabet letter))
             +ALPHABET-SIZE+))
      letter)))

;;; -------------------------------------------------------

(defun get-previous-character (character)
  "Returns the charater immediately preceding the CHARACTER."
  (declare (type character character))
  (the character
    (cond
      ((digit-char-p character)
        (get-previous-digit character))
      ((letter-p character)
        (get-previous-letter character))
      (T
        (error "The character ~s does not represent a letter or digit."
          character)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIATE represents a mathematical sign,
   either \"+\" (plus) or \"-\" (minus), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a character serving to
   introduce a command, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (find candidate "ABDEFGH$" :test #'char=)
          (char= candidate #\Newline))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class represents a subtree or leaf in an abstract syntax
   tree (AST), categorized by its type, and capable of storing pertinent
   data in an ``attribute-map''."
  (type       (error "Missing node type.") :type keyword)
  (attributes (make-hash-table :test #'eq) :type attribute-map))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' of the specified TYPE, its
   attributes optionally resolving to the INITIAL-ATTRIBUTES, supplied
   as a flat list of key-value pairs in immediate succession."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) initial-attributes))
  (let ((new-node (initialize-node type)))
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
  "Returns the value of NODE attribute associated with the
   ATTRIBUTE-NAME, or signals an error if no such designator could be
   detected."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (multiple-value-bind (attribute-value contains-attribute-p)
      (gethash attribute-name (node-attributes node))
    (declare (type T attribute-value))
    (declare (type T contains-attribute-p))
    (the T
      (if contains-attribute-p
        attribute-value
        (error "Invalid attribute for node ~s: ~s."
          node attribute-name)))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (attribute-value node attribute-name)
  "Associates the ATTRIBUTE-NAME with the ATTRIBUTE-VALUE in the NODE
   and returns no value."
  (declare (type T       attribute-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (node-attributes node))
        attribute-value)
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "(Node ~s, " (node-type node))
  (format stream "attributes=[")
  (loop
    for attribute-name
      of-type keyword
      being the hash-keys in (node-attributes node)
    using
      (hash-value attribute-value)
    and first-attribute-p
      of-type boolean
      = T then NIL
    do
      (unless first-attribute-p
        (format stream ", "))
      (format stream "~s=>~s" attribute-name attribute-value))
  (format stream "])"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (or null string)    *source*))
(declaim (type fixnum              *position*))
(declaim (type (or null character) *character*))
(declaim (type string              *copy-of-source*))

;;; -------------------------------------------------------

(defparameter *source* NIL
  "The string comprehending the piece of Apple Pie code to evaluate.")

(defparameter *position* 0
  "The index into the *SOURCE*'s current character.")

(defparameter *character* NIL
  "The character at the current *POSITION* into the *SOURCE*.")

(defparameter *copy-of-source*
  (make-array 0 :element-type 'character :adjustable T :fill-pointer 0)
  "A dynamic string which collects all characters traversed by the
   *SOURCE*, in case of a quine's necessitation.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signal-eof-error (&key (expected-content "a character")
                              (position          *position*))
  "Signals an error of an unspecified type informing about the
   EXPECTED-CONTENT having been failed to match at the POSITION because
   of the lexer source having arrived at the end of file (EOF)."
  (declare (type T      expected-content))
  (declare (type fixnum position))
  (error "Expected ~a at position ~d, but encountered EOF."
    expected-content position)
  (values))

;;; -------------------------------------------------------

(defun signal-character-error (&key (expected-content "a character")
                                    (position *position*))
  "Signals an error of an unspecified type informing about the
   EXPECTED-CONTENT having been failed to match the POSITION because
   of the lexer source having encountered the mismatching current
   *CHARACTER*."
  (declare (type T      expected-content))
  (declare (type fixnum position))
  (error "Expected ~a at position ~d, but encountered the character ~c."
    expected-content position *character*)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eof-p ()
  "Checks whether the *SOURCE* is exhausted, in which case the
   *POSITION* has reached its desinent location and the current
   *CHARACTER* has assumed a value of ``NIL'', returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (null *character*)))

;;; -------------------------------------------------------

(defun on-character-p (expected-character)
  "Determines whether the *CHARACTER* currently resides on the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character expected-character))
  (the boolean
    (not (null
      (and *character*
           (char= *character* expected-character))))))

;;; -------------------------------------------------------

(defun on-space-p ()
  "Checks whether the *CHARACTER* currently resides on a space,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (not (null
      (and *character*
           (char= *character* #\Space))))))

;;; -------------------------------------------------------

(defun on-digit-p (base)
  "Checks whether the *CHARACTER* currently resides on a digit in the
   BASE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type radix base))
  (the boolean
    (not (null
      (and *character*
           (digit-char-p *character* base))))))

;;; -------------------------------------------------------

(defun on-comment-p ()
  "Determines whether the *CHARACTER* currently resides on the start of
   a comment section, instigated by the token \"B\", returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (and *character*
           (char= *character* #\B))))))

;;; -------------------------------------------------------

(defun on-letter-p ()
  "Checks whether the *CHARACTER* currently resides on a letter from the
   +ALPHABET+, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (not (null
      (and *character*
           (alpha-char-p *character*))))))

;;; -------------------------------------------------------

(defun on-dollar-sign-p ()
  "Checks whether the *CHARACTER* currently resides on the dollar sign
   \"$\", returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (not (null
      (and *character*
           (char= *character* #\$))))))

;;; -------------------------------------------------------

(defun memorize-character ()
  "Appends the *CHARACTER* to the *COPY-OF-SOURCE*, returning no value."
  (vector-push-extend *character* *copy-of-source*)
  (values))

;;; -------------------------------------------------------

(defun advance ()
  "Moves the *POSITION* cursor to the next character in the *SOURCE*, if
   possible, updates the current *CHARACTER*, and returns no value."
  (memorize-character)
  (when (array-in-bounds-p *source* (1+ *position*))
    (incf *position*)
    (setf *character* (char *source* *position*)))
  (values))

;;; -------------------------------------------------------

(defun initialize-source (code)
  "Sets the *SOURCE* to the CODE, resets all state variables, and
   returns no value."
  (setf *source*   code)
  (setf *position* 0)
  (setf *character*
    (when (array-in-bounds-p *source* *position*)
      (char *source* *position*)))
  (setf (fill-pointer *copy-of-source*) 0)
  (values))

;;; -------------------------------------------------------

(defun expect-character (expected-character)
  "Determines whether the *CHARACTER* equals the EXPECTED-CHARACTER, on
   confirmation simply terminating and returning no value, otherwise
   signaling an error of an unspecified type."
  (declare (type character expected-character))
  (cond
    ((null *character*)
      (error "Expected the character ~c at position ~d, ~
              but encountered EOF."
        expected-character *position*))
    ((char= *character* expected-character)
      (advance))
    (T
      (error "Expected the character ~c at position ~d, ~
              but encountered ~c."
        expected-character *position* *character*)))
  (values))

;;; -------------------------------------------------------

(defun expect-string (expected-string)
  "Determines whether the *SOURCE* portion starting with the current
   *CHARACTER* equals the EXPECTED-STRING, on confirmation simply
   terminating and returning no result, otherwise signaling an error of
   an unspecified type."
  (declare (type string expected-string))
  (loop
    for expected-character
      of-type character
      across  expected-string
    do
      (cond
        ;; The source is exhausted ere matching the EXPECTED-STRING?
        ((eof-p)
          (signal-eof-error :expected-content expected-character))
        ;; The current character differs from the expected one?
        ((char/= *character* expected-character)
          (signal-character-error :expected-content expected-character))
        ;; Current character matches the expected character?
        (T
          (advance))))
  (values))

;;; -------------------------------------------------------

(defun expect-variable-name ()
  "Determines whether the *CHARACTER* currently resides on a valid
   variable identifier, on confirmation returning the *CHARACTER* while
   advancing to the next position in the *SOURCE*, otherwise signaling
   an error of an unspecified type."
  (the character
    (if (on-letter-p)
      (prog1 *character*
        (advance))
      (error "Expected an identifier, but encountered ~s at ~
              position ~d."
        *character* *position*))))

;;; -------------------------------------------------------

(defun expect-space ()
  "Determines whether the *CHARACTER* currently resides on a space, on
   confirmation simply terminating and returning no value, otherwise
   signaling an error of an unspecified type."
  (if (and (not (eof-p)) (on-space-p))
    (advance)
    (error "Expected a space, but encoutered ~s at position ~d."
      *character* *position*))
  (values))

;;; -------------------------------------------------------

(defun expect-terminator-for (command-name)
  "Determines whether the *CHARACTER* equals the terminating character
   corresponding to the COMMAND-NAME, on confirmation simply terminating
   and returning no value, otherwise signaling an error of an
   unspecified type."
  (declare (type character command-name))
  (expect-space)
  (let ((command-terminator (get-terminating-character command-name)))
    (declare (type character command-terminator))
    (if (and (not (eof-p))
             (on-character-p command-terminator))
      (advance)
      (error "Expected the command terminator ~c for the command name ~
              ~c, but encountered ~s at position ~d."
        command-terminator command-name *character* *position*)))
  (values))

;;; -------------------------------------------------------

(defun read-number (&optional (base 10))
  "Starting at the current *POSITION* into the *SOURCE*, reads a
   sequence of zero or more digits in the BASE and returns the parsed
   value in the decimal system."
  (declare (type (integer 2 36) base))
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        ;; Prepend optional sign.
        (when (sign-character-p *character*)
          (write-char *character* digits)
          (advance))
        ;; Read digits.
        (loop while (on-digit-p base) do
          (write-char *character* digits)
          (advance)))
      :radix base)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () node-list) parse-commands))
(declaim (ftype (function () Node)      parse-command-F))

;;; -------------------------------------------------------

(defun parse-number (&optional (base 10))
  "Parses a number interpreted in the specified BASE and returns a
   node representation thereof."
  (declare (type (integer 2 36) base))
  (the Node
    (make-node :integer
      :value (read-number base))))

;;; -------------------------------------------------------

(defun parse-string ()
  "Parses a string and returns a node representation thereof."
  (the Node
    (make-node :string :value
      (with-output-to-string (letters)
        (declare (type string-stream letters))
        (loop while (on-letter-p) do
          (write-char *character* letters)
          (advance))))))

;;; -------------------------------------------------------

(defun parse-variable-name ()
  "Parses a one-letter variable name and returns a node representation
   thereof."
  (let ((variable-name (expect-variable-name)))
    (declare (type character variable-name))
    (the Node
      (make-node :variable-name
        :name (string variable-name)))))

;;; -------------------------------------------------------

(defun skip-comment ()
  "If the *SOURCE*'s *POSITION* cursor is currently located on a \"B\"
   command token, skips the comment and returns no value."
  (when (on-character-p #\B)
    (advance)
    (loop do
      (case *character*
        ((NIL)
          (signal-eof-error :expected-content "end of comment"))
        (#\Space
          (expect-terminator-for #\B)
          (loop-finish))
        (otherwise
          (advance)))))
  (values))

;;; -------------------------------------------------------

(defun skip-comments ()
  "Skips an adjacent series of zero or more comment blocks, each
   introduced via a \"B\" command identifier, and returns no value."
  (loop while (and *character* (char= *character* #\B)) do
    (skip-comment))
  (values))

;;; -------------------------------------------------------

(defun parse-variable-access ()
  "Parses a variable value query command, \"$xF\", and returns a
   ``:variable-value'' node representation thereof."
  (advance)
  (let ((variable-name (parse-variable-name)))
    (declare (type Node variable-name))
    (expect-character #\F)
    (the Node
      (make-node :variable-value :variable variable-name))))

;;; -------------------------------------------------------

(defun parse-word ()
  "Parses a word compact of one or more alphanumeric characters and
   terminated by a space and returns a ``:string'' node representation
   thereof."
  (the Node
    (make-node :string :value
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop while (and *character* (alphanumericp *character*)) do
          (write-char *character* content)
          (advance))))))

;;; -------------------------------------------------------

(defun skip-word ()
  "Skips the alphanumeric characters comprising a word until a space is
   encountered and returns no value."
  (loop while (and *character* (alphanumericp *character*)) do
    (advance))
  (values))

;;; -------------------------------------------------------

(defun parse-output-argument ()
  "Parses an argument for the print command \"A\" invocation and returns
   a node representation thereof."
  (the Node
    (cond
      ((eof-p)
        (signal-eof-error :expected-content "an input"))
      ((on-dollar-sign-p)
        (parse-variable-access))
      ((alphanumericp *character*)
        (parse-word))
      (T
        (signal-character-error :expected-content "an input")))))

;;; -------------------------------------------------------

(defun parse-command-A ()
  "Parses an invocation of the print command \"A\" and returns an
   ``:output'' node representation thereof."
  (expect-character #\A)
  (let ((input (parse-output-argument)))
    (declare (type Node input))
    (the Node
      (make-node :output :argument input))))

;;; -------------------------------------------------------

(defun parse-assignment-argument (base)
  "Parses a value eligible for the assignment to a variable in the
   variable definition command \"D\" invocation and returns a node
   representation thereof."
  (declare (type radix base))
  (the Node
    (cond
      ((eof-p)
        (signal-eof-error :expected-content
          (format NIL "a base~d number or variable access" base)))
      ((on-digit-p base)
        (parse-number base))
      ((sign-character-p *character*)
        (parse-number base))
      ((on-dollar-sign-p)
        (parse-variable-access))
      ((char= *character* #\F)
        (parse-command-F))
      ((on-letter-p)
        (parse-string))
      (T
        (signal-character-error :expected-content
          (format NIL "a base-~d number or variable access" base))))))

;;; -------------------------------------------------------

(defun parse-command-D ()
  "Parses a variable definition command invocation, \"DxDx\", and
   returns a ``:variable-definition'' node representation thereof."
  (expect-character #\D)
  (skip-comment)
  (let ((variable (parse-variable-name)))
    (declare (type Node variable))
    (expect-character #\D)
    (let ((value (parse-assignment-argument 10)))
      (declare (type Node value))
      (the Node
        (make-node :variable-definition
          :variable variable
          :value    value)))))

;;; -------------------------------------------------------

(defun parse-numeric-expression (base)
  "Parses an expression which produces a numeric value, embracing either
   an integer number in the BASE, a variable access (\"$xF\"), or an
   \"F\" command invocation, and returns a node representation thereof."
  (declare (type radix base))
  (the Node
    (cond
      ((eof-p)
        (signal-eof-error :expected-content
          (format NIL "a base~d number of variable access" base)))
      ((on-digit-p base)
        (parse-number base))
      ((sign-character-p *character*)
        (parse-number base))
      ((on-dollar-sign-p)
        (parse-variable-access))
      ((char= *character* #\F)
        (parse-command-F))
      (T
        (signal-character-error :expected-content
          (format NIL "a base-~d number of variable access" base))))))

;;; -------------------------------------------------------

(defun parse-command-C ()
  "Parses the loop terminator command, \"C\", and returns no value."
  (skip-comment)
  (expect-string "C L")
  (values))

;;; -------------------------------------------------------

(defun parse-command-EepbeepQ ()
  "Parses the loop start command \"EepbeepQx\", extending to the closing
   \"C\" marker, and returns a ``:loop'' node representation thereof."
  (expect-string "EepbeepQ")
  (let ((repetitions (parse-numeric-expression 3)))
    (declare (type Node repetitions))
    (expect-terminator-for #\Q)
    (let ((body (parse-commands)))
      (declare (type node-list body))
      (parse-command-C)
      (the Node
        (make-node :loop :repetitions repetitions :body body)))))

;;; -------------------------------------------------------

(defun parse-binary-operator ()
  "Parses an operator token and returns the corresponding ``operator''."
  (the operator
    (prog1
      (case *character*
        ((NIL)
          (signal-eof-error :expected-content "an operator token"))
        (#\+ :plus)
        (#\- :minus)
        (#\* :times)
        (#\/ :division)
        (#\^ :power)
        (otherwise
          (signal-character-error
            :expected-content "an operator token")))
      (advance))))

;;; -------------------------------------------------------

(defun parse-command-F ()
  "Parses an \"FxFxFx\" command invocation and returns a
   ``:binary-operation'' node representation thereof."
  (expect-character #\F)
  (let ((left-operand  NIL)
        (operator      NIL)
        (right-operand NIL))
    (declare (type (or null Node)     left-operand))
    (declare (type (or null operator) operator))
    (declare (type (or null Node)     right-operand))
    (setf left-operand (parse-numeric-expression 10))
    (expect-character #\F)
    (setf operator (parse-binary-operator))
    (expect-character #\F)
    (setf right-operand (parse-numeric-expression 10))
    (the Node
      (make-node :binary-operation
        :left     left-operand
        :operator operator
        :right    right-operand))))

;;; -------------------------------------------------------

(defun parse-HQ9+-commands ()
  "Parses a sequence of zero or more HQ9+ instructions, terminating upon
   a space's encounter, and returns a list of their node
   representations."
  (let ((hq9+-commands NIL))
    (declare (type node-list hq9+-commands ))
    (flet ((collect-command (command-type &optional (argument NIL))
            "Creates a new HQ9+ command node, encapsulating in its
             attributes the COMMAND-TYPE and an optional ARGUMENT,
             prepends the new node to the HQ9+-COMMANDS list, and
             returns no value."
            (declare (type keyword command-type))
            (declare (type T       argument))
            (push (make-node :HQ9+-command
                    :command  command-type
                    :argument argument)
                  hq9+-commands)
            (values)))
      (loop do
        (case *character*
          ((NIL)
            (signal-eof-error))
          ((#\H #\h)
            (advance)
            (collect-command :hello-world))
          ((#\Q #\q)
            (advance)
            (collect-command :quine *copy-of-source*))
          (#\9
            (advance)
            (collect-command :99-bottles-of-beer))
          (#\+
            (advance)
            (collect-command :increment))
          (#\Space
            (loop-finish))
          (otherwise
            (error "Invalid HQ9+ command: ~s." *character*)))))
    (the node-list (nreverse hq9+-commands))))

;;; -------------------------------------------------------

(defun parse-command-G ()
  "Parses a \"Gx\" command invocation and returns an ``:interpret-HQ9+''
   node representation thereof."
  (expect-character #\G)
  (let ((instructions NIL))
    (declare (type node-list instructions))
    (if *character*
      (setf instructions (parse-HQ9+-commands))
      (error "Expected an HQ9+ command or a space, ~
              but encountered EOF."))
    (the Node
      (make-node :interpret-HQ9+ :commands instructions))))

;;; -------------------------------------------------------

(defun parse-command-H ()
  "Parses an \"H44x\" command invocation and returns a
   ``:variable-clearance'' node representation thereof."
  (expect-string "H44")
  (let ((variable (parse-variable-name)))
    (declare (type Node variable))
    (the Node
      (make-node :variable-clearance :variable variable))))

;;; -------------------------------------------------------

(defun parse-newline ()
  "Parses a newline character and returns a quine node."
  (advance)
  (the Node (make-node :quine :code *copy-of-source*)))

;;; -------------------------------------------------------

(defun parse-command ()
  "Parses a single command and returns a node representation thereof, or
   responds with the ``NIL'' value if none could be detected."
  (the (or null Node)
    (case *character*
      (#\A
        (prog1
          (parse-command-A)
          (expect-terminator-for #\A)))
      
      (#\C
        (error "Unmatched loop end marker C at position ~d."
          *position*))
      
      (#\D
        (prog1
          (parse-command-D)
          (expect-terminator-for #\D)))
      
      (#\E
        (parse-command-EepbeepQ))
      
      (#\F
        (prog1
          (parse-command-F)
          (expect-terminator-for #\F)))
      
      (#\G
        (prog1
          (parse-command-G)
          (expect-terminator-for #\G)))
      
      (#\H
        (prog1
          (parse-command-H)
          (expect-terminator-for #\H)))
      
      (#\Newline
        (parse-newline))
      
      (#\$
        (prog1
          (parse-variable-access)
          (expect-terminator-for #\F)))
      
      (otherwise
        (error "No command character: ~s." *character*)))))

;;; -------------------------------------------------------

(defun parse-commands ()
  "Parses a list of zero or more command nodes and returns the same."
  (the node-list
    (loop
      do      (skip-comments)
      while   (and *character* (command-character-p *character*))
      collect (parse-command))))

;;; -------------------------------------------------------

(defun parse-start-of-program ()
  "Parse the start of the program and returns no value."
  (expect-string "Good luck reading this lol u")
  (values))

;;; -------------------------------------------------------

(defun parse-end-of-program ()
  "Parses the end of the program and returns no value."
  (expect-string "!!!")
  (values))

;;; -------------------------------------------------------

(defun parse-program ()
  "Parses an Apple Pie program, compact of zero or more nodes, and
   returns the abstract syntax tree (AST) root in the form of a program
   node."
  (parse-start-of-program)
  (prog1
    (make-node :program :body (parse-commands))
    (parse-end-of-program)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "APVariable".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (APVariable
  (:constructor make-apvariable (name value)))
  "The ``APVariable'' class represents a variable composed of an
   identifying name and an arbitrary value."
  (name  (error "Missing variable name.")  :type string)
  (value (error "Missing variable value.") :type T))

;;; -------------------------------------------------------

(defun apvariable-clear (variable)
  "Clears the VARIABLE's value and returns the modified VARIABLE."
  (declare (type APVariable variable))
  (setf (apvariable-value variable) 0)
  (the APVariable variable))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of output operation.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric output (object)
  (:documentation
    "Prints the OBJECT to the standard output in a form appropriate for
     its nature, and returns no value."))

;;; -------------------------------------------------------

(defmethod output ((variable APVariable))
  "Prints the VARIABLE's value backswards to the standard output and
   returns no value."
  (declare (type APVariable variable))
  (format T "~a"
    (reverse
      (format NIL "~a" (apvariable-value variable))))
  (values))

;;; -------------------------------------------------------

(defmethod output ((string string))
  "Prints the letter immediately preceding the STRING's first character
   to the standard output and returns no value."
  (declare (type string string))
  (format T "~c"
    (get-previous-character
      (char string 0)))
  (values))

#|
;;; -------------------------------------------------------

(defmethod output ((integer integer))
  "Prints the digit immediately preceding the INTEGER's first character
   to the standard output and returns no value."
  (declare (integer integer))
  (format T "~d"
    (get-previous-number integer))
  (values))
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-operator (operator left-operand right-operand)
  (:documentation
    "Applies the OPERATOR to the LEFT-OPERAND and the RIGHT-OPERAND in
     this order and returns a result meet for this combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operation (operator-type
                                   (left-operand-type
                                    right-operand-type)
                                   &body body)
  "Defines an implementation of the generic function ``apply-operator''
   with the operator set to an automatically created symbol and employed
   as the dispatching criterion through its equality to the
   OPERATOR-TYPE, the left operand being agnominated ``left-operand''
   and specified with the LEFT-OPERAND-TYPE, the right operand stated as
   ``right-operand'' and specified with the RIGHT-OPERAND-TYPE,
   evaluates the BODY forms, and returns the last executed form's
   results."
  (let ((operator-variable (gensym)))
    (declare (type symbol operator-variable))
    `(defmethod apply-operator
         ((,operator-variable (eql ,operator-type))
          (left-operand       ,left-operand-type)
          (right-operand      ,right-operand-type))
       (declare (type operator            ,operator-variable))
       (declare (type ,left-operand-type  left-operand))
       (declare (type ,right-operand-type right-operand))
       (declare (ignore                   ,operator-variable))
       (declare (ignorable                left-operand))
       (declare (ignorable                right-operand))
       ,@body)))

;;; -------------------------------------------------------

(define-binary-operation :plus (integer integer)
  (the integer
    (+ left-operand right-operand)))

;;; -------------------------------------------------------

(define-binary-operation :minus (integer integer)
  (the integer
    (- left-operand right-operand)))

;;; -------------------------------------------------------

(define-binary-operation :times (integer integer)
  (the integer
    (* left-operand right-operand)))

;;; -------------------------------------------------------

(define-binary-operation :division (integer integer)
  (the integer
    (round left-operand right-operand)))

;;; -------------------------------------------------------

(define-binary-operation :power (integer integer)
  (the integer
    (expt left-operand right-operand)))

;;; -------------------------------------------------------

(defmethod apply-operator ((operator      T)
                           (left-operand  APVariable)
                           (right-operand integer))
  (declare (type T          operator))
  (declare (type APVariable left-operand))
  (declare (type integer    right-operand))
  (the integer
    (apply-operator operator
      (apvariable-value left-operand)
      right-operand)))

;;; -------------------------------------------------------

(defmethod apply-operator ((operator      T)
                           (left-operand  integer)
                           (right-operand APVariable))
  (declare (type T          operator))
  (declare (type integer    left-operand))
  (declare (type APVariable right-operand))
  (the integer
    (apply-operator operator left-operand
      (apvariable-value right-operand))))

;;; -------------------------------------------------------

(defmethod apply-operator ((operator      T)
                           (left-operand  APVariable)
                           (right-operand APVariable))
  (declare (type T          operator))
  (declare (type APVariable left-operand))
  (declare (type APVariable right-operand))
  (the integer
    (apply-operator operator
      (apvariable-value left-operand)
      (apvariable-value right-operand))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation interpretation operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-quine ()
  "Prints the *SOURCE* program to the standard output and returns no
   value."
  (format T "~a" *copy-of-source*)
  (values))

;;; -------------------------------------------------------

(defun print-99-bottles-of-beer ()
  "Prints the lyrics of the program \"99 bottles of beer\" to the
   standard output and returns no value."
  (loop
    for number-of-bottles of-type (integer 1 99) from 99 downto 2
    do
      (format T "~d~:* bottles of beer on the wall,~@
                 ~d bottles of beer.~@
                 Take one down, pass it around,~@
                 ~d~:* bottle~p of beer on the wall.~2%"
        number-of-bottles
        (1- number-of-bottles))
    finally
      (format T "1 bottle of beer on the wall,~@
                 1 bottle of beer.~@
                 Take one down, pass it around,~@
                 No bottles of beer on the wall."))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of HQ9+ interpreter.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (HQ9+-Context
  (:constructor make-hq9+-context (quine-code console-height)))
  "The ``HQ9+-Context'' class encapsulates all data requisite for the
   execution of a series of connected HQ9+ instructions."
  (accumulator    0  :type integer)
  (quine-code     "" :type string)
  (console-height 20 :type (integer 0 *)))

;;; -------------------------------------------------------

(defgeneric execute-HQ9+-instruction (context instruction)
  (:documentation
    "Applies the HQ9+ INSTRUCTION in the specified CONTEXT and returns
     no value.")
  
  (:method ((context HQ9+-Context) (instruction (eql :hello-world)))
    (declare (type HQ9+-Context context))
    (declare (type keyword      instruction))
    (declare (ignore            context))
    (declare (ignore            instruction))
    (format T "hello, world")
    (values))
  
  (:method ((context HQ9+-Context) (instruction (eql :quine)))
    (declare (type HQ9+-Context context))
    (declare (type keyword      instruction))
    (declare (ignore            instruction))
    (format T "~v%~a"
      (hq9+-context-console-height context)
      (hq9+-context-quine-code     context))
    (values))
  
  (:method ((context     HQ9+-Context)
            (instruction (eql :99-bottles-of-beer)))
    (declare (type HQ9+-Context context))
    (declare (type keyword      instruction))
    (declare (ignore            context))
    (declare (ignore            instruction))
    (print-99-bottles-of-beer)
    (values))
  
  (:method ((context HQ9+-Context) (instruction (eql :increment)))
    (declare (type HQ9+-Context context))
    (declare (type keyword      instruction))
    (declare (ignore            instruction))
    (incf (hq9+-context-accumulator context))
    (values))
  
  (:method ((context HQ9+-Context) (instruction T))
    (declare (type HQ9+-Context context))
    (declare (type keyword      instruction))
    (declare (ignore            context))
    (error "The HQ9+ instruction ~s is invalid." instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Apple Pie interpreter.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing interpreter AST.")
    :type          Node
    :documentation "The abstract syntax tree (AST) to process.")
   (variables
    :initarg       :variable
    :initform      (make-hash-table :test #'equal)
    :type          variable-map
    :documentation "The variable registry which associated with the
                    variable names under its governance the
                    representative ``APVariable'' objects.")
   (console-height
    :initarg       :console-height
    :initform      20
    :type          (integer 0 *)
    :documentation "The number of lines comprising the console recepient
                    of the printing operations, used for simulating the
                    clearing of all content preceding a quine
                    operation."))
  (:documentation
    "The ``Interpreter'' class furnishes an entity apportioned the
     responsibility for applying actual effect to an abstract syntax
     tree (AST) representation of an Apple Pie program."))

;;; -------------------------------------------------------

(defun make-interpreter (tree &key (console-height 20))
  "Creates and returns a new ``Interpreter'', responsible for the
   traversal of the abstract syntax TREE."
  (declare (type Node          tree))
  (declare (type (integer 0 *) console-height))
  (the Interpreter
    (make-instance 'Interpreter
      :tree           tree
      :console-height console-height)))

;;; -------------------------------------------------------

(defun interpreter-register-variable (interpreter variable)
  "Registers the VARIABLE with its name at the INTERPRETER, superseding
   any contingent entry with an equal identifier, and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type APVariable  variable))
  (setf (gethash (apvariable-name variable)
                 (slot-value interpreter 'variables))
        variable)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-contains-variable-p (interpreter name)
  "Determines whether the INTERPRETER maintains a variable identified by
   the NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash name
          (slot-value interpreter 'variables)))))))

;;; -------------------------------------------------------

(defun interpreter-get-variable (interpreter name)
  "Returns the variable registered with the NAME at the INTERPRETER, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (multiple-value-bind (variable contains-name-p)
      (gethash name
        (slot-value interpreter 'variables))
    (declare (type (or null APVariable) variable))
    (declare (type T                    contains-name-p))
    (the (or null APVariable)
      (if contains-name-p
        variable
        (error "No variable with the name ~s found." name)))))

;;; -------------------------------------------------------

(defun interpreter-get-variable-value (interpreter name)
  "Returns the value of the variable registered with the NAME at the
   INTERPRETER, or responds with the default result, the letter \"L\",
   upon its absence."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (multiple-value-bind (variable contains-name-p)
      (gethash name
        (slot-value interpreter 'variables))
    (declare (type (or null APVariable) variable))
    (declare (type T                    contains-name-p))
    (the T
      (if contains-name-p
        (apvariable-value variable)
        "L"))))

;;; -------------------------------------------------------

(defun interpreter-clear-variable (interpreter name)
  "Clears the value of the variable registered with the NAME at the
   INTERPRETER, or refrains from doing so upon its absence, in any case
   returning the INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (multiple-value-bind (variable contains-name-p)
      (gethash name
        (slot-value interpreter 'variables))
    (declare (type (or null APVariable) variable))
    (declare (type T                    contains-name-p))
    (when contains-name-p
      (apvariable-clear variable)
      (remhash name (slot-value interpreter 'variables))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Traverses the NODE, dispatching on the NODE-TYPE, in the context of
     the INTERPRETER, and returns a result meet for the NODE."))

;;; -------------------------------------------------------

(defun visit-node (interpreter node)
  "Traverses the NODE using the INTERPRETER by invoking the
   ``dispatch-node'' generic function implementation affiliated with the
   NODE's type, and returns a  result meet for the same."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T
    (dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defmacro define-node-dispatch (node-type
                                (interpreter-variable node-variable)
                                &body body)
  "Defines an implementation of the ``dispatch-node'' generic function,
   utilizing the INTERPRETER-VARIABLE as the name of the first argument,
   the NODE-VARIABLE as that of the third, while dispatching on an
   automatically named second variable that dispatches on the NODE-TYPE,
   evaluates the BODY forms, and returns the last evaluated form's
   results."
  (let ((node-type-variable (gensym)))
    (declare (type symbol node-type-variable))
    `(defmethod dispatch-node
         ((,interpreter-variable Interpreter)
          (,node-type-variable   (eql ,node-type))
          (,node-variable        Node))
       (declare (type Interpreter ,interpreter-variable))
       (declare (type keyword     ,node-type-variable))
       (declare (type Node        ,node-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (ignore           ,node-type-variable))
       (declare (ignorable        ,node-variable))
       ,@body)))

;;; -------------------------------------------------------

(define-node-dispatch :program (interpreter node)
  (let ((commands (node-attribute node :body)))
    (declare (type node-list commands))
    (dolist (command commands)
      (declare (type Node command))
      (visit-node interpreter command)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :output (interpreter node)
  (let ((argument (node-attribute node :argument)))
    (declare (type T argument))
    (output
      (if (eq (node-type argument) :variable-value)
        (let ((variable-name
                (node-attribute
                  (node-attribute argument :variable)
                  :name)))
          (declare (type string variable-name))
          (if (interpreter-contains-variable-p interpreter variable-name)
            (interpreter-get-variable interpreter variable-name)
            "L"))
        (visit-node interpreter
          (node-attribute node :argument)))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :string (interpreter node)
  (the string
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :integer (interpreter node)
  (the integer
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :variable-name (interpreter node)
  (the string
    (node-attribute node :name)))

;;; -------------------------------------------------------

(define-node-dispatch :variable-value (interpreter node)
  (let ((variable (node-attribute node :variable)))
    (declare (type Node variable))
    (let ((variable-name (visit-node interpreter variable)))
      (declare (type string variable-name))
      (the T
        (interpreter-get-variable-value interpreter variable-name)))))

;;; -------------------------------------------------------

(define-node-dispatch :variable-definition (interpreter node)
  (let ((variable-name
          (visit-node interpreter
            (node-attribute node :variable)))
        (variable-value
          (visit-node interpreter
            (node-attribute node :value))))
    (declare (type string variable-name))
    (declare (type T      variable-value))
    (interpreter-register-variable interpreter
      (make-apvariable variable-name variable-value)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :loop (interpreter node)
  (let ((repetitions
          (visit-node interpreter
            (node-attribute node :repetitions)))
        (commands
          (node-attribute node :body)))
    (declare (type (integer 0 *) repetitions))
    (declare (type node-list     commands))
    (declare (ignorable          commands))
    (loop repeat repetitions do
      (dolist (command commands)
        (declare (type Node command))
        (visit-node interpreter command))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :binary-operation (interpreter node)
  (let ((left-operand
          (visit-node interpreter
            (node-attribute node :left)))
        (right-operand
          (visit-node interpreter
            (node-attribute node :right)))
        (operator
          (node-attribute node :operator)))
    (declare (type integer  left-operand))
    (declare (type integer  right-operand))
    (declare (type operator operator))
    (the integer
      (apply-operator operator left-operand right-operand))))

;;; -------------------------------------------------------

(define-node-dispatch :interpret-HQ9+ (interpreter node)
  (let ((hq9+-commands (node-attribute node :commands))
        (hq9+-context  (make-hq9+-context *copy-of-source*
                         (slot-value interpreter 'console-height))))
    (declare (type node-list    hq9+-commands))
    (declare (type HQ9+-Context hq9+-context))
    (dolist (hq9+-command hq9+-commands)
      (declare (type Node HQ9+-command))
      (execute-HQ9+-instruction hq9+-context
        (node-attribute hq9+-command :command))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :variable-clearance (interpreter node)
  (let ((variable-name
          (visit-node interpreter
            (node-attribute node :variable))))
    (declare (type string variable-name))
    (interpreter-clear-variable interpreter variable-name))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :quine (interpreter node)
  (format T "~v%~a"
    (slot-value interpreter 'console-height)
    (node-attribute node :code))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Evaluates the INTERPRETER's abstract syntax tree (AST) and returns no
   value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-Apple-Pie (code)
  "Interprets the piece of Apple Pie CODE and returns no value."
  (declare (type string code))
  (initialize-source code)
  (interpreter-interpret
    (make-interpreter
      (parse-program)))
  (values))

;;; -------------------------------------------------------

(defun read-file-content (source)
  "Reads the SOURCE's content and returns it as a simple string."
  (declare (type source source))
  (with-open-file (input source
                   :direction         :input
                   :element-type      'character
                   :if-does-not-exist :error)
    (declare (type file-stream input))
    (let ((content (make-array
                     (file-length input)
                     :element-type 'character
                     :adjustable   NIL
                     :fill-pointer NIL)))
      (declare (type simple-string content))
      (read-sequence content input)
      (the simple-string content))))

;;; -------------------------------------------------------

(defun load-Apple-Pie-file (source)
  "Loads the Apple Pie code from the SOURCE, interprets it, and returns
   no value."
  (declare (type source source))
  (interpret-Apple-Pie
    (read-file-content source))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the number 1, as it precedes the digit 2.
(interpret-Apple-Pie
  "Good luck reading this lol uA2 J!!!")

;;; -------------------------------------------------------

;; Calculate the power 2^3 (= 8) and print it to the standard output.
(interpret-Apple-Pie
  "Good luck reading this lol uDxDF2F^F3 MA$xF J!!!")

;;; -------------------------------------------------------

;; Print "Ha".
(interpret-Apple-Pie
  "Good luck reading this lol uAI JAb J!!!")

;;; -------------------------------------------------------

;; Print "HaHaHa" using a loop.
(interpret-Apple-Pie
  "Good luck reading this lol uEepbeepQ10 ZAI JAb JC L!!!")

;;; -------------------------------------------------------

;; Print the lyrics to the song "99 Bottles of Beer" using the HQ9+
;; interpreter.
(interpret-Apple-Pie
  "Good luck reading this lol uG9 P!!!")

;;; -------------------------------------------------------

;; Demonstrate the use of a comment, introduced with "B" and terminated
;; with a "K".
(interpret-Apple-Pie
  "Good luck reading this lol uBThisCommentIsIgnored K!!!")

;;; -------------------------------------------------------

;; Define a variable "v" with the numeric value 123, unset it, and print
;; it, returning "K", as an undefined variable value's query assumes
;; this letter as a default.
(interpret-Apple-Pie
  "Good luck reading this lol uDvD123 MH44v QA$vF J!!!")

;;; -------------------------------------------------------

;; Define a variable "v" with the numeric value 123, unset it, and print
;; it, returning "K", as an undefined variable value's query assumes
;; this letter as a default. Please note the extensive usance of
;; comments preceding each effective command invocation.
(interpret-Apple-Pie
  "Good luck reading this lol uBSet_the_variable_v_to_123 KDvD123 MBNow_unset_v KH44v QBThe_default_of_a_missing_variable,_which_is_the_letter_K,_will_be_returned,_but_replaced_by_its_precedessor,_L,_in_the_alphabet KA$vF J!!!")

;;; -------------------------------------------------------

;; Print "HelloWorld".
(interpret-Apple-Pie "Good luck reading this lol uAI JAf JAm JAm JAp JAX JAp JAs JAm JAe J!!!")

;;; -------------------------------------------------------

;; Print "hello, world" by the HQ9+ interpreter's mediation.
(interpret-Apple-Pie "Good luck reading this lol uGH P!!!")

;;; -------------------------------------------------------

;; Quine using the newline command.
(interpret-Apple-Pie "Good luck reading this lol u
!!!")

;;; -------------------------------------------------------

;; Quine using the HQ9+ interpreter.
(interpret-Apple-Pie "Good luck reading this lol uGQ P!!!")

;;; -------------------------------------------------------

;; Print "HelloWorld" three times.
(interpret-Apple-Pie "Good luck reading this lol uEepbeepQ10 ZAI JAf JAm JAm JAp JAX JAp JAs JAm JAe JC L!!!")

;;; -------------------------------------------------------

;; Define a variable "X" with the value
;;   X = 5 + 5
;;     = 10
;; and print it, thus displaying "01", as the output operation reverses
;; a variable's content.
(interpret-Apple-Pie "Good luck reading this lol uDXDF5F+F5 MA$XF J!!!")

;;; -------------------------------------------------------

;; Print the numbers from 0 to 10.
(interpret-Apple-Pie "Good luck reading this lol uDXD1 MEepbeepQ100 ZA$XF JDXDF$XFF+F1 MC LA2 JA1 J!!!")

;;; -------------------------------------------------------

;; Define a variable "X" with the value zero (0), increment it by
;; one (1), and print the new content.
(interpret-Apple-Pie "Good luck reading this lol uDXD0 MDXDF$XFF+F1 MA$XF J!!!")
