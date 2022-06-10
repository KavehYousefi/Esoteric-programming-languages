;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Asig", invented by the Esolang user "Adam" on March 30,
;; 2014.
;; 
;; Concept
;; =======
;; The Asig programming language accomplishes its kenspeckle nature by
;; by two aspects of its haecceity, the syntactical component of the
;; same relating to its choice of symbols for the command designations;
;; the functional other manifesting in the exclusivity of variables for
;; the arithmetic, string, and input/output operations.
;; 
;; == ASIG PROGRAMS OPERATE ON VARIABLES ==
;; Programs in this language consist of particular symbol combinations
;; for the commands' diorisms, operating in a paravaunt manner upon
;; numeric and string variables.
;; 
;; == VARIABLES ==
;; Two kinds of variables are accommodated introduction into utility:
;; numbers and strings. A twofold application of regulation determines
;; their identifying features, the first being a nominal stringency,
;; reserving only Latin letters, digits, and the underscore character
;; as a name's constituents, with the incipient position restricted to
;; the premier set among the treble. A second constriction promulgates
;; the requisite of any variable identifier, regardless of its type, to
;; be a unique entity. In concrete diction, the following legislation
;; dominates:
;;   
;;   (1) A numeric and a string variable must not share the same name.
;;   (2) The name of a variable of a specific type must not be reused to
;;       create a variable of the same type.
;;   (3) Each variable must have been declared ere its usance.
;; 
;; == NUMERIC VARIABLES ==
;; The urgency for numerically typed variables does not germinate,
;; overthwart to the circumstances alluding to strings, from the lack of
;; literal numbers in currency, but instead defines a corollary issuing
;; from several operations' requirements. Numeric variables partake of
;; arithmetic operations as operands and recipient of the result.
;; 
;; == STRING VARIABLES: THE ONLY WAY TO USE STRINGS ==
;; A contingently vacuous character sequence's preservation inside of a
;; string already exhausts the means of string statements in the
;; language, bifurcated into two instances during a program's execution:
;; Any such variable must be initialized with the string addition
;; command, at which point its value is determined. If not subjected to
;; an equivalent treatment by reuse in this operation type, the second
;; method reserved for a new assignment involves the user input command,
;; the stipulation of the same requires the placeholder's prior
;; existency, as counterdistinguished to the opposite polarity commorant
;; in the previous instruction.
;; 
;; == COMMANDS ARE DESIGNATED BY SYMBOL COMBINATIONS ==
;; The most patent peculiarity applicable to Asig's haecceity occupies
;; the compass of its command syntax. While the recognition of
;; instructions depends upon their introduction via the prefix "~{" and
;; and conclusion by a terminating right brace "}", the operations
;; themselves are distinguished by a dedicated symbol or a combination
;; thereof.
;; 
;; 
;; Architecture
;; ============
;; No particular mold donates to the Asig programming language's
;; structure --- merely the variable registry as an entity respondent to
;; the placeholders' access exercises some mete of architectural
;; semblance.
;; 
;; The variable registry, apart from its amenability to a variable's
;; address by its name, embraces further requisite information,
;; including, of course, its value for both indagation and manipulation,
;; but additionally the data type --- numeric or string ---, for
;; purposes of validity checks. No reificiation anenst the actual
;; data structure design experiences a vouchsafement according to the
;; language specification, other than the aforementioned facilities.
;; 
;; 
;; Data Types
;; ==========
;; The Asig type system is enumerated by the member twain of real
;; numbers and strings.
;; 
;; == NUMBERS: SIGNED REAL VALUES OF ANY MAGNITUDE ==
;; Determined in the application as literals as well as variables, the
;; numbers' expressions are delineated by the assumption as real values,
;; comprehending integers and floating-point data in signed guise. No
;; natural confinement is imposed upon such value's magnitude, maugre
;; the reservation of bourns based on particular implementations of
;; the language.
;; 
;; == STRINGS: POTENTIALLY VACANT CHARACTER SEQUENCES ==
;; Strings partake of a program in a paravaunt variable form, albeit
;; such an object's initialization perforce germinates with a character
;; sequence literal's assignment. Strings may be empty or endowed with
;; any desidered length.
;; 
;; 
;; Syntax
;; ======
;; The language's signum embraces a set of symbols allotted as
;; constituents in variegated syntactical compositions for the sake of
;; creating, modifying, and printing variables.
;; 
;; == INSTRUCTIONS ==
;; Any command's distinguishment founds upon a particular symbol
;; combination, which please consult in the "Instructions" section.
;; 
;; == CHARACTER SET ==
;; The language's participants in the operational units are compact of
;; the following characters:
;;   
;;   Character | Name              | Apostille
;;   ----------+-------------------+-----------------------------------
;;    `        | grave             | Not used.
;;   ..................................................................
;;    ~        | tilde             | Starts a command.
;;   ..................................................................
;;    !        | exclamation mark  | Not used.
;;   ..................................................................
;;    @        | at sign           | Designates strings and textual
;;             |                   | input.
;;   ..................................................................
;;    #        | hash sign         | Used to separate a numeric
;;             |                   | variable from an arithmetic
;;             |                   | operand; also designates numeric
;;             |                   | input.
;;   ..................................................................
;;    $        | dollar sign       | Not used.
;;   ..................................................................
;;    %        | percentage sign   | Designates a numeric variable.
;;   ..................................................................
;;    ^        | circumflex        | Starts both an input command and
;;             |                   | a conditional statement.
;;   ..................................................................
;;    &        | ampersand         | Designates an arithmetic
;;             |                   | operation.
;;   ..................................................................
;;    +        | plus              | Designates the addition operation.
;;   ..................................................................
;;    -        | hyphen            | Designates the subtraction
;;             |                   | operation.
;;   ..................................................................
;;    *        | asterisk          | Designates the multiplication
;;             |                   | operation.
;;   ..................................................................
;;    /        | slash             | Designates the division operation.
;;   ..................................................................
;;    (        | left parenthesis  | Starts the input type designator.
;;   ..................................................................
;;    )        | right parenthesis | Terminates the input type
;;             |                   | designator.
;;   ..................................................................
;;    _        | underscore        | Used for variable names.
;;   ..................................................................
;;    [        | left bracket      | Starts a string literal.
;;   ..................................................................
;;    ]        | right bracket     | Terminates a string literal.
;;   ..................................................................
;;    {        | left brace        | Used in starting a command.
;;   ..................................................................
;;    }        | right brace       | Terminates a command.
;;   ..................................................................
;;    ;        | semicolon         | Not used.
;;   ..................................................................
;;    :        | colon             | Used in designating an arithmetic
;;             |                   | operation.
;;   ..................................................................
;;    '        | apostrophe        | Not used.
;;   ..................................................................
;;    "        | double quote      | Not used.
;;   ..................................................................
;;    ,        | comma             | Not used.
;;   ..................................................................
;;    .        | dot               | Not used.
;;   ..................................................................
;;    =        | equals sign       | Used in conditional testing.
;;   ..................................................................
;;    <        | less than sign    | Used in conditional testing.
;;   ..................................................................
;;    >        | greater than sign | Used in conditional testing.
;;   ..................................................................
;;    \        | backslash         | Introduces and separates "then"
;;             |                   | parts of a conditional statement.
;;   ..................................................................
;;    |        | vertical bar      | Used in printing a variable.
;;   ..................................................................
;;    ?        | question mark     | Not used.
;; 
;; == WHITESPACES ARE IGNORED ==
;; The presence and distribution of whitespaces, including in this
;; perimeter spaces, tabs, and newlines, does not carry any
;; significance, expect for their occurrence inside of strings as
;; literal constituents and around token names as a hint to their
;; boundaries.
;; 
;; == NO PROVISIONS FOR COMMENTS ==
;; Provisions for comments are not accommodated in the current iteration
;; of the Asig programming language.
;; 
;; == GRAMMAR ==
;; The language's amenability in syntactical matters resolves to an
;; expression in the following Extended Backus-Naur Form (EBNF):
;;   
;;   program                 := { command } ;
;;   command                 := "~" , "{", commandBody , "}" ;
;;   commandBody             := numericVariableCreation
;;                           |  mathematicalOperation
;;                           |  stringVariableCreation
;;                           |  print
;;                           |  input
;;                           |  test ;
;;   
;;   numericVariableCreation := numericVariable ;
;;   mathematicalOperation   := "&" , ":" , numericVariable , "#",
;;                              ( "+" | "-" | "*" | "/" ) ,
;;                              ( number | numericVariable ) ;
;;   stringVariableCreation  := stringVariable , string ;
;;   print                   := "|" , anyVariable ;
;;   input                   := "^" , "(" , ( "#" | "@" ) , ")" ,
;;                              anyVariable ;
;;   test                    := testPart , { conditionalPart } ;
;;   testPart                := "^" , ":" ,
;;                              anyVariable ,
;;                              ( "=" | ">" | "<" ) ,
;;                              ( number | anyVariable ) ;
;;   conditionalPart         := "\" , command ;
;;   
;;   anyVariable             := numericVariable | stringVariable ;
;;   numericVariable         := "%" , identifier ;
;;   stringVariable          := "@" , identifier ;
;;   
;;   number                  := [ "+" | "-" ] , digit, { digit } ,
;;                              [ "." , { digit } ] ;
;;   string                  := "[" , { character } , "]" ;
;;   identifier              := letter ,
;;                              { letter | digit | underscore } ;
;;   
;;   underscore              := "_" ;
;;   letter                  := "a" | ... | "z" | "A" | ... | "Z" ;
;;   digit                   := "0" | "1" | "2" | "3" | "4"
;;                           |  "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Instructions
;; ============
;; The language's capacitation enumerates basic arithmetic, conditional,
;; and input/output operations.
;; 
;; == OVERVIEW ==
;; An apercu over the instructions shall be bekent in tabular form, ere
;; a section of their own attends to each member's appreciation:
;;   
;;   Command                   | Effect
;;   --------------------------+---------------------------------------
;;    %varName                 | Creates a new numeric variable with
;;                             | the name {varName} and an initial
;;                             | value of zero.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    @varName [text]          | Creates a new string variable with
;;                             | the name {varName} and the initial
;;                             | text content {text}.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    &:%varName#op right      | Sets the value of the numeric variable
;;                             | with the name {varName} to the result
;;                             | of applying it as the left operand
;;                             | in the operation {op}, using the
;;                             | number or numeric variable {right} as
;;                             | the right operand.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    |%varName                | Prints the value of the numeric
;;                             | variable with the name {varName} to
;;                             | the standard output.
;;   ..................................................................
;;    |@varName                | Prints the value of the string
;;                             | variable with the name {varName} to
;;                             | the standard output.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    ^(#)%varName             | Queries the user for a number and
;;                             | stores it in the numeric variable with
;;                             | the name {varName}.
;;   ..................................................................
;;    ^(@)@varName             | Queries the user for a string and
;;                             | stores it in the string variable with
;;                             | the name {varName}.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    ^:%varName comp test     | Checks whether the value of the
;;      \commands              | numeric variable with the {varName}
;;                             | satisfies the {comp} predicate when
;;                             | juxtaposed to the {test} expression,
;;                             | which might be a number or the name of
;;                             | another numeric variable, executing
;;                             | on confirmation the sequence of zero
;;                             | or more {commands}, otherwise
;;                             | desisting from further operations.
;;   ..................................................................
;;    ^:@varName comp test     | Checks whether the value of the
;;      \commands              | numeric variable with the {varName}
;;                             | satisfies the {comp} predicate when
;;                             | juxtaposed to the {test} expression,
;;                             | which must be the name of another
;;                             | string variable, executing on
;;                             | confirmation the sequence of zero or
;;                             | more {commands}, otherwise desisting
;;                             | from further operations.
;; 
;; == CREATE NUMERIC VARIABLE ==
;; Creates a numeric variable identified by the specified name and
;; initialized to zero (0).
;; 
;; Signature:
;;   %variableName
;;    ************
;; 
;; Interface:
;;   createNumericVariable (variableName : variable) : void
;; 
;; Description:
;;   Creates a numeric variable with the {variableName}, initialized to
;;   the value zero (0).
;; 
;; Side effects:
;;   - A new numeric variable associated with the {variableName} is
;;     created and registered.
;; 
;; Exceptional situations:
;;   - An error of the type "DuplicateVariable" is thrown if a variable,
;;     numeric or textual, with the {variableName} already exists.
;; 
;; == CREATE STRING VARIABLE ==
;; Creates a string variable identified by the specified name.
;; 
;; Signature:
;;   @variableName [text]
;;    ************  ****
;; 
;; Interface:
;;   createStringVariable (variableName : variable,
;;                         text         : string) : void
;; 
;; Description:
;;   Creates a string variable with the {variableName}, initialized with
;;   the content {text}.
;;   
;;   The {text} must a sequence of zero or more characters
;;   differentiated from the right bracket "]", which terminates the
;;   textual section.
;; 
;; Side effects:
;;   - A new string variable associated with the {variableName} and
;;     containing the {text} is created and registered.
;; 
;; Exceptional situations:
;;   - An error of the type "DuplicateVariable" is thrown if a variable,
;;     numeric or textual, with the {variableName} already exists.
;; 
;; == MATHEMATICAL OPERATIONS ==
;; Modifies a numeric variable's value using an arithmetic operation.
;; 
;; Signature:
;;   &:%leftOperand # operator rightOperand
;;      ***********   ******** ************
;; 
;; Interface:
;;   mathematicalOperation (leftOperand : variable,
;;                          operator       : operator,
;;                          rightOperand   : number or variable) : void
;; 
;; Description:
;;   Applies the {operator} using the {leftOperand} variable as the left
;;   operand and the {rightOperand} as the right operand, storing the
;;   result back into the {leftOperand}.
;;   
;;   Four options exist regarding the {operator}:
;;     
;;     Operator | Description
;;     ---------+------------------------------------------------------
;;      +       | Addition.
;;     ................................................................
;;      -       | Subtract.
;;     ................................................................
;;      *       | Multiplication.
;;     ................................................................
;;      /       | Division.
;; 
;; Side effects:
;;   - The variable amenable to the identifier {leftOperand} will be
;;     modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariable" is thrown if no numeric
;;     variable amenable to the identifier {leftOperand} exists.
;;   - An error of the type "IllegalArgument" is thrown if the
;;     {operator} does not designate a valid arithmetic operator.
;;   - An error of the type "UndefinedVariable" is thrown if the
;;     {rightOperand} constitutes a variable name and no such object
;;     amenable to the identifier {rightOperand} exists.
;; 
;; == PRINT ==
;; Prints the content of a numeric or string variable to the standard
;; output.
;; 
;; Syntax:
;;   | %variableName
;;      ************
;;   | @variableName
;;      ************
;; 
;; Interface:
;;   print (variableName : variable) : void
;; 
;; Description:
;;   Prints the content of the variable designated by the {variableName}
;;   to the standard output.
;; 
;; Side effects:
;;   - The standard output conduit is modified by a print operation.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariable" is thrown if no variable
;;     amenable to the identifier {variableName} exists.
;;   - An error of an implementation-dependend type may be thrown if
;;     the output conduit in any way fails to react to the print
;;     request.
;; 
;; == INPUT ==
;; Queries the user for a numeric or textual input and stores the same
;; in a variable.
;; 
;; Syntax:
;;   ^(inputType) %variableName
;;     *********   ************
;;   ^(inputType) @variableName
;;     *********   ************
;; 
;; Interface:
;;   input (inputType    : inputType,
;;          variableName : variable) : void
;; 
;; Description:
;;   Using the standard input, queries the user either for a number or a
;;   string, depending upon the {inputType}, and stores the input in
;;   the variable amenable to the {variableName}.
;;   
;;   The {inputType} determines the kind of object expected when
;;   prompting the user's input, with two options being defined:
;;     
;;     Input type | Description
;;     -----------+----------------------------------------------------
;;      #         | A numeric input, that is, a real-valued number, is
;;                | expected. Its destination, designated by the
;;                | {variableName}, must refer to a numeric variable.
;;     ................................................................
;;      @         | A string input, that is, a sequence of zero or more
;;                | characters, is expected. Its destination,
;;                | designated by the {variableName}, must refer to a
;;                | string variable.
;; 
;; Side effects:
;;   - Optionally, a prompt text is printed to the standard output.
;;   - A number or string is consumed from the standard input.
;;   - The value of the variable amenable to the identifier
;;     {variableName} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariable" is thrown if no variable
;;     amenable to the identifier {variableName} exists.
;;   - An error of the type "InvalidArgument" is thrown if the
;;     {inputType} assumes an invalid value, that is, it resolves
;;     neither to "#" nor to "@".
;;   - An error of the type "InvalidArgument" is thrown if the variable
;;     amenable to the identifier {variableName} is of a type
;;     incompatible with the {inputType}. Concretely, it holds:
;;     (a) If the {inputType} equals "#", the {variableName} must
;;         produce a variable of the numeric type.
;;     (b) If the {inputType} equals "@", the {variableName} must
;;         produce a variable of the string type.
;; 
;; == CONDITIONAL ==
;; Executes a series of statements if a variable satisfies a specified
;; predicate.
;; 
;; Syntax:
;;   ^: %variableName comparison testValue \ statement-1 \ ... \ statement-N
;;       ************ ********** *********   ***********         ***********
;;   ^: @variableName comparison testValue \ statement-1 \ ... \ statement-N
;;       ************ ********** *********   ***********         ***********
;; 
;; Interface:
;;   if (variableName : variable,
;;       comparison   : comparison,
;;       testValue    : number, string, or variable,
;;       statements   : statement[0..N]) : void
;; 
;; Description:
;;   Checks whether the variable identified by the {variableName}
;;   satisfies the relationship defined by the {comparison} operator and
;;   the {testValue}, executing on confirmation in the order of their
;;   occurrences the zero or more statements {statement-1} through
;;   {statement-N}.
;;   
;;   The {variableName} must designate any numeric or string variable.
;;   The {testValue} must be a number or a variable of any kind, whose
;;   value juxtaposes with the content of the variable designated by the
;;   {variableName}.
;;   
;;   Three valid options for the {comparison} operator exist:
;;     
;;     Operator | Description
;;     ---------+------------------------------------------------------
;;      =       | Is satisfied if the value of the variable with the
;;              | {variableName} is exactly equal to the {testValue}.
;;              | String comparison is performed in a case-sensitive
;;              | manner.
;;     ................................................................
;;      <       | Is satisfied if the value of the variable with the
;;              | {variableName} is strictly less than the {testValue}.
;;              | String comparison is performed in a case-sensitive
;;              | manner.
;;     ................................................................
;;      >       | Is satisfied if the value of the variable with the
;;              | {variableName} is strictly greater than the
;;              | {testValue}.
;;              | String comparison is performed in a case-sensitive
;;              | manner.
;;   
;;   The statements {statement-1} through {statement-N} must be a
;;   sequence of zero or more arbitrary commands, executed if the
;;   predicate applies.
;; 
;; Side effects:
;;   - If any of the conditional statements {statement-1} through
;;     {statement-N} are executed, their respective side effects apply.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariable" is thrown if no variable
;;     amenable to the identifier {variableName} exists.
;;   - An error of the type "InvalidArgument" is thrown if the
;;     {comparison} does not designate a valid relation.
;;   - An error of the type "UndefinedVariable" is thrown if the
;;     {testValue} constitutes a variable and no variable amenable to
;;     the identifier {testValue} exists.
;;   - If any of the conditional statements {statement-1} through
;;     {statement-N} are executed, any exceptional situation
;;     appertaining to their application propagates throughout this
;;     operation.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The curtailment commorant in the original Asig specification
;; reverberates in some frailties regarding detailed aspects of the
;; language. A selection thereof shall be enumerated below.
;; 
;; == ARE STRINGS VARIABLES OR CONSTANTS? ==
;; The original specification relates to "variables" merely in the sense
;; of numeric placeholders, barring from this nomenclature character
;; storages, and in particular distinguishing them from "strings" as
;; named entities pursuant in the maintenance of textual data. A
;; discrepancy from the numeric variables, with their amenability to
;; mathematical operations in order to manipulate the content, strings
;; lack the explicit apportionment of such contingency. It is
;; nevertheless possible to reach a conclusion from the input statement,
;; with its bifurcation into the reception of numeric and textual user
;; replies, that both storage entities partake of a reactive character.
;; It is thus adjudged in this documentation that two types of variables
;; reside in the language: those holding numbers and those for the
;; persistence of strings. The document will relate to "variables" only
;; in cases where both species concord into the same subject.
;; 
;; == HOW SHALL AN INPUT BE HANDLED? ==
;; Asig advances a statement intended to receive user input in the form
;; of a number or a string, the parcel following its obtention, however,
;; registers a debt in exposition. Three scenarios retain some mete of
;; credibility:
;;   
;;   (1) The user input is stored in an extant numeric or string
;;       variable.
;;   (2) The user input is stored in a numeric or string variable
;;       declared and initialized with dedication to this exclusive
;;       cause.
;;   (3) The user input may be approriated in any of the two instances
;;       involving a numeric or string variable creation:
;;         {%variableName ^(#)}
;;         {@variableName ^(@)}
;; 
;; Considerations applying to the third possibility are too far eloigned
;; from the explicated Asig specification, and the second case
;; betokening no adminiculum as an alternative to the standard variable
;; creation mechanisms, the first solution is presented in that an
;; already existing variable of a compatible ilk must be supplied to the
;; input facility.
;; 
;; == HOW SHALL THE CONDITIONAL STATEMENT BE USED? ==
;; The conditional statement's deployment, instigated by the syntax
;;   ^: variableName = testValue
;; does not promulgate its exactness in delineation into the section
;; regarding the test value, nor the statement or statements following
;; the positive testing's conclusion. Several questions emanate from the
;; ambivalence:
;;   
;;   (a) May only numeric variables or also strings be employed as test
;;       candidates?
;;   (b) Against which values may the test candidate be juxtaposed ---
;;       only numeric literals or variables of any kind?
;;   (c) May only one statement occupy the confirmation section or an
;;       arbitrary tally, spanning zero to theoretically infinitely
;;       many?
;;   (d) Shall the statement or statements following the test, thus
;;       succeeding the separating backslash ("\"), be ensconced in the
;;       command delineators "~{ ... }", or applied in a raw fashion?
;; 
;; The following adjudgments have been subjected to an establishment:
;;   
;;   (a) All types of variables are homologated as test candidates.
;;   (b) The value to be tested against may be any kind of variable or
;;       a literal number.
;;   (c) An arbitrary number of commands, including zero, may be stated
;;       following the test expression.
;;   (d) In order to accommodate a spatial reservoir for contingent
;;       syntax extensions, each command following the test expression
;;       must be ensconced in the marker sequence "~{...}".
;; 
;; 
;; Implementation
;; ==============
;; This implementation of the Asig programming language in Common Lisp
;; dedicates its investments to a thoroughly simplistic solution.
;; 
;; A treble graduation participates in the realization of a program:
;;   
;;   (1) A lexer generates tokens from a piece of Asig code.
;;   (2) A parser transforms these tokens into an abstract syntax tree.
;;   (3) An interpreter processes this tree and imbues it with effect.
;; 
;; == THE SOURCE CODE IS TOKENIZED BY A LEXER ==
;; In the course of the lexical analyzation, performed by a lexer's
;; effort, a piece of Asig source code is reformulated as a sequence of
;; tokens, each such a representation of a significant datum.
;; 
;; == THE TOKENS ARE TRANSFORMED INTO AN AST BY A PARSER ==
;; The token stream designates the objects of purveyance from the lexer
;; to the parser, the latter of which assays the correctness of the
;; inputs' order and combination, constructing in a positive reckoning
;; the abstract syntax tree (AST), a hierarchical structure that
;; describes the original Asig code in the form of a tree, composed of
;; nodes as the language construct's encapsulations.
;; 
;; == GENERIC NODES COMPRISE THE AST ==
;; Chosen as a plain solution, the AST components' representation is
;; accomplished by means of a generic node class, as
;; counterdistinguished from the more common specialized subtypes
;; admitted tenancy in usual implementations. As an emblem of the
;; represented language facility, the node type designates the concrete
;; species, whereas a collection of attributes supplies their
;; properties. These characteristics, intended to act in surrogacy for
;; subclass fields, are reified in a hash table's mold, the keys as
;; keyword symbols manifesting the attribute names, mapped to arbitrary
;; values.
;; 
;; A response to the curtailed stringency eloigned by eschewing a static
;; class hierarchy for the procrustean generic node solution, the
;; following associations betwixt Asig's language constructs and a
;; representing node's properties shall be supplied:
;;   
;;   ==================================================================
;;    Asig construct  | Literal number
;;   ..................................................................
;;    Node type       | :number
;;   ..................................................................
;;    Node attributes | value : real
;;   ==================================================================
;;    Asig construct  | Numeric variable
;;   ..................................................................
;;    Node type       | :identifier
;;   ..................................................................
;;    Node attributes | kind : keyword = ":number"
;;                    | name : string
;;   ==================================================================
;;    Asig construct  | String variable
;;   ..................................................................
;;    Node type       | :identifier
;;   ..................................................................
;;    Node attributes | kind : keyword = ":string"
;;                    | name : string
;;   ==================================================================
;;    Asig construct  | Demarcated string
;;   ..................................................................
;;    Node type       | :string
;;   ..................................................................
;;    Node attributes | content : string
;;   ==================================================================
;;    Asig construct  | Command for numeric variable creation
;;   ..................................................................
;;    Node type       | :numeric-variable-creation
;;   ..................................................................
;;    Node attributes | name : Node of type ":identifier"
;;   ==================================================================
;;    Asig construct  | Command for string variable creation
;;   ..................................................................
;;    Node type       | :string-variable-creation
;;   ..................................................................
;;    Node attributes | name : Node of type ":identifier"
;;                    | text : Node of type ":string"
;;   ==================================================================
;;    Asig construct  | Command for mathematical operation
;;   ..................................................................
;;    Node type       | :mathematical-operation
;;   ..................................................................
;;    Node attributes | operator      : keyword
;;                    | left-operand  : Node of type ":identifier"
;;                    | right-operand : Node of type ":number" or
;;                    |                              ":identifier"
;;   ==================================================================
;;    Asig construct  | Command for printing
;;   ..................................................................
;;    Node type       | :print
;;   ..................................................................
;;    Node attributes | argument : Node of type ":identifier"
;;   ==================================================================
;;    Asig construct  | Command for input
;;   ..................................................................
;;    Node type       | :input
;;   ..................................................................
;;    Node attributes | input-type : keyword
;;                    | variable   : Node of type ":number" or
;;                    |                           ":identifier"
;;   ==================================================================
;;    Asig construct  | Command for test
;;   ..................................................................
;;    Node type       | :test
;;   ..................................................................
;;    Node attributes | variable   : Node of type ":number" or
;;                    |                           ":identifier"
;;                    | comparison : keyword
;;                    | expression : Node of type ":number" or
;;                    |                            "identifier"
;;                    | statements : list of Node objects of any type
;;   ==================================================================
;; 
;; == THE INTERPRETER APPLIES EFFECT TO THE AST ==
;; The interpreter's share of the investments embraces the assignment of
;; effects to the AST's static nature. Two districts necessitate
;; particular attention:
;;   
;;   (a) The traversal and processing of the AST.
;;   (b) The interactions betwixt a program and its variables.
;; 
;; == (A) THE INTERPRETER AS A VISITOR DISPATCHING ON NODE TYPES ==
;; The most natural approach to the tree traversal problem is realized
;; in the "visitor" design pattern, an object-oriented solution which
;; mandates the responsible unit, known as the visitor, to dispatch on a
;; node and its child nodes in a recursive fashion while evaluating
;; these vertices.
;; 
;; The inquisition into the dispatch criterion constitutes a subject in
;; dependency upon the concrete programming language's capabilities. In
;; the case of Common Lisp, the furcation modes encompass, among others,
;; the identity of an object. By harnessing the node type inside of this
;; concept, the generic nodes may be visited using the node type slot.
;; 
;; The visitor in our context resolves to the ``Interpreter'', a class
;; ordained to this office by adminicle of two functions operating in
;; close conjunction:
;;   
;;   visit-node    (interpreter node)
;;   dispatch-node (interpreter node-type node)
;; 
;; The ``visit-node'' operation establishes the official visitor
;; interface member responsible for processing a node. Commorant in its
;; curt argument list, no presence of the discriminating node type can
;; be attested; thus, without appurtenant means, its implementation
;; would perforce be coerced into a monolithic conditional branching,
;; each such produce of this ramosity another node type's case. In
;; consectary, the ``dispatch-node'' generic function, or "method",
;; applies itself in the dispatching agency: With the ``interpreter''
;; and generic ``node'' instances being paregal, the variation
;; materializes in the additional ``node-type'' argument, which for
;; every type of node invokes another implementation of this operation,
;; for instance:
;;   
;;   (defmethod dispatch-node ((interpreter Interpreter)
;;                             (node-type   (eql :numeric-variable-creation))
;;                             (node        Node))
;;     ...)
;;   
;;   (defmethod dispatch-node ((interpreter Interpreter)
;;                             (node-type   (eql :mathematical-operation))
;;                             (node        Node))
;;     ...)
;; 
;; Whereas the interface operation ``visit-node'' simply invokes the
;; correct ``dispatch-node'' variant by induction of the pertinent
;; node type symbol, as demonstrated in its excerpt
;;   
;;   (defun visit-node (interpreter node)
;;     "Visits the NODE using the INTERPRETER by dispatching on the
;;      NODE's type."
;;     (declare (type Interpreter interpreter))
;;     (declare (type Node        node))
;;     (dispatch-node interpreter (node-type node) node))
;; 
;; the main effort is redirected into the implementation of the
;; ``dispatch-node'' functions.
;; 
;; == (B) VARIABLES ARE STORED IN A HASH TABLE ==
;; In their agency as an Asig program's main tokens in currency,
;; variables appropriate a fair share of this unit's cynosure.
;; 
;; The variables' castaldy describes an onus consigned to the efforts of
;; a hash table, the variable names of which are assigned the role of
;; the key, associated to the placeholder object itself. Founded upon
;; an identifier's uniqueness in both realms, numbers and strings, their
;; conflation into a singular repository does not impose an adscititious
;; constraint.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-06
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Asig"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of package.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a package in order to shadow the symbol "variable" reserved by
;; the Common Lisp implementation "Steel Bank Common Lisp" (SBCL).
(defpackage :asig
  (:use :cl)
  (:shadow #:variable))

(in-package :asig)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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

(deftype property-list-of (&optional (key-type T) (value-type T))
  "The ``property-list-of'' type defines a property list as a flat list
   of zero or more elements, the tally of which must be even, with the
   elements at even indices constituting a key of the KEY-TYPE,
   associated to the immediately succeeding value as its value, being of
   the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (evenp (the (integer 0 *) (length (the list object))))
            (loop
              for (key value)
                of-type (T T)
                on      (the list object)
                by      #'cddr
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which associates with the KEY-TYPE, mapped to a
   value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   entailing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype arithmetic-operation ()
  "The ``arithmetic-operation'' type enumerates the recognized
   arithmetic operations."
  '(member :addition :subtraction :multiplication :division))

;;; -------------------------------------------------------

(deftype comparison ()
  "The ``comparison'' type enumerates the recognized comparison
   relations."
  '(member :equal :less-than :greater-than))

;;; -------------------------------------------------------

(deftype input-type ()
  "The ``input-type'' type enumerates the recognized user input
   formats."
  '(member :number :text))

;;; -------------------------------------------------------

(deftype variable-type ()
  "The ``variable-type'' type enumerates the recognized types of
   variables."
  '(member :number :string))

;;; -------------------------------------------------------

(deftype variable-value ()
  "The ``variable-value'' type enumerates the possible values a variable
   might assume."
  '(or real string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type fragment value)))
  "The ``Token'' class avails in the encapsulation of a significant
   portion in a piece of Asig source code."
  (type     (error "Missing token type.") :type keyword)
  (fragment NIL                           :type (or null string))
  (value    NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN is of the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identifier-character-p (character)
  "Checks whether the CHARACTER represents an identifier character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (alpha-char-p character)
          (digit-char-p character)
          (char=        character #\_))))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Asig code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE. If ``NIL'' the analyzation has been
                    finalized."))
  (:documentation
    "The ``Lexer'' class serves in the partitioning of an Asig program
     into its tokens."))

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
  "Creates and returns a new ``Lexer'' which operates on the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character in its source, if possible, and
   returns the modified LEXER."
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

(defun lexer-read-spaces (lexer)
  "Starting the current LEXER position, reads a sequence of one or more
   spaces and returns a ``Token'' representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((spaces ""))
      (declare (type string spaces))
      (setf spaces
        (with-output-to-string (output)
          (declare (type string-stream output))
          (loop while (and character (char= character #\Space)) do
            (write-char character output)
            (lexer-advance lexer))))
      (the Token (make-token :spaces spaces spaces)))))

;;; -------------------------------------------------------

(defun lexer-read-character (lexer token-type)
  "Consumes the next character in the LEXER's source and returns a
   ``Token'' representation, associating the TOKEN-TYPE with the just
   consumed character in its string form."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (prog1
        (make-token token-type (string character) character)
        (lexer-advance lexer)))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current LEXER position, reads a number and returns a
   ``Token'' representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((digits ""))
      (declare (type string digits))
      (setf digits
        (with-output-to-string (output)
          (declare (type string-stream output))
          ;; Read optional sign.
          (when (and character (find character "+-" :test #'char=))
            (write-char character output)
            (lexer-advance lexer))
          ;; Read the integer part.
          (loop while (and character (digit-char-p character)) do
            (write-char character output)
            (lexer-advance lexer))
          ;; Does a fractional part follow?
          (when (and character (char= character #\.))
            (write-char character output)
            (lexer-advance lexer)
            (loop while (and character (digit-char-p character)) do
              (write-char character output)
              (lexer-advance lexer)))))
      (the Token
        (make-token :number digits (read-from-string digits))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current LEXER position, reads an identifier and
   returns a ``Token'' representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((identifier ""))
      (declare (type string identifier))
      (setf identifier
        (with-output-to-string (output)
          (declare (type string-stream output))
          (loop
            while (and character (identifier-character-p character))
            do
              (write-char character output)
              (lexer-advance lexer))))
      (the Token (make-token :identifier identifier identifier)))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER return on each invocation
   a new token of the type ``:eof'', being a signification of the code's
   termination."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ;; End of code.
        ((null character)
          (make-token :eof NIL NIL))
        
        ((char= character #\Space)
          (lexer-read-spaces lexer))
        
        ((char= character #\Newline)
          (lexer-read-character lexer :newline))
        
        ((char= character #\~)
          (lexer-read-character lexer :tilde))
        
        ((char= character #\{)
          (lexer-read-character lexer :left-brace))
        
        ((char= character #\})
          (lexer-read-character lexer :right-brace))
        
        ((char= character #\()
          (lexer-read-character lexer :left-parenthesis))
        
        ((char= character #\))
          (lexer-read-character lexer :right-parenthesis))
        
        ((char= character #\[)
          (lexer-read-character lexer :left-bracket))
        
        ((char= character #\])
          (lexer-read-character lexer :right-bracket))
        
        ((char= character #\%)
          (lexer-read-character lexer :percentage))
        
        ((char= character #\&)
          (lexer-read-character lexer :ampersand))
        
        ((char= character #\:)
          (lexer-read-character lexer :colon))
        
        ((char= character #\#)
          (lexer-read-character lexer :hash))
        
        ((char= character #\+)
          (lexer-read-character lexer :plus))
        
        ((char= character #\-)
          (lexer-read-character lexer :minus))
        
        ((char= character #\*)
          (lexer-read-character lexer :asterisk))
        
        ((char= character #\/)
          (lexer-read-character lexer :slash))
        
        ((char= character #\\)
          (lexer-read-character lexer :backslash))
        
        ((char= character #\^)
          (lexer-read-character lexer :caret))
        
        ((char= character #\@)
          (lexer-read-character lexer :at-sign))
        
        ((char= character #\|)
          (lexer-read-character lexer :vertical-bar))
        
        ((char= character #\?)
          (lexer-read-character lexer :question-mark))
        
        ((char= character #\=)
          (lexer-read-character lexer :equal))
        
        ((char= character #\<)
          (lexer-read-character lexer :less-than))
        
        ((char= character #\>)
          (lexer-read-character lexer :greater-than))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        (T
          (lexer-read-character lexer :character))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      (error "Missing node type.")
    :type          keyword
    :documentation "The identifying node type.")
   (attributes
    :initarg       :attributes
    :initform      (make-hash-table :test #'eq)
    :type          (hash-table-of keyword T)
    :documentation "A mapping of attribute names to values."))
  (:documentation
    "The ``Node'' class provides a generic abstract syntax tree (AST)
     component, utible for storing the Asig language's represented
     facility in conjunction with the pertinent characteristics."))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'', optionally initialized to store
   the INITIAL-ATTRIBUTES, supplied as a flat sequence of attribute
   name-value pairs."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) initial-attributes))
  (let ((node (make-instance 'Node :type type)))
    (declare (type Node node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (slot-value node 'attributes))
              attribute-value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-type (node)
  "Returns the NODE type."
  (declare (type Node node))
  (the keyword (slot-value node 'type)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the NODE attribute value associated with the ATTRIBUTE-NAME,
   or ``NIL'' if none such exists."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (the T (gethash attribute-name (slot-value node 'attributes))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (new-value node attribute-name)
  "Modifies the NODE attribute associated with the ATTRIBUTE-NAME to
   the NEW-VALUE, and returns the modified NODE."
  (declare (type T       new-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (slot-value node 'attributes))
        new-value)
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "Node(type=~s, attributes=[" (slot-value node 'type))
  (loop
    for attribute-name
      of-type keyword
      being the hash-keys in (slot-value node 'attributes)
    using
      (hash-value attribute-value)
    for first-attribute-p
      of-type boolean
      =       T
      then    NIL
    do
      (unless first-attribute-p 
        (format stream ", "))
      (format stream "~a=~s" attribute-name attribute-value))
  (format stream "])"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The lexer whose tokens shall be assembled into an
                    abstract syntax tree (AST).")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL NIL)
    :type          Token
    :documentation "The most recent token obtained from the LEXER."))
  (:documentation
    "The ``Parser'' class represents an entity responsible for the
     assemblage of a sequence of tokens into an abstract syntax tree
     (AST)."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which obtains its tokens from
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token is of the
   EXPECTED-TOKEN-TYPE, on confirmation loading and storing the next
   token from the PARSER's internal lexer, otherwise signaling an
   error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (if (token-type-p current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-spaces (parser)
  "Parses zero or more space tokens obtained by the PARSER, and returns
   the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop while (token-type-p current-token :spaces) do
      (parser-eat parser :spaces)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-newlines (parser)
  "Parses zero or more newline tokens obtained by the PARSER, and
   returns the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop while (token-type-p current-token :newline) do
      (parser-eat parser :newline)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-whitespaces (parser)
  "Parses zero or more space or newline tokens obtained by the PARSER,
   and returns the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop
      while (or (token-type-p current-token :newline)
                (token-type-p current-token :spaces))
      do    (parser-eat parser (token-type current-token))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-numeric-variable (parser)
  "Parses a numeric variable using the PARSER and returns a token
   representation thereof.
   ---
   The modeled grammar is:
     numericVariable := '%' , identifier ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-spaces parser)
    (parser-eat parser :percentage)
    (parser-skip-spaces parser)
    (let ((identifier current-token))
      (declare (type Token identifier))
      (parser-eat parser :identifier)
      (the Node
        (make-node
          :identifier
          :kind :number
          :name (token-value identifier))))))

;;; -------------------------------------------------------

(defun parser-parse-string-variable (parser)
  "Parses a string variable using the PARSER and returns a token
   representation thereof.
   ---
   The modeled grammar is:
     stringVariable := '@' , identifier ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-spaces parser)
    (parser-eat parser :at-sign)
    (parser-skip-spaces parser)
    (let ((identifier current-token))
      (declare (type Token identifier))
      (parser-eat parser :identifier)
      (the Node
        (make-node
          :identifier
          :kind :string
          :name (token-value identifier))))))

;;; -------------------------------------------------------

(defun parser-parse-any-variable (parser)
  "Parses a numeric or string variable using the PARSER and returns a
   token representation thereof.
   ---
   The modeled grammar is:
     anyVariable := numericVariable | stringVariable ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-spaces parser)
    (the Node
      (case (token-type current-token)
        (:percentage
          (parser-parse-numeric-variable parser))
        (:at-sign
          (parser-parse-string-variable parser))
        (otherwise
          (error "Invalid variable marker: ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-string (parser)
  "Parses a string ensconced in a jumelle of brackets '[' and ']', and
   returns a token representation thereof.
   ---
   The modeled grammar is:
     string := '[', { character } , ']' ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :left-bracket)
    (the Node
      (make-node :string
        :content
          (with-output-to-string (content)
            (declare (type string-stream content))
            (loop do
              (case (token-type current-token)
                (:eof
                  (error "Unterminated string literal."))
                (:right-bracket
                  (parser-eat parser :right-bracket)
                  (parser-skip-spaces parser)
                  (loop-finish))
                (otherwise
                  (format content "~a"
                    (token-fragment current-token))
                  (parser-eat parser (token-type current-token))))))))))

;;; -------------------------------------------------------

(defun parser-parse-number (parser)
  "Parses a number and returns a token representation thereof.
   ---
   The modeled grammar is:
     number := [ '+' | '-' ] , digit, { digit } , [ '.' , { digit } ] ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((number (token-value current-token)))
      (declare (type real number))
      (parser-eat parser :number)
      (the Node (make-node :number :value number)))))

;;; -------------------------------------------------------

(defun parser-parse-numeric-variable-creation (parser)
  "Parses a numeric variable creation command and returns a node
   representation thereof.
   ---
   The modeled grammar is:
     numericVariableCreation := numericVariable ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-spaces parser)
    (the Node
      (make-node
        :numeric-variable-creation
        :name (parser-parse-numeric-variable parser)))))

;;; -------------------------------------------------------

;; stringAddition := "@" , identifier , string ;
;; 
(defun parser-parse-string-variable-creation (parser)
  "Parses a string variable creation and returns a node representation
   thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-spaces parser)
    (let ((identifier (parser-parse-string-variable parser))
          (text       NIL))
      (declare (type Node           identifier))
      (declare (type (or null Node) text))
      (parser-skip-spaces parser)
      (setf text (parser-parse-string parser))
      (parser-skip-spaces parser)
      (the Node
        (make-node
          :string-variable-creation
          :name identifier
          :text text)))))

;;; -------------------------------------------------------

(defun parser-parse-mathematical-operation (parser)
  "Parses a mathematical operation and returns a node representation
   thereof.
   ---
   The modeled grammar is:
     mathematicalOperation := '&' , ':' , numericVariable , '#',
                              ( '+' | '-' | '*' | '/' ) ,
                              ( number | numericVariable ) ;"
  (declare (type Parser parser))
  
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (parser-skip-spaces parser)
    (parser-eat parser :ampersand)
    (parser-skip-spaces parser)
    (parser-eat parser :colon)
    (parser-skip-spaces parser)
    
    (let ((identifier    (parser-parse-numeric-variable parser))
          (operator      NIL)
          (right-operand NIL))
      (declare (type Node                           identifier))
      (declare (type (or null arithmetic-operation) operator))
      (declare (type (or null Node)                 right-operand))
      
      (parser-skip-spaces parser)
      (parser-eat         parser :hash)
      (parser-skip-spaces parser)
      
      (setf operator
        (case (token-type current-token)
          (:plus     :addition)
          (:minus    :subtraction)
          (:asterisk :multiplication)
          (:slash    :division)
          (otherwise (error "Invalid operator token: ~s."
                       current-token))))
      
      (parser-eat         parser (token-type current-token))
      (parser-skip-spaces parser)
      
      (setf right-operand
        (case (token-type current-token)
          (:number
            (parser-parse-number parser))
          (:percentage
            (parser-parse-numeric-variable parser))
          (otherwise
            (error "Invalid right operand token: ~s."
              current-token))))
      
      (the Node
        (make-node :mathematical-operation
          :operator      operator
          :left-operand  identifier
          :right-operand right-operand)))))

;;; -------------------------------------------------------

(defun parser-parse-print (parser)
  "Parses a print command body and returns a node representation
   thereof.
   ---
   The modeled grammar is:
     print := '|' , anyVariable ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-spaces parser)
    (parser-eat parser :vertical-bar)
    (parser-skip-spaces parser)
    (let ((argument NIL))
      (declare (type (or null Node) argument))
      (case (token-type current-token)
        ((:percentage :at-sign)
          (setf argument (parser-parse-any-variable parser)))
        (otherwise
          (error "Invalid print argument token: ~s." current-token)))
      (parser-skip-spaces parser)
      (the Node
        (make-node
          :print
          :argument argument)))))

;;; -------------------------------------------------------

(defun parser-parse-input (parser)
  "Parses an input command body and returns a node representation
   thereof.
   ---
   The modeled grammar:
     input := '^' , '(' , ( '#' | '@' ) , ')' , anyVariable ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    ;(parser-eat parser :caret)
    ;(parser-skip-spaces parser)
    (parser-eat parser :left-parenthesis)
    (parser-skip-spaces parser)
    (let ((input-type NIL)
          (variable   NIL))
      (declare (type (or null input-type) input-type))
      (declare (type (or null Node)       variable))
      (setf input-type
        (case (token-type current-token)
          (:hash
            (prog1 :number
              (parser-eat parser :hash)))
          (:at-sign
            (prog1 :text
              (parser-eat parser :at-sign)))
          (otherwise
            (error "Invalid input type token: ~s." current-token))))
      (parser-skip-spaces parser)
      (parser-eat parser :right-parenthesis)
      (parser-skip-spaces parser)
      (setf variable (parser-parse-any-variable parser))
      (the Node
        (make-node
          :input
          :input-type input-type
          :variable   variable)))))

;;; -------------------------------------------------------

(defun parser-parse-test (parser)
  "Parses a test or conditional command body and returns a node
   representation thereof.
   ---
   The modeled grammar is:
     test            := testPart , { conditionalPart } ;
     testPart        := '^' , ':' ,
                        anyVariable ,
                        ( '=' | '>' | '<' ) ,
                        ( number | anyVariable ) ;
     conditionalPart := '\' , command ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :colon)
    (parser-skip-spaces parser)
    (let ((variable   NIL)
          (comparison NIL)
          (expression NIL)
          (statements NIL))
      (declare (type (or null Node)       variable))
      (declare (type (or null comparison) comparison))
      (declare (type (or null Node)       expression))
      (declare (type (list-of Node)       statements))
      
      ;; Parse the variable to test.
      (setf variable (parser-parse-any-variable parser))
      (parser-skip-spaces parser)
      
      ;; Parse the test operation.
      (case (token-type current-token)
        (:equal
          (setf comparison :equal)
          (parser-eat parser :equal))
        (:less-than
          (setf comparison :less-than)
          (parser-eat parser :less-than))
        (:greater-than
          (setf comparison :greater-than)
          (parser-eat parser :greater-than))
        (otherwise
          (error "Unexpected comparison token: ~s." current-token)))
      
      (parser-skip-spaces parser)
      
      ;; Parse the expression to test against.
      (setf expression
        (case (token-type current-token)
          (:number
            (parser-parse-number parser))
          
          ((:percentage :at-sign)
            (parser-parse-any-variable parser))
          
          (otherwise
            (error "Invalid test expression token: ~s."
              current-token))))
      
      ;; Parse zero or more "then" statements.
      (parser-skip-spaces parser)
      (loop while (token-type-p current-token :backslash) do
        (parser-eat parser :backslash)
        (parser-skip-spaces parser)
        (push (parser-parse-command parser) statements)
        (parser-skip-spaces parser))
      
      (the Node
        (make-node :test
          :variable   variable
          :comparison comparison
          :expression expression
          :statements (nreverse statements))))))

;;; -------------------------------------------------------

(defun parser-parse-command-body (parser)
  "Parses the body of a command and returns a node representation
   thereof.
   ---
   The modeled grammar is:
     commandBody := numericVariableCreation
                 |  mathematicalOperation
                 |  stringVariableCreation
                 |  print
                 |  input
                 |  test ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Node
      (case (token-type current-token)
        (:percentage
          (parser-parse-numeric-variable-creation parser))
        
        (:ampersand
          (parser-parse-mathematical-operation parser))
        
        (:at-sign
          (parser-parse-string-variable-creation parser))
        
        (:vertical-bar
          (parser-parse-print parser))
        
        (:caret
          (parser-eat parser :caret)
          (parser-skip-spaces parser)
          
          (case (token-type current-token)
            ;; Input.
            (:left-parenthesis
              (parser-parse-input parser))
            ;; Test.
            (:colon
              (parser-parse-test parser))
            (otherwise
              (error "Unrecognized command token: ~s." current-token))))
        
        (otherwise
          (error "Unrecognized command body token: ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-command (parser)
  "Parses a command and returns a node representation thereof.
   ---
   The modeled grammar is:
     command := '~' , '{', commandBody , '}' ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-skip-whitespaces parser)
    (parser-eat parser :tilde)
    (parser-skip-spaces parser)
    (parser-eat parser :left-brace)
    (parser-skip-spaces parser)
    (let ((body (parser-parse-command-body parser)))
      (declare (type (or null Node) body))
      (unless body
        (error "Expected a command between '~~{' and '}', but ~
                encountered the token ~s."
          current-token))
      (parser-skip-spaces parser)
      (parser-eat parser :right-brace)
      (the Node body))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Creates and returns the root node of an abstract syntax tree (AST)
   using the PARSER.
   ---
   The modeled grammar is:
     program := { command } ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((statements NIL))
      (declare (type (list-of Node) statements))
      (parser-skip-whitespaces parser)
      (loop while (token-type-p current-token :tilde) do
        (let ((command (parser-parse-command parser)))
          (declare (type (or null Node) command))
          (if command
            (push command statements)
            (error "Expected a command between '~~{' and '}', but ~
                    encountered the token ~s."
              current-token))
          (parser-skip-whitespaces parser)))
      (the Node
        (make-node :program :statements (nreverse statements))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Variable".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Variable
  (:constructor make-variable (type name &optional (value 0))))
  "The ``Variable'' class models a variable of either numeric or string
   content."
  (type  :number                          :type variable-type)
  (name  (error "Missing variable name.") :type string)
  (value 0                                :type variable-value))

;;; -------------------------------------------------------

(defun make-numeric-variable (name)
  "Creates and returns a numeric ``Variable'' identified by the NAME
   and initialized to the value zero."
  (declare (type string name))
  (the Variable
    (make-variable :number name 0)))

;;; -------------------------------------------------------

(defun make-string-variable (name value)
  "Creates and returns a string ``Variable'' identified by the NAME and
   initialized with the VALUE."
  (declare (type string name))
  (declare (type string value))
  (the Variable
    (make-variable :string name value)))

;;; -------------------------------------------------------

(defun variable-add (variable addend)
  "Increases the numeric VARIABLE's value by the quantity of the ADDED
   and returns the modified VARIABLE."
  (declare (type Variable variable))
  (declare (type real     addend))
  (if (eq (variable-type variable) :number)
    (incf (variable-value variable) addend)
    (error "The variable ~s is not a numeric variable and thus cannot ~
            be augmented by the addend ~a."
      (variable-name variable) addend))
  (the Variable variable))

;;; -------------------------------------------------------

(defun variable-subtract (variable subtrahend)
  "Decrements the numeric VARIABLE's value by the quantity of the
   SUBTRAHEND and returns the modified VARIABLE."
  (declare (type Variable variable))
  (declare (type real     subtrahend))
  (if (eq (variable-type variable) :number)
    (decf (variable-value variable) subtrahend)
    (error "The variable ~s is not a numeric variable and thus cannot ~
            be decremented by the subtrahend ~a."
      (variable-name variable) subtrahend))
  (the Variable variable))

;;; -------------------------------------------------------

(defun variable-multiply (variable multiplier)
  "Multiplies the numeric VARIABLE's value by the MULTIPLIER and returns
   the modified VARIABLE."
  (declare (type Variable variable))
  (declare (type real     multiplier))
  (if (eq (variable-type variable) :number)
    (setf (variable-value variable)
          (* (variable-value variable) multiplier))
    (error "The variable ~s is not a numeric variable and thus cannot ~
            be multiplied by the multiplier ~a."
      (variable-name variable) multiplier))
  (the Variable variable))

;;; -------------------------------------------------------

(defun variable-divide (variable divisor)
  "Divides the numeric VARIABLE's value by the DIVISOR and returns the
   modified VARIABLE."
  (declare (type Variable variable))
  (declare (type real     divisor))
  (if (eq (variable-type variable) :number)
    (setf (variable-value variable)
          (/ (variable-value variable) divisor))
    (error "The variable ~s is not a numeric variable and thus cannot ~
            be divided by the divisor ~a."
      (variable-name variable) divisor))
  (the Variable variable))

;;; -------------------------------------------------------

(defun variable-satisfies-p (variable comparison value)
  "Checks whether the VARIABLE's value satisfies the COMPARISON with the
   VALUE, returning a ``boolean'' result of ``T'' on confirmation,
   otherwise ``NIL''."
  (declare (type Variable   variable))
  (declare (type comparison comparison))
  (declare (type T          value))
  (the boolean
    (not (null
      (case (variable-type variable)
        ;; Compare a numeric VARIABLE.
        (:number
          (unless (realp value)
            (error "Cannot compare the numeric variable ~s against the ~
                    non-numeric value ~s."
              variable value))
          (case comparison
            (:equal
              (= (variable-value variable) value))
            (:less-than
              (< (variable-value variable) value))
            (:greater-than
              (> (variable-value variable) value))
            (otherwise
              (error "Invalid test comparison: ~s." comparison))))
        
        ;; Compare a string VARIABLE.
        (:string
          (unless (stringp value)
            (error "Cannot compare the string variable ~s against the ~
                    non-string value ~s."
              variable value))
          (case comparison
            (:equal
              (string= (variable-value variable) value))
            (:less-than
              (string< (variable-value variable) value))
            (:greater-than
              (string> (variable-value variable) value))
            (otherwise
              (error "Invalid test comparison: ~s." comparison))))
        
        ;; Invalid VARIABLE type.
        (otherwise
          (error "Invalid variable for comparison: ~s." variable)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Duplicate-Variable-Error (error)
  ((identifier
    :initarg       :identifier
    :reader        duplicate-variable-error-identifier
    :type          string
    :documentation "The offending variable name."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Variable-Error condition))
      (declare (type destination              stream))
      (format stream "A variable with the name ~s already exists."
        (duplicate-variable-error-identifier condition))))
  (:documentation
    "Signals that an invalid request has been committed with the intent
     to define a variable whose name is already reserved for such
     purpose."))

;;; -------------------------------------------------------

(defun signal-duplicate-variable-error (identifier)
  "Signals a ``Duplicate-Variable-Error'' mentioning the IDENTIFIER as
   already reserved by a variable."
  (declare (type string identifier))
  (error 'Duplicate-Variable-Error :identifier identifier))

;;; -------------------------------------------------------

(define-condition Undefined-Variable-Error (error)
  ((identifier
    :initarg       :identifier
    :reader        undefined-variable-error-identifier
    :type          string
    :documentation "The missing variable's name."))
  (:report
    (lambda (condition stream)
      (declare (type Undefined-Variable-Error condition))
      (declare (type destination              stream))
      (format stream "No variable with the name ~s exists."
        (undefined-variable-error-identifier condition))))
  (:documentation
    "Signals that an invalid request for an access has been committed
     towards a nonextant variable."))

;;; -------------------------------------------------------

(defun signal-undefined-variable-error (identifier)
  "Signals an ``Undefined-Variable-Error'' mentioning the IDENTIFIER as
   the missing variable name."
  (declare (type string identifier))
  (error 'Undefined-Variable-Error :identifier identifier))

;;; -------------------------------------------------------

(define-condition Invalid-Argument-Error (error)
  ((offending-argument
    :initarg       :offending-argument
    :initform      NIL
    :reader        invalid-argument-error-offending-argument
    :type          T
    :documentation "The unexpected argument which, having caused this
                    condition, constitutes its culprit."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Argument-Error condition))
      (declare (type destination            stream))
      (format stream "The argument ~s is invalid in this context."
        (invalid-argument-error-offending-argument condition))))
  (:documentation
    "Signals that an argument is proscribed in the context of its
     attempted use, either by virtue of its value or by its type."))

;;; -------------------------------------------------------

(defun signal-invalid-argument-error (offending-argument)
  "Signals an ``Invalid-Argument-Error'' reporting the
   OFFENDING-ARGUMENT as the anomaly's etiology."
  (declare (type T offending-argument))
  (error 'Invalid-Argument-Error
    :offending-argument offending-argument))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing tree for the interpreter.")
    :type          Node
    :documentation "The abstract syntax tree (AST) to process.")
   (variables
    :initarg       :variables
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string Variable)
    :documentation "Maintains a list of variable names mapped to their
                    respective ``Variable'' objects."))
  (:documentation
    "The ``Interpreter'' class fulfils the onus of traversing an
     abstract syntax tree (AST) in order to imbue the same with an
     effect."))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE, identified by the NODE-TYPE, using the
     INTERPRETER and returns a value meet for the handled NODE."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns an ``Interpreter'' evaluating the abstract
   syntax TREE."
  (declare (type Node tree))
  (the Interpreter (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun get-variable (interpreter name)
  "Returns the variable registered with the NAME at the INTERPRETER, or
   ``NIL'' if no such association holds."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the (or null Variable)
    (gethash name
      (slot-value interpreter 'variables))))

;;; -------------------------------------------------------

(defun set-variable (interpreter variable)
  "Registers the VARIABLE with its name at the INTERPRETER and returns
   the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type Variable    variable))
  (setf (gethash (variable-name variable)
                 (slot-value interpreter 'variables))
        variable)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun check-if-variable-exists (interpreter name)
  "Checks whether a variable with the NAME is registered at the
   INTERPRETER, on confirmation returning the respective ``Variable''
   object, otherwise signaling an error of the type
   ``Undefined-Variable-Error''."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the Variable
    (or (get-variable interpreter name)
        (signal-undefined-variable-error name))))

;;; -------------------------------------------------------

(defun check-if-name-is-available (interpreter name)
  "Checks whether the variable NAME is available for an assignment to
   a variable, that is, not registered at the INTERPRETER, on
   confirmation returning the NAME, otherwise signaling an error of the
   type ``Duplicate-Variable-Error''."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (when (get-variable interpreter name)
    (signal-duplicate-variable-error name))
  (the string name))

;;; -------------------------------------------------------

(defun input-number ()
  "Reads from the standard input until a valid ``real'' number has been
   obtained, finally returning the same."
  (flet ((read-number ()
          "Attempts to read a real number from the standard input,
           returning on success its parsed value, otherwise ``NIL''."
          (let ((input (read-from-string (read-line))))
            (declare (type T input))
            (the (or null real)
              (when (typep input 'real)
                input)))))
    (the real
      (loop
        for     input of-type (or null real) = (read-number)
        until   input
        finally (return input)))))

;;; -------------------------------------------------------

(defun visit-node (interpreter node)
  "Visits the NODE using the INTERPRETER by dispatching on the NODE's
   type."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (dispatch-node interpreter (node-type node) node))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :program))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((statements (node-attribute node :statements)))
    (declare (type (list-of Node) statements))
    (dolist (statement statements)
      (declare (type Node statement))
      (visit-node interpreter statement)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :identifier))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((identifier (node-attribute node :name)))
    (declare (type string identifier))
    (the variable-value
      (variable-value
        (check-if-variable-exists interpreter identifier)))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :number))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :string))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the string (node-attribute node :content)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :numeric-variable-creation))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((identifier (node-attribute node :name)))
    (declare (type Node identifier))
    (let ((name (node-attribute identifier :name)))
      (declare (type string name))
      (check-if-name-is-available interpreter name)
      (set-variable interpreter (make-numeric-variable name))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :mathematical-operation))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((left-operand  (node-attribute node :left-operand))
        (operator      (node-attribute node :operator))
        (right-operand (node-attribute node :right-operand)))
    (declare (type Node                 left-operand))
    (declare (type arithmetic-operation operator))
    (declare (type Node                 right-operand))
    (let ((left-variable (check-if-variable-exists interpreter
                           (node-attribute left-operand :name)))
          (right-value   0))
      (declare (type Variable left-variable))
      (declare (type real     right-value))
      (setf right-value (visit-node interpreter right-operand))
      (case operator
        (:addition
          (variable-add left-variable right-value))
        (:subtraction
          (variable-subtract left-variable right-value))
        (:multiplication
          (variable-multiply left-variable right-value))
        (:division
          (variable-divide left-variable right-value))
        (otherwise
          (error "Invalid operator: ~s." operator)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :string-variable-creation))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((identifier (node-attribute node :name))
        (text       (node-attribute node :text)))
    (declare (type Node identifier))
    (declare (type Node text))
    (let ((variable-name (node-attribute identifier :name))
          (text-string   (node-attribute text       :content)))
      (declare (type string variable-name))
      (declare (type string text-string))
      (check-if-name-is-available interpreter variable-name)
      (set-variable interpreter
        (make-string-variable variable-name text-string))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :print))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((argument (node-attribute node :argument)))
    (declare (type Node argument))
    (let ((variable-name (node-attribute argument :name)))
      (declare (type string variable-name))
      (format T "~a"
        (variable-value
          (check-if-variable-exists interpreter variable-name)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :input))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((input-type (node-attribute node :input-type))
        (variable   (node-attribute node :variable)))
    (declare (type input-type input-type))
    (declare (type Node       variable))
    (let ((variable-name (node-attribute variable :name)))
      (declare (type string variable-name))
      (let ((variable-object
              (check-if-variable-exists interpreter variable-name)))
        (declare (type Variable variable-object))
        (setf (variable-value variable-object)
          (case input-type
            (:number
              (if (eq (variable-type variable-object) :number)
                (input-number)
                (signal-invalid-argument-error variable-object)))
            (:text
              (if (eq (variable-type variable-object) :string)
                (read-line)
                (signal-invalid-argument-error variable-object)))
            (otherwise
              (signal-invalid-argument-error input-type)))))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :test))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((variable   (node-attribute node :variable))
        (comparison (node-attribute node :comparison))
        (expression (node-attribute node :expression))
        (statements (node-attribute node :statements)))
    (declare (type Node           variable))
    (declare (type comparison     comparison))
    (declare (type Node           expression))
    (declare (type (list-of Node) statements))
    (let ((variable-object
            (check-if-variable-exists interpreter
              (node-attribute variable :name)))
          (expected-value
            (visit-node interpreter expression)))
      (declare (type Variable         variable-object))
      (declare (type (or real string) expected-value))
      (when (variable-satisfies-p variable-object
                                  comparison
                                  expected-value)
        (dolist (statement statements)
          (declare (type Node statement))
          (visit-node interpreter statement)))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree stored in the INTERPRETER and
   returns the INTERPRETER itself."
  (declare (type Interpreter interpreter))
  (visit-node interpreter (slot-value interpreter 'tree))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Asig (code)
  "Interprets the piece of Asig CODE and returns no value."
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

;; Create a variable called "Joe", add the value 2 to it, and print it
;; to the standard output.
(interpret-Asig
  "~{%Joe}
   ~{&:%Joe#+2}
   ~{|%Joe}")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-Asig
  "~{@greeting [Hello, World!]}
   ~{|@greeting}")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Asig
  "~{@input[]}
   ~{^(@)@input}
   ~{|@input}")

;;; -------------------------------------------------------

;; Greeting:
;;   Query the user name, store it in a variable, and output a greeting
;;   text entailing the same.
(interpret-Asig
  "~{@yourName[]}
   
   ~{@greetingPrefix[Hello, ]}
   ~{@greetingSuffix[.]}
   ~{@promptMessage[Please enter your name: ]}
   
   ~{|@promptMessage}
   
   ~{^(@)@yourName}
   
   ~{|@greetingPrefix}
   ~{|@yourName}
   ~{|@greetingSuffix}")

;;; -------------------------------------------------------

;; Addition:
;;   Query the user for an addition's augend and addend, compute the
;;   sum, and print it to the standard output.
(interpret-Asig
  "~{@promptMessageFirstNumber  [Please enter the augend: ]}
   ~{@promptMessageSecondNumber [Please enter the addend: ]}
   ~{@resultMessage             [The sum of these numbers is: ]}
   
   ~{%leftOperand}
   ~{%rightOperand}
   ~{%result}
   
   ~{|@promptMessageFirstNumber}
   ~{^(#)%leftOperand}
   
   ~{|@promptMessageSecondNumber}
   ~{^(#)%rightOperand}
   
   ~{&:%result#+%leftOperand}
   ~{&:%result#+%rightOperand}
   
   ~{|@resultMessage}
   ~{|%result}")

;;; -------------------------------------------------------

;; Calculator:
;;   Query the user for two operands and an operator, compute the
;;   result, and print it to the standard output.
;;   If an invalid string is entered as an operator name, no action is
;;   performed, and no result is printed.
(interpret-Asig
  "~{@promptMessageFirstNumber  [Please enter the left operand: ]}
   ~{@promptMessageSecondNumber [Please enter the right operand: ]}
   ~{@promptMessageOperator     [Please enter the operator: ]}
   ~{@resultMessageSpace        [ ]}
   ~{@resultMessageEquals       [ = ]}
   ~{@constantPlus              [+]}
   ~{@constantMinus             [-]}
   ~{@constantTimes             [*]}
   ~{@constantDivide            [/]}
   
   ~{%leftOperand}
   ~{%rightOperand}
   ~{@operator []}
   ~{%result}
   
   ~{|@promptMessageFirstNumber}
   ~{^(#)%leftOperand}
   
   ~{|@promptMessageSecondNumber}
   ~{^(#)%rightOperand}
   
   ~{|@promptMessageOperator}
   ~{^(@)@operator}
   
   ~{&:%result#+%leftOperand}
   
   ~{^:@operator = @constantPlus   \\ ~{&: %result # + %rightOperand}}
   ~{^:@operator = @constantMinus  \\ ~{&: %result # - %rightOperand}}
   ~{^:@operator = @constantTimes  \\ ~{&: %result # * %rightOperand}}
   ~{^:@operator = @constantDivide \\ ~{&: %result # / %rightOperand}}
   
   ~{|%leftOperand}
   ~{|@resultMessageSpace}
   ~{|@operator}
   ~{|@resultMessageSpace}
   ~{|%rightOperand}
   ~{|@resultMessageEquals}
   ~{|%result}")

;;; -------------------------------------------------------

;; Number sign check:
;;   Input a number and check whether the same is positive, negative, or
;;   zero, responding upon each matching case with a different message.
(interpret-Asig
  "~{@promptText      [Please input a number: ]}
   ~{@messagePositive [Your input number was positive.]}
   ~{@messageZero     [Your input number was zero.]}
   ~{@messageNegative [Your input number was negative.]}
   
   ~{|%promptText}
   ~{%input}
   ~{^(#)%input}
   
   ~{^:%input > 0 \\ ~{|@messagePositive}}
   ~{^:%input = 0 \\ ~{|@messageZero}}
   ~{^:%input < 0 \\ ~{|@messageNegative}}")
