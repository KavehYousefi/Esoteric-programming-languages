;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "8", invented by the Esolang user "AmNow" and presented on
;; June 26th, 2021, the kenspeckle haecceity of which is commorant in
;; its commands' weftage that naits a combination of numbers, colons,
;; brackets, parentheses, and further special symbols in order to convey
;; its purposes.
;; 
;; 
;; Concept
;; =======
;; The 8 programming language designates a specimen imbued with an
;; enumerative manifestation of its commands. Its agnomination, "8",
;; bewrays the cardinality of its instruction set, and the unique
;; identification aided by the numeric allocation encompasses variable
;; manipulation, simple arithmetics, string operations, Boolean logic,
;; input/output conduits, and conditional facilities.
;; 
;; == INSTRUCTIONS TYPES ARE IDENTIFIED BY NUMERALS ==
;; A signum of its conception, the available commands are enumerated
;; utilizing an unsigned integer identifier in a nominal agency; the
;; octal account, as an apostil, justifies the language's agnomination.
;; Ensuing from the identifying aspect, all operations, if dependents
;; upon inputs, employ brackets in various forms and sepiment tokens
;; just as variegated in order to establish an operative unit.
;; 
;; == PROGRAMS OPERATE IN AN INFINITE LOOP ==
;; A basic attribute of its operation model relates to the implicit
;; loop, infinite in its incitations, inwith which the whole program
;; executes, halting only upon a confrontation with the terminating
;; "0::" command.
;; 
;; == VARIABLES ESTABLISH THE SOLE DATA STORAGE ==
;; In terms of program data management no other accommodation is
;; proffered besides untyped variables. All ilks of objects, the compass
;; including numbers, strings and Boolean values, are admitted in such
;; scalar placeholders.
;; 
;; 
;; Architecture
;; ============
;; Maugre its independence from a specific architectural concept or
;; storage principle, the existence of variables conditions a design
;; meet for the cause.
;; 
;; Variables, as mutable entities, require the faculty of their inquest
;; as well as their modification, in both cases amenable to their unique
;; identifier name. Their castaldy, as a consequence of these
;; characteristics, recommends its manifestation inside of a registry in
;; the form of a mapping, associating with the identification the
;; respective variable. The scantiness applying to such a placeholder's
;; attributes permits a minimum of information to be involved in the
;; representation --- merely its value and existence participate
;; therein.
;; 
;; 
;; Data Types
;; ==========
;; The circumference of 8's type system may reckoned a rather wealthy
;; specimen among esoteric programming languages, embracing in its
;; extent Boolean values, number of both the integer and floating-point
;; variation, and strings.
;; 
;; == BOOLEANS ==
;; The language is capacitated with the representation of binary choices
;; in the form of Boolean values, a set whose exhaustion is already
;; achieved by an affirmative "true" and a negative "false" member. Its
;; chief field of application locates it in the realms of predicates, as
;; necessitated by the "while" loop and conditional "if" statements.
;; 
;; == NUMBERS ==
;; The numeric aspect limns a government sufficiently powerful to
;; accommodate both integers and floating-point objects. The membership
;; applying to both does not encumber with the imposition of constraints
;; regarding the magnitude of either the integral nor the fractional
;; aspect, if entailing the latter at all. All numbers may be positive
;; or negative. Such a datum's internal representation, in particular
;; airted at the floating-point species' convolutions, eludes a
;; standardization.
;; 
;; == STRINGS ==
;; Textual content may be stated in the form of character sequences,
;; tallying an arbitrary length, and including the empty string. The
;; constituents are homologated their borrowing from the Unicode set,
;; by which a wide range of expressions can be provided. A string is
;; introduced by the opening double quote "“" and terminated using the
;; closing equivalent "”". This kenspeckle diorism administers the
;; language a special potential in that sections demarcated by jumelles
;; of "“" and "”" will be incorporated when detected inside of a string.
;; For instance, the text
;;   “He exclaimed, “Pigeons are cute,” during the discourse.”
;; will not inflict a program with an error, instead producing
;;   He exclaimed, “Pigeons are cute,” during the discourse.
;; 
;; 
;; Syntax
;; ======
;; Whereas preponderantly a rather homogenous pattern's produce, the
;; concrete expressions of the language regarding its facilities entail
;; a commixture of syntactical pecularities.
;; 
;; == INSTRUCTIONS ==
;; Each instruction is introduced by an unsigned integer's adminiculum,
;; stating its type, followed by separators that, if requisite, ensconce
;; the bracketed arguments, as well as terminate the command itself.
;; 
;; The concrete choice of sepiment character depends upon a command and
;; its variation, and may resolve to:
;;   
;;   - The colon ":".
;;   - The semicolon ";".
;;   - The comma ",'.
;;   - The apostrophe "’".
;;   - The paragraph character "§".
;;   - The less-than sign "<".
;; 
;; Any arguments, if participants in the statement, are either
;; surrounded by a set of brackets or parenthesis, as such intended as
;; demarcations from the environment.
;; 
;; == BOOLEAN VALUES ==
;; The dichotomoy of the Boolean data type, composed of a true and false
;; membership, projects into the 8 language. The affirmative moeity is
;; represented by the constant "TRUE", whereas is opposite bears the
;; euonymous designation "FALSE". No other variant exists in this
;; strongly restricted set.
;; 
;; == NUMBERS ==
;; Two general tiers of numbers are admitted into the language: integers
;; and floating-point value, both signed and potentially unbounded in
;; their magnitude.
;; 
;; == STRINGS ==
;; A string entails a sequence of zero or more characters, demarcated to
;; its left by an opening double quote "“", and by its right to the
;; closing equivalent "”".
;; 
;; == IDENTIFIERS ==
;; A variable's agnomination proceeds by means of an identifier, as
;; character sequence incited by a letter and followed by a combination
;; of zero or more letters, digits, or underscores, the total tally of
;; which does not obey to any particular imposition.
;; 
;; == WHITESPACES ==
;; The insertion and distribution of whitespace, except for the inherent
;; significance apportioned to them in strings, adheres to one's own
;; liberality. The category enumerates the space, tab, and newline
;; character.
;; 
;; == COMMENTS ==
;; No provisions for comments are accommodated in the current language
;; iteration.
;; 
;; == GRAMMAR ==
;; The 8 language's syntax shall be following Extended Backus-Naur
;; Form's (EBNF) subject:
;;   
;;   program          := commandList ;
;;   commandList      := { command } ;
;;   command          := print
;;                    |  setVariable
;;                    |  getVariableValue
;;                    |  input
;;                    |  whileLoop
;;                    |  binaryOperation
;;                    |  if
;;                    |  halt
;;                    ;
;;   
;;   print            := ( "1" , ":" , "[" , expression , "]" , ":" )
;;                    |  ( "1" , ";" , "[" , expression , "]" , ";" )
;;                    ;
;;   
;;   setVariable      := "2" , ":" , "[" , identifier , "]" , "<" ,
;;                                   "[" , expression , "]" , ":"
;;                    ;
;;   
;;   getVariableValue := "3" , ":" , "[" , identifier , "]" ;
;;   
;;   input            := "4" , ":" , "[" , identifier , "]" ;
;;   
;;   whileLoop        := "5" , ":" , "[" , expression  , "]" , ";" ,
;;                                   "(" , commandList , ")" , ":"
;;                    ;
;;   
;;   binaryOperation  := ( "6" , ":" , "[" , expression , "]" , "’" ,
;;                                     "[" , operator   , "]" , "’" ,
;;                                     "[" , expression , "]" , ":" )
;;                    |  ( "6" , ":" , "[" , expression , "]" , "," ,
;;                                     "[" , operator   , "]" , "," ,
;;                                     "[" , expression , "]" , ":" )
;;                    ;
;;   
;;   if               := "7" , ":" , "[" , expression  , "]" , "§" ,
;;                                   "(" , commandList , ")" , ":"
;;                    ;
;;   
;;   halt             := "0" , ":" , ":" ;
;;   
;;   operator         := "==" | "!=" | "<" | ">" | ">=" | "<="
;;                    |  "+"  | "-"  | "*" | "/" | "%"
;;                    ;
;;   
;;   identifier       := letter , { letter | digit | underscore } ;
;;   string           := "“" , { character } , "”" ;
;;   number           := integer | float ;
;;   float            := integer , "." , digit , { digit } ;
;;   integer          := [ "+" | "-" ] , digit , { digit } ;
;;   
;;   underscore       := "_" ;
;;   letter           := "a" | ... | "z" | "A" | ... | "Z" ;
;;   digit            := "0" | "1" | "2" | "3" | "4"
;;                    |  "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Instructions
;; ============
;; The 8 language's designation already parlays of its instruction set's
;; cardinality, with its members being the result of a certain principle
;; of assemblage: The introduction of all commands proceeds by aide of
;; an integer number in the closed range of zero (0) to seven (7),
;; acting as the operation identifier.
;; 
;; Whereas the identity conforms to the established tally, a subset of
;; commands manifest in variations, distinguished herein by a
;; discrepancy reigning over particular constituent characters.
;; 
;; == CATEGORIES OF INSTRUCTIONS ==
;; The rather niggardly investment into the quantity of operations does
;; fortunately not maintain an equilibrium in the language's potency.
;; A rather complete selection of facilities, subsumable into several
;; categories, embraces the dations to the developer:
;;   
;;   - Binary operations:
;;     Binary operations may be applied to the 8 object types,
;;     comprising not only arithmetics, but also the logical and string
;;     manipulation bailiwick. If confronted with a left operand in its
;;     variable form, destructive modifications can be involved; in any
;;     case, a non-destructive returning of the result is homologated.
;;   
;;   - Input and output commands:
;;     Literal objects and variable contents may be reproduced on the
;;     standard output. User input in variegated designs may be
;;     transmitted into variables for further processions.
;;   
;;   - Conditional facilities:
;;     The language offers two conditional constructs: a while loop and
;;     an "if" statement which comprehends merely the positive moeity,
;;     that is, a "then" compartment.
;; 
;; == OVERVIEW ==
;; An apercu shall impart a fist gnarity anenst the available constructs
;; and their effects. Being a work of inchoation, the succeeding
;; sections will evaluate each member with deeper sophistication.
;; 
;; When perusing the table, please note that the identifiers "x", "y"
;; and "op" define the placeholders for the operation arguments; any
;; other character is to be considered a verbatim constituent.
;;   
;;   ------------------------------------------------------------------
;;   Command         | Effect
;;   ----------------+-------------------------------------------------
;;   0::             | Terminates the program.
;;   ..................................................................
;;   1:[x]:          | Prints {x}, followed by a newline.
;;   ..................................................................
;;   1;[x];          | Prints {x} without appending a newline.
;;   ..................................................................
;;   2:[x]<[y]:      | Sets the value of the variable designated by the
;;                   | identifier {x} to {y}.
;;   ..................................................................
;;   3:[x]:          | Returns the value of the variable designated by
;;                   | the identifier {y}.
;;   ..................................................................
;;   4:[x]:          | Queries the user for an input and stores it in
;;                   | the variable designated by the identifier {x}.
;;   ..................................................................
;;   5:[x];(y):      | Repeatedly executes the sequence of zero or more
;;                   | commands {y} as long as the condition {x} is
;;                   | satisfied.
;;                   | This command simulates a "while" loop.
;;   ..................................................................
;;   6:[x]’[op]’[y]: | Performs the binary operation {op}, using {x}
;;                   | as its left operand and {y} as the right one,
;;                   | and stores the result back in {x}.
;;                   | For a list of all homologated operations {op}
;;                   | please consult the dedicated section below.
;;                   | {x} Must be designate a variable.
;;   ..................................................................
;;   6:[x],[op],[y]: | Performs the binary operation {op}, using {x}
;;                   | as its left operand and {y} as the right one,
;;                   | and returns the result.
;;                   | {x} Must be designate a variable.
;;                   | For a list of all homologated operations {op}
;;                   | please consult the dedicated section below.
;;   ..................................................................
;;   7:[x]§(y):      | Executes the sequence of zero or more commands
;;                   | {y} if the condition {x} is satisfied; otherwise
;;                   | demonstrates no effect.
;;                   | This command simulates an "if" statement without
;;                   | an "else" branch.
;;   ------------------------------------------------------------------
;; 
;; == 0: HALT ==
;; Terminates the program.
;; 
;; Signature:
;;   0::
;; 
;; Interface:
;;   halt () : void
;; 
;; Decription:
;;   Halts the program, thus terminating the tacit infinite loop
;;   impelling a program's execution.
;; 
;; Side effects:
;;   - Terminates the tacitly operating infinite program loop.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == 1: PRINT ==
;; Prints an object to the standard output.
;; 
;; Signature --- newline variant:
;;   1:[x]:
;; Signature --- continuous variant:
;;   1;[x];
;; 
;; Interface --- newline variant:
;;   printLine (x : {boolean, number, string}) : void
;; Interface --- continuous variant:
;;   print     (x : {boolean, number, string}) : void
;; 
;; Description:
;;   Prints the object {x} to the standard output. If colons (":") are
;;   employed in the agency of separators, a single newline character is
;;   adhibited to the output. Upon the utilization of semicolons (";")
;;   no newline suffix is appended.
;; 
;; == 2: SET ==
;; Sets the value of a variable to a specified value.
;; 
;; Signature:
;;   2:[x]<[y]:
;; 
;; Interface:
;;   set (x : variable, y : {boolean, number, string}) : void
;; 
;; Description:
;;   Assigns to the variable designated by the name {x} the result of
;;   the evaluated expression {y}.
;;   If no variable with such identifier exists, a new one is created;
;;   otherwise the extant variable's content is replaced.
;; 
;; Side effects:
;;   - If no variable with the name {x} exists, a new variable using
;;     this identifier is created, registered, and associated with the
;;     value {y}.
;;   - An error of the type "IllegalArgument" is signaled if the
;;     variable name {x} designates a reserved identifier.
;;   - If a variable with name {x} already exists, its content is
;;     replaced by the value {y}.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == 3: GET ==
;; Returns a variable's value.
;; 
;; Signature:
;;   3:[x]:
;; 
;; Interface:
;;   get (x : variable) : any
;; 
;; Description:
;;   Returns the variable amenable to the name {x}.
;; 
;; Side effects:
;;   - None.
;; 
;; Exceptional situations:
;;   - An error of the type "MissingVariable" is signaled if the
;;     identifier name {x} does not associate with a registered
;;     variable.
;; 
;; == 4: INPUT ==
;; Stores a user input in a variable.
;; 
;; Signature:
;;   4:[x]:
;; 
;; Interface:
;;   input (x : variable) : void
;; 
;; Description:
;;   Queries the user for an input and stores the response in the
;;   variable amenable to the name {x}.
;; 
;; Side effects:
;;   - If no variable with the name {x} exists, a new variable using
;;     this identifier is created, registered, and associated with the
;;     value supplied by the user.
;;   - If a variable with name {x} already exists, its content is
;;     replaced by the value supplied by the user.
;; 
;; Exceptional situations:
;;   - An implementation-dependent error may be signaled if the input
;;     conduit responsible for the transmission of user data to the
;;     program is inflicted with a defect.
;; 
;; == 5: WHILE LOOP ==
;; Performs a series of zero or more statements while a condition is
;; satisfied.
;; 
;; Signature:
;;   5:[x];(y):
;; 
;; Interface:
;;   while (x : command, y : command[0..*]) : void
;; 
;; Description:
;;   Performs the sequence of zero or more statements {y} as long as the
;;   predicate {x} remains satisfied.
;; 
;; Side effects:
;;   - No intrinsic side effects govern the operation.
;;   - Any side effects inhering inside of the conditional expression
;;     {x} or any of statements in {y} propagate through this command.
;; 
;; Exceptional situations:
;;   - Any exceptional situations inhering inside of the conditional
;;     expression {x} or any of the statements in {y} propagate through
;;     this command.
;; 
;; == 6: BINARY OPERATION ==
;; Performs a binary operation and eithers stores the result in the left
;; operand variable or returns it without side effects.
;; 
;; Signature --- destructive variant:
;;   6:[x]’[op]’[y]:
;; Signature --- non-destructive variant:
;;   6:[x],[op],[y]:
;; 
;; Interface --- destructive variant:
;;   operate (x : variable, op : operator, y : any) : void
;; Interface --- non-destructive variant:
;;   operate (x : any,      op : operator, y : any) : any
;; 
;; Description:
;;   Applies the operator {op} to the left operand {x} and the right
;;   operand {y}. In the destructive variant, stores the result back in
;;   {x}, which must be a variable name. In the non-destructive variant,
;;   returns the result without any modifications of {x}.
;;   
;;   The following values enumerate the valid options for the operator
;;   {op}:
;;   
;;     ----------------------------------------------------------------
;;     Operator | Effect
;;     ---------+------------------------------------------------------
;;     +        | Addition or string concatenation.
;;     ................................................................
;;     -        | Subtraction.
;;     ................................................................
;;     *        | Multiplication or string repetition.
;;     ................................................................
;;     /        | Division.
;;     ................................................................
;;     ^        | Exponentiation.
;;     ................................................................
;;     =        | Equality.
;;     ................................................................
;;     !=       | Inequality.
;;     ................................................................
;;     <        | Less than.
;;     ................................................................
;;     <=       | Less than or equal.
;;     ................................................................
;;     >        | Greater than.
;;     ................................................................
;;     >=       | Greater than or equal.
;;     ----------------------------------------------------------------
;;   
;;   The nimiety commorant in the multifarious combinations and their
;;   effects, including discordance that culminates in errors, vindicate
;;   the following tables' establishment, with their compass embracing
;;   the operators and their interplay in confrontation with the various
;;   available types of the language. Please note that the first column,
;;   "Op" designates the operator, while "x" identifies the left
;;   operand, succeeded by "y" as the right participant.
;;   
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     +  | boolean | boolean | Logical inclusive OR: {x} OR {y}.
;;     ................................................................
;;     +  | boolean | number  | Not homologated.
;;     ................................................................
;;     +  | boolean | string  | String concatenation: {x}, {y}.
;;     ................................................................
;;     +  | number  | boolean | Not homologated.
;;     ................................................................
;;     +  | number  | number  | Sum: {x} + {y}.
;;     ................................................................
;;     +  | number  | string  | String concatenation: {x}, {y}.
;;     ................................................................
;;     +  | string  | boolean | String concatenation: {x}, {y}.
;;     ................................................................
;;     +  | string  | number  | String concatenation: {x}, {y}.
;;     ................................................................
;;     +  | string  | string  | String concatenation: {x}, {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     -  | boolean | boolean | Not homologated.
;;     ................................................................
;;     -  | boolean | number  | Not homologated.
;;     ................................................................
;;     -  | boolean | string  | Not homologated.
;;     ................................................................
;;     -  | number  | boolean | Not homologated.
;;     ................................................................
;;     -  | number  | number  | Difference: {x} - {y}.
;;     ................................................................
;;     -  | number  | string  | Not homologated.
;;     ................................................................
;;     -  | string  | boolean | Not homologated.
;;     ................................................................
;;     -  | string  | number  | Not homologated.
;;     ................................................................
;;     -  | string  | string  | Not homologated.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     *  | boolean | boolean | Logical AND: {x} AND {y}.
;;     ................................................................
;;     *  | boolean | number  | Not homologated.
;;     ................................................................
;;     *  | boolean | string  | Not homologated.
;;     ................................................................
;;     *  | number  | boolean | Not homologated.
;;     ................................................................
;;     *  | number  | number  | Product: {x} * {y}.
;;     ................................................................
;;     *  | number  | string  | String repetition: {x} times {y}.
;;     ................................................................
;;     *  | string  | boolean | Not homologated.
;;     ................................................................
;;     *  | string  | number  | String repetition: {x} by {y}.
;;     ................................................................
;;     *  | string  | string  | Not homologated.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     /  | boolean | boolean | Not homologated.
;;     ................................................................
;;     /  | boolean | number  | Not homologated.
;;     ................................................................
;;     /  | boolean | string  | Not homologated.
;;     ................................................................
;;     /  | number  | boolean | Not homologated.
;;     ................................................................
;;     /  | number  | number  | Difference: {x} / {y}.
;;     ................................................................
;;     /  | number  | string  | Not homologated.
;;     ................................................................
;;     /  | string  | boolean | Not homologated.
;;     ................................................................
;;     /  | string  | number  | Not homologated.
;;     ................................................................
;;     /  | string  | string  | Not homologated.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     ^  | boolean | boolean | Logical exclusive OR: {x} XOR {y}.
;;     ................................................................
;;     ^  | boolean | number  | Not homologated.
;;     ................................................................
;;     ^  | boolean | string  | Not homologated.
;;     ................................................................
;;     ^  | number  | boolean | Not homologated.
;;     ................................................................
;;     ^  | number  | number  | Exponentiation: {x} raised to power {y}.
;;     ................................................................
;;     ^  | number  | string  | Not homologated.
;;     ................................................................
;;     ^  | string  | boolean | Not homologated.
;;     ................................................................
;;     ^  | string  | number  | Not homologated.
;;     ................................................................
;;     ^  | string  | string  | Not homologated.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     =  | boolean | boolean | Logical equivalence: {x} <-> {y}.
;;     ................................................................
;;     =  | boolean | number  | Always false.
;;     ................................................................
;;     =  | boolean | string  | Always false.
;;     ................................................................
;;     =  | number  | boolean | Always false.
;;     ................................................................
;;     =  | number  | number  | Numeric equality: {x} = {y}.
;;     ................................................................
;;     =  | number  | string  | Always false.
;;     ................................................................
;;     =  | string  | boolean | Always false.
;;     ................................................................
;;     =  | string  | number  | Always false.
;;     ................................................................
;;     =  | string  | string  | Case-sensitive equality: {x} = {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     != | boolean | boolean | Logical XOR: {x} XOR {y}.
;;     ................................................................
;;     != | boolean | number  | Always true.
;;     ................................................................
;;     != | boolean | string  | Always true.
;;     ................................................................
;;     != | number  | boolean | Always true.
;;     ................................................................
;;     != | number  | number  | Numeric inequality: {x} != {y}.
;;     ................................................................
;;     != | number  | string  | Always true.
;;     ................................................................
;;     != | string  | boolean | Always true.
;;     ................................................................
;;     != | string  | number  | Always true.
;;     ................................................................
;;     != | string  | string  | Case-sensitive inequality: {x} != {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     <  | boolean | boolean | (not p) and q.
;;     ................................................................
;;     <  | boolean | number  | Not homologated.
;;     ................................................................
;;     <  | boolean | string  | Not homologated.
;;     ................................................................
;;     <  | number  | boolean | Not homologated.
;;     ................................................................
;;     <  | number  | number  | Numeric less than: {x} < {y}.
;;     ................................................................
;;     <  | number  | string  | Not homologated.
;;     ................................................................
;;     <  | string  | boolean | Not homologated.
;;     ................................................................
;;     <  | string  | number  | Not homologated.
;;     ................................................................
;;     <  | string  | string  | Case-sensitive comparison: {x} < {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     <= | boolean | boolean | Logical implication: {x} -> {y}.
;;     ................................................................
;;     <= | boolean | number  | Not homologated.
;;     ................................................................
;;     <= | boolean | string  | Not homologated.
;;     ................................................................
;;     <= | number  | boolean | Not homologated.
;;     ................................................................
;;     <= | number  | number  | Numeric less or equal: {x} <= {y}.
;;     ................................................................
;;     <= | number  | string  | Not homologated.
;;     ................................................................
;;     <= | string  | boolean | Not homologated.
;;     ................................................................
;;     <= | string  | number  | Not homologated.
;;     ................................................................
;;     <= | string  | string  | Case-sensitive comparison: {x} <= {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     >  | boolean | boolean | Negated implication: not({x} -> {y}).
;;     ................................................................
;;     >  | boolean | number  | Not homologated.
;;     ................................................................
;;     >  | boolean | string  | Not homologated.
;;     ................................................................
;;     >  | number  | boolean | Not homologated.
;;     ................................................................
;;     >  | number  | number  | Numeric greater than: {x} > {y}.
;;     ................................................................
;;     >  | number  | string  | Not homologated.
;;     ................................................................
;;     >  | string  | boolean | Not homologated.
;;     ................................................................
;;     >  | string  | number  | Not homologated.
;;     ................................................................
;;     >  | string  | string  | Case-sensitive comparison: {x} > {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;     
;;     ----------------------------------------------------------------
;;     Op | x       | y       | Result
;;     ---+---------+---------+----------------------------------------
;;     >= | boolean | boolean | (not ((not p) and q)).
;;     ................................................................
;;     >= | boolean | number  | Not homologated.
;;     ................................................................
;;     >= | boolean | string  | Not homologated.
;;     ................................................................
;;     >= | number  | boolean | Not homologated.
;;     ................................................................
;;     >= | number  | number  | Numeric greater or equal: {x} >= {y}.
;;     ................................................................
;;     >= | number  | string  | Not homologated.
;;     ................................................................
;;     >= | string  | boolean | Not homologated.
;;     ................................................................
;;     >= | string  | number  | Not homologated.
;;     ................................................................
;;     >= | string  | string  | Case-sensitive comparison: {x} >= {y}.
;;     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; Side effects:
;;   - If invoked in the destructive variant, the variable amenable to
;;     the identifier {x} is modified.
;; 
;; Exceptional situations:
;;   - Signals an error of the type "MissingVariable" if {x} constitutes
;;     a identifier, with whom no variable associates.
;;   - Signals an error of the type "MissingVariable" if {y} constitutes
;;     a identifier, with whom no variable associates.
;;   - Signals an error of the type "IllegalArgument" if the destructive
;;     variant is employed, thus pursuing to write the result to the
;;     left operand {x}, but this does not constitute a variable name.
;;   - Signals an error of the type "IllegalArgument" if the operator
;;     {op} does not resolve to any of the recognized values.
;;   - Signals an error of the type "IllegalArgument" if the operator
;;     {op} is applied to one or more operands for whom singly or in
;;     this particular combination no compatibility is defined.
;; 
;; == 7: CONDITIONAL ==
;; Performs a series of zero or more statements if a condition is
;; satisfied.
;; 
;; Signature:
;;   7:[x]§(y):
;; 
;; Interface:
;;   if (x : command, y : command[0..*]) : void
;; 
;; Description:
;;   Performs the sequence of zero or more statements {y} once if the
;;   predicate {x} is satisfied. Otherwise, skips this block.
;; 
;; Side effects:
;;   - No intrinsic side effects govern the operation.
;;   - Any side effects inhering inside of the conditional expression
;;     {x} or any of statements in {y} propagate through this command.
;; 
;; Exceptional situations:
;;   - Any exceptional situations inhering inside of the conditional
;;     expression {x} or any of the statements in {y} propagate through
;;     this command.
;; 
;; 
;; Implementation
;; ==============
;; The Common Lisp implementation of the 8 programming language applies
;; itself in a paravaunt manner to the principles of simplicity, the
;; concessions of which embrace the in particular a neglect of security
;; in the parsing and interpretation process.
;; 
;; The employment of a lexer, parser and interpreter constitutes the
;; evaluation's basis, whence all other elements of design derive.
;; 
;; == INTERPRETATION PROCEEDS IN THREE STEPS ==
;; The interpretation proceeds in accordance with a well-studied treble
;; approach:
;;   
;;   (1) The division of a piece of 8 source code into a sequence of
;;       tokens using a lexical analyzer, or lexer.
;;   (2) The evaluation and assemblage of the token stream into an
;;       abstract syntax tree (AST) representation, a hierarchical
;;       data structure which mirrors the language constructs, provided
;;       by the efforts of a parser.
;;   (3) The traversal and evaluation of the AST using an interpreter.
;; 
;; == (1) LEXICAL ANALYZATION: SOURCE CODE BECOMES TOKENS ==
;; The incipient tier concerns itself with the production of a token
;; sequence.
;; 
;; To this end, the 8 source code, supplied in string form, will be
;; analyzed and divided into significant portions, the so-called tokens.
;; A token's diorism entails two constituents: the type, which
;; identifies the category of its nature, and a value that stores the
;; perceived datum itself.
;; 
;; The lexical analyzer, or simply lexer, accounts for the responsible
;; participant in this transformation process.
;; 
;; == (2) PARSING: TOKENS ASSEMBLE INTO A TREE STRUCTURE ==
;; Gnarity anenst the tokenized form of a program does not avail a
;; sufficient representation of the modeled language constructs for the
;; anticipated interpretation stage; thus, a transcription into a more
;; potent data structure in the form of an abstract syntax tree, or
;; abbreviated AST, issues.
;; 
;; The entity known as the parser receives from the lexer a stream of
;; tokens, which, when validated concerning their order, are assembled
;; into AST nodes, each such compact of an identifying type and a set of
;; attributes in description of the reflected language facility.
;; Resulting from this endeavor, the tree structure is prepared for the
;; ultimate interpretation tier.
;; 
;; == (3) INTERPRETATION: THE AST EXPERIENCES EVALUATION ==
;; Ensuing from the presence of the abstract syntax tree, the
;; interpreter traverses its nodes and imbues each such with an effect.
;; The processing function's return value in regard of its existence and
;; concrete manifestation constitutes a variable of the node type and
;; represented 8 instruction's variant.
;; 
;; == DATA TYPE MAPPING ==
;; An insufficiency betwixt the 8 type system and the expressiveness
;; commorant in the Common Lisp programming language coerces the
;; encapsulating representation of literal objects in the form of a
;; dedicated class.
;; 
;; In particular the ``boolean'' type as construed by Lisp introduces a
;; set of hindrances, including its manifestation in a derived type
;; rather than a class and its ``NIL'' member as an entity apportioned a
;; very expansive comprehension.
;; 
;; The ``boolean'' as a type does not permit a dispatching in the course
;; of the ``defmethod'' implementation, as merely classes or concrete
;; objects indagated by their identity may avail in this fashion. The
;; inclusion of the ``NIL'' object, on the other hand, incurs a capacity
;; for ambiguity, as the same may be used as a sentinel in a number and
;; string context.
;; 
;; Any object participating in an 8 program thus experiences its
;; representation in the form of a ``8Object'' instance, a compound
;; defined by a type and a value. The former datum acts as a surrogate
;; for a more elaborate type hierarchy which would inherit from the
;; ``8Object'' in order to distinguish in a more lucid manner the
;; entities' discrepancies. In an attempt to reduce circumambulations of
;; this ilk a generic approach has been established.
;; 
;; The following transposition shall locate the 8 data types and their
;; manifestations in the implementation in a common overview:
;; 
;;   ------------------------------------------------------------------
;;   8 data type | 8Object type | 8Object value
;;   ------------+--------------+--------------------------------------
;;   boolean     | :boolean     | A ``boolean'' value.
;;   ..................................................................
;;   number      | :number      | A ``real'' value.
;;   ..................................................................
;;   string      | :string      | A ``string'' value.
;;   ..................................................................
;;   void        | :null        | The ``NIL'' constant.
;;   ------------------------------------------------------------------
;; 
;; == UNICODE SUPPORT ==
;; The symbols naited by the 8 programming language institute the
;; imposition of Unicode support.
;; 
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-04-02
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2021_8]
;;   The Esolang contributors, "8", June 28th, 2021
;;   URL: "https://esolangs.org/wiki/8"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE."
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
  "The ``property-list-of'' type defines a property list of either no
   elements or an even tally of such, with each element at an even index
   being of the KEY-TYPE, followed by an element of the VALUE-TYPE at
   the subsequent odd index."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (evenp (length (the list object)))
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE."
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
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype access-mode ()
  "The ``access-mode'' enumerates the recognized handling policies for
   the value calculated by the '6' (binary operation) command.
   ---
   This type defines whether the binary operation, identified by the
   command number '6', shall store its result in the left operand or
   return it instead, without the variable's modification."
  '(member :store :return))

;;; -------------------------------------------------------

(deftype operator ()
  "The ``operator'' type enumerates the valid binary operators utilized
   by the command '6' (binary operation)."
  '(member
    :less-than
    :less-or-equal
    :equal
    :not-equal
    :greater-than
    :greater-or-equal
    :plus
    :minus
    :asterisk
    :slash
    :modulo
    :power))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list compact of zero or more
   ``Node'' instances."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype 8Object-type ()
  "The ``8Object-type'' type enumerates the recognized species of
   objects participating in an 8 program."
  '(member
    :boolean
    :null
    :number
    :string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class models a significant portion extracted from a
   piece of 8 source code."
  (type  (error "Invalid token type.") :type keyword)
  (value NIL                           :type T))

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

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline #\Return)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER represents a constituent fitten for an
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (alphanumericp character)
          (char= character #\_))))))

;;; -------------------------------------------------------

(defun operator-character-p (character)
  "Checks whether the CHARACTER represents an operator or a constituent
   of such, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (find character "=!<>%^" :test #'char=)))))

;;; -------------------------------------------------------

(defun command-marker-character-p (character)
  "Checks whether the CHARACTER represents an entity succeeding a
   command identifier number as its terminating sentinel, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (find character ":;" :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of 8 source code to analyze.")
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
                    SOURCE. If ``NIL'', the SOURCE has been
                    exhausted."))
  (:documentation
    "The ``Lexer'' class applies itself to the production of tokens as
     representatives of an 8 program's significant constituents."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Updates the LEXER's character reference to the first character of its
   source, if possible, and returns the modified LEXER."
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
  "Moves the LEXER to the next position in its source, if possible,
   updates its current character, and returns the modified LEXER."
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

(defun lexer-skip-whitespaces (lexer)
  "Starting at the LEXER's current position, skips a series of zero or
   more adjacent whitespaces, relocates the position cursor to the first
   non-whitespace character, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Reads a number from the LEXER source, returning a token
   representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (flet
        ((read-sign (destination)
          "Potentially reads a numerical sign from the LEXER source,
           writing it, if present, to the DESTINATION, and returns no
           value."
          (declare (type string-stream destination))
          (when (and character (find character "+-" :test #'char=))
            (write-char character destination)
            (lexer-advance lexer))
          (values))
         
         (read-digits (destination)
          "Reads zero or more adjacent digits from the LEXER source,
           writes them to the DESTINATION, and returns no value."
          (declare (type string-stream destination))
          (loop while (and character (digit-char-p character)) do
            (write-char character destination)
            (lexer-advance lexer))
          (values))
         
         (command-marker-found-p ()
          "Checks whether a command marker, contingently separated from
           the current LEXER source position by zero or more
           whitespaces, has been detected, and returns on confirmation a
           ``boolean'' value of ``T'', otherwise ``NIL''."
          (lexer-skip-whitespaces lexer)
          (the boolean
            (not (null
              (and character
                   (command-marker-character-p character)))))))
      
      (let ((token-value 0)
            (token-type  :integer))
        (declare (type real    token-value))
        (declare (type keyword token-type))
        (setf token-value
          (read-from-string
            (with-output-to-string (digits)
              (declare (type string-stream digits))
              (read-sign   digits)
              (read-digits digits)
              (cond
                ((and character (char= character #\.))
                  (setf token-type :float)
                  (write-char character digits)
                  (lexer-advance lexer)
                  (read-digits digits))
                ((command-marker-found-p)
                  (setf token-type :command-id))
                (T
                  NIL)))))
        (the Token
          (make-token token-type token-value))))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Starting at the LEXER's current position, reads a string and returns
   a token representation thereof."
  (declare (type Lexer lexer))
  (lexer-advance lexer)
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :string
        (with-output-to-string (text)
          (declare (type string-stream text))
          (loop
            with nesting-level of-type (integer 0 *) = 0
            do
              (case character
                ((NIL)
                  (error "Unterminated string."))
                (#\”
                  (cond
                    ((zerop nesting-level)
                      (lexer-advance lexer)
                      (loop-finish))
                    (T
                      (decf nesting-level)
                      (write-char character text)
                      (lexer-advance lexer))))
                (#\“
                  (incf nesting-level)
                  (write-char character text)
                  (lexer-advance lexer))
                (otherwise
                  (write-char character text)
                  (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position, reads an identifier and
   returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((identifier ""))
      (declare (type string identifier))
      (setf identifier
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop while (identifier-character-p character) do
            (write-char character identifier)
            (lexer-advance lexer))))
      (the Token
        (cond
          ((string= identifier "TRUE")
            (make-token :true identifier))
          ((string= identifier "FALSE")
            (make-token :false identifier))
          (T
            (make-token :identifier identifier)))))))

;;; -------------------------------------------------------

(defun lexer-read-operator (lexer)
  "Starting at the LEXER's current position, reads an operator and
   returns"
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((identifier ""))
      (declare (type string identifier))
      (setf identifier
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop
            while (and character (operator-character-p character))
            do
              (write-char character content)
              (lexer-advance lexer))))
      (the Token
        (cond
          ((string= identifier "<")
            (make-token :less-than identifier))
          ((string= identifier "<=")
            (make-token :less-or-equal identifier))
          ((string= identifier "=")
            (make-token :assign identifier))
          ((string= identifier "==")
            (make-token :equal identifier))
          ((string= identifier "!=")
            (make-token :not-equal identifier))
          ((string= identifier ">")
            (make-token :greater-than identifier))
          ((string= identifier ">=")
            (make-token :greater-or-equal identifier))
          ((string= identifier "%")
            (make-token :modulo identifier))
          ((string= identifier "^")
            (make-token :power identifier))
          (T
            (error "Invalid operator: ~s." identifier)))))))

;;; -------------------------------------------------------

(defun lexer-digit-follows-p (lexer)
  "Checks whether the next character in the LEXER's source represents a
   decimal digit, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the boolean
      (not (null
        (and (array-in-bounds-p source (1+ position))
             (digit-char-p (char source (1+ position)))))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh instance of a token of the type ``:eof''."
  (declare (type Lexer lexer))
  
  (with-slots (character) lexer
    (declare (type (or null character) character))
    
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((char= character #\:)
          (prog1
            (make-token :colon character)
            (lexer-advance lexer)))
        
        ((char= character #\;)
          (prog1
            (make-token :semicolon character)
            (lexer-advance lexer)))
        
        ((char= character #\,)
          (prog1
            (make-token :comma character)
            (lexer-advance lexer)))
        
        ((char= character #\’)
          (prog1
            (make-token :apostrophe character)
            (lexer-advance lexer)))
        
        ((char= character #\§)
          (prog1
            (make-token :section-sign character)
            (lexer-advance lexer)))
        
        ((char= character #\[)
          (prog1
            (make-token :left-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\])
          (prog1
            (make-token :right-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\()
          (prog1
            (make-token :left-parenthesis character)
            (lexer-advance lexer)))
        
        ((char= character #\))
          (prog1
            (make-token :right-parenthesis character)
            (lexer-advance lexer)))
        
        ((operator-character-p character)
          (lexer-read-operator lexer))
        
        ((char= character #\“)
          (lexer-read-string lexer))
        
        ((char= character #\+)
          (if (lexer-digit-follows-p lexer)
            (lexer-read-number lexer)
            (prog1
              (make-token :plus character)
              (lexer-advance lexer))))
        
        ((char= character #\-)
          (if (lexer-digit-follows-p lexer)
            (lexer-read-number lexer)
            (prog1
              (make-token :minus character)
              (lexer-advance lexer))))
        
        ((char= character #\*)
          (prog1
            (make-token :asterisk character)
            (lexer-advance lexer)))
        
        ((char= character #\/)
          (prog1
            (make-token :slash character)
            (lexer-advance lexer)))
        
        ((char= character #\%)
          (prog1
            (make-token :modulo character)
            (lexer-advance lexer)))
        
        (T
          (lexer-read-identifier lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor create-node (type)))
  "The ``Node'' class encapsulates the data necessary for modeling a
   particular construct or facility in the 8 programming language."
  (type
    (error "Missing node type.")
    :type keyword)
  (attributes
    (make-hash-table :test #'eq)
    :type (hash-table-of keyword T)))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' of the TYPE, initialized with the
   INITIAL-ATTRIBUTES, supplied as a flat list, with each attribute name
   followed by its associated attribute value."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) initial-attributes))
  (let ((node (create-node type)))
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
  "Returns the attribute value associated with the ATTRIBUTE-NAME in the
   NODE, or ``NIL'' in the absence of such a correspondence."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (the T (gethash attribute-name (node-attributes node))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (new-attribute-value node attribute-name)
  "Associates the ATTRIBUTE-NAME in the NODE with the
   NEW-ATTRIBUTE-VALUE, replacing any contingent extant relations with
   the name, and returns the modified NODE."
  (declare (type T       new-attribute-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (node-attributes node))
        new-attribute-value)
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node node))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "Node(type=~s" (node-type node))
  (loop
    for attribute-name
      of-type T
      being the hash-keys in (node-attributes node)
    using
      (hash-value attribute-value)
    for first-attribute-p
      of-type boolean
      =       T
      then    NIL
    do
      (if first-attribute-p
        (format stream ", attributes=[")
        (format stream ", "))
      
      (format stream "~s=~s" attribute-name attribute-value)
    finally
      (format stream "]"))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "8Object".                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (8Object
  (:constructor make-8Object (type value)))
  "The ``8Object'' class furnishes an abstract superclass serving as a
   common base for the various concrete encapsulations of objects in the
   8 programming language.
   ---
   The resulting hierarchical class system permits a recognition and
   dispatchment on the distinct object types."
  (type  (error "Missing 8Object type.") :type keyword)
  (value NIL                             :type T))

;;; -------------------------------------------------------

(defstruct (8Boolean
  (:include     8Object)
  (:constructor make-8Boolean (value &aux (type :boolean))))
  "The ``8Boolean'' class applies itself to a Boolean truth value's
   encapsulation.")

;;; -------------------------------------------------------

(defstruct (8Number
  (:include     8Object)
  (:constructor make-8Number (value &aux (type :number))))
  "The ``8Number'' class applies itself to a numeric object's
   encapsulation.")

;;; -------------------------------------------------------

(defstruct (8String
  (:include     8Object)
  (:constructor make-8String (value &aux (type :string))))
  "The ``8String'' class applies itself to a string object's
   encapsulation.")

;;; -------------------------------------------------------

(defstruct (8Null
  (:include     8Object)
  (:constructor make-8Null (&aux (type :null))))
  "The ``8Null'' class applies itself to an empty or nil object's
   encapsulation.")

;;; -------------------------------------------------------

(defgeneric print-8Object (8object &optional stream)
  (:documentation
    "Writes the 8OBJECT in a nait form to the STREAM and returns for a
     non-``NIL'' value ``NIL'', otherwise responding with a fresh string
     comprehending the output."))

;;; -------------------------------------------------------

(defmethod print-8Object ((boolean 8Boolean) &optional (stream T))
  (declare (type 8Boolean    boolean))
  (declare (type destination stream))
  (format stream "~:[FALSE~;TRUE~]" (8Object-value boolean)))

;;; -------------------------------------------------------

(defmethod print-8Object ((number 8Number) &optional (stream T))
  (declare (type 8Number     number))
  (declare (type destination stream))
  (format stream "~a" (8Object-value number)))

;;; -------------------------------------------------------

(defmethod print-8Object ((string 8String) &optional (stream T))
  (declare (type 8String     string))
  (declare (type destination stream))
  (format stream "~a" (8Object-value string)))

;;; -------------------------------------------------------

(defmethod print-8Object ((8null 8Null) &optional (stream T))
  (declare (type   8Null       8null))
  (declare (type   destination stream))
  (declare (ignore             8null))
  (format stream "null"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type 8Boolean +TRUE+))
(declaim (type 8Boolean +FALSE+))
(declaim (type 8Null    +VOID+))

;;; -------------------------------------------------------

(defparameter +TRUE+
  (make-8Boolean T)
  "The 8 Boolean 'true' value.")

(defparameter +FALSE+
  (make-8Boolean NIL)
  "The 8 Boolean 'false' value.")

(defparameter +VOID+
  (make-8Null)
  "The 8 'void' type.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) Node)      parser-parse-expression))
(declaim (ftype (function (Parser) node-list) parser-parse-command-list))

;;; -------------------------------------------------------

(defun operator-token-p (token)
  "Checks whether the TOKEN represents an operator, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (typep (token-type token) 'operator)))))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing parser lexer.")
    :type          Lexer
    :documentation "The lexer employed for the token purveyance to the
                    parser.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recent token obtained from the LEXER."))
  (:documentation
    "The ``Parser'' class provides a unit responsible for the assemblage
     of an abstract syntax tree (AST) from a lexer's supplied tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Queries and stores the first token from the PARSER's internally
   managed lexer and returns the modified PARSER."
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which receives its tokens from
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation loading the next token into the
   PARSER, otherwise signaling an error."
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

(defun parser-parse-boolean (parser)
  "Parses a Boolean value using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Node
      (case (token-type current-token)
        (:false
          (parser-eat parser :false)
          (make-node :boolean :value (make-8Boolean NIL)))
        (:true
          (parser-eat parser :true)
          (make-node :boolean :value (make-8Boolean T)))
        (otherwise
          (error "Invalid Boolean token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-number (parser)
  "Parses a number using the PARSER and returns a node representation
   thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((number current-token))
      (declare (type Token number))
      (parser-eat parser (token-type number))
      (the Node
        (make-node :number
          :value (make-8Number (token-value number)))))))

;;; -------------------------------------------------------

(defun parser-parse-string (parser)
  "Parses a string using the PARSER and returns a node representation
   thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Node
      (prog1
        (make-node :string
          :value (make-8String (token-value current-token)))
        (parser-eat parser :string)))))

;;; -------------------------------------------------------

;; identifier := letter , { letter | digit | "_" } ;
(defun parser-parse-variable (parser)
  "Parses a variable identifier using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((identifier current-token))
      (declare (type Token identifier))
      (parser-eat parser :identifier)
      (the Node
        (make-node :variable :name (token-value identifier))))))

;;; -------------------------------------------------------

;; haltExecution := "0::"
(defun parser-parse-halt (parser)
  "Parses a halt command using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :colon)
  (parser-eat parser :colon)
  (the Node (make-node :halt)))

;;; -------------------------------------------------------

;; print := "1:" , "[" , expression , "]" , ":"
;;       |  "1;" , "[",  expression , "]" , ";" ;
(defun parser-parse-print (parser)
  "Parses a print command using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (let ((argument        NIL)
          (print-newline-p NIL)
          (separator       NIL))
      (declare (type (or null Node)    argument))
      (declare (type boolean           print-newline-p))
      (declare (type (or null keyword) separator))
      
      (case (token-type current-token)
        (:colon
          (setf print-newline-p T)
          (setf separator       :colon))
        (:semicolon
          (setf print-newline-p NIL)
          (setf separator       :semicolon))
        (otherwise
          (error "Expected a colon or semicolon, but encountered ~s."
            current-token)))
      
      (parser-eat parser separator)
      (parser-eat parser :left-bracket)
      (setf argument (parser-parse-expression parser))
      (parser-eat parser :right-bracket)
      (parser-eat parser separator)
      
      (the Node
        (make-node :print
          :argument        argument
          :print-newline-p print-newline-p)))))

;;; -------------------------------------------------------

;; setVariable := "2:[x]<[y]:"
(defun parser-parse-set-variable-value (parser)
  "Parses a variable value modification using the PARSER and returns a
   node representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :colon)
    (parser-eat parser :left-bracket)
    (let ((identifier (parser-parse-variable parser)))
      (declare (type Node identifier))
      (parser-eat parser :right-bracket)
      (parser-eat parser :less-than)
      (parser-eat parser :left-bracket)
      (let ((new-value (parser-parse-expression parser)))
        (declare (type Node new-value))
        (parser-eat parser :right-bracket)
        (parser-eat parser :colon)
        (the Node
          (make-node :set-variable
            :variable identifier
            :value    new-value))))))

;;; -------------------------------------------------------

;; getVariableValue := "3:[x]:"
(defun parser-parse-get-variable-value (parser)
  "Parses a variable value query command using the PARSER and returns a
   node representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :colon)
    (parser-eat parser :left-bracket)
    (let ((identifier (parser-parse-variable parser)))
      (declare (type Node identifier))
      (parser-eat parser :right-bracket)
      (parser-eat parser :colon)
      (the Node (make-node :query-variable :variable identifier)))))

;;; -------------------------------------------------------

;; input := "4:[x]:"
(defun parser-parse-input (parser)
  "Parses an input command using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :colon)
    (parser-eat parser :left-bracket)
    (let ((identifier (parser-parse-variable parser)))
      (declare (type Node identifier))
      (parser-eat parser :right-bracket)
      (parser-eat parser :colon)
      (the Node (make-node :input :variable identifier)))))

;;; -------------------------------------------------------

;; whileLoop := "5:[x];(y):"
(defun parser-parse-while-loop (parser)
  "Parses a while loop command using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (parser-eat parser :colon)
    (parser-eat parser :left-bracket)
    
    (let ((condition (parser-parse-expression parser)))
      (declare (type Node condition))
      
      (parser-eat parser :right-bracket)
      (parser-eat parser :semicolon)
      (parser-eat parser :left-parenthesis)
      
      (let ((body (parser-parse-command-list parser)))
        (declare (type (list-of Node) body))
        (parser-eat parser :right-parenthesis)
        (parser-eat parser :colon)
        
        (the Node
          (make-node :while-loop
            :condition condition
            :body      body))))))

;;; -------------------------------------------------------

;; binaryOperation := 6:[x]’[op]’[y]:
;;                 |  6:[x],[op],[y]:
(defun parser-parse-binary-operation (parser)
  "Parses a binary operation command using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    
    (let ((left-operand  NIL)
          (operator      NIL)
          (right-operand NIL)
          (access-mode   :store)
          (separator     :apostrophe))
      (declare (type (or null Node)              left-operand))
      (declare (type (or null operator)          operator))
      (declare (type (or null Node)              right-operand))
      (declare (type access-mode                 access-mode))
      (declare (type (member :apostrophe :comma) separator))
      
      (parser-eat parser :colon)
      
      (parser-eat parser :left-bracket)
      (setf left-operand (parser-parse-expression parser))
      (parser-eat parser :right-bracket)
      
      (case (token-type current-token)
        (:apostrophe
          (setf access-mode :store)
          (setf separator   :apostrophe))
        (:comma
          (setf access-mode :return)
          (setf separator   :comma))
        (otherwise
          (error "Invalid separator token in command 6: ~s."
            current-token)))
      
      (parser-eat parser separator)
      
      (parser-eat parser :left-bracket)
      (if (operator-token-p current-token)
        (setf operator (token-type current-token))
        (error "Invalid operator token: ~s." current-token))
      (parser-eat parser (token-type current-token))
      (parser-eat parser :right-bracket)
      
      (parser-eat parser separator)
      
      (parser-eat parser :left-bracket)
      (setf right-operand (parser-parse-expression parser))
      (parser-eat parser :right-bracket)
      
      (parser-eat parser :colon)
      
      (the Node
        (make-node :binary-operation
          :left-operand  left-operand
          :operator      operator
          :right-operand right-operand
          :access-mode   access-mode)))))

;;; -------------------------------------------------------

;; if := "7:[x]§(y):"
(defun parser-parse-if (parser)
  "Parses a conditional if command using the PARSER and returns a node
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (parser-eat parser :colon)
    (parser-eat parser :left-bracket)
    
    (let ((condition (parser-parse-expression parser)))
      (declare (type Node condition))
      
      (parser-eat parser :right-bracket)
      (parser-eat parser :section-sign)
      (parser-eat parser :left-parenthesis)
      
      (let ((body (parser-parse-command-list parser)))
        (declare (type (list-of Node) body))
        (parser-eat parser :right-parenthesis)
        (parser-eat parser :colon)
        
        (the Node
          (make-node :if
            :condition condition
            :body      body))))))

;;; -------------------------------------------------------

(defun parser-parse-expression (parser)
  "Parses an expressiong employing the PARSER and returns a ``Node''
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Node
      (case (token-type current-token)
        (:command-id
          (let ((command-id current-token))
           (declare (type Token command-id))
           (parser-eat parser :command-id)
           (case (token-value command-id)
             (3
               (parser-parse-get-variable-value parser))
             (6
               (parser-parse-binary-operation parser))
             (otherwise
               (error "No expression token: ~s." current-token)))))
        ((:true :false)
          (parser-parse-boolean parser))
        ((:integer :float)
          (parser-parse-number parser))
        (:string
          (parser-parse-string parser))
        (:identifier
          (parser-parse-variable parser))
        (otherwise
          (error "Invalid expression token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-command (parser)
  "Parses a command using the PARSER and returns a node representation
   thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((command-id current-token))
      (declare (type Token command-id))
      (parser-eat parser :command-id)
      (the Node
        (case (token-value command-id)
          (1 (parser-parse-print              parser))
          (2 (parser-parse-set-variable-value parser))
          (3 (parser-parse-get-variable-value parser))
          (4 (parser-parse-input              parser))
          (5 (parser-parse-while-loop         parser))
          (6 (parser-parse-binary-operation   parser))
          (7 (parser-parse-if                 parser))
          (0 (parser-parse-halt               parser))
          (otherwise
            (error "Invalid command identifier token: ~s."
              command-id)))))))

;;; -------------------------------------------------------

(defun parser-parse-command-list (parser)
  "Parses a sequence of zero or more commands using the PARSER and
   returns a list containing their node reperesentations."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the node-list
      (loop
        while   (token-type-p current-token :command-id)
        collect (parser-parse-command parser)
        into    commands
        finally (return commands)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the program represented by the tokens supplied via the
   PARSER's internally managed lexer and returns the root node of the
   abstract syntax tree (AST)."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Node
      (make-node :program
        :statements (parser-parse-command-list parser)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Halt".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (type Halt-Condition condition))
      (declare (type destination    stream))
      (declare (ignore              condition))
      (format stream "The program shall be halted.")))
  (:documentation
    "The ``Halt-Condition'' serves to signal that an 8 program has
     encountered a halting instruction ('0::'), thus acting according
     to the notion of an interrupt."))

;;; -------------------------------------------------------

(defun signal-halt-condition ()
  "Signals a ``Halt-Condition''."
  (signal 'Halt-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of error "Missing-Variable-Error".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Missing-Variable-Error (error)
  ((missing-identifier
    :initarg       :missing-identifier
    :initform      (error "No missing identifier specified.")
    :reader        missing-variable-missing-identifier
    :type          string
    :documentation "The name of the irretrievable variable."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Variable-Error condition))
      (declare (type destination            stream))
      (format stream "The variable name ~s is undefined."
        (missing-variable-missing-identifier condition))))
  (:documentation
    "The ``Missing-Variable'' condition signals the absence of an
     identifier expected to be associated with a variable."))

;;; -------------------------------------------------------

(defun signal-missing-variable-error (variable-name)
  "Signals a ``Missing-Variable-Error'' related to the absent
   VARIABLE-NAME."
  (declare (type string variable-name))
  (error 'Missing-Variable-Error :missing-identifier variable-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of error "Illegal-Argument-Error".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Illegal-Argument-Error (error)
  ((offending-object
    :initarg       :offending-object
    :initform      (error "No offending object defined.")
    :reader        illegal-argument-error-offending-object
    :type          T
    :documentation ""))
  (:report
    (lambda (condition stream)
      (declare (type Illegal-Argument-Error condition))
      (declare (type destination            stream))
      (format stream "The parameter ~s is invalid in this context."
        (illegal-argument-error-offending-object condition))))
  (:documentation
    "The ``Illegal-Argument-Error'' condition avails in signaling that
     a datum in the context of its employment violates the program's
     expectation."))

;;; -------------------------------------------------------

(defun signal-illegal-argument-error (offending-object)
  "Signals an ``Illegal-Argument-Error'' based uon the
   OFFENDING-OBJECT's. abuse"
  (declare (type T offending-object))
  (error 'Illegal-Argument-Error :offending-object offending-object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Reference".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Reference
  (:constructor make-reference (name)))
  "The ``Reference'' class encapsulates a reference to a variable or
   constant, identified by its name."
  (name (error "Missing reference name.") :type string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operation handlers.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-operator (left operator right)
  (:documentation
    "Applies the OPERATOR to the LEFT operand and RIGHT operand and
     returns the result.
     ---
     As an operator's functioning in both its intrinsics and results
     defines a variable of its operands, this function's output depends
     on the combination of its parameters."))

;;; -------------------------------------------------------

(defmacro define-binary-operation ((left-operand-type
                                    operator-type
                                    right-operand-type)
                                   &body body)
  "Defines a ``defmethod'' implementation of the ``apply-operation''
   generic function with a first argument named LEFT and resolving to
   the LEFT-OPERAND-TYPE, a second argument named OPERATOR equal to the
   OPERATOR-TYPE, and a third argument named RIGHT of the
   RIGHT-OPERAND-TYPE, composed of the BODY statements, which returns
   the last evaluated BODY form.
   ---
   The expanded macro thus resolves to the following:
     (defmethod apply-operator ((left     LEFT-OPERAND-TYPE)
                                (operator (eql OPERATOR-TYPE))
                                (right    RIGHT-OPERATOR-TYPE))
       BODY)
   ---
   Please note that the symbols ``left'', ``operator'', and ``right''
   are implicitly bound in the scope of the BODY."
  `(defmethod apply-operator ((left     ,left-operand-type)
                              (operator (eql ,operator-type))
                              (right    ,right-operand-type))
     (declare (type ,left-operand-type  left))
     (declare (type keyword             operator))
     (declare (type ,right-operand-type right))
     (declare (ignorable                left))
     (declare (ignorable                operator))
     (declare (ignorable                right))
     ,@body))

;;; -------------------------------------------------------

(define-binary-operation (8Boolean :plus 8Boolean)
  "Performs a logical OR combination of the LEFT operand and the RIGHT
   operand, and returns a new ``8Boolean'' containing the result."
  (the 8Boolean
    (make-8Boolean
      (not (null
        (or (8Object-value left)
            (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :plus 8Number)
  "Increments the LEFT operand by the RIGHT operand, and returns a new
   ``8Number'' containing the result."
  (the 8Number
    (make-8Number
      (+ (8Object-value left)
         (8Object-value right)))))

;;; -------------------------------------------------------

(define-binary-operation (8Object :plus 8String)
  "Concatenates the string representation of the LEFT operand, obtained
   via an invocation of its ``print-8Object'' method, to the RIGHT
   string operand and returns a new ``8String'' containing the result."
  (the 8String
    (make-8String
      (with-output-to-string (content)
        (declare (type string-stream content))
        (print-8Object left  content)
        (print-8Object right content)))))

;;; -------------------------------------------------------

(define-binary-operation (8String :plus 8Object)
  "Concatenates the RIGHT string operand to string representation of the
  RIGHT operand, obtained via an invocation of its ``print-8Object''
  method, and returns a new ``8String'' containing the result."
  (the 8String
    (make-8String
      (with-output-to-string (content)
        (declare (type string-stream content))
        (print-8Object left  content)
        (print-8Object right content)))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :minus 8Number)
  "Decrements the LEFT operand by the RIGHT operand and returns a new
   ``8Number'' containing the difference."
  (the 8Number
    (make-8Number
      (- (8Object-value left)
         (8Object-value right)))))


;;; -------------------------------------------------------

(define-binary-operation (8Boolean :asterisk 8Boolean)
  "Performs a logical AND combination of the LEFT operand and the RIGHT
   operand, and returns a new ``8Boolean'' containing the result."
  (the 8Boolean
    (make-8Boolean
      (not (null
        (and (8Object-value left)
             (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :asterisk 8Number)
  "Multiplies the LEFT operand by the RIGHT operand and returns a new
   ``8Number'' containing the product."
  (the 8Number
    (make-8Number
      (* (8Object-value left)
         (8Object-value right)))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :asterisk 8String)
  "Repeats the RIGHT operand string a tally of times determined by
   reckoning the absolute value of the rounded LEFT operand number and
   returns a new ``8String'' containing the result."
  (the 8String
    (make-8String
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop repeat (abs (round (8Object-value left))) do
          (print-8Object right content))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :asterisk 8Number)
  "Repeats the LEFT operand string a tally of times determined by
   reckoning the absolute value of the rounded RIGHT operand number and
   returns a new ``8String'' containing the result."
  (the 8String
    (make-8String
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop repeat (abs (round (8Object-value right))) do
          (print-8Object left content))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :slash 8Number)
  "Divides the numeric LEFT operand by the numeric RIGHT operand and
   returns a new ``8Number'' containing the quotient."
  (the 8Number
    (make-8Number
      (float
        (/ (8Object-value left)
           (8Object-value right))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :modulo 8Number)
  "Divides the LEFT operand by the RIGHT operand and returns a new
   ``8Number'' containing the division remainder."
  (the 8Number
    (make-8Number
      (mod (8Object-value left)
           (8Object-value right)))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :power 8Number)
  "Raises the LEFT operand as the base to the power of the RIGHT operand
   as its exponent and returns a new ``8Number'' containing the
   exponentiation result.
   ---
   Please note that, by utilizing a fractional RIGHT operand, this
   function is capable of representing the root function to an arbitrary
   exponent."
  (the 8Number
    (make-8Number
      (expt (8Object-value left)
            (8Object-value right)))))

;;; -------------------------------------------------------

;; Logical equivalence P <-> Q.
(define-binary-operation (8Boolean :equal 8Boolean)
  "Performs a logical equivalence by combining the LEFT operand and the
   RIGHT operand, and returns a new ``8Boolean'' containing the result."
  (the 8Boolean
    (make-8Boolean
      (not (null
        (eq (8Object-value left)
            (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :equal 8Number)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (= (8Object-value left)
           (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :equal 8String)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (string= (8Object-value left)
                 (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Object :equal 8Object)
  (the 8Boolean
    (make-8Boolean NIL)))

;;; -------------------------------------------------------

;; Exclusive OR (= XOR).
(define-binary-operation (8Boolean :not-equal 8Boolean)
  "Performs an exclusive OR (XOR) combination of the LEFT operand and
   the RIGHT operand and returns a new ``8Boolean'' containing the
   result."
  (the 8Boolean
    (make-8Boolean
      (not (null
        (not
          (eq (8Object-value left)
              (8Object-value right))))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :not-equal 8Number)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (/= (8Object-value left)
            (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :not-equal 8String)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (string/= (8Object-value left)
                  (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Object :not-equal 8Object)
  (the 8Boolean
    (make-8Boolean T)))

;;; -------------------------------------------------------

;;  p | q | (not p) and q
;; ---+---+--------------
;;  T | T | F
;;  T | F | F
;;  F | T | T
;;  F | F | F
(define-binary-operation (8Boolean :less-than 8Boolean)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (and (not (8Object-value left))
             (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :less-than 8Number)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (< (8Object-value left)
           (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :less-than 8String)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (string< (8Object-value left)
                 (8Object-value right)))))))

;;; -------------------------------------------------------

;; Logical implication.
(define-binary-operation (8Boolean :less-or-equal 8Boolean)
  "Performs a material implication by combining the LEFT operand and the
   RIGHT operand and stores the result in a new ``8Boolean'', which is
   returned."
  (the 8Boolean
    (make-8Boolean
      (not
        (and (8Object-value left)
             (not (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :less-or-equal 8Number)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (<= (8Object-value left)
            (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :less-or-equal 8String)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (string<= (8Object-value left)
                  (8Object-value right)))))))

;;; -------------------------------------------------------

;; Complement of the logical implication p -> q:
;; 
;;  p | q | p and (not q) | p -> q
;; ---+---+---------------+-------
;;  T | T | F             | T
;;  T | F | T             | F
;;  F | T | F             | T
;;  F | F | F             | F
(define-binary-operation (8Boolean :greater-than 8Boolean)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (and (8Object-value left)
             (not (8Object-value right))))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :greater-than 8Number)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (> (8Object-value left)
           (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :greater-than 8String)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (string> (8Object-value left)
                 (8Object-value right)))))))

;;; -------------------------------------------------------

;;  p | q | (not ((not p) and q))
;; ---+---+----------------------
;;  T | T | T
;;  T | F | T
;;  F | T | F
;;  F | F | T
(define-binary-operation (8Boolean :greater-or-equal 8Boolean)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (not (and (not (8Object-value left))
                  (8Object-value right))))))))

;;; -------------------------------------------------------

(define-binary-operation (8Number :greater-or-equal 8Number)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (>= (8Object-value left)
            (8Object-value right)))))))

;;; -------------------------------------------------------

(define-binary-operation (8String :greater-or-equal 8String)
  (the 8Boolean
    (make-8Boolean
      (not (null
        (string>= (8Object-value left)
                  (8Object-value right)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Visitor".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Visitor ()
  ()
  (:documentation
    "The ``Visitor'' interface establishes a common base for objects
     operating on an abstract syntax tree (AST) in order to evaluate
     its nodes."))

;;; -------------------------------------------------------

(defgeneric dispatch-node (visitor node-type node)
  (:documentation
    "Processes the NODE identified by the NODE-TYPE using the VISITOR,
     returning a value appropriate for the concrete circumstance."))

;;; -------------------------------------------------------

(defun visit-node (visitor node)
  "Processes the NODE using the VISITOR and returns a result appropriate
   for the concrete circumstance."
  (declare (type Visitor visitor))
  (declare (type Node    node))
  (the T (dispatch-node visitor (node-type node) node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-constant-table ()
  "Creates, populates and returns a table associating the constant names
   with the respective objects."
  (let ((constants (make-hash-table :test #'equal)))
    (declare (type (hash-table-of string 8Object) constants))
    (setf (gethash "FALSE" constants) +FALSE+)
    (setf (gethash "TRUE"  constants) +TRUE+)
    (the (hash-table-of string 8Object) constants)))

;;; -------------------------------------------------------

(defclass Interpreter (Visitor)
  ((tree
    :initarg       :tree
    :initform      (error "Missing interpreter AST.")
    :type          Node
    :documentation "The abstract syntax tree (AST) to process.")
   (constants
    :initarg       :constants
    :initform      (build-constant-table)
    :type          (hash-table-of string 8Object)
    :documentation "Maintains the 8 language's built-in constants.")
   (variables
    :initarg       :variables
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string 8Object)
    :documentation "Maintains a program's variables.")
   (done-p
    :initarg       :done-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the program, executing in an
                    implicitly operating infinite loop, has been
                    terminated by an encounter with a halt command
                    ('0::')."))
  (:documentation
    "The ``Interpreter'' class endows an abstract syntax tree (AST) with
     an effect."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a new ``Interpreter'' operating on the abstract
   syntax TREE."
  (declare (type Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun constant-name-p (interpreter identifier)
  "Determines whether the IDENTIFIER refers to a 8 language constant as
   defined by the INTERPRETER, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type string      identifier))
  (the boolean
    (not (null
      (gethash identifier
        (slot-value interpreter 'constants))))))

;;; -------------------------------------------------------

(defun get-constant-value (interpreter identifier)
  "Returns the constant value associated with the IDENTIFIER in the
   INTERPRETER, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type Interpreter interpreter))
  (declare (type string       identifier))
  (the 8Object
    (or (gethash identifier (slot-value interpreter 'constants))
        (error "Unrecognized constant name: ~s." identifier))))

;;; -------------------------------------------------------

(defun check-variable-name (interpreter identifier)
  "Determines whether the IDENTIFIER represents a valid variable name
   as specified by the INTERPRETER's configurations, on confirmation
   returning no value; otherwise an error of an unspecified is
   signaled."
  (declare (type Interpreter interpreter))
  (declare (type string      identifier))
  (when (constant-name-p interpreter identifier)
    (signal-illegal-argument-error identifier))
  (values))

;;; -------------------------------------------------------

(defun check-if-variable-exists (interpreter variable-name)
  "Determines whether a variable identified by the VARIABLE-NAME exists
   in the INTERPRETER, on confirmation returning no value; otherwise an
   error of the type ``Missing-Variable-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (declare (type string      variable-name))
  (multiple-value-bind (variable contains-variable-p)
      (gethash variable-name (slot-value interpreter 'variables))
    (declare (type (or null 8Object) variable))
    (declare (type T                 contains-variable-p))
    (declare (ignore                 variable))
    (unless contains-variable-p
      (signal-missing-variable-error variable-name)))
  (values))

;;; -------------------------------------------------------

(defun get-variable-value (interpreter identifier)
  "Returns the value associated with the variable IDENTIFIER in the
   INTERPRETER, or signals an error of the type
   ``Missing-Variable-Error'' upon its disrespondency."
  (check-if-variable-exists interpreter identifier)
  (the 8Object
    (gethash identifier
      (slot-value interpreter 'variables))))

;;; -------------------------------------------------------

(defun get-identifier-value (interpreter identifier)
  "Returns the value associated with the IDENTIFIER in the INTERPRETER,
   either answering to a constant or variable entry, or signals an error
   of the type ``Missing-Variable-Error'' upon its disrespondency."
  (declare (type Interpreter interpreter))
  (declare (type string      identifier))
  (the 8Object
    (if (constant-name-p interpreter identifier)
      (get-constant-value interpreter identifier)
      (get-variable-value interpreter identifier))))

;;; -------------------------------------------------------

(defun set-variable (interpreter variable-name new-value)
  "Associates the VARIBLE-NAME with the NEW-VALUE in the INTERPRETER and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type string      variable-name))
  (declare (type 8Object     new-value))
  (check-variable-name interpreter variable-name)
  (setf (gethash variable-name (slot-value interpreter 'variables))
        new-value)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun get-boolean-truth-value (object)
  "Checks whether the OBJECT represents a Boolean true value, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type 8Object object))
  (the boolean
    (and
      (not (equalp object +FALSE+))
      (not (eq (8Object-type object) :null)))))

;;; -------------------------------------------------------

(defun parse-user-input (input)
  "Parse the user INPUT and return an appropriate representation
   thereof."
  (declare (type string input))
  (the (or null 8Object)
    (cond
      ((null input)
        NIL)
      ((zerop (length input))
        (make-8String input))
      ((digit-char-p (char input 0))
        (make-8Number (read-from-string input)))
      ((find (char input 0) "+-" :test #'char=)
        (make-8Number (read-from-string input)))
      (T
        (make-8String input)))))

;;; -------------------------------------------------------

(defgeneric resolve-value (interpreter object)
  (:method ((interpreter Interpreter)
            (reference   Reference))
    "Returns the value of the variable registered at the INTERPRETER
     under the name stored in the REFERENCE."
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (the 8Object
      (get-identifier-value interpreter
        (reference-name reference))))
  
  (:method ((interpreter Interpreter)
            (8object     8Object))
    "Returns the 8OBJECT instance itself, ignoring the INTERPRETER."
    (declare (type Interpreter interpreter))
    (declare (type 8Object     8object))
    (the 8Object 8object))
  
  (:method ((interpreter Interpreter)
            (object      T))
    "Signals an error communicating the INTERPRETER and the OBJECT."
    (declare (type Interpreter interpreter))
    (declare (type T           object))
    (error "The object ~s cannot be resolved by the interpreter ~s."
      object interpreter))
  
  (:documentation
    "Returns the OBJECT's value in the INTERPRETER's context."))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :program))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  "Recognized by the NODE-TYPE, executes the program NODE's statements
   in the INTERPRETER's context and returns no value."
  (let ((statements (node-attribute node :statements)))
    (declare (type node-list statements))
    (loop do
      (dolist (statement statements)
        (declare (type Node statement))
        (visit-node interpreter statement))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :boolean))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             interpreter))
  (declare (ignore             node-type))
  (the 8Boolean
    (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :number))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             interpreter))
  (declare (ignore             node-type))
  (the 8Number
    (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :string))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             interpreter))
  (declare (ignore             node-type))
  (the 8String
    (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :variable))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((variable-name (node-attribute node :name)))
    (declare (type string variable-name))
    (the Reference
      (make-reference variable-name))))

;;; -------------------------------------------------------

;; 1:[x]:
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :print))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((argument-node   (node-attribute node :argument))
        (print-newline-p (node-attribute node :print-newline-p)))
    (declare (type Node    argument-node))
    (declare (type boolean print-newline-p))
    
    (print-8Object
      (resolve-value interpreter
        (visit-node interpreter argument-node)))
    
    (when print-newline-p
      (format T "~%")))
  
  (the 8Object +VOID+))

;;; -------------------------------------------------------

;; 2:[x]<[y]:
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :set-variable))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((variable-node (node-attribute node :variable))
        (value-node    (node-attribute node :value)))
    (declare (type Node variable-node))
    (declare (type Node value-node))
    (set-variable interpreter
      (reference-name
        (visit-node interpreter variable-node))
      (resolve-value interpreter
        (visit-node interpreter value-node))))
  (the 8Object +VOID+))

;;; -------------------------------------------------------

;; 3:[x]:
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :query-variable))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((variable-node (node-attribute node :variable)))
    (declare (type Node variable-node))
    (the 8Object
      (resolve-value interpreter
        (visit-node interpreter variable-node)))))

;;; -------------------------------------------------------

;; 4:[x]:
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :input))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((variable-node (node-attribute node :variable))
        (input         (parse-user-input (read-line))))
    (declare (type Node              variable-node))
    (declare (type (or null 8Object) input))
    (clear-input)
    (set-variable interpreter
      (reference-name
        (visit-node interpreter variable-node))
      input))
  (the 8Object +VOID+))

;;; -------------------------------------------------------

;; 5:[x]:(y):
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :while-loop))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((condition-node (node-attribute node :condition))
        (body-nodes     (node-attribute node :body)))
    (declare (type Node      condition-node))
    (declare (type node-list body-nodes))
    (loop
      while
        (get-boolean-truth-value
          (resolve-value interpreter
            (visit-node interpreter condition-node)))
      do
        (dolist (statement body-nodes)
          (declare (type Node statement))
          (visit-node interpreter statement))))
  (the 8Object +VOID+))

;;; -------------------------------------------------------

;; 6:[x]’[op]’[y]:
;; 6:[x],[op],[y]:
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :binary-operation))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((left-operand-node  (node-attribute node :left-operand))
        (operator           (node-attribute node :operator))
        (right-operand-node (node-attribute node :right-operand))
        (access-mode        (node-attribute node :access-mode)))
    (declare (type Node        left-operand-node))
    (declare (type operator    operator))
    (declare (type Node        right-operand-node))
    (declare (type access-mode access-mode))
    (case access-mode
      (:return
        (apply-operator
          (resolve-value interpreter
            (visit-node interpreter left-operand-node))
          operator
          (resolve-value interpreter
            (visit-node interpreter right-operand-node))))
      (:store
        (set-variable interpreter
          (reference-name
            (visit-node interpreter left-operand-node))
          (apply-operator
            (resolve-value interpreter
              (visit-node interpreter left-operand-node))
            operator
            (resolve-value interpreter
              (visit-node interpreter right-operand-node))))
        +VOID+)
      (otherwise
        (error "Invalid access mode: ~s." access-mode)))))

;;; -------------------------------------------------------

;; 7:[x]§(y):
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :if))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (let ((condition-node (node-attribute node :condition))
        (body-nodes     (node-attribute node :body)))
    (declare (type Node      condition-node))
    (declare (type node-list body-nodes))
    (when (get-boolean-truth-value
            (resolve-value interpreter
              (visit-node interpreter condition-node)))
      (dolist (statement body-nodes)
        (declare (type Node statement))
        (visit-node interpreter statement))))
  (the 8Object +VOID+))

;;; -------------------------------------------------------

;; 0::
(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :halt))
                          (node        Node))
  (declare (type   Interpreter interpreter))
  (declare (type   keyword     node-type))
  (declare (type   Node        node))
  (declare (ignore             node-type))
  (declare (ignore             node))
  (setf (slot-value interpreter 'done-p) T)
  (signal-halt-condition)
  (the 8Object +VOID+))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the 8 program represented by the INTERPRETER and returns
   the INTERPRETER."
  (declare (type Interpreter interpreter))
  (handler-case
    (visit-node interpreter
      (slot-value interpreter 'tree))
    
    (Halt-Condition ()
      NIL)
    (error (e)
      (error e)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-8 (code)
  "Interprets the piece of 8 CODE and returns the result of the last
   evaluated command."
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

;; Print "Hello, World!".
(interpret-8
  "1:[“Hello, World!”]:
   0::")

;;; -------------------------------------------------------

;; Concatenates "Hello, " and "World!", printing "Hello, World!".
(interpret-8
  "1:[6:[“Hello, ”],[+],[“World!”]:]:
   0::")

;;; -------------------------------------------------------

;; Produces "Hello Hello Hello" and prints the string.
(interpret-8
  "1:[6:[“Hello”],[*],[3]:]:
   0::")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-8
  "4:[input]:
   1:[3:[input]:]:
   0::")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-8
  "4:[input]:
   1:[3:[input]:]:")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which ceases if no input, that is,
;; an empty input string, is supplied.
(interpret-8
  "4:[input]:
   5:[6:[3:[input]:],[!=],[“”]:];
   (
     1:[3:[input]:]:
     4:[input]:
   ):
   0::")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-8
  "4:[inp]:
   1;[3:[inp]:];
   5:[6:[3:[inp]:],[==],[1]:];(
       1;[1];
   ):
   0::")

;;; -------------------------------------------------------

;; Create a numeric variable named "test" with an initial value of one,
;; add the quantity ten to it, and print the modified value (11).
(interpret-8
  "2:[test]<[1]:
   6:[test]’[+]’[10]:
   1:[3:[test]:]:
   0::")

;;; -------------------------------------------------------

;; Define a variable named "counter" and utilize it in a while loop for
;; printing the numbers 1 to 5.
(interpret-8
  "2:[counter]<[1]:
   
   5:[6:[3:[counter]:],[<=],[5]:];
   (
     1:[3:[counter]:]:
     6:[counter]’[+]’[1]:
   ):
   0::")

;;; -------------------------------------------------------

;; Define a variable named "counter" and utilize it in a while loop for
;; printing the numbers -5 to 5.
(interpret-8
  "2:[counter]<[-5]:
   
   5:[6:[3:[counter]:],[<=],[5]:];
   (
     1:[3:[counter]:]:
     6:[counter]’[+]’[1]:
   ):
   0::")

;;; -------------------------------------------------------

;; Queries the user for two numbers, the first supplying the counter
;; start value, the second its inclusive end state, and naits a while
;; loop to print the integers in the thus spanned range.
;; 
;; Please note that the start value must be less than or equal to the
;; end in order to succeed in printing.
(interpret-8
  "1:[“Please input the counter start value: ”]:
   4:[counterStart]:
   
   1:[“Please input the counter end value: ”]:
   4:[counterEnd]:
   
   2:[counter]<[3:[counterStart]:]:
   
   5:[6:[3:[counter]:],[<=],[3:[counterEnd]:]:];
   (
     1:[3:[counter]:]:
     6:[counter]’[+]’[1]:
   ):
   0::")
