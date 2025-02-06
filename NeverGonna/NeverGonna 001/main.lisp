;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "NeverGonna", invented by the Esolang user "DPS2004" and
;; presented on October 17th, 2019, the abode of its idiopathic element
;; is accommodated by the expression of its programs in an emulation of
;; the lyrics from the song "Never Gonna Give You Up" by Rick Astley.
;; 
;; 
;; Concept
;; =======
;; The NeverGonna programming language ostends the kenspeckle haecceity
;; of its programs' expression by fragmentary excerpts from the song
;; "Never Gonna Give You Up", operating upon a theoretically
;; inenumerable tally of variables whose capacity admits Boolean values,
;; signed integral objects, and strings of any mickleness.
;; 
;; == NEVERGONNA: A TRIBUTE TO PROGRAM IN ==
;; The linguistic specimen's agnomination, as well as its donet, is
;; desumed from the song "Never Gonna Give You Up" by the English
;; musician Rick Astley, dated in its release to July 27th, 1987.
;; 
;; == VARIABLES ENGAGE IN THE DATA CASTALDY ==
;; The aefauld stewardry in reference to the data management is realized
;; in the efforts of the variables, the enumeration of which proceeds in
;; a bourneless fashion by association with an identifier begotten by
;; one's personal deliberations.
;; 
;; All three data types commorant in the NeverGonna language, Boolean
;; truth values, integers, and strings, may be transmitted to and
;; queried from these placeholders.
;; 
;; 
;; Architecture
;; ============
;; NeverGonna's architectural department, as a consectary of its
;; reliance upon variables as the aefauld medium of memorization, levies
;; a scant mete of impositions.
;; 
;; The haecceity appertaining to such identifiers' eath agnomination,
;; admitting preponderantly Latin letters, decimal digits, and the
;; underscore, and its unambiguous association with a value from the
;; realm of Boolean truth values, signed integers, and strings, redes
;; the latreutical involvement of an associative data structure,
;; inclining towards a hashed solution.
;; 
;; 
;; Data Types
;; ==========
;; NeverGonna intrines the basic types of Boolean truth values, signed
;; integers, and strings in its data realm, the first of which serves in
;; it paravaunt appreciation to assay predicate, while the second refers
;; to the arithmetical capacities, concluding with the third species'
;; input/output intercourse.
;; 
;; == BOOLEAN TRUTH VALUES ==
;; The traditional twissel of Boolean truth values, comprised by the
;; affirmative "True" and the negative "False", partakes of the
;; language's potentials, homologating the application of logical
;; warklumes in a program's circumference.
;; 
;; == INTEGER NUMBERS: SIGNED AND UNBOUNDED ==
;; The numeric component answers to signed integers of any magnitude and
;; signum, expressed in the decimal numeral system.
;; 
;; == STRINGS: BOURNELESS CHARACTER SEQUENCES ==
;; An arbitrary tally of character entities, desumed from the ASCII
;; repertoire, may be communicated by any of two connatural
;; significaions: one requiring an ensconcement into double quotation
;; marks ('"'), the other in the singular counterpart ("'").
;; 
;; 
;; Syntax
;; ======
;; NeverGonna's syntactical peculiarities relocate the language to the
;; realm of pseudonatural specimens, composed of lyrical phrases, with
;; each "verse" contributing a single statement's participation.
;; 
;; == INSTRUCTIONS ==
;; The indicial attribute applied to any instruction differs from the
;; concrete instances, but always presents an emulation of natural
;; language.
;; 
;; Arguments as a contingent requisitum are inserted at designated
;; locations, maintaining a lealty to the idiopathic case as much as the
;; syntaxis' exposition.
;; 
;; == ARGUMENTS: INTEGERS, STRINGS, AND VARAIBLES ==
;; The arguments' presence intrines integer and string literals in
;; conjunction with variables.
;; 
;; == INTEGERS ==
;; The expression of integers obeys to the decimal notation.
;; 
;; == STRINGS ==
;; Strings literals may be expressed in one of a twain of slight
;; variations in embracing the either double quotation marks ('"') for
;; their content's delineation, or the single-quoted alternative ("'").
;; 
;; == VARIABLES ==
;; Variable identifiers follow the design of one or or more Latin
;; letters, decimal digits, and underscore ("_") symbols.
;; 
;; == WHITESPACES ==
;; Spaces, a terminology superimposed upon the traditional space and the
;; horizontal tab, except for their dever in the segregation of tokens,
;; proffer warklumes loaned from callotechnics rather than urgencies,
;; and thus homologate their distribution at any spatial and
;; quantitative encheson.
;; 
;; Newlines, on the other hand, are infused with a discriminating onus
;; in their separation of statements; as such, their agency must be
;; encountered with respect, while supererogative instances retain the
;; program's patience.
;; 
;; == COMMENTS ==
;; NeverGonna's provisions includes a single type of comments, such
;; expand across the rest of the line. Proceeding from a sequence of
;; two accolent hyphens, "--", all subsequent content until the next
;; linebreak or the end of the source is construed as an apostille.
;; 
;; Concretely, the following weftage's imposition holds:
;; 
;;   -- COMMENT TEXT
;;      ************
;; 
;; where the {COMMENT TEXT} extends to the commencing line's dextral
;; bourne.
;; 
;; Nesting, as well as premature cessation, eludes this facility's
;; competences.
;; 
;; == GRAMMAR ==
;; A formulation of the language's donet in accordance with the Extended
;; Backus-Naur Form (EBNF) regulations shall be adhibited as a vehicle
;; for a more stringent approach to the construction:
;;   
;;   program           := { innerEmptyLine }
;;                     ,  { innerCommandLine | innerEmptyLine }
;;                     ,  [ lastCommandLine  | lastEmptyLine ]
;;                     ;
;;   
;;   innerEmptyLine    := [ comment ] , newlines ;
;;   lastEmptyLine     := [ comment ] ;
;;   
;;   innerCommandLine  := statement , [ comment ] , newlines ;
;;   lastCommandLine   := statement , [ comment ] ;
;;   
;;   comment           := "--" , { character - newline } ;
;;   
;;   statementList     := { statementListItem } ;
;;   statementListItem := { innerEmptyLine    }
;;                     ,  [ innerCommandLine  ]
;;                     ,  { innerEmptyLine    }
;;                     ;
;;   statement         := varDeclaration
;;                     |  varAssignment
;;                     |  inputExpression
;;                     |  printCommand
;;                     |  ifStatement
;;                     |  forLoop
;;                     |  whileLoop
;;                     ;
;;   
;;   varDeclaration    := "we're no strangers to" , varName ;
;;   varAssignment     := "gotta make" , varName , expression ;
;;   
;;   printCommand      := "i just wanna tell you" , expression ;
;;   
;;   ifStatement       := ifThenPart
;;                     ,  newlines
;;                     ,  { elseIfPart }
;;                     ,  [ elsePart ]
;;                     ,  endOfStatement
;;                     ;
;;   ifThenPart        := "inside we both know" , expression , "then"
;;                     ,  newlines
;;                     ,  statementList
;;                     ;
;;   elseIfPart        := "never gonna turn around" , expression
;;                     ,  "then"
;;                     ,  newlines
;;                     ,  statementList
;;                     ;
;;   elsePart          := "never gonna let you down"
;;                     ,  newlines
;;                     ,  statmentList
;;                     ;
;;   
;;   forLoop           := "we've known" , varName , "for" , expression
;;                     ,  newlines
;;                     ,  statementList
;;                     ,  newlines
;;                     ,  endOfStatement
;;                     ;
;;   
;;   whileLoop         := "a full commitment's what I'm thinking of"
;;                     ,  expression
;;                     ,  newlines
;;                     ,  statementList
;;                     ,  newlines
;;                     ,  endOfStatement
;;                     ;
;;   
;;   endOfStatement    := "never gonna give you up" ;
;;   
;;   expression        := booleanValue
;;                     |  number
;;                     |  singleQuoteString
;;                     |  doubleQuoteString
;;                     |  varName
;;                     |  inputExpression
;;                     |  unaryOperator , expression
;;                     |  expression , binaryOperator   , expression
;;                     |  expression , relationOperator , expression
;;                     |  "(" , expression , ")"
;;                     ;
;;   inputExpression   := "your heart's been aching but you're too shy to say"
;;                     ,  expression
;;                     ;
;;   unaryOperator     := "+" | "-" | "!" ;
;;   binaryOperator    := "+" | "-" | "*" | "/" | "%" | "^"
;;                     |  "&&" | "||"
;;                     ;
;;   relationOperator  := "==" | "!=" | "<" | "<=" | ">" | ">=" ;
;;   
;;   varName           := varNameCharacter , { varNameCharacter } ;
;;   varNameCharacter  := "a" | ... | "z" | "A" | ... | "Z"
;;                     |  "_"
;;                     |  digit
;;                     ;
;;   booleanValue      := "False" | "True" ;
;;   singleQuoteString := "'" , { character - "'" } , "'" ;
;;   doubleQuoteString := '"' , { character - '"' } , '"' ;
;;   number            := [ "+" | "-" ] , digit , { digit } ;
;;   digit             := "0" | "1" | "2" | "3" | "4"
;;                     |  "5" | "6" | "7" | "8" | "9"
;;                     ;
;;   newline           := "\n" ;
;;   newlines          := newline , { newline } ;
;;   optionalNewlines  := { newline } ;
;; 
;; 
;; Instructions
;; ============
;; NeverGonna's instruction set tallies a septuple cardinality, the
;; circumference of which enumerates a variable management, input and
;; output conduits, a conditional "if" construct, as well as the twissel
;; of iterance mechanisms realized in a "for" counter loop and a "while"
;; iteration.
;; 
;; == OVERVIEW ==
;; An inchoate grade of nortelry shall be communicated by the following
;; tabular exposition, ere further details accrue the reader's gnarity
;; with the operative competences.
;; 
;;   +-----------------------------------------------------------------+
;;   | we're no strangers to {name}                                    |
;;   |-----------------------------------------------------------------|
;;   | Declares a new variable designated by the {name}.               |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | gotta make {name} {value}                                       |
;;   |-----------------------------------------------------------------|
;;   | Assigns the {value} to the variable designated by the {name}.   |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | your heart's been aching but you're too shy to say {prompt}     |
;;   |-----------------------------------------------------------------|
;;   | Queries for and returns an input presenting the {prompt} text.  |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | i just wanna tell you {argument}                                |
;;   |-----------------------------------------------------------------|
;;   | Prints the {argument}.                                          |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | inside we both know {thenGuard} then                            |
;;   |   {thenStatements}                                              |
;;   | never gonna give you up                                         |
;;   |-----------------------------------------------------------------|
;;   | Executes the {thenStatements} if the {thenGuard} is satisfied.  |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | inside we both know {thenGuard} then                            |
;;   |   {thenStatements}                                              |
;;   | never gonna let you down                                        |
;;   |   {elseStatements}                                              |
;;   | never gonna give you up                                         |
;;   |-----------------------------------------------------------------|
;;   | Executes the {thenStatements} if the {guard} is satisfied,      |
;;   | otherwise executes the {elseStatements}.                        |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | inside we both know {thenGuard} then                            |
;;   |   {thenStatements}                                              |
;;   | never gonna turn around {elseIfGuard_1} then                    |
;;   |   {elseIfStatements_1}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_i} then                    |
;;   |   {elseIfStatements_i}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_N} then                    |
;;   |   {elseIfStatements_N}                                          |
;;   | never gonna give you up                                         |
;;   |-----------------------------------------------------------------|
;;   | Executes the {thenStatements} if the {guard} is satisfied,      |
;;   | otherwise probes in this order the {elseIfGuard1} through       |
;;   | {elseIfGuard_N} predicates, performing the first eligible       |
;;   | specimen's {elseIfStatements_i}, or, if none matches,           |
;;   | accompasses no effect.                                          |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | inside we both know {thenGuard} then                            |
;;   |   {thenStatements}                                              |
;;   | never gonna turn around {elseIfGuard_1} then                    |
;;   |   {elseIfStatements_1}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_i} then                    |
;;   |   {elseIfStatements_i}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_N} then                    |
;;   |   {elseIfStatements_N}                                          |
;;   | never gonna let you down                                        |
;;   |   {elseStatements}                                              |
;;   | never gonna give you up                                         |
;;   |-----------------------------------------------------------------|
;;   | Executes the {thenStatements} if the {guard} is satisfied,      |
;;   | otherwise probes in this order the {elseIfGuard1} through       |
;;   | {elseIfGuard_N} predicates, performing the first eligible       |
;;   | specimen's {elseIfStatements_i}, or, if none matches, executes  |
;;   | the {elseStatements}.                                           |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | we've known {counterName} for {repetitionCount}                 |
;;   |   {statements}                                                  |
;;   | never gonna give you up                                         |
;;   |-----------------------------------------------------------------|
;;   | Executes the {statements} a {repetitionCount} of times, while   |
;;   | incrementing the variable {counterName} from its initial value  |
;;   | on each cycle by one (1).                                       |
;;   +-----------------------------------------------------------------+
;;   +-----------------------------------------------------------------+
;;   | a full commitment's what I'm thinking of {guard}                |
;;   |   {statements}                                                  |
;;   | never gonna give you up                                         |
;;   |-----------------------------------------------------------------|
;;   | Executes the {statements} repeatedly until the {guard} is not   |
;;   | satisfied anymore.                                              |
;;   +-----------------------------------------------------------------+
;; 
;; == SPECIFICATION ==
;; A listing of the operations endowed with enhanced nimiety to its
;; details shall be the subsequent treatise's material.
;; 
;; Every instruction entry's illustration proceeds by a particular
;; forbisen, designed as a box of the following componency:
;; 
;;    _________________________________________________________________
;;   | Effect                                                          |
;;   | <<Type of form>>                                                |
;;   |=================================================================|
;;   | NeverGonna command syntax                                       |
;;   |=================================================================|
;;   | Parameters and anomalous situations descriptions.               |
;;   | --------------------------------------------------------------- |
;;   | ...                                                             |
;;   |_________________________________________________________________|
;; 
;; The parasceve's establishment may now be complemented by the actual
;; specifications:
;; 
;;    _________________________________________________________________
;;   | Variable declaration                                            |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | we're no strangers to {name}                                    |
;;   |=================================================================|
;;   | Declares a new variable designated by the {name}.               |
;;   | --------------------------------------------------------------- |
;;   | The {name} must be a valid variable identifier.                 |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "DuplicateVariableError" is signaled if a  |
;;   | variable with the {name} is already declared.                   |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | Variable assignment                                             |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | gotta make {name} {value}                                       |
;;   |=================================================================|
;;   | Stores the {value} in the variable designated by the {name}.    |
;;   | --------------------------------------------------------------- |
;;   | The {name} must be a valid, already declared variable           |
;;   | identifier.                                                     |
;;   | --------------------------------------------------------------- |
;;   | The {value} must be an expression, the evaluated form of which  |
;;   | is stored in the variable with the {name}.                      |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "UnknownVariableError" is signaled if a    |
;;   | variable with the {name} is not yet declared.                   |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | Input                                                           |
;;   | <<expression>>                                                  |
;;   |=================================================================|
;;   | your heart's been aching but you're too shy to say {prompt}     |
;;   |=================================================================|
;;   | Queries the user for an input by mediation of the standard      |
;;   | input, preceded by the {prompt} message's issuance to the       |
;;   | standard output, and returns the obtained datum.                |
;;   | --------------------------------------------------------------- |
;;   | The {prompt} must be an expression, the evaluated form of which |
;;   | is naited as the prompt message to output immediately before    |
;;   | the input reception.                                            |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | Output                                                          |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | i just wanna tell you {argument}                                |
;;   |=================================================================|
;;   | Prints the {argument} to the standard output, concluded by a    |
;;   | single newline character.                                       |
;;   | --------------------------------------------------------------- |
;;   | The {argument} must an expression, the evaluated form of which  |
;;   | is issued to the standard output.                               |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | If-then                                                         |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | inside we both know {guard} then                                |
;;   |   {thenStatements}                                              |
;;   | never gonna give you up                                         |
;;   |=================================================================|
;;   | If the {guard} expression is satisfied, executes the            |
;;   | {thenStatements}, otherwise omits the entire construct.         |
;;   | --------------------------------------------------------------- |
;;   | The {guard} must be an expression whose evaluated form results  |
;;   | in a Boolean object.                                            |
;;   | --------------------------------------------------------------- |
;;   | The {thenStatements} must be a sequence of zero or more         |
;;   | statements.                                                     |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if the  |
;;   | {guard} does not resolve to a Boolean truth value.              |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | If-else                                                         |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | inside we both know {guard} then                                |
;;   |   {thenStatements}                                              |
;;   | never gonna let you down                                        |
;;   |   {elseStatements}                                              |
;;   | never gonna give you up                                         |
;;   |=================================================================|
;;   | If the {guard} expression is satisfied, executes the            |
;;   | {thenStatements}, otherwise executes the {elseStatements}.      |
;;   | --------------------------------------------------------------- |
;;   | The {guard} must be an expression whose evaluated form results  |
;;   | in a Boolean object.                                            |
;;   | --------------------------------------------------------------- |
;;   | The {thenStatements} must be a sequence of zero or more         |
;;   | statements.                                                     |
;;   | --------------------------------------------------------------- |
;;   | The {elseStatements} must be a sequence of zero or more         |
;;   | statements.                                                     |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if the  |
;;   | {guard} does not resolve to a Boolean truth value.              |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | If-elseIf                                                       |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | inside we both know {thenGuard} then                            |
;;   |   {thenStatements}                                              |
;;   | never gonna turn around {elseIfGuard_1} then                    |
;;   |   {elseIfStatements_1}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_i} then                    |
;;   |   {elseIfStatements_i}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_N} then                    |
;;   |   {elseIfStatements_N}                                          |
;;   | never gonna give you up                                         |
;;   |=================================================================|
;;   | If the {thenGuard} expression is satisfied, executes the        |
;;   | {thenStatements} and subsequently terminates the compound.      |
;;   | Otherwise probes the {elseIfGuard_1} through {elseIfGuard_n},   |
;;   | executing the first matching guard's statements, designated by  |
;;   | {elseIfStatements_1} through {elseIfStatements_N} respectively, |
;;   | while immediately concluding with a termination. If none of     |
;;   | these guards has been ascertained, not effect is accompassed.   |
;;   | --------------------------------------------------------------- |
;;   | The {thenGuard} must be an expression whose evaluated form      |
;;   | results in a Boolean object.                                    |
;;   | --------------------------------------------------------------- |
;;   | The {thenStatements} must be a sequence of zero or more         |
;;   | statements.                                                     |
;;   | --------------------------------------------------------------- |
;;   | {elseIfGuard_1} through {elseIfGuard_N} must be expressions     |
;;   | whose evaluated forms result in Boolean objects.                |
;;   | --------------------------------------------------------------- |
;;   | {elseIfStatements_1} through {elseIfStatements_N} must be       |
;;   | sequences of zero or more statements.                           |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if the  |
;;   | {thenGuard} does not resolve to a Boolean truth value.          |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if any  |
;;   | of the conditions {elseIfGuard_1} through {elseIfGuard_N} do    |
;;   | not resolve to a Boolean truth value.                           |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | If-elseIf-else                                                  |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | inside we both know {thenGuard} then                            |
;;   |   {thenStatements}                                              |
;;   | never gonna turn around {elseIfGuard_1} then                    |
;;   |   {elseIfStatements_1}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_i} then                    |
;;   |   {elseIfStatements_i}                                          |
;;   | ...                                                             |
;;   | never gonna turn around {elseIfGuard_N} then                    |
;;   |   {elseIfStatements_N}                                          |
;;   | never gonna let you down                                        |
;;   |   {elseStatements}                                              |
;;   | never gonna give you up                                         |
;;   |=================================================================|
;;   | If the {thenGuard} expression is satisfied, executes the        |
;;   | {thenStatements} and subsequently terminates the compound.      |
;;   | Otherwise probes the {elseIfGuard_1} through {elseIfGuard_n},   |
;;   | executing the first matching guard's statements, designated by  |
;;   | {elseIfStatements_1} through {elseIfStatements_N} respectively, |
;;   | while immediately concluding with a termination. Finally, if    |
;;   | none of these guards has been ascertained, the {elseStatements} |
;;   | will be performed.                                              |
;;   | --------------------------------------------------------------- |
;;   | The {thenGuard} must be an expression whose evaluated form      |
;;   | results in a Boolean object.                                    |
;;   | --------------------------------------------------------------- |
;;   | The {thenStatements} must be a sequence of zero or more         |
;;   | statements.                                                     |
;;   | --------------------------------------------------------------- |
;;   | {elseIfGuard_1} through {elseIfGuard_N} must be expressions     |
;;   | whose evaluated forms result in Boolean objects.                |
;;   | --------------------------------------------------------------- |
;;   | {elseIfStatements_1} through {elseIfStatements_N} must be       |
;;   | sequences of zero or more statements.                           |
;;   | --------------------------------------------------------------- |
;;   | The {elseStatements} must be a sequence of zero or more         |
;;   | statements.                                                     |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if the  |
;;   | {thenGuard} does not resolve to a Boolean truth value.          |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if any  |
;;   | of the conditions {elseIfGuard_1} through {elseIfGuard_N} do    |
;;   | not resolve to a Boolean truth value.                           |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | For loop                                                        |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | we've known {counterName} for {repetitionCount}                 |
;;   |   {statements}                                                  |
;;   | never gonna give you up                                         |
;;   |=================================================================|
;;   | Iterates a {repetitionCount} number of times, on each iteration |
;;   | updating the value of the variable designated by {counterName}, |
;;   | commencing from its value contained immediately prevenient to   |
;;   | this construct's first invocation, and concluding with the      |
;;   | inclusive upper bourne of                                       |
;;   |   {repetitionCount} + (valueOf({counterName}) - 1)              |
;;   | while concomitantly executing the {statements}.                 |
;;   | --------------------------------------------------------------- |
;;   | Please note that for a {repetitionCount} less than or equal to  |
;;   | zero (0), the iterance will not accompass any effect.           |
;;   | --------------------------------------------------------------- |
;;   | The {counterName} must be a valid, already declared variable    |
;;   | identifier whose content produces a signed integer number.      |
;;   | --------------------------------------------------------------- |
;;   | The {repetitionCount} must be an expression whose evaluated     |
;;   | form produces a signed integer number.                          |
;;   | --------------------------------------------------------------- |
;;   | The {statements} must be a sequence of zero or more statements. |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if the  |
;;   | {counterName} variable does not resolve to an integer number.   |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "UnknownVariableError" is signaled if a    |
;;   | variable with the {counterName} is not yet declared.            |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IncompleteVariableError" is signaled if a |
;;   | variable with the {counterName} is declared, but not yet        |
;;   | assigned a value.                                               |
;;   |_________________________________________________________________|
;;   
;;    _________________________________________________________________
;;   | While loop                                                      |
;;   | <<statement>>                                                   |
;;   |=================================================================|
;;   | a full commitment's what I'm thinking of {guard}                |
;;   |   {statements}                                                  |
;;   | never gonna give you up                                         |
;;   |=================================================================|
;;   | Executes the {statements} while the {guard} is satisfied,       |
;;   | probing the same as a prevenient step at each iteration's       |
;;   | inchoation.                                                     |
;;   | --------------------------------------------------------------- |
;;   | The {guard} must be an expression, the evaluated form of which  |
;;   | either resolves to a  Boolean "True" or "False" value.          |
;;   | --------------------------------------------------------------- |
;;   | The {statements} must be a sequence of zero or more statements. |
;;   | --------------------------------------------------------------- |
;;   | An error of the type "IllegalArgumentError" is signaled if the  |
;;   | {guard} does not resolve to a Boolean truth value.              |
;;   |_________________________________________________________________|
;; 
;; == EXPRESSIONS: COEFFICIENCY OF OPERATORS ==
;; The operative ambit's dependency upon expressions relays to the
;; collaborative investments of unary and binary operations, their
;; membership shall now be enumerated in a more sophisticated ilk of
;; perquisition.
;; 
;; == UNARY OPERATORS ==
;; The set of unary operators --- always specimens of the prefix
;; realm --- does not endeavor a mimicry of the binary compernage's
;; ramosity; imprimis, forecause its operand tally is restricted to an
;; aefauld element; also, as the complete species' exhaustion can be
;; intrined into a twain of signs and the logical NOT operator:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Operand | Effect
;;   ---------+---------+----------------------------------------------
;;   +        | boolean | invalid
;;            |--------------------------------------------------------
;;            | integer | plus sign
;;            |--------------------------------------------------------
;;            | string  | invalid
;;   ..................................................................
;;   -        | boolean | invalid
;;            |--------------------------------------------------------
;;            | integer | minus sign
;;            |--------------------------------------------------------
;;            | string  | invalid
;;   ..................................................................
;;   !        | boolean | logical NOT
;;            |--------------------------------------------------------
;;            | integer | invalid
;;            |--------------------------------------------------------
;;            | string  | invalid
;;   ------------------------------------------------------------------
;; 
;; == BINARY OPERATORS ==
;; A rather generous nimiety applies to the purview of binary operations
;; in the Counting programming language.
;; 
;; The trinity of its types, in conjunction with these operators, begets
;; a rather intricate circumstance of causatum and invaliditiy, and thus
;; shall be the following elucidating table's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Left op. | Right op. | Effect
;;   ---------+----------+-----------+---------------------------------
;;   +        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | addition
;;            |--------------------------------------------------------
;;            | integer  | string    | string concatenation
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | string concatenation
;;            |--------------------------------------------------------
;;            | string   | string    | string concatenation
;;   ..................................................................
;;   -        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | subtraction
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   *        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | multiplication
;;            |--------------------------------------------------------
;;            | integer  | string    | string repetition
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | string repetition
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   /        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | division
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   %        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | remainder
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   ^        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | exponentiation, power
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   &&       | boolean  | boolean   | logical AND
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | invalid
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   ||       | boolean  | boolean   | logical OR
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | invalid
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   ==       | boolean  | boolean   | equality
;;            |--------------------------------------------------------
;;            | boolean  | integer   | equality
;;            |--------------------------------------------------------
;;            | boolean  | string    | equality
;;            |--------------------------------------------------------
;;            | integer  | boolean   | equality
;;            |--------------------------------------------------------
;;            | integer  | integer   | equality
;;            |--------------------------------------------------------
;;            | integer  | string    | equality
;;            |--------------------------------------------------------
;;            | string   | boolean   | equality
;;            |--------------------------------------------------------
;;            | string   | integer   | equality
;;            |--------------------------------------------------------
;;            | string   | string    | equality
;;   ..................................................................
;;   !=       | boolean  | boolean   | inequality
;;            |--------------------------------------------------------
;;            | boolean  | integer   | inequality
;;            |--------------------------------------------------------
;;            | boolean  | string    | inequality
;;            |--------------------------------------------------------
;;            | integer  | boolean   | inequality
;;            |--------------------------------------------------------
;;            | integer  | integer   | inequality
;;            |--------------------------------------------------------
;;            | integer  | string    | inequality
;;            |--------------------------------------------------------
;;            | string   | boolean   | inequality
;;            |--------------------------------------------------------
;;            | string   | integer   | inequality
;;            |--------------------------------------------------------
;;            | string   | string    | inequality
;;   ..................................................................
;;   <        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | less than
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | less than (lexicographic)
;;   ..................................................................
;;   <=       | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | less than or equal to
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | less than or equal to (lexicog.)
;;   ..................................................................
;;   >        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | greater than
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | greater than (lexicographic)
;;   ..................................................................
;;   >=       | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | greater than or equal to
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | greater than or equal to (lex.)
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the adminicula invested by its treatise and examples, the
;; NeverGonna programming language's original specification tholes some
;; mete of ambiguity, the same tharfs explications. Extricated from this
;; membership, a select parcel shall be administered further attendance.
;; 
;; == HOW SHALL DECLARED BUT UNASSIGNED VARIABLES RESPOND? ==
;; The intermede governing the spatial sepiment betwixt a variable's
;; declaration and its assignment encumbers the language with a further
;; imposition, such as appertains to the subliminal placeholder state
;; in its respondency to indagations and modifications.
;; 
;; If a variable, declared but not yet assigned, is rendered amenable to
;; queries and manipulations, the following potential reactions rede
;; their contemplation:
;; 
;;   (1) An error is issued, apprizing the responsible party about the
;;       kensback circumstance.
;;   
;;   (2) A default value is tacitly affiliated as an epiphenomenon
;;       during the declaration, whence inquisitions are served.
;; 
;; It has been adjudged, proceeding from extant forbisens' adduction in
;; the protolog, to whom the necessity of sane initializations, even for
;; "for" loop counter variables, ostends a requisitum, to regulate the
;; mechanisms in covenableness with the first (1) alternative; in
;; consectary all variable, in the aftermath of their declaration,
;; ought to be explicitly assigned a value ere an indagation.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of abstract syntax tree
;; (AST) nodes from tokens; the kenspeckle proprium of this project,
;; however, wones in its conjunction of the parser combinator principle
;; with the top-down precedence technique proposed by Vaughan
;; Pratt,[pratt1973top] begetting a champarty which in the latter case
;; facilitates the involvement of operator precedence handling in
;; expressions.
;; 
;; == PARSERS AND COMBINATORS ARE FUNCTIONS ==
;; In eath diction, the parser combinator approach constructs a complete
;; parser entity from a sequence of interoperating smaller parsers,
;; their coefficiency enabled through combinators.
;; 
;; Both parsers and combinators are, in their pristine diorism,
;; represented by functions, accepting a source to parse and returning
;; in the case of a successful application a composition apprehending at
;; least
;; 
;;   - The remaining portion of the source, curtailed by the consumed
;;     items.
;;     If, for instance, the source represents a string, the first
;;     characters matching the parsing predicate will be removed; for
;;     tokens in lieu of this direct input, the residue following the
;;     accepted token objects are delivered.
;;   - An object designating the parser's or combinator's contribution
;;     to the encompassing whole, that is, usually an AST node.
;; 
;; A failure in the parser's or combinator's operations usually
;; concludes either with a communicative flag or an error signaling.
;; 
;; Conforming to an augmentation in formality, the following signature
;; may be proffered for parsers and combinators:
;; 
;;   function (source : any) -> (newSource : any, output : any)
;; 
;; == PARSERS AND COMBINATORS ARE INTERWOVEN IN SERIES ==
;; Considering the successful case, the modified parser or combinator
;; source is utilized as an input to the subsequent parser/combinator,
;; chaining these into a series of processors that, in concluding in an
;; ultimately empty source, build the output structure, for instance,
;; the abstract syntax tree.
;; 
;; == PARSERS EQUAL COMBINATORS ==
;; The discrepancy betwixt parsers and combinators constitutes a rather
;; puisne question of terminology for most objectives, as both partake
;; of a functional commonality. Parsers are usually "stand-alone"
;; components, responsible for the actual modification of the source,
;; whereas combinators ligate zero or more parsers, or other
;; combinators, in order to accompass a result.
;; 
;; If we have, as an example, a parser "characterOf", defined as
;; 
;;   function characterOf (expectedCharacter : character)
;;     let characterParser <- function (source : string)
;;       if source[0] = expectedCharacter then
;;         return (source.substring (1, source.length),
;;                 makeNode(NODE_TYPE_CHARACTER, source[0])
;;       else
;;         return null
;;       end if
;;     end function
;;     
;;     return characterParser
;;   end function
;; 
;; the requisitum involved in parsing more than one character coerces us
;; to discover a chaining of mandatorily matching "characterOf"
;; invocations. To this end, we define the following combinator:
;; 
;;   function allMatch (parsers : parserFunction[0..*])
;;     let allCombinator <- function (source : string)
;;       let newSource <- source
;;       let nodes     <- empty node list
;;       for every parser currentParser in parsers do
;;         let parserResult <- currentParser(source)
;;         
;;         if parserResult is null then
;;           return null
;;         else
;;           newSource <- parserResult[0]
;;           append parserResult[1] to nodes
;;         end if
;;       end for
;;       
;;       return (newSource, nodes)
;;     end function
;;     
;;     return allCombinator
;;   end function
;; 
;; An exemplary invocation of the combinator "allMatch" with several
;; instances of the "characterOf" parser could involve:
;; 
;;   parse (allMatch (characterOf ('h'),
;;                    characterOf ('e'),
;;                    characterOf ('l'),
;;                    characterOf ('l'),
;;                    characterOf ('o')),
;;          "hello")
;; 
;; == A PARSER COMBINATOR IN AN OBJECT-ORIENTED CONTEXT ==
;; The principal and onomastic substrate derives from Jeffrey Massung's
;; "parse" package for Common Lisp, which please see under
;; [massung2020parse]. A diverging aspect is apportioned its commorancy
;; in the object-oriented variation, substituting the functional notions
;; in order to emphasize the coefficacy partaken of by the several
;; components.
;; 
;; 
;; Appendices
;; ==========
;; The following sections shall provide a series of additaments in
;; reference to such topics that ostend a mete of pertinence
;; intermediate betwixt a vindication of its attendance and a docimasy
;; producing no stringency to incorporate in the primary writ.
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
;;   =        | 20            | right-to-left | Not used.
;;   ..................................................................
;;   +=       | 20            | right-to-left | 
;;   ..................................................................
;;   -=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   *=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   /=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   %=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   ||       | 40            | left-to-right | 
;;   ..................................................................
;;   &&       | 50            | left-to-right | 
;;   ..................................................................
;;   =        | 90            | left-to-right | 
;;   ..................................................................
;;   !=       | 90            | left-to-right | 
;;   ..................................................................
;;   <        | 100           | left-to-right | 
;;   ..................................................................
;;   <=       | 100           | left-to-right | 
;;   ..................................................................
;;   >        | 100           | left-to-right | 
;;   ..................................................................
;;   >=       | 100           | left-to-right | 
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
;;   ^        | 150           | right-to-left | 
;;   ..................................................................
;;   - (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   + (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   !        | 170           | right-to-left | 
;;   ------------------------------------------------------------------
;; 
;; == APPENDIX B: PROJECT FILES ==
;; A paragon of complex intercourse among components, this project's
;; circumference has been assayed with a supputation that vindicates its
;; concameration into several files, rather than a unity in spatial
;; coherence.
;; 
;; The ensuing conformation will enjoy a cursory, yet hopefully
;; didascalic, species of explication.
;; 
;; Please note that at least one infrastructure of nearly official
;; weight exists for such project management purposes, realized in the
;; definition of file interfaces using packages, and their orders and
;; relationships' enunciation by the "ASDF" system. This simple example,
;; however, has been adjudged as rather inflicted with a digressive
;; cumbrance in an advenient structuring's adhibition, rather than its
;; enjoying — a more serious enterprise certainly would be assayed in an
;; athwart airt.
;; 
;;   ------------------------------------------------------------------
;;   No. | File                   | Role
;;   ----+------------------------+------------------------------------
;;    1  | types.lisp             | Defines the custom types employed
;;       |                        | in the subsequent files, such as
;;       |                        | ``list-of'' and ``destination''.
;;   ..................................................................
;;    2  | errors.lisp            | Maintains the bespoke NeverGonna
;;       |                        | condition types, comprehending,
;;       |                        | among others, the various variable
;;       |                        | handling errors.
;;   ..................................................................
;;    3  | token.lisp             | Implements the tokens,
;;       |                        | encapsulations of significant
;;       |                        | objects originating in the lexer
;;       |                        | and its conation in a lexical
;;       |                        | analyzation of a NeverGonna source
;;       |                        | code, and propagated through the
;;       |                        | parser, to whom the dever of their
;;       |                        | assemblage into an abstract syntax
;;       |                        | tree (AST) is consigned.
;;   ..................................................................
;;    4  | lexer.lisp             | Implements the lexer, responsible
;;       |                        | for extracting the tokens from a
;;       |                        | piece of NeverGonna source code
;;       |                        | specified in string form.
;;   ..................................................................
;;    5  | token-queue.lisp       | Implements a queue for the castaldy
;;       |                        | of token in a singly linked format,
;;       |                        | capacitating the gradual appendage
;;       |                        | of tokens, while successor
;;       |                        | relationships may be queried by the
;;       |                        | nodes' adminiculum.
;;   ..................................................................
;;    6  | parse-state.lisp       | Implements the parse state, an
;;       |                        | encapsulation of the entire parsing
;;       |                        | activity's progress, and intended
;;       |                        | to be committed to a parser or
;;       |                        | combinator in order to receive a
;;       |                        | parse result.
;;   ..................................................................
;;    7  | parse-result.lisp      | Implements the parse result, an
;;       |                        | encapsulation of a parser's or
;;       |                        | combinator's response to its
;;       |                        | invocation with a parse state,
;;       |                        | determining its success or failure,
;;       |                        | the advancement through the next
;;       |                        | parse state, and its contribution
;;       |                        | to the complete abstract syntax
;;       |                        | tree (AST).
;;   ..................................................................
;;    8  | ast-node.lisp          | Implements the abstract syntax tree
;;       |                        | (AST) nodes, each specimen
;;       |                        | encapsulating a NeverGonna language
;;       |                        | construct, such as a loop or a
;;       |                        | binary set operation with its
;;       |                        | operands, intended to be assembled
;;       |                        | by a parser.
;;   ..................................................................
;;    9  | basic-parsers.lisp     | Implements the basic parsers and
;;       |                        | parser combinators, the haecceity
;;       |                        | of whom allocates to them a very
;;       |                        | generic vallidom, as opposed to
;;       |                        | such whose accommodation to the
;;       |                        | NeverGonna programming language is
;;       |                        | kenspeckle. The parsers and
;;       |                        | combinators furnish the substrate
;;       |                        | to all other parser files.
;;   ..................................................................
;;   10  | statement-parsers.lisp | Implements the parsers and parser
;;       |                        | combinators accommodated for the
;;       |                        | NeverGonna language's idiopathic
;;       |                        | necessities, while relying on both
;;       |                        | the basic parsers and the Pratt
;;       |                        | parser's services.
;;   ..................................................................
;;   11  | pratt-parser.lisp      | Implements a Pratt parser whose
;;       |                        | foundation is empighted upon the
;;       |                        | parser combinators principle, and
;;       |                        | whose bailiwick amplects merely the
;;       |                        | handling of expressions in the
;;       |                        | statements' circumference.
;;   ..................................................................
;;   12  | ngobjects.lisp         | Implements the "NGObject"s,
;;       |                        | encapsulations of Common Lisp
;;       |                        | objects in a mode so as to
;;       |                        | approximate NeverGonna's type
;;       |                        | system with its trifurcation into
;;       |                        | Boolean, integral, and string data.
;;   ..................................................................
;;   13  | unary-operations.lisp  | Implements the unary operations
;;       |                        | on the "NGObject" encapsulations of
;;       |                        | Boolean, integers, and strings.
;;   ..................................................................
;;   14  | binary-operations.lisp | Implements the binary operations
;;       |                        | on the "NGObject" encapsulations of
;;       |                        | Boolean, integers, and strings.
;;   ..................................................................
;;   15  | ngvariable.lisp        | Implements the "NGVariable" model
;;       |                        | for a NeverGonna variable, the same
;;       |                        | stores the name, value, and a
;;       |                        | declaration flag.
;;   ..................................................................
;;   16  | reference.lisp         | Implements the reference concept, a
;;       |                        | dedicated value whose resolution,
;;       |                        | yielded by the interpreter's
;;       |                        | procession of a variable node,
;;       |                        | serves to distinguish the string
;;       |                        | identifier from an actual literal
;;       |                        | string.
;;   ..................................................................
;;   17  | interpreter.lisp       | Implements the interpreter, the
;;       |                        | agent tasked with traversing the
;;       |                        | abstract syntax tree (AST) produced
;;       |                        | by the parser in order to induce
;;       |                        | actual effect.
;;   ..................................................................
;;   18  | tests.lisp             | Implements the test cases and
;;       |                        | examples for demonstrating the
;;       |                        | interpreter's conformance with the
;;       |                        | NeverGonna programming language.
;;   ..................................................................
;;   19  | main.lisp              | Establishes the starting point into
;;       |                        | this application, in particular
;;       |                        | loading the aforementioned Common
;;       |                        | Lisp source files.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-12-21
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
;;   [devanla2021minimalparsecomb]
;;   Guru Devanla, "Minimal Parser Combinator in Python",
;;                 26th October 2021
;;   URL: "https://gdevanla.github.io/posts/
;;         write-a-parser-combinator-in-python.html"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in Python.
;;   
;;   [elouafi2018gentleintroparscomb]
;;   Yassine Elouafi, "A gentle introduction to parser combinators",
;;                    2018
;;   URL: "https://dev.to/yelouafi/
;;         a-gentle-introduction-to-parser-combinators-21a0"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [elouafi2021introparsercomb]
;;   Yassine Elouafi, "introduction-to-parser-combinators.md",
;;                    June 28, 2021 
;;   URL: "https://gist.github.com/yelouafi/
;;         556e5159e869952335e01f6b473c4ec1"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [esolang2020NeverGonna]
;;   The Esolang contributors, "NeverGonna", June 11th, 2020
;;   URL: "https://esolangs.org/wiki/NeverGonna"
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", sixth edition, 2014,
;;     pages 122--127
;;   Notes:
;;     - Describes the concept and an implementation of the singly
;;       linked list in the Java programming language.
;;     - The pages 276 through 280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
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
;;   [massung2020parse]
;;   Jeffrey Massung, "The PARSE Package", 2020
;;   URL: "https://github.com/massung/parse"
;;   Notes:
;;     - GitHub repository of the "parse" package, a Common Lisp library
;;       for token parsing which employs parser combinators.
;;   
;;   [mulligan2023unlocking]
;;   Rory Mulligan, "Unlocking the Power of Parser Combinators: A
;;                   Beginner's Guide", February 9, 2023
;;   URL: "https://www.sitepen.com/blog/
;;         unlocking-the-power-of-parser-combinators-a-beginners-guide"
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
  "The ``file-source'' type defines a source for the obtention of
   files, obeying to the stipulation of conformity with Common Lisp's
   data consumption facilities, such as the ``open'' function and the
   ``with-open-file'' macro."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "The directory amplecting the Common Lisp source files appertaining to
   the project.
   ---
   It is essential for this project's operation to correctly specify the
   directory inwhich the Common Lisp source code files are accommodated
   their commorancy.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of import operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-project-file (file-name)
  "Imports the Common Lisp file designated by the FILE-NAME, and
   expected to reside in the +PROJECT-DIRECTORY+, and returns no value."
  (declare (type file-source file-name))
  (load (merge-pathnames +PROJECT-DIRECTORY+ file-name))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Loading of project files.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-project-file "types.lisp")
(load-project-file "errors.lisp")
(load-project-file "token.lisp")
(load-project-file "lexer.lisp")
(load-project-file "token-queue.lisp")
(load-project-file "parse-state.lisp")
(load-project-file "parse-result.lisp")
(load-project-file "ast-nodes.lisp")
(load-project-file "basic-parsers.lisp")
(load-project-file "statement-parsers.lisp")
(load-project-file "pratt-parser.lisp")
(load-project-file "ngobjects.lisp")
(load-project-file "unary-operations.lisp")
(load-project-file "binary-operations.lisp")
(load-project-file "ngvariable.lisp")
(load-project-file "reference.lisp")
(load-project-file "interpreter.lisp")
(load-project-file "tests.lisp")
