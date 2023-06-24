;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Phile", presented by the Esolang user
;; "PythonshellDebugwindow" in the year 2020, the foundry of which
;; constitutes the emulation of file operation concepts for the
;; representation of the various programmatic facilities.
;; 
;; 
;; Concept
;; =======
;; The Phile programming language's affiliation serves as a prelude to
;; its kenspeckle attributes, whence programs mimicking applications
;; upon files originate.
;; 
;; == NOMENCLATURE AS AN ALLUSION TO THE BASIC CONCEPT ==
;; In allusion to its aspired notions, and as a further affordance of
;; entertainment in a calembour's display, the "Phile" agnomination
;; incorporates the "file" component.
;; 
;; == FILE HANDLING MAPS TO OPERATIONS ==
;; The dioristic guise, the simulation of a sequence of operations
;; performed on files, reproduces in a tacit manner the explicit
;; potentials of common programming languages.
;; 
;; == FILES ARE VARIABLES ==
;; The juncture of consilience betwixt the file metaphore and the
;; programmatic urgencies of data management is satisfied in the files'
;; clandestine agency of variable representation. Each file may store
;; a string or a signed integer of any magnitude; its untyped nature
;; permitting a bidirectional transit betwixt these differentiated
;; realms.
;; 
;; The metaphorical equivalencies betwixt the file operations and their
;; effects in the context of variable management can be summarized as
;; follows:
;; 
;;   ------------------------------------------------------------------
;;   File action | Effect
;;   ------------+-----------------------------------------------------
;;   Open        | Opening a file constitutes the equivalent of a
;;               | variable declaration without an initial value's
;;               | assignment.
;;   ..................................................................
;;   Read        | Reading from a file is construed as returning a
;;               | variable's value.
;;   ..................................................................
;;   Overwrite   | Overwriting a file replaces the variable's complete
;;               | content by the new value.
;;   ..................................................................
;;   Write       | Writing to a file accompasses the appending of the
;;               | new content to the already extant variable data.
;;   ..................................................................
;;   Close       | Closing a file represents the variable's deletion.
;;   ------------------------------------------------------------------
;; 
;; == SPECIAL FILES CONTRIBUTE INPUT/OUTPUT FACILITIES ==
;; A concomitant to the definition of custom files in the pursuit of
;; object storage, input and output conduits are represented by three
;; predefined files:
;; 
;;   - The file "stdin.stream" relays to the system's standard input
;;     input. By reading from the same, user input can be queried.
;;     Writing and overwriting both constitute a violation.
;;   - The file "stderr.stream" connects to the system's standard error
;;     output. Writing to the same or overwriting permits the commission
;;     of error messages.
;;   - The file "stdout.stream" establishes a connection to the system's
;;     standard output conduit. Writing to the same, as well as the
;;     equipollent overwriting, permits the issuing of output. Reading
;;     is interdicting and will beget an error.
;; 
;; == ALL FILES MUST BE CLOSED ERE THE PROGRAM'S TERMINATION ==
;; A stringent prerequisite to a program's termination, all files having
;; been opened must be closed, otherwise an error of the type
;; "UnclosedFilesError" is issued.
;; 
;; == CONDITIONAL JUMPS PERMIT NAVIGATION ==
;; An aefauld control flow mechanism participates as a warkloom for
;; navigating inside of a program, redirecting the instruction pointer,
;; upon the predicate's satisfaction, to the zero-based line index
;; stated as the consequence.
;; 
;; 
;; Architecture
;; ============
;; The language's construction remains independent of a particular
;; prescription, except for the management of files in the agency of
;; variables, and the related structure for their handling.
;; 
;; All data pertaining to a program resides in the circumference of
;; files, conceptually a linear sequence of objects responding to
;; inquries of and modifications to their content. From a pragmatic
;; point of view, this fact alludes to a mapping from a string to a
;; variable storing its content in a textual form admissive to
;; various manipulations. No particular rarification's design is
;; administered by the language standard; natheless, the specification
;; of the key-value associations partakes of an associative array's
;; nature. The indifference applied to the entries' ordering redes a
;; hashed solution, for instance in the hash table mold, as a trenchant
;; choice.
;; 
;; 
;; Data Types
;; ==========
;; Phile's type system comprehends two members: signed integer numbers
;; of any magnitude and strings.
;; 
;; == INTEGERS ==
;; Integers are neither confined anest their sign nor their extent,
;; thus occupying the theoretical range [-infinity, +infinity]. Their
;; deployment is exercised in a paravaunt manner as constituents of
;; binary operations, enumerating both arithmetics and the bidirectional
;; conversion betwixt numeric and string data. Their application in the
;; context of line numberings, for the goto facility, allocates them a
;; further utility. If stated as literals, their form resolves to a
;; sequence of base-10 digits.
;; 
;; == STRINGS ==
;; Textual content's conveyance is realized in the form of strings,
;; ensconced in double quotes, and of arbitrary extent, including the
;; contingence for vacancy. A compernage of the literally construed
;; characters, several escape sequences, affiliated with the backslash
;; ("\") prefix are defined:
;; 
;;   ------------------------------------------------------------------
;;   Escape sequence | Description
;;   ----------------+-------------------------------------------------
;;   \"              | Inserts a double quote without terminating the
;;                   | comprehending string.
;;   ..................................................................
;;   \\              | Inserts a single backslash character ("\").
;;   ..................................................................
;;   \n              | Inserts a linebreak. Equivalent to "\r\n".
;;   ..................................................................
;;   \r\n            | Inserts a linebreak. Equivalent to "\n".
;;   ..................................................................
;;   \t              | Inserts a horizontal tab.
;;   ------------------------------------------------------------------
;; 
;; Any other character preceded by a "\" is construed as a superfluous
;; instance of escaping and thus not affected in its default meaning.
;; 
;; == BOOLEAN VALUES ==
;; Albeit logical inquisitions in the form of predicates comprise the
;; elements of the goto instruction, no provisions for Boolean values
;; project into the language; in lieu of this basic constituent, several
;; particular specimens of integers and strings act in the delegacy of
;; sentinels with a causatum tantamount to the Boolean false object.
;; In concrete diction, every integer datum and string object resolves
;; to a true response, except for the following treble that simulates
;; a false statement:
;; 
;;   -------------------------------------------------------------
;;   False sentinel | Type    | Description
;;   ---------------+---------+-----------------------------------
;;   0              | integer | An integer value of zero (0).
;;   .............................................................
;;   ""             | string  | The empty string.
;;   .............................................................
;;   "0"            | string  | A string composed of a zero digit.
;;   -------------------------------------------------------------
;; 
;; 
;; Syntax
;; ======
;; Phile programs entail a sequence of zero or more statements, each
;; enharboring a separate line and terminated by a semicolon.
;; Expressions supplement the instrucction argument portions.
;; 
;; == FILE NAMES ==
;; File names are represented by string literals, a composite of zero or
;; more arbitrary characters, including escape sequences, enclosed in
;; double quotes ('"').
;; 
;; == INSTRUCTIONS ==
;; An instruction resides on a line of its own, assuming one of three
;; possible guises: prefix statements, infix statements, and
;; expressions.
;; 
;; The quantitative preponderance among the members, prefix statements
;; demonstrate a homogeneous construction, with the command name being
;; succeeded by the file designator, and concluding with zero or one
;; values as the ultimate argument. A semicolon as the terminator closes
;; the statement. Five specimens subsume into this tier:
;; 
;;   CLOSE {fname}
;;   OPEN  {fname};
;;   OVERWRITE {fname} {value};
;;   READ {fname};
;;   WRITE {fname} {value};
;; 
;; The READ exemplary deviates in some mete from its peer's strictures
;; by its dual nature as a statement and an expression, homologating its
;; incorporation into complex linkages.
;; 
;; An exhaustion of the infix statements realizes in a single forbisen
;; already: the jump or goto facility, according with the signature
;; 
;;   {value}? {lineNumber};
;; 
;; An arbitary expression, {value}, serves as an introduction,
;; establishing the predicate, segregated by a question mark ("?") from
;; the destination line index {lineNumber}, and again demarcated by a
;; semicolon.
;; 
;; Expressions, the ultimate of the treble taxonomy, experience their
;; utility as the value providers, conflated with the {value}
;; placeholder whose acquaintance has already been encountered in the
;; previous twain of divisions. Any programmatic constituent capable of
;; concatenation is permitted adit to participation. This comprehends
;; 
;;   binary expressions
;;   READ {fname}
;; 
;; == SPACES ==
;; At least one space, comprehending the space character and the
;; horizontal tab, separates any two tokens, with tolerance's
;; administration unto preceding and trailing occurrences.
;; 
;; == LINEBREAKS ==
;; With each line being either a statement's occupancy, or an empty
;; instance, linebreaks appropriate the role of sepiments betwixt
;; successive instructions. Their vacant forms may appear in an
;; arbitrary tally and at any location.
;; 
;; == COMMENTS ==
;; Line comments are provided, commencing with a sequence of three
;; slashes in immediate succession ("///"), and demarcated by the end of
;; the respective line.
;; 
;; == GRAMMAR ==
;; The Extended Backus-Naur Form (EBNF) describes the language syntax:
;; 
;;   program              := { emptyLine } ,
;;                           { innerInstruction } ,
;;                           { emptyLine } ,
;;                           terminalInstruction ;
;;   innerInstruction     := instruction , newline ;
;;   terminalInstruction  := { instruction } , { emptyLine } ;
;;   emptyLine            := { space } , [ linebreak ] ;
;;   
;;   value                := simpleValue , [ operator , value ] ;
;;   simpleValue          := integer
;;                        |  string
;;                        |  readInstruction
;;                        ;
;;   operator             := "+" | "-" | "*" | "/"
;;                        |  "<" | ">"
;;                        |  "=" | "!"
;;                        ;
;;   
;;   instruction          := closeInstruction
;;                        |  jumpInstruction
;;                        |  openInstruction
;;                        |  overwriteInstruction
;;                        |  readInstruction
;;                        |  writeInstruction
;;                        ;
;;   
;;   closeInstruction     := "CLOSE" , fileName ;
;;   jumpInstruction      := value , "?" , lineNumber ;
;;   openInstruction      := "OPEN" , fileName ;
;;   overwriteInstruction := "OVERWRITE" , fileName , value ;
;;   readInstruction      := "READ" , fileName ;
;;   writeInstruction     := "WRITE" , fileName , value ;
;;   
;;   fileName             := string ;
;;   
;;   string               := '"' , { character | escapeSequence } , '"' ;
;;   lineNumber           := [ "+" ] , digit , { digit } ;
;;   integer              := [ "+" | "-" ] , digit , { digit } ;
;;   
;;   escapeSequence       := '\"' | "\n" | "\r\n" | "\\" | "\t" ;
;;   digit                := "0" | "1" | "2" | "3" | "4"
;;                        |  "5" | "6" | "7" | "8" | "9"
;;                        ;
;; 
;; 
;; Instructions
;; ============
;; Phile's instruction set is compact of quintuple membership engaged in
;; the simulation of file operations --- a tally conceptually expanded
;; by binary operations spanning basic arithmetics and relational
;; specimens.
;; 
;; == OVERVIEW ==
;; A cursory apercu shall juxtapose the language's command set to the
;; ulterior intentions, ere a more invested rendition's dation proceeds
;; with explications concerning the syntactical details and parameters:
;; 
;;   ------------------------------------------------------------------
;;   Command   | Simulated causatum
;;   ----------+-------------------------------------------------------
;;   OPEN      | Creation of a variable, or import of an input/ouput
;;             | library.
;;   ..................................................................
;;   READ      | Obtention of a portion or the whole of a variable's
;;             | value, or query of a user input line from the standard
;;             | input.
;;   ..................................................................
;;   WRITE     | Appendage of an integer or string value to a variable,
;;             | or printing of such an object to the standard output.
;;   ..................................................................
;;   OVERWRITE | Replacement of a variable's value by an integer or
;;             | string, or printing of such an object to the standard
;;             | output.
;;   ..................................................................
;;   CLOSE     | Deletion of a variable, deallocation of the memory
;;             | reserved for a variable, or deletion of an imported
;;             | input/output library.
;;   ------------------------------------------------------------------
;; 
;; An apercu over the available commands, invested with their
;; parameters, shall be purveyed in the following:
;; 
;;   ------------------------------------------------------------------
;;   Command                   | Effect
;;   --------------------------+---------------------------------------
;;   OPEN {fname}              | Creates a file with the name {fname},
;;                             | if not yet extant, and sets it as the
;;                             | current one.
;;                             | If a file with the name {fname}
;;                             | already exists, an error of the type
;;                             | "DuplicateFileError" is signaled.
;;   ..................................................................
;;   CLOSE {fname}             | Removes the file {fname}.
;;                             | If no file with the name {fname}
;;                             | exists, an error of the type
;;                             | "MissingFileError" is signaled.
;;   ..................................................................
;;   WRITE {fname} {value}     | Appends the {value} to the content of
;;                             | the file {fname}.
;;                             | If no file with the name {fname}
;;                             | exists, an error of the type
;;                             | "MissingFileError" is signaled.
;;   ..................................................................
;;   OVERWRITE {fname} {value} | Overwrites the content of the file
;;                             | {fname} with the {value}.
;;                             | If no file with the name {fname}
;;                             | exists, an error of the type
;;                             | "MissingFileError" is signaled.
;;   ..................................................................
;;   {value}? {lineNumber}     | If the {value} is none of "", "0", or
;;                             | 0, the instruction pointer moves to
;;                             | the zero-based {lineNumber}.
;;   ..................................................................
;;   READ {fname}              | Reads a line from the file {fname}
;;                             | and returns it.
;;                             | If no file with the name {fname}
;;                             | exists, an error of the type
;;                             | "MissingFileError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; == RELATIONSHIP BETWEEN OPERATIONS AND FILE TYPES ==
;; An explication with focus on augmented explicity regarding the
;; vinculum betwixt the file-based operations CLOSE, OPEN, READ, WRITE,
;; and OVERWRITE shall be enlisted here:
;; 
;;   +-----------------------------------------------------------------+
;;   | CLOSE {fname}                                                   |
;;   |-----------------------------------------------------------------|
;;   | Normal file   | Closes the file designated by {fname}.          |
;;   |.................................................................|
;;   | stdin.stream  | Disables the use of the standard input.         |
;;   |.................................................................|
;;   | stdout.stream | Disables the use of the standard output.        |
;;   |.................................................................|
;;   | stderr.stream | Disables the use of the standard error stream.  |
;;   +-----------------------------------------------------------------+
;;   
;;   +-----------------------------------------------------------------+
;;   | OPEN {fname}                                                    |
;;   |-----------------------------------------------------------------|
;;   | Normal file   | Opens a file designated by {fname}.             |
;;   |.................................................................|
;;   | stdin.stream  | Enables the use of the standard input.          |
;;   |.................................................................|
;;   | stdout.stream | Enables the use of the standard output.         |
;;   |.................................................................|
;;   | stderr.stream | Enables the use of the standard error stream.   |
;;   +-----------------------------------------------------------------+
;;   
;;   +-----------------------------------------------------------------+
;;   | OVERWRITE {fname} {value}                                       |
;;   |-----------------------------------------------------------------|
;;   | Normal file   | Replaces the content of the file {fname} with   |
;;   |               | the {value}.                                    |
;;   |.................................................................|
;;   | stdin.stream  | Is prohibited and results in an error.          |
;;   |.................................................................|
;;   | stdout.stream | Writes the {value} to the standard output.      |
;;   |.................................................................|
;;   | stderr.stream | Writes the {value} to the standard error stream.|
;;   +-----------------------------------------------------------------+
;;   
;;   +-----------------------------------------------------------------+
;;   | READ {fname}                                                    |
;;   |-----------------------------------------------------------------|
;;   | Normal file   | Reads a line from the file {fname} and returns  |
;;   |               | it in an appropriate form.                      |
;;   |.................................................................|
;;   | stdin.stream  | Reads a line of user input from the standard    |
;;   |               | input and returns it in an appropriate form.    |
;;   |.................................................................|
;;   | stdout.stream | Is prohibited and results in an error.          |
;;   |.................................................................|
;;   | stderr.stream | Is prohibited and results in an error.          |
;;   +-----------------------------------------------------------------+
;;   
;;   +-----------------------------------------------------------------+
;;   | WRITE {fname} {value}                                           |
;;   |-----------------------------------------------------------------|
;;   | Normal file   | Appends the {value} to the content of the file  |
;;   |               | {fname}.                                        |
;;   |.................................................................|
;;   | stdin.stream  | Is prohibited and results in an error.          |
;;   |.................................................................|
;;   | stdout.stream | Writes the {value} to the standard output.      |
;;   |.................................................................|
;;   | stderr.stream | Writes the {value} to the standard error stream.|
;;   +-----------------------------------------------------------------+
;; 
;; == SPECIAL FILES ==
;; A triple of file names is subject to reservations; as a corollary,
;; the respondency to queries may deviate in such cases. This exclusive
;; membership does not seize significance because of arbitrary judgment,
;; but as a means to supply the conduits for input/output facilities.
;; 
;;   ------------------------------------------------------------------
;;   Special file  | Role
;;   --------------+---------------------------------------------------
;;   stdin.stream  | Represents the standard input STDIN.
;;                 | ..................................................
;;                 | Reading: The user is prompted for an input line,
;;                 |          the same either produces a string or an
;;                 |          integer.
;;                 | ..................................................
;;                 | Writing: Is prohibited and results in an error.
;;   ..................................................................
;;   stdout.stream | Represents the standard output STDOUT.
;;                 | ..................................................
;;                 | Reading: Is prohibited and results in an error.
;;                 | ..................................................
;;                 | Writing: The value is written to the standard
;;                 |          output, usually the console.
;;   ..................................................................
;;   stderr.stream | Represents the standard error output STDERR.
;;                 | ..................................................
;;                 | Reading: Is prohibited and results in an error.
;;                 | ..................................................
;;                 | Writing: The value is written to the standard
;;                 |          output, usually the console.
;;   ------------------------------------------------------------------
;; 
;; Please beware that, as with any file, the protocols of opening and
;; closing are obliged to be respected.
;; 
;; == BINARY OPERATIONS ==
;; Phile's capacitation is extended by a supply of binary operations,
;; both arithmetic and relational in their nature.
;; 
;; The following tabular exposition enumerates the operators with
;; decreasing precedence, incepting with the most powerful member and
;; advancing towards the least potent one.
;; 
;;   ----------------------
;;   Operator | Precedence
;;   ---------+------------
;;   READ     | 1 (highest)
;;   ......................
;;   *        | 2
;;   ......................
;;   /        | 3
;;   ......................
;;   +        | 4
;;   ......................
;;   -        | 5
;;   ......................
;;   =        | 6
;;   ......................
;;   !        | 7
;;   ......................
;;   <        | 8
;;   ......................
;;   >        | 9 (lowest)
;;   ----------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Phile's protolog, by its detailed explications and illustrative
;; examples, obviates the inroads of nearly any ambiguities.
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
;; == STATEMENTS AS AN EXTENSION OF THE PRATT PARSER ==
;; Among Crockford's variegated accomplishments enumerates one very
;; kenspeckle variation on the principles of the "nud" or operand token
;; processing. Pratt's endeavors applied in a singular conspectuity to
;; the evaluation of a functional programming language, the consectary
;; of which vouched any constituent's deployment as both a statement
;; with separate utility, and a dependency in the form of an argument or
;; operand.[crockford2007topdownopprec]
;; 
;; An example shall be extended. In Pratt's realm, every routine
;; automatically subsumes into the functional species, returning a value
;; in any case. If confronted with a specimen "open", of undisclosed and
;; insignificant arity, the homogeneous nature permits ubiquity in its
;; deployment, rendering the following forbisens equally admissive:
;; 
;;   open "myFile.txt"
;;   5 + (open "myFile.txt") + 12
;; 
;; Please consider how the "open" routine, being a function, may be
;; employed in singularity, yet also interspersed into an arbitrary
;; expression. However, what if the "open" routine would be a pure
;; procedure, without return value --- or, in the parlance of some
;; languages, returning a "void" sentinel to designate its
;; incompatibility for the usance as an intermittent constituent? The
;; first example line would remain as a canonical use case, while the
;; second instigates a breach of the syntactical integrity. The original
;; Pratt parser does not extract the restricted variant.
;; 
;; The benefits administering concinnity in such a mete to a function
;; environment obtrude incompatibly into programming languages
;; discriminating betwixt pure statements, intended to be independent
;; constituents, and expressions, which like functions expose a nexible
;; path to integration. This latter linguistic stratum, furcated along
;; the procedural axis, does not concord with flawless patration into
;; Pratt's model.
;; 
;; Crockford's Simplified JavaScript product adhered to this demading
;; category, instigating his induction of a third facette to the token's
;; extant "nud" and "led" phenotypes: the "std", an abbreviated term for
;; "statement denotation", a method associated with a token object as a
;; compernage to the standardized twain. Physically a kindred of the
;; "nud", by relying on the token itself, desisting from a sinistral
;; expression's perquisition, the "std" invocation engages in a trial
;; merely at the start of the statement parsing. Its failure to be
;; located resorts to the token's reappropriation in a "nud" context.
;; 
;; The following pseudocode extract shall explicate the basic ideas:
;; 
;;   function parseStatement (tokens : TokenStream)
;;     let initialToken <- tokens.peek()
;;     
;;     if initialToken starts a statement then
;;       tokenStream.consume()
;;       let statementNode <- std(tokens, initialToken)
;;       return statementNode
;;     else if initialToken is an operand then
;;       { This will invoke the "nud" function. }
;;       let expressionNode <- parseExpression(tokens)
;;       return expressionNode
;;     else
;;       signal error: "Unexpected token where operand expected."
;;     end if
;;   end function
;; 
;; == CONJUGALITY OF PURPOSES: PHILE & PRATT ==
;; This implementation has been entalented with a bifurcated mode of
;; purpose: the patent being, naturally, the entelechy of the Phile
;; programming language; the second, a fortiori, amplecting an attempt
;; at solving the parsing complexity by aid of a Pratt parser. The
;; conclusion thus relates to the solution as the conjugality of
;; practical aspirations with deitic pursuits and erudition as the
;; second compartment.
;; 
;; == FOUR COMPONENTS COLLABORATE IN THE REALIZATION ==
;; The Phile interpretation process delineates a quadruple champarty's
;; collaboration, the participants in the same shall be enumerated alow.
;; 
;; Source code:
;;   The vehicle of a Phile program's transmission, the source code's
;;   expression proceeds as a string entity. Albeit media with further
;;   abstracting qualities retain conceivability, such as character
;;   streams or iterators, their costage to be defrayed in accrued
;;   complexity is reckoned as a too peisant cumbrance in this project.
;; 
;; Lexer:
;;   In the lexer, the lexical analyzation engine, a discriminating unit
;;   whose intelligence extracts objects from characters, is
;;   established, generating from a piece of Phile source code, provided
;;   in string form, a sequence of tokens which encapsulate significant
;;   elements of the indagated language.
;; 
;; Token stream:
;;   An adminiculum betwixt the lexer's token production and the
;;   parser's consumption, the token stream's agency embraces the
;;   purveyance of services exceeding the former's core bailiwick, yet
;;   necessitated by the latter's requirements. Concretely, a token
;;   stream, akin to an iterator, provides access to the next token from
;;   the lexer's effort, as well as a look-ahead or "peeking" operation,
;;   which, without removal, returns the subsequent token --- a facility
;;   much appreciated by any parser, in particular Pratt's variant. As
;;   an instance of supererogation, the entity incorporates routines for
;;   matching the coming token against an expected type, contingently
;;   succeeding by its digestion in the positive case. Whether this last
;;   commodity would better be a commorant of the parser, as renders the
;;   preference in many projects, belongs to the realm of personal
;;   disquisitions.
;; 
;; Parser:
;;   Albeit destitute of an object-oriented incarnation, the parser's
;;   influence exercises via an infrastructure composed of dedicated
;;   data and procedural facilities in conjunction with and reliance
;;   upon the token stream. Imprimis, registries for associations
;;   betwixt tokens and parselets, functions responsible for the
;;   translation of tokens into nodes in a narrow context, are
;;   entertained, the periphery of which being populated with dependent
;;   definitions regarding the binding power. The evaluation of
;;   statements and expressions as the parselets' service receivers
;;   contributes to the most significant operational elements of the
;;   parser concept.
;; 
;; == LEXER: A TOKEN GENERATOR ==
;; The ``Lexer'' class establishes a twifaced unity --- a lean interface
;; concealing the complexity of the underlying analytics.
;; 
;; Nominally, the only operation provided comprehends the supply of the
;; tokens, one after the other, in the correct order.
;; 
;; The practical compass involves the incremental consumption of the
;; Phile source string, thus detecting in a gradual manner its
;; constituents. This "lazy" evaluation obviates the time and space
;; penalty impositions inextricably inhabiting the complete input's
;; immediate transformation into a list of tokens.
;; 
;; As an apostille, and a demarcation from the surrounding program
;; elements, neither a look-ahead (peeking) nor a token type check
;; enhances this class' circumference.
;; 
;; The listing alow aids with intelligence about the lexer's
;; capacitations:
;; 
;;   LEXER-GET-NEXT-TOKEN (lexer)
;;     Returns the next token from the lexer and advances the lexer.
;;     If its source is exhausted, each query is responded with a fresh
;;     end-of-file token.
;; 
;; A summary shall apply to us some concise nortelry:
;; 
;;   - The LEXER provides each next token.
;; 
;; == TOKEN STREAM: ABSTRACTION AND AUGMENTATION OF TOKEN ACCESS ==
;; A superimposition of one abstracting stratum upon an extant one,
;; at an inchoate inspection, certainly occurs as a betise. The lexer
;; itself indulges in no sportive aspiration when it returns as its
;; singular public service the next token, the means and modes of such
;; realization being ensconced hermetically behind the procedural unit
;; ``lexer-get-next-token''.
;; 
;; The ``Token-Stream'' class in this environment may be supputated a
;; nimious participant if responsible for the nurturing of the parser's
;; token consumption. Its vindication's reification results from the
;; assessment of the Pratt parser's duties. Deeply intertwined with its
;; assembly of tokens, the central expression parsing involves the
;; requisitum of a look-ahead, or peeking --- a tentative scrutiny in
;; order to adjudge the perogative betwixt two operator's claim for the
;; same operand. Additionally, an indagation anenst the current or the
;; successor token's conformance to a specified type is incorporated
;; into many parsers. Both bailiwicks might be embedded in this unit ---
;; or exported into a mediator between such assembling entity and the
;; lexer, as is the case with this ``Token-Stream'' class.
;; 
;; The following listing enumerates the ``Token-Stream'' class'
;; capacities:
;; 
;;   MAKE-TOKEN-STREAM (lexer)
;;     Creates and returns a new token stream which obtains its tokens
;;     from the specified lexer.
;;   
;;   TOKEN-STREAM-CONSUME (token-stream)
;;     Consumes and returns the next token from the token stream's
;;     underlying lexer.
;;   
;;   TOKEN-STREAM-PEEK (token-stream)
;;     Returns without consumption the next token from the token
;;     stream's underlying lexer.
;;   
;;   TOKEN-STREAM-EXPECT (token-stream expected-token-type)
;;     Checks whether the next token in the TOKEN-STREAM conforms to the
;;     expected token type. Upon affirmation the probed token is
;;     returned without its consumption --- akin to the
;;     TOKEN-STREAM-PEEK function. In the case of a mismatch, an error
;;     is signaled.
;;     This function may be regarded as a convenience operation.
;;   
;;   TOKEN-STREAM-EAT (token-stream expected-token-type)
;;     Checks whether the next token in the TOKEN-STREAM conforms to the
;;     expected token type. Upon affirmation the probed token is
;;     consumed and returned--- akin to the TOKEN-STREAM-CONSUME
;;     function. In the case of a mismatch, an error is signaled.
;;     This function may be regarded as a convenience operation.
;; 
;; A parlecue shall augment our memory:
;; 
;;   - The LEXER provides each next token.
;;   - The TOKEN STREAM stores the current lexer token and the
;;     subsequent token.
;;     o The latter may be queried for a look-ahead or peeking.
;;     o Both may be probed for the correct token type.
;; 
;; == PARSER: TOKEN ASSEMBLER INTO NODES ==
;; The ultimity of confluence from all preceding entities, the parser,
;; almost in its entirety a descendent from Pratt's notions in
;; conjunction with Crockford's statement extensions, assembles the
;; token stream's token objects into an abstract syntax tree's (AST)
;; subtrees or nodes.
;; 
;;   - The LEXER provides each next token.
;;   - The TOKEN STREAM stores the current lexer token and the
;;     subsequent token.
;;     o The latter may be queried for a look-ahead or peeking.
;;     o Both may be probed for the correct token type.
;;   - The PARSER assembles the tokens queried from token stream into an
;;     abstract syntax tree (AST), composed of nodes whose summit
;;     comprehends the complete program as its root.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-06-23
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
;;   [esolang2020Phile]
;;   The Esolang contributors, "Phile", 2020
;;   URL: "https://esolangs.org/wiki/Phile"
;;   Notes:
;;     - Specification of the Phile esoteric programming language.
;;   
;;   [esolang2021Mirrormachine]
;;   The Esolang contributors, "Mirror-machine", 2021
;;   URL: "https://esolangs.org/wiki/Mirror-machine"
;;   Notes:
;;     - Specification of the "Mirror-machine", a program for testing
;;       certain programming language facilities, such as addition,
;;       subtraction, iteration, input, and output.
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
;;   [stackoverflow2009q1279779]
;;   URL: "https://stackoverflow.com/questions/1279779/
;;         what-is-the-difference-between-r-and-n"
;;   Notes:
;;     - Discusses the difference betwixt the escape sequences "\r" and
;;       "\n".
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE."
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

(deftype attribute-list ()
  "The ``attribute-list'' type defines a particular ilk of the property
   list, requiring the indicators, or keys, to be keyword symbols,
   whereas any object may pose as the associated value."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (evenp (length (the list object)))
            (loop
              for (attribute-name attribute-value)
                of-type (T T)
                on      (the list object)
                by      #'cddr
              always
                (and (typep attribute-name  'keyword)
                     (typep attribute-value T))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype phile-program ()
  "The ``phile-program'' type defines an executable Phile program as a
   vector of zero or more ``Node'' objects."
  '(vector Node *))

;;; -------------------------------------------------------

(deftype pfile-type ()
  "The ``pfile-type'' type enumerates the subdivision of file categories
   recognized by the Phile programming language."
  '(member :stdin :stdout :stderr :normal))

;;; -------------------------------------------------------

(deftype phile-object ()
  "The ``phile-object'' type defines the kinds of objects in currency
   during a Phile program, which embraces integers and strings."
  '(or integer string))

;;; -------------------------------------------------------

(deftype file-table ()
  "The ``file-table'' type defines a mapping of file names to actual
   files in the form of a hash table, the keys of which are assigned
   the indicative purpose, whereas the values provide the file objects."
  '(hash-table-of string PFile))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant constituent of a Phile
   program."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global tokens.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal)
  "Maintains an association of the Phile language keyword names to
   representative token objects.")

;;; -------------------------------------------------------

(flet ((add-identifier (name token)
        "Associates the NAME with the TOKEN in the +IDENTIFIERS+ table
         and returns no value.
         ---
         Exant entries with the NAME as a key will be silently
         superseded."
        (declare (type string name))
        (declare (type Token  token))
        (setf (gethash name +IDENTIFIERS+) token)
        (values)))
  (add-identifier "CLOSE"     (make-token :close     "CLOSE"))
  (add-identifier "OPEN"      (make-token :open      "OPEN"))
  (add-identifier "OVERWRITE" (make-token :overwrite "OVERWRITE"))
  (add-identifier "READ"      (make-token :read      "READ"))
  (add-identifier "WRITE"     (make-token :write     "WRITE"))
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (name)
  "Returns the token associated with the NAME, or signals an error of an
   unspecified type if no correspondence exists."
  (declare (type string name))
  (the Token
    (or (gethash name +IDENTIFIERS+)
        (error "Unrecognized identifier name: ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Phile source code to tokenize.")
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
    "The ``Lexer'' class provides a unit capable of detecting
     significant objects in a piece of Phile source code and returning
     the same as tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initializes the LEXER's current character and returns no value."
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
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character in its source, updates its
   internal state, and returns the modified LEXER."
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

(defun lexer-move-to (lexer new-position)
  "Relocates the LEXER's position cursor to the NEW-POSITION, updates
   its internal state, and returns the modified LEXER."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf position new-position)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-characters-follow-p (lexer expected-string)
  "Checks whether the next characterS in the LEXER's source amount to
   the EXPECTED-STRING, on confirmation relocating its position cursor
   to the first unmatched location and returning a ``boolean'' value of
   ``T'', otherwise returning the cursor to the state preceding this
   operation's invocation, while returning ``NIL''."
  (declare (type Lexer  lexer))
  (declare (type string expected-string))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (let ((return-position position))
      (declare (type fixnum return-position))
      (the boolean
        (loop
          for expected-character
            of-type character
            across  expected-string
          do
            (cond
              ((and character (char= character expected-character))
                (lexer-advance lexer))
              (T
                (lexer-move-to lexer return-position)
                (return NIL)))
          finally
            (return T))))))

;;; -------------------------------------------------------

(defun lexer-read-integer (lexer)
  "Starting at the current position into the LEXER's source, reads a
   signed integer number and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :integer
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Starting at the current position into the LEXER's source, reads a
   string and returns a token representation thereof."
  (declare (type Lexer lexer))
  (lexer-advance lexer)
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :string
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop do
            (case character
              ((NIL)
                (error "Unterminated string literal."))
              (#\"
                (lexer-advance lexer)
                (loop-finish))
              (#\\
                (lexer-advance lexer)
                (case character
                  ((NIL)
                    (error "Unterminated escape sequence."))
                  (#\"
                    (write-char #\" content)
                    (lexer-advance lexer))
                  (#\n
                    (write-char #\Newline content)
                    (lexer-advance lexer))
                  (#\r
                    (lexer-advance lexer)
                    (cond
                      ;; Sequence is "\r\n"?
                      ;; => Add single newline.
                      ((lexer-characters-follow-p lexer "\\n")
                        (write-char #\Newline content))
                      ;; "\r" not followed by "\n"?
                      ;; => Escaped literal "r".
                      (T
                        (write-char #\r content)
                        (lexer-advance lexer))))
                  (#\\
                    (write-char #\\ content)
                    (lexer-advance lexer))
                  (#\t
                    (write-char #\Tab content)
                    (lexer-advance lexer))
                  (otherwise
                    (write-char character content)
                    (lexer-advance lexer))))
              (otherwise
                (write-char character content)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current position into the LEXER's source, reads an
   identifier and returns a token representation thereof.
   ---
   An error of an unspecified type is signaled if the consumed
   identifier cannot be recognized as a Phile language keyword."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (get-identifier-token
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop while (and character (alpha-char-p character)) do
            (write-char character identifier)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Starting at the current position into the LEXER's source, skips a
   sequence of zero or more subsequent space characters and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-comment-follows-p (lexer)
  "Checks whether, starting at the current position into the LEXER's
   source, a comment section starts, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (flet ((slash-at-position-p (offset)
            "Checks whether the character located at the OFFSET from the
             current POSITION into the SOURCE exists and constitutes a
             slash (\"/\"), returning on confirmation a ``boolean''
             value of ``T'', otherwise ``NIL''."
            (declare (type fixnum offset))
            (let ((peek-position (+ position offset)))
              (declare (type fixnum peek-position))
              (the boolean
                (not (null
                  (and (array-in-bounds-p source peek-position)
                       (char= (char source peek-position) #\/))))))))
      (the boolean
        (and (slash-at-position-p 0)
             (slash-at-position-p 1)
             (slash-at-position-p 2))))))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current position into the LEXER's source, skips a
   comment section and relocates its cursor to the position immediately
   following the processed line, finally returning the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop until (or (null character) (char= character #\Newline)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns from the LEXER the next token.
   ---
   Upon its exhaustion, the LEXER responds to any request with a fresh
   instance of an end-of-file (EOF) token.
   ---
   If an unrecognized character or token is encountered, an error of an
   unspecified type is signaled."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((lexer-comment-follows-p lexer)
          (lexer-skip-comment   lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\Newline)
          (prog1
            (make-token :newline character)
            (lexer-advance lexer)))
        
        ((space-character-p character)
          (lexer-skip-spaces    lexer)
          (lexer-get-next-token lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        ((digit-char-p character)
          (lexer-read-integer lexer))
        
        ((char= character #\")
          (lexer-read-string lexer))
        
        ((char= character #\+)
          (prog1
            (make-token :plus character)
            (lexer-advance lexer)))
        
        ((char= character #\-)
          (prog1
            (make-token :minus character)
            (lexer-advance lexer)))
        
        ((char= character #\*)
          (prog1
            (make-token :times character)
            (lexer-advance lexer)))
        
        ((char= character #\/)
          (prog1
            (make-token :slash character)
            (lexer-advance lexer)))
        
        ((char= character #\=)
          (prog1
            (make-token :equal character)
            (lexer-advance lexer)))
        
        ((char= character #\!)
          (prog1
            (make-token :not-equal character)
            (lexer-advance lexer)))
        
        ((char= character #\<)
          (prog1
            (make-token :less-than character)
            (lexer-advance lexer)))
        
        ((char= character #\>)
          (prog1
            (make-token :greater-than character)
            (lexer-advance lexer)))
        
        ((char= character #\?)
          (prog1
            (make-token :question-mark character)
            (lexer-advance lexer)))
        
        ((char= character #\;)
          (prog1
            (make-token :semicolon character)
            (lexer-advance lexer)))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Stream ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing token stream lexer.")
    :type          Lexer
    :documentation "The lexer responsible for the token purveyance.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recent token obtained from the LEXER.")
   (next-token
    :initarg       :next-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The next token obtained from the LEXER, buffered for
                    look-ahead (peeking) operations."))
  (:documentation
    "The ``Token-Stream'' class defines a supply of tokens in an
     abstract way."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((token-stream Token-Stream) &key)
  "Queries the first two tokens from the internally managed lexer,
   stores these as the current and next token respectively, and returns
   the modified TOKEN-STREAM."
  (declare (type Token-Stream token-stream))
  (with-slots (lexer current-token next-token) token-stream
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (declare (type Token next-token))
    (setf current-token (lexer-get-next-token lexer))
    (setf next-token    (lexer-get-next-token lexer)))
  (the Token-Stream token-stream))

;;; -------------------------------------------------------

(defun make-token-stream (lexer)
  "Creates and returns a new ``Token-Stream'' which receives its tokens
   from the LEXER."
  (declare (type Lexer lexer))
  (the Token-Stream
    (make-instance 'Token-Stream :lexer lexer)))

;;; -------------------------------------------------------

(defun token-stream-consume (token-stream)
  "Returns the next token from the TOKEN-STREAM's underlying lexer."
  (declare (type Token-Stream token-stream))
  (with-slots (lexer current-token next-token) token-stream
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (declare (type Token next-token))
    (the Token
      (prog1
        current-token
        (shiftf current-token next-token
          (lexer-get-next-token lexer))))))

;;; -------------------------------------------------------

(defun token-stream-peek (token-stream)
  "Returns without consuming the next token from the TOKEN-STREAM's
   underlying lexer."
  (declare (type Token-Stream token-stream))
  (the Token (slot-value token-stream 'current-token)))

;;; -------------------------------------------------------

(defun token-stream-expect (token-stream expected-token-type)
  "Checks whether the next token in the TOKEN-STREAM conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning without consumption
   this token; otherwise signaling an error of an unspecified type.
   ---
   Upon desideration of a matching token's concomitant consumption,
   please contemplate the ``token-stream-eat'' operation as an
   alternative."
  (declare (type Token-Stream token-stream))
  (declare (type keyword      expected-token-type))
  (with-slots (current-token) token-stream
    (declare (type Token current-token))
    (unless (token-type-p current-token expected-token-type)
      (error "Expected a token of the type ~s, ~
              but encountered the token ~s."
        expected-token-type current-token))
    (the Token current-token)))

;;; -------------------------------------------------------

(defun token-stream-eat (token-stream expected-token-type)
  "Checks whether the next token in the TOKEN-STREAM conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation consuming and returning this
   token; otherwise signaling an error of an unspecified type.
   ---
   Upon desideration of a matching token's perquisition without
   concomitant consumption, please contemplate the
   ``token-stream-expect'' operation as an alternative."
  (declare (type Token-Stream token-stream))
  (declare (type keyword      expected-token-type))
  (with-slots (current-token) token-stream
    (declare (type Token current-token))
    (the Token
      (if (token-type-p current-token expected-token-type)
        (token-stream-consume token-stream)
        (error "Expected a token of the type ~s, ~
                but encountered the token ~s."
          expected-token-type current-token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class represents a node or subtree in an abstract syntax
  tree (AST), defined by its categorizing type and a set of attributes."
  (type
    (error "Missing node type.")
    :type keyword)
  (attributes
    (make-hash-table :test #'eq)
    :type (hash-table-of keyword T)))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' of the TYPE, optionally described
   by the INITIAL-ATTRIBUTES as a flat list of even size providing the
   entries, with each attribute name followed by the associated value."
  (declare (type keyword        type))
  (declare (type attribute-list initial-attributes))
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
  "Returns the value associated with the ATTRIBUTE-NAME in the NODE, or
   ``NIL'' if no correspondence exists."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (the T (gethash attribute-name (node-attributes node))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (new-attribute-value node attribute-name)
  "Associates the NEW-ATTRIBUTE-VALUE with the ATTRIBUTE-NAME in the
   NODE and returns the modified NODE.
   ---
   Any extant entry amenable to the ATTRIBUTE-NAME will be replaced
   silently."
  (declare (type T       new-attribute-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (node-attributes node))
        new-attribute-value)
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "Node(type=~s, attributes=[" (node-type node))
  (loop
    for attribute-name
      of-type keyword
      being the hash-key in (node-attributes node)
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
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;====================================================================;;
;; Parser: Declaration of types.                                      ;;
;;====================================================================;;

(deftype statement-parselet ()
  "The ``statement-parselet'' type defines a function intended for the
   local parsing of a single token representing a statement's
   introduction.
   ---
   This function expects a token stream for the contingent processing of
   subsequent expressions, as well as the statement token itself. The
   return value must be a node encapsulating the thus generated
   statement. As a corollary, the signature conforms to:
     lambda (Token-Stream Token) => Node"
  '(function (Token-Stream Token) Node))

;;; -------------------------------------------------------

(deftype initial-parselet ()
  "The ``initial-parselet'' type defines a function intended for the
   local parsing of a single token, either representing an operand or a
   prefix operator.
   ---
   The function expects a token stream for the contingent processing of
   subsequent expressions in the case of the initial parselet's
   employment as a prefix operator, and the operand or prefix operator
   token itself. The return value must be a node encapsulating the thus
   generated operand or prefix expression. As a corollary, the signature
   conforms to:
     lamda (Token-Stream Token) => Node
   ---
   In the nomenclature consilient with Pratt's terminology, this
   parselet applies itself to the \"nud\" function."
  '(function (Token-Stream Token) Node))

;;; -------------------------------------------------------

(deftype consequent-parselet ()
  "The ``consequent-parselet'' type defines a function intended for the
   local parsing of a token dependent on the preceding (left-hand)
   subexpression, and contingently on a succeeding (right-hand)
   sub-expression, that is, a token representing a unary or binary infix
   operator.
   ---
   The function expects a token stream for the contingent processing of
   subsequent expressions in the case of the consequential parselet's
   employment as a binary infix operator, the previously parsed
   preceding node as the left operand to this operator, and the infix
   operator token itself. The return value must be a node encapsulating
   the thus generated infix operator expression. As a corollary, the
   signature conforms to:
     lambda (Token-Stream Node Token) => Node
   ---
   In the nomenclature consilient with Pratt's terminology, this
   parselet applies itself to the \"led\" function."
  '(function (Token-Stream Node Token) Node))


;;====================================================================;;
;; Parser: Global variables and constants.                            ;;
;;====================================================================;;

(declaim (type (hash-table-of keyword statement-parselet)
         *statement-parselets*))
(declaim (type (hash-table-of keyword initial-parselet)
         *initial-parselets*))
(declaim (type (hash-table-of keyword consequent-parselet)
         *consequent-parselets*))

;;; -------------------------------------------------------

(defparameter *statement-parselets*  (make-hash-table :test #'eq)
  "Associates with tokens representing statements the
   ``statement-parselet'' objects employed in their transformation into
   a node.")

(defparameter *initial-parselets*    (make-hash-table :test #'eq)
  "Associates with tokens representing operands the
   ``initial-parselet'' objects employed in their transformation into
   a node.")

(defparameter *consequent-parselets* (make-hash-table :test #'eq)
  "Associates with tokens representing operators the
   ``consequent-parselet'' objects employed in their transformation into
   a node.")


;;====================================================================;;
;; Parser: Function prototypes.                                       ;;
;;====================================================================;;

(declaim (ftype (function (Token-Stream integer) Node)
                parse-expression))
(declaim (ftype (function (Token-Stream) Node)
                parse-statement))
(declaim (ftype (function (Token-Stream) phile-program)
                parse-statements))


;;====================================================================;;
;; Parser: Token precedence and associativity.                        ;;
;;====================================================================;;

(defun get-binding-power (token)
  "Returns the binding power associated with the TOKEN, or signals an
   error of an unspecified type upon its absence."
  (declare (type Token token))
  (the integer
    (case (token-type token)
      (:question-mark  5)
      
      (:read          90)
      (:times         80)
      (:slash         70)
      (:plus          60)
      (:minus         50)
      (:equal         40)
      (:not-equal     30)
      (:less-than     20)
      (:greater-than  10)
      (:eof            0)
      (otherwise
        (error "No binding power defined for the token ~s." token)))))

;;; -------------------------------------------------------

#|
(defun is-left-associative (token)
  (declare (type Token token))
  (declare (ignore     token))
  (the boolean T))
|#


;;====================================================================;;
;; Parser: Appurtenance functions.                                    ;;
;;====================================================================;;

(defun parser-expect-line-terminator (tokens)
  "Checks whether the closure of a statement, that is, either a newline
   or an end-of-file (EOF) token, follows in the token stream TOKENS, in
   the first case consuming the token, in the second proceeding without
   modifications, and in both cases returning the modified TOKENS. Upon
   a mismatch an error of an unspecified type is signaled."
  (declare (type Token-Stream tokens))
  (token-stream-eat tokens :semicolon)
  (let ((next-token (token-stream-peek tokens)))
    (declare (type Token next-token))
    (unless (token-type-p next-token :eof)
      (token-stream-eat tokens :newline)))
  (the Token-Stream tokens))


;;====================================================================;;
;; Parser: Statement token parselets.                                 ;;
;;====================================================================;;

(defun statement-token-p (token)
  "Checks whether a statement parselet is associated with the TOKEN,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) *statement-parselets*))))))

;;; -------------------------------------------------------

(defun get-statement-parselet (token)
  "Returns the statement parselet associated with the TOKEN, or ``NIL''
   if no such correspondence holds."
  (declare (type Token token))
  (the (or null function)
    (nth-value 0
      (gethash (token-type token) *statement-parselets*))))

;;; -------------------------------------------------------

(defun register-statement-parselet (token-type parselet)
  "Associates the statement PARSELET with the TOKEN-TYPE in the
   *STATEMENT-PARSELETS* table and returns no value.
   ---
   Any entry extant for the TOKEN-TYPE will be silently superseded."
  (declare (type keyword            token-type))
  (declare (type statement-parselet parselet))
  (setf (gethash token-type *statement-parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-simple-statement-parselet (token-type)
  "Associates with the TOKEN-TYPE an automatically generated statement
   parselet which upon its invocation returns a new ``Node'' of a type
   equal to the TOKEN-TYPE, and containing an attribute ``filename''
   associated with the following string. This function itself returns no
   value.
   ---
   The Extended Backus-Naur Form (EBNF) description resolves to:
     TOKEN-TYPE , fname , ';' ;
   This concept matches the following choices:
     ( 'OPEN' | 'CLOSE' ) , fname , ';' ;"
  (declare (type keyword token-type))
  (register-statement-parselet token-type
    #'(lambda (tokens token)
        (declare (type Token-Stream tokens))
        (declare (type Token        token))
        (declare (ignore            token))
        (let ((filename (token-stream-eat tokens :string)))
          (declare (type Token filename))
          (parser-expect-line-terminator tokens)
          (the Node
            (make-node token-type
              :filename (token-value filename)))))))

;;; -------------------------------------------------------

(defun register-dyadic-statement-parselet (token-type)
  "Associates with the TOKEN-TYPE an automatically generated statement
   parselet which upon its invocation returns a new ``Node'' of a type
   equal to the TOKEN-TYPE, containing an attribute ``filename'' set to
   following string and a ``value'' attribute containing an arbitrary
   expression. This function itself returns no value.
   ---
   The Extended Backus-Naur Form (EBNF) description resolves to:
     TOKEN-TYPE , fname , value , ';' ;
   This concept matches the following choices:
     ( 'OVERWRITE' | 'WRITE' ) , fname , value , ';' ;"
  (declare (type keyword token-type))
  (register-statement-parselet token-type
    #'(lambda (tokens token)
        (declare (type Token-Stream tokens))
        (declare (type Token        token))
        (declare (ignore            token))
        (let ((filename (token-stream-expect tokens :string)))
          (declare (type Token filename))
          (token-stream-consume tokens)
          (let ((value (parse-expression tokens 0)))
            (declare (type Node value))
            (parser-expect-line-terminator tokens)
            (the Node
              (make-node token-type
                :filename (token-value filename)
                :value    value)))))))


;;====================================================================;;
;; Parser: Operand (initial) token parselets.                         ;;
;;====================================================================;;

(defun operand-token-p (token)
  "Checks whether the TOKEN represents an operand or prefix operator,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) *initial-parselets*))))))

;;; -------------------------------------------------------

(defun get-initial-parselet (token)
  "Returns the initial parselet associated with the TOKEN, or ``NIL'' if
   no correspondence exists."
  (declare (type Token token))
  (the (or null function)
    (nth-value 0
      (gethash (token-type token) *initial-parselets*))))

;;; -------------------------------------------------------

(defun register-nud-parselet (token-type parselet)
  "Associates the initial or \"nud\" PARSELET with the TOKEN-TYPE in the
   *INITIAL-PARSELETS* table and returns no value.
   ---
   Any entry extant for the TOKEN-TYPE will be silently superseded."
  (declare (type keyword          token-type))
  (declare (type initial-parselet parselet))
  (setf (gethash token-type *initial-parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defmacro define-nud-parselet
    (token-type
     (token-stream-variable token-variable)
     &body body)
  "Associates the evaluated TOKEN-TYPE with an automatically generated
   parselet which accepts the TOKEN-STREAM-VARIABLE for its token
   stream input and the TOKEN-VARIABLE for the nud token to convert,
   evaluating the BODY forms and returning the last evaluated form's
   result, which should be a ``Node'' representation of the token.
   ---
   This macro offers a more convenient alternative to the function
   ``register-nud-parselet'', whose service itself consumes. In addition
   to the reduction in the code quantity, the abstraction benefits
   provide a scant glimpse of a domain-specific language (DSL)."
  (let ((evaluated-token-type (gensym)))
    (declare (type symbol evaluated-token-type))
    `(let ((,evaluated-token-type ,token-type))
       (declare (type keyword ,evaluated-token-type))
       (register-nud-parselet ,evaluated-token-type
         #'(lambda (,token-stream-variable ,token-variable)
             (declare (type Token-Stream ,token-stream-variable))
             (declare (type Token        ,token-variable))
             (declare (ignorable         ,token-stream-variable))
             (declare (ignorable         ,token-variable))
             ,@body)))))


;;====================================================================;;
;; Parser: Operator (consequent) token parselets.                     ;;
;;====================================================================;;

(defun consequent-parselet-p (token)
  "Checks whether a consequent parselet is defined for the TOKEN,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) *consequent-parselets*))))))

;;; -------------------------------------------------------

(defun get-consequent-parselet (token)
  "Returns the consequent parselet for the TOKEN, or ``NIL'' if none
   corresponds to the same."
  (declare (type Token token))
  (the (or null function)
    (nth-value 0
      (gethash (token-type token) *consequent-parselets*))))

;;; -------------------------------------------------------

(defun register-consequent-parselet (token-type parselet)
  "Associates the consequent PARSELET with the TOKEN-TYPE in the
   *CONSEQUENT-PARSELETS* table and returns no value.
   ---
   Any entry extant for the TOKEN-TYPE will be silently superseded."
  (declare (type keyword             token-type))
  (declare (type consequent-parselet parselet))
  (setf (gethash token-type *consequent-parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-binary-operation-parselet (token-type operator)
  "Associates a consequent parselet conforming to a binary expression's
   structure in a convenient way with the TOKEN-TYPE, the invocation of
   which produces a new ``Node'' of the type ``binary-operation'',
   with an ``operator'' attribute set to the OPERATOR in conjunction
   with a ``left-operand'' being the left node and a ``right-operand''
   parsed as an expression. This function itself returns no value."
  (declare (type keyword token-type))
  (declare (type keyword operator))
  (register-consequent-parselet token-type
    #'(lambda (tokens left-node token)
        (declare (type Token-Stream tokens))
        (declare (type Node         left-node))
        (declare (type Token        token))
        (the Node
          (make-node :binary-operation
            :operator      operator
            :left-operand  left-node
            :right-operand (parse-expression tokens
                             (get-binding-power token)))))))


;;====================================================================;;
;; Parser: Definition of parselets.                                   ;;
;;====================================================================;;

(register-simple-statement-parselet :CLOSE)
(register-simple-statement-parselet :OPEN)
(register-dyadic-statement-parselet :OVERWRITE)
(register-dyadic-statement-parselet :WRITE)

;;; -------------------------------------------------------

;; Unsigned integer operand.
(register-nud-parselet :integer
  #'(lambda (tokens token)
      (declare (type Token-Stream tokens))
      (declare (type Token        token))
      (declare (ignore            tokens))
      (the Node
        (make-node :integer
          :value (token-value token)))))

;;; -------------------------------------------------------

;; A string literal as an operand.
;; ---
;; This registration instance utilizes the macro ``define-nud-parselet''
;; in lieu of the equipollent ``register-nud-parselet'' for the deictic
;; purpose of demonstrating Common Lisp's macro facility's convenience.
;; The functional equivalent would resolve to the more verbose
;; 
;;   (register-nud-parselet :string
;;     #'(lambda (tokens token)
;;         (declare (type Token-Stream tokens))
;;         (declare (type Token        token))
;;         (declare (ignore            tokens))
;;         (the Node
;;           (make-node :string
;;             :value (token-value token)))))
;; 
(define-nud-parselet :string (tokens token)
  (the Node
    (make-node :string
      :value (token-value token))))

;;; -------------------------------------------------------

;; Plus as a prefix operator preceding an integer token.
(register-nud-parselet :plus
  #'(lambda (tokens token)
      (declare (type Token-Stream tokens))
      (declare (type Token        token))
      (declare (ignore            token))
      (let ((integer (token-value (token-stream-eat tokens :integer))))
        (declare (type integer integer))
        (the Node
          (make-node :integer
            :value integer)))))

;;; -------------------------------------------------------

;; Minus as a prefix operator preceding an integer token.
;; ---
;; This prefix parselet is implemented using the ``define-nud-parselet''
;; for demonstrative purposes. The alternative using the function
;; ``register-nud-parselet'' would constitute the following:
;; 
;;   (register-nud-parselet :minus
;;     #'(lambda (tokens token)
;;         (declare (type Token-Stream tokens))
;;         (declare (type Token        token))
;;         (declare (ignore            token))
;;         (let ((integer (token-value
;;                          (token-stream-eat tokens :integer))))
;;           (declare (type integer integer))
;;           (the Node
;;             (make-node :integer
;;               :value (- integer))))))
;; 
(define-nud-parselet :minus (tokens token)
  (let ((integer (token-value (token-stream-eat tokens :integer))))
    (declare (type integer integer))
    (the Node
      (make-node :integer
        :value (- integer)))))

;;; -------------------------------------------------------

(register-nud-parselet :read
  #'(lambda (tokens token)
      (declare (type Token-Stream tokens))
      (declare (type Token        token))
      (declare (ignore            token))
      (let ((filename (token-stream-expect tokens :string)))
        (declare (type Token filename))
        (token-stream-consume tokens)
        (the Node
          (make-node :read
            :filename (token-value filename))))))

;;; -------------------------------------------------------

(register-binary-operation-parselet :plus         :addition)
(register-binary-operation-parselet :minus        :subtraction)
(register-binary-operation-parselet :times        :multiplication)
(register-binary-operation-parselet :slash        :division)
(register-binary-operation-parselet :equal        :equal)
(register-binary-operation-parselet :not-equal    :not-equal)
(register-binary-operation-parselet :less-than    :less-than)
(register-binary-operation-parselet :greater-than :greater-than)

;;; -------------------------------------------------------

(register-consequent-parselet :question-mark
  #'(lambda (tokens left-node token)
      (declare (type Token-Stream tokens))
      (declare (type Node         left-node))
      (declare (type Token        token))
      (let ((line-number
              (parse-expression tokens
                (get-binding-power token))))
        (declare (type Node line-number))
        (the Node
          (make-node :jump-if
            :condition   left-node
            :line-number line-number)))))


;;====================================================================;;
;; Parser: Parsing functions.                                         ;;
;;====================================================================;;

(defun parse-expression (tokens current-binding-power)
  "Parses an expression using the TOKENS, with the calling environment,
   usually an operator, being represented by the left binding power
   value CURRENT-BINDING-POWER."
  (declare (type Token-Stream tokens))
  (declare (type integer      current-binding-power))
  
  (let ((initial-token    NIL)
        (initial-parselet NIL)
        (left-node        NIL))
    (declare (type (or null Token)            initial-token))
    (declare (type (or null initial-parselet) initial-parselet))
    (declare (type (or null Node)             left-node))
    
    ;; Obtain the operand token from the token stream.
    (setf initial-token    (token-stream-consume tokens))
    ;; Attempt to obtain a parselet for the operand token.
    (setf initial-parselet (get-initial-parselet initial-token))
    
    ;; No initial parselet exists fro the INITIAL-TOKEN?
    ;; => The INITIAL-TOKEN does not represent a valid operand.
    (unless initial-parselet
      (error "No initial token: ~s." initial-token))
    
    ;; Evaluate the operand token using its associated parselet, and
    ;; return the thus created node.
    (setf left-node (funcall initial-parselet tokens initial-token))
    
    (loop for next-token of-type Token = (token-stream-peek tokens) do
      (cond
        ;; End of the source code reached?
        ;; => The calling operator receives the LEFT-NODE without
        ;;    further inspection.
        ((token-type-p next-token :eof)
          (loop-finish))
        
        ;; No operator follows?
        ;; => The calling operator receives the LEFT-NODE without
        ;;    further inspection.
        ((not (consequent-parselet-p next-token))
          (loop-finish))
        
        ;; Right operator exists, but is not stronger than the calling
        ;; operator?
        ;; => Finish and thus cede the LEFT-NODE to the calling
        ;;    operator.
        ((<= (get-binding-power next-token) current-binding-power)
          (loop-finish))
        
        ;; Right operator exists and is stronger than the calling
        ;; operator?
        ;; => Use the LEFT-NODE as the left operand for this right
        ;;    operator in order to return the thus generated node or
        ;;    subtree to the calling operator.
        (T
          (let ((consequent-parselet
                  (get-consequent-parselet next-token)))
            (declare (type consequent-parselet consequent-parselet))
            (token-stream-consume tokens)
            (setf left-node
              (funcall consequent-parselet tokens
                                           left-node
                                           next-token))))))
    
    (the Node left-node)))

;;; -------------------------------------------------------

(defun parse-statement (tokens)
  "Expected to be positioned at the beginning of a line, utilizes the
   TOKENS to either parse a statement or, if not possible, an
   expression, and returns a node representation of the effort."
  (declare (type Token-Stream tokens))
  (let ((initial-token (token-stream-peek tokens)))
    (declare (type Token initial-token))
    (the Node
      (cond
        ;; Does the INITIAL-TOKEN introduce a statement?
        ((statement-token-p initial-token)
          (token-stream-consume tokens)
          (let ((statement-parselet (get-statement-parselet initial-token)))
            (declare (type statement-parselet statement-parselet))
            (funcall statement-parselet tokens initial-token)))
        
        ;; Does the INITIAL-TOKEN introduce an expression?
        ((operand-token-p initial-token)
          (prog1
            (parse-expression tokens 0)
            (parser-expect-line-terminator tokens)))
        
        ;; Neither a statement nor an expression?
        ;; => Invalid token.
        (T
          (error "The token ~s does neither represent a statement nor ~
                  an operand."
            initial-token))))))

;;; -------------------------------------------------------

(defun parse-statements (tokens)
  "Parses a sequence of zero or more statements from the TOKENS and
   returns a one-dimensional simple array of node representations
   thereof."
  (declare (type Token-Stream tokens))
  (let ((statements NIL))
    (declare (type (list-of Node) statements))
    (loop do
      (case (token-type (token-stream-peek tokens))
        ;; End of program reached?
        ;; => Terminate.
        (:eof
          (loop-finish))
        
        ;; Newline found?
        ;; => Empty line.
        ;;    => Skip.
        (:newline
          (token-stream-consume tokens))
        
        ;; Start of a line?
        ;; => Expect and parse statement.
        (otherwise
          (push (parse-statement tokens) statements))))
    
    (the phile-program
      (map '(simple-array Node (*))
        #'(lambda (statement)
            (declare (type Node statement))
            (the Node
              (make-node :line :statement statement)))
        (nreverse statements)))))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Parses a Phile program by assembling the elements of the token stream
   TOKENS and returns a one-dimensional simple array of nodes
   representing its statements."
  (declare (type Token-Stream tokens))
  (the (simple-array Node (*))
    (parse-statements tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type bit +BOOLEAN-FALSE+))
(declaim (type bit +BOOLEAN-TRUE+))

;;; -------------------------------------------------------

(defparameter +BOOLEAN-FALSE+ 0)
(defparameter +BOOLEAN-TRUE+  1)

;;; -------------------------------------------------------

(defun get-binary-truth-value (object)
  "Returns a bit representing the Boolean equivalent of the OBJECT,
   responding with zero (0), for a Boolean \"false\", in the case of a
   ``NIL'' OBJECT, otherwise with one (1), for a Boolean \"true\"."
  (declare (type T object))
  (the bit
    (if object
      +BOOLEAN-TRUE+
      +BOOLEAN-FALSE+)))

;;; -------------------------------------------------------

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the OPERATOR to the LEFT-OPERAND and RIGHT-OPERAND and
     returns a value fitten for this combination."))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      T)
                                  (left-operand  T)
                                  (right-operand T))
  (declare (type T operator))
  (declare (type T left-operand))
  (declare (type T right-operand))
  (error "The combination of the operator ~s with the left operand ~s ~
          and the right operand ~s is not defined."
    operator left-operand right-operand))

;;; -------------------------------------------------------

(defmacro define-binary-operation
    ((left-operand-type operator right-operand-type)
     &body body)
  "Implements the generic function ``apply-binary-operator'' by
   dispatching on the LEFT-OPERAND-TYPE, OPERATOR, and
   RIGHT-OPERAND-TYPE, binding the symbol LEFT to the first, OPERATOR
   to the second, and RIGHT to the third, evaluates the BODY forms,
   and returns the last evaluated form's result."
  `(defmethod apply-binary-operator
       ((operator (eql ,operator))
        (left     ,left-operand-type)
        (right    ,right-operand-type))
     (declare (type ,left-operand-type  left))
     (declare (type keyword             ,operator))
     (declare (type ,right-operand-type right))
     (declare (ignorable                left))
     (declare (ignore                   operator))
     (declare (ignorable                right))
     ,@body))

;;; -------------------------------------------------------

(define-binary-operation (integer :addition integer)
  (the integer
    (+ left right)))

;;; -------------------------------------------------------

(define-binary-operation (integer :addition string)
  (the (or integer string)
    ;; 0 + string => string-to-integer conversion.
    (if (zerop left)
      (nth-value 0
        (parse-integer
          (format NIL "~d~a" left right)))
      (format NIL "~d~a" left right))))

;;; -------------------------------------------------------

(define-binary-operation (integer :subtraction integer)
  (the integer
    (- left right)))

;;; -------------------------------------------------------

(define-binary-operation (integer :multiplication integer)
  (the integer
    (* left right)))

;;; -------------------------------------------------------

(define-binary-operation (integer :division integer)
  (the integer
    (round left right)))

;;; -------------------------------------------------------

(define-binary-operation (integer :equal integer)
  (the bit
    (get-binary-truth-value
      (= left right))))

;;; -------------------------------------------------------

(define-binary-operation (integer :equal string)
  (the bit +BOOLEAN-FALSE+))

;;; -------------------------------------------------------

(define-binary-operation (integer :not-equal integer)
  (the bit
    (get-binary-truth-value
      (/= left right))))

;;; -------------------------------------------------------

(define-binary-operation (integer :not-equal string)
  (the bit +BOOLEAN-TRUE+))

;;; -------------------------------------------------------

(define-binary-operation (integer :less-than integer)
  (the bit
    (get-binary-truth-value
      (< left right))))

;;; -------------------------------------------------------

(define-binary-operation (integer :greater-than integer)
  (the bit
    (get-binary-truth-value
      (> left right))))

;;; -------------------------------------------------------

(define-binary-operation (string :addition integer)
  (the string
    (format NIL "~a~d" left right)))

;;; -------------------------------------------------------

(define-binary-operation (string :addition string)
  (the string
    (format NIL "~a~a" left right)))

;;; -------------------------------------------------------

(define-binary-operation (string :equal string)
  (the bit
    (get-binary-truth-value
      (string= left right))))

;;; -------------------------------------------------------

(define-binary-operation (string :equal integer)
  (the bit 0))

;;; -------------------------------------------------------

(define-binary-operation (string :not-equal string)
  (the bit
    (get-binary-truth-value
      (string/= left right))))

;;; -------------------------------------------------------

(define-binary-operation (string :not-equal integer)
  (the bit 1))

;;; -------------------------------------------------------

(define-binary-operation (string :less-than string)
  (the bit
    (get-binary-truth-value
      (string< left right))))

;;; -------------------------------------------------------

(define-binary-operation (string :greater-than string)
  (the bit
    (get-binary-truth-value
      (string> left right))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "PFile".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-string ()
  "Creates and returns an initially empty dynamic string, fitten for the
   appropriation as a file content."
  (the string
    (make-array 0
      :element-type    'character
      :initial-element #\Null
      :adjustable      T
      :fill-pointer    0)))

;;; -------------------------------------------------------

(defun integer-string-p (string)
  "Checks whether the STRING represents a signed integer number,
   returning on confirmation the parsed integer value, otherwise
   responding with ``NIL''."
  (declare (type string string))
  (the (or null integer)
    (ignore-errors
      (parse-integer string))))

;;; -------------------------------------------------------

(defun parse-phile-object (input)
  "Evaluates the INPUT and returns a ``phile-object'' appropriate for
   its content."
  (declare (type string input))
  (the phile-object
    (or
      ;; No input is construed as an empty string.
      (and (null input)           "")
      ;; An empty input corresponds to an empty string.
      (and (zerop (length input)) input)
      ;; An integer input is returned as its parsed value.
      (integer-string-p           input)
      ;; Any other input simply returns verbatim.
      input)))

;;; -------------------------------------------------------

(defun determine-file-type (name)
  "Determines and returns a file's type based upon its NAME."
  (declare (type string name))
  (the pfile-type
    (cond
      ((string= name "stdin.stream")  :stdin)
      ((string= name "stdout.stream") :stdout)
      ((string= name "stderr.stream") :stderr)
      (T                              :normal))))

;;; -------------------------------------------------------

(defstruct (PFile
  (:constructor make-pfile (name
                            &aux (type (determine-file-type name)))))
  "A ``PFile'' represents a file conforming to the Phile language's
   notion regarding the topic, determined by an identifying name and the
   content.
   ---
   An adminicle to facilitated castaldy, a ``PFile'' stores its content
   exclusively as a dynamically expandable string. When enquired about
   the same, a parsing operation determines the most eligible format for
   the ultimate delivery."
  (name    (error "Missing file name.") :type string)
  (type    :normal                      :type pfile-type)
  (content (make-dynamic-string)        :type string))

;;; -------------------------------------------------------

(defmacro with-pfile ((pfile) &body body)
  "Evaluates the PFILE, binds its slots ``name'' and ``content'' to
   eponymous symbol macros for general access, executes the BODY forms,
   and returns the last form's results."
  (let ((evaluated-pfile (gensym)))
    (declare (type symbol evaluated-pfile))
    `(let ((,evaluated-pfile ,pfile))
       (declare (type PFile ,evaluated-pfile))
       (declare (ignorable  ,evaluated-pfile))
       (symbol-macrolet
           ((name    (the string (pfile-name    ,evaluated-pfile)))
            (content (the string (pfile-content ,evaluated-pfile))))
         ,@body))))

;;; -------------------------------------------------------

(defun pfile-read (pfile)
  "Reads a line from the PFILE and returns its content in a form
   appropriate for the file type."
  (declare (type PFile pfile))
  (with-pfile (pfile)
    (the phile-object
      (case (pfile-type pfile)
        (:stdin
          (format T "~&Please input a value: ")
          (let ((input (read-line)))
            (declare (type string input))
            (clear-input)
            (parse-phile-object input)))
        ((:stdout :stderr)
          (error 'Invalid-Operation-Error
            :operation "read"
            :file      pfile))
        (otherwise
          (parse-phile-object
            (with-input-from-string (input-stream content)
              (read-line input-stream NIL ""))))))))

;;; -------------------------------------------------------

(defun pfile-write (pfile value)
  "Appends the VALUE to the end of the PFILE's content and returns the
   modified PFILE."
  (declare (type PFile        pfile))
  (declare (type phile-object value))
  (with-pfile (pfile)
    (case (pfile-type pfile)
      (:stdin
        (error 'Invalid-Operation-Error
          :operation "write"
          :file      pfile))
      ((:stdout :stderr)
        (format T "~a" value))
      (otherwise
        (format content "~a" value))))
  (the PFile pfile))

;;; -------------------------------------------------------

(defun pfile-overwrite (pfile value)
  "Replaces the content of the PFILE with the VALUE and returns the
   modified PFILE."
  (declare (type PFile        pfile))
  (declare (type phile-object value))
  (with-pfile (pfile)
    (case (pfile-type pfile)
      (:stdin
        (error 'Invalid-Operation-Error
          :operation "overwrite"
          :file      pfile))
      ((:stdout :stderr)
        (format T "~a" value))
      (otherwise
        (setf (fill-pointer content) 0)
        (format content "~a" value))))
  (the PFile pfile))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Phile-Error ()
  ()
  (:documentation
    "The ``Phile-Error'' condition serves as a common foundry for all
     conditions affiliated with the context of a Phile program's
     execution."))

;;; -------------------------------------------------------

(define-condition PFile-Error (Phile-Error)
  ()
  (:documentation
    "The ``PFile-Error'' conditions serves a common foundry for all
     conditions affiliated with the context of file access and
     manipulations."))

;;; -------------------------------------------------------

(define-condition Missing-File-Error (PFile-Error)
  ((file-name
    :initarg       :file-name
    :initform      (error "Missing file name.")
    :reader        missing-file-error-file-name
    :type          string
    :documentation "The name having been failed to match against an
                    actual ``PFile'' object."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-File-Error condition))
      (declare (type destination        stream))
      (format stream "No file with the name ~s could be detected."
        (missing-file-error-file-name condition))))
  (:documentation
    "The ``Missing-File-Error'' serves to signal an anomalous situation
     in which an attempt is committed to perform an operation on a file
     that at the instant of the invocation does not exist."))

;;; -------------------------------------------------------

(define-condition Duplicate-File-Error (PFile-Error)
  ((file-name
    :initarg       :file-name
    :initform      (error "Missing file name.")
    :reader        duplicate-file-error-file-name
    :type          string
    :documentation "The name having been failed to match against an
                    actual ``PFile'' object."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-File-Error condition))
      (declare (type destination          stream))
      (format stream "A file with the name ~s already exists."
        (duplicate-file-error-file-name condition))))
  (:documentation
    "The ``Duplicate-File-Error'' serves to signal an anomalous
     situation in which an attempt is committed to open a file whose
     name has already been assigned to a currently active file."))

;;; -------------------------------------------------------

(define-condition Invalid-Operation-Error (PFile-Error)
  ((operation
    :initarg       :operation
    :initform      (error "Missing offending operation.")
    :reader        invalid-operation-error-operation
    :type          keyword
    :documentation "The attempted file operation having been deemed as
                    inappropriate for the offending FILE.")
   (file
    :initarg       :file
    :initform      (error "Missing offending PFile object.")
    :reader        invalid-operation-error-file
    :type          PFile
    :documentation "The ``PFile'' object whose misjudged collaboration
                    with the offending OPERATION has accompassed this
                    error."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Operation-Error condition))
      (declare (type destination             stream))
      (format stream "The operation ~s is not valid for the PFile ~s."
        (invalid-operation-error-operation condition)
        (invalid-operation-error-file      condition))))
  (:documentation
    "The ``Invalid-Operation-Error'' serves to signal an anomalous
     situation instigated by the attempt to apply an operation on a file
     unreceptive to such a request by cause of its nature, and thus
     violated in its purpose through the intention.
     ---
     Common patterns, void of exclusivity, involve the behest to read
     from an output stream or write to an input stream."))

;;; -------------------------------------------------------

(define-condition Unclosed-Files-Error (Phile-Error)
  ((files
    :initarg       :files
    :initform      NIL
    :reader        unclosed-files-error-files
    :type          (list-of PFile)
    :documentation "A list of the files not yet closed during a Phile
                    program's termination."))
  (:report
    (lambda (condition stream)
      (declare (type Unclosed-Files-Error condition))
      (declare (type destination          stream))
      (format stream "The program cannot be closed because the ~
                      following files are still open: ~{~s~^, ~}."
        (unclosed-files-error-files condition))))
  (:documentation
    "The ``Unclosed-Files-Error'' serves to signal an anomalous
     situation in which a program endeavours to terminate, yet one or
     more formerly opened files have not yet been closed."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file manager.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass File-Manager ()
  ((files
    :initarg       :files
    :initform      (make-hash-table :test #'equal)
    :type          file-table
    :documentation "Associates opened file names with their ``PFile''
                    objects."))
  (:documentation
    "The ``File-Manager'' class is assigned the onus of maintaining a
     collection of files, as simulated by the Phile programming
     language, and represented in the ``PFile'' class, with its entries
     being amenable to the file names."))

;;; -------------------------------------------------------

(defun make-file-manager ()
  "Returns a new empty ``File-Manager''."
  (the File-Manager
    (make-instance 'File-Manager)))

;;; -------------------------------------------------------

(defun file-manager-probe-file (file-manager name)
  "Determines whether a file with the NAME is registered at the
   FILE-MANAGER, on confirmation returning the respective ``PFile''
   object; otherwise the ``NIL'' value is delivered."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (the (or null PFile)
    (gethash name
      (slot-value file-manager 'files))))

;;; -------------------------------------------------------

(defun file-manager-ascertain-file-exists (file-manager name)
  "Determines whether a file with the NAME is registered at the
   FILE-MANAGER, on confirmation returning the respective ``PFile''
   object; otherwise an error of an unspecified type is signaled."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (the PFile
    (or (file-manager-probe-file file-manager name)
        (error 'Missing-File-Error :file-name name))))

;;; -------------------------------------------------------

(defun file-manager-open-file (file-manager name)
  "Opens a file with specified by the NAME, registers it at the
   FILE-MANAGER, and returns no value.
   ---
   An error of the type ``Duplicate-File-Error'' is signaled if a file
   with the NAME is open at the time of this operation's invocation."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (if (file-manager-probe-file file-manager name)
    (error 'Duplicate-File-Error :file-name name)
    (setf (gethash name (slot-value file-manager 'files))
          (make-pfile name)))
  (values))

;;; -------------------------------------------------------

(defun file-manager-read-file (file-manager name)
  "Reads a line from the file registered with the NAME at the
   FILE-MANAGE and returns it.
   ---
   An error of the type ``Missing-File-Error'' is signaled if the
   specified NAME does not exist.
   ---
   An error of the type ``Invalid-Operation-Error'' is signaled if the
   file does not homologate its inquisition."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (the phile-object
    (pfile-read
      (file-manager-ascertain-file-exists file-manager name))))

;;; -------------------------------------------------------

(defun file-manager-write-file (file-manager name value)
  "Appends the VALUE to the file registered with the NAME at the
   FILE-MANAGER and returns no value.
   ---
   An error of the type ``Missing-File-Error'' is signaled if the
   specified NAME does not exist.
   ---
   An error of the type ``Invalid-Operation-Error'' is signaled if the
   file does not homologate its modification."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (declare (type phile-object value))
  (pfile-write
    (file-manager-ascertain-file-exists file-manager name)
    value)
  (values))

;;; -------------------------------------------------------

(defun file-manager-overwrite-file (file-manager name value)
  "Replaces the content of the file registered with the NAME at the
   FILE-MANAGER by the VALUE and returns no value.
   ---
   An error of the type ``Missing-File-Error'' is signaled if the
   specified NAME does not exist.
   ---
   An error of the type ``Invalid-Operation-Error'' is signaled if the
   file does not homologate its modification."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (declare (type phile-object value))
  (pfile-overwrite
    (file-manager-ascertain-file-exists file-manager name)
    value)
  (values))

;;; -------------------------------------------------------

(defun file-manager-close-file (file-manager name)
  "Closes the file registered with the NAME at the FILE-MANAGER and
   returns no value.
   ---
   An error of the type ``Missing-File-Error'' is signaled if the
   specified NAME does not exist or if the file is already closed."
  (declare (type File-Manager file-manager))
  (declare (type string       name))
  (file-manager-ascertain-file-exists file-manager name)
  (remhash name (slot-value file-manager 'files))
  (values))

;;; -------------------------------------------------------

(defun file-manager-check-for-open-files (file-manager)
  "Determines whether all files registered at the FILE-MANAGER are
   closed, on confirmation returning no value; otherwise an error of the
   type ``Unclosed-Files-Error'' is signaled."
  (declare (type File-Manager file-manager))
  (with-slots (files) file-manager
    (declare (type file-table files))
    (unless (zerop (hash-table-count files))
      (error 'Unclosed-Files-Error :files
        (loop
          for file
            of-type PFile
            being the hash-values in files
          collect file))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of predicate operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 0)   +EMPTY-STRING+))
(declaim (type (simple-string 1)   +ZERO-STRING+))
(declaim (type (integer       0 0) +ZERO-INTEGER+))

;;; -------------------------------------------------------

(defparameter +EMPTY-STRING+ ""
  "The empty string of zero length.")

(defparameter +ZERO-STRING+  "0"
  "The string containing the digit zero (0) a its single character.")

(defparameter +ZERO-INTEGER+ 0
  "The integer value zero (0).")

;;; -------------------------------------------------------

(defun positive-string-p (value)
  "Checks whether the VALUE represents a string tantamount to a Boolean
   true, that is, either being the empty string \"\" or the zero string
   \"0\", and returns on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type phile-object value))
  (the boolean
    (not (null
      (and
        (stringp value)
        (not (string= value +EMPTY-STRING+))
        (not (string= value +ZERO-STRING+)))))))

;;; -------------------------------------------------------

(defun non-zero-integer-p (value)
  "Checks whether the VALUE represents an integer tantamount to a
   Boolean true, that is, the value zero (0), and returns on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type phile-object value))
  (the boolean
    (not (null
      (and
        (integerp value)
        (not (= value +ZERO-INTEGER+)))))))

;;; -------------------------------------------------------

(defun condition-satisfied-p (condition-value)
  "Checks whether the CONDITION-VALUE can be viewed as satisfied,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type phile-object condition-value))
  (the boolean
    (not (null
      (or
        (positive-string-p  condition-value)
        (non-zero-integer-p condition-value))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Interpreter Node) (or null phile-object))
                interpreter-process-node))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          phile-program
    :documentation "The statements to process.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The position of the current node among the
                    INSTRUCTIONS.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null Node)
    :documentation "The node at the position IP in the INSTRUCTIONS.")
   (jump-destination
    :initarg       :jump-destination
    :initform      NIL
    :type          (or null fixnum)
    :documentation "The optional goto (jump) location in the
                    INSTRUCTIONS sequence, contingently specified by the
                    most recent jump instruction.
                    ---
                    If set by such a jump command, the instruction
                    pointer (IP) is relocated to the JUMP-DESTINATION
                    ere invoking the next command, concomitantly
                    unsetting this specification; otherwise, the IP
                    simply advances to the next position.")
   (files
    :initarg       :files
    :initform      (make-file-manager)
    :reader        interpreter-files
    :type          File-Manager
    :documentation "Associates a filename with a ``PFile'' object."))
  (:documentation
    "The ``Interpreter'' class defines an entity entrusted with
     evaluating and inducing effect into a sequence of statements."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the INTERPRETER's current instruction and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type phile-program   instructions))
    (declare (type fixnum          ip))
    (declare (type (or null Node)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' which processes the
   INSTRUCTIONS."
  (declare (type (vector Node *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-advance-ip (interpreter)
  "Moves the INTERPRETER's instruction pointer (IP) either to the next
   instruction or to the location specified by the most recent jump
   command and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip jump-destination current-instruction)
      interpreter
    (declare (type (vector Node *)  instructions))
    (declare (type fixnum           ip))
    (declare (type (or null fixnum) jump-destination))
    (declare (type (or null Node)   current-instruction))
    ;; Either jump to the most recent jump instruction's destination, or
    ;; simply advance to the next command.
    (setf ip
      (or jump-destination
          (1+ ip)))
    ;; Reset the JUMP-DESTINATION in case that a instruction pointer
    ;; goto has occurred.
    (setf jump-destination NIL)
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (values))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE, dispatched on its NODE-TYPE, in the
     INTERPRETER's context, and returns a value appropriate for this
     combination."))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node
    ((interpreter Interpreter)
     (node-type   (eql :binary-operation))
     (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((left-operand  (node-attribute node :left-operand))
        (right-operand (node-attribute node :right-operand)))
    (declare (type Node left-operand))
    (declare (type Node right-operand))
    (the phile-object
      (apply-binary-operator
        (node-attribute node :operator)
        (interpreter-process-node interpreter left-operand)
        (interpreter-process-node interpreter right-operand)))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :close))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (file-manager-close-file
    (interpreter-files interpreter)
    (node-attribute node :filename))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :integer))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the integer
    (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :jump-if))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((condition-value
          (interpreter-process-node interpreter
            (node-attribute node :condition))))
    (declare (type phile-object condition-value))
    (when (condition-satisfied-p condition-value)
      (setf (slot-value interpreter 'jump-destination)
        (interpreter-process-node interpreter
          (node-attribute node :line-number)))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :line))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (interpreter-process-node interpreter
    (node-attribute node :statement))
  (interpreter-advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :open))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (file-manager-open-file
    (interpreter-files interpreter)
    (node-attribute node :filename))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :overwrite))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((filename   (node-attribute node :filename))
        (value-node (node-attribute node :value)))
    (declare (type string filename))
    (declare (type Node   value-node))
    (file-manager-overwrite-file
      (interpreter-files interpreter)
      filename
      (interpreter-process-node interpreter value-node)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :read))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the phile-object
    (file-manager-read-file
      (interpreter-files interpreter)
      (node-attribute node :filename))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :string))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the string
    (node-attribute node :value)))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :write))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((filename   (node-attribute node :filename))
        (value-node (node-attribute node :value)))
    (declare (type string filename))
    (declare (type Node   value-node))
    (file-manager-write-file
      (interpreter-files interpreter)
      filename
      (interpreter-process-node interpreter value-node)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   T)
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type T           node-type))
  (declare (type Node        node))
  (error "Unrecognized node type ~s for node ~s." node-type node))

;;; -------------------------------------------------------

(defun interpreter-process-node (interpreter node)
  "Processes the NODE in the INTERPRETER's context and returns a value
   appropriate for this combination."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the (or null phile-object)
    (interpreter-dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Executes the INTERPRETER's instructions and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  
  (with-slots (current-instruction) interpreter
    (declare (type (or null Node) current-instruction))
    (loop while current-instruction do
      (interpreter-process-node interpreter current-instruction)))
  
  ;; Check whether all opened files are closed ere terminating.
  (file-manager-check-for-open-files
    (interpreter-files interpreter))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Phile (code)
  "Interprets the piece of Phile CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parse-program
        (make-token-stream
          (make-lexer code)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-Phile-script (source)
  "Loads the Phile program from the SOURCE and returns it as a simple
   string."
  (declare (type (or pathname stream string) source))
  (with-open-file (input source
                   :direction         :input
                   :element-type      'character
                   :if-does-not-exist :error)
    (declare (type file-stream input))
    (let ((code (make-string (file-length input))))
      (declare (type simple-string code))
      (read-sequence code input)
      (the simple-string code))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Phile
  "OPEN \"stdout.stream\";
   WRITE \"stdout.stream\" \"Hello, World!\";
   CLOSE \"stdout.stream\";")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Phile
  "OPEN \"stdin.stream\";
   OPEN \"stdout.stream\";
   WRITE \"stdout.stream\" READ \"stdin.stream\";
   1? 2;")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Phile
  "OPEN \"stdin.stream\";
   OPEN \"stdout.stream\";
   READ \"stdin.stream\" = 0? 5;
   WRITE \"stdout.stream\" 1;
   1? 3;
   WRITE \"stdout.stream\" 0;
   CLOSE \"stdin.stream\";
   CLOSE \"stdout.stream\";")

;;; -------------------------------------------------------

;; "99 Bottles of Beer" program.
(interpret-Phile
  "
   OPEN \"stdout.stream\";
   OPEN \"numberOfBottles.dat\";
   
   WRITE \"numberOfBottles.dat\" 99;
   
   /// Line number 3 follows:
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottles of beer on the wall,\\n\";
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottles of beer.\\n\";
   WRITE \"stdout.stream\" \"Take one down, pass it around,\\n\";
   OVERWRITE \"numberOfBottles.dat\" READ \"numberOfBottles.dat\" - 1;
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottles of beer on the wall.\\n\";
   WRITE \"stdout.stream\" \"\\n\";
   
   READ \"numberOfBottles.dat\" > 2? 3;
   READ \"numberOfBottles.dat\" = 2? 11;
   
   /// Line number 11 follows:
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottles of beer on the wall,\\n\";
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottles of beer.\\n\";
   WRITE \"stdout.stream\" \"Take one down, pass it around,\\n\";
   OVERWRITE \"numberOfBottles.dat\" READ \"numberOfBottles.dat\" - 1;
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottle of beer on the wall.\\n\";
   WRITE \"stdout.stream\" \"\\n\";
   
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottle of beer on the wall,\\n\";
   WRITE \"stdout.stream\" READ \"numberOfBottles.dat\" + \" bottle of beer.\\n\";
   WRITE \"stdout.stream\" \"Take one down, pass it around,\\n\";
   WRITE \"stdout.stream\" \"No bottles of beer on the wall.\\n\";
   
   CLOSE \"stdout.stream\";
   CLOSE \"numberOfBottles.dat\";
  ")

;;; -------------------------------------------------------

;; Counter that counts down from ten (10) to inclusive zero (0),
;; printing each state to the standard output.
(interpret-Phile
  "
  /// Line 0.
  OPEN \"stdout.stream\";
  
  /// Line 1.
  OPEN \"counter.num\";
  
  /// Line 2.
  OVERWRITE \"counter.num\" 10;
  
  /// Line 3: Print counter value.
  OVERWRITE \"stdout.stream\" READ \"counter.num\";
  
  /// Line 4: Decrement counter value by one.
  OVERWRITE \"counter.num\" READ \"counter.num\" - 1;
  
  /// Line 5: Append linebreak.
  OVERWRITE \"stdout.stream\" \"\\n\";
  
  /// Line 6: Repeat if counter value is greater or equal to zero (0).
  READ \"counter.num\" ! -1? 3;
  
  /// Line 7.
  CLOSE \"counter.num\";
  
  /// Line 8.
  CLOSE \"stdout.stream\";
  ")

;;; -------------------------------------------------------

;; Counter which counts up or down depending on user input.
;; 
;; This counter queries the user for a start value, either counting down
;; from the positive input or counting up from a negative value, until
;; the end predicate zero (0) is reached. The same is also printed, ere
;; the program terminates. For an input of zero, the program simply
;; outputs 0.
(interpret-Phile
  "
  /// Line 0.
  OPEN \"stdout.stream\";
  /// Line 1.
  OPEN \"stdin.stream\";
  /// Line 2: The countdown/countup memory.
  OPEN \"counter.num\";
  /// Line 3: The step size, which decrements/increments the counter.
  OPEN \"stepsize.num\";
  
  /// Line 4: Query user for counter start value.
  OVERWRITE \"counter.num\" READ \"stdin.stream\";
  
  /// Line 5: Positive user input? => Count down.
  READ \"counter.num\" > 0? 7;
  
  /// Line 6: Negative user input? => Count up.
  READ \"counter.num\" < 1? 9; 
  
  /// Line 7: Positive user input? => Count down. => Decrement counter.
  OVERWRITE \"stepsize.num\" -1;
  /// Line 8: Unconditionally go to counting iteration section.
  1? 11;
  
  /// Line 9: Negative user input? => Count down. => Increment counter.
  OVERWRITE \"stepsize.num\" +1;
  /// Line 10
  1? 11;
  /// Line 11: Unconditionally go to counting iteration section.
  READ \"counter.num\" = 0? 16;
  
  /// Line 12: Print counter value.
  OVERWRITE \"stdout.stream\" READ \"counter.num\";
  
  /// Line 13: Decrement/Increment counter by step size.
  OVERWRITE \"counter.num\" READ \"counter.num\" + READ \"stepsize.num\";
  
  /// Line 14: Append linebreak.
  OVERWRITE \"stdout.stream\" \"\\n\";
  
  /// Line 15: Terminate the loop if the counter has reached zero (0).
  READ \"counter.num\" ! 0? 11;
  
  /// Line 16: Print the concluding counter value zero (0).
  OVERWRITE \"stdout.stream\" READ \"counter.num\";
  
  /// Line 17.
  CLOSE \"stepsize.num\";
  /// Line 18.
  CLOSE \"counter.num\";
  /// Line 19.
  CLOSE \"stdin.stream\";
  /// Line 20.
  CLOSE \"stdout.stream\";
  ")

;;; -------------------------------------------------------

;; Mirror-machine.
(interpret-Phile
  "
  OPEN \"stdin.stream\";
  OPEN \"stdout.stream\";
  OPEN \"a\";
  OPEN \"b\";
  
  /// Input variables a, b.
  OVERWRITE \"a\" READ \"stdin.stream\";
  OVERWRITE \"b\" READ \"stdin.stream\";
  
  /// Increment a.
  OVERWRITE \"a\" READ \"a\" + 1;
  /// Decrement b.
  OVERWRITE \"b\" READ \"b\" - 1;
  
  /// Test of a.
  READ  \"a\" = 0? 11;
  OVERWRITE \"stdout.stream\" READ \"a\" + \"\\n\";
  OVERWRITE \"a\" 0;
  
  /// Line 11:
  /// Test of b.
  READ  \"b\" = 1? 13;
  OVERWRITE \"stdout.stream\" READ \"b\" + \"\\n\";
  
  /// Line 13:
  /// While loop.
  READ  \"b\" = 0? 17;
  OVERWRITE \"stdout.stream\" READ \"b\" + \"\\n\";
  OVERWRITE \"b\" READ \"b\" - 1;
  READ  \"b\" ! 0? 13;
  
  /// Line 17:
  /// Input a, b.
  OVERWRITE \"a\" READ \"stdin.stream\";
  OVERWRITE \"b\" READ \"stdin.stream\";
  
  /// Print a - b.
  OVERWRITE \"stdout.stream\" READ \"a\" - READ \"b\";
  OVERWRITE \"stdout.stream\" \"\\n\";
  /// Print \"ABC\".
  OVERWRITE \"stdout.stream\" \"ABC\" + \"\\n\";
  
  /// Terminate program.
  CLOSE \"stdin.stream\";
  CLOSE \"stdout.stream\";
  CLOSE \"a\";
  CLOSE \"b\";
  ")

;;; -------------------------------------------------------

;; Load Mirror-machine program from an external file.
(interpret-Phile
  (load-Phile-Script
    "resources/Mirror-machine.phile"))
