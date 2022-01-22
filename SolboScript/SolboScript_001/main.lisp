;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "SolboScript", invented by the Esolang user "Ganondork",
;; and intended to reiterate the phrase "BWEEEEE" from the webcomic
;; "sweet bro and hella jeff".
;; 
;; Concepts
;; ========
;; SolboScript constitutes an output-only joke language, expressing in
;; a variation of the term "BWEE" its functionality, exclusively
;; operating on variables.
;; 
;; == SOLBOSCRIPT IS A JOKE LANGUAGE ==
;; The language enumerates as one of a particular ilk among esoteric
;; programming language, known as joke languages, such are in their
;; character and intent predominantly a warklume of humorous, as
;; counterdistinguished from the experimental and explorative purpose
;; often connoted with the general species. In addition to the
;; exposition of linguistic peculiarities, SolboScript inherits from
;; this specialized category the typifying restrictions in abilities.
;; 
;; == "BWEE"S PRESIDE OVER ALL TOKENS ==
;; Any non-whitespace token, except for the prelude "ROCK SOLBO", is
;; defined in terms of the prefix "BWEE", extended by zero or more
;; "E" majuscules. This pattern's ubiquity, comprehending both commands
;; and their arguments, which themselves are either variable names or
;; integer "literals", necessitates a more throughout treatise.
;; 
;; == EACH COMMAND IS A DIFFERENT "BWEE..." ==
;; The diorism of the tokens resolves to the tally of "E"s appended
;; to the stem "BWEE". In the context of command designators the
;; distinguishment inheres in this exact fact.
;; 
;; Commands themselves rely on one or more mandatory arguments, the same
;; either representing a variable or an integer value. The separation
;; betwixt these basic constituents being incapable of intersecting with
;; the pattern --- all data is composed of "BWEE"s ---, the position
;; in the operation's argument list determines the role.
;; 
;; For a list comprehending the available operations please consult the
;; "Instructions" section.
;; 
;; == VARIABLES PRESIDE OVER ALL DATA ==
;; The castaldy of data in SolboScript is devised to reside exclusively
;; in variables. Their agnomination proceeds in the same homogeneous
;; manner as with all effective elements: by being a variation of the
;; prefix "BWE" extended with one or more immediate appendages of "E".
;; By adminicle of the syntax, instructions and such data conditories
;; are vouched never to conflict. The lack of a maximum token's length
;; produces the fact that an infinite number of variables may be
;; defined.
;; 
;; As variables ensconce at any time exactly one integer value of
;; unbounded magnitude, the association of which may be established or
;; manipulated by a declaration or certain arithmetic operations. Note
;; that some commands require the existence of a variable prior to the
;; execution, or else errors will corrupt the program.
;; 
;; == INTEGER LITERALS ARE ALSO "BWEE"S ==
;; The conformance to the "BWEE" pattern does not interrupt in regard to
;; the sole entity enumerated among literal data: the integer values.
;; These positive numbers again bound on a bottom march imposed by
;; "BWEE" as a value of one (1). Produced therefrom, the consectary
;; prescribes that literal integer values less than 1 cannot be stated
;; directly, although they might germinate by adminicle of certain
;; arithmetic operations.
;; 
;; The following table shall provide a summary of the correlation
;; betwixt a "BWEE"-style token and the represented integer value.
;;   
;;   Token          | Numeric value
;;   ---------------+--------------------
;;    BWEE          | 1
;;   ....................................
;;    BWEEE         | 2
;;   ....................................
;;    ...           | ...
;;   ....................................
;;    BWEE{"E" * i} | i - 1, with i >= 0
;; 
;; The conclusion yielded from this formulation establishes that, "BWE"
;; supplied as a base, the specified integer equals the number of "E"s
;; appended, with one instance being the lower bound.
;; 
;; == CHARACTERS ==
;; Confined to the assignment of integers as literal values, a variable
;; may yet produce a datum in the form of a character by printing to the
;; standard output the glyph associated with its numeric value. The
;; correlation does not pertain to the acquainted ASCII standard, and,
;; being rather cursorily nevened in the specification, aligns with the
;; pairing of a letter's position in the alphabet.
;; 
;; The table below enumerates all available characters in conjunction
;; with their position ("Pos"), which correspondence with an integer
;; value, as well as its ASCII character code, official name, and
;; category:
;; 
;;   Pos | Glyph | ASCII code | Name                     | Category
;;   ----+-------+------------+--------------------------+----------------
;;   001 | a     |  97        | small a                  | letter
;;   002 | b     |  98        | small b                  | letter
;;   003 | c     |  99        | small c                  | letter
;;   004 | d     | 100        | small d                  | letter
;;   005 | e     | 101        | small e                  | letter
;;   006 | f     | 102        | small f                  | letter
;;   007 | g     | 103        | small g                  | letter
;;   008 | h     | 104        | small h                  | letter
;;   009 | i     | 105        | small i                  | letter
;;   010 | j     | 106        | small j                  | letter
;;   011 | k     | 107        | small k                  | letter
;;   012 | l     | 108        | small l                  | letter
;;   013 | m     | 109        | small m                  | letter
;;   014 | n     | 110        | small n                  | letter
;;   015 | o     | 111        | small o                  | letter
;;   016 | p     | 112        | small p                  | letter
;;   017 | q     | 113        | small q                  | letter
;;   018 | r     | 114        | small r                  | letter
;;   019 | s     | 115        | small s                  | letter
;;   020 | t     | 116        | small t                  | letter
;;   021 | u     | 117        | small u                  | letter
;;   022 | v     | 118        | small v                  | letter
;;   023 | w     | 119        | small w                  | letter
;;   024 | x     | 120        | small x                  | letter
;;   025 | y     | 121        | small y                  | letter
;;   026 | z     | 122        | small z                  | letter
;;   027 | A     |  65        | capital A                | letter
;;   028 | B     |  66        | capital B                | letter
;;   029 | C     |  67        | capital C                | letter
;;   030 | D     |  68        | capital D                | letter
;;   031 | E     |  69        | capital E                | letter
;;   032 | F     |  70        | capital F                | letter
;;   033 | G     |  71        | capital G                | letter
;;   034 | H     |  72        | capital H                | letter
;;   035 | I     |  73        | capital I                | letter
;;   036 | J     |  74        | capital J                | letter
;;   037 | K     |  75        | capital K                | letter
;;   038 | L     |  76        | capital L                | letter
;;   039 | M     |  77        | capital M                | letter
;;   040 | N     |  78        | capital N                | letter
;;   041 | O     |  79        | capital O                | letter
;;   042 | P     |  80        | capital P                | letter
;;   043 | Q     |  81        | capital Q                | letter
;;   044 | R     |  82        | capital R                | letter
;;   045 | S     |  83        | capital S                | letter
;;   046 | T     |  84        | capital T                | letter
;;   047 | U     |  85        | capital U                | letter
;;   048 | V     |  86        | capital V                | letter
;;   049 | W     |  87        | capital W                | letter
;;   050 | X     |  88        | capital X                | letter
;;   051 | Y     |  89        | capital Y                | letter
;;   052 | Z     |  90        | capital Z                | letter
;;   053 | 0     |  48        | digit 0                  | digit
;;   054 | 1     |  49        | digit 1                  | digit
;;   055 | 2     |  50        | digit 2                  | digit
;;   056 | 3     |  51        | digit 3                  | digit
;;   057 | 4     |  52        | digit 4                  | digit
;;   058 | 5     |  53        | digit 5                  | digit
;;   059 | 6     |  54        | digit 6                  | digit
;;   060 | 7     |  55        | digit 7                  | digit
;;   061 | 8     |  56        | digit 8                  | digit
;;   062 | 9     |  57        | digit 9                  | digit
;;   .....................................................................
;;   063 |       | 32         | space                    | whitespaces
;;   064 | \t    | 9          | horizontal tab           | whitespaces
;;   065 | \n    | 10         | newline                  | whitespaces
;;   .....................................................................
;;   066 | !     | 33         | exclamation mark         | punctuation
;;   067 | ?     | 63         | question mark            | punctuation
;;   068 | .     | 46         | period, dot              | punctuation
;;   069 | :     | 58         | colon                    | punctuation
;;   070 | ,     | 44         | comma                    | punctuation
;;   071 | ;     | 59         | semicolon                | punctuation
;;   072 | ~     | 126        | tilde                    | punctuation
;;   073 | -     | 45         | hyphen, minus sign       | punctuation
;;   074 | _     | 95         | underscore               | punctuation
;;   .....................................................................
;;   075 | /     | 47         | solidus, slash           | slashes
;;   076 | |     | 124        | vertical bar, pipe       | slashes
;;   077 | \     | 92         | backslash                | slashes
;;   .....................................................................
;;   078 | '     | 39         | apostrophe, single quote | quotation marks
;;   079 | "     | 34         | double quote             | quotation marks
;;   080 | `     | 96         | grave accent, backquote  | quotation marks
;;   .....................................................................
;;   081 | [     | 91         | left square bracket      | brackets
;;   082 | ]     | 93         | right square bracket     | brackets
;;   083 | (     | 40         | left parenthesis         | brackets
;;   084 | )     | 41         | right parenthesis        | brackets
;;   085 | {     | 123        | left brace               | brackets
;;   086 | }     | 125        | right brace              | brackets
;;   .....................................................................
;;   087 | +     | 43         | plus sign                | mathematics
;;   088 | -     | 45         | minus sign, hyphen       | mathematics
;;   089 | *     | 42         | asterisk, star           | mathematics
;;   090 | =     | 61         | equals sign              | mathematics
;;   091 | <     | 60         | less-than sign           | mathematics
;;   092 | >     | 62         | greater-than sign        | mathematics
;;   .....................................................................
;;   093 | ^     | 94         | circumflex accent        | miscellany
;;   094 | @     | 64         | commercial at, at-sign   | miscellany
;;   095 | $     | 36         | dollar sign              | miscellany
;;   096 | %     | 37         | percent sign             | miscellany
;;   097 | &     | 38         | ampersand                | miscellany
;;   098 | #     | 35         | number sign, sharp sign  | miscellany
;;   
;;   099 | NUL   | 0          | null character           | controls
;; 
;; 
;; Architecture
;; ============
;; A further testament to its simplicity, no patent architecture fixates
;; SolboScript's ecosystem, its stewardship concerning the data being
;; the onus merely to its arbitrary tally of variables.
;; 
;; 
;; Data Types
;; ==========
;; SolboScript's reliance constitutes a twyfold notion, with integers as
;; occupants of a paravaunt rank and characters acting as adminicles for
;; display purposes.
;; 
;; == INTEGERS ESTABLISH THE PRIMARY DATA TYPE ==
;; Variables, the only data storage serviceable to a program, store
;; integer values of unbounded spectrum, that is, allocated inside of
;; [-infinity, +infinity]. The quantity alone apportioned to arithmetic
;; facilities indicates the numeric objects' superior role; its
;; magnitude embraces incrementing, decremeting, the four basic
;; arithmetic operations, in addition to the same's display. An implicit
;; significant wones in the conditional and iterative constructs, which
;; apply comparisons exclusively on such objects.
;; 
;; == CHARACTERS AID IN THE DISPLAY ==
;; Characters, a subordinated perspective on the variables' integer
;; data, partake of an active agency in the course of the program merely
;; when ordained to print the letter associated with the numeric value.
;; Its nature as an output-only language restricts this airt of
;; interaction to a singular justification of the non-numeric species.
;; 
;; 
;; Syntax
;; ======
;; SolboScript is accorded a program structure equiponderant in
;; homogeneity and peculiarity. The syntax relies on, with the single
;; exemption manifested in the program launch instruction, on the term
;; "BWEE" and its variations, accomplished by appendages of further
;; "E"s. This particular phrase furnishes the design for instructions
;; as well as arguments, while whitespaces and indentations coerce their
;; consideration in some contexts, bearing none in others.
;; 
;; == INSTRUCTIONS ==
;; Every SolboScript program instigates with the designator tokens
;;   
;;   ROCK SOLBO
;; 
;; The source code's remainder constitutes a sequence of zero or more
;; instructions, each introduced by a variant of the "BWEE" phrase, and
;; potentially a tally of one or more arguments. An arbitrary number of
;; instructions may occupy a single line, which includes the spatial
;; proximity with the "ROCK SOLBO" preface.
;; 
;; == SENSITIVITY TO WHITESPACES DEPENDS ON CONTEXT ==
;; Albeit distinguishable by mediation of its own design, each two
;; tokens must be separated by at least one space or newline character,
;; the former set subsuming in its diorism the actual space character as
;; well as the horizontal tab, the letter assuming any concrete guise
;; imposed by the system environment, including combinations of linefeed
;; and carriage return.
;; 
;; A particular treatment incurs in the question of indentation as a
;; means of uniting connected statements. Usually, the number of spaces
;; preceding the first instruction on a line does not carry any
;; significance; however, as a vehicle of conveying compound statements,
;; which is the case with the three commands
;;   
;;   - "BWEEEEEEEEEEE"   (if)
;;   - "BWEEEEEEEEEEEE"  (loop forever)
;;   - "BWEEEEEEEEEEEEE" (loop until)
;; 
;; a criterion for discrimination betwixt subordinated body statements
;; and such not participating in the union requires implementation.
;; Furnished as an example, the following conditional code shall
;; emulate the pseudocode
;;   
;;   BWEE <- 6
;;   if BWEE = BWEE then
;;     printAsNumber (BWEE)
;;     printAsLetter (BWEE)
;;   end if
;;   
;;   BWEEEE BWEE BWEEEEEEE 
;;   BWEEEEEEEEEEE BWEE BWEE BWEEE
;;   BWEE  BWEE
;;   BWEEE BWEE
;; 
;; Given a hypothetical concord with this syntax, the following
;; alternative pseudocode, which contains an unconditional section
;; outside of the "if" block:
;;   
;;   BWEE <- 6
;;   if BWEE = BWEE then
;;     printAsNumber (BWEE)
;;   end if
;;   printAsLetter (BWEE)
;; 
;; would be incapable of distinguishment in SobolScript:
;;   
;;   BWEEEE BWEE BWEEEEEEE 
;;   BWEEEEEEEEEEE BWEE BWEE BWEEE
;;   BWEE  BWEE
;;   BWEEE BWEE
;; 
;; A solution to this predicament, indentation, which is defined as a
;; series of zero or more spaces preceding the first command of a line,
;; retains significance only in instructions associated with a body of
;; their own, into which set the three members enumerated above enlist.
;; Regarding any other command, which shall be designated as "atomar",
;; indentation of subsequent lines exhibits ineffectuality. In those
;; statements building a hierarchy, each appertaining body command line
;; must be indented by at least one additional space as juxtaposed to
;; the encompassing command's column position. The first line, whether
;; empty or not, with an indentation less than or equal to the
;; ensconcing command terminates the latter's body and starts a unit of
;; its own. This concept shall be known as the "context-sensitive
;; off-side rule", a variety on the common off-side rule that
;; consistently appreciates indentation.
;; 
;; With this context-sensitive off-side rule as an appurtenance, we may
;; define the disruptive second pseudocode example
;;   
;;   BWEE <- 6
;;   if BWEE = BWEE then
;;     printAsNumber (BWEE)
;;   end if
;;   printAsLetter (BWEE)
;; 
;; as this SobolScript code, where the final two lines are particularly
;; peisant:
;;   
;;   BWEEEE BWEE BWEEEEEEE 
;;   BWEEEEEEEEEEE BWEE BWEE BWEEE
;;     BWEE  BWEE
;;   BWEEE BWEE
;; 
;; == COMMENTS ==
;; The language in its current rendition does not extend the capacity
;; for comments.
;; 
;; == GRAMMAR ==
;; The language syntax can be expressed in the following Extended
;; Backus-Naur form (EBNF) description:
;;   
;;   program        := prelusion , { instruction } ;
;;   prelusion      := "ROCK" , "SOLBO" ;
;;   instruction    := printNumber
;;                  |  printLetter
;;                  |  declaration
;;                  |  increment
;;                  |  decrement
;;                  |  addition
;;                  |  subtraction
;;                  |  multiplication
;;                  |  division
;;                  |  if
;;                  |  loopForever
;;                  |  loopUntil ;
;;   printNumber    := "BWEE" , variable ;
;;   printLetter    := "BWEEE" , variable ;
;;   declaration    := "BWEEEE" , variable , value ;
;;   increment      := "BWEEEEE" , variable ;
;;   decrement      := "BWEEEEEE" , variable ;
;;   addition       := "BWEEEEEEE" , variable , variable , variable ;
;;   subtraction    := "BWEEEEEEEE" , variable , variable , variable ;
;;   multiplication := "BWEEEEEEEEE" , variable , variable , variable ;
;;   division       := "BWEEEEEEEEEE" , variable , variable , variable ;
;;   if             := "BWEEEEEEEEEEE" , variable, comparison, variable , { instruction } ;
;;   loopForever    := "BWEEEEEEEEEEEE" , { instruction } ;
;;   loopUntil      := "BWEEEEEEEEEEEEE" , variable , comparison, variable , { instruction } ;
;;   comparison     := "BWEE" | "BWEEE" | "BWEEEE" ;
;;   value          := "BWEE" , { "E" } ;
;; 
;; 
;; Instructions
;; ============
;; All instructions and arguments are expressed in "BWEE" patterns, the
;; concrete variation determines the operation to execute. The induced
;; values to the same always constitute integer objects. The language
;; encompasses arithmetic routines, input/output facilities, one
;; conditional, and two iterative constructs.
;; 
;; == OVERVIEW ==
;; The following apercu shall furnish a description of the instruction
;; set.
;;   
;;   Command          | ID | Role
;;   -----------------+----+----------------------------
;;    BWEE            |  2 | Print variable as number.
;;    BWEEE           |  3 | Print variable as letter.
;;    BWEEEE          |  4 | Define variable with value.
;;    BWEEEEE         |  5 | Increment variable.
;;    BWEEEEEE        |  6 | Decrement variable.
;;    BWEEEEEEE       |  7 | Add two variables.
;;    BWEEEEEEEE      |  8 | Subtract two variables.
;;    BWEEEEEEEEE     |  9 | Multiply two variables.
;;    BWEEEEEEEEEE    | 10 | Divide two variables.
;;    BWEEEEEEEEEEE   | 11 | Compare two variables.
;;    BWEEEEEEEEEEEE  | 12 | Loop forever.
;;    BWEEEEEEEEEEEEE | 13 | Loop until.
;; 
;; == "BWEE": PRINT VARIABLE AS NUMBER ==
;; Prints a variable value in its numeric form to the standard output.
;; 
;; Signature:
;;   BWEE {variable}
;; 
;; Interface:
;;   printAsNumber (variable : variable) : void
;; 
;; Description:
;;   Prints to the standard output the integer value stored in the
;;   {variable}.
;; 
;; Side effects:
;;   - The content of the {variable} is written to the standard output.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {variable} has not yet been declared.
;; 
;; == "BWEEE": PRINT VARIABLE AS AS LETTER ==
;; Prints the character associated with a variable's value to the
;; standard output.
;; 
;; Signature:
;;   BWEEE {variable}
;; 
;; Interface:
;;   printAsLetter (variable : variable) : void
;; 
;; Description:
;;   Prints to the standard output character associated with the integer
;;   value stored in the {variable}. For a complete list of the
;;   number to character mapping please consult the "Concepts" section's
;;   subsection "Characters".
;; 
;; Side effects:
;;   - The content of the {variable} is written to the standard output.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {variable} has not yet been declared.
;; 
;; == "BWEEEE": DECLARE A VARIABLE WITH A VALUE ==
;; Creates a variable, if not already extant, and assigns to its an
;; integer value.
;; 
;; Signature:
;;   BWEEEE {variable} {value}
;; 
;; Interface:
;;   declareVariable (variable : variable, value : integer) : void
;; 
;; Description:
;;   Creates a variable with the name {variable}, concomitantly
;;   assigning to it the integer {value}. If the variable already
;;   exists, its content is silently overwritten.
;; 
;; Side effects:
;;   - If no variable with the identifier {variable} exists, a new
;;     entity is registered at the system and associated with the
;;     {value}.
;;   - If a variable with the identifier {variable} already exists, its
;;     value is replaced by the {value}.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == "BWEEEEE": INCREMENT VARIABLE ==
;; Increments a variable's integer value by one.
;; 
;; Signature:
;;   BWEEEEE {variable}
;; 
;; Interface:
;;   increment (variable : variable) : void
;; 
;; Description:
;;   Increments the integer value stored in the {variable} by one. If
;;   the variable does not exist, an error is signaled.
;; 
;; Side effects:
;;   - The {variable} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {variable} has not yet been declared.
;; 
;; == "BWEEEEEE": DECREMENT VARIABLE ==
;; Decrements a variable's integer value by one.
;; 
;; Signature:
;;   BWEEEEE {variable}
;; 
;; Interface:
;;   decrement (variable : variable) : void
;; 
;; Description:
;;   Decrements the integer value stored in the {variable} by one. If
;;   the variable does not exist, an error is signaled.
;; 
;; Side effects:
;;   - The {variable} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {variable} has not yet been declared.
;; 
;; == "BWEEEEEEE": ADD VARIABLES ==
;; Adds two variables and stores the sum into a third one.
;; 
;; Signature:
;;   BWEEEEEEE {augendVariable} {addendVariable} {outputVariable}
;; 
;; Interface:
;;   add (augendVariable : variable,
;;        addendVariable : variable,
;;        outputVariable : variable) : void
;; 
;; Description:
;;   Adds the value of the {addendVariable} to the {augendVariable} and
;;   stores the sum in the {outputVariable}. The {augendVariable} and
;;   {addendVariable} will not be modified. Any two or all three of the
;;   variables may refer to the same entity. If the {augendVariable} or
;;   the {addendVariable} does not exist, an error is signaled. If the
;;   {outputVariable} does not exist, it is created.
;; 
;; Side effects:
;;   - The {outputVariable} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {augendVariable} has not yet been declared.
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {addendVariable} has not yet been declared.
;; 
;; == "BWEEEEEEEE": SUBTRACT VARIABLES ==
;; Subtracts two variables and stores the difference in a third one.
;; 
;; Signature:
;;   BWEEEEEEEE {minuendVariable} {subtrahendVariable} {outputVariable}
;; 
;; Interface:
;;   subtract (minuendVariable    : variable,
;;             subtrahendVariable : variable,
;;             outputVariable     : outputVariable) : void
;; 
;; Description:
;;   Subtracts the value of the {subtrahendVariable} from the
;;   {minuendVariable} and stores the difference in the
;;   {outputVariable}. The {minuendVariable} and {subtrahendVariable}
;;   will not be modified. Any two or all three of the variables may
;;   refer to the same entity. If the {minuendVariable} or the
;;   {subtrahendVariable} does not exist, and error is signaled. If the
;;   {outputVariable} does not exist, it is created.
;; 
;; Side effects:
;;   - The {outputVariable} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {minuendVariable} has not yet been declared.
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {subtrahendVariable} has not yet been declared.
;; 
;; == "BWEEEEEEEEE": MULTIPLY VARIABLES ==
;; Multiplies two variables and stores the product in a third one.
;; 
;; Signature:
;;   BWEEEEEEEEE {multiplicandVariable} {multiplierVariable} {outputVariable}
;; 
;; Interface:
;;   multiply (multiplicandVariable : variable,
;;             multiplierVariable   : variable,
;;             outputVariable       : variable) : void
;; 
;; Description:
;;   Multiplies the value of the {multiplicandVariable} by that of the
;;   {multiplierVariable} and stores the product in the
;;   {outputVariable}. The {multiplicandVariable} and
;;   {multiplierVariable} will not be modified. Any two or all three of
;;   the variables may refer to the same entity. If the
;;   {multiplicandVariable} or the {multiplierVariable} does not exist,
;;   and error is signaled. If the {outputVariable} does not exist, it
;;   is created.
;; 
;; Side effects:
;;   - The {outputVariable} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {multiplicandVariable} has not yet been declared.
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {multiplierVariable} has not yet been declared.
;; 
;; == "BWEEEEEEEEEE": DIVIDE VARIABLES ==
;; Divides two variables and stores the quotient in a third one.
;; 
;; Signature:
;;   BWEEEEEEEEEE {dividendVariable} {divisorVariable} {outputVariable}
;; 
;; Interface:
;;   multiply (dividendVariable : variable,
;;             divisorVariable  : variable,
;;             outputVariable   : variable) : void
;; 
;; Description:
;;   Divides the value of the {dividendVariable} by that of the
;;   {divisorVariable} and stores the quotient in the {outputVariable}.
;;   The {dividendVariable} and {divisorVariable} will not be modified.
;;   Any two or all three of the variables may refer to the same entity.
;;   If the {dividendVariable} or the {divisorVariable} does not exist,
;;   and error is signaled. If the {outputVariable} does not exist, it
;;   is created.
;; 
;; Side effects:
;;   - The {outputVariable} is modified.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {dividendVariable} has not yet been declared.
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {divisorVariable} has not yet been declared.
;; 
;; == "BWEEEEEEEEEEE": IF ==
;; Executes a series of instructions if a condition is met.
;; 
;; Signature:
;;   BWEEEEEEEEEEE {firstVariable} {comparison} {secondVariable}
;;     {instructions}
;; 
;; Interface:
;;   if (firstVariable  : variable,
;;       comparison     : comparison,
;;       secondVariable : variable,
;;       instructions   : command[0..*]) : void
;; 
;; Description:
;;   Executes the sequence of zero or more {instructions} if the
;;   {comparison} operation involving the {firstVariable} and the
;;   {secondVariable} is satisfied; otherwise exercises no effect.
;;   
;;   Three valid options for the {comparison} operator exist:
;;     
;;     Token   | Operator | Description
;;     --------+----------+--------------------------------------------
;;      BWEE   | =        | Is satisfied if the {firstVariable} is
;;             |          | exactly equal to the {secondVariable},
;;             |          | that is:
;;             |          |   firstVariable = secondVariable
;;     ................................................................
;;      BWEEE  | >        | Is satisfied if the {firstVariable} is
;;             |          | strictly greater than the {secondVariable},
;;             |          | that is:
;;             |          |   firstVariable > secondVariable
;;     ................................................................
;;      BWEEEE | <        | Is satisfied if the {firstVariable} is
;;             |          | strictly less than the {secondVariable},
;;             |          | that is:
;;             |          |   firstVariable < secondVariable
;; 
;; Side effects:
;;   - None, but depends on the executed body {instructions}.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {firstVariable} has not yet been declared.
;;   - An error of the type "InvalidComparisonError" is thrown if the
;;     {comparison} operator assumes an invalid value.
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {secondVariable} has not yet been declared.
;; 
;; == "BWEEEEEEEEEEEE": LOOP FOREVER ==
;; Executes a sequence of instructions an infinite number of times.
;; 
;; Signature:
;;   BWEEEEEEEEEEEE
;;     {instructions}
;; 
;; Interface:
;;   loopForever (instructions : command[0..*]) : void
;; 
;; Description:
;;   Executes the sequence of zero or more {instructions} in an infinite
;;   loop.
;; 
;; Side effects:
;;   - None, but depends on the executed body {instructions}.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == "BWEEEEEEEEEEEEE": LOOP UNTIL ==
;; Repeatedly executes a series of instructions until a termination
;; condition is satisfied.
;; 
;; Signature:
;;   BWEEEEEEEEEEEEE {firstVariable} {comparison} {secondVariable}
;;     {instructions}
;; 
;; Interface:
;;   loopUntil (firstVariable  : variable,
;;              comparison     : comparison,
;;              secondVariable : variable,
;;              instructions   : command[0..*]) : void
;; 
;; Description:
;;   Executes the sequence of zero or more {instructions} until the
;;   {comparison} operation involving the {firstVariable} and the
;;   {secondVariable} is satisfied.
;;   
;;   Three valid options for the {comparison} operator exist:
;;     
;;     Token   | Operator | Description
;;     --------+----------+--------------------------------------------
;;      BWEE   | =        | Is satisfied if the {firstVariable} is
;;             |          | exactly equal to the {secondVariable},
;;             |          | that is:
;;             |          |   firstVariable = secondVariable
;;     ................................................................
;;      BWEEE  | >        | Is satisfied if the {firstVariable} is
;;             |          | strictly greater than the {secondVariable},
;;             |          | that is:
;;             |          |   firstVariable > secondVariable
;;     ................................................................
;;      BWEEEE | <        | Is satisfied if the {firstVariable} is
;;             |          | strictly less than the {secondVariable},
;;             |          | that is:
;;             |          |   firstVariable < secondVariable
;; 
;;   Depending on the {comparison} operator chosen, the following
;;   principle applies to the type of loop:
;;   
;;     Token   | Loop type
;;     --------+-------------------------------------------------------
;;      BWEE   | loop until a = b
;;     ................................................................
;;      BWEEE  | loop until a > b
;;     ................................................................
;;      BWEEEE | loop until a < b
;; 
;; Side effects:
;;   - None, but depends on the executed body {instructions}.
;; 
;; Exceptional situations:
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {firstVariable} has not yet been declared.
;;   - An error of the type "InvalidComparisonError" is thrown if the
;;     {comparison} operator assumes an invalid value.
;;   - An error of the type "UndefinedVariableError" is thrown if the
;;     {secondVariable} has not yet been declared.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The SolboScript specification, however complete and comprehsible,
;; is inflicted with a set of ambiguities, a selection thereof shall be
;; presented below.
;; 
;; == WHICH LETTERS ARE EXACTLY MAPPED TO VARIABLE VALUES? ==
;; The command "BWEEE" permits an alphanumeric output of a variable
;; value, declaiming that a correspondence of the numeric entity to the
;; letter of the alphabet presides. Yet the exact mapping regarding the
;; circumference, association, and case elude this definition, inciting
;; the following inquisitions:
;;   
;;   (a) Which characters are embraced by the mapping?
;;   (b) Which positions map to these characters?
;;   (c) A special case of (a), do minuscules precedes or follow
;;       majuscules?
;; 
;; The sole utilities in implication, identified by the supplied
;; examples, intimate, without stark corroboration, a solution to the
;; last point (c) in that minuscular letters prevail in the respective
;; code titles. Any other of the two remnants, (a) and (b), is delivered
;; to own's personal delectation. The detailed mapping chosen as
;; canonical is presented in the section "Concepts", subsection
;; "Characters", which to consult is implored; natheless, an extract in
;; the form of a coarse categorization will be purveyed in the following
;; table:
;;   
;;   Value range | Members
;;   ------------+----------------------
;;    1--26      | lowercase letters a--z
;;    2--52      | uppercase letters A--Z
;;    53--62     | digits 0--9
;;    63--65     | whitespaces
;;    66--74     | punctuation marks
;;    75--77     | slashes
;;    78--80     | quotation marks
;;    81--86     | brackets
;;    87--92     | mathematical symbols
;;    93--98     | miscellaneous symbols
;;    99         | control characters
;; 
;; == WHAT IS THE CONCRETE SYNTAX OF THE "LOOP UNTIL" COMMAND? ==
;; The "loop until" facility, coerced into action with the command
;; "BWEEEEEEEEEEEEE", and located at the desinence of the tabular
;; instruction listing, responds to its termination condition with the
;; obscure signification of an ellipsis. The curt nature inhering both
;; the syntax and the associated description excludes any further
;; scrutinity. Referring to the necessity of a terminating predicate,
;; and the precedent designed by the conditional "BWEEEEEEEEEEE", the
;; iterative construct is reckoned eiusdem generis:
;;   
;;   BWEEEEEEEEEEEEE {variable1} {BWEE | BWEEE | BWEEEE} {variable2}
;;     {statements*}
;; where
;;   "BWEE"   is satisfied if variable1 = variable2
;;   "BWEEE"  is satisfied if variable1 > variable2
;;   "BWEEEE" is satisfied if variable1 < variable2
;; 
;; Conforming to the "if" heritage, zero or more statements, indented
;; by at least one space, are ensconced as the loop body.
;; 
;; == IS VARIABLE DECLARATION A PREREQUISITE TO ASSIGNMENT? ==
;; A direct specification of a variable's content manifests only in the
;; declaration instruction "BWEEEE". Yet a subset of commands, exhausted
;; by the binary operations "BWEEEEEEE", "BWEEEEEEEE", "BWEEEEEEEEE",
;; and "BWEEEEEEEEEE", incorporates in the course of its execution an
;; assignment of a value disencumbered from its prior content. This
;; impels an inquisition into the question whether an nonextant output
;; variable may be declared and initialized in the same location in
;; addition to the dedicated statement.
;; 
;; This specification has adjudged it reasonable to permit the ad hoc
;; creation and assignment in the context of a binary operation.
;; 
;; 
;; Implementation
;; ==============
;; The interpretation of SolboScript code proceeds through three stages
;; acquainted to the solution of such tasks:
;;   
;;   (1) The tokenization of the source code.
;;   (2) The composition of an abstract syntax tree (AST) from the
;;       extracted tokens.
;;   (3) The processing of the AST's nodes in order to imbue the code
;;       with actual effect.
;; 
;; The first unit's cotidian nature does not warrant its further
;; indagation --- it suffices to retain cognizance about the generation
;; of tokens and the fact that their existence is built upon a composite
;; of four pieces: a type, a value, a row and a column index. Meanwhile,
;; the two concluding remnants harbor significant traits obtained from
;; the language's haecceity in a mete as to assess their scrutinity as
;; a thing of requisite estimation.
;; 
;; == TOKEN COLUMNS AID IN THE CONTEXT-SENSITIVE OFF-SIDE RULE ==
;; The conditional significance of horizontal positioning assumed by
;; commands subordinated to their complex peers --- namely, "if",
;; "loop forever", and "loop until" --- is solved by a piece of
;; information commorant in the ``Token'' class: the column index. This
;; non-negative integer number tallies a constituent's horizontal
;; distance from a preceding linebreak or, if located in the first line,
;; from the start of the program. If representing a command name,
;; especially the first on a line, this column position is tantamount to
;; the statement's indentation.
;; 
;; == A SINGLE NODE CLASS ==
;; The variety of information stored in accord with the different
;; facilities of a programming language, such as the operand twain
;; partaking in a binary operation or the statements appertaining to a
;; iterative construct, incite in the usual case an architecture compact
;; of a node interface and specialized concrete implementations. An
;; attendance realized in this sort veridically applies to the best
;; practices; however, the tentative aspect of our simple implementation
;; with full cognizance veers from the natural adjudgement and resorts
;; to a throughout plain devise. A single ``Node'' class, identified by
;; a symbolic type, representive of the emulated language construct,
;; and a collection maintaining the necessary information in the form of
;; a hash table, the keys of which describe the attribute names, while
;; the values associate with the same to provide a simulacrum of custom
;; fields or slots, substitutes the elaborate type hierarchy encountered
;; in professional endeavors.
;; 
;; == NODE TYPES DISTINGUISH COMMANDS ==
;; Inflicted with caligation regarding the defining characteristics
;; usually molded into its fields, the correlation of SobolScript
;; commands, manifesting node types, and their inherent attributes
;; coerce an explicit documentation. This necessity shall be satisfied
;; in the following:
;;   
;;   Command         | ROCK SOLBO
;;                   |   {statements}
;;   Node type       | :program
;;   Node attributes | :statements : Node[0..*]
;;   ..................................................................
;;   Command         | BWEE {variable}
;;   Node type       | :print-number
;;   Node attributes | :variable : string
;;   ..................................................................
;;   Command         | BWEEE {variable}
;;   Node type       | :print-letter
;;   Node attributes | :variable : string
;;   ..................................................................
;;   Command         | BWEEEE {variable} {value}
;;   Node type       | :define
;;   Node attributes | :variable : string
;;                   | :value    : string
;;   ..................................................................
;;   Command         | BWEEEEE
;;   Node type       | :increment
;;   Node attributes | :variable : string
;;   ..................................................................
;;   Command         | BWEEEEEE {variable}
;;   Node type       | :decrement
;;   Node attributes | :variable : string
;;   ..................................................................
;;   Command         | BWEEEEEEE {leftVariable} {rightVariable} {outputVariable}
;;   Node type       | :binary-operation
;;   Node attributes | :operation       : keyword, equals ":addition"
;;                   | :left-variable   : string
;;                   | :right-variable  : string
;;                   | :output-variable : string
;;   ..................................................................
;;   Command         | BWEEEEEEEE {leftVariable} {rightVariable} {outputVariable}
;;   Node type       | :binary-operation
;;   Node attributes | :operation       : keyword, equals ":subtraction"
;;                   | :left-variable   : string
;;                   | :right-variable  : string
;;                   | :output-variable : string
;;   ..................................................................
;;   Command         | BWEEEEEEEEE {leftVariable} {rightVariable} {outputVariable}
;;   Node type       | :binary-operation
;;   Node attributes | :operation       : keyword, equals ":multiplication"
;;                   | :left-variable   : string
;;                   | :right-variable  : string
;;                   | :output-variable : string
;;   ..................................................................
;;   Command         | BWEEEEEEEEEE {leftVariable} {rightVariable} {outputVariable}
;;   Node type       | :binary-operation
;;   Node attributes | :operation       : keyword, equals ":division"
;;                   | :left-variable   : string
;;                   | :right-variable  : string
;;                   | :output-variable : string
;;   ..................................................................
;;   Command         | BWEEEEEEEEEEE {leftVariable} {comparison} {rightVariable}
;;                   |   {statements}
;;   Node type       | :if
;;   Node attributes | :left-variable  : string
;;                   | :comparison     : keyword, in {:equal,
;;                   |                                :greater-than,
;;                   |                                :less-than}
;;                   | :right-variable : string
;;                   | :statements     : Node[0..*]
;;   ..................................................................
;;   Command         | BWEEEEEEEEEEEE
;;                   |   {statements}
;;   Node type       | :loop-forever
;;   Node attributes | :statements : Node[0..*]
;;   ..................................................................
;;   Command         | BWEEEEEEEEEEEEE {leftVariable} {comparison} {rightVariable}
;;                   |   {statements}
;;   Node type       | :loop-until
;;   Node attributes | :left-variable  : string
;;                   | :comparison     : keyword, in {:equal,
;;                   |                                :greater-than,
;;                   |                                :less-than}
;;                   | :right-variable : string
;;                   | :statements     : Node[0..*]
;; 
;; == THE INTERPRETER: AN AST VISITOR ==
;; The ``Interpreter'' class assumes the task of effectuating the
;; abstract syntax tree generated by the parser. These responsibilities
;; embrace paravaunt the traversal of the node hierarchy and the
;; processing of its nodes.
;; 
;; The stringency of justice inflicts us as the amenities enjoyed in the
;; singular ``Node'' class extort a costage in disaccommodation tholed
;; when traversing the node network. A proven, extensible solution to
;; reacting in response to distinguishable node subclasses is exercised
;; by means of the "visitor" design pattern. The same's basic tenet
;; prescribes a single method, adjusted to our context as
;;   
;;   visitNode (node : Node)
;; 
;; and transliterated into the Common Lisp syntax as
;;   
;;   visit-node ((interpreter Interpreter) (node <NODE-SUBCLASS>))
;; 
;; Without the encroaching Procrustean administration, generic function
;; implementations would assume the guise typified in the following
;; selection:
;;   
;;   visit-node ((interpreter Interpreter) (node Program-Node))
;;   
;;   visit-node ((interpreter Interpreter) (node Print-Letter-Node))
;;   
;;   visit-node ((interpreter Interpreter) (node Loop-Until-Node))
;;   
;;   ...
;; 
;; The amorphous nature inhabiting the comprehensive ``Node'' type,
;; barring the diorisms of contingent specializations, encumbers this
;; pattern with indiscriminate attendance:
;;   
;;   visit-node ((interpreter Interpreter) (node Node))
;; 
;; The visitor pattern's haecceity, its respondency to different node
;; types, suffers obliteration. A soteriological warkloom, however,
;; exists in Common Lisp that deems the predicament soluble: Generic
;; functions in Common Lisp dispatch on two possible specificiations,
;; the one enumerated already with class identities, the second realized
;; in the ``eql'' predicate. The general pattern assumes:
;;   
;;   my-method (... (argument (eql <OBJECT-TO-DISPATCH-ON>)) ...)
;; 
;; Our ``Node'' class' slot ``type'' exhibits an equivalency to the
;; identity propagated usually by classes; dispatchment on this datum
;; avails us with an equipollence commensurate to the class hierarchy's
;; lost potential, concretized:
;;   
;;   visit-node ((interpreter Interpreter)
;;               (node-type   (eql <NODE-TYPE-KEYWORD>))
;;               (node        Node))
;; 
;; It would be a thing of tenability to require each invocation of
;; ``visit-node'' to assume such a signature --- yet, this advenient
;; imposition may be obviated. The adminicular function
;;   
;;   dispatch-node ((interpreter Interpreter)
;;                  (node-type   (eql <NODE-TYPE-KEYWORD>))
;;                  (node        Node))
;; 
;; is ordained with the main operative functionality, superseding
;; ``visit-node'' in this duty; for instance:
;;   
;;   dispatch-node ((interpreter Interpreter)
;;                  (node-type   :program)
;;                  (node        Node))
;;   
;;   dispatch-node ((interpreter Interpreter)
;;                  (node-type   :print-letter)
;;                  (node        Node))
;;   
;;   dispatch-node ((interpreter Interpreter)
;;                  (node-type   :loop-until)
;;                  (node        Node))
;; 
;; While being laden both with application logic and the recognition of
;; diorisms in the ``node-type'', the invocation ``visit-node'' remains
;; the official interface operation:
;;   
;;   visit-node ((interpreter Interpreter)
;;               (node        Node))
;; 
;; ``dispatch-node'' never experiences direct usage, as visible in the
;; code excerpt
;;   
;;   (defmethod dispatch-node ((interpreter Interpreter)
;;                             (node-type   (eql :program))
;;                             (node        Node))
;;     (declare (type Interpreter interpreter))
;;     (declare (type keyword     node-type))
;;     (declare (ignore           node-type))
;;     (declare (type Node        node))
;;     (let ((statements (node-attribute node :statements)))
;;       (declare (type (list-of Node) statements))
;;       (dolist (statement statements)
;;         (declare (type Node statement))
;;         (visit-node interpreter statement)))
;;     (values))
;; 
;; Please heed the invocation of ``visit-node'', applying itself to the
;; dissemination of the control by furcation. Its implementation amounts
;; to
;; 
;;   (defmethod visit-node ((interpreter Interpreter) (node Node))
;;     (declare (type Interpreter interpreter))
;;     (declare (type Node        node))
;;     (let ((node-type (node-type node)))
;;       (declare (type keyword node-type))
;;       (dispatch-node interpreter node-type node)))
;; 
;; Contemplation redes the inquiry into the reasonability of the
;; ``visit-node'' as a generic function instead of a completely
;; sufficient and more efficient usual function, in the form of
;;   
;;   (defun visit-node (interpreter node)
;;     (declare (type Interpreter interpreter))
;;     (declare (type Node        node))
;;     (let ((node-type (node-type node)))
;;       (declare (type keyword node-type))
;;       (dispatch-node interpreter node-type node)))
;; 
;; A sense of adherence to the visitor pattern in this context inclines
;; towards the interface style and thus the more elaborate solution.
;; 
;; == VARIABLES: A HASH TABLE OF NAMES AND INTEGERS ==
;; By nature of its duties, the interpreter is assigned the maintenance
;; of a program's variables. A meager degree of intricacy designates the
;; fulfilment a subject of a plain hash table, mapping each variable
;; name as a string to its integer value. A dedicated ``Variable'' class
;; is, in the face of such sufficiency, beyond the claim of
;; introduction.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-01-18
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/SolboScript"
;;   -> "https://ruslanspivak.com/lsbasi-part1/"
;;       o Describes the implementation of an interpreter on the example
;;         of the programming language Pascal realized in Python.
;;   -> "https://en.wikipedia.org/wiki/Integer_square_root"
;;       o Definition of the integer square operation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which assumes the ELEMENT-TYPE, defaulting to ``T''."
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
  "The ``property-list-of'' type defines a property list composed of
   alternating KEY-TYPE keys and VALUE-TYPE values."
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
   entries, the keys of which assume the KEY-TYPE and the values the
   VALUE-TYPE, both defaulting to ``T''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Token
  "The ``Token'' class represents a significant portion embedded in a
   piece of source code."
  (type   NIL :type (or null keyword))
  (value  NIL :type T)
  (column 0   :type fixnum)
  (line   0   :type fixnum))

;;; -------------------------------------------------------

(defun token-is-of-type (token &rest valid-types)
  "Checks whether the TOKEN type matches any of the VALID-TYPES,
   returning a ``boolean'' value of ``T'' if confirmed, otherwise
   responding with ``NIL''."
  (declare (type Token             token))
  (declare (type (list-of keyword) valid-types))
  (the boolean
    (not (null (member (token-type token) valid-types :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "The lexer requires a source.")
    :type          string
    :documentation "The SolboScript source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The index into the current character of the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character located at the current POSITION in
                    the SOURCE.")
   (column
    :initarg       :column
    :initform      0
    :type          fixnum
    :documentation "The column index of the current line in the SOURCE,
                    necessary for storing the tokens' indentations.")
   (line
    :initarg       :line
    :initform      0
    :type          fixnum
    :documentation "The current line in the SOURCE, necessary for
                    storing the tokens' indentations."))
  (:documentation
    "The ``Lexer'' class splits a piece of SolboScript source code into
     its significant portions, known as the tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (plusp (length source))
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a ``Lexer'' which analyzes the SolboScript
   SOURCE code."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning a
   ``boolean'' value of ``T'' on confirmation, otherwise ``NIL''."
  (declare (type character character))
  (not (null (member character '(#\Space #\Tab) :test #'char=))))

;;; -------------------------------------------------------

(defun newline-character-p (character)
  "Checks whether the CHARACTER represents a newline, returning a
   ``boolean'' value of ``T'' on confirmation, otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Newline #\Linefeed #\Return)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character in its maintained source, if
   possible, and updates its current character, position, as well as the
   line and column indices, before returning the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character column line) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (declare (type fixnum              column))
    (declare (type fixnum              line))
    (cond
      ((< position (1- (length source)))
        (cond
          ;; Linebreak occurred?
          ;; => Relocate the LINE and COLUMN to the beginning of the
          ;;    next line.
          ((newline-character-p character)
            (incf line)
            (setf column 0))
          ;; No linebreak occurred?
          ;; => Move to the next COLUMN.
          (T
            (incf column)))
        (incf position)
        (setf character (char source position)))
      (T
        (setf character NIL))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position, reads an alphabet word and,
   if valid, returns it as a ``Token'', otherwise signaling an error."
  (declare (type Lexer lexer))
  (with-slots (character column line) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              column))
    (declare (type fixnum              line))
    (let ((token-column column)
          (identifier   ""))
      (declare (type fixnum token-column))
      (declare (type string identifier))
      (setf identifier
        (with-output-to-string (characters)
          (declare (type string-stream characters))
          (loop while (and character (alpha-char-p character)) do
            (write-char character characters)
            (lexer-advance lexer))))
      (the Token
        (cond
          ((string= identifier "ROCK")
            (make-token
              :type   :ROCK
              :value  identifier
              :column token-column
              :line   line))
          ((string= identifier "SOLBO")
            (make-token
              :type   :SOLBO
              :value  identifier
              :column token-column
              :line   line))
          (T
            (error "Invalid word: ~s." identifier)))))))

;;; -------------------------------------------------------

(defun lexer-expect (lexer expected-character)
  "Checks whether the LEXER's current character equals the
   EXPECTED-CHARACTER, on confirmation advancing to the next character
   and returning the modified LEXER, otherwise throwing an error."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots ((current-character character) position) lexer
    (declare (type (or null character) current-character))
    (declare (type fixnum              position))
    (if (and current-character
             (char= current-character expected-character))
      (lexer-advance lexer)
      (error "Lexer expected character ~s at position ~d, ~
              but encountered ~s."
        expected-character position current-character)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns from the LEXER the next ``Token''."
  (declare (type Lexer lexer))
  
  (with-slots (character column line) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              column))
    (declare (type fixnum              line))
    
    (cond
      ;; End of source?
      ((null character)
        (make-token :type :EOF :value NIL :column column :line line))
      
      ;; Read a newline character as a token.
      ((newline-character-p character)
        (prog1
          (make-token
            :type   :newline
            :value  character
            :column column
            :line   line)
          (lexer-advance lexer)))
      
      ;; Read one or more whitespace characters as a single token.
      ((whitespace-character-p character)
        (prog1
          (let ((token-column column))
            (declare (type fixnum token-column))
            (make-token
              :type :space
              :value  (loop while (and character (whitespace-character-p character))
                        do (lexer-advance lexer)
                        count 1)
              :column token-column
              :line   line))))
      
      ;; "B" found?
      ;; => Expect a variation of "BWEE...".
      ((char= character #\B)
        (let ((token-column column))
          (declare (type fixnum token-column))
          (lexer-advance lexer)
          (lexer-expect  lexer #\W)
          (lexer-expect  lexer #\E)
          (lexer-expect  lexer #\E)
          (make-token
            :type :BWEE
            :value
              (with-output-to-string (identifier)
                (declare (type string-stream identifier))
                (write "BWEE" :stream identifier :escape NIL)
                (loop while (and character (char= character #\E)) do
                  (write-char character identifier)
                  (lexer-advance lexer)))
            :column token-column
            :line   line)))
      
      ;; Alphabetic character found?
      ;; => Expected an identifier ("ROCK" or "SOLBO").
      ((alpha-char-p character)
        (lexer-read-identifier lexer))
      
      (T
        (error "Invalid character ~s at column ~d of line ~d."
          character column line)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      NIL
    :type          (or null keyword)
    :documentation "")
   (attributes
    :initarg       :attributes
    :initform      (make-hash-table :test #'eq)
    :type          (hash-table-of keyword T)
    :documentation ""))
  (:documentation
    "The ``Node'' class represents a node, tree, or subtree in an
     abstract syntax tree (AST), identified regarding its nature by a
     type symbol, and capacitated to store the information induced by
     the processing ``Parser'' using a mapping of attributes."))

;;; -------------------------------------------------------

(defun make-node (type &rest attributes)
  "Creates and returns a ``Node'' of the specified TYPE, initially
   containing the ATTRIBUTES, supplied as a property list of attribute
   name keys associated with arbitrary values."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) attributes))
  (let ((node (make-instance 'Node :type type)))
    (declare (type Node node))
    (loop
      for (name value) of-type (keyword T) on attributes by #'cddr
      do  (setf (gethash name (slot-value node 'attributes)) value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-type (node)
  "Returns the type of the NODE."
  (declare (type Node node))
  (the keyword (slot-value node 'type)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name &optional (default NIL))
  "Returns the NODE attribute value associated with the ATTRIBUTE-NAME,
   if present, or the DEFAULT value on failure."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (declare (type T       default))
  (the T (gethash attribute-name (slot-value node 'attributes) default)))

;;; -------------------------------------------------------

(defun (setf node-attribute) (new-value node attribute-name)
  "Associates in the NODE the attribute designated by the ATTRIBUTE-NAME
   with the NEW-VALUE, overwritting any extant pairing with this key,
   and returns the modified NODE."
  (declare (type T       new-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (slot-value node 'attributes)) new-value)
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node                            node))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "Node(type=~s, attributes=~s)"
    (slot-value node 'type)
    (loop
      for key
        of-type keyword
        being the hash-keys
        in (slot-value node 'attributes)
      using   (hash-value value)
      collect (list key value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Undefined-Variable-Error".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Undefined-Variable-Error (error)
  ((variable-name
    :initarg       :variable-name
    :initform      (error "No variable name has been specified.")
    :reader        undefined-variable-error-variable-name
    :type          string
    :documentation "The unretrievable variable name."))
  (:report
    (lambda (condition stream)
      (declare (type Undefined-Variable-Error        condition))
      (declare (type (or null (eql T) stream string) stream))
      (format stream "No variable with the name ~s could be found."
        (undefined-variable-error-variable-name condition))))
  (:documentation
    "Signals that a nonexisting variable has been requested in a
     situation that mandatorily requires its presence."))

;;; -------------------------------------------------------

(defun throw-undefined-variable-error (variable-name)
  "Signals an ``Undefined-Variable-Error'' concerning the invalid
   VARIABLE-NAME."
  (declare (type string variable-name))
  (error 'Undefined-Variable-Error :variable-name variable-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Invalid-Comparison-Error".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Comparison-Error (error)
  ((comparison
    :initarg       :comparison
    :initform      (error "No comparison identity has been specified.")
    :reader        invalid-comparison-error-comparison
    :type          keyword
    :documentation ""))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Comparison-Error        condition))
      (declare (type (or null (eql T) stream string) stream))
      (format stream "The comparison identifier ~s is invalid."
        (invalid-comparison-error-comparison condition))))
  (:documentation
    "Signals that an invalid variable comparison has been specified."))

;;; -------------------------------------------------------

(defun throw-invalid-comparison-error (comparison)
  "Signals an ``Invalid-Comparison-Error'' concerning the offending
   COMPARISON."
  (declare (type keyword comparison))
  (error 'Invalid-Comparison-Error :comparison comparison))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "The parser requires a lexer.")
    :type          Lexer
    :documentation "The lexical analyzer employed to supply the tokens.")
   (current-token
    :initarg       :current-token
    :initform      NIL
    :type          (or null Token)
    :documentation "The last token requested from the LEXER."))
  (:documentation
    "The ``Parser'' class is responsible for evaluating a sequence of
     tokens, obtained from a lexer, as a valid or invalid program,
     producing the in the former case an abstract syntax tree (AST)
     representation of the consumed code, or throwing in the latter
     situation an error."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a ``Parser'' which obtains its tokens from the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token belongs to the
   EXPECTED-TOKE-TYPE, on success requesting the next token and
   returning the modified PARSER; on failure signaling an error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (if (token-is-of-type current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Parser expected to eat a token of the type ~s, but ~
              encountered the token ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip (parser &rest negligible-token-types)
  "Skips zero or more subsequent tokens of any of the
   NEGLIGIBLE-TOKEN-TYPES using the PARSER, and returns the modified
   PARSER."
  (declare (type Parser            parser))
  (declare (type (list-of keyword) negligible-token-types))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (loop
      while (and current-token
                 (apply #'token-is-of-type
                        current-token negligible-token-types))
      do (setf current-token (lexer-get-next-token lexer))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-whitespaces (parser)
  "Skips zero or more subsequent space or newline tokens using the
   PARSER, and returns the modified PARSER."
  (declare (type Parser parser))
  (parser-skip parser :newline :space)
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-eat-whitespaces (parser)
  "Skips one or more space or newline tokens using the PARSER, and
   returns the modified PARSER.
   ---
   An error is signaled if not at least one space or newline token can
   be detected."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (case (token-type current-token)
      (:newline
        (parser-eat              parser :newline)
        (parser-skip-whitespaces parser))
      (:space
        (parser-eat              parser :space)
        (parser-skip-whitespaces parser))
      (otherwise
        (error "Expected newline or whitespace, but encountered ~s."
          current-token))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-binary-operation (parser operation)
  "Parses binary operation conforming to the pattern
     binaryOperation := leftVariable , rightVariable , outputVariable ;
   using the PARSER, and returns three values:
     (1) the left variable name as a string,
     (2) the right variable name as a string, and
     (3) the output variable name as a string."
  (declare (type Parser  parser))
  (declare (type keyword operation))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    
    (parser-eat parser :bwee)
    (parser-eat-whitespaces parser)
    
    (let ((left-variable   NIL)
          (right-variable  NIL)
          (output-variable NIL))
      (declare (type (or null Token) left-variable))
      (declare (type (or null Token) right-variable))
      (declare (type (or null Token) output-variable))
      
      (setf left-variable current-token)
      (parser-eat parser :bwee)
      (parser-eat-whitespaces parser)
      
      (setf right-variable current-token)
      (parser-eat parser :bwee)
      (parser-eat-whitespaces parser)
      
      (setf output-variable current-token)
      (parser-eat parser :bwee)
      
      (the Node
        (make-node :binary-operation
          :operation operation
          :left-variable   (token-value left-variable)
          :right-variable  (token-value right-variable)
          :output-variable (token-value output-variable))))))

;;; -------------------------------------------------------

(defun parser-parse-conditional (parser)
  "Parses a conditional operation of the pattern
    conditionalOperation := leftVariable, comparison , rightVariable ;
    comparison           := 'BWEE' | 'BWEEE' | 'BWEEEE' ;
   using the PARSER, and returns four values:
     (1) the left variable name as a string
     (2) the right variable name as a string
     (3) the comparison operator as a keyword symbol among
           ``:equal'' for 'BWEE',
           ``:greater-than'' for 'BWEEE',
           ``:less-than'' for 'BWEEEE',
         and
     (4) a list of ``Node'' objects, containing the body statements."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((indentation    0)
          (left-variable  NIL)
          (comparison     NIL)
          (right-variable NIL)
          (statements     NIL))
      (declare (type (integer 0 *)   indentation))
      (declare (type (or null Token) left-variable))
      (declare (type (or null Token) comparison))
      (declare (type (or null Token) right-variable))
      (declare (type (list-of Node)  statements))
      
      (setf indentation (token-column current-token))
      
      (parser-eat parser :bwee)
      (parser-eat-whitespaces parser)
      
      (setf left-variable current-token)
      (parser-eat parser :bwee)
      (parser-eat-whitespaces parser)
      
      (setf comparison current-token)
      (parser-eat parser :bwee)
      (parser-eat-whitespaces parser)
      
      (setf right-variable current-token)
      (parser-eat parser :bwee)
      (parser-eat-whitespaces parser)
      
      (setf statements
        (parser-parse-compound-statement parser indentation))
      
      (the (values string string keyword (list-of Node))
        (values
          (token-value left-variable)
          (token-value right-variable)
          (let ((identifier (token-value comparison)))
            (declare (type string identifier))
            (cond
              ((string= identifier "BWEE")   :equal)
              ((string= identifier "BWEEE")  :greater-than)
              ((string= identifier "BWEEEE") :less-than)
              (T
                (throw-invalid-comparison-error comparison))))
          statements)))))

;;; -------------------------------------------------------

(defun parser-parse-statement (parser)
  "Parses a statement using the PARSER, and returns a ``Node''
   containing its data."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    
    (let ((identifier (token-value current-token)))
      (declare (type string identifier))
      
      (the Node
        (cond
          ;; BWEE (variable)
          ;; => Print variable value verbatim, that is, as a number.
          ((string= identifier "BWEE")
            (parser-eat             parser :bwee)
            (parser-eat-whitespaces parser)
            (let ((variable current-token))
              (declare (type Token variable))
              (parser-eat parser :bwee)
              (make-node
                :print-number
                :variable (token-value variable))))
          
          ;; BWEEE (variable)
          ;; => Print variable value as a letter.
          ((string= identifier "BWEEE")
            (parser-eat             parser :bwee)
            (parser-eat-whitespaces parser)
            (let ((variable current-token))
              (declare (type Token variable))
              (parser-eat parser :bwee)
              (make-node
                :print-letter
                :variable (token-value variable))))
          
          ;; BWEEEE (variable) (value)
          ;; => Define a variable and its value.
          ((string= identifier "BWEEEE")
            (parser-eat parser :bwee)
            (parser-eat-whitespaces parser)
            (let ((variable current-token))
              (declare (type Token variable))
              (parser-eat             parser :bwee)
              (parser-eat-whitespaces parser)
              (let ((value current-token))
                (declare (type Token variable))
                (parser-eat parser :bwee)
                (make-node
                  :define
                  :variable (token-value variable)
                  :value    (token-value value)))))
          
          ;; BWEEEEE (variable)
          ;; => Increment variable.
          ((string= identifier "BWEEEEE")
            (parser-eat             parser :bwee)
            (parser-eat-whitespaces parser)
            (let ((variable current-token))
              (declare (type Token variable))
              (parser-eat parser :bwee)
              (make-node
                :increment
                :variable (token-value variable))))
          
          ;; BWEEEEEE (variable)
          ;; => Decrement variable.
          ((string= identifier "BWEEEEEE")
            (parser-eat             parser :bwee)
            (parser-eat-whitespaces parser)
            (let ((variable current-token))
              (declare (type Token variable))
              (parser-eat parser :bwee)
              (make-node :decrement
                :variable (token-value variable))))
          
          ;; BWEEEEEEE (augendVariable) (addendVariable) (sumVariable)
          ;; => Add two variables.
          ((string= identifier "BWEEEEEEE")
            (parser-parse-binary-operation parser :addition))
          
          ;; BWEEEEEEEE (minuendVariable) (subtrahendVariable) (differenceVariable)
          ;; => Subtract two variables.
          ((string= identifier "BWEEEEEEEE")
            (parser-parse-binary-operation parser :subtraction))
          
          ;; BWEEEEEEEEE (multiplicandVariable) (multiplierVariable) (productVariable)
          ;; => Multiply two variables.
          ((string= identifier "BWEEEEEEEEE")
            (parser-parse-binary-operation parser :multiplication))
          
          ;; BWEEEEEEEEEE (dividendVariable) (divisorVariable) (quotientVariable)
          ;; => Divide two variables.
          ((string= identifier "BWEEEEEEEEEE")
            (parser-parse-binary-operation parser :division))
          
          ;; BWEEEEEEEEEEE (variable1) (BWEE | BWEEE | BWEEEE) (variable2)
          ;; => If variable1 ( = | > | < ) variable2
          ((string= identifier "BWEEEEEEEEEEE")
            (multiple-value-bind
                (left-variable right-variable comparison statements)
                (parser-parse-conditional parser)
              (declare (type string         right-variable))
              (declare (type string         left-variable))
              (declare (type keyword        comparison))
              (declare (type (list-of Node) statements))
              (make-node :if
                :left-variable  left-variable
                :right-variable right-variable
                :comparison     comparison
                :statements     statements)))
          
          ;; BWEEEEEEEEEEEE
          ;; => Loop forever.
          ((string= identifier "BWEEEEEEEEEEEE")
            (let ((indentation (token-column current-token)))
              (declare (type (integer 0 *) indentation))
              (parser-eat parser :bwee)
              (make-node :loop-forever
                :statements
                  (parser-parse-compound-statement parser indentation))))
          
          ;; BWEEEEEEEEEEEEE (leftVariable) (BWEE | BWEEE | BWEEEE) (rightVariable)
          ;; => Loop until...
          ;;      leftVariable = rightVariable, if BWEE
          ;;      leftVariable > rightVariable, if BWEEE
          ;;      leftVariable < rightVariable, if BWEEEE.
          ((string= identifier "BWEEEEEEEEEEEEE")
            (multiple-value-bind (left-variable right-variable comparison statements)
                (parser-parse-conditional parser)
              (declare (type string         right-variable))
              (declare (type string         left-variable))
              (declare (type keyword        comparison))
              (declare (type (list-of Node) statements))
              (make-node :loop-until
                :left-variable  left-variable
                :right-variable right-variable
                :comparison     comparison
                :statements     statements)))
          
          (T
            (error "Not an instruction identifier: ~s." identifier)))))))

;;; -------------------------------------------------------

(defun parser-parse-compound-statement (parser indentation)
  "Parses a sequence of zero or more statements, whose introducing
   command identifiers must start at a column after the INDENTATION,
   using the PARSER, and returns a list of ``Node'' objects, each of
   which represents one such recognized statement."
  (declare (type Parser        parser))
  (declare (type (integer 0 *) indentation))
  
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    
    (let ((statements NIL))
      (declare (type (list-of Node) statements))
      
      (loop do
        (cond
          ((null current-token)
            (loop-finish))
          
          ((token-is-of-type current-token :eof)
            (loop-finish))
          
          ;; Command token inside of INDENTATION?
          ;; => Belongs to the compound STATEMENTS list.
          ((and (token-is-of-type current-token :bwee)
                (> (token-column current-token) indentation))
            (push (parser-parse-statement parser) statements))
          
          ;; Command token outside of INDENTATION?
          ;; => End of the compound STATEMENTS list.
          ((and (token-is-of-type current-token :bwee)
                (<= (token-column current-token) indentation))
            (loop-finish))
          
          ((token-is-of-type current-token :space)
            (parser-eat parser (token-type current-token)))
          
          ((token-is-of-type current-token :newline)
            (parser-eat parser (token-type current-token)))
          
          (T
            (loop-finish))))
      
      (the (list-of Node) (nreverse statements)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Orders the PARSER to parse the tokens received from its internally
   managed lexer and returns the root node of the thus generated
   abstract syntax tree (AST)."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    
    (parser-skip parser :newline :space)
    
    (parser-eat  parser :rock)
    (parser-eat-whitespaces parser)
    (parser-eat  parser :solbo)
    
    (let ((program    (make-node :program))
          (statements NIL))
      (declare (type Node           program))
      (declare (type (list-of Node) statements))
      
      (loop do
        (cond
          
          ((null current-token)
            (loop-finish))
          
          ((token-is-of-type current-token :eof)
            (loop-finish))
          
          ((token-is-of-type current-token :bwee)
            (push (parser-parse-statement parser) statements))
          
          ((token-is-of-type current-token :newline)
            (parser-skip-whitespaces parser))
          
          ((token-is-of-type current-token :space)
            (parser-skip-whitespaces parser))
          
          (T
            (error "Encountered unexpected token ~s while parsing."
              current-token))))
      
      (setf (node-attribute program :statements)
            (nreverse statements))
      
      (the Node program))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array character (*)) +CHARACTER-REPERTOIRE+))

;;; -------------------------------------------------------

(defparameter +CHARACTER-REPERTOIRE+
  (make-array 99
    :element-type 'character
    :adjustable   NIL 
    :fill-pointer NIL
    :initial-contents
    '(
      ;; Letters.
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      ;; Digits.
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      ;; Whitespaces.
      #\Space #\Tab #\Newline
      ;; Punctuation marks.
      #\! #\? #\. #\: #\, #\; #\~ #\- #\_
      ;; Slashes.
      #\/ #\| #\\
      ;; Quotation marks.
      #\' #\" #\`
      ;; Brackets.
      #\[ #\] #\( #\) #\{ #\}
      ;; Mathematics.
      #\+ #\- #\* #\= #\< #\>
      ;; Miscellany.
      #\^ #\@ #\$ #\% #\& #\#
      ;; Controls.
      #\Null))
  "The default SolboScript character repertoire, mapping to an integer
   position one character.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "The interpreter requires an AST root node.")
    :type          Node
    :documentation "The root node of the abstract syntax tree (AST)
                    to process with this interpreter.")
   (variables
    :initarg       :variables
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string integer)
    :documentation "Maps a variable name to an integer value.")
   (letters
    :initarg       :letters
    :initform      +CHARACTER-REPERTOIRE+
    :accessor      interpreter-letters
    :type          (simple-array character (*))
    :documentation "Maps a positive integer number to a character
                    intended to represent the 'letter' equivalent for a
                    numeric variable value."))
  (:documentation
    "The ``Interpreter'' class processes the nodes of an abstract syntax
     tree, usually furnished by a ``Parser'', and exercises effects
     which manifest a SolboScript program's intended functionality."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a new ``Interpreter'' which processes the
   abstract syntax TREE."
  (declare (type Node tree))
  (the Interpreter (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun get-numeric-value (identifier)
  "Returns the numeric value associated with the IDENTIFIER, which is
   tacitly supposed to be of the pattern 'BWEE...', terminating in an
   arbitrary number of 'E's."
  (declare (type string identifier))
  (the (integer 1 *) (1- (count #\E identifier :test #'char=))))

;;; -------------------------------------------------------

(defun interpreter-get-letter-for (interpreter numeric-value)
  "Returns the letter associated with the NUMERIC-VALUE according to the
   definitions maintained by the INTERPRETER."
  (declare (type Interpreter   interpreter))
  (declare (type (integer 1 *) numeric-value))
  (the character
    (aref (slot-value interpreter 'letters) (1- numeric-value))))

;;; -------------------------------------------------------

(defun comparison-satisfied-p (comparison left-value right-value)
  "Checks whether the COMPARISON, when applied to the LEFT-VALUE and
   the RIGHT-VALUE in this order, is to be considered satisfied,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type keyword comparison))
  (declare (type integer left-value))
  (declare (type integer right-value))
  (the boolean
    (not (null
      (case comparison
        (:equal        (= left-value right-value))
        (:greater-than (> left-value right-value))
        (:less-than    (< left-value right-value))
        (otherwise     (throw-invalid-comparison-error comparison)))))))

;;; -------------------------------------------------------

(defun interpreter-variable-with-name (interpreter name)
  "Returns the integer value of the variable associated with the NAME
   in the INTERPRETER, signaling an error of the type
   ``Undefined-Variable-Error'' upon failure to locate the identifier."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (with-slots (variables) interpreter
    (declare (type (hash-table-of string integer) variables))
    (multiple-value-bind (variable-value contains-variable)
        (gethash name variables)
      (declare (type (or null integer) variable-value))
      (declare (type T                 contains-variable))
      (the integer
        (if contains-variable
          variable-value
          (throw-undefined-variable-error name))))))

;;; -------------------------------------------------------

(defun (setf interpreter-variable-with-name) (new-value interpreter name)
  "Sets the value of the variable registered with the NAME at the
   INTERPRETER to the NEW-VALUE, or, if no such entity exists, creates
   and stores a new association of the variable NAME with this value,
   in both cases returning the modified INTERPRETER."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (with-slots (variables) interpreter
    (declare (type (hash-table-of string integer) variables))
    (setf (gethash name variables) new-value))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Visits the NODE using the INTERPRETER, returning a value
     appropriate for the processed NODE's purpose."))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE of the specified NODE-TYPE using the
     INTERPRETER, returning a value appropriate for the processed NODE's
     purpose.
     ---
     This generic function constitutes an adminicular warkloom for the
     realization of the ``visit-node'' operation in the absence of a
     signum in specialized ``Node'' subclasses expected to prevail in
     orderly solutions. In concise parlance, with only one ``Node''
     class defined, the ``visit-node'' function cannot dispatch on
     conceptually disjoint node types. This function, when invoked by
     the routine, employs the NODE-TYPE, extracted from the NODE
     argument's ``type'' slot, to dispatch on the identifying type
     keyword symbols. In pseudocode we obtain:
       
       function visitNode (interpreter, node)
         let (nodeType : keyword) <- node.getType ()
         dispatchNode (interpreter, nodeType, node)
       end function"))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter) (node Node))
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (let ((node-type (node-type node)))
    (declare (type keyword node-type))
    (dispatch-node interpreter node-type node)))

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
                          (node-type   (eql :print-number))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  
  (let ((variable-name (node-attribute node :variable)))
    (declare (type string variable-name))
    (write (interpreter-variable-with-name interpreter variable-name)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :print-letter))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  
  (let ((variable-name (node-attribute node :variable)))
    (declare (type string variable-name))
    (write-char
      (interpreter-get-letter-for interpreter
        (interpreter-variable-with-name interpreter variable-name))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :define))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  
  (let ((variable-name  (node-attribute node :variable))
        (variable-value (node-attribute node :value)))
    (declare (type string variable-name))
    (setf (interpreter-variable-with-name interpreter variable-name)
          (get-numeric-value variable-value)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :increment))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (type Node        node))
  (let ((variable (node-attribute node :variable)))
    (declare (type string variable))
    (incf (interpreter-variable-with-name interpreter variable)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :decrement))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (type Node        node))
  (let ((variable (node-attribute node :variable)))
    (declare (type string variable))
    (decf (interpreter-variable-with-name interpreter variable)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :binary-operation))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((left-variable   (node-attribute node :left-variable))
        (right-variable  (node-attribute node :right-variable))
        (output-variable (node-attribute node :output-variable))
        (operation       (node-attribute node :operation)))
    (declare (type string  left-variable))
    (declare (type string  right-variable))
    (declare (type string  output-variable))
    (declare (type keyword operation))
    (let ((callback
            (case operation
              (:addition       #'+)
              (:subtraction    #'-)
              (:multiplication #'*)
              (:division       #'round)
              (otherwise       (error "Invalid operation: ~s."
                                 operation)))))
      (declare (type (function (integer integer) integer) callback))
      (setf (interpreter-variable-with-name interpreter output-variable)
        (funcall callback
          (interpreter-variable-with-name interpreter left-variable)
          (interpreter-variable-with-name interpreter right-variable)))))
  (values))


;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :if))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((left-variable  (node-attribute node :left-variable))
        (right-variable (node-attribute node :right-variable))
        (comparison     (node-attribute node :comparison))
        (statements     (node-attribute node :statements)))
    (declare (type string         left-variable))
    (declare (type string         right-variable))
    (declare (type keyword        comparison))
    (declare (type (list-of Node) statements))
    (let ((left-value
            (interpreter-variable-with-name interpreter left-variable))
          (right-value
            (interpreter-variable-with-name interpreter right-variable)))
      (declare (type integer left-value))
      (declare (type integer right-value))
      (when (comparison-satisfied-p comparison left-value right-value)
        (dolist (statement statements)
          (visit-node interpreter statement)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :loop-forever))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((statements (node-attribute node :statements)))
    (declare (type (list-of Node) statements))
    (loop do
      (dolist (statement statements)
        (declare (type Node statement))
        (visit-node interpreter statement))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :loop-until))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((left-variable  (node-attribute node :left-variable))
        (right-variable (node-attribute node :right-variable))
        (comparison     (node-attribute node :comparison))
        (statements     (node-attribute node :statements)))
    (declare (type string         left-variable))
    (declare (type string         right-variable))
    (declare (type keyword        comparison))
    (declare (type (list-of Node) statements))
    (loop
      until
        (comparison-satisfied-p comparison
          (interpreter-variable-with-name interpreter left-variable)
          (interpreter-variable-with-name interpreter right-variable))
      do
        (dolist (statement statements)
          (visit-node interpreter statement))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree maintained by the INTERPRETER
   and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-SolboScript (code)
  "Interprets the piece of SolboScript CODE and returns no value."
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

;; Print "hello".
(interpret-SolboScript
"
ROCK SOLBO
BWEEEE BWEE BWEEEEEEEEE
BWEEEE BWEEE BWEEEEEE
BWEEEE BWEEEE BWEEEEEEEEEEEEE
BWEEEE BWEEEEE BWEEEEEEEEEEEEEEEE
BWEEE BWEE BWEEE BWEEE BWEEE BWEEEE BWEEE BWEEEE BWEEE BWEEEEE
")

;;; -------------------------------------------------------

;; If a = b then print c
(interpret-SolboScript
"
ROCK SOLBO
BWEEEE BWEE BWEEEEEEE 
BWEEEE BWEEE BWEEEEE 
BWEEEE BWEEEE BWEEEE 
BWEEEEEEEEEEE BWEE BWEE BWEEE 
  BWEEE BWEEEE
")

;;; -------------------------------------------------------

;; Fibonacci numbers.
(interpret-SolboScript
"
ROCK SOLBO
BWEEEE BWEE BWEE
BWEEEE BWEEE BWEE
BWEEEEEEEEEEEE
  BWEE BWEE
  BWEE BWEEE
  BWEEEEEEE BWEE BWEEE BWEE
  BWEEEEEEE BWEE BWEEE BWEEE
")

;;; -------------------------------------------------------

;; Print the letters "a" to "z".
(interpret-SolboScript
"
ROCK SOLBO

BWEEEE BWEE  BWEE
BWEEEE BWEEE BWEEEEEEEEEEEEEEEEEEEEEEEEEEE

BWEEEEEEEEEEEEE BWEE BWEEE BWEEE
  BWEEE   BWEE
  BWEEEEE BWEE
")

;;; -------------------------------------------------------

;; Multiplication table from 1x1 to 10x10.
;; 
;; Concepts:
;;   N = 10    // BWEE
;;   A = 1     // BWEEE
;;   B = 1     // BWEEEE
;;   C = NIL   // BWEEEEE
;;   T = 'X'   // BWEEEEEE
;;   E = '='   // BWEEEEEEE
;;   S = ' '   // BWEEEEEEEE
;;   
;;   LOOP UNTIL A > N
;;     LOOP UNTIL B > N
;;       PRINT A
;;       PRINT-LETTER T
;;       PRINT-B
;;       PRINT-LETTER E
;;       SET C = A * B
;;       PRINT C
;;       PRINT-LETTER S
;;       B = B + 1
;;     END LOOP
;;     A = A + 1
;;     B = 1
;;   END LOOP
(interpret-SolboScript
"
ROCK SOLBO

BWEEEE BWEE       BWEEEEEEEEEEE
BWEEEE BWEEE      BWEE
BWEEEE BWEEEE     BWEE
BWEEEE BWEEEEEE   BWEEEEEEEEEEEEEEEEEEEEEEEEE
BWEEEE BWEEEEEEE  BWEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
BWEEEE BWEEEEEEEE BWEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE

BWEEEEEEEEEEEEE BWEEE BWEEE BWEE
  BWEEEEEEEEEEEEE BWEEEE BWEEE BWEE
    BWEE BWEEE
    BWEEE BWEEEEEE
    BWEE BWEEEE
    BWEEE BWEEEEEEE
    BWEEEEEEEEE BWEEE BWEEEE BWEEEEE
    BWEE BWEEEEE
    BWEEEEE BWEEEE
    BWEEE BWEEEEEEEE
  BWEEEEE BWEEE
  BWEEEE BWEEEE BWEE
")

;;; -------------------------------------------------------

;; Implements the integer square root operation isqrt(y) using linear
;; search.
;; 
;; Algorithm:
;;   
;;   Input:
;;     y : integer
;;   
;;   Output:
;;     None.
;;   
;;   
;;   Process:
;;     x <- 1   { Result of isqrt(y).                      }
;;     z <- 1   { Temporary variable with value = (x * x). }
;;     
;;     loop until (z > y)
;;       x <- x + 1
;;       z <- x * x
;;     end loop
;;     
;;     x <- x - 1
;;     print(x)
;; 
;; The following variable mappings are employed:
;;   BWEE   -> y
;;   BWEEE  -> x
;;   BWEEEE -> z
(interpret-SolboScript
"
ROCK SOLBO

BWEEEE BWEE   BWEEEEEEEEEEEEEEEEEEEEEEEEEEEE
BWEEEE BWEEE  BWEE
BWEEEE BWEEEE BWEE

BWEEEEEEEEEEEEE BWEEEE BWEEE BWEE
  BWEEEEE     BWEEE
  BWEEEEEEEEE BWEEE BWEEE BWEEEE

BWEEEEEE BWEEE
BWEE BWEEE
")
