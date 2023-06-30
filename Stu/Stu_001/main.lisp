;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Stu", invented by the Esolang user "PythonshellDebugwindow"
;; and presented in the year 2019, its design being inspired by a
;; fictitious young lad nevened in accordance with the language, whose
;; desiderata's expression follows a simulacrum of natural English
;; sentences.
;; 
;; 
;; Concept
;; =======
;; The Stu programming language simulates the requests of an eponymous
;; little boy, the inchoation of each such proceeds via the fragmentary
;; wording "Stu wants to", in its succession completed by several
;; euonyms that serve in the command diorisms' differentiation.
;; 
;; Every phrase not distinguishable as a valid instruction is attributed
;; with irrelevance, and consequently neglected.
;; 
;; == STU: A PSEUDONATURAL LANGUAGE ==
;; The language Stu follows the English grammar in order to express its
;; instructions in conformance with an optative request for actions.
;; 
;; == SIMON SAYS ... AND "STU WANTS TO ..."
;; All operations are expressed in English sentences akin to natural
;; language, entailing the optations communicated by a boy named Stu,
;; its inchoation always introduced with the parasceuastic
;; "Stu wants to " fragment.
;; 
;; == EVERY LINE AN OPTATION ==
;; A Stu program constitutes a composition of zero or more lines, each
;; effective specimen of which accommodates the commorancy of a single
;; instruction.
;; 
;; == EMPTY AND INVALID LINES ARE IGNORED ==
;; Vacancy on a line, and invalid sentences similiter, receive no
;; attention.
;; 
;; == VARIABLES ==
;; Variables permit the insertion of dynamic content in the language,
;; however, tallying among the exclusive warklooms of user input
;; storage, as no other avenue for their initialization or assignment
;; is offered.
;; 
;; A variable's identification follows from a sequence of one or more
;; Latin letters and/or decimal digits.
;; 
;; 
;; Architecture
;; ============
;; No particular demands are levied against the architectural
;; requisitums, except for the castaldy of an arbitrary amount of
;; variables.
;; 
;; == VARIABLES MAY BE STORED IN A DICTIONARY OR MAP ==
;; Any variable is addressed by a unique identifier, the indagation of
;; the same answers to the stored string content. This concept's
;; plain character, as well as the indifference allied with the entries'
;; order, redes a simple associative solution, contingently manifesting
;; in a hashed variant, for instance as a hash table, the affiliations
;; of which map string keys to string values.
;; 
;; 
;; Data Types
;; ==========
;; The Stu programming language wists of one data type only: strings of
;; arbitrary length, ensconced in a jumelle of double quotations marks
;; ("...").
;; 
;; 
;; Syntax
;; ======
;; Stu's syntactical department is defined by a sequence of zero or more
;; lines, each non-empty member comprehends a single instruction in the
;; vein of a valid English sentence, representing a conceived boy's
;; optative statements.
;; 
;; == INSTRUCTIONS ==
;; Instructions engage in the mimicry of natural language allocated to
;; the volitive expressions of a young lad.
;; 
;; Every instruction occupies the commorancy of its personal line,
;; contingently preceded and concluded by optional spaces, but empighted
;; with a stringent design in the main section, and interdicted to share
;; the horizontal reservation with a peer.
;; 
;; Empty lines, as well as lines that do not contribute valid commands,
;; are simply ignored, in lieu of an error's encumbrance for the
;; program.
;; 
;; == VARIABLES ==
;; The variable agnomination requires the identifiers' assumption to
;; prescribe a length of one or more characters, any such either a Latin
;; letter or a decimal digit.
;; 
;; == COMMENTS ==
;; No dedicated facility partakes of the current Stu standard's
;; rendition for comments --- however, any line not embracing a valid
;; instruction is simply ignored, without incurring an error, and thus
;; serves in a gloze's sense.
;; 
;; == GRAMMAR ==
;; An illustration of the Stu syntaxis shall be the following Extended
;; Backus-Naur Form's (EBNF) cynosure:
;; 
;;   program           := { innerEmptyLine }
;;                     ,  { innerCommandLine | innerEmptyLine }
;;                     ,  ( lastCommandLine | lastEmptyLine )
;;                     ;
;;   
;;   innerCommandLine  := optionalSpaces , command , optionalSpaces
;;                     ,  newline
;;                     ;
;;   lastCommandLine   := optionalSpaces , command , optionalSpaces ;
;;   innerEmptyLine    := optionalSpaces , newline ;
;;   lastEmptyLine     := optionalSpaces ;
;;   
;;   command           := input | output | quit | jump ;
;;   
;;   prelude           := "Stu wants to " ,
;;   
;;   input             := prelude , inputStart , inputStore
;;                     ,  ( "!"
;;                        | ( inputNew , "." )
;;                        | ( inputNew , condition )
;;                        | condition
;;                        )
;;                     ;
;;   inputStart        := ( "know something " , [ ", " ] )
;;                     |  ( "ask you something " , [ ", "] )
;;                     ;
;;   inputStore        := "and put it in " , variable ;
;;   inputNew          := " unless he already knows it" ;
;;   
;;   output            := prelude , "tell you something:"
;;                     ,  outputExpressions
;;                     ,  [ condition ]
;;                     ;
;;   outputExpressions := { " " , expression } ;
;;   
;;   quit              := prelude , "leave now" , ( "." | condition ) ;
;;   
;;   jump              := prelude , "go home now"
;;                     , ( "." |  condition )
;;                     ;
;;   
;;   condition         := " if " , expression , " and " , expression
;;                     ,  " are " , [ "not " ] , "similar."
;;                     ;
;;   
;;   newline           := "\n" ;
;;   optionalSpaces    := { space } ;
;;   space             := " " | "\t" ;
;;   expression        := string | variable ;
;;   string            := '"' , { character - '"' } , '"' ;
;;   variable          := nameCharacter , { nameCharacter } ;
;;   nameCharacter     := letter | digit ;
;;   letter            := "A" | ... | "Z" | "a" | ... | "z" ;
;;   digit             := "0" | "1" | "2" | "3" | "4"
;;                     |  "5" | "6" | "7" | "8" | "9"
;;                     ;
;; 
;; 
;; Instructions
;; ============
;; Stu's instruction set ostends a quadruple variation, its amplectation
;; entailing input and output facilities, a program restarting
;; mechanism, and a termination command.
;; 
;; Every command is amenable to augmentation by a conditional part, a
;; faculty resident in the equality and inequality predicates' realm.
;; 
;; == OVERVIEW ==
;; Stu's four operations shall be the following apercu's material, ere a
;; more explicit nortelry will be adhibited in the coming sections.
;; 
;; Please note a set of important regulations in the adopted form:
;; 
;;   (a) Placeholder portions are ensconced in a jumelle of braces,
;;       "{" and "}". The delimitation marks must be elided in the
;;       effective statement and the amplected body substituted by
;;       admissive code.
;;   (b) Variations on the same statement, especially concerning the
;;       input operation, form a list separated by a line of periods
;;       (".").
;;   (c) The statements' nimious expanse imposes a linebreak in the
;;       presentation that does not actually partake of the command's
;;       design.
;; 
;; The commands for input, output, quitting, and jumping are produced
;; alow in this order:
;; 
;;   +-----------------------------------------+
;;   | Input, with reassignment, unconditional |
;;   +-----------------------------------------+-----------------------
;;   | Queries the user for an input string and stores it in the
;;   | {variable}. If the same already exists, its content is replaced.
;;   +-----------------------------------------------------------------
;;   Stu wants to know something and put it in {variable}!
;;   ..................................................................
;;   Stu wants to know something, and put it in {variable}!
;;   ..................................................................
;;   Stu wants to ask you something and put it in {variable}!
;;   ..................................................................
;;   Stu wants to ask you something, and put it in {variable}!
;;   ------------------------------------------------------------------
;;   +----------------------------------------------------+
;;   | Input, with reassignment, conditional, affirmative |
;;   +----------------------------------------------------+------------
;;   | Queries the user for an input string and stores it in the
;;   | {variable}, contingently replacing the extant content, if the
;;   | {left} and {right} operands are equal. Otherwise no effect is
;;   | imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to know something and put it in {variable}
;;     if {left} and {right} are similar.
;;   ..................................................................
;;   Stu wants to know something, and put it in {variable}
;;     if {left} and {right} are similar.
;;   ..................................................................
;;   Stu wants to ask you something and put it in {variable}
;;     if {left} and {right} are similar.
;;   ..................................................................
;;   Stu wants to ask you something, and put it in {variable}
;;     if {left} and {right} are similar.
;;   ------------------------------------------------------------------
;;   +-------------------------------------------------+
;;   | Input, with reassignment, conditional, negative |
;;   +-------------------------------------------------+---------------
;;   | Queries the user for an input string and stores it in the
;;   | {variable}, contingently replacing the extant content, if the
;;   | {left} and {right} operands are not equal. Otherwise no effect
;;   | is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to know something and put it in {variable}
;;     if {left} and {right} are not similar.
;;   ..................................................................
;;   Stu wants to know something, and put it in {variable}
;;     if {left} and {right} are not similar.
;;   ..................................................................
;;   Stu wants to ask you something and put it in {variable}
;;     if {left} and {right} are not similar.
;;   ..................................................................
;;   Stu wants to ask you something, and put it in {variable}
;;     if {left} and {right} are not similar.
;;   ------------------------------------------------------------------
;;   +--------------------------------------------+
;;   | Input, without reassignment, unconditional |
;;   +--------------------------------------------+--------------------
;;   | Queries the user for an input string and stores it in the
;;   | {variable}. If the same already exists, no query is issued, and
;;   | no effect is imparted.
;;   +-----------------------------------------------------------------
;;   Stu wants to know something and put it in {variable}
;;     unless he already knows it.
;;   ..................................................................
;;   Stu wants to know something, and put it in {variable}
;;     unless he already knows it.
;;   ..................................................................
;;   Stu wants to ask you something and put it in {variable}
;;     unless he already knows it.
;;   ..................................................................
;;   Stu wants to ask you something, and put it in {variable}
;;     unless he already knows it.
;;   ------------------------------------------------------------------
;;   +-------------------------------------------------------+
;;   | Input, without reassignment, conditional, affirmative |
;;   +-------------------------------------------------------+---------
;;   | Queries the user for an input string and stores it in the
;;   | {variable}, presuming that such a variable does not yet exist,
;;   | if the {variable} does not exist yet and the {left} and {right}
;;   | operands are equal. Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to know something and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are similar.
;;   ..................................................................
;;   Stu wants to know something, and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are similar.
;;   ..................................................................
;;   Stu wants to ask you something and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are similar.
;;   ..................................................................
;;   Stu wants to ask you something, and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are similar.
;;   ------------------------------------------------------------------
;;   +----------------------------------------------------+
;;   | Input, without reassignment, conditional, negative |
;;   +----------------------------------------------------+------------
;;   | Queries the user for an input string and stores it in the
;;   | {variable}, presuming that such a variable does not yet exist,
;;   | if the {variable} does not exist yet and the {left} and {right}
;;   | operands are not equal. Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to know something and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are not similar.
;;   ..................................................................
;;   Stu wants to know something, and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are not similar.
;;   ..................................................................
;;   Stu wants to ask you something and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are not similar.
;;   ..................................................................
;;   Stu wants to ask you something, and put it in {variable}
;;     unless he already knows it
;;     if {left} and {right} are not similar.
;;   ------------------------------------------------------------------
;;   
;;   +-----------------------+
;;   | Output, unconditional |
;;   +-----------------------+-----------------------------------------
;;   | Concatenates the {expressions} into a single string, without an
;;   | adscititious separator, and prints the same to the standard
;;   | output.
;;   | ----------------------------------------------------------------
;;   | The {expressions} must be a sequence of zero or more items, each
;;   | either a string or a variable name, demarcated by a single space
;;   | character. If a string, the value is directly employed;
;;   | variables are substituted by the values.
;;   +-----------------------------------------------------------------
;;   Stu wants to tell you something: {expressions}
;;   ------------------------------------------------------------------
;;   +----------------------------------+
;;   | Output, conditional, affirmative |
;;   +----------------------------------+------------------------------
;;   | Concatenates the {expressions} into a single string, without an
;;   | adscititious separator, and prints the same to the standard
;;   | output, if the {left} and {right} operands are equal. Otherwise
;;   | no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {expressions} must be a sequence of zero or more items, each
;;   | either a string or a variable name, demarcated by a single space
;;   | character. If a string, the value is directly employed;
;;   | variables are substituted by the values.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to tell you something: {expressions}
;;     if {left} and {right} are similar.
;;   ------------------------------------------------------------------
;;   +-------------------------------+
;;   | Output, conditional, negative |
;;   +-------------------------------+---------------------------------
;;   | Concatenates the {expressions} into a single string, without an
;;   | adscititious separator, and prints the same to the standard
;;   | output, if the {left} and {right} operands are not equal.
;;   | Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {expressions} must be a sequence of zero or more items, each
;;   | either a string or a variable name, demarcated by a single space
;;   | character. If a string, the value is directly employed;
;;   | variables are substituted by the values.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to tell you something: {expressions}
;;     if {left} and {right} are not similar.
;;   ------------------------------------------------------------------
;;   
;;   +-------------------------+
;;   | Quitting, unconditional |
;;   +-------------------------+---------------------------------------
;;   | Immediately terminates the program.
;;   +-----------------------------------------------------------------
;;   Stu wants to leave now.
;;   ------------------------------------------------------------------
;;   +------------------------------------+
;;   | Quitting, conditional, affirmative |
;;   +------------------------------------+----------------------------
;;   | Immediately terminates the program, if the {left} and {right}
;;   | operands are equal. Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to leave now
;;     if {left} and {right} are similar.
;;   ------------------------------------------------------------------
;;   +---------------------------------+
;;   | Quitting, conditional, negative |
;;   +---------------------------------+-------------------------------
;;   | Immediately terminates the program, if the {left} and {right}
;;   | operands are not equal. Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to leave now
;;     if {left} and {right} are not similar.
;;   ------------------------------------------------------------------
;;   
;;   +------------------------+
;;   | Jumping, unconditional |
;;   +------------------------+----------------------------------------
;;   | Returns to the start of the program.
;;   +-----------------------------------------------------------------
;;   Stu wants to go home now.
;;   ------------------------------------------------------------------
;;   +-----------------------------------+
;;   | Jumping, conditional, affirmative |
;;   +-----------------------------------+-----------------------------
;;   | Returns to the start of the program, if the {left} and {right}
;;   | operands are equal. Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to go home now
;;     if {left} and {right} are similar.
;;   ------------------------------------------------------------------
;;   +--------------------------------+
;;   | Jumping, conditional, negative |
;;   +--------------------------------+--------------------------------
;;   | Returns to the start of the program, if the {left} and {right}
;;   | operands are not equal. Otherwise no effect is imparted.
;;   | ----------------------------------------------------------------
;;   | The {left} and {right} operands may be strings or variable
;;   | names; in the former case the value is directly employed, in the
;;   | latter the variable's value is utilized.
;;   +-----------------------------------------------------------------
;;   Stu wants to go home now
;;     if {left} and {right} are not similar.
;;   ------------------------------------------------------------------
;; 
;; == COMMANDS ARE DOCUMENTED IN A TREE-LIKE HIERARCHY ==
;; The mete of ramosity applicable to the commands and the ensuing
;; nimiety in variations corresponding with the consequences rede an
;; accommodated explication of each instruction and its forms. The mold
;; chosen in this document adheres to a hierarchical schematic
;; presentation whose superimposed entries present the base, extended
;; by the trailing fragments as direct or indirect scions. The ultimity
;; of this approach limns a relationship conforming to the tree
;; structure acquainted to bailiwicks consanguinous in their ordonnance,
;; in that the subtree roots of which are expanded via the dependent
;; branches.
;; 
;; The following diagram applies to a completely fictitious example,
;; unrelated to the actual Stu programming language, describing a
;; sentence repertoire that enumerates the following four members only:
;; 
;;   "abc"
;;   "abcd"
;;   "abcde."
;;   "abc!"
;; 
;; Its schematic illustration following the above established rule thus
;; comprehends:
;; 
;;   "abc"
;;   |
;;   |(abcd...)
;;   |
;;   +--> "d"
;;   |    |
;;   |    |(abcd)
;;   |    |
;;   |    +--> ""
;;   |    |
;;   |    |(abcde.)
;;   |    |
;;   |    +--> "e."
;;   |
;;   |(abc!)
;;   |
;;   +--> "!"
;; 
;; == CONDITIONAL EXECUTION ==
;; All instructions' acceptance embraces the appendage of a conditional
;; execution clause, stated using one of the phrase variants
;; 
;;   " if {left} and {right} are similar."
;;   " if {left} and {right} are not similar."
;; 
;; Where {left} and {right} constitute expressions, that is, either a
;; literal string or a variable identifier. In the former case the value
;; is employed next-ways; whereas the latter case requires the variable
;; value's appropriation for the cause.
;; 
;; The "not" keyword separates the affirmative from the negative case,
;; in consectary defining the test operator.
;; 
;; The extended command solely eventuates if the predicate resolves to
;; a Boolean "true" value, otherwise remains ineffective.
;; 
;;   - In the affirmative case, the predicate is satisfied if the
;;     {left} and {right} operands are equal.
;;   - In the negative case, the predicate's satisfaction depends on the
;;     {left} and {right} operands being unequal.
;; 
;; == INPUT ==
;; A line of input, construed as a string and curtailed of its
;; contingent newline character, is capacitated to be queried from the
;; standard input by means of the following equivalent forms:
;; 
;;   "Stu wants to ask you something and put it in {variable}"
;;   "Stu wants to ask you something, and put it in {variable}"
;;   "Stu wants to know something and put it in {variable}"
;;   "Stu wants to know something, and put it in {variable}"
;; 
;; The {variable} specifies a valid variable identifier, either already
;; begotten into existence before, or to be freshly instantiated. From
;; the former case's causatum ensues a destructive substitution of the
;; extant variable value, whereas the latter initializes and assigns the
;; string object for its storage in a contemporaneous manner.
;; 
;; The desideratum involving an input request in the exlusive condition
;; of the variable's hitherto absence is expressed via the phrase
;; 
;;   " unless he already knows it"
;; 
;; the single space as a sepiment from the preceding fragment
;; inflicting a mandatory constituent. This option's ultimity ascertains
;; that the user input is only issued if the storage with the {variable}
;; name does not yet exist, aliter being adhibited desuetude.
;; 
;; If any of the two input variants, that is, distinguished by the
;; variable's existence, shall be effected in dependence on an equality
;; or inequality test, any of the two conditional phrases
;; 
;;   " if {left} and {right} are similar."
;;   " if {left} and {right} are not similar."
;; 
;; may be appended; for their effects please consult the section
;; "CONDITIONAL EXECUTION" aboon.
;; 
;; A kenspeckle terminating curiosity, the basic form, expressed in the
;; first four designs, must ostend an ecphoneme ("!") as its mark of
;; conclusion:
;; 
;;   "Stu wants to ask you something and put it in {variable}!"
;;   "Stu wants to ask you something, and put it in {variable}!"
;;   "Stu wants to know something and put it in {variable}!"
;;   "Stu wants to know something, and put it in {variable}!"
;; 
;; A diagram as an expression of the manifold possiblities shall educate
;; the reader:
;; 
;;   "Stu wants to ask you something and put it in {variable}"
;;   "Stu wants to ask you something, and put it in {variable}"
;;   "Stu wants to know something and put it in {variable}"
;;   "Stu wants to know something, and put it in {variable}"
;;   |
;;   |(reassignment form)
;;   |
;;   +--> "!"
;;   |
;;   |(initialization form)
;;   |
;;   +--> " unless he already knows it"
;;   |    |
;;   |    |(unconditional initialization form)
;;   |    |
;;   |    +--> "."
;;   |    |
;;   |    |(conditional initialization form)
;;   |    |
;;   |    +--> " if {left} and {right} are similar."
;;   |         " if {left} and {right} are not similar."
;;   |
;;   |(conditional rassignment form)
;;   |
;;   +--> " if {left} and {right} are similar."
;;        " if {left} and {right} are not similar."
;; 
;; == OUTPUT ==
;; The issuing of output to the standard conduit proceeds by aid of the
;; form
;; 
;;   "Stu wants to tell you something: {expressions}"
;; 
;; Where {expressions} constitutes a sequence of zero or more strings
;; or variable names in succession, each twain's intermede pronounced by
;; a single space as the sepiment. In more concise diction, the variadic
;; argument list {expressions} can be resolved into its constituents as
;; 
;;   {expr_1} " " {expr_2} " " ... " " {expr_i} " " ... " " {expr_N}
;; 
;; A string datum is inserted into the final display message verbatim,
;; while a variable identifier by substitution yields its associated
;; value. All such items are concatenated without a separator into a
;; single unity --- a string that is ultimately printed.
;; 
;; A capacitation to endow the instruction's efficacy with a triggering
;; criterion is realized in the appendage of a conditional phrase from
;; the twain
;; 
;;   " if {left} and {right} are similar."
;;   " if {left} and {right} are not similar."
;; 
;; Please note the potential for ambiguities, when "if" serves in both
;; the introduction of this conditional section and the statement of a
;; variable, which ought to be a prior indagation's material. In
;; particular, the definition of a variable whose agnomination coincides
;; with the Stu language keyword "if" encumbers the responsible program
;; with severe perplexity --- a fortiori in conjunction with the
;; remaining conditional phrase keywords' reappropriation.
;; 
;; A dioristic attribute of this command interdicts its adhibition of a
;; a desinent mark, such as a period (".") or ecphoneme ("!") if
;; abstaining from a condition's provision.
;; 
;; The syntactical contingencies applying to the output behest are
;; limned in the diagram below:
;; 
;;   "Stu wants to tell you something:"
;;   |
;;   |(expression form)
;;   |
;;   +--> " ", expression_1, " " , expression_i, " ", expression_N
;;   |    |
;;   |    |(unconditional expression form)
;;   |    |
;;   |    +--> ""
;;   |    |
;;   |    |(conditional expression form)
;;   |    |
;;   |    +--> " if {left} and {right} are similar."
;;   |         " if {left} and {right} are not similar."
;;   |
;;   |(empty conditional form)
;;   |
;;   +--> " if {left} and {right} are similar."
;;        " if {left} and {right} are not similar."
;; 
;; == QUITTING ==
;; A program termination's coercion may be accompassed by mediation of
;; the command
;; 
;;   "Stu wants to leave now"
;; 
;; As with other instructions, a conditional execution may be appended
;; via the suffixes
;; 
;;   " if {left} and {right} are similar."
;;   " if {left} and {right} are not similar."
;; 
;; Otherwise the statement must be concluded with a simple period (".").
;; 
;; The variation tree for the quitting command ostends:
;; 
;;   "Stu wants to leave now"
;;   |
;;   |(unconditional form)
;;   |
;;   +--> "."
;;   |
;;   |(conditional form)
;;   |
;;   +--> " if {left} and {right} are similar."
;;        " if {left} and {right} are not similar."
;; 
;; == JUMPING ==
;; The aefauld control flow mechanism incorporated into the language is
;; exhausted by a basically unconditional returning to the program
;; start, issued using the formula
;; 
;;   "Stu wants to go home now"
;; 
;; The conditional suffixes
;; 
;;   " if {left} and {right} are similar."
;;   " if {left} and {right} are not similar."
;; 
;; may, of course, participate in this command in order to introduce a
;; dependency criterion for the restart.
;; 
;; The condition's lacunae encumbers the syntax with a period (".") as
;; the statement's concluding symbol.
;; 
;; The following diagram exhausts the jump instruction's possibilities:
;; 
;;   "Stu wants to go home now"
;;   |
;;   |(unconditional form)
;;   |
;;   +--> "."
;;   |
;;   |(conditional form)
;;   |
;;   +--> " if {left} and {right} are similar."
;;        " if {left} and {right} are not similar."
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Stu protolog establishing nearly a nonpareil of lucidity, only a
;; scant purview of insecurities inhabits its communication. From these
;; a subset shall be extracted below.
;; 
;; == MAY KEYWORDS BE EMPLOYED AS VARIABLE NAMES? ==
;; The Stu standard apportions to the variable identifiers' diorism the
;; regular expression (regex)
;; 
;;   [A-Za-z0-9]+
;; 
;; A corollary therefrom, a variable's agnomination must embrace one or
;; more Latin letters or decimal digits. At the same time no express
;; proscription nor homologation refers to the appropriation of the
;; language's keywords, such as "Stu" and "if", the same equally subsume
;; into this composition.
;; 
;; A provenance of conundrums, whose dation in tenability is not
;; scarcely invested, ascribes to the "if" word's usance in cases of
;; this ilk, as the output command, introduced by
;; 
;;   Stu wants to tell you something:
;; 
;; accepts an arbitrary account of strings and variable names. The
;; circumstance whose perimeter embraces the latter item species cannot
;; with a certitude assayed as patration distinguish betwixt the two
;; equipollent interpretations of the phrases
;; 
;;   if {left} and {right} are similar
;;   if {left} and {right} are not similar
;; 
;; in their aspect as the conditional facility or a plain variable name
;; sequence; that is, as an example
;; 
;;   Stu want to tell you something: "Hello" if "yes" and "no" are not similar.
;; 
;; may, provided that all tokens among "if", "and", "are", "not", and
;; "similar" are also defined as variables, concomitantly refer to the
;; separate variables or the conditional case.
;; 
;; It has been adjudged that, maugre the contingency for ambiguities'
;; encroachments, Stu language keywords, may be employed as variable
;; identifiers, appropriating a paravaunt echolon in the situtations of
;; agons. As a consectary, the programmer in his own responsibility
;; shall meditate and evaluate the consequences of such dubious
;; definitions. In the particular subject of output arguments and
;; conditional execution, the terminating period assigned to the latter
;; vouches for some mete of differentiating competence.
;; 
;; == ARE SINGULAR SPACES MANDATORY? ==
;; Apart from the imperative requirement that each output argument
;; twain's intermede must be segregated by exactly one space, no express
;; impositions anent the words' demarcation maintain a purview. Merely
;; the examples provide forbisens for the syntactical expectations, yet
;; destitute of veridical mandatoriness.
;; 
;; A patefaction in a tangible weftage, the sentences
;; 
;;   Stu wants to
;; 
;; and
;; 
;;   Stu    wants     to
;; 
;; may in a disjunct degree of liberality in eisegesis either resolve to
;; equality or divergence.
;; 
;; It has been deemed as tenable to assume that all spaces as ostended
;; shall be replicated ipsissima verba. Concretely, any two words'
;; segregation proceeds by means of one and only one space character.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of abstract syntax tree
;; (AST) nodes from tokens.
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
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-06-27
;; 
;; Sources:
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
;;   [esolang2021Stu]
;;   The Esolang contributors, "Stu", 2021
;;   URL: "https://esolangs.org/wiki/Stu"
;;   
;;   [goodrich214datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", sixth edition, 2014,
;;     pages 122--127
;;   Notes:
;;     - Describes the concept and an implementation of the singly
;;       linked list in the Java programming language.
;;     - The pages 276 through 280 describes the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
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
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
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
                (and
                  (typep key   key-type)
                  (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype token-provider ()
  "The ``token-provider'' type defines an entity responsible for the
   delivery of tokens upon request, manifesting in the form of a niladic
   function which, when queried, responds with a ``Token'' object.
   ---
   The function signature amounts to:
     lambda () => Token"
  '(function () Token))

;;; -------------------------------------------------------

(deftype parser-processor ()
  "The ``parser-processor'' type defines the operative unit of a
   ``Parser'' object, responsible for the generation of a response to a
   parsing request, and manifesting as a unary function which, when
   confronted with a ``Parse-State'' as its input, responds with a
   ``Parse-Result''.
   ---
   The signature thus amounts to:
     lambda (Parse-State) => Parse-Result"
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list composed of zero or more
   ``Node'' objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype comparison-operator ()
  "The ``comparison-operator'' type enumerates the recognized predicates
   for testing a conditional expression in the Stu programming language,
   following the patterns \"if <x> and <y> are [not] similar.\""
  '(member :equal :not-equal))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates the information requisite for the
   delination of a significant object extracted during a piece of Stu
   source code's lexical analyzation."
  (type  (error "Missing token type.")  :type keyword)
  (value (error "Missing token value.") :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents an identifier name
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alphanumericp candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Stu source code to analyze.")
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
                    SOURCE, or ``NIL'' if the same is exhausted."))
  (:documentation
    "The ``Lexer'' class' onus designates its lexical analyzation of a
     piece of Stu source code in order to extract and return the
     perceived tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Sets the LEXER's position cursor to the first character in its
   SOURCE, if possible, and returns the modified LEXER."
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
  "Creates and returns a new ``Lexer'' which analyzes the Stu SOURCE
   code."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Returns the LEXER's current character, while concomitantly moving the
   LEXER's position cursor to the next character in its source, if
   possible, updating in the process the current character."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the (or null character)
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source (incf position))))))))

;;; -------------------------------------------------------

(defun lexer-read-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   single word, composed of one or more identifier characters, and
   returns a ``:word'' token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :word
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop
            while (and character (identifier-character-p character))
            do    (write-char (lexer-advance lexer) identifier)))))))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Reads the character at the current position into the LEXER's source,
   returns a TOKEN-TYPE representation thereof with the character as the
   token value, and concomitantly advances the position cursor."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (make-token token-type
      (lexer-advance lexer))))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a comment section which extends to the end of the line or the end of
   the file, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (char/= character #\Newline)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Proceeding from the current position in the LEXER's source, reads a
   string, delimited by double quotation marks, and returns a
   ``:string'' token representation thereof."
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
                (error "Unterminated string at position ~d."
                  (slot-value lexer 'position)))
              (#\Newline
                (error "Linebreak in string at position ~d."
                  (slot-value lexer 'postiion)))
              (#\"
                (lexer-advance lexer)
                (loop-finish))
              (otherwise
                (write-char (lexer-advance lexer) content)))))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon the LEXER source's exhaustion, every request is answered with a
   fresh ``:eof'' (end of file) token."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((char= character #\;)
          (lexer-skip-comment   lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\Space)
          (lexer-read-symbol lexer :space))
        
        ((char= character #\Newline)
          (lexer-read-symbol lexer :newline))
        
        ((char= character #\,)
          (lexer-read-symbol lexer :comma))
        
        ((char= character #\!)
          (lexer-read-symbol lexer :exclamation-mark))
        
        ((char= character #\:)
          (lexer-read-symbol lexer :colon))
        
        ((char= character #\.)
          (lexer-read-symbol lexer :period))
        
        ((char= character #\")
          (lexer-read-string lexer))
        
        ((alphanumericp character)
          (lexer-read-word lexer))
        
        (T
          (lexer-read-symbol lexer :character))))))

;;; -------------------------------------------------------

(defun make-lexer-token-provider (lexer)
  "Creates and returns a function which acts as a ``token-provider'' for
   the LEXER, upon each invocation returning its next token."
  (declare (type Lexer lexer))
  (the function
    #'(lambda ()
        (the Token
          (lexer-get-next-token lexer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "SLNode".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SLNode ()
  ((element
    :initarg       :element
    :initform      NIL
    :accessor      slnode-element
    :type          (or null Token)
    :documentation "A reference to the element stored in this node")
   (next
    :initarg       :next
    :initform      NIL
    :accessor      slnode-next
    :type          (or null SLNode)
    :documentation "A reference to the successor node, or ``NIL'' if
                    this node represents the queue's tail."))
  (:documentation
    "The ``SLNode'' class represent a singly linked node, intended for
     the deployment in a ``Token-Queue''.
     ---
     A paragon of efficiency and simplicity's coefficacy, the singly
     linked node comprehends merely two pieces of information: the
     element to the be stored, which constitutes in our case a ``Token'',
     and a reference to the succeeding node, or ``NIL'' if none such
     exists."))

;;; -------------------------------------------------------

(defun make-slnode (element next)
  "Creates and returns a new ``SLNode'' which stores the ELEMENT, while
   being linked to the optional NEXT node as its successor."
  (declare (type (or null Token)  element))
  (declare (type (or null SLNode) next))
  (the SLNode
    (make-instance 'SLNode
      :element element
      :next    next)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Queue".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Queue ()
  ((head
    :initarg       :head
    :initform      NIL
    :type          (or null SLNode)
    :documentation "The head node.")
   (tail
    :initarg       :tail
    :initform      NIL
    :type          (or null SLNode)
    :documentation "The tail node.")
   (size
    :initarg       :size
    :initform      0
    :reader        token-queue-size
    :type          (integer 0 *)
    :documentation "The number of elements in the queue.
                    ---
                    Please note that the HEAD and TAIL nodes, of course,
                    do contribute to this account."))
  (:documentation
    "The ``Token-Queue'' class implements a positional list stored as a
     singly linked list, intended to maintain ``Token'' objects.
     ---
     Both the head and tail node, for a non-empty queue, constitute
     actual nodes, not sentinels."))

;;; -------------------------------------------------------

(defun make-token-queue ()
  "Creates and returns an empty ``Token-Queue''."
  (the Token-Queue
    (make-instance 'Token-Queue)))

;;; -------------------------------------------------------

(defun token-queue-empty-p (queue)
  "Determines whether the token QUEUE is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Queue queue))
  (the boolean
    (not (null
      (zerop (slot-value queue 'size))))))

;;; -------------------------------------------------------

(defun token-queue-first (queue)
  "Returns the first ``SLNode'' in the QUEUE, or ``NIL'' if the same is
   empty."
  (declare (type Token-Queue queue))
  (the (or null SLNode)
    (unless (token-queue-empty-p queue)
      (slot-value queue 'head))))

;;; -------------------------------------------------------

(defun token-queue-add-last (queue element)
  "Inserts the ELEMENT at the back of the QUEUE and returns the
   ``SLNode'' generated for it."
  (declare (type Token-Queue queue))
  (declare (type Token       element))
  (let ((new-node (make-slnode element NIL)))
    (declare (type SLNode new-node))
    (if (token-queue-empty-p queue)
      (setf (slot-value queue 'head) new-node)
      ;; The NEW-NODE is inserted after the TAIL.
      (setf (slnode-next (slot-value queue 'tail)) new-node))
    ;; The NEW-NODE becomes the TAIL.
    (setf (slot-value queue 'tail) new-node)
    (incf (slot-value queue 'size))
    (the SLNode new-node)))

;;; -------------------------------------------------------

(defun token-queue-after (queue node)
  "Returns the ``SLNode'' immediately succeeding the NODE in the QUEUE,
   or ``NIL'', if the NODE represents the QUEUE's desinent component."
  (declare (type Token-Queue queue))
  (declare (ignore           queue))
  (declare (type SLNode      node))
  (the (or null SLNode)
    (slnode-next node)))

;;; -------------------------------------------------------

(defmethod print-object ((queue Token-Queue) stream)
  (declare (type Token-Queue queue))
  (declare (type destination stream))
  (with-slots (head tail) queue
    (declare (type SLNode head))
    (declare (type SLNode tail))
    (format stream "(Token-Queue")
    (loop
      for node of-type (or null SLNode)
        =    head
        then (slnode-next node)
      while node
      do
        (format stream " ~s"
          (slnode-element node)))
    (format stream ")")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-state-current-element (parse-state)
  (:documentation
    "Returns the token associated with the PARSE-STATE, commorant under
     its personal cursor inside of the shared token queue."))

(defgeneric parse-state-advance (parse-state)
  (:documentation
    "Returns a new ``Parse-State'' as a derivation of the extant
     PARSE-STATE, representing an advance in its source's processing."))

;;; -------------------------------------------------------

(defclass Parse-State ()
  ((tokens
    :initarg       :tokens
    :initform      (error "Missing token queue.")
    :accessor      parse-state-tokens
    :type          Token-Queue
    :documentation "The token queue shared betwixt all parse states, to
                    whom each instance stores a CURSOR, this being a
                    reference to the queue node comprehending the token
                    affiliated with the state.")
   (token-provider
    :initarg       :token-provider
    :initform      (error "Missing token provider.")
    :accessor      parse-state-token-provider
    :type          token-provider
    :documentation "A function shared betwixt all parse states,
                    responsible for producing the tokens to be added at
                    the shared token queue TOKENS' rear.")
   (cursor
    :initarg       :cursor
    :initform      NIL
    :accessor      parse-state-cursor
    :type          (or null SLNode)
    :documentation "A reference to the node in the shared TOKENS queue
                    whose token has been tested with this state."))
  (:documentation
    "The ``Parse-State'' class serves in the encapsulation of the
     parsing process' advancement, as its paravaunt constituent
     maintaining the node in the token queue shared betwixt all parse
     states which contains its personally indagated token."))

;;; -------------------------------------------------------

(defun parse-state-load-next-token (state)
  "Queries from the parse STATE's token provider the next token, inserts
   it in the shared token queue's rear, and returns the STATE."
  (declare (type Parse-State state))
  (with-slots (tokens token-provider) state
    (token-queue-add-last tokens
      (funcall token-provider)))
  (the Parse-State state))

;;; -------------------------------------------------------

(defun parse-state-initialize (state)
  "Inserts at the rear of the STATE's shared token queue the next token
   from its token provider, sets the STATE's cursor to the same, and
   returns the modified STATE."
  (declare (type Parse-State state))
  (parse-state-load-next-token state)
  (setf (parse-state-cursor state)
    (token-queue-first
      (parse-state-tokens state)))
  (the Parse-State state))

;;; -------------------------------------------------------

(defun make-parse-state (tokens token-provider)
  "Creates and returns a new ``Parse-State'' which refers to the shared
   token queue TOKENS and the shared TOKEN-PROVIDER, but contains no
   node cursor into the former yet."
  (the Parse-State
    (make-instance 'Parse-State
      :tokens         tokens
      :token-provider token-provider)))

;;; -------------------------------------------------------

(defun make-initial-parse-state (token-provider)
  "Creates and returns a new ``Parse-State'' which incorporates a fresh
   token queue intended to be shared among all parse states, in
   conjunction with the equally shared TOKEN-PROVIDER, but lacks the
   node cursor into the queue."
  (the Parse-State
    (parse-state-initialize
      (make-instance 'Parse-State
        :tokens         (make-token-queue)
        :token-provider token-provider))))

;;; -------------------------------------------------------

(defmethod parse-state-current-element ((state Parse-State))
  "Returns the parse STATE's token, located in the shared token queue's
   node referenced by this instance."
  (declare (type Parse-State state))
  (the Token
    (slnode-element
      (parse-state-cursor state))))

;;; -------------------------------------------------------

(defmethod parse-state-advance ((state Parse-State))
  "Creates and returns a new ``Parse-State'' based upon the input STATE,
   comprehending a reference to the node in the shared token queue
   immediately succeeding the template STATE's cursor node.
   ---
   In concrete diction, the following steps apply to the process:
     (1) Create a new ``Parse-State'' NEW-STATE, appropriating from the
         template STATE the shared token queue reference and the shared
         token provider. Please note that these are references, not
         copies.
     (2) If the template STATE's own cursor node is succeeded by at
         least one node in the token queue, that is, it does not
         constitute the desinent position, store a reference to the
         immediate successor node in the NEW-STATE.
     (3) If instead the template STATE's cursor node constitutes the
         shared token queue's last node, query the next token from the
         shared token provider, insert it at the token queue's rear, and
         store a reference to the newly created node into the NEW-STATE
         as its cursor."
  (declare (type Parse-State state))
  (let ((new-state
          (make-parse-state
            (parse-state-tokens         state)
            (parse-state-token-provider state))))
    (declare (type Parse-State new-state))
    
    ;; If the STATE's cursor possedes no successor node, query and
    ;; insert a new one to the shared token queue.
    (unless (token-queue-after (parse-state-tokens state)
              (parse-state-cursor state))
      (parse-state-load-next-token state))
    
    ;; Set the NEW-STATE to the node immediately following the template
    ;; STATE's cursor.
    (setf (parse-state-cursor new-state)
          (token-queue-after (parse-state-tokens state)
            (parse-state-cursor state)))
    
    (the Parse-State new-state)))

;;; -------------------------------------------------------

(defmethod print-object ((state Parse-State) stream)
  (declare (type Parse-State state))
  (declare (type destination stream))
  (format stream "(Parse-State cursor=~s)"
    (slot-value state 'cursor)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-result-succeeded-p (parse-result)
  (:documentation
    "Determines whether the ``Parser'' responsible for this
     PARSE-RESULT's generation has matched, returning on confirmation a
     ``boolean'' value of ``T'', otherwise ``NIL''."))

(defgeneric parse-result-state (parse-result)
  (:documentation
    "Returns the ``Parse-State'' allied with this PARSE-RESULT"))

(defgeneric parse-result-output (parse-result)
  (:documentation
    "Returns the output of the ``Parser'' responsible for this
     PARSE-RESULT's generation, usually constituting a contribution to
     the complete parsing process result.
     ---
     The output commonly amounts to ``NIL'' for a failed ``Parser'', for
     which also see the return value of the ``parse-result-succeeded-p''
     operation."))

;;; -------------------------------------------------------

(defclass Parse-Result ()
  ((succeeded-p
    :initarg       :succeeded-p
    :initform      NIL
    :reader        parse-result-succeeded-p
    :type          boolean
    :documentation "Determines whether the parser or combinator having
                    produced this result has matched its input state.")
   (state
    :initarg       :state
    :initform      NIL
    :reader        parse-result-state
    :type          (or null Parse-State)
    :documentation "The parse state affiliated with the result, either
                    constituting the final output or an input into the
                    consequent ``Parser'' invocation.")
   (output
    :initarg       :output
    :initform      NIL
    :reader        parse-result-output
    :type          T
    :documentation "The parsing process' output, usually an abstract
                    syntax tree (AST) node in the case of a successful
                    matching, and ``NIL'' commonly chosen if failed."))
  (:documentation
    "The ``Parse-Result'' encapsulates a ``Parser'' instance's response
     to its invocation, comprehending a success/failure flag, the parse
     state ensuing from the attempt, and an optional output that
     accounts for the parser's contribution to the complete process'
     product, usually in the form of an abstract syntax tree (AST)
     node."))

;;; -------------------------------------------------------

(defun make-parse-result (succeeded-p
                          &optional (state NIL) (output NIL))
  "Creates and returns a new ``Parse-Result'' whose SUCCEEDED-P flag
   determines its success, comprehending an optional parse STATE for
   contingently following parsing operations, and an OUTPUT that
   represents the generating ``Parser'' object's contribution to the
   complete parsing result."
  (declare (type boolean               succeeded-p))
  (declare (type (or null Parse-State) state))
  (declare (type T                     output))
  (the Parse-Result
    (make-instance 'Parse-Result
      :succeeded-p succeeded-p
      :state       state
      :output      output)))

;;; -------------------------------------------------------

(defmethod print-object ((result Parse-Result) stream)
  (declare (type Parse-Result result))
  (declare (type destination  stream))
  (format stream "(Parse-Result succeeded-p=~s, state=~s, output=~s)"
    (slot-value result 'succeeded-p)
    (slot-value result 'state)
    (slot-value result 'output)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ()
  (:documentation
    "The ``Node'' interface represents an abstract syntax tree (AST)
     node."))

;;; -------------------------------------------------------

(defclass NOP-Node (Node)
  ()
  (:documentation
    "The ``NOP-Node'' class represents an abstract syntax tree (AST)
     node encapsulating a no-operation, that is, an ineffective
     operation."))

;;; -------------------------------------------------------

(defclass String-Node (Node)
  ((value
    :initarg       :value
    :initform      (error "Missing string node value.")
    :reader        string-node-value
    :type          string
    :documentation "The string encapsulated in this node."))
  (:documentation
    "The ``String-Node'' class represents an abstract syntax tree (AST)
     node encapsulating the services of a string literal."))

;;; -------------------------------------------------------

(defmethod print-object ((node String-Node) stream)
  (declare (type String-Node node))
  (declare (type destination stream))
  (format stream "(String-Node value=~s)"
    (slot-value node 'value)))

;;; -------------------------------------------------------

(defclass Variable-Node (Node)
  ((name
    :initarg       :name
    :initform      (error "Missing variable node name.")
    :reader        variable-node-name
    :type          string
    :documentation "The reference variable's name."))
  (:documentation
    "The ``Variable-Node'' class represents an abstract syntax tree
     (AST) node encapsulating the services of a variable."))

;;; -------------------------------------------------------

(defmethod print-object ((node Variable-Node) stream)
  (declare (type Variable-Node node))
  (declare (type destination   stream))
  (format stream "(Variable-Node name=~s)"
    (slot-value node 'name)))

;;; -------------------------------------------------------

(defclass Output-Node (Node)
  ((arguments
    :initarg       :arguments
    :initform      NIL
    :reader        output-node-arguments
    :type          node-list
    :documentation "The expressions to concatenate and print."))
  (:documentation
    "The ``Output-Node'' class represents an abstract syntax tree (AST)
     node encapsulating a print operation, composed of one or more
     expressions whose concatenated entirety shall be displayed."))

;;; -------------------------------------------------------

(defmethod print-object ((node Output-Node) stream)
  (declare (type Output-Node node))
  (declare (type destination stream))
  (format stream "(Output-Node arguments=~s)"
    (slot-value node 'arguments)))

;;; -------------------------------------------------------

(defclass Input-Node (Node)
  ((target
    :initarg       :target
    :initform      (error "Missing target.")
    :reader        input-node-target
    :type          Node
    :documentation "The variable intended to store the user input.")
   (requires-unbound-variable-p
    :initarg       :requires-unbound-variable-p
    :initform      (error "Missing unbound variable flag.")
    :reader        input-node-requires-unbound-variable-p
    :type          boolean
    :documentation "Determines whether an input shall only be requested
                    if the TARGET variable is yet unbound."))
  (:documentation
    "The ``Input-Node'' class represents an abstract syntax tree (AST)
     node encapsulating a request for user input, composed of the target
     variable to store the response, as well as a Boolean flag which
     determines whether the request only materializes if the target
     variable is unbound."))

;;; -------------------------------------------------------

(defmethod print-object ((node Input-Node) stream)
  (declare (type Input-Node  node))
  (declare (type destination stream))
  (format stream "(Input-Node target=~s, ~
                              requires-unbound-variable-p=~s)"
    (slot-value node 'target)
    (slot-value node 'requires-unbound-variable-p)))

;;; -------------------------------------------------------

(defclass Quit-Node (Node)
  ()
  (:documentation
    "The ``Quit-Node'' class represents an abstract syntax tree (AST)
     node encapsulating a program termination command."))

;;; -------------------------------------------------------

(defmethod print-object ((node Quit-Node) stream)
  (declare (type Quit-Node   node))
  (declare (type destination stream))
  (format stream "(Quit-Node)"))

;;; -------------------------------------------------------

(defclass Jump-Node (Node)
  ()
  (:documentation
    "The ``Jump-Node'' class represents an abstract syntax tree (AST)
     node encapsulating a jump back to the program start."))

;;; -------------------------------------------------------

(defmethod print-object ((node Jump-Node) stream)
  (declare (type Jump-Node   node))
  (declare (type destination stream))
  (format stream "(Jump-Node)"))

;;; -------------------------------------------------------

(defclass Conditional-Node (Node)
  ((left-operand
    :initarg       :left-operand
    :initform      (error "Missing left operand.")
    :reader        conditional-node-left-operand
    :type          Node
    :documentation "The left predicate operand.")
   (right-operand
    :initarg       :right-operand
    :initform      (error "Missing right operand.")
    :reader        conditional-node-right-operand
    :type          Node
    :documentation "The right predicate operand.")
   (operator
    :initarg       :operator
    :initform      :equal
    :reader        conditional-node-operator
    :type          comparison-operator
    :documentation "The predicate whose satisfaction renders this
                    conditional node active.")
   (statement
    :initarg       :statement
    :initform      (error "Missing conditional node statement.")
    :reader        conditional-node-statement
    :type          Node
    :documentation "The statement to execute if this conditional node's
                    predicate assumes a Boolean true value."))
  (:documentation
    "The ``Input-Node'' class represents an abstract syntax tree (AST)
     node encapsulating a conditional statement, delineated by its
     operand twain, the success predicate, and the body statement to
     execute upon confirmation."))

;;; -------------------------------------------------------

(defmethod print-object ((node Conditional-Node) stream)
  (declare (type Conditional-Node node))
  (declare (type destination      stream))
  (format stream "(Conditional-Node left=~s, right=~s, operator=~s, ~
                                    statement=~s)"
    (slot-value node 'left-operand)
    (slot-value node 'right-operand)
    (slot-value node 'operator)
    (slot-value node 'statement)))

;;; -------------------------------------------------------

(defclass Program-Node (Node)
  ((statements
    :initarg       :statements
    :initform      NIL
    :reader        program-node-statements
    :type          node-list
    :documentation "The statements comprising the Stu program, each such
                    a ``Node'' encapsulation."))
  (:documentation
    "The ``Program-Node'' class represents an abstract syntax tree (AST)
     encapsulating a Stu program compact of zero or more statements,
     each such itself a ``Node'' object.
     ---
     The program node acts as the AST root begotten by the parsing
     stage."))

;;; -------------------------------------------------------

(defmethod print-object ((node Program-Node) stream)
  (declare (type Program-Node node))
  (declare (type destination  stream))
  (format stream "(Program-Node statements=~s)"
    (slot-value node 'statements)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parser-parse (parser parse-state)
  (:documentation
    "Invokes the PARSER on the PARSE-STATE in order to determine its
     eligibility, especially with respect to the currently processed
     token, and returns a ``Parse-Result'' stating the match attempt's
     success or failure."))

;;; -------------------------------------------------------

(defclass Parser ()
  ((processor
    :initarg       :processor
    :initform      (error "Missing processor.")
    :type          parser-processor
    :documentation "The function actual entalented with the
                    responsibility of parsing a ``Parse-State'' in order
                    to return a ``Parse-Result''."))
  (:documentation
    "The ``Parser'' class implements a parser or parser combinator,
     responsible for the evaluation of a ``Parse-State'' in order to
     produce a ``Parse-Result'', the former of which encapsulates the
     token stream, in particular the currently indagated token, the
     latter serves in the description of the success or failure, in
     conjunction with the parse state for a subsequent parser or
     combinator, and the actual output, intended to partake of the
     ultimate parsing process result."))

;;; -------------------------------------------------------

(defun make-parser (processor)
  "Creates and returns a new ``Parser'' whose working is realized by the
   PROCESSOR function.
   ---
   The PROCESSOR constitutes a function which accepts a ``Parse-State'',
   whose token in special shall be probed, and returns a
   ``Parse-Result'' defining the success and potential output. This
   function, as a corollary, conforms to the signature
     lambda (Parse-State) => Parse-Result"
  (declare (type parser-processor processor))
  (the Parser
    (make-instance 'Parser :processor processor)))

;;; -------------------------------------------------------

(defmethod parser-parse ((parser Parser) (state Parse-State))
  "Invokes the PARSER's processor function and applies it to the parse
   STATE, returning the thus produced ``Parse-Result''."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall (slot-value parser 'processor) state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of general parsers and combinators.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun accept-token (predicate)
  "Creates and returns a new ``Parser'' which matches if the PREDICATE
   succeeds, on confirmation returning a successful ``Parse-Result'',
   otherwise a failed instance.
   ---
   The PREDICATE must be a function conformant to the signature
     lambda (current-token) => Parse-Result
   where the CURRENT-TOKEN represents the parse state's token."
  (declare (type (function (Token) *) predicate))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((probed-token (parse-state-current-element state)))
            (declare (type Token probed-token))
            (the Parse-Result
              (if (funcall predicate probed-token)
                (make-parse-result T
                  (parse-state-advance state)
                  probed-token)
                (make-parse-result NIL state NIL))))))))

;;; -------------------------------------------------------

(defun .or (&rest choices)
  "Returns a new ``Parser'' which succeeds if at least one of the
   CHOICES match, returning on confirmation in its ``Parse-Result''
   output the first eligible member's output."
  (declare (type (list-of Parser) choices))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (if choices
              (loop
                for parser
                  of-type Parser
                  in      choices
                for result
                  of-type Parse-Result
                  =       (parser-parse parser state)
                when (parse-result-succeeded-p result) do
                  (return result)
                finally
                  (return
                    (make-parse-result NIL state NIL)))
              (make-parse-result T state NIL)))))))

;;; -------------------------------------------------------

(defun .chain (&rest parsers)
  "Returns a new ``Parser'' which succeeds if all of the PARSERS, in
   this exact order, match, returning on confirmation in its
   ``Parse-Result'' output a list of the collected PARSERS outputs in
   the specified sequence."
  (declare (type (list-of Parser) parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (if parsers
              (loop
                for new-state
                  of-type Parse-State
                  =       state
                  then    (parse-result-state result)
                for parser
                  of-type Parser
                  in      parsers
                for result
                  of-type Parse-Result
                  =       (parser-parse parser new-state)
                unless (parse-result-succeeded-p result) do
                  (return result)
                finally
                  (return result))
              (make-parse-result T state NIL)))))))

;;; -------------------------------------------------------

(defun .bind (antecedent parser-generator)
  "Returns a new ``Parser'' which implements a monadic binding,
   succeeding if both the ANTECEDENT parser matches, in this case
   requesting from the PARSER-GENERATOR, invoked with the ANTECEDENT's
   result state, a consequent parser that also requires to match,
   returning on confirmation the consequent parser's result, otherwise
   the failed ANTECEDENT's one."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) parser-generator))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((antecedent-result (parser-parse antecedent state)))
            (declare (type Parse-Result antecedent-result))
            (the Parse-Result
              (if (parse-result-succeeded-p antecedent-result)
                (let ((antecedent-output
                        (parse-result-output antecedent-result)))
                  (declare (type T antecedent-output))
                  (let ((consequent-parser
                          (funcall parser-generator antecedent-output))
                        (antecedent-state
                          (parse-result-state antecedent-result)))
                    (declare (type Parser      consequent-parser))
                    (declare (type Parse-State antecedent-state))
                    (parser-parse consequent-parser antecedent-state)))
                antecedent-result)))))))

;;; -------------------------------------------------------

(defmacro .let ((output-variable antecedent) &body body)
  "Returns a new ``Parser'' that simplifies the monadic binding,
   invoking the ANTECEDENT parser and, if matching, binding its result
   output to the OUTPUT-VARIABLE, ere evaluating the BODY forms and
   returning the desinent form's result, which is expected to resolve to
   a parser itself."
  `(the Parser
     (.bind ,antecedent
       #'(lambda (,output-variable)
           (declare (type T    ,output-variable))
           (declare (ignorable ,output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun .return (output)
  "Returns a new ``Parser'' which always succeeds, comprehending in its
   ``Parse-Result'' the OUTPUT object as the output."
  (declare (type T output))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (make-parse-result T state output))))))

;;; -------------------------------------------------------

(defun .optional (parser &optional (default NIL))
  "Returns a new ``Parser'' which always succeeds, returning, if the
   PARSER matches, in its ``Parse-Result'' output the PARSER's output,
   otherwise the DEFAULT value."
  (declare (type Parser parser))
  (declare (type T      default))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((probed-result (parser-parse parser state)))
            (declare (type Parse-Result probed-result))
            (the Parse-Result
              (if (parse-result-succeeded-p probed-result)
                probed-result
                (make-parse-result T state default))))))))

;;; -------------------------------------------------------

(defun .many (parser)
  "Returns a new ``Parser'' which always succeeds, returning in its
   ``Parse-Result'' output a contingently empty list of the consecutive
   PARSER occurrences' outputs."
  (declare (type Parser parser))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state result)
              for result
                of-type Parse-Result
                =       (parser-parse parser new-state)
              if (parse-result-succeeded-p result)
                collect (parse-result-output result)
                into    outputs
              else do
                (return (make-parse-result T new-state outputs))
              end
              finally
                (return (make-parse-result T new-state outputs))))))))

;;; -------------------------------------------------------

(defun .all (parsers)
  "Returns a new ``Parser'' which succeeds if all PARSERS match in this
   exact order, returning on confirmation in its ``Parse-Result'' output
   a list of the PARSERS' outputs in accordance with their specified
   order."
  (declare (type (list-of Parser) parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state result)
              for parser
                of-type Parser
                in      parsers
              for result
                of-type Parse-Result
                =       (parser-parse parser new-state)
              if (parse-result-succeeded-p result)
                collect (parse-result-output result)
                into    outputs
              else do
                (return result)
              end
              finally
                (return
                  (make-parse-result T new-state outputs))))))))

;;; -------------------------------------------------------

(defun .one-or-more-separated-by (parser separator)
  "Returns a new ``Parser'' which succeeds if the PARSER matches one or
   more times, with each two occurrences delimited by the SEPARATOR,
   returning on confirmation in its ``Parse-Result'' output a list of
   the collected PARSER outputs in the encounter order."
  (declare (type Parser parser))
  (declare (type Parser separator))
  (the Parser
    (.let (first-output parser)
      (declare (type T first-output))
      (.let (further-outputs (.many (.chain separator parser)))
        (declare (type list further-outputs))
        (.return (cons first-output further-outputs))))))

;;; -------------------------------------------------------

(defun .zero-or-more-separated-by (parser separator)
  "Returns a new ``Parser'' which always succeeds, expecting each two
   occurrences delimited by the SEPARATOR, returning in its
   ``Parse-Result'' output a list of the collected PARSER outputs in the
   encounter order."
  (declare (type Parser parser))
  (declare (type Parser separator))
  (the Parser
    (.optional
      (.one-or-more-separated-by parser separator))))

;;; -------------------------------------------------------

(defun .all-separated-by (parsers separator)
  "Returns a new ``Parser'' which succeeds if all of the PARSERS, in
   this order, match, each twain delimited by the SEPARATOR, returning
   on confirmation in its ``Parse-Result'' output a list of the
   collected PARSERS outputs in accordance with the specified order."
  (declare (type (list-of Parser) parsers))
  (declare (type Parser           separator))
  (the Parser
    (.let (first-output (first parsers))
      (declare (type T first-output))
      (.let (further-outputs
              (.all
                (mapcar
                  #'(lambda (parser)
                      (declare (type Parser parser))
                      (the Parser
                        (.chain separator parser)))
                  (rest parsers))))
        (declare (type (list-of T) further-outputs))
        (.return
          (cons first-output further-outputs))))))

;;; -------------------------------------------------------

(defun .skip-zero-or-more (parser)
  "Returns a new ``Parser'' which always succeeds by skipping zero or
   more consecutive instances of the input PARSER and returning in its
   ``Parse-Result'' output the ``NIL'' value."
  (declare (type Parser parser))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (loop
            for new-state
              of-type Parse-State
              =       state
              then    (parse-result-state result)
            for result
              of-type Parse-Result
              =       (parser-parse parser new-state)
            unless (parse-result-succeeded-p result) do
              (loop-finish)
            finally
              (return (make-parse-result T new-state NIL)))))))

;;; -------------------------------------------------------

(defun .many-until (parser terminator
                    &key (consumes-terminator-p NIL))
  "Returns a new ``Parser'' which succeeds if zero or more consecutive
   instances of the PARSER match, followed by the TERMINATOR, the latter
   of which is probed but not consumed if CONSUMES-TERMINATOR-P does not
   resolve to ``T'', returning on confirmation in its ``Parse-Result''
   output a list of the collected PARSER outputs according to the
   encounter order."
  (declare (type Parser  parser))
  (declare (type Parser  terminator))
  (declare (type boolean consumes-terminator-p))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (loop
            with outputs   of-type (list-of T) = NIL
            with new-state of-type Parse-State = state
            for terminator-result
              of-type Parse-Result
              =       (parser-parse terminator new-state)
            if (parse-result-succeeded-p terminator-result) do
              (return
                (make-parse-result T
                  (if consumes-terminator-p
                    (parse-result-state terminator-result)
                    new-state)
                  (nreverse outputs)))
            else do
              (let ((parser-result (parser-parse parser new-state)))
                (declare (type Parse-Result parser-result))
                (cond
                  ((parse-result-succeeded-p parser-result)
                    (push (parse-result-output parser-result) outputs)
                    (setf new-state (parse-result-state parser-result)))
                  (T
                    (return parser-result))))
            end)))))

;;; -------------------------------------------------------

(defun parse (parser initial-state)
  "Invokes the PARSER on the INITIAL-STATE and returns its
   ``Parse-Result''."
  (declare (type Parser      parser))
  (declare (type Parse-State initial-state))
  (the Parse-Result
    (parser-parse parser initial-state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of specialized parsers.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun .word (expected-word)
  "Returns a new ``Parser'' which succeeds if the current token conforms
   to the ``:word'' type and entails a string equal to the
   EXPECTED-WORD, upon confirmation returning in its ``Parse-Result''
   output the matched token."
  (declare (type string expected-word))
  (the Parser
    (accept-token
      #'(lambda (token)
          (declare (type Token token))
          (and (token-type-p token :word)
               (string= (token-value token) expected-word))))))

;;; -------------------------------------------------------

(defun .type-of (expected-type)
  "Returns a new ``Parser'' which succeeds if its input
   ``Parse-State'''s current token conforms to the EXPECTED-TYPE, on
   confirmation returning in its ``Parse-Result'' output the matched
   token."
  (declare (type keyword expected-type))
  (the Parser
    (accept-token
      #'(lambda (token)
          (declare (type Token token))
          (token-type-p token expected-type)))))

;;; -------------------------------------------------------

(defun .space ()
  "Returns a new ``Parser'' which succeeds if a space token follows,
   returning on confirmation in its ``Parse-Result'' output the matching
   token."
  (the Parser
    (.type-of :space)))

;;; -------------------------------------------------------

(defun .colon ()
  "Returns a new ``Parser'' which succeeds if a colon token follows,
   returning on confirmation in its ``Parse-Result'' output the matching
   token."
  (the Parser
    (.type-of :colon)))

;;; -------------------------------------------------------

(defun .period ()
  "Returns a new ``Parser'' which succeeds if a period token follows,
   returning on confirmation in its ``Parse-Result'' output the matching
   token."
  (the Parser
    (.type-of :period)))

;;; -------------------------------------------------------

(defun .exclamation-mark ()
  "Returns a new ``Parser'' which succeeds if an exclamation mark token
   follows, returning on confirmation in its ``Parse-Result''
   output the matching token."
  (the Parser
    (.type-of :exclamation-mark)))

;;; -------------------------------------------------------

(defun .newline ()
  "Returns a new ``Parser'' which succeeds if a newline token follows,
   returning on confirmation in its ``Parse-Result'' output the matching
   token."
  (the Parser
    (.type-of :newline)))

;;; -------------------------------------------------------

(defun .eof ()
  "Returns a new ``Parser'' which succeeds if an end-of-file (EOF)
   token follows, returning on confirmation in its ``Parse-Result''
   output the matching token."
  (the Parser
    (.type-of :eof)))

;;; -------------------------------------------------------

(defun .any ()
  "Returns a new ``Parser'' which succeeds if no end-of-file (EOF)
   token follows, returning on confirmation in its ``Parse-Result''
   output the matching token."
  (the Parser
    (accept-token
      #'(lambda (token)
          (declare (type Token token))
          (not (token-type-p token :eof))))))

;;; -------------------------------------------------------

(defun .stu-wants-to ()
  "Returns a new ``Parser'' which succeeds if the phrase
   \"Stu want to\", followed by a space, can be ascertained."
  (the Parser
    (.chain
      (.all-separated-by
        (list (.word "Stu") (.word "wants") (.word "to"))
        (.space))
      (.space))))

;;; -------------------------------------------------------

(defun .words (&rest terms)
  "Returns a new ``Parser'' which succeeds if all of its TERMS,
   represented by tokens of the type ``:word'', and separated each by
   one space, match, comprehending in its ``Parse-Result'' output a list
   of the matching tokens."
  (declare (type (list-of string) terms))
  (the Parser
    (.chain
      (.all-separated-by
        (mapcar #'.word terms)
        (.space)))))

;;; -------------------------------------------------------

(defun .string ()
  "Returns a new ``Parser'' which succeeds if a string follows,
   comprehending in its result a fresh ``String-Node'' representation of
   the matched token's value."
  (the Parser
    (.let (string-token (.type-of :string))
      (declare (type Token string-token))
      (.return
        (make-instance 'String-Node :value
          (token-value string-token))))))

;;; -------------------------------------------------------

(defun .variable ()
  "Returns a new ``Parser'' which succeeds if a non-keyword identifier
   follows, construed as a variable name, comprehending in its results a
   fresh ``Variable-Node'' representation of the matched token's value."
  (the Parser
    (.let (variable-token (.type-of :word))
      (declare (type Token variable-token))
      (.return
        (make-instance 'Variable-Node :name
          (token-value variable-token))))))

;;; -------------------------------------------------------

(defun .expression ()
  "Returns a new ``Parser'' which succeeds if an expression, either a
   string or a variable identifier, follows, on confirmation returning
   in its ``Parse-Result'' output the detected node representation."
  (the Parser
    (.or
      (.variable)
      (.string))))

;;; -------------------------------------------------------

(defun .optional-not ()
  "Returns a new ``Parser'' which always succeeds, either returning in
   its ``Parse-Result'' output the value ``:not-equal'', if the word
   \"not\" followed by a space could be detected, otherwise the object
   ``:equal''."
  (the Parser
    (.optional
      (.chain
        (.word "not")
        (.space)
        (.return :not-equal))
      :equal)))

;;; -------------------------------------------------------

(defun .conditional (statement-node)
  "Returns a new ``Parser'' which succeeds if a conditional fragment
   follows, on confirmation returning in its ``Parse-Result'' output a
   ``Conditional-Node'' that references the STATEMENT-NODE."
  (declare (type Node statement-node))
  (the Parser
    (.chain
      (.space)
      (.word "if")
      (.space)
      (.let (left-operand (.expression))
        (declare (type Node left-operand))
        (.chain
          (.space)
          (.word "and")
          (.space)
          (.let (right-operand (.expression))
            (declare (type Node right-operand))
            (.chain
              (.space)
              (.word "are")
              (.space)
              (.let (not-keyword (.optional-not))
                (declare (type comparison-operator not-keyword))
                (.chain
                  (.word "similar")
                  (.period)
                  (.return
                    (make-instance 'Conditional-Node
                      :left-operand  left-operand
                      :right-operand right-operand
                      :operator      not-keyword
                      :statement     statement-node)))))))))))

;;; -------------------------------------------------------

(defun .probe-if-not (parser)
  "Returns a new ``Parser'' which succeeds if the PARSER does not match,
   returning in any case in its ``Parse-Result'' output the PARSER's
   output, always in conjunction with the the new parser's input state,
   that is, the parse state does not advance."
  (declare (type Parser parser))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((parser-result (parser-parse parser state)))
            (declare (type Parse-Result parser-result))
            (the Parse-Result
              (if (parse-result-succeeded-p parser-result)
                (make-parse-result NIL state
                  (parse-result-output parser-result))
                (make-parse-result T state
                  (parse-result-output parser-result)))))))))

;;; -------------------------------------------------------

(defun .no-conditional ()
  "Returns a new ``Parser'' which succeeds if no conditional fragment
   follows, returning on confirmation in its ``Parse-Result'' output the
   concluding period token, in any case not advancing the parse state."
  (the Parser
    (.probe-if-not
      (.chain
        (.space)
        (.word "if")
        (.space)
        (.let (left-operand (.expression))
          (declare (type Node left-operand))
          (.chain
            (.space)
            (.word "and")
            (.space)
            (.let (right-operand (.expression))
              (declare (type Node right-operand))
              (.chain
                (.space)
                (.word "are")
                (.space)
                (.let (not-keyword (.optional-not))
                  (declare (type comparison-operator not-keyword))
                  (.chain
                    (.word "similar")
                    (.period)))))))))))

;;; -------------------------------------------------------

(defun .output-expressions ()
  "Returns a new ``Parser'' which succeeds if zero or more string or
   variable expressions, each differentiated from the next by a single
   space, follow, returning in its ``Parse-Result'' output a list of
   these items' node representations."
  (the Parser
    (.or
      (.many
        (.chain
          (.no-conditional)
          (.space)
          (.expression))))))

;;; -------------------------------------------------------

(defun .output ()
  "Returns a new ``Parser'' which succeeds if an output statement,
   represented by its fragmentary introduction \"tell you something\",
   on confirmation returning in its ``Parse-Result'' output an
   ``Output-Node''."
  (the Parser
    (.chain
      (.words "tell" "you" "something")
      (.colon)
      (.let (arguments (.output-expressions))
        (declare (type node-list arguments))
        (let ((output-node
                (make-instance 'Output-Node :arguments arguments)))
          (declare (type Output-Node output-node))
          (.or
            (.conditional output-node)
            (.return output-node)))))))

;;; -------------------------------------------------------

(defun .input-variable-check ()
  "Returns a new ``Parser'' which always succeeds, testing whether the
   sentinel for an input command's conditional assignment can be found,
   returning in its ``Parse-Result'' output, if confronted with the
   phrase \"unless he already knows it\", a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the Parser
    (.optional
      (.chain
        (.space)
        (.words "unless" "he" "already" "knows" "it")
        (.return T))
      NIL)))

;;; -------------------------------------------------------

(defun .input ()
  "Returns a new ``Parser'' which succeeds if an input command follows,
   returning on confirmation in its ``Parse-Result'' output an
   ``Input-Node'' representation."
  (the Parser
    (.chain
      (.or (.words "know" "something")
           (.words "ask" "you" "something"))
      (.optional (.type-of :comma))
      (.space)
      (.words "and" "put" "it" "in")
      (.space)
      (.let (variable (.variable))
        (declare (type Node variable))
        (.or
          (.chain
            (.exclamation-mark)
            (.return
              (make-instance 'Input-Node
                :target                      variable
                :requires-unbound-variable-p NIL)))
          (.let (requires-unbound-variable-p
                  (.input-variable-check))
            (declare (type boolean requires-unbound-variable-p))
            (let ((input-node
                    (make-instance 'Input-Node
                      :target variable
                      :requires-unbound-variable-p
                        requires-unbound-variable-p)))
              (declare (type Input-Node input-node))
              (.or
                (.conditional input-node)
                (.chain
                  (.period)
                  (.return input-node))))))))))

;;; -------------------------------------------------------

(defun .quit ()
  "Returns a new ``Parser'' which succeeds if a quit command follows,
   returning on confirmation in its ``Parse-Result'' output a
   ``Quit-Node'' representation."
  (the Parser
    (.chain
      (.words "leave" "now")
      (let ((quit-node (make-instance 'Quit-Node)))
        (declare (type Quit-Node quit-node))
        (.or
          (.conditional quit-node)
          (.chain
            (.period)
            (.return quit-node)))))))

;;; -------------------------------------------------------

(defun .jump ()
  "Returns a new ``Parser'' which succeeds if a jump command follows,
   returning on confirmation in its ``Parse-Result'' output a
   ``Jump-Node'' representation."
  (the Parser
    (.chain
      (.words "go" "home" "now")
      (let ((jump-node (make-instance 'Jump-Node)))
        (declare (type Jump-Node jump-node))
        (.or
          (.conditional jump-node)
          (.chain
            (.period)
            (.return jump-node)))))))

;;; -------------------------------------------------------

(defun .nop ()
  "Returns a new ``Parser'' which matches a no-operation (NOP) by
   succeeding if zero or more tokens are encountered, followed by a
   newline or end-of-file (EOF) token, and returns a ``NOP-Node''
   representation of the parsed operation."
  (the Parser
    (.chain
      (.many-until
        (.any)
        (.or (.newline)
             (.eof)))
      (.return
        (make-instance 'NOP-Node)))))

;;; -------------------------------------------------------

(defun .command ()
  "Returns a new ``Parser'' which succeeds if a command follows,
   returning on confirmation in its ``Parse-Result'' output a node
   representation thereof."
  (the Parser
    (.or
      (.input)
      (.output)
      (.quit)
      (.jump))))

;;; -------------------------------------------------------

(defun .optional-spaces ()
  "Returns a new ``Parser'' which always succeeds by skiipping zero or
   more adjacent space tokens, returning in its ``Parse-Result'' output
   the ``NIL'' value."
  (the Parser
    (.skip-zero-or-more
      (.space))))

;;; -------------------------------------------------------

(defun .statement ()
  "Returns a new ``Parser'' which always succeeds, either returning
   in its ``Parse-Result'' output a recognized Stu command's node
   representation, or responding with a ``NOP-Node''."
  (the Parser
    (.or
      (.chain
        (.optional-spaces)
        (.stu-wants-to)
        (.command))
      (.nop))))

;;; -------------------------------------------------------

(defun .end-of-line ()
  "Returns a new ``Parser'' which succeeds if a sequence of zero or more
   spaces follows, concluded by a mandatory newline token, on
   confirmation returning in its ``Parse-Result'' output the latter
   datum."
  (the Parser
    (.chain
      (.optional-spaces)
      (.newline))))

;;; -------------------------------------------------------

(defun .statement-list ()
  "Returns a new ``Parser'' which always succeeds, returning in its
   ``Parse-Result'' output a potentially empty list of statement nodes."
  (the Parser
    (.zero-or-more-separated-by
      (.statement)
      (.end-of-line))))

;;; -------------------------------------------------------

(defun .program ()
  "Returns a new ``Parser'' which succeeds if a valid Stu program has
   been parsed, returning in its ``Parse-Result'' the root
   ``Program-Node'' of the Stu program's abstract syntax tree (AST)
   representation."
  (.let (statements (.statement-list))
    (declare (type node-list statements))
    (.chain
      (.optional-spaces)
      (.eof)
      (.return
        (make-instance 'Program-Node :statements statements)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Stu-Condition (Condition)
  ()
  (:documentation
    "The ``Stu-Condition'' serves as an abstract common base for all
     conditions allied with a Stu program's execution."))

;;; -------------------------------------------------------

(define-condition Quit-Condition (Stu-Condition)
  ()
  (:documentation
    "The ``Quit-Condition'' serves to signal the desire to immediately
     terminate the program execution."))

;;; -------------------------------------------------------

(define-condition Jump-Condition (Stu-Condition)
  ()
  (:documentation
    "The ``Jump-Condition'' serves to signal the desire to return to
     the start of the program, which usually constitutes an iterum
     processing of the same's abstract syntax tree (AST) representation
     incepting with the root node."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of comparison operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-comparison-operator-predicate (comparison-operator)
  "Returns a string comparison predicate that ostends conformance to the
   COMPARISON-OPERATOR's specification.
   ---
   The thus returned function accepts two strings as its inputs,
   responding with a generalized Boolean of \"true\" in the case of
   their equality, otherwise with ``NIL''. In corollary, it adheres to
   the following signature:
     lambda (first-string second-string) => generalized-boolean"
  (declare (type comparison-operator comparison-operator))
  (the function
    (case comparison-operator
      (:equal     #'string=)
      (:not-equal #'string/=)
      (otherwise
        (error "Invalid comparison operator: ~s."
          comparison-operator)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree (AST).")
    :type          Node
    :documentation "An abstract syntax tree (AST) representation of the
                    parsed Stu program.")
   (variables
    :initarg       :variables
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string string)
    :documentation "Associates the declared variable names with their
                    values."))
  (:documentation
    "The ``Interpreter'' class applies itself to accompassing the
     processing of a Stu program's abstract syntax tree (AST)
     representation in pursuit of imbuing the same with actual
     effect."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a new ``Interpreter'' capacitated to process the
   abstract syntax TREE (AST)."
  (declare (type Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defun interpreter-variable-exists-p (interpreter name)
  "Determines whether a variable with the NAME is registered at the
   INTERPRETER, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash name
          (slot-value interpreter 'variables)))))))

;;; -------------------------------------------------------

(defun interpreter-variable-value (interpreter name)
  "Returns the value of the variable registered with the NAME at the
   INTERPRETER, or signals an error of an unspecified type upon its
   absence."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the string
    (or (gethash name (slot-value interpreter 'variables))
        (error "Unrecognized variable name: ~s." name))))

;;; -------------------------------------------------------

(defun (setf interpreter-variable-value) (new-value interpreter name)
  "Registers a variable with the NAME at the INTERPRETER, associating
   the same with the NEW-VALUE, if none such exists, otherwise
   overwriting the extant variable's content with the new datum, and in
   any case returns no value."
  (declare (type string      new-value))
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (setf (gethash name (slot-value interpreter 'variables))
        new-value)
  (values))

;;; -------------------------------------------------------

(defgeneric interpreter-visit-node (interpreter node)
  (:documentation
    "Processes the NODE in the INTERPRETER's context and returns a value
     appropriate for this combination."))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Program-Node))
  "Processes the program NODE's statements in their specified order in
   the INTERPRETER's context, concomitantly heeding contingent quit and
   jump conditions, which may relocate the control flow, finally returns
   no value."
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (handler-case
    (dolist (statement (program-node-statements node))
      (declare (type Node statement))
      (interpreter-visit-node interpreter statement))
    ;; Terminate the program.
    (Quit-Condition ()
      NIL)
    ;; Repeat the program from its root node.
    (Jump-Condition ()
      (interpreter-visit-node interpreter node))
    ;; Propagate any other error.
    (error (e)
      (error e)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Conditional-Node))
  "Processes the conditional NODE in the INTERPRETER's context,
   executing the embraced statement only if the condition predicate is
   satisfied, in any case returning no value."
  (declare (type Interpreter      interpreter))
  (declare (type Conditional-Node node))
  (let ((left-operand  (conditional-node-left-operand  node))
        (right-operand (conditional-node-right-operand node))
        (operator      (conditional-node-operator      node)))
    (declare (type Node                left-operand))
    (declare (type Node                right-operand))
    (declare (type comparison-operator operator))
    (when (funcall
            (get-comparison-operator-predicate  operator)
            (interpreter-visit-node interpreter left-operand)
            (interpreter-visit-node interpreter right-operand))
      (interpreter-visit-node interpreter
        (conditional-node-statement node))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Input-Node))
  "Processes the input NODE in the INTERPRETER's context, querying the
   user for a line of text which is subsequently stored in the specified
   variable, depending upon the requisitum of an unbounded speciment,
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Input-Node  node))
  (let ((target
          (input-node-target node))
        (requires-unbound-variable-p
          (input-node-requires-unbound-variable-p node)))
    (declare (type Variable-Node target))
    (declare (type boolean       requires-unbound-variable-p))
    (when (or (not requires-unbound-variable-p)
              (and requires-unbound-variable-p
                   (not (interpreter-variable-exists-p interpreter
                          (variable-node-name target)))))
      (format T "~&>> ")
      (setf (interpreter-variable-value interpreter
              (variable-node-name target))
            (read-line))
      (clear-input)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Jump-Node))
  "Processes the jump NODE in the INTERPRETER's context, signaling a
   ``Jump-Condition'' in order to communicate to the INTERPRETER's
   condition handler the desire to return to the program's inchoation,
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Jump-Node   node))
  (declare (ignore           node))
  (signal 'Jump-Condition)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Output-Node))
  (declare (type Interpreter interpreter))
  (declare (type Output-Node node))
  (let ((arguments (output-node-arguments node)))
    (declare (type node-list arguments))
    (dolist (argument arguments)
      (declare (type Node argument))
      (format T "~a"
        (interpreter-visit-node interpreter argument))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Quit-Node))
  "Processes the quit NODE in the INTERPRETER's context, signaling a
   ``Quit-Condition'' in order to communicate to the INTERPRETER's
   condition handler the desire to return to halt the program, and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Quit-Node   node))
  (declare (ignore           node))
  (signal 'Quit-Condition)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        String-Node))
  "Processes the string NODE in the INTERPRETER's context, returning the
   incorporated string value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type String-Node node))
  (the string
    (string-node-value node)))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Variable-Node))
  "Processes the variable NODE in the INTERPRETER's context, returning
   the string value associated with the embraced variable name, or
   signals an error of an unspecified type upon its absence."
  (declare (type Interpreter   interpreter))
  (declare (type Variable-Node node))
  (let ((name (variable-node-name node)))
    (declare (type string name))
    (the string
      (interpreter-variable-value interpreter name))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        NOP-Node))
  "Processes the no-operation (NOP) NODE in the INTERPRETER's context,
   effectively accompassing no consequences, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type NOP-Node    node))
  (declare (ignore           node))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Processes the abstract syntax tree (AST) maintained by the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-Stu (code)
  "Interprets the piece of Stu source CODE and returns no value."
  (declare (type string code))
  (let ((result
          (parse
            (.program)
            (make-initial-parse-state
              (make-lexer-token-provider
                (make-lexer code))))))
    (declare (type Parse-Result result))
    (if (parse-result-succeeded-p result)
      (interpreter-interpret
        (make-interpreter
          (parse-result-output result)))
      (error "The parsing has failed.")))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!"
(interpret-Stu "Stu wants to tell you something: \"Hello, World!\"")

;;; -------------------------------------------------------

;; Print "Hello, World!", demonstrating the intricate possibilities
;; appertaining conditional output and keywords as variable names.
(interpret-Stu
  "Stu wants to know something and put it in if!
   Stu wants to know something and put it in and!
   Stu wants to know something and put it in are!
   Stu wants to tell you something: \"Hello, World!\" if if and and are not similar.")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-Stu "Stu wants to go home now.")

;;; -------------------------------------------------------

;; Infinitely print the text "Ha".
(interpret-Stu "Stu wants to tell you something: \"Ha\"
                Stu wants to go home now.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on an empty input.
(interpret-Stu
  "Stu wants to know something and put it in input!
   Stu wants to tell you something: input
   Stu wants to go home now if input and \"\" are not similar.")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Stu
  "Stu wants to know something and put it in input unless he already knows it.
   Stu wants to tell you something: input
   Stu wants to go home now if input and \"0\" are not similar.")
