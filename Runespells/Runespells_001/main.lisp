;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Runespells", invented by the Esolang user "TehZ", and
;; composed of peculiar words acting in the mimicry of runes.
;; 
;; Concepts
;; ========
;; The Runespells programming language represents programs by a
;; dioristic syntax alluding to the recondite diction commonly
;; affiliated with runes. Procedures, known as "runes" and "spells"
;; contribute to the control portion, while the data compartment relies
;; on a stack of these code blocks.
;; 
;; == RUNESPELLS PROGRAMS ASSUME AN ARCANE LANGUAGE ==
;; The paravaunt commorancy of the language's diorism ascribes to the
;; deployed terms, vectors of the arcane dictionary whose intention
;; is airted towards a certain mysterious ambient's establishment.
;; Beside the nomenclature, a system of pronunciations has been
;; committed in the context of the specification, which shall now be
;; enumerated in a tabular guise:
;;   
;;   Term    | Pronunciation          | Role
;;   --------+------------------------+---------
;;   Fa      | FAH                    | Variable
;;   Gora    | gõ-ra                  | Variable
;;   Jyiku   | schiKU                 | Variable
;;   Nahy    | na-HY                  | Variable
;;   Rin     | rin                    | Variable
;;   Zeha    | zeHA                   | Variable
;;   ...........................................
;;   Chixo   | CHIK-so                | Command
;;   Chiyo   | CHI-yo                 | Command
;;   Chizo   | CHI-so-o               | Command
;;   Giyah   | gi-AH                  | Command
;;   Gorpdne | GORP-NEH               | Command
;;   Lafi    | LA-fi                  | Command
;;   Mizi    | mi-SI                  | Command
;;   Mizo    | mi-SO                  | Command
;;   Nahweh  | NAH-WEHY               | Command
;;   Ne      | neh                    | Command
;;   Ni      | n-IH                   | Command
;;   Tazi    | TAH-ZI                 | Command
;;   Yah     | jaar, Yah as in Yahweh | Command
;;   Zi      | si                     | Command
;; 
;; == RUNES REPRESENT PROCEDURES ==
;; Runes basically delineate procedures, their potencies encompass an
;; association with zero to six arguments, known as "variables", and
;; their body's definition embraces zero or more commands. Two variants
;; of such procedural entities exist: simple runes, introduced
;; by the "Rune" keyword, and spells, their statement accords to the
;; eponymous "Spell" reservation.
;; 
;; == THE SPELL DEFINES THE PROGRAM START ==
;; Simple runes, bearing a unique identifier, do not rise aboon a
;; functional unit's interpretation; a spell, on the other hand,
;; constitutes an entry point into a program, tantamout to the "main"
;; function in the programming languages C and Java. Its invocation
;; goads the control flow; its absence retires the code without
;; response. Duplication in the form of more than one spell renders the
;; program malformed and thus erroneous.
;; 
;; == SIMPLE RUNES ARE DESCRIBED BY AN ID, VARIABLES, AND COMMANDS ==
;; For a simple rune, the following signature holds:
;;   
;;   Rune <runeID> [ <variables> ]: <commands>
;; 
;; Its constituents enumerate as follows:
;;   
;;   <runeID>
;;     A positive integer number that may be chosen by the programmer
;;     at his own liberty, but must be unique among all runes. It most
;;     closely enters into a correspondence with a procedure name in
;;     traditional programming languages.
;;   
;;   <variables>
;;     A list of zero to six positive integer values which designate
;;     rune IDs to be induced as additional information in the form of
;;     the actual parameters. A rune's parameter list subscribes to a
;;     positional species, the order and nomenclature of the formal
;;     parameters being implicitly defined in this exact order:
;;     
;;       (1) Fa
;;       (2) Rin
;;       (3) Gora
;;       (4) Jyiku
;;       (5) Nahy
;;       (6) Zeha
;;     
;;     Each variable value v[i], with the index i enclosed in ther range
;;     [1, 6], is associated with the respective i-th variable name as
;;     enumerated aboon. Unspecified variable correspondences, resulting
;;     from a parameter list with less than the maximum of six members,
;;     cannot be accessed by the rune. Supernumerary variable values are
;;     proscribed.
;;   
;;   <commands>
;;     A sequence of zero or more instructions, in their coefficiency
;;     composing the rune's effect. A dichotomy's exercise permits the
;;     commands' subsumption into:
;;     
;;       (a) True commands:
;;           These encompass the 14 actual instructions offered by the
;;           Runespells language, enlisting
;;             - Chixo
;;             - Chiyo
;;             - Chizo
;;             - Giyah
;;             - Gorpdne
;;             - Lafi
;;             - Mizi
;;             - Mizo
;;             - Nahweh
;;             - Ne
;;             - Ni
;;             - Tazi
;;             - Yah
;;             - Zi
;;           For a complete treatise on these language facilities,
;;           please consult the "Instructions" section.
;;       
;;       (b) Variable invocations:
;;           The statement of one of the six possible variables
;;             - Fa
;;             - Rin
;;             - Gora
;;             - Jyiku
;;             - Nahy
;;             - Zeha
;;           pushes the rune with the represented ID unto the rune
;;           stack. Please consult for this also the "Instructions"
;;           section.
;; 
;; == SPELLS ARE DEFINED BY VARIABLES AND COMMANDS ==
;; A spell approximates the simple rune design, however by its
;; singularity in existence dissociated from an identifying datum:
;;   
;;   Spell [ <variables> ]: <commands>
;; 
;; The components <variables> and <commands> assume the exact same
;; construe as the simple "Rune"  counterpart, which please see.
;; 
;; == DATA IS STORED IN A STACK OF RUNES ==
;; A global stack dedicated to the persistence of runes --- excluding,
;; however, the spell --- is invested into a program's data section.
;; Unbounded in its capacity, several facilities exist to manipulate
;; the content.
;; 
;; == SPELL AND STACK REALIZE A PROGRAM FLOW'S DUCTION ==
;; The deployment of the spell as the entry point, whose linear
;; arrangement prescribes the commands' order of execution, supplies one
;; moeity of the program flow, whose champarty is subject to patration
;; in the engagement of the rune stack, several instructions' target,
;; including such that perform a selected member.
;; 
;; 
;; Architecture
;; ============
;; Runespells programs are assembled from procedures, known as "runes",
;; a designated specimen of which, the "spell", provides the exclusive
;; adit to their execution. Each rune or spell consists of a list of
;; arguments, known as "variables", and a composition of commands
;; forming its body, a subset of the same accesses a global stack of
;; runes to accomplish an effect.
;; 
;; == VARIABLES: ARGUMENTS TO THE RUNES ==
;; For every rune or spell a sequence of zero to six positive integers
;; may be defined, interpreted as the identifiers of other runes to
;; reference in the defining context's body. These positional arguments
;; are known as "variables" in the Runespells language, associated with
;; prescribed identifications, namely:
;; 
;;   (1) Fa
;;   (2) Rin
;;   (3) Gora
;;   (4) Jyiku
;;   (5) Nahy
;;   (6) Zeha
;; 
;; A consectary thereof, a rune or spell possesses a local variable
;; registry, affiliating with a variable name, in the case of its
;; definition, a positive integer value. Unassigned names are exempted
;; from inquiries. Most commonly, such a relation will be molded into an
;; associative data structure, with the paravaunt choice being the map
;; or dictionary.
;; 
;; == THE RUNE STACK: AN AUGMENTED LAST-IN FIRST-OUT COLLECTION ==
;; The rune stack exercise its significance as the only global memory,
;; instituted as a salvatory for runes, but not spells. Its capacity is
;; unbounded, and operations exist for its access as well as its
;; manipulation.
;; 
;; A derivation of the general stack data structure, the foundational
;; operations are naturally consolidated in this language-specific
;; variant:
;;   
;;   Stack operation | Effect
;;   ----------------+-------------------------------------------------
;;   push (x)        | Pushes the element {x} unto the top.
;;   ..................................................................
;;   peek ()         | Returns without removing the topmost element.
;;   ..................................................................
;;   pop  ()         | Removes and returns the topmost element.
;; 
;; An adulerating influence, several enhancements form impositions from
;; the Runespells command set, thus extending the stock-father's
;; haecceity. The interface of the stack rune, logically, involves a
;; more convolute and demanding design:
;;   
;;   Rune stack operation | Effect
;;   ---------------------+--------------------------------------------
;;   push (rune)          | Pushes the RUNE unto the stack.
;;   ..................................................................
;;   peek ()              | Returns without removing the top rune on
;;                        | the stack.
;;   ..................................................................
;;   peekSecond ()        | Returns without removing the rune
;;                        | immediately below the top rune on the
;;                        | stack.
;;   ..................................................................
;;   peekThird ()         | Returns without removing the rune two
;;                        | positions below the top rune on the stack.
;;   ..................................................................
;;   pop ()               | Removes and returns the top rune from the
;;                        | stack.
;;   ..................................................................
;;   removeThird ()       | Removes and returns the rune two positions
;;                        | below the top rune from the stack.
;; 
;; 
;; Data Types
;; ==========
;; Runespells basically relies on three distinct data types: integers,
;; acting as rune IDs and command counters, ASCII characters for the
;; human-machine interaction, and the rune stack for operations on the
;; interface betwixt the various commands.
;; 
;; == UNSIGNED INTEGERS RELATE TO RUNE IDS AND COMMAND COUNTS ==
;; Runespells's singular numeric type resolves to unsigned, non-negative
;; integer numbers whose involvement signifies either the identifier of
;; a rune or its number of commands.
;; 
;; The incipient usage, directed at positive integers with an exclusion
;; of the zero value, partakes in the context of a rune or spell's
;; variable list as well as in the argument to the "Giyah" command. The
;; first occasion relates to the exclusive statement of literal
;; integers, intended to be mapped to the up to six available variable
;; names. These entities themselves, as a corollary, incorporate a
;; number datum without its immediate mentioning. The "Giyah" command's
;; dependence upon an argument in the form of such a placeholder
;; exemplifies the preponderance in mediated numeric specifications.
;; 
;; A particular and prominent role's assignment stresses a rune's code
;; department, as the second variant by which integers, non-negative in
;; their gamut, intrude into a program is realized in the tally of a
;; rune's commands. A triple of instruction, namely "Giyah", "Mizo" and
;; "Nahweh", appertain to the perquisition into this attribute.
;; 
;; == CHARACTERS OPERATE ON THE INPUT/OUTPUT CONDUITS ==
;; The deviation from the essential integer type manifests itself solely
;; along the interface betwixt the user and a program, ensuing from the
;; transmission along the input/output conduits.
;; 
;; The "Mizi" command requests an ASCII character from the user, the
;; numeric representation thereof avails in the determination of the
;; number of no-ops to embed in a freshly to build rune, which is then
;; pushed unto the respective stack.
;; 
;; In an analogous manner, the "Mizo" command indagates the tally of
;; commands harbored by the topmost rune on the stack, converts this
;; numeric item into an ASCII character, whose printing on the standard
;; output ultimately ensues.
;; 
;; In no other agency nor occasion do characters register their utility
;; inside of a program.
;; 
;; == THE RUNE STACK: THE PROGRAM'S MEMORY ==
;; A stack of runes, globally steadable to all entities engaged during
;; runtime, and amenable to inquests and manipulations by runes and
;; spells, memorizes the procedures in currency during the various
;; operations.
;; 
;; This stack, a last-in first-out (LIFO) storage of unbounded capacity,
;; must homologate, besides those capabilities traditionally expected
;; from the conceptual data structure, the query anenst the second-top
;; and third-top element, as well as the latter's removal.
;; 
;; 
;; Syntax
;; ======
;; Along its physical dimension, a Runespells program is constructed
;; linewise, with a non-empty horizontal extent construed as a rune or
;; spell definition, any of which must be stated on a single line of its
;; own. Vacant rows fail to invest a conducive causatum and thus are
;; ignored.
;; 
;; == A RUNE DEFINITION OCCUPIES A LINE OF ITS OWN ==
;; A rune definition's introduction mentions in its inchoation a type,
;; either "Rune" for a simple exemplary or "Spell" for the main rune.
;; The "Rune" keyword is separated by at least one space from its
;; positive integer identifier, followed either directly or via the same
;; sepiment's adminiculum by a pair of brackets "[" and "]" containing
;; the zero to six positive integers acting as variable values for the
;; referenced rune IDs. A colon succeeds the argument list, potentially
;; stressed by spaces, and itself trailed by the command body. Whereas
;; the incipient command does not require a segregation from the colon,
;; each two tokens inside of the instruction list must be distinguished
;; through one or more instances of such. The rune definition concludes
;; with a newline character or the end of the file (EOF).
;; 
;; Except for the missing identifier, a "Spell" partakes of the exact
;; same signature and body as its thus delineated counterpart.
;; 
;; == LINEBREAKS ==
;; Linebreaks impose a requisite for a rune or spell definition's
;; conclusion, provided that the same does not immediately precede the
;; source code's desinence.
;; 
;; Empty lines, including both such without any content as well as
;; compositions of spaces only, are encountered with tolerance.
;; 
;; == SPACES ==
;; The interspersion of spaces, a definition embracing the space
;; character and the horizontal tab, shall be, apart from the mandated
;; single occurrence betwixt tokens, an act guided by the programmer's
;; own appreciation.
;; 
;; == COMMENTS ==
;; No provisions regarding comments have been accommodated by the
;; current language iteration.
;; 
;; == GRAMMAR ==
;; Following the Extended Backus-Naur Form (EBNF), a description of the
;; Runespells grammar shall be adduced:
;; 
;;   program      := { emptyLine | runeLine } ,
;;                   ( optSpaces | rune | spell ) ;
;;   
;;   emptyLine    := optSpaces , newline;
;;   runeLine     := ( rune | spell ) , newline ;
;;   
;;   rune         := optSpaces ,
;;                   "Rune" , optSpaces ,
;;                   runeID , optSpaces ,
;;                   variableList , optSpaces ,
;;                   commandList ,
;;                   optSpaces;
;;   spell        := optSpaces ,
;;                   "Spell" , optSpaces ,
;;                   variableList , optSpaces ,
;;                   commandList ,
;;                   optSpaces ;
;;   
;;   commandList  := [ statement , { spaces , statement } ];
;;   statement    := command | variableName ;
;;   command      := "Chixo"
;;                |  "Chiyo"
;;                |  "Chizo"
;;                |  "Giyah" , spaces , variableName
;;                |  "Gorpdne"
;;                |  "Lafi"
;;                |  "Mizi"
;;                |  "Mizo"
;;                |  "Nahweh"
;;                |  "Ne"
;;                |  "Ni"
;;                |  "Tazi"
;;                |  "Yah"
;;                |  "Zi ;
;;   
;;   variableName := "Fa" | "Rin" | "Gora" | "Jyiku" | "Nahy" | "Zeha" ;
;;   
;;   variableList := "[" , { runeID } , "]" , ":" ;
;;   
;;   runeID       := digit , { digit };
;;   
;;   newline      := "\n" ;
;;   optSpaces    := [ spaces ] ;
;;   spaces       := space , { space } ;
;;   space        := " " | "\t" ;
;;   digit        := "0" | "1" | "2" | "3" | "4"
;;                |  "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Instructions
;; ============
;; Runespells's instruction set attends to a tally of fourteen commands,
;; enhanced by six variable names appropriating an operative character
;; in a rune's body.
;; 
;; The commands encompass functionality for manipulating the rune stack,
;; invoking other runes, and committing inputs and outputs along the
;; respective channels.
;; 
;; == OVERVIEW ==
;; A concise apercu shall educate about the available commands.
;; 
;;   Command      | Effect
;;   -------------+----------------------------------------------------
;;   Chixo        | Moves to the first command of the currently
;;                | processed rune if the rune stack is empty.
;;   ..................................................................
;;   Chiyo        | Moves to the first command of the currently
;;                | processed rune if the rune stack is not empty.
;;   ..................................................................
;;   Chizo        | Moves to the first command of the currently
;;                | processed rune.
;;   ..................................................................
;;   Giyah <rune> | Pops the top rune, whose number of commands shall
;;                | be designated as "N", and replaces the rune
;;                | associated with the ID <rune> with the rune
;;                | amenable to the ID "N".
;;   ..................................................................
;;   Gorpdne      | Terminates the program.
;;   ..................................................................
;;   Lafi         | Duplicates the top rune on the stack.
;;   ..................................................................
;;   Mizi         | Prompts an ASCII character from the user, whose
;;                | numeric value shall be designated as "N". Then
;;                | creates a new rune without any variables, but with
;;                | a number of no-operations (no-ops) equal to "N",
;;                | and pushes this new rune unto the rune stack.
;;   ..................................................................
;;   Mizo         | Pops the top rune, whose number of commands shall
;;                | be designated as "N", and writes to the standard
;;                | output the ASCII character corresponding to "N".
;;   ..................................................................
;;   Nahweh       | Considers the number of commands in the top rune,
;;                | designated as "N(1)", and the tally of commands in
;;                | the third top rune, "N(3)". If N(1) = N(3), both
;;                | the top and third top rune are removed, and the
;;                | formerly second top rune is executed.
;;   ..................................................................
;;   Ne           | A no-operation (no-op). It provides no effect.
;;   ..................................................................
;;   Ni           | Pops the top rune from the rune stack and removes
;;                | all commands from it.
;;   ..................................................................
;;   Tazi         | Pops the top rune.
;;   ..................................................................
;;   Yah          | Pops the top rune and executes it.
;;   ..................................................................
;;   Zi           | Appends to the body of the top rune all commands of
;;                | the second top rune, and pops both from the stack.
;;   ..................................................................
;;   <variable>   | Pushes the rune associated with the ID stored in
;;                | the argument <variable> unto the stack.
;; 
;; 
;; Implementation
;; ==============
;; The Common Lisp implementation at hand attempts a simple realization
;; of the Runespells specification.
;; 
;; == INTERPRETATION SUCCEEDS FOLLOWING THREE STAGES ==
;; The interpretation objective's fulfilment is established upon a three
;; tiers' apportionment of responsibilities:
;; 
;;   (1) A lexical analyzer, or lexer, distils from the Runespells
;;       source codes a sequence of tokens, each a significant object's
;;       representative.
;;   (2) The tokens are assembled using a parser, its produce manifested
;;       in a list of rune definitions, encapsulating all data requisite
;;       for the later rune and spell creations.
;;   (3) Ultimately, the interpreter processes the rune definitions to
;;       apply effect to its components. This entails in particular the
;;       creation of one rune or spell per definition and its
;;       registration in a mapping from the unique rune ID to the thus
;;       established instance.
;; 
;; The first two layers' destitution anenst interesting properties
;; redirects the coming sections' cynosure towards the process'
;; desinence, that is, the interpreter. Some kenspeckle items from this
;; stage shall be a further elucidation's focus.
;; 
;; == VARIABLES RESIDE IN A PROPERTY LIST ==
;; A rune's variables are maintained in a property list, associating
;; with the variable name the value in the form a referenced rune ID.
;; 
;; The patent characteristic of a rune's variables, encompassing the
;; tacit enumeration by the associated value's position in the list, as
;; well as the modest and fixed quantity of a sextuple being to a rune's
;; avail, serves to limn a set of undemanding requirements. In order to
;; ascertain their satisfaction, a simple property list, also known as
;; the curtailed agnomination of the "plist", has been adjudged a viable
;; solution.
;; 
;; The property list constitutes, in compernage with the association
;; list and the hash table, a basic data structure for the
;; implementation of the dictionary or map concept. A plist plainly
;; represents its entries in the form of alternating keys and values,
;; destitute of further sepiments' adminiculum, and persisted in the
;; foundational linked list type. Thus, a set of N entries, consisting
;; of the keys k[1] through k[N], associated to the respective values
;; v[1] to v[N], experiences its statement in the form
;; 
;;   (k[1] v[1] k[2] v[2] ... k[i] v[i] ... k[N] v[N])
;; 
;; An essential tenet involves the requisite of the plist's size to be
;; even, as each two successive elements form a logical compound. As a
;; concomitant ramification ensuing from its representation as a list,
;; all Common Lisp operations appertaining to the general list type
;; apply to its specialization. As an act of supererogation, additional
;; functions exist for the property list's perquisition and
;; manipulation.
;; 
;; A restriction, its reverberation shows in the devoved facilities,
;; imposes the use of symbols as keys, as these operations implicitly
;; impute identity as the comparison predicate. In our particular
;; project, this stricture does not levy any detrimental implications,
;; as the variable identifiers, designated by the custom type
;; ``variable-name'', are already defined as keyword symbols.
;; 
;; When engaged in the agon betwixt its two competitors, the association
;; list and the hash table, the plist partakes of the same frailties
;; regarding the lookup performance as the first agent: a linear
;; function of its size --- that is, a time requirement of O(n), with
;; "n" being the tally of entries submitted to its castaldy. The hash
;; table, at least conceptually, might proffer a superior amenability to
;; inquests and modifications --- meted in the anticipation of O(1);
;; natheless, this data structure's expected overhead, in affiliation
;; with the scant maximum size of six entries, each for one possible
;; variable, conduces the steadable nature of the most simple choice,
;; manifested in the property list.
;; 
;; == RUNE INVOCATIONS INVOLVE ACTIVATION RECORDS ==
;; A consequence of its invocation, an activation record is generated
;; for the respective rune, maintaining an instruction pointer into its
;; command list.
;; 
;; Runespells programs operate by tracing, starting with a "spell" as
;; the entry point, the invoked runes. The requirement of maintaining an
;; instruction pointer (IP) into the zero or more rune commands is
;; satisfied by aide of activation records. Such an entity is created
;; in the moment of a rune's invocation, storing the same in conjunction
;; with an instruction pointer and a reference to the currently selected
;; item among the rune's command list.
;; 
;; The duties' circumference reckoned in traditional treatises on
;; activation records --- in particular the stewardship of the arguments
;; and local bindings --- do not directly infringe upon this case: As a
;; product of Runespells's diorism in the rune and spell definition,
;; where actual parameters participate directly in the specification,
;; and the formal allies work tacitly, all requisite data, apart from
;; the pointer into the currently selected command, finds itself already
;; incorporated in a ``Rune'' instance. A record's reference thus relays
;; to this exact piece of gnarity.
;; 
;; The activation records are maintained by the interpreter inside of a
;; stack, known as the "control stack", and agnominated in our case by
;; the more specific slot designation "records", a last-in first-out
;; (LIFO) storage, which upon detecting a rune's summoning receives the
;; newly produced record and pushes it unto its top, thus rendering it
;; as the current context for the interpreter's workings.
;; 
;; If the commands stored in the activation record's rune have been
;; processed in their entirety, the record is popped from the control
;; stack, by which action the previously active item, now being located
;; at the storage top, receives the purview over the program flow's.
;; 
;; At the program's incipiency, the first activation record is generated
;; for the "Spell" rune, the entry point. The spell record's removal
;; ultimately conditions the code's termination.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Despite its delineation by aide of command descriptions and an
;; example, the Runespells specification is inflicted with a few minor
;; lacunae, a selection of which shall be enumerated in the following.
;; 
;; == DOES THE PRESENCE OF A SPELL IMPOSE A REQUIREMENT? ==
;; A "Spell" defines the entry point into a Runespells program. The
;; official standard, however, does not mention the causatum incited by
;; its absence nor its duplicate establishment.
;; 
;; Two contingencies compete in the ambivalence's resolution:
;; 
;;   (1) LENIENCY AND TERMINATION
;;       A program without a spell definition silently terminates
;;       immediately following its execution.
;;   (2) STRINGENCY AND FAILURE
;;       A missing spell aborts the program with an error.
;; 
;; Maugre the second alternative's allurement, as posing an acquaintance
;; from the C and Java programming languages whose dependence upon a
;; "main" function or method delineates a requisite for a program's
;; operation, the more liberal environment into which esoteric languages
;; are subsumed shall locate Runespell's behavior in the bailiwick of
;; the first solution: A program without a single spell shall terminate
;; immediately, but not irregularly.
;; 
;; In the case of more than one spell's definition, by which agency no
;; disambiguation can be derived, an error shall incite an abortion.
;; 
;; == MAY RUNE IDENTIFIERS BE SPECIFIED DIRECTLY? ==
;; The Runespells command set includes two occasions upon which a rune
;; ID might be stated immediately by the programmer: as the argument to
;; the "Giyah" command and as the "<rune>" statement, responsible for
;; the insertion of the associated rune unto the stack. A concrete
;; proclamation is wanting about whether such a datum must be conveyed
;; inside of a variable, or is homologated to be rendered as a literal
;; integer.
;; 
;; Considering the fact that the only source sufficiently potent to
;; supply a deictic vallidom exclusively employs the variable solution,
;; the conclusion has been reached that only these placeholders may
;; apply themselves to this task.
;; 
;; == WHICH MODALITY APPLIES TO INPUT AND OUTPUT? ==
;; The Runespells definition claims a correspondence betwixt ASCII
;; characters and numeric values along the communication channels,
;; without explication of its details. Concretely, a user input shall
;; be an ASCII character acting as the basis for a no-ops rune to be
;; created and added to the stack. Conversely, the output operation
;; converts the number of commands in the top rune to a numeric datum
;; and prints the affiliated ASCII character.
;; 
;; An explanation lacks about the actual transformation process from the
;; character to the integer realm and vice versa. Thus, two
;; possibilities can be assayed as tenable:
;; 
;;   (1) The ASCII character is construed as a literal digit, without
;;       involvement of the character code.
;;   (2) The ASCII character is converted into the integer equivalent
;;       to its character code.
;; 
;; Verily, the second option accords more closely to most programming
;; languages; yet, the sole example provided in the original standard
;; signifies the printing of numbers and mentions a character of "0",
;; which has been reckoned as sufficiently peisant to assume the first
;; contingency, the interpretation of an ASCII character as a digit. A
;; corollary thereof, inputs and outputs are confiend to the range
;; [0, 9].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-08-09
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Runespells"
;;       o The current Runespells specification.
;;   -> "https://esolangs.org/w/index.php?title=Runespells&oldid=19251"
;;       o An older version of the Runespells specification, which
;;         natheless provides additional apostilles wanting in the
;;         current rendition.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based last-in first-out (LIFO)
   structure, each element of which conforms to the ELEMENT-TYPE,
   defaulting to ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist, as a
   potentially empty list of even element count, producing its entries
   by succeeding each key, or indicator, with its associated value,
   the former of which conforms to the INDICATOR-TYPE, while the latter
   assumes the VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            ;; The OBJECT must be a list ...
            (listp object)
            ;; ... with an even tally of elements, which also
            ;; comprehends a zero quantity, ...
            (evenp
              (the (integer 0 *)
                (length (the list object))))
            ;; ... each indicator, or key, of which conforms to the
            ;; INDICATOR-TYPE and is succeeded by the associated value,
            ;; being of the VALUE-TYPE.
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype variable-name ()
  "The ``variable-name'' type enumerates the possible variable names for
   employment in a rune's definition."
  '(member
    :Fa
    :Rin
    :Gora
    :Jyiku
    :Nahy
    :Zeha))

;;; -------------------------------------------------------

(deftype command-name ()
  "The ``command-name'' type enumerates the recognized variants of
   commands, including an identifier for the general rune push
   designation."
  '(member
    :Zi
    :Yah
    :Ni
    :Giyah
    :Ne
    :Mizo
    :Mizi
    :Lafi
    :Chizo
    :Chixo
    :Chiyo
    :Nahweh
    :Tazi
    :Gorpdne))

;;; -------------------------------------------------------

(deftype command-identifier ()
  "The ``command-identifier'' type defines the identifiers valid as
   constituents of a rune or spell definition's code section, which
   includes command names, enumerated by the ``command-name'' type, and
   variable names, comprehended in the ``variable-name'' type."
  '(or command-name variable-name))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' enumerates the category of identifiers expected
   to be encountered in a rune or spell's code section.
   ---
   Two classes are incorporated in this declaration:
      :command
        Identifiers which correspond to the fourteen Runespells
        commands.
      :rune
        Identifiers which correspond to the six possible variable
        names.
   ---
   Please note that this current implementation does apportions no usage
   to the ``command-type'', despite its association with a parsed
   ``Rune-Definition''. The act of inclusion merely serves a
   complementary and informative purpose. Future versions may refer to
   its presence with more gratitude."
  '(member :command :rune))

;;; -------------------------------------------------------

(deftype rune-id ()
  "The ``rune-id'' type defines an designator, intended for identifying
   a rune in a unique manner, by utilizing a positive integer number in
   the range [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype rune-type ()
  "The ``rune-type'' enumerates the possible variants of rune
   definitions."
  '(member :rune :spell))

;;; -------------------------------------------------------

(deftype variable-id-list ()
  "The ``variable-id-list'' type defines a list of rune identifiers,
   each such a positive integer number."
  '(list-of rune-id))

;;; -------------------------------------------------------

(deftype variable-set ()
  "The ``variable-set'' type defines a mapping of variable names to
   rune IDs in the form of a property list, the keys of which conform to
   the ``variable-name'' type, while the associated values constitute
   ``rune-id'' objects."
  '(property-list-of variable-name rune-id))

;;; -------------------------------------------------------

(deftype command-list ()
  "The ``command-list'' type defines a vector of commands."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype rune-table ()
  "The ``rune-table'' type defines a hash table of rune IDs associated
   to actual Rune objects."
  '(hash-table-of rune-id Rune))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant object detected during
   the lexical analyzation of piece of Runespells source code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global tokens.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal)
  "Associates with the recognized Runespells keywords the respective
   tokens representing the same.")

;;; -------------------------------------------------------

(flet ((add-identifier (name token-type token-value)
        "Creates a new token designated by the TOKEN-TYPE and the
         TOKEN-VALUE, associates it with the NAME in the +IDENTIFIERS+
         table, and returns no value.
         ---
         If an entry with the NAME already exists, it is silently
         superseded."
        (declare (type string  name))
        (declare (type keyword token-type))
        (declare (type T       token-value))
        (setf (gethash name +IDENTIFIERS+)
              (make-token token-type token-value))
        (values)))
  
  (add-identifier "Rune"    :rune     :rune)
  (add-identifier "Spell"   :spell    :spell)
  
  (add-identifier "Fa"      :variable :Fa)
  (add-identifier "Rin"     :variable :Rin)
  (add-identifier "Gora"    :variable :Gora)
  (add-identifier "Jyiku"   :variable :Jyiku)
  (add-identifier "Nahy"    :variable :Nahy)
  (add-identifier "Zeha"    :variable :Zeha)
  
  (add-identifier "Zi"      :command  :Zi)
  (add-identifier "Yah"     :command  :Yah)
  (add-identifier "Ni"      :command  :Ni)
  (add-identifier "Giyah"   :command  :Giyah)
  (add-identifier "Ne"      :command  :Ne)
  (add-identifier "Mizo"    :command  :Mizo)
  (add-identifier "Mizi"    :command  :Mizi)
  (add-identifier "Lafi"    :command  :Lafi)
  (add-identifier "Chizo"   :command  :Chizo)
  (add-identifier "Chixo"   :command  :Chixo)
  (add-identifier "Chiyo"   :command  :Chiyo)
  (add-identifier "Nahweh"  :command  :Nahweh)
  (add-identifier "Tazi"    :command  :Tazi)
  (add-identifier "Gorpdne" :command  :Gorpdne)
  
  (values))

;;; -------------------------------------------------------

(defun get-identifier-for-name (name)
  "Returns the token associated with the NAME, or signals an error if no
   correspondence exists."
  (declare (type string name))
  (the Token
    (or (gethash name +IDENTIFIERS+)
        (error "No identifier registered for the name ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun newline-character-p (character)
  "Checks whether the CHARACTER represents a linebreak, returning a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Newline #\Linefeed #\Return)
        :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class performs a division of a piece of Runespells
     source code into its tokens."))

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
  "Creates and returns a new ``Lexer'' operating on the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its three slots, \"source\", \"position\",
   and \"character\", to eponymous symbol macros for reading and
   writing, and evaluates the BODY statements, returing the last
   evaluated form's results.
   ---
   This macro offers a more concise alternative to the inherent
   ``with-slots'' facility, configured with type specifications and
   ``ignorable'' declarations for each slot.
   ---
   Please note that, as a significant deviation from its cognate, the
   ``with-lexer'' macro does not permit the free assignment of the
   symbols bound to the original slot name, perforce conflating both
   definitions. As a consectary, local bindings should abstain from
   choosing these three identifiers, if not deliberately purported to
   employ shadowing."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (with-slots (source position character) ,evaluated-lexer
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         ,@body))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next character in the
   source, if possible, updates the internal state, and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position in the LEXER's source, reads an
   unsigned integer number and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current position in the LEXER's source, reads an
   identifier and returns the associated token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (get-identifier-for-name
        (with-output-to-string (name)
          (declare (type string-stream name))
          (loop while (and character (alpha-char-p character)) do
            (write-char character name)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-spaces (lexer)
  "Starting at the current position in the LEXER's source, reads a
   series of one or more spaces, coalesces these, and returns a single
   token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :spaces
        (with-output-to-string (spaces)
          (declare (type string-stream spaces))
          (loop while (and character (space-character-p character)) do
            (write-char character spaces)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-newlines (lexer)
  "Starting at the current position in the LEXER's source, reads a
   series of one or more linebreaks, coalesces these, and returns a
   single token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :newlines
        (with-output-to-string (newlines)
          (declare (type string-stream newlines))
          (loop while (and character (newline-character-p character)) do
            (write-char character newlines)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its exhaustion, the LEXER responds to any request by a fresh
   end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (lexer-read-spaces lexer))
        
        ((newline-character-p character)
          (lexer-read-newlines lexer))
        
        ((char= character #\[)
          (prog1
            (make-token :left-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\])
          (prog1
            (make-token :right-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\,)
          (prog1
            (make-token :comma character)
            (lexer-advance lexer)))
        
        ((char= character #\:)
          (prog1
            (make-token :colon character)
            (lexer-advance lexer)))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (type identifier
                              &optional (argument NIL))))
  "The ``Command'' class models an instruction contained in the body of
   a rune or spell, containing besides its type and agnomination the
   contingency of a single argument."
  (type       (error "Missing command type.") :type command-type)
  (identifier (error "Missing command name.") :type command-identifier)
  (argument   NIL                             :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Rune-Definition".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Rune-Definition
  (:constructor make-rune-definition
    (id variable-ids commands &aux (type :rune)))
  (:constructor make-spell-definition
    (variable-ids commands &aux (type :spell))))
  "The ``Rune-Definition'' class encapsulates the data necessary for the
   description of a rune or spell definition.
   ---
   A ``Rune-Definition'' is generated for each \"Rune\" or \"Spell\"
   definition encountered by a parser, maintaining all information
   requisite for reproduction of a rune in the interpreter."
  (type         NIL :type (or null rune-type))
  (id           NIL :type (or null rune-id))
  (variable-ids NIL :type (or null variable-id-list))
  (commands     NIL :type (or null command-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The lexer responsible for the token supply.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The token most recently acquired from the LEXER."))
  (:documentation
    "The ``Parser'' class' responsibility constitutes the assemblage of
     a list of rune definitions from the tokens purveyed by a lexer."))

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
  "Creates and returns a new ``Parser'' whose tokens are supplied by the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots LEXER and CURRENT-TOKEN as
   readable and writable symbol macros to their respective names, and
   evaluates the BODY forms, returning their last result values.
   ---
   The slots LEXER and CURRENT-TOKEN are subjected to the their matching
   type specifiers and designated as ignorable. Please note that their
   establishment bars further configurations, in particular the custom
   binding of an alternative name."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (with-slots (lexer current-token) ,evaluated-parser
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         (declare (ignorable  lexer))
         (declare (ignorable  current-token))
         ,@body))))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation loading and storing the next
   token from internally managed lexer and returning the just indagated
   one; otherwise signal an error of an unspecified type."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Token
      (if (token-type-p current-token expected-token-type)
        (prog1
          current-token
          (setf current-token
            (lexer-get-next-token lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun parser-skip-spaces (parser)
  "Starting with the PARSER's current token, skips zero or more space
   tokens and returns the modified PARSER."
  (declare (type Parser parser))
  (with-parser (parser)
    (loop while (token-type-p current-token :spaces) do
      (parser-eat parser :spaces)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-variable-list (parser)
  "Starting with the PARSER's current token, parses a bracketed list of
   variable IDs and returns a list containing these.
   ---
   The represented grammar amounts to:
     variableList := '[' , runeID , ']' ;
     runeID       := digit , { digit } ;"
  (declare (type Parser parser))
  
  (with-parser (parser)
    (let ((variable-ids NIL))
      (declare (type (list-of rune-id) variable-ids))
      
      (parser-eat         parser :left-bracket)
      (parser-skip-spaces parser)
      
      (loop while (token-type-p current-token :number) do
        (let ((variable-id (parser-eat parser :number)))
          (declare (type Token variable-id))
          
          (push (token-value variable-id) variable-ids)
          
          (parser-skip-spaces parser)
          
          (cond
            ((token-type-p current-token :comma)
              (parser-eat         parser :comma)
              (parser-skip-spaces parser))
            (T
              (loop-finish)))))
      
      (parser-skip-spaces parser)
      (parser-eat         parser :right-bracket)
      
      (the (list-of rune-id)
        (nreverse variable-ids)))))

;;; -------------------------------------------------------

(defun parser-parse-command-list (parser)
  "Starting with the PARSER's current token, reads a sequence of zero or
   more commands and returns a one-dimensional simple array containing
   these.
   ---
   The represented grammar amounts to:
     commandList  := { command } ;
     command      := 'Zi' | 'Yah' | ... | 'Gorpdne'
                  |  'Fa' | 'Rin' | ... | 'Zeha' ;"
  (declare (type Parser parser))
  (with-parser (parser)
    
    (let ((commands NIL))
      (declare (type (list-of Command) commands))
      
      (flet ((add-command (type identifier &optional (argument NIL))
              "Creates a new command defined by the TYPE and IDENTIFIER,
               optionally depending on the ARGUMENT, inserts it at the
               COMMANDS list's front, and returns no value."
              (declare (type command-type       type))
              (declare (type command-identifier identifier))
              (declare (type T                  argument))
              (push (make-command type identifier argument) commands)
              (values)))
        
        (loop do
          (parser-skip-spaces parser)
          
          (case (token-type current-token)
            ;; Command is a rune name, that is: <rune>.
            (:variable
              (add-command :rune (token-value current-token))
              (parser-eat         parser :variable)
              (parser-skip-spaces parser))
            
            ;; Command is a command name.
            (:command
              (let ((command-identifier (token-value current-token)))
                (declare (type command-identifier command-identifier))
                (parser-eat parser :command)
                (cond
                  ;; The "Giyah" command expects a rune as its argument.
                  ((eq command-identifier :Giyah)
                    (parser-skip-spaces parser)
                    (let ((command-argument (parser-eat parser :variable)))
                      (declare (type Token command-argument))
                      (add-command :command command-identifier
                        (token-value command-argument))
                      (parser-skip-spaces parser)))
                  ;; All other commands abstain from arguments.
                  (T
                    (add-command :command command-identifier)
                    (parser-skip-spaces parser)))))
            
            (otherwise
              (loop-finish)))))
      
      (the command-list
        (coerce (nreverse commands)
          '(simple-array Command (*)))))))

;;; -------------------------------------------------------

(defun parser-parse-rune-configurations (parser)
  "Starting with the PARSER's current token, reads the portion common
   to both runes and spells, entailing a bracketed list of variable IDs
   and a sequence of zero or more commands, and returns two values:
     (1) A list of the rune IDs representing the variable values
     (2) A one-dimensional simple array of ``Command'' objects
         representing the rune body.
   ---
   The represented grammar amounts to:
     runeConfiguration := variableList , ':' , commandList ;
     variableList      := '[' , runeID , ']' ;
     runeID            := digit , { digit } ;
     commandList       := { command } ;
     command           := 'Zi' | 'Yah' | ... | 'Gorpdne'
                       |  'Fa' | 'Rin' | ... | 'Zeha' ;"
  (declare (type Parser parser))
  (with-parser (parser)
    (parser-skip-spaces parser)
    (let ((variable-ids (parser-parse-variable-list parser)))
      (declare (type variable-id-list variable-ids))
      (parser-skip-spaces parser)
      (parser-eat         parser :colon)
      (parser-skip-spaces parser)
      (let ((commands (parser-parse-command-list parser)))
        (declare (type command-list commands))
        (parser-skip-spaces parser)
        (the (values variable-id-list command-list)
          (values variable-ids commands))))))

;;; -------------------------------------------------------

(defun parser-expect-end-of-definition (parser)
  "Checks whether the PARSER's current token represents the end of a
   rune or spell definition, on confirmation either consuming the
   expected newlines or ignoring the end-of-file (EOF), and returning
   the modified PARSER; otherwise signals an error of an unspecified
   type."
  (declare (type Parser parser))
  (with-parser (parser)
    (parser-skip-spaces parser)
    (case (token-type current-token)
      (:newlines
        (parser-eat parser :newlines))
      (:eof
        NIL)
      (otherwise
        (error "Expected a newline or end-of-file to terminate the ~
                rune definition, but encountered the token ~s."
          current-token))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-rune-definition (parser)
  "Starting with the PARSR's current token, parses a rune or spell and
   returns a ``Rune-Definition'' representation thereof.
   ---
   The represented grammar amounts to:
     runeDefinition := ( 'Rune' | 'Spell' ) ,
                       variableList , ':' , commandList ,
                       ( newLine | EOF ) ;
     variableList   := '[' , runeID , ']' ;
     runeID         := digit , { digit } ;
     commandList    := { command } ;
     command        := 'Zi' | 'Yah' | ... | 'Gorpdne'
                    |  'Fa' | 'Rin' | ... | 'Zeha' ;"
  (declare (type Parser parser))
  (with-parser (parser)
    (parser-skip-spaces parser)
    
    (the Rune-Definition
      (case (token-type current-token)
        (:rune
          (parser-eat parser :rune)
          (parser-eat parser :spaces)
          (let ((rune-id (parser-eat parser :number)))
            (declare (type Token rune-id))
            (parser-skip-spaces parser)
            (multiple-value-bind (variable-ids commands)
                (parser-parse-rune-configurations parser)
              (declare (type variable-id-list variable-ids))
              (declare (type command-list     commands))
              (prog1
                (make-rune-definition
                  (token-value rune-id)
                  variable-ids
                  commands)
                (parser-expect-end-of-definition parser)))))
        
        (:spell
          (parser-eat parser :spell)
          (parser-eat parser :spaces)
          (multiple-value-bind (variable-ids commands)
                (parser-parse-rune-configurations parser)
            (declare (type variable-id-list variable-ids))
            (declare (type command-list     commands))
            (prog1
              (make-spell-definition variable-ids commands)
              (parser-expect-end-of-definition parser))))
        
        (otherwise
          (error "Expected a rune or spell definition, but encountered ~
                  the token ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-rune-definitions (parser)
  "Starting with the PARSER's current token, parses a sequence of zero
   or more runes or spells and returns a list containing the
   ``Rune-Definition'' representations thereof.
   ---
   The represented grammar amounts to:
     runeDefinitions := { runeDefinitions } ;
     runeDefinition  := ( 'Rune' | 'Spell' ) ,
                        variableList , ':' , commandList ,
                        ( newLine | EOF ) ;
     variableList    := '[' , runeID , ']' ;
     runeID          := digit , { digit } ;
     commandList     := { command } ;
     command         := 'Zi' | 'Yah' | ... | 'Gorpdne'
                     |  'Fa' | 'Rin' | ... | 'Zeha' ;"
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((definitions NIL))
      (declare (type (list-of Rune-Definition) definitions))
      (loop do
        (case (token-type current-token)
          ((:rune :spell)
            (push (parser-parse-rune-definition parser) definitions)
            (parser-skip-spaces parser))
          
          (:spaces
            (parser-skip-spaces parser))
          
          (:newlines
            (parser-eat parser :newlines))
          
          (:eof
            (loop-finish))
          
          (otherwise
            (error "Unexpected token while reading rune definitions: ~s."
              current-token))))
      (the (list-of Rune-Definition)
        (nreverse definitions)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Orders the PARSER to assemble the tokens of its internally managed
   lexer into a list of rune definitions."
  (declare (type Parser parser))
  (with-parser (parser)
    (the (list-of Rune-Definition)
      (prog1
        (parser-parse-rune-definitions parser)
        (parser-eat                    parser :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable set.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of variable-name) +VARIABLE-NAMES+))

;;; -------------------------------------------------------

(defparameter +VARIABLE-NAMES+
  '(:Fa :Rin :Gora :Jyiku :Nahy :Zeha)
  "The variable names ordered and collected for perquisition purposes.
   ---
   This list's telos wones in a facilitated association of variable
   names with a list of rune IDs, thus generating a property list of
   name-value pairs.")

;;; -------------------------------------------------------

(defun make-variable-set (variable-ids)
  "Creates and returns a property list which associates in the correct
   order the variable names with the VARIABLE-IDs.
   ---
   The thus produced property list, or plist, adheres to the following
   pattern:
     (variable-name-1 variable-id-1
      variable-name-2 variable-id-2
      ...
      variable-name-N variable-id-N)"
  (declare (type variable-id-list variable-ids))
  (the variable-set
    (loop
      for     variable-id   of-type rune-id       in variable-ids
      for     variable-name of-type variable-name in +VARIABLE-NAMES+
      collect variable-name
      collect variable-id)))

;;; -------------------------------------------------------

(defun variable-set-get-value-of (variable-set variable-name)
  "Returns the rune ID associated with the VARIABLE-NAME in the property
   list VARIABLE-SET, or signals an error on the indicator's absence."
  (declare (type variable-set  variable-set))
  (declare (type variable-name variable-name))
  (the rune-id
    (or
      (getf variable-set variable-name)
      (error "The variable set does not contain a value for the ~
              variable name ~s."
        variable-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Rune".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-command-list ()
  "Creates and returns an empty vector of commands."
  (the command-list
    (make-array 0
      :element-type    'Command
      :initial-element (make-command :command :Gorpdne)
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defclass Rune ()
  ((type
    :initarg       :type
    :initform      (error "Missing rune type.")
    :type          rune-type
    :documentation "Determines whether this object represents a simple
                    rune or a spell, that is, an entry function.")
   (id
    :initarg       :id
    :initform      NIL
    :type          (or null rune-id)
    :documentation "A positive integer identifier for this rune, if it
                    constitutes a simple rune. In the case of a spell
                    this resolves to ``NIL''.")
   (variables
    :initarg       :variables
    :initform      NIL
    :type          variable-set
    :documentation "A property list containing the variable names and
                    their associated values.")
   (commands
    :initarg       :commands
    :initform      (make-empty-command-list)
    :type          command-list
    :documentation "A vector of the rune's commands."))
  (:documentation
    "The ``Rune'' class represents the procedural unit known as a
     \"rune\" in the Runespells programming language, embracing in a
     single concept both the simple rune and the spell as a particular
     case.
     ---
     A rune is defined by four attributes:
       - Its type, designating its membership among the simple rune or
         spell species.
       - An optional ID, obligatory for a simple rune, set to ``NIL''
         for spell.
       - A set of variables in the form of a property list, mapping to
         zero or more of the six available variable names the referenced
         rune IDs, thus establishing the argument list.
       - A vector of zero or more commands comprising the body.
     ---
     Most commonly, but not exclusively, a ``Rune'' instance is derived
     from a ``Rune-Definition'' produced by the ``Parser'' class and
     already containing the most significant attributes."))

;;; -------------------------------------------------------

(defun make-rune (type id variables commands)
  "Creates and returns a new ``Rune'' of the specified TYPE and ID,
   relying on the VARIABLES, and ensconcing the COMMANDS."
  (declare (type rune-type         type))
  (declare (type (or null rune-id) id))
  (declare (type variable-set      variables))
  (declare (type command-list      commands))
  (the Rune
    (make-instance 'Rune
      :type      type
      :id        id
      :variables variables
      :commands  commands)))

;;; -------------------------------------------------------

(defun make-no-ops-rune (id number-of-no-ops)
  "Creates and returns a new ``Rune'' designated by the ID and
   ensconcing exclusively a tally of NUMBER-OF-NO-OPS no-ops, that is
   ``Ne'' commands."
  (declare (type rune-id       id))
  (declare (type (integer 0 *) number-of-no-ops))
  (the Rune
    (make-rune :rune id NIL
      (coerce
        (loop repeat number-of-no-ops collect
          (make-command :command :Ne))
        '(simple-array Command (*))))))

;;; -------------------------------------------------------

(defun make-rune-from-definition (definition)
  "Creates and returns a new ``Rune'' based upon the rune DEFINITION."
  (declare (type Rune-Definition definition))
  (the Rune
    (make-instance 'Rune
      :type      (rune-definition-type definition)
      :id        (rune-definition-id   definition)
      :variables (make-variable-set
                   (rune-definition-variable-ids definition))
      :commands  (rune-definition-commands definition))))

;;; -------------------------------------------------------

(defun rune-id (rune)
  "Returns the RUNE's identifier, if defined, otherwise yielding the
   ``NIL'' value."
  (declare (type Rune rune))
  (the (or null rune-id)
    (slot-value rune 'id)))

;;; -------------------------------------------------------

(defun rune-type (rune)
  "Returns the RUNE type."
  (declare (type Rune rune))
  (the rune-type
    (slot-value rune 'type)))

;;; -------------------------------------------------------

(defun rune-variables (rune)
  "Returns a property list containing the variables associated with the
   RUNE."
  (declare (type Rune rune))
  (the variable-set
    (slot-value rune 'variables)))

;;; -------------------------------------------------------

(defun rune-variable-value (rune variable-name)
  "Returns the rune ID associated with the VARIABLE-NAME in the RUNE's
   variable set, or signals an error if this VARIABLE-NAME has not been
   defined for the RUNE."
  (declare (type Rune          rune))
  (declare (type variable-name variable-name))
  (the rune-id
    (or
      (variable-set-get-value-of (rune-variables rune) variable-name)
      (error "No ID (value) defined for the variable ~s in the rune ~s."
        variable-name rune))))

;;; -------------------------------------------------------

(defun rune-commands (rune)
  "Returns a vector containing the RUNE's commands."
  (declare (type Rune rune))
  (the command-list
    (slot-value rune 'commands)))

;;; -------------------------------------------------------

(defun (setf rune-commands) (new-commands rune)
  "Sets the RUNE's command vector to the NEW-COMMANDS and returns the
   modified RUNE."
  (declare (type command-list new-commands))
  (declare (type Rune         rune))
  (setf (slot-value rune 'commands) new-commands)
  (the Rune rune))

;;; -------------------------------------------------------

(defun rune-command-count (rune)
  "Returns the number of commands comprising the RUNE."
  (declare (type Rune rune))
  (the (integer 0 *)
    (length (slot-value rune 'commands))))

;;; -------------------------------------------------------

(defun rune-enhance (receiver donor)
  "Appends to the RECEIVER rune all the commands stored in the DONOR
   and returns the modified RECEIVER."
  (declare (type Rune receiver))
  (declare (type Rune donor))
  (setf (rune-commands receiver)
        (concatenate '(vector Command *)
          (rune-commands receiver)
          (rune-commands donor)))
  (the Rune receiver))

;;; -------------------------------------------------------

(defun rune-disenhance (rune)
  "Removes all commands from the RUNE and returns the modified RUNE."
  (declare (type Rune rune))
  (setf (rune-commands rune)
        (make-empty-command-list))
  (the Rune rune))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Activation-Record".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Activation-Record ()
  ((rune
    :initarg       :rune
    :initform      NIL
    :type          (or null Rune)
    :documentation "The rune whose commands shall be processed.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The index of the RUNE's processed command.")
   (command
    :initarg       :command
    :initform      NIL
    :type          (or null Command)
    :documentation "The command at the current index IP in the RUNE's
                    command list."))
  (:documentation
    "The ``Activation-Record'' class encapsulates all information
     necessary for a rune invocation's castaldy, including in particular
     the maintenance of its instruction pointer and command
     selection.
     ---
     A rune's invocation reflects in a new activation record's creation,
     followed by the insertion of the same on a special stack, the
     so-called \"control stack\", as an implement for the memorization
     of the call orders. The activation record residing on top of this
     collection defines the currently active and processed rune.
     Succeeding its completion by exhausting the represented rune's
     command sequence, the top activation record is removed, relaying
     the operational onus to the new stack head. The storage's vacancy
     signifies the completed processing of all program instructions, and
     the subsequent termination of the program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((record Activation-Record) &key)
  (declare (type Activation-Record record))
  (with-slots (rune ip command) record
    (declare (type (or null Rune)    rune))
    (declare (type fixnum            ip))
    (declare (type (or null Command) command))
    (setf command
      (when (array-in-bounds-p (rune-commands rune) ip)
        (aref (rune-commands rune) ip))))
  (the Activation-Record record))

;;; -------------------------------------------------------

(defun make-activation-record (rune)
  "Creates and returns a new ``Activation-Record'' used for processing
   the RUNE."
  (declare (type Rune rune))
  (the Activation-Record
    (make-instance 'Activation-Record :rune rune)))

;;; -------------------------------------------------------

(defun activation-record-rune (record)
  "Returns the rune associated with the activation RECORD."
  (declare (type Activation-Record record))
  (the Rune
    (slot-value record 'rune)))

;;; -------------------------------------------------------

(defun activation-record-command (record)
  "Returns the currently selected command of the rune associated with
   the activation RECORD."
  (declare (type Activation-Record record))
  (the (or null Command)
    (slot-value record 'command)))

;;; -------------------------------------------------------

(defun activation-record-advance (record)
  "Moves the activation RECORD's instruction pointer to the next command
   of the associated rune, updates the current command, and returns the
   modified RECORD."
  (declare (type Activation-Record record))
  (with-slots (rune ip command) record
    (declare (type (or null Rune)    rune))
    (declare (type fixnum            ip))
    (declare (type (or null Command) command))
    (setf command
      (when (array-in-bounds-p (rune-commands rune) (1+ ip))
        (aref (rune-commands rune) (incf ip)))))
  (the Activation-Record record))

;;; -------------------------------------------------------

(defun activation-record-jump-to-start (record)
  "Moves the activation RECORD's instruction pointer to the first
   command of the associated rune, updates the current command, and
   returns the modified RECORD."
  (declare (type Activation-Record record))
  (with-slots (rune ip command) record
    (declare (type (or null Rune)    rune))
    (declare (type fixnum            ip))
    (declare (type (or null Command) command))
    (setf ip 0)
    (setf command
      (when (array-in-bounds-p (rune-commands rune) ip)
        (aref (rune-commands rune) ip))))
  (the Activation-Record record))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((runes
    :initarg       :runes
    :initform      (make-hash-table :test #'eql)
    :type          rune-table
    :documentation "Associates with each rune ID the respective rune
                    object.")
   (spell
    :initarg       :spell
    :initform      NIL
    :type          (or null Rune)
    :documentation "The rune representing the entry (main) function.")
   (next-vacant-rune-id
    :initarg       :next-vacant-rune-id
    :initform      1
    :type          rune-id
    :documentation "The next rune ID not reserved by any of the RUNES,
                    memorized for the contingent creation of no-op runes
                    by the Runespells command \"Mizi\".")
   (rune-stack
    :initarg       :rune-stack
    :initform      NIL
    :type          (stack-of Rune)
    :documentation "The stack of runes.")
   (records
    :initarg       :records
    :initform      NIL
    :type          (stack-of Activation-Record)
    :documentation "The control stack, a last-in first-out (LIFO)
                    storage maintaining the activation records, with the
                    top member being the currently processed one.")
   (active-record
    :initarg       :active-record
    :initform      NIL
    :type          (or null Activation-Record)
    :documentation "The currently processed activation record, that is,
                    the top element of the RECORDS control stack."))
  (:documentation
    "The ``Interpreter'' class defines a unit responsible for the
     application of effect to a sequence of instructions represented by
     rune definitions."))

;;; -------------------------------------------------------

(defun make-interpreter (definitions)
  "Creates and returns a new ``Interpreter'' which derives its runes
   from the DEFINITIONS."
  (declare (type (list-of Rune-Definition) definitions))
  
  (let ((interpreter (make-instance 'Interpreter)))
    (declare (type Interpreter interpreter))
    
    (with-slots (runes spell next-vacant-rune-id) interpreter
      (declare (type rune-table     runes))
      (declare (type (or null Rune) spell))
      (declare (type rune-id        next-vacant-rune-id))
      
      (loop for definition of-type Rune-Definition in definitions do
        (case (rune-definition-type definition)
          (:rune
            (multiple-value-bind (rune-with-id contains-id-p)
                (gethash (rune-definition-id definition) runes)
              (declare (type (or null Rune) rune-with-id))
              (declare (type T              contains-id-p))
              
              (if contains-id-p
                (error "A rune with the ID ~d does already exist: ~s."
                  (rune-definition-id definition) rune-with-id)
                (setf (gethash (rune-definition-id definition) runes)
                      (make-rune-from-definition definition)))
              
              (setf next-vacant-rune-id
                (max next-vacant-rune-id
                  (rune-definition-id definition)))))
          
          (:spell
            (if spell
              (error "A spell is already defined: ~s." spell)
              (setf spell (make-rune-from-definition definition))))
          
          (otherwise
            (error "Invalid rune definition: ~s." definition))))
      
      ;; The assignment of automatically generated rune IDs shall start
      ;; with the integer number immediately succeeding the largest rune
      ;; ID encountered among the rune definitions.
      (incf next-vacant-rune-id))
    
    (the Interpreter interpreter)))

;;; -------------------------------------------------------

(defun interpreter-push-activation-record (interpreter new-record)
  "Pushes the activation record NEW-RECORD unto the INTERPRETER's
   control stack and returns the modified INTERPRETER."
  (declare (type Interpreter       interpreter))
  (declare (type Activation-Record new-record))
  (with-slots (records active-record) interpreter
    (declare (type (stack-of Activation-Record) records))
    (declare (type (or null  Activation-Record) active-record))
    (push new-record records)
    (setf active-record new-record))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-pop-activation-record (interpreter)
  "Pops the top activation record from the INTERPRETER's control stack
   and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (records active-record) interpreter
    (declare (type (stack-of Activation-Record) records))
    (declare (type (or null  Activation-Record) active-record))
    (pop records)
    (setf active-record (first records))
    (the (or null Activation-Record) active-record)))

;;; -------------------------------------------------------

(defun interpreter-get-rune (interpreter rune-id)
  "Returns the rune associated with the RUNE-ID in the INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type rune-id     rune-id))
  (or
    (gethash rune-id (slot-value interpreter 'runes))
    (error "No rune with the ID ~d can be found." rune-id)))

;;; -------------------------------------------------------

(defun interpreter-remove-rune (interpreter rune-id)
  "Removes the rune associated with the RUNE-ID from the INTERPRETER and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type rune-id     rune-id))
  (remhash rune-id (slot-value interpreter 'runes))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-replace-rune (interpreter ousted-rune-id new-rune-id)
  "Replaces the rune associated with the OUSTED-RUNE-ID in the
   INTERPRETER by the rune associated with the NEW-RUNE-ID and returns
   the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type rune-id     ousted-rune-id))
  (declare (type rune-id     new-rune-id))
  (with-slots (runes) interpreter
    (declare (type rune-table runes))
    (setf (gethash ousted-rune-id runes)
          (gethash new-rune-id    runes)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-push-no-ops-rule (interpreter number-of-no-ops)
  "Creates a new simple rune with an automatically generated and
   assigned rune ID, depending upon no variables, whose body is composed
   of the NUMBER-OF-NO-OPS no-op commands only, pushes it unto the
   INTERPRETER's rune stack, registers it by its identifier, and returns
   the modified INTERPRETER."
  (declare (type Interpreter   interpreter))
  (declare (type (integer 0 *) number-of-no-ops))
  (with-slots (runes next-vacant-rune-id rune-stack) interpreter
    (declare (type rune-table      runes))
    (declare (type rune-id         next-vacant-rune-id))
    (declare (type (stack-of Rune) rune-stack))
    (let ((rune-with-no-ops
            (make-no-ops-rune next-vacant-rune-id number-of-no-ops)))
      (declare (type Rune rune-with-no-ops))
      (setf (gethash next-vacant-rune-id runes) rune-with-no-ops)
      (push rune-with-no-ops rune-stack))
    (incf next-vacant-rune-id))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instructions maintained by the INTERPRETER and returns
   the involved INTERPRETER."
  (declare (type Interpreter interpreter))
  
  (with-slots (spell records active-record rune-stack) interpreter
    (declare (type (or null Rune)               spell))
    (declare (type (stack-of Activation-Record) records))
    (declare (type (or null  Activation-Record) active-record))
    (declare (type (stack-of Rune)              rune-stack))
    
    (when spell
      (interpreter-push-activation-record interpreter
        (make-activation-record spell))
      
      (loop
        for command
          of-type (or null Command)
          =       (activation-record-command active-record)
        while command
        do
          (case (command-identifier command)
            
            ;; Enhance top rune with second top rune and pop both.
            (:Zi
              (let ((top-rune    (pop rune-stack))
                    (second-rune (pop rune-stack)))
                (declare (type Rune top-rune))
                (declare (type Rune second-rune))
                (rune-enhance top-rune second-rune)
                (activation-record-advance active-record)))
            
            ;; Pop and execute top rune.
            (:Yah
              (let ((top-rune (pop rune-stack)))
                (declare (type Rune top-rune))
                (activation-record-advance active-record)
                (interpreter-push-activation-record interpreter
                  (make-activation-record top-rune))))
            
            ;; Pop top rune and remove its definition.
            (:Ni
              (let ((top-rune (pop rune-stack)))
                (declare (type Rune top-rune))
                (rune-disenhance top-rune)
                
                ;; Terminate this activation record if the TOP-RUNE
                ;; constitutes the currently processed one.
                (when (eq top-rune
                          (activation-record-rune active-record))
                  (interpreter-pop-activation-record interpreter))
                
                (interpreter-remove-rune interpreter
                  (rune-id top-rune)))
              (activation-record-advance active-record))
            
            ;; Replace rune by another.
            (:Giyah
              (let ((rune-to-replace (command-argument command))
                    (new-rune-id     (rune-command-count
                                       (pop rune-stack))))
                (declare (type rune-id rune-to-replace))
                (declare (type rune-id new-rune-id))
                (interpreter-replace-rune interpreter
                  (rune-variable-value
                    (activation-record-rune active-record)
                    rune-to-replace)
                  new-rune-id))
              (activation-record-advance active-record))
            
            ;; No-op.
            (:Ne
              (activation-record-advance active-record))
            
            ;; Output ASCII character corresponding to top rune command
            ;; count.
            (:Mizo
              (let ((top-rune (pop rune-stack)))
                (declare (type Rune top-rune))
                (format T "~&~a"
                  (rune-command-count top-rune)))
              (activation-record-advance active-record))
            
            ;; Input no-op tally for new rune.
            (:Mizi
              (format T "~&Please input an ASCII character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (interpreter-push-no-ops-rule interpreter
                  (digit-char-p input)))
              (activation-record-advance active-record))
            
            ;; Push rune designated by ID stored in variable.
            ((:Fa :Rin :Gora :Jyiku :Nahy :Zeha)
              (let ((variable-name (command-identifier command)))
                (declare (type variable-name variable-name))
                (push
                  (interpreter-get-rune interpreter
                    (rune-variable-value
                      (activation-record-rune active-record)
                      variable-name))
                  rune-stack))
              (activation-record-advance active-record))
            
            ;; Duplicate top rune of stack.
            (:Lafi
              (push (first rune-stack) rune-stack)
              (activation-record-advance active-record))
            
            ;; Jump to first command of the rune.
            (:Chizo
              (activation-record-jump-to-start active-record))
            
            ;; Jump to first command of the rune if stack is empty.
            (:Chixo
              (if (null rune-stack)
                (activation-record-jump-to-start active-record)
                (activation-record-jump-to-start active-record)))
            
            ;; Jump to first command of the rune if stack is not empty.
            (:Chiyo
              (if rune-stack
                (activation-record-jump-to-start active-record)
                (activation-record-jump-to-start active-record)))
            
            ;; Execute second-top rune if command count of top rune and
            ;; third-top rune conform.
            (:Nahweh
              (let ((first-rune  (first  rune-stack))
                    (second-rune (second rune-stack))
                    (third-rune  (third  rune-stack)))
                (declare (type Rune first-rune))
                (declare (type Rune second-rune))
                (declare (type Rune third-rune))
                
                (cond
                  ((= (rune-command-count third-rune)
                      (rune-command-count first-rune))
                    (pop rune-stack)
                    ;; Push the third rune to the top for its subsequent
                    ;; removal.
                    (rotatef
                      (first rune-stack)
                      (second rune-stack))
                    (pop rune-stack)
                    (activation-record-advance active-record)
                    (interpreter-push-activation-record interpreter
                      (make-activation-record second-rune)))
                  (T
                    (activation-record-advance active-record)))))
            
            ;; Pop top rune.
            (:Tazi
              (pop rune-stack)
              (activation-record-advance active-record))
            
            ;; Terminate the program.
            (:Gorpdne
              (loop-finish))
            
            ;; Invalid command.
            (otherwise
              (error "Unrecognized command ~s in rune ~s."
                command (activation-record-rune active-record))))
          
          ;; If the current activation record is exhausted, select the
          ;; next lower on the record stack.
          (unless (activation-record-command active-record)
            (interpreter-pop-activation-record interpreter))
          
          ;; If all activation records have been exhausted, terminate
          ;; the program.
          (unless active-record
            (loop-finish)))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Runespells (code)
  "Interprets the piece of Runespells CODE and returns no value."
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

;; Print integer numbers counting up from zero (0) while the user inputs
;; a digit not equal to zero (0).
(interpret-Runespells
  "Rune 1[2, 3]: Rin Fa Zi
   Rune 2[]:
   Rune 3[]: Ne
   Rune 4[]:
   Rune 5[]: Gorpdne
   Spell [1, 2, 4, 5]: Rin Mizo Fa Yah Mizi Jyiku Gora Nahweh Chizo")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-Runespells
  "Rune 1[1]: Fa Yah
   Spell [1]: Fa Yah")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program.
(interpret-Runespells
  "Rune 1[]: Mizi Mizo Chizo
   Spell [1]: Fa Yah")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Concept:
;;   Spell:
;;     Push 1     // Future third top rune for comparison with "Nahweh"
;;                // in input=1?
;;     Push 2     // If "Nahweh" confirms input=1, go to "Rune 2".
;;     Push input // Future first top rune for comparison with "Nahweh"
;;                // in input=1?
;;     
;;     Duplicate input for printing
;;     Output and pop copy of input
;;     If input = 1, go to "Rune 2"
;;     Terminate program
;;   
;;   Rune 2:
;;     Push 1 // Reference to "Rune 1" whose tally of commands (= 1)
;;            // shall be printed infinitely.
;;     Output and pop 1
;;     Repeat unconditionally
;;   
;;   Rune 1:
;;     No-op  // The tally of commands (= 1) is used for printing the
;;            // value "1" to the standard output.
;;   
(interpret-Runespells
  "Rune 1[]:     Ne
   Rune 2[1]:    Fa Mizo Chizo
   Spell [1, 2]: Fa Rin Mizi Lafi Mizo Nahweh Gorpdne")
