;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "VarStack", invented by the Esolang user "ChuckEsoteric08"
;; and presented on April 29th, 2023, the kenspeckle haecceity of whose
;; designment entertains its woning in the statements' conformation,
;; thilk, each such a commorant of its personal line, wist of their
;; partage, governed with a mandate's stringency, into a tripartite
;; format; commencing in an identifying line label, as a prevenience to
;; a single command --- whence ensues the epiphenomenal potential ---,
;; and concluding in the destination label name, the same, in the
;; absence of a traditional seriatim statement traversal mechanism,
;; clepes the next line to sojourn.
;; 
;; 
;; Concept
;; =======
;; The VarStack programming language's caract is edified upon the
;; firmament of an arbitrary account of character-valued stacks, their
;; indagation and manipulation the wike of statement lines thilk intrine
;; an identification label for contingent future references, a command,
;; and a destination label signifying the subsequently to visit line.
;; 
;; == VARSTACK PROGRAMS: LINES SPECIFYING LABEL, COMMAND, AND TARGET ==
;; A VarStack program enumerates zero or more lines, each such an
;; aefauld statement's haft; the same always presents a partage into
;; three components.
;; 
;; A single line's conformation complies to the forbisen:
;; 
;;   lineLabel;command;destinationLabelName
;; 
;; where the components' participations, any among these a mandatory
;; specification, limn the following ordainments' simulacrum:
;; 
;;   (1) lineLabel:
;;       Defines a unique name for this line, intended for its
;;       contingent visitation as a forinsecal or this line's
;;       destination; for which please consult the third compartment
;;       "destinationLabelName".
;;   
;;   (2) command:
;;       The causatum to unconditionally accompass when traversing this
;;       line.
;;   
;;   (3) destinationLabelName:
;;       A reference to the label associated with the next line to
;;       sojourn. VarStack programs do not progress across their
;;       statements in a linear fashion; as such, every statement line's
;;       desinent member ought to clepe the destination to visit. If no
;;       affiliated line can be detected, the program terminates in an
;;       orderly fashion.
;; 
;; == THE MEMORY: CHARACTER-VALUED STACKS IDENTIFIED BY NAMES ==
;; The means of its data castaldy in VarStack's memory are incarnated
;; in a theoretically infinite accompt of stacks, any of these the haft
;; to a bourneless tally of characters.
;; 
;; The stack adit's mode is produced by mediation of the unique names,
;; strings chosen in concord with the developer's deliberations, which
;; at siccan storage unit's inchoation ought to be supplied, and whose
;; constitution is subjected to a few stipulations' impositions only.
;; 
;; == A PAIR ROYALE OF POSSIBLE TERMINATION CONDITIONS EXISTS ==
;; The execution's cessation, in a connotation that incorporates an
;; orderly egress, receives its gendrure from one of the following
;; cases' encheson:
;; 
;;   (1) UNRECOGNIZED LABEL REQUEST:
;;       The control flow, when navigated to a line label not defined
;;       at any location in the program, immediately aborts the
;;       execution.
;;   
;;   (2) UNRECOGNIZED STACK REQUEST:
;;       A behest directed at a stack by an agnomination not partaking
;;       of the contemporaneously defined names ceases the program.
;;   
;;   (3) INEXISTENT STACK ELEMENT ACCESS:
;;       The trial at an empty stack's top element perquisition
;;       (peeking) or removal (popping) conduces a valediction from a
;;       further progress.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical species of conspection's adhibition, a VarStack
;; program's constitution ostends a catena of statements, distributed
;; across lines intended to be construed serelepes; each such woning
;; entreparted into a unique label identifying the line, a mandatory
;; singular command, and the next line label to visit.
;; 
;; == THE PROGRAM: A SEQUENCE OF LINES ==
;; A program in VarStack's construe tallies a catena of zero or more
;; lines, everichon among these, if not governed by vacancy, a single
;; statement's commorancy.
;; 
;; == LINES: COMPOSITIONS OF LABEL, COMMAND, AND JUMP DESTINATION ==
;; A non-blank line's componency, wisting of no exemption, intrines the
;; following constituents, an aefauld semicolon (";") in their
;; interstices naited as a merist:
;; 
;;   lineLabel;command;destinationLabelName
;; 
;; Where the "lineLabel" and "destinationLabel" are mandated to comply
;; with the label names' stipulations, while the ensconced "command"
;; tmema's designment proceeds from a more complex nomothesia.
;; 
;; == LABEL NAMES ==
;; A label's nominative indicium derives from a catena enumerating
;; one or more characters whose homologation is impounded merely by a
;; set of symbols assigned to a purpose of particular significance.
;; 
;; The alow tabular exposition's cynosure shall be the inadmissible
;; characters' nominatim ostention:
;; 
;;   --------------------------------------------
;;   Character | Rationale for interdiction
;;   ----------+---------------------------------
;;   ;         | Instruction components sepiment
;;   ............................................
;;   :         | Instruction argument sepiment
;;   ............................................
;;   =         | Comparison signifier
;;   ............................................
;;   (newline) | Line separator
;;   --------------------------------------------
;; 
;; == STACK NAMES ==
;; No further illumination's requisitum merits its woning in the stack
;; identifier's nomothesia, as their conflation with the adduced label
;; name stipulations, which please consult aboon, confers upon the
;; syntaxis the comfort of an ejusdem generis treatise.
;; 
;; As an adminicular dation, the tabular listing of the interdicted
;; symbols accessible loco citato shall be copied verbatim anew:
;; 
;;   --------------------------------------------
;;   Character | Rationale for interdiction
;;   ----------+---------------------------------
;;   ;         | Instruction components sepiment
;;   ............................................
;;   :         | Instruction argument sepiment
;;   ............................................
;;   =         | Comparison signifier
;;   ............................................
;;   (newline) | Line separator
;;   --------------------------------------------
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form's (EBNF) dation shall limn
;; an enhaused stringency in the language donat's treatise:
;; 
;;   program                 := blankLines
;;                           ,  { innerStatement , blankLines }
;;                           ,  [ lastStatement ]
;;                           ;
;;   
;;   innerStatement          := statementBody , newline ;
;;   lastStatement           := statementBody ;
;;   
;;   statementBody           := labelName
;;                           ,  ";" , command
;;                           ,  ";" , labelName
;;                           ;
;;   
;;   command                 := createCommand
;;                           |  pushCommand
;;                           |  popCommand
;;                           |  printLiteralCommand
;;                           |  printStackCommand
;;                           |  inputCommand
;;                           |  jumpIfEqLiteralCommand
;;                           |  jumpIfNeqLiteralCommand
;;                           |  jumpIfEqStackCommand
;;                           |  swapTopCommand
;;                           |  reverseCommand
;;                           |  moveTopToBottomCommand
;;                           ;
;;   createCommand           := "@" , stackName
;;                           ,  ":" , { identifierChar }
;;                           ;
;;   pushCommand             := "$" , identifierChar
;;                           ,  ":" , stackName
;;                           ;
;;   popCommand              := "!" , stackName ;
;;   printLiteralCommand     := '"' , { character - newline } ;
;;   printStackCommand       := "'" , stackName ;
;;   inputCommand            := "?" , stackName ;
;;   jumpIfEqLiteralCommand  := "#" , stackName
;;                           ,  "=" , identifierChar
;;                           ,  ":" , labelName
;;                           ;
;;   jumpIfNeqLiteralCommand := "!" , stackName
;;                           ,  "=" , identifierChar
;;                           ,  ":" , labelName
;;                           ;
;;   jumpIfEqStackCommand    := "%" , stackName
;;                           ,  "=" , stackName
;;                           ,  ":" , labelName
;;                           ;
;;   swapTopCommand          := "~" , stackName ;
;;   reverseCommand          := "&" , stackName ;
;;   moveTopToBottomCommand  := "^" , stackName ;
;;   
;;   stackName               := identifier ;
;;   labelName               := identifier ;
;;   identifier              := identifierChar , { identifierChar } ;
;;   identifierChar          := character - reservedChar ;
;;   reservedChar            := newline | ";" | ":" | "=" ;
;;   
;;   blankLines              := { newline } ;
;;   newline                 := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; VarStack's instruction set constitutes a duodecimal membership, its
;; competences' compass enumerating foundational stack manipulation and
;; extended rearrangement facilities, input obtention and output
;; issuance, as well as a label-based goto mechanism.
;; 
;; == VARSTACK'S INSTRUCTIONS SUBSUME INTO FOUR SPECIES ==
;; On a coarser stratum, the entirety's twelve specimens may be
;; distributed across a quadruple componency that adhibits a logical
;; coherence to the multuple of heterogeneous conformation:
;; 
;;   (1) BASIC STACK OPERATIONS:
;;       Appertain to the standard stack abstract data type (ADT)
;;       interface competences, the compass of which in particular
;;       amplects the insertion at the stack's top and the removal from
;;       selfsame position.
;;   
;;   (2) STACK REARRANGEMENT OPERATIONS:
;;       Augment the foundational aspects of the stack potential by a
;;       set of features airted at the collection order's manipulation
;;       without its size's alteration.
;;   
;;   (3) INPUT AND OUTPUT OPERATIONS:
;;       Furnish adit to the input and output conduits.
;;   
;;   (4) CONTROL FLOW OPERATIONS:
;;       Capacitate the castaldy of the control flow by conditional
;;       jumping to labels.
;; 
;; A tabular exposition concerning this quadruple membership's diorisms
;; shall be the following equinumerant listing's cynosure.
;; 
;; Please heed that succedaneous tmemata are emphasized by their
;; ensconcement in a jumelle of braces, "{" and "}".
;; 
;;   ------------------------------------------------------------------
;;   (1) BASIC STACK OPERATIONS
;;   ------------------------------------------------------------------
;;   @{name}:{initElements}      | Create stack identified by {name}
;;                               | with incipial content
;;                               | {initialElements}.
;;   ..................................................................
;;   ${newElement}:{stack}       | Push {newElement} onto {stack}.
;;   ..................................................................
;;   !{stack}                    | Pop from {stack}.
;;   ------------------------------------------------------------------
;;   
;;   ------------------------------------------------------------------
;;   (2) STACK REARRANGEMENT OPERATIONS
;;   ------------------------------------------------------------------
;;   ~{stack}                    | Swap {stack}'s two top elements.
;;   ..................................................................
;;   &{stack}                    | Reverse {stack}.
;;   ..................................................................
;;   ^{stack}                    | Move {stack} top to bottom.
;;   ------------------------------------------------------------------
;;   
;;   ------------------------------------------------------------------
;;   (3) INPUT/OUTPUT OPERATIONS
;;   ------------------------------------------------------------------
;;   "{character}                | Print {character}.
;;   ..................................................................
;;   '{stack}                    | Print {stack}.
;;   ..................................................................
;;   ?{stack}                    | Push user input on {stack}.
;;   ------------------------------------------------------------------
;;   
;;   ------------------------------------------------------------------
;;   (4) CONTROL FLOW OPERATIONS
;;   ------------------------------------------------------------------
;;   #{stack}={guard}:{target}   | Jump to {target} label if top of
;;                               | {stack} equals {guard}.
;;   ..................................................................
;;   !{stack}={guard}:{target}   | Jump to {target} label if top of
;;                               | {stack} does not equal {guard}.
;;   ..................................................................
;;   %{stack1}={stack2}:{target} | Jump to {target} label if top of
;;                               | {stack1} equals top of {stack2}.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity's communication with the operative aspects
;; shall constitute the following tabular illustration's dever.
;; 
;; Please heed the demarcation of succedaneous parcels by a catena of
;; asterisks ("*"), their occupied dispansion's purpose that of a
;; supersession via actual VarStack code tmemata in the ultimate
;; program.
;; 
;;   ------------------------------------------------------------------
;;   Command            | Effect
;;   -------------------+----------------------------------------------
;;   @name:initElements | Creates a new stack amenable to the {name},
;;    **** ************ | containing the inicipial items specified by
;;                      | {initElements}.
;;                      |----------------------------------------------
;;                      | {name} must be a stack name.
;;                      |----------------------------------------------
;;                      | {initElements} must be a sequence of zero or
;;                      | more character, each of these serelepes
;;                      | state a symbol to insert into the stack, the
;;                      | ordonnance in the listing being replicated
;;                      | on the stack from its top to the bottom;
;;                      | that is, the {initElements} are pushed in the
;;                      | reverse order, the final stack top's item
;;                      | being identical to the leftmost initial
;;                      | element.
;;                      |----------------------------------------------
;;                      | In a pseudocode diction, the following holds:
;;                      |   let newStack  <- prepareNewStack(name)
;;                      |   let elemCount <- length(initialElements)
;;                      |   for elemIndex from elemCount down to 1 do
;;                      |     newStack.push(initElements[elemIndex])
;;                      |   end for
;;   ..................................................................
;;   $element:stack     | Pushes the {element} onto the {stack}'s top.
;;    ******* *****     |----------------------------------------------
;;                      | {element} must be a character.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ..................................................................
;;   !stack             | Pops the {stack}'s top element and discards
;;    *****             | thilk.
;;                      |----------------------------------------------
;;                      | If the {stack} is empty, the program
;;                      | immediately halts.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ==================================================================
;;   ~stack             | Swaps the positions of the two top elements
;;    *****             | on the {stack}.
;;                      |----------------------------------------------
;;                      | If the {stack} is empty, the program
;;                      | immediately halts.
;;                      |----------------------------------------------
;;                      | If the {stack} contains a single element
;;                      | only, no causatum applies.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ..................................................................
;;   &stack             | Reverses the order of elements stored in the
;;    *****             | {stack}.
;;                      |----------------------------------------------
;;                      | If the {stack} is empty, no causatum applies.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ..................................................................
;;   ^stack             | Relocates the top {stack} element to its
;;    *****             | bottom.
;;                      |----------------------------------------------
;;                      | If the {stack} is empty, the program
;;                      | immediately halts.
;;                      |----------------------------------------------
;;                      | If the {stack} contains a single element
;;                      | only, no causatum applies.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ==================================================================
;;   "argument          | Prints the argument to the standard output.
;;    ********          |----------------------------------------------
;;                      | {argument} must be a sequence of zero or more
;;                      | characters.
;;   ..................................................................
;;   'stack             | Prints all elements comprehended in the
;;    *****             | {stack}, proceeding from its top to its
;;                      | bottom, to the standard output.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ..................................................................
;;   ?stack             | Queries the standard input for a single
;;    *****             | character and pushes thilk onto the {stack}.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;   ==================================================================
;;   #stack=guard:label | Peeks without removing the {stack}'s top
;;    ***** ***** ***** | element; if this item equals the {guard},
;;                      | relocates the instruction pointer (IP) to the
;;                      | position of the {label}'s definition.
;;                      | Otherwise proceeds as usual.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;                      |----------------------------------------------
;;                      | {guard} must be a character.
;;                      |----------------------------------------------
;;                      | {label} must be a label name.
;;   ..................................................................
;;   !stack=guard:label | Peeks without removing the {stack}'s top
;;    ***** ***** ***** | element; if this item does not equal the
;;                      | {guard}, relocates the instruction pointer
;;                      | (IP) to the position of the {label}'s
;;                      | definition. Otherwise proceeds as usual.
;;                      |----------------------------------------------
;;                      | {stack} must be a stack name.
;;                      |----------------------------------------------
;;                      | {guard} must be a character.
;;                      |----------------------------------------------
;;                      | {label} must be a label name.
;;   ..................................................................
;;   %stac1=stac2:label | Peeks without removing the top of the stack
;;    ***** ***** ***** | {stac1} and likewise the top of the stack
;;                      | {stac2}; if these top elements are equal,
;;                      | relocates the instruction pointer (IP) to the
;;                      | position of the {label}'s definition.
;;                      | Otherwise proceeds as usual.
;;                      |----------------------------------------------
;;                      | {stac1} must be a stack name.
;;                      |----------------------------------------------
;;                      | {stac2} must be a stack name.
;;                      |----------------------------------------------
;;                      | {label} must be a label name.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been exercised in the
;; programming language Common Lisp, for its operations prepending the
;; evaluation stage with a transcription of the program lines into
;; dedicated statement objects, intended as a parasceuastic investement
;; whose sequela homologates a more comfortable processing of the
;; complex statements.
;; 
;; The most peisant contribution, however, to whom a commorancy inwith
;; this project serves a vouchsafement is nevened in the "META" parser's
;; participation, a compound endowed with both bailiwicks of a lexical
;; analyzer (lexer), as a necessary parergon, and, in a paravaunt
;; agency, a recursive-descent parser, their reification's gendrure
;; ensues from the requisite Common Lisp source code's assemblage via
;; macros, rather than the coefficiency accoutred by immediate
;; functions.
;; 
;; For the treatise dedicated to this technique's elucidation, please
;; exercise your conspectuity on the section alow, endowed with the
;; euonym "META", as well as, partially and ordained in the wike of
;; supererogation, in the consequent "Appendices".
;; 
;; 
;; META
;; ====
;; META represents a technique airted at a recursive descent parser's
;; assemblage, its distinguishment's proprium that of the requisite
;; code's generation via a series of macros, aided in this endeavor by
;; dedicated "META" expression encapsulation structures, concluding in
;; the thus constructed behests' application onto the optated Common
;; Lisp object --- most commonly, but not necessarily, a source string.
;; 
;; == THIS TREATISE'S CONSTITUTION ==
;; The treatise in its entirety may receive an entreparting into the
;; following logical compartments, whose enumeration conflates exactly
;; with the consequent sections' ordonnance:
;; 
;;   (1) What are META's goals?
;;       The purpose vindicating the META technique's gendrure will
;;       receive its condeign elucidations.
;;   
;;   (2) How does a typical META invocation appear?
;;       An exemplary code tmema, intended as a forbisen for a META
;;       parser utilization, demonstrates the foundational design.
;;   
;;   (3) What constitutes META's unique diorism?
;;       The propria edifying the provenance of META's discrepancy from
;;       common parsers will be enumerated.
;;   
;;   (4) How does META achieve its objectives?
;;       With the objective of compendiousness, the commodities
;;       appurtenancing META with the appuis for its onuses'
;;       satisfaction will be bewrayed, including in particular two
;;       species of macros. The thus begotten inchoate gnarity shall
;;       administer a parasceve to the later sections, with special
;;       regard the points -> (7) and -> (8), inwith those bailiwick
;;       are immersed the concrete implementation details.
;;   
;;   (5) How does META compare to regular expressions?
;;       Those aspects that serve in the alligation betwixt META and
;;       widespread regular expressions, as well as such acting in the
;;       agency of the twissel's merists, shall be reproduced.
;;   
;;   (6) Which META language constructs are defined and how do they
;;       correspond to Common Lisp?
;;       The object-oriented design to whom the reification of META's
;;       theoretical potentials to actual entelechia is assigned, with
;;       a special cynosure on the representative classes, enjoy their
;;       woning in this locality. A fortiori, the feelefold META
;;       constructs and their generalized expressions are mapped to
;;       tangible classes.
;;   
;;   (7) What code fragments do the META constructs generate?
;;       The champarty in which functions, macros, and classes announce
;;       their engagement with the purpose of the parser code's
;;       assemblage express their statements in this parcel. The thus
;;       established dispansion, however, does not yet encompass the
;;       kenspeckle regular-expression-like syntaxis commorant in META;
;;       the accommodation for this aspect shall be the succeeding
;;       point -> (8).
;;   
;;   (8) How do reader macros form the regular-expression-like syntax?
;;       The definitions of reader macros thilk install a bespoke
;;       syntax, in its designment and syntomy an aspirant of regular
;;       expressions, are capacitated for their extraction.
;; 
;; == A PROEM CONCERNING THE IMPLEMENTATION'S DIORISMS ==
;; This treatise shall not be encumbered with the wite of lapsing in
;; certain peisant aspects' rubrication, inwith whose perimeter
;; deviations and extensions of the original META principle germinate:
;; 
;;   (1) REDUCTION TO STRINGS
;;       Maugre the versality whose inherence in META redes the same
;;       meritous of plaudit, a compass not curtailed of lists and
;;       other forms as a compernage to the traditional string
;;       processing, this treatise's cynosure, as a consilience with the
;;       project's implementation, constricts itself to the latter
;;       subject only, scilicet, character sequences as the parsing
;;       effort's material.
;;   
;;   (2) RAMOSITY IN THE META OBJECT HIERARCHY
;;       A META expression's physical manifestation in Baker's
;;       implementation [baker1991prag] emerges in a single structure
;;       class' guise, employing as the requisite form's enscapsulation
;;       a field of a generic type, and for the discriminating criterion
;;       in the behavior's selection a sentinel character.
;;       
;;       The implementation deployed in this project, on the other hand,
;;       attends to a ramosity endowed with an elevated intricacy, by
;;       establishment of a dedicated class for each member of the
;;       quintuple META constructs. Two ultimities' emergences may be
;;       attested in this approach; imprimis, each ensconcing class
;;       bears the contingency for its specific form's concrete
;;       delineation, in the tally and types appertaining to such. As an
;;       example adduced, the META sequence and alternative constructs
;;       expect a list enumerating zero or more forms; a type test's
;;       reliance registers a twissel; while repetition and evaluation
;;       do not mandate an accompt aboon an aefauld membership.
;;       
;;       A second behoovable chevisance parlays of the capacity for
;;       dispatchments on the discrepant classes as succedanea to the
;;       original choice's case switch by the affiliated character slot.
;;   
;;   (3) DISCREPANCY IN THE INTERFACE
;;       A twain of deviations in conjunction with the nominal and
;;       interface subjects serve as kenspeckle merists from the
;;       provenance's design.
;;       
;;       Anent the agnomination's choice, a preponderance among the
;;       functions' and macros' monikers experiences a reformulation,
;;       in their caract both latreutical as the fulfilled wikes'
;;       more lucid significations, and, an overthwart to a bote, a
;;       fount to an enhaused nimiety's incursion. As a forbisen, the
;;       entry point, the macro "matchit", transforms through a change
;;       in its stevening to "match-expression".
;;       
;;       In the pertinence of its structural digression from the source,
;;       this implementation assigns as a concomitant to every macro a
;;       function whose notion realizes the underlying operative
;;       principles' furnishment; for instance, the original "matchit"
;;       macro, founded upon a type switch, in this design distributes
;;       across the implementations of the generic function
;;       "compile-expression", covering the selfsame variety of
;;       character, string, and META expression species.
;; 
;; == META'S TELOS: A PARSER OSTENDING A COMPENDIOUS SYNTAX ==
;; The telos comprising META's encheson of pursuit appertains to a
;; scannerless parser, one whose lexical analyzer (lexer) establishes
;; an item of the conjoined haecceity, the efforts of which capacitate
;; the same with its wikes' attendance, while a compendiousness limining
;; an approximation to the regular expressions' acquaintance designs a
;; paravaunt obbligato.
;; 
;; In an eath diction's conveyance, the META concept allows a parser
;; to be constructed utilizing a syntax desumed from regular
;; expressions, however, inserted directly into the surrounding Common
;; Lisp program, rather than communicated as a string of directives.
;; 
;; == META'S DESIGN: AN EXAMPLE ==
;; The alow forbisen shall furnish an endeictic vallidom to the META's
;; traditional bailiwicks. A simplistic demonstration, this parser
;; accepts a single digit, either unsigned or preceded by a plus "+" or
;; minus "-" symbol.
;; 
;; The first code tmema ostends the invocation of "match-expression" in
;; a form maintaining fealty to its pursued syntomy:
;; 
;;   (let ((sign              +1)
;;         (current-character NIL))
;;     (declare (type (member +1 -1)      sign))
;;     (declare (type (or null character) current-character))
;;     
;;     ;; Match a single signed or unsigned digit.
;;     (match-expression
;;       [ { #\+ [#\-! (setf sign -1)] !T }
;;         @(digit current-character) ])
;;     
;;     (print (list sign current-character)))
;; 
;; The aboon example's second rendition, maintaining owelty in all
;; aspects, introduces a few opiferous descants in the
;; "match-expression" segment:
;; 
;;   (let ((sign              +1)
;;         (current-character NIL))
;;     (declare (type (member +1 -1)      sign))
;;     (declare (type (or null character) current-character))
;;     
;;     ;; Match a single signed or unsigned digit.
;;     (match-expression
;;       [
;;         {
;;           ;; If a plus sign follows, consume it without further
;;           ;; epiphenomena, as the default "sign" variable's value
;;           ;; already equals +1.
;;           #\+
;;           
;;           ;; If a minus sign follows, consume it and set the "sign"
;;           ;; variable to -1.
;;           [
;;             #\-
;;             !(setf sign -1)
;;           ]
;;           
;;           ;; If no sign follows, accept the optional sign's absence
;;           ;; and progress without consuming the probed character.
;;           !T
;;         }
;;         
;;         ;; Expect a single digit and store it in the
;;         ;; CURRENT-CHARACTER.
;;         @(digit current-character)
;;       ]
;;     )
;;     
;;     (print (list sign current-character)))
;; 
;; Finally, a tantamount to the META parser's compendious form, the same
;; is molded by several reader macros' adminiculum, shall be adduced.
;; Please heed the complex expression commencing with the "and" form:
;; 
;;   (let ((sign              +1)
;;         (current-character NIL))
;;     (declare (type (member +1 -1)      sign))
;;     (declare (type (or null character) current-character))
;;     
;;     ;; Match a single signed or unsigned digit without naiting the
;;     ;; compendious META syntax.
;;     (and (or (match-content #\+)
;;              (and (match-content #\-)
;;                   (setf sign -1))
;;              T)
;;          (match-type digit current-character))
;;     
;;     (print (list sign current-character)))
;; 
;; == META'S DIORISMS: PROPRIA DIVERGING FROM TRADITIONAL PARSERS ==
;; A set comprehending several diorisms appertains to this species of
;; solution:
;; 
;;   (1) META ASSEMBLES THE PARSER CODE VIA MACROS:
;;       META proceeds by the recursive descent parser code's
;;       assemblage, ere thilk's adhibition, rather than the
;;       necessitated functions' definitions and invocations, as would
;;       be a common solution's approach.
;;       
;;       This entelechy stems from the Common Lisp macro concept's
;;       potentials, acting in an agency cognate to source code
;;       "templates", these, at the intended instant's execution,
;;       actuate their efficacy, employing the succedaneous sections
;;       as dynamically reacting constituents.
;;       
;;       Macros, expressed in lucid verbs, build Common Lisp code,
;;       while functions present, as in any other programming language,
;;       a computed result.
;;   
;;   (2) META PROVIDES A REGULAR-EXPRESSION-LIKE SYNTAX:
;;       The product's syntomy as an express bourne partaking of this
;;       concept's haecceity experiences its communication via a
;;       particular species of macros: the reader macros.
;;       
;;       As a divergent faculty, these constructs in their potentiation
;;       achieve a bespoke language's immediate insertion into the
;;       circumambient code, rather than as an advenient and, in its
;;       design, forinsecal string object. As a corollary, the mimicry
;;       of regular expressions becomes part of the Common Lisp syntax
;;       itself.
;;   
;;   (3) META'S CONCEPTS EXTEND TO ANY SPECIES OF SOURCE:
;;       As counterdistinguished from the traditional aspect of a
;;       parser's provenance, META's potency does not thole a
;;       constriction to strings as the sole subject of a parsing
;;       effort: The abstract nature of this technique homologates an
;;       extension across any Common Lisp form recluded to the
;;       implementor's apprehension.
;;       
;;       This allowance encompasses, without the claim of exhaustion,
;;       lists and numeric objects. The versatility embodied in this
;;       Procrustean mode of application enables, for instance, the
;;       parsing of Common Lisp argument lists themselves via a
;;       dedicated META parser implementation.
;;       
;;       The elevated capacitation's commorancy emerges from the META
;;       parser's coefficient engagement with reader macros, the same
;;       permit the substitution of the Common Lisp reader for a bespoke
;;       lexical analyzer and parser solution. In some sense, a reader
;;       macro halts the infrastructure apportioned the responsibility
;;       of the Lisp source code's consumption and interpretation and
;;       consigns this onus to the programmer's own castaldy. The code
;;       tmema produced by the developer-supplied reader macro callback
;;       function is subsequently invoked as if having naturally been
;;       part of the overall program.
;; 
;; == META'S WARKLUMES: MACROS THAT ASSEMBLE AND CURTAIL CODE ==
;; A twifold agency befalls the available Common Lisp macro variants'
;; competences and dispands its necessitation across an equinumerant
;; membership from thilk; scilicet, imprimis, general macros for the
;; recursive descent parser code's assemblage on one hand, and reader
;; macros for the bespoke regular-expression-like syntaxis' provision
;; on the other.
;; 
;; More details in an exposition shall be furnished by this epexegetical
;; listing:
;; 
;;   (1) GENERAL MACROS ASSEMBLE THE PARSER CODE:
;;       General macros are ordained to the recursive descent parser
;;       code's assemblage, the same ultimately produces an interplay of
;;       source string element accesses, character equality tests, type
;;       comparisons, logical combinations involving "and" as well as
;;       "or", and iterances.
;;       
;;       Maintaining fealty with the highest mete of comprehensiveness'
;;       tenability, macros define substitutions by replacing one form
;;       by an arbitrary account of other forms. Pursuing the covergence
;;       with our particular case, expressed via the "defmacro"
;;       facility, a macro limns a tantamount of a function that does
;;       not evaluate its arguments and which assembles and returns a
;;       program, contingently enriched with succedaneous fragments,
;;       which subsequently is executed.
;;       
;;       In an eather diction, while a function yields a resulting
;;       value, a macro produces a program that, after its execution,
;;       yields a resulting value.
;;       
;;       META's deployment of macros aspires the production of a
;;       parser's requisite instruction sequence by the assemblage of
;;       several code tmemata, each a dedicated macro's contribution.
;;   
;;   (2) READER MACROS PROVIDE A REGULAR-EXPRESSION-LIKE SYNTAX:
;;       Reader macros act in an opiferous modus as the warklumes for
;;       the syntactical syntomy's accomplishment, thilk encompasses
;;       the mimicry of regular expressions.
;;       
;;       Counterdistinguished from the preponderantly dispensed
;;       consuetude of such expressions' conveyance in a string form,
;;       the reader macro capacitates the Common Lisp syntaxis'
;;       veridicous modulation, whence ensues the possibility to insert
;;       META's variation of this "regular expression" immediately into
;;       the code in the circumambiency, vanquishing the discrepancy
;;       betwixt the language's autochthonous and META's adscititious
;;       provisions.
;; 
;; == A PROEM: META'S SYNTAXIS AND COMPETENCES ==
;; A inchoation of gnarity's conveyance concerning META's syntax and
;; competences shall be the following tabular interlude; their
;; spatial allocation in particular vindicated as a parasceuastic
;; accoutrement prevenient to the juxtaposition with the regular
;; expression design.
;; 
;; Please heed that succedaneous content is demarcated by a catena of
;; asterisks ("*") limning an underline.
;; 
;;   ------------------------------------------------------------------
;;   META construct   | Role         | Purpose
;;   -----------------+--------------+---------------------------------
;;   [ f1 f2 ... fN ] | sequence     | Expects all forms, F1 through
;;     ** ** *** **   |              | FN, to succeed in this order.
;;   ..................................................................
;;   { a1 a2 ... aN } | alternatives | Expects at least one form among
;;     ** ** *** **   |              | A1 through AN to succeed.
;;   ..................................................................
;;   $ form           | repetition   | Repeats the Common Lisp FORMS
;;     ****           |              | zero or more times.
;;   ..................................................................
;;   @( type place )  | type test    | Tests the current source
;;      **** *****    |              | character for its compliance a
;;                    |              | TYPE specifier and, if positive,
;;                    |              | stores the character in the
;;                    |              | PLACE.
;;   ..................................................................
;;   ! form           | evaluation   | Executes an arbitrary piece of
;;     ****           |              | Common Lisp code, the FORM, and
;;                    |              | returns its value.
;;   ------------------------------------------------------------------
;; 
;; Upon an epexegesis desideration, the following two entries, for
;; character and string literals, may valorize the compendious dation.
;; 
;; Please heed that succedaneous content is demarcated by a catena of
;; asterisks ("*") limning an underline.
;; 
;;   ------------------------------------------------------------------
;;   Expression       | Role         | Purpose
;;   -----------------+--------------+---------------------------------
;;   character        | literal      | Expects the current source
;;   *********        | character    | character to comply with the
;;                    |              | specified CHARACTER.
;;   ..................................................................
;;   string           | literal      | Expects the character sequence
;;   ******           | string       | commencing at the current source
;;                    |              | position to comply with the
;;                    |              | specified STRING.
;;   ..................................................................
;;   [ f1 f2 ... fN ] | sequence     | Expects all forms, F1 through
;;     ** ** *** **   |              | FN, to succeed in this order.
;;   ..................................................................
;;   { a1 a2 ... aN } | alternatives | Expects at least one form among
;;     ** ** *** **   |              | A1 through AN to succeed.
;;   ..................................................................
;;   $ form           | repetition   | Repeats the Common Lisp FORMS
;;     ****           |              | zero or more times.
;;   ..................................................................
;;   @( type place )  | type test    | Tests the current source
;;      **** *****    |              | character for its compliance a
;;                    |              | TYPE specifier and, if positive,
;;                    |              | stores the character in the
;;                    |              | PLACE.
;;   ..................................................................
;;   ! form           | evaluation   | Executes an arbitrary piece of
;;     ****           |              | Common Lisp code, the FORM, and
;;                    |              | returns its value.
;;   ------------------------------------------------------------------
;; 
;; == CORRELATION BETWIXT META AND REGULAR EXPRESSIONS ==
;; Regular expressions as an entheus to the META technique's aesthetics,
;; despite the presence of a discrepancy constructed by both META's
;; failure to accomplish an equipollence with its inspiration, and
;; several dioristic features' absence along the overthwart airt, are
;; merited a further effort of juxtaposition:
;; 
;;   ------------------------------------------------------------------
;;   META construct   | Role         | Regular expression
;;   -----------------+--------------+---------------------------------
;;   [ f1 f2 ... fN ] | sequence     | sequence
;;   ..................................................................
;;   { a1 a2 ... aN } | alternatives | union
;;   ..................................................................
;;   $ form           | repetition   | Kleene star
;;   ..................................................................
;;   @( type place )  | type test    | (none)
;;   ..................................................................
;;   ! form           | evaluation   | (none)
;;   ------------------------------------------------------------------
;; 
;; == META IS RANKED ALOW THE POTENTIALS OF REGULAR EXPRESSIONS ==
;; Maugre its status as one blemished by an eloigment from the entire
;; competences embodied in regular expressions, META's capacitation lays
;; its amplection around a rather extensively measured set of solutions
;; to common problems, including, without an exhaustive potential's
;; claim, the parsing of integer and floating-point numbers and Common
;; Lisp's own lambda lists.
;; 
;; == FROM PRACTICAL META CONSTRUCTS TO ABSTRACT META EXPRESSIONS ==
;; The necessity to accommodate a linguistic reference to the
;; regular-expression-like constructs whose collaboration edifies META's
;; operative contingencies conditions a hafted terminology's
;; introduction for these five warklumes, the abstract
;; "META expressions", the same, in conjunction with their represented
;; concepts, shall be ostended alow:
;; 
;;   ------------------------------------------------------------------
;;   META construct   | Role                | META expression
;;   -----------------+---------------------+--------------------------
;;   [ f1 f2 ... fN ] | and,                | META sequence
;;                    | concatenation,      | 
;;                    | sequence            | 
;;   ..................................................................
;;   { a1 a2 ... aN } | or,                 | META alternative
;;                    | alternative         | 
;;   ..................................................................
;;   $ form           | repeat/while,       | META repetition
;;                    | Kleene star,        | 
;;                    | zero or more times  | 
;;   ..................................................................
;;   @( type place )  | type test           | META type test
;;   ..................................................................
;;   ! form           | evaluation          | META evaluation
;;   ------------------------------------------------------------------
;; 
;; == EACH META CONSTRUCT CORRESPONDS TO A LISP CONTROL STRUCTURE ==
;; From the nortelry about the META parser's purpose as a transcription
;; originating in a peculiar syntax and concluding in a tantamount
;; Common Lisp code tmema's yield, a juxtaposition betwixt the former,
;; this being META constructs, and the latter's standard Common Lisp
;; functions and macros, may be established.
;; 
;; Please heed that neither are this owelties measured aboon a
;; contingency for the implementation, one of several possible
;; solutions, nor do they necessarily impose a physical consanguinity
;; in the resulting Common Lisp output.
;; 
;;   ------------------------------------------------------------------
;;   META construct   | Role         | Modeled Common Lisp
;;                    |              | control structure
;;   -----------------+--------------+---------------------------------
;;   [ f1 f2 ... fN ] | sequence     | AND
;;   ..................................................................
;;   { a1 a2 ... aN } | alternatives | OR
;;   ..................................................................
;;   $ form           | repetition   | DO
;;                    |              | LOOP WHILE
;;   ..................................................................
;;   @( type place )  | type test    | TYPEP
;;   ..................................................................
;;   ! form           | evaluation   | EVAL
;;   ------------------------------------------------------------------
;; 
;; == META CONSTRUCTS AND THEIR CAUSATA ==
;; META's approximate equiparation with regular expressions furnishes
;; the provenance of its reader macro syntaxis' affiliation with the
;; corresponding META expression constructs' propria.
;; 
;; The quintuple of syntactical diorisms whose champarty conditions the
;; META parser's kenspeckle syntomy shall accompt for the following
;; tabulation's cynosure.
;; 
;; Please heed that succedaneous content is demarcated by a catena of
;; asterisks ("*") limning an underline.
;; 
;;   ------------------------------------------------------------------
;;   META construct   | Role
;;   -----------------+------------------------------------------------
;;   [ f1 f2 ... fN ] | SEQUENCE: Succeeds if all of its members, F1
;;     ** ** *** **   | through FN, probed in the specified order,
;;                    | match; otherwise fails.
;;                    |------------------------------------------------
;;                    | An empty sequence complex always succeeds.
;;                    |------------------------------------------------
;;                    | F1 through FN designate a series of zero or
;;                    | more arbitrary forms.
;;   ..................................................................
;;   { a1 a2 ... aN } | ALTERNATIVE: Succeeds if any of its options,
;;     ** ** *** **   | A1 through AN, probed in the specified order,
;;                    | matches; otherwise fails.
;;                    |------------------------------------------------
;;                    | An empty alternative complex always fails.
;;                    |------------------------------------------------
;;                    | A1 through AN designate a series of zero or
;;                    | more arbitrary forms.
;;   ..................................................................
;;   $ form           | KLEENE STAR: Always succeeds, matching the
;;     ****           | FORM zero or more times while its evaluated
;;                    | result is non-``NIL''; concludes if the ``NIL''
;;                    | value is produced by the FORM.
;;                    |------------------------------------------------
;;                    | The FORM may be any form.
;;   ..................................................................
;;   @( type place )  | TYPE TEST: Succeeds if the currently probed
;;      **** *****    | source element complies with the TYPE
;;                    | specifier, upon confirmation storing the
;;                    | indagated element in the PLACE; otherwise fails
;;                    | without any assignment.
;;                    |------------------------------------------------
;;                    | The TYPE must be a valid type specifier,
;;                    | connable for a fathom via the "typep" function.
;;                    |------------------------------------------------
;;                    | The PLACE must be a ``setf''-able place.
;;   ..................................................................
;;   ! form           | EVALUATION: Evaluates the FORM and succeeds if
;;     ****           | the same produces a non-``NIL'' result;
;;                    | otherwise fails.
;;                    |------------------------------------------------
;;                    | The FORM may be any form.
;;   ------------------------------------------------------------------
;; 
;; == META CONSTRUCTS AND THE LISP CODE THEY GENERATE ==
;; Conceived similiter in its objectives to reader macros, whose
;; capacitation's entelechy realizes in a custom syntaxis' manual
;; conversion into valid Common Lisp code, the five META-introduced
;; specimens in their ultimity pursue the telos of their compendious
;; construction's transliteration into the responsible META expressions.
;; 
;; The following tabulation's dation involves a treatise on the Lisp
;; code tmemata produced by each such reader macro.
;; 
;; Please heed that succedaneous content is demarcated by a catena of
;; asterisks ("*") limning an underline.
;; 
;;   ------------------------------------------------------------------
;;   META construct   | Generated Common Lisp code
;;   -----------------+------------------------------------------------
;;   [ f1 f2 ... fN ] | Generates and returns a Common Lisp code tmema
;;     ** ** *** **   | which evaluates the forms F1 through FN,
;;                    | returning, if all match, the desinent form's
;;                    | result as a mark of success; otherwise, for the
;;                    | first failing member, the traversal immediately
;;                    | aborts with a ``NIL'' return value as the
;;                    | failure's signification.
;;   ------------------------------------------------------------------
;;   { a1 a2 ... aN } | Generates and returns a Common Lisp code tmema
;;     ** ** *** **   | which probes the forms A1 through AN, returning
;;                    | the result of first among these alternatives
;;                    | which produces a non-``NIL'' answer; if all
;;                    | options fail, the abortive ``NIL'' value is
;;                    | returned.
;;   ..................................................................
;;   $ form           | Generates and returns a Common Lisp code tmema
;;     ****           | which repeatedly evaluates the FORM until the
;;                    | the same produces a ``NIL'' result, and finally
;;                    | responds with a ``boolean'' value of ``T'' as
;;                    | a constant signification of success.
;;                    |------------------------------------------------
;;                    | In corollary, this META expression always
;;                    | succeeds, even if the FORM matches zero times.
;;   ..................................................................
;;   @( type place )  | Generates and returns a Common Lisp code tmema
;;      **** *****    | which probes the source string's current
;;                    | character for its compatibility with the TYPE
;;                    | specifier, on confirmation storing this subject
;;                    | of the docimasy in the PLACE, and finally
;;                    | producing a ``boolean'' value of ``T''; in the
;;                    | case of the type indagation's failure, a
;;                    | ``NIL'' return value is communicated.
;;   ..................................................................
;;   ! form           | Evaluates the FORM and returns its result.
;;     ****           |------------------------------------------------
;;                    | Whether this META expression shall be construed
;;                    | as matching or failing depends on the FORM's
;;                    | return value, with a non-``NIL'' output limning
;;                    | the positive case's tantamount, and the ``NIL''
;;                    | output the abortive counterpart.
;;                    |------------------------------------------------
;;                    | This META expression homologates the
;;                    | introduction of epiphenomena in the parsing
;;                    | emprise.
;;   ------------------------------------------------------------------
;; 
;; == EACH META EXPRESSION IS MOLDED INTO A COMMON LISP CLASS ==
;; In a mode agnostic to the programming language's splanchnic hyle,
;; the quintuple avail of META constructs may be colocated to abstract
;; concepts, chosen in this treatise inwith the terminology of
;; "META expressions". Perambulating in the athwart direction, a diction
;; compernage to the highest echelon of reification, the Common Lisp
;; implementation itself, parlays of the manifesting "META classes".
;; 
;; The vincula betwixt the META *constructs*, whose deployment in the
;; program in this exact form entalents our optations, their abstract
;; notions as META *expressions*, and the implementation in Common Lisp
;; as META *classes* shall limn the following tabulation's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   META construct   | META expression   | Implementing Lisp class
;;   -----------------+-------------------+----------------------------
;;   [ f1 f2 ... fN ] | META sequence     | META-Sequence
;;   ..................................................................
;;   { a1 a2 ... aN } | META alternative  | META-Alternative
;;   ..................................................................
;;   $ form           | META repetition   | META-Repetition
;;   ..................................................................
;;   @( type place )  | META type test    | META-Type-Test
;;   ..................................................................
;;   ! form           | META evaluation   | META-Evaluation
;;   ------------------------------------------------------------------
;; 
;; == META'S OPERATIONS: THREE FUNCTIONS AND THREE MACROS ==
;; A sextuple contingent for operative warklumes, its partage that of
;; antilibration into a treble moeity of functions and an equinumerant
;; accompt of macros, the latter actually only invoke the former,
;; exhausts the META parser's foundational interface.
;; 
;; The six definitions shall be adduced alow:
;; 
;; compile-expression (expression : character)
;; compile-expression (expression : string)
;; compile-expression (expression : META-Object)
;;   A function which generates the Common Lisp code for testing the
;;   current input source character, or a sequence of character
;;   commencing at the current position into the source, against a
;;   desired character or string; or which, in the case of a
;;   META-OBJECT, probes its compliance with the source content
;;   starting at the current position.
;;   
;;   Upon a match's confirmation, this code accompasses the epiphenomena
;;   defined in the actuated macros "match-content" or "match-type", or
;;   any causata produced by the META-Object's inner workings, which
;;   might, as a concomitant to the prevenient twain, involve the
;;   "compile-expression" function's recursive invocation. In any case,
;;   the thus produced code fragment returns a generalized boolean
;;   value of "true".
;;   
;;   If no success could be attested, the code tmema incorporates the
;;   presentation of a generalized boolean value of "false".
;;   
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; match-expression (expression)
;;   A macro which invokes the function "compile-expression" with its
;;   EXPRESSION as the sole argument and returns the code fragment
;;   produced by the same function.
;; 
;; --------------------------------------------------------------------
;; 
;; generate-content-match-code (expected-content : character)
;; generate-content-match-code (expected-content : string)
;;   A function which generates the Common Lisp code for testing the
;;   the current input source character, or a sequence of characters
;;   commencing from the current one, against a desired character or
;;   string.
;;   
;;   Upon the equality's confirmation, this code advances beyond the
;;   probed character or character sequence in the source, while
;;   returning a generalized boolean value of "true".
;;   
;;   If no match could be confirmed, the source's position cursor does
;;   not move, while the generated code delivers a generalized boolean
;;   value of "false".
;;   
;;   This function is utilized by the higher-level "match-content"
;;   macro, which please see.
;; 
;; match-content (expected-content)
;;   A macro which invokes the function "generate-content-match-code"
;;   with its EXPECTED-CONTENT as the sole argument and returns the
;;   code fragment produced by the same function.
;;   
;;   This macro may be used by the function "compile-expression", which
;;   please consult.
;; 
;; --------------------------------------------------------------------
;; 
;; generate-type-match-code (expected-type : type-specifier,
;;                           target        : place)
;;   A function which generates the Common Lisp code for testing the
;;   current input source character for its compliance with a specified
;;   type specifier.
;;   
;;   Upon the character eligibility's confirmation, this code fragment
;;   assigns the probed character to the optated target place, advances
;;   to the next character in the source, and returns a generalized
;;   boolean value of "true".
;;   
;;   If no type compliance could be attested, the code fragment neither
;;   engages in an assignment, nor in the source position cursor's
;;   advancement, instead returning a generalized boolean value of
;;   "false".
;;   
;;   This function is utilized by the higher-level "match-type" macro,
;;   which please see.
;; 
;; match-type (expected-type, place)
;;   A macro which invokes the function "generate-type-match-code"
;;   with its EXPECTED-TYPE and the PLACE as the arguments and returns
;;   the code fragment produced by the same function.
;;   
;;   This macro may be used by the function "compile-expression", which
;;   please consult.
;; 
;; == A DETAILED ACCOUNT ON META'S FUNCTIONS AND MACROS ==
;; META's original implementation, a treatise capable of consultation in
;; [baker1991prag], wists of a champarty partaking in by both macros and
;; functions, in which case the formers enjoy a more peisant parcery's
;; recipience in both significance and tally.
;; 
;; Maugre its foundational principles' retention, the solution pursued
;; in this project inclines toward an antilibration in the two operative
;; constructs' commitments, where functional definitions serves as a
;; ubiquitous substratum for the macro counterparts, their vindication
;; commorant in the potential for fungability, as the macro diorisms in
;; both their signature and implementation may be emancipated from the
;; functions responsible for the Common Lisp code fragments' production;
;; in their ultimity admitting the homologation for modulations in the
;; implementing aspects.
;; 
;; The following treatise's hyle shall accept the macro and function
;; contingency's elucidation, which in three tiers attain their
;; collaborative entelechy, proceeding from the top-level conspection
;; to the lower strata.
;; 
;; --------------------------------------------------------------------
;; 
;; ENTRY OPERATIONS WHICH GENERATE FOR RECOGNIZED META CONSTRUCTS OR
;; CHARACTER LITERALS COMMON LISP PARSING CODE
;; 
;; << function/method >>
;; compile-expression (expression : character)
;; compile-expression (expression : string)
;; compile-expression (expression : META-Object)
;;   Generates and returns a Common Lisp code fragment which probes the
;;   expression for its compatibility with the input source's current
;;   state, that is, its contemporaneously selected character or
;;   characters, responding with a generalized boolean "true" value
;;   upon its conformation, otherwise with "false".
;;   
;;   The following principles apply to the expression in dependency
;;   upon its concrete type or class:
;;   
;;     (a) If the expression represents a "META-Alternative", the
;;         generated code constitutes an OR-combination of the
;;         "META-Alternative" options' produced code fragments, each
;;         such yielded by an invocation of the option "op" in
;;         "compile-expression(op)". Please heed that an empty option
;;         set always fails; this circumstance may be harnessed to
;;         produce a logical contradiction.
;;     
;;     (b) If the expression represents a "META-Evaluation", the
;;         generated code is yielded by simply invoking the contained
;;         form.
;;     
;;     (c) If the expression represents a "META-Repetition", the
;;         generated code constitutes a loop which perpetuates as long
;;         as its contained form "f", in its evaluated guise, obtained
;;         via "compile-expression(f)", yields a generalized boolean
;;         "true" value, with the loop code finally returning a
;;         generalized boolean value of "true" itself.
;;     
;;     (d) If the expression represents a "META-Sequence", the
;;         generated code constitutes an AND-combination of the
;;         "META-Sequence" elements' produced code fragments, each
;;         such yielded by an invocation of the element "elem" in
;;         "compile-expression(elem)". Please heed that an empty
;;         sequence always matches; this circumstance may be harnessed
;;         to produce a logical tautology.
;;     
;;     (e) If the expression represents a "META-Type-Test", the
;;         generated code constitutes a "match-type" invocation, the
;;         "META-Type-Test"'s evaluated type specifier "type-spec" and
;;         its unevaluated assignment place "target" amounting to
;;         "match-type(type-spec, target)".
;;     
;;     (f) If the expression represents a character, the generated code
;;         constitutes a "match-content" invocation utilizing the
;;         expression's evaluated character form "char", amounting to
;;         "match-content(char)".
;;     
;;     (g) If the expression represents a string, the generated code
;;         constitutes a "match-content" invocation utilizing the
;;         expression's evaluated string form "str", amounting to
;;         "match-content(str)".
;;     
;;     (h) Any other expression cannot be evaluated, and thus
;;         instigates an error reaction.
;; 
;; << macro >>
;; match-expression (expression)
;;   Invokes the function "compile-expression(expression)", which in
;;   turn generates and returns a Common Lisp code fragment that probes
;;   the "expression" for its compatibility with the input source's
;;   current state, that is, its contemporaneously selected character
;;   or characters, responding with a generalized boolean "true" value
;;   upon its conformation, otherwise with "false".
;;   
;;   This macro represents the entry point into the META parser, whence
;;   proceed all further causata.
;; 
;; --------------------------------------------------------------------
;; 
;; ADMINICULAR OPERATIONS WHICH PROBE THE CURRENT INPUT SOURCE
;; CHARACTER FOR EQUALITY WITH AN EXPECTED CHARACTER
;; 
;; << function/method >>
;; generate-content-match-code (expected-content : character)
;; generate-content-match-code (expected-content : string)
;;   Generates a Common Lisp code fragment which tests whether the
;;   input source's current character or characters match the
;;   "expected-content", on confirmation advancing beyond the
;;   successfully probed tmema in the input source and returning a
;;   generalized boolean "true" value; otherwise produces "false"
;;   without modulation of the input source position cursor.
;; 
;; << macro >>
;; match-content (expected-content)
;;   Invokes the function
;;   "generate-content-match-code(expected-content)", which in turn
;;   generates a Common Lisp code fragment that tests whether the input
;;   source's current character or characters match the
;;   "expected-content", on confirmation advancing beyond the
;;   successfully probed tmema in the input source and returning a
;;   generalized boolean "true" value; otherwise produces "false"
;;   without modulation of the input source position cursor.
;; 
;; --------------------------------------------------------------------
;; 
;; ADMINICULAR OPERATIONS WHICH PROBE THE CURRENT INPUT SOURCE
;; CHARACTER FOR CONFORMANCE WITH AN EXPECTED TYPE SPECIFIER
;; 
;; << function >>
;; generate-type-match-code (expected-type : typeSpecifier,
;;                           target        : place)
;;   Generates a Common Lisp code fragment which tests whether the
;;   input source's current character conforms to the "expected-type",
;;   on confirmation storing the just probed character in the place
;;   "target", ere advancing the input source's position cursor to the
;;   next character, and ultimately returning a generalized boolean
;;   "true" value; otherwise produces "false" without the input source
;;   position cursor's modulation.
;; 
;; << macro >>
;; match-type (expected-type, target)
;;   Invokes the function
;;   "generate-type-match-code(expected-type, target)", which in turn
;;   generates a Common Lisp code fragment that tests whether the input
;;   source's current character conforms to the "expected-type", on
;;   confirmation storing the just probed character in the place
;;   "target", ere advancing the input source's position cursor to the
;;   next character, and ultimately returning a generalized boolean
;;   "true" value; otherwise produces "false" without the input source
;;   position cursor's modulation.
;; 
;; == RELATIONSHIPS BETWIXT META FUNCTIONS/MACROS INVOCATIONS ==
;; The ordonnance and principles whose coefficiency governs the META
;; parser application, airted downwards from the "match-expression"
;; macro as the top echelon's interface operation, shall be the alow
;; variation on a UML activity diagram's cynosure, where the following
;; stipulations, thilk act as sepiments in agency from the standard's
;; notions, merit to receive special heed:
;; 
;;   (1) Activities (rectangles) represent functions or macros, bearing
;;       the subsuming species as a stereotype in chevrons aboon the
;;       signature. Please also note the traditional curvature's
;;       deprivation whose expected presence in the UML design furnishes
;;       a proprium of activities as counterdinguished from data
;;       objects, and the same in our case, with its disencumberance
;;       from the latter species of component, ostends a stringently
;;       orthogonal conformation, while yet retaining its agency as an
;;       activity's succedaneum.
;;   
;;   (2) Transitions (arrows) serve to simulate function or macro
;;       invocations.
;;   
;;   (3) Recursive stevenings are unraveled; that is, a reference to a
;;       function or macro already extant at another location in the
;;       diagram, without discrimination of its paregal or modulated
;;       status, will be limned in a separate activity, in lieu of a
;;       transition to the present one.
;; 
;;            (O)
;;             |
;;             V
;; +----------------------+
;; |     << macro >>      |
;; | match-expression (T) |
;; +----------------------+
;;             |
;;             |
;;             V
;; +------------------------+
;; |     << function >>     |
;; | compile-expression (T) |
;; +------------------------+
;;             |
;;             |
;;             V
;;             ^
;;            / \
;;            \ /
;;             v
;;             |
;;             | switch on type of argument "T"
;;             |
;;    +--------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[META-Alternative]--> |            << function >>             |
;;    |                        | compile-expression (META-Alternative) |
;;    |                        |                   +                   |
;;    |                        |                   or                  |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[META-Evaluation]---->|     execute META evaluation form      |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[META-Repetition]---> |            << function >>             |
;;    |                        | compile-expression (META-Repetition)  |
;;    |                        |                   +                   |
;;    |                        |                  loop                 |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[META-Sequence]-----> |            << function >>             |
;;    |                        |  compile-expression (META-Sequence)   |
;;    |                        |                   +                   |
;;    |                        |                  and                  |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[META-Type-Test]----> |              << macro >>              |
;;    |                        |         match-type (..., ...)         |
;;    |                        +---------------------------------------+
;;    |                                            |
;;    |                                            V
;;    |                        +---------------------------------------+
;;    |                        |            << function >>             |
;;    |                        |  generate-type-match-code (..., ...)  |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[character]---------> |              << macro >>              |
;;    |                        |       match-content (character)       |
;;    |                        +---------------------------------------+
;;    |                                            |
;;    |                                            V
;;    |                      +-----------------------------------------+
;;    |                      |             << function >>              |
;;    |                      | generate-content-match-code (character) |
;;    |                      +-----------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[string]------------> |              << macro >>              |
;;    |                        |        match-content (string)         |
;;    |                        +---------------------------------------+
;;    |                                            |
;;    |                                            V
;;    |                      +-----------------------------------------+
;;    |                      |             << function >>              |
;;    |                      |  generate-content-match-code (string)   |
;;    |                      +-----------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[otherwise]---------> |                 error                 |
;;                             +---------------------------------------+
;;                                                 |
;;                                                 V
;;                                                (X)
;; 
;; == READER MACROS GENERATE CODE BY CALLBACKS WHEN DETECTING SYMBOLS ==
;; The claviger to the META parser's approximation of regular
;; expressions in the matter of its syntactical conspection is furnished
;; in the septuple accompt of reader macro definitions; whence ensues,
;; by a modulation applied to the Common Lisp environment' syntaxis, the
;; capacitation for a compendious design.
;; 
;; Please recall that one of the aspirations inherent to META maintains
;; its existency in a similacrum to regular expressions as an enhaused
;; syntomy's provenance. The dioristic traditions vauncing from siccan
;; expressions' designment are molded into the malleable Common Lisp
;; syntax by mediation of the aforementioned reader macro
;; infrastructure, to whom the governance over a bespoke lexical
;; analyzation and parsing of the Lisp source code is imparted.
;; 
;; Upon a registered macro character's recognition in the code, the
;; standard Lisp reader's wike experiences an abeyance, instead
;; redirecting the control flow to a callback function affiliated with
;; this instigating symbol whose propagation ceases with the responsible
;; function's cessation. This handler's onus manifests in the generation
;; and returning of a valid piece of Common Lisp code, thilk, as a
;; consequence of the function's conclusion, immediately consigns to the
;; standard Lisp reader's castaldy, evaluation, and execution.
;; 
;; If, as an example adduced, the entity "$" commits to an engagement
;; with a reader macro as a macro character, maintaining a lealty with
;; this approximate forbisen
;; 
;;   (set-macro-character #\$
;;      #'(lambda (stream macro-character)
;;          (declare (type stream    stream))
;;          (declare (type character macro-character))
;;          ;; Return a Common Lisp form or code fragment to evaluate.
;;          ;; One usually reads from the input STREAM in order to
;;          ;; obtain the requisite data for this emprise.
;;      )
;;   )
;; 
;; the result returned by the callback function will be inserted into
;; the respective position in the executing Common Lisp program, in lieu
;; of the "$" character.
;; 
;; == SEVEN READER MACROS GOVERN THE META EXPRESSIONS GENERATION ==
;; In our META project's particular circumstance, siccan reader macro
;; definition for the symbol "$" may be chosen in this fashion:
;; 
;;   (set-macro-character #\$
;;     #'(lambda (stream macro-character)
;;         (declare (type stream    stream))
;;         (declare (type character macro-character))
;;         (declare (ignore         macro-character))
;;         (let ((form-to-repeat (read stream)))
;;           (declare (type T form-to-repeat))
;;           (the META-Repetition
;;             (make-meta-repetition form-to-repeat)))))
;; 
;; Please heed in the aboon exemplary tmema that the variable
;; "form-to-repeat" constitutes a piece of supererogation: The
;; ``META-Repetition'' instance, produced by adminiculum of the
;; constructor ``make-meta-repetition'', enjoys the homologation of the
;; ``(read stream)'' result's immediate consumption. As a consectary,
;; the principle of owelty with the more detailed solution applies to
;; the definition
;; 
;;   (set-macro-character #\$
;;     #'(lambda (stream macro-character)
;;         (declare (type stream    stream))
;;         (declare (type character macro-character))
;;         (declare (ignore         macro-character))
;;         (the META-Repetition
;;           (make-meta-repetition
;;             (read stream)))))
;; 
;; In the case of the META technique, the reader macros apply themselves
;; to shorthand notations' provisions, everichon from this membership
;; responsible for a specific META expression type's assemblage and
;; instantiation, by which agency the necessity of explicit META object
;; creations is rendered an otiose emprise.
;; 
;; These seven members shall receive a more reified ilk of elucidation:
;; 
;;   ------------------------------------------------------------------
;;   Macro     | Affiliation      | Description
;;   character |                  |
;;   ----------+------------------+------------------------------------
;;   [         | META sequence    | Demarcates a META sequence block's
;;             | (inchoation)     | inchoation; expects zero or more
;;             |                  | forms or META expressions,
;;             |                  | terminating in a "]" token.
;;   ..................................................................
;;   ]         | META sequence    | Demarcates a META sequence block's
;;             | (conclusion)     | conclusion.
;;             |                  |------------------------------------
;;             |                  | Actually constitutes a paregal of
;;             |                  | the standard list terminator ")".
;;   ..................................................................
;;   {         | META alternative | Demarcates a META alternative
;;             | (inchoation)     | block's inchoation; expects zero or
;;             |                  | more forms or META expressions,
;;             |                  | terminating in a "}" token.
;;   ..................................................................
;;   }         | META alternative | Demarcates a META alternative
;;             | (conclusion)     | block's conclusion.
;;             |                  |------------------------------------
;;             |                  | Actually constitutes a paregal of
;;             |                  | the standard list terminator ")".
;;   ..................................................................
;;   $         | META repetition  | Demarcates a META repetition;
;;             |                  | instigates the next form's
;;             |                  | consumption, which might be any
;;             |                  | form or META expression.
;;   ..................................................................
;;   @         | META type test   | Demarcates a META type test;
;;             |                  | instigates a list of two elements'
;;             |                  | consumption in order to define a
;;             |                  | type specifier and an assignment
;;             |                  | target.
;;   ..................................................................
;;   !         | META evaluation  | Demarcates a META evaluation;
;;             |                  | instigates the next form's
;;             |                  | consumption in order to evaluate
;;             |                  | the same.
;;   ------------------------------------------------------------------
;; 
;; 
;; Appendices
;; ==========
;; A certain set of topics' involvement may be delivered to one's
;; apprehension to whom a gravity meted above pure autotelic assay
;; can be assigned; yet no elevation to a plenipotentiary for the
;; intrusion into this treatise's chief body constitutes a thing of
;; vouchsafement. Material of such medioxumous meed shall establish the
;; coming sections' cynosure.
;; 
;; == APPENDIX A: META'S REQUISITES AS DEMANDS TO THE LANGUAGE ==
;; One rationale for the META concept's development in Common Lisp
;; may be discovered in the faculties levied against its implementing
;; programming language, thilk, as a sequela to this Lisp dialect's
;; natural accommodations, conduces the twain's historical alligation.
;; 
;; The set of necessitated competences that a programming language in
;; its desideration of the META technique's implementation per force
;; ought to adduce as the compliance's vouch shall be the following
;; enumeration's subject:
;; 
;;   (1) CODE GENERATION CAPABILITIES
;;       The declaimed diorism of META as a warklume for the
;;       *CONSTRUCTION* of a parser, rather than simply its actuation,
;;       manifests in an approach that generates and assembles the code
;;       in the source language.
;;       
;;       As a consectary, a programming language desiderating this
;;       technique's involvement ought to provide the contingency for
;;       its own code evaluation. The emphasis is empighted on the
;;       output of code, akin to a template with variable segments, as
;;       counterdistinguished from computation results. For instance,
;;       such a productive facility would, for the cause of a printing
;;       behest, respond with an exemplary program tmema
;;         
;;         (print "Hello")
;;       
;;       in lieu of the actual printing of message
;;       
;;         "Hello"
;;       
;;       In an eath and simplified parlance, the requisite is levied
;;       upon functions to return code fragments compatible with the
;;       language itself, admissible for future execution.
;;       
;;       In Common Lisp, this dever befalls the general macro
;;       infrastructure, obtainable via "defmacro" specifications.
;;       
;;       A simplified example, endeictic in its potential and, albeit
;;       didascalic, executable, shall limn the explicated concept.
;;       Please heed that the function ``compile-expression'' forms the
;;       code's cynosure.
;;       
;;         (declaim (type simple-string *source-code*))
;;         (declaim (type fixnum        *current-position*))
;;         (declaim (type character     *current-character*))
;;         
;;         (defparameter *source-code*      "sparrow")
;;         (defparameter *current-position* 0)
;;         
;;         (define-symbol-macro *current-character*
;;           (the character
;;             (schar *source-code* *current-position*)))
;;         
;;         ;; Generate and return, but do not yet execute, code which
;;         ;; matches the EXPECTED-CHARACTER against the
;;         ;; *CURRENT-CHARACTER*.
;;         (defun compile-expression (expected-character)
;;           (declare (type character expected-character))
;;           `(when (and (< *current-position*
;;                          (length *source-code*))
;;                       (char= *current-character*
;;                              ,expected-character))
;;              (incf *current-position*)
;;              T))
;;         
;;         ;; Execute the character matching code requested from the
;;         ;; function "COMPILE-EXPRESSION".
;;         (defmacro match-expression (expression)
;;           (compile-expression expression))
;;       
;;       The function ``compile-expression'' assembles a Common Lisp
;;       code tmema which may be evaluated later on without its
;;       immediate execution; instead delivering the fragment as its
;;       return value. The ``match-expression'' macro accepts the thus
;;       delineated code parcel, evaluates thilk, and thus accompasses
;;       its appertaining causata, in addition to its contingent
;;       resulting outputs.
;;   
;;   (2) INTEGRABLE SYNTAX CUSTOMIZATIONS
;;       The ultimity of its deployment appertains for the META parser
;;       in its services' immediate integration into the amplecting
;;       source code, their actuation's intention communicated via
;;       particular symbols.
;;       
;;       In order to auspiciously pursue this telos, the implementing
;;       programming language's compass ought to homologate the
;;       definition of bespoke syntactical configurations, which
;;       establishes a tantamount to the syntaxis' extension or
;;       modification.
;;       
;;       In the Common Lisp realm, such capacitation is elicitated by
;;       mediation of reader macros, thilk permit the introduction of a
;;       custom design in the standard's stipulations. The consequent
;;       malleability provides for this Lisp dialect's widely propagated
;;       covenableness for the realization of domain-specific language
;;       (DSL) integrations.
;;       
;;       As a forbisen, the code tmema
;;       
;;         (match-expression
;;           $[@(decimal-digit current-character)
;;             !(progn (print current-character) T)])
;;       
;;       demonstrates the reservations of the characters "$", "[", "]",
;;       "@", and "!" for certain objectives, which trigger the
;;       execution of routines implemented by the programmer, that is,
;;       their personally imposed lexical analyzation and parsing
;;       procedures. Destitute of reader macros, these symbols would
;;       introduce the etiologies for errors, being meaningless in the
;;       default Common Lisp syntax.
;; 
;; A veridicous affedavit to the META technique transcription's
;; potential across other environments attests its tendance in the
;; programming language "Dylan" [dylanhackers2024opendylan] for whom a
;; dedicated library, euonymous in its agnomination as "Meta",
;; [dylanhackers2024metalibrary], and accommodated for the particular
;; specimen's propria, tallies among the afforded avails.
;; 
;; == APPENDIX B: META'S DISENCUMBRANCE FROM THE SOURCE OBJECT ==
;; A kenspeckle attribute commorant in META's approach pertains to its
;; disencumberance from the submitted source's designment: The lexical
;; analyzation and parsing stages most commonly expect the received
;; input to subsume into a string or a stream species; META is
;; counterdistinguished in this aspect by its lack of such imposing
;; covenant. Any object admissible to the Common-Lisp-internal
;; evaluation mechanism may be induced into the analyzation process and
;; incorporated into the assemblage stage.
;; 
;; This liberty ensues from the special META expressions' actuations,
;; which, when operated, query their necessitated arguments from the
;; input source, usually by adminiculum of the autochthonous function
;; "read", thilk consumes the subsequent form from the Lisp code and
;; returns a covenable datum in compliance with the Common Lisp
;; standard for its personal detection tier. The thus produced object
;; may register its membership among any conceivable type, for instance,
;; a character, a string, a symbol, or a list.
;; 
;; In a pseudocode diction, Procrustean to all tongues of programming,
;; one would conceive:
;; 
;;   let nextObject <- programStream.read()
;; 
;; In Common Lisp's concrete parlance, a forbisen entalented with owelty
;; limns:
;;   (let ((next-object (read program-stream)))
;;     ;; [...]
;;   )
;; 
;; The contingency of paravaunt interest for these purposes shall be
;; the ostended in the below apercu. No potential for the avail's
;; exhaustion is purported by this listing.
;; 
;;   ------------------------------------------------------------------
;;   Operation | Causatum
;;   ----------+-------------------------------------------------------
;;   read      | Consumes and returns the next Common Lisp object in
;;             | compliance with the language standard's interpretation
;;             | rules. The yielded result may be of any admissible
;;             | type.
;;   ..................................................................
;;   read-char | Consumes and returns the next character.
;;   ..................................................................
;;   read-line | Consumes, commencing from the current position and
;;             | terminating immediately before the nearest following
;;             | linebreak character, or with the input's exhaustion,
;;             | an entire line and returns its string representation.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-02-17
;; 
;; Sources:
;;   [baker1991prag]
;;   Henry G. Baker, "Pragmatic Parsing in Common Lisp", January, 1991
;;   URL: "https://archive.ph/wkRh4"
;;   Notes:
;;     - Describes the META parser for Common Lisp.
;;   
;;   [cliki2020meta]
;;   The CLiki contributors, "CLiki: Meta", October 24th, 2020
;;   URL: "https://www.cliki.net/META"
;;   Notes:
;;     - Discusses the META parsing technique.
;;     - Provides references to related implementations and libraries.
;;     - These include, among others:
;;       o The original treatise on this subject by Henry G. Baker.
;;         |-> Please see the bibligraphy entry [baker1991prag].
;;       o The "meta" library by Jochen Schmidt and Francois-Rene Rideau
;;         |-> Please see the bibliography entry [schmidt2015meta].
;;       o The "Scribble" library by Francois-Rene Rideau.
;;         |-> Please see the bibliography entry [rideau2023scribble].
;;   
;;   [dylanhackers2024opendylan]
;;   Dylan Hackers, "Open Dylan", 2024
;;   URL: "https://opendylan.org/"
;;   Notes:
;;     - Homepage of the "OpenDylan" implementation of the programming
;;       language "Dylan".
;;     - A "META" parser library exists for the same, for which please
;;       consign to your conspectuity the bibliography entry
;;       [dylanhackers2024metalibrary].
;;   
;;   [dylanhackers2024metalibrary]
;;   Dylan Hackers, "The Meta Library", 2024
;;   URL: "https://package.opendylan.org/meta/meta.html"
;;   Notes:
;;     - Documentation of the "Meta" library for the programming
;;       language "Dylan".
;;     - This library furnishes an implementation of the "META" parsing
;;       technique adjusted for the Dylan programming language.
;;   
;;   [esolang2023VarStack]
;;   The Esolang contributors, "VarStack", November 11th, 2023
;;   URL: "https://esolangs.org/wiki/VarStack"
;;   
;;   [rideau2023scribble]
;;   Francois-Rene Rideau,
;;     "Francois-Renc Rideau / scribble - GitLab", October 11th, 2023
;;   URL: "https://gitlab.common-lisp.net/frideau/scribble"
;;   Notes:
;;     - Repository of the external library "Scribble".
;;     - Depends, among others, on the library "meta", an implementation
;;       of Henry G. Baker's META parsing technique, co-authored by the
;;       Scribble project's author themself.
;;   
;;   [schmidt2015meta]
;;   Jochen Schmidt, Francois-René Rideau,
;;     "Francois-Rene Rideau / meta - GitLab", May 27th, 2015
;;   URL: "https://gitlab.common-lisp.net/frideau/meta"
;;   Notes:
;;     - Repository of the external library "meta".
;;     - Comprehends, in its project file "meta-src.lisp", an
;;       implementation of Henry G. Baker's META parsing technique.
;;     - Furnishes the foundry for the external library "Scribble",
;;       created by one of the co-authors, "Francois-Rene Rideau".
;;   
;;   [schorre1964meta]
;;   D. V. Schorre,
;;     "META II: A Syntax-Oriented Compiler Writing Language", 1964
;;   URL: "https://dl.acm.org/doi/pdf/10.1145/800257.808896"
;;   Notes:
;;     - Furnishes the original description of the META parsing
;;       technique.
;;   
;;   [stackoverflow2015q32297292]
;;   The Stack Overflow contributors,
;;     'Is there any "switch case" in UML use-case specification?',
;;     August 30th, 2015
;;   URL: "https://stackoverflow.com/questions/32297292/
;;         is-there-any-switch-case-in-uml-use-case-specification"
;;   Notes:
;;     - Demonstrates the visualization of switch-case constructs in
;;       a UML activity diagram.
;;   
;;   [stackoverflow2017q46955170]
;;   The Stack Overflow contributors,
;;     "Easy way to create string-stream from file-stream",
;;     October 26th, 2017
;;   URL: "https://stackoverflow.com/questions/46955170/
;;         easy-way-to-create-string-stream-from-file-stream"
;;   Notes:
;;     - Describes the usage of the "meta-sexp" package for parsing.
;;     - This offers a parser generator using LL(1) grammars in
;;       conjunction with S-expressions.
;;     - The parser is based on the META language described by
;;       Henry G. Baker.
;;   
;;   [yazici2017metasexp]
;;   Volkan Yazici,
;;     "GitHub - vy/meta-sexp: A META parser generator using LL(1)
;;      grammars with s-expressions.",
;;     October 30th, 2017
;;   URL: "https://github.com/vy/meta-sexp"
;;   Notes:
;;     - GitHub page of the Common Lisp library "meta-sexp".
;;     - This offers a parser generator using LL(1) grammars in
;;       conjunction with S-expressions.
;;     - The parser is based on the META language described by
;;       Henry G. Baker.
;;   
;;   [yellowrabbit2016lisptoy1dom]
;;   Yellow Rabbit, "DOM and the First Nodes of the Document",
;;     May 7th, 2016
;;   URL: "https://yrabbit.github.io/blog/2016/lisp-toy-web-engine/"
;;   Notes:
;;     - Describes the META parser.
;;     - Applies the META parser concept to the parsing of HTML.
;;     - Describes the modeling of HTML elements as object in
;;       Common Lisp.
;;   
;;   [yellowrabbit2016lisptoy2metaparser]
;;   Yellow Rabbit, "META Parser - Classical Parsing System",
;;     May 22nd, 2016
;;   URL: "https://yrabbit.github.io/blog/2016/
;;         lisp-toy-web-parsing-meta/"
;;   Notes:
;;     - Describes the META parser.
;;     - Applies the META parser concept to the parsing of HTML.
;;   
;;   [yellowrabbit2016lisptoy3htmltext]
;;   Yellow Rabbit, "HTML Grammar in the Very First Approximation",
;;     May 25th, 2016
;;   URL: "https://yrabbit.github.io/blog/2016/lisp-toy-web-parse-text/"
;;   Notes:
;;     - Describes the META parser.
;;     - Applies the META parser concept to the parsing of HTML.
;;     - Cynosure: the parsing of HTML "text" elements.
;;   
;;   [yellowrabbit2016lisptoy4comments]
;;   Yellow Rabbit,
;;     "Comments as the Second Most Important HTML Element",
;;     May 31st, 2016
;;   URL: "https://yrabbit.github.io/blog/2016/
;;         lisp-toy-web-parse-comment/"
;;   Notes:
;;     - Describes the META parser.
;;     - Applies the META parser concept to the parsing of HTML
;;       comments.
;;   
;;   [yellowrabbit2016lisptoy5elements]
;;   Yellow Rabbit, "Parse the Complex Element", June 2nd, 2016
;;   URL: "https://yrabbit.github.io/blog/2016/
;;         lisp-toy-web-parse-element/"
;;   Notes:
;;     - Describes the META parser.
;;     - Applies the META parser concept to the parsing of HTML
;;       elements.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination acquires the TYPE-NAME,
   the formal parameters accounting for the LAMBDA-LIST's verbatim
   dation, and whose subject of the docimasy is imparted the
   CANDIDATE-NAME as its nevening, evaluates the BODY forms, and
   construes the desinent form's primary result as the assessment's
   conclusion, with a \"generalized boolean\" value of \"true\" serving
   to signify the candidate object's compliance with the type's
   covenant, while a failure shall be communicated with a \"false\"
   response.
   ---
   The first BODY form, if assuming a string species, will be
   interpreted as the derived type's documentation string, being
   reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            (format NIL "Defines the type ``~a''." type-name))
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name)
                        (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type list-of
    (candidate
     &optional (element-type '*) (size '*))
  "The ``list-of'' type defines a list composed of the SIZE tally of
   members, everichon among these adheres to the ELEMENT-TYPE, both
   configurations are governed by the generic sentinel ``*'', the same
   for the first attribute serves to signify any cardinality's
   homologation, and for the second its disencumbrance from a specific
   element type."
  (and
    (listp candidate)
    (or (eq size '*)
        (=  size (length (the list candidate))))
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table comprehending zero or
   more entries, everichon among these a twissel listing a key of the
   KEY-TYPE and an affiliated value that subscribes to the VALUE-TYPE,
   for both is accommodated the generic sentinel ``*'' as a default."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable VarStack program as a
   one-dimensional simple array comprehending zero or more ``Statement''
   objects."
  '(simple-array Statement (*)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations, the
   compass of which amplects, among other specimens, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of function prototypes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (*) *) compile-expression))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\"
   entity, upon which construe is exerted a veridicous Boolean
   tantamount's edification, producing for a non-``NIL'' input a
   ``boolean'' value of ``T''; otherwise, for the ``NIL'' OBJECT,
   responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substring-starts-at-p (haystack start-index-into-haystack needle)
  "Determines whether the NEEDLE occurs entirely in the HAYSTACK,
   commencing at the inclusive START-INDEX-INTO-HAYSTACK, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string haystack))
  (declare (type fixnum start-index-into-haystack))
  (declare (type string needle))
  (the boolean
    (get-boolean-value-of
      (string= haystack needle
        :start1 start-index-into-haystack
        :end1   (min (+ start-index-into-haystack
                        (length needle))
                     (length haystack))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "META".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct META-Object
  "The ``META-Object'' interface accommodates the substratum for all
   classes pursuing the implementation of META expressions.")

;;; -------------------------------------------------------

(defgeneric compile-meta-object (meta)
  (:documentation
    "Generates and returns a Common Lisp code fragment which probes the
     input source's in its contemporaneous state for its covenableness
     with the META object's nomothesia."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Alternative".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Alternative
  (:include     META-Object)
  (:constructor make-meta-alternative (alternatives)))
  "The ``META-Alternative'' class serves in the representation of an
   expression whose successful matching against a source string depends
   upon at least one of its options' stipulated antecedent's
   conformation."
  (alternatives (error "Missing META alternatives.")
                :type      (list-of T)
                :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta-object ((meta META-Alternative))
  "Generates and returns a Common Lisp code tmema which comprises an
   OR-combination of the META alternative's elements in their specified
   order, and which, upon the first eligible member's successful
   matching, returns thilk's result; otherwise produces a generalized
   boolean \"false\" value."
  (declare (type META-Alternative meta))
  `(the T
     (or ,@(mapcar #'compile-expression
             (meta-alternative-alternatives meta)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Alternative".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Evaluation
  (:include     META-Object)
  (:constructor make-meta-evaluation (form)))
  "The ``META-Evaluation'' class serves in the ensconcement of a META
   expression whose causatum is begotten from a Common Lisp form's
   evaluation.
   ---
   This construct's competence amplects the capacitation for actions of
   arbitrary complexity and sophistication to participate in the META
   parser's procession --- including such of epiphenomenal potential."
  (form (error "Missing META evaluation form.")
        :type      T
        :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta-object ((meta META-Evaluation))
  "Evaluates the form consigned to the META evaluation object's castaldy
   and returns the thus produced values."
  (declare (type META-Evaluation meta))
  (the T
    (meta-evaluation-form meta)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Repetition".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Repetition
  (:include     META-Object)
  (:constructor make-meta-repetition (form)))
  "The ``META-Repetition'' class serves in a repetitive form
   application's production, ultimately assembling a Common Lisp code
   tmema which, tantamount to the Kleene star's notions, attempts to
   match the ensconced form zero or more times, always succeeding with
   a Boolean \"true\" return value."
  (form (error "Missing META repetition form.")
        :type      T
        :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta-object ((meta META-Repetition))
  "Assembles and returns a Common Lisp program fragment entalented with
   the capacity to match the META repetition object's form zero or more
   times in an iterative structure, the thus edified code tmema in any
   case producing a ``boolean'' value of ``T''."
  (declare (type META-Repetition meta))
  `(the boolean
     (loop
       while
         ,(compile-expression
            (meta-repetition-form meta))
       finally
         (return T))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Sequence".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Sequence
  (:include     META-Object)
  (:constructor make-meta-sequence (forms)))
  "The ``META-Sequence'' class serves in the representation of an
   expression whose successful matching against a source string depends
   upon the conformation of its membership's entirety in the specified
   order."
  (forms (error "Missing META sequence forms.")
         :type      (list-of T)
         :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta-object ((meta META-Sequence))
  "Generates and returns a Common Lisp code tmema which comprises an
   AND-combination of the META sequence's elements in their specified
   order, and which, upon succeeding in all of its members' matchings,
   returns the desinent element's result; otherwise produces a
   generalized boolean \"false\" value."
  (declare (type META-Sequence meta))
  `(the T
     (and ,@(mapcar #'compile-expression
              (meta-sequence-forms meta)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of clas "META-Type-Test".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Type-Test
  (:include     META-Object)
  (:constructor make-meta-type-test (expected-type receiving-place)))
  "The ``META-Type-Test'' class is consigned the parcery of the onus to
   generate a Common Lisp program fragment capable of an input source's
   character's docimasy concerning its compliance with an optated type
   specifier, resorting in the case of an affirmation to the apodosis
   which stores the probed character in the specified place, ere
   returning a Boolean \"true\" output, while responding in the
   otherwise case with a \"false\" signification."
  (expected-type   (error "Missing expected type.")
                   :type      T
                   :read-only T)
  (receiving-place (error "Missing receiving place.")
                   :type      T
                   :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta-object ((meta META-Type-Test))
  "Generates and returns a Common Lisp code tmema which probes the
   input source's current character for its compliance with the META
   type test's stipulated type specifier, and which upon its matching
   assigns the probed character to the designated target place, while
   consuming this character, and finally returning a ``boolean'' value
   of ``T''; otherwise, neither an assignment nor consumption precedes
   the ``NIL'' value response."
  (declare (type META-Type-Test meta))
  `(the boolean
     (match-type
       ,(meta-type-test-expected-type   meta)
       ,(meta-type-test-receiving-place meta))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input source.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source-code*))
(declaim (type fixnum        *source-position*))
(declaim (type fixnum        *source-length*))
(declaim (type character     *source-character*))
(declaim (type boolean       *source-has-next-character-p*))

;;; -------------------------------------------------------

(defparameter *source-code* ""
  "The input string to parse.")

(defparameter *source-position* 0
  "The current position into the *SOURCE-CODE*.")

(defparameter *source-length* 0
  "The number of characters constituting the *SOURCE-CODE*.")

;; The character at the current *SOURCE-POSITION* into the
;; *SOURCE-CODE*.
(define-symbol-macro *source-character*
  (the character
    (schar *source-code*
           *source-position*)))

;; Determines whether the current *SOURCE-POSITION* constitutes a valid
;; index into the *SOURCE-CODE*.
(define-symbol-macro *source-has-next-character-p*
  (the boolean
    (get-boolean-value-of
      (< *source-position*
         *source-length*))))

;;; -------------------------------------------------------

(defun set-source-code (new-code)
  "Sets the program *SOURCE-CODE* to the NEW-CODE, resets appertaining
   state variables, and returns no value."
  (declare (type string new-code))
  (setf *source-code*     (coerce new-code 'simple-string))
  (setf *source-position* 0)
  (setf *source-length*   (length *source-code*))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of content matching code generator.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric generate-content-match-code (expected-content)
  (:documentation
    "Generates and returns a Common Lisp code tmema which probes the
     *SOURCE-CODE*'s current state anent its compliance with the
     EXPECTED-CONTENT, on affirmation advancing the *SOURCE-POSITION*
     cursor to the location immediately succeeding the matched segment,
     while returning a ``boolean'' value of ``T''; otherwise
     accompassing no causatum when responding with ``NIL''.")
  
  (:method ((expected-character character))
    "Generates and returns a Common Lisp code tmema which probes the
     *SOURCE-CODE*'s current character for its owelty with the
     EXPECTED-CHARACTER, the code producing on confirmation a
     ``boolean'' value of ``T'', while concomitantly advancing the
     *SOURCE-POSITION* cursor to the subsequent position; otherwise,
     the resulting program fragment responds with ``NIL'', without any
     further epiphenomenon's actuation."
    (declare (type character expected-character))
    `(the boolean
       (when (and *source-has-next-character-p*
                  (char= *source-character* ,expected-character))
         (incf *source-position*)
         T)))
  
  (:method ((expected-string string))
    "Generates and returns a Common Lisp code tmema which probes the
     characters commencing in the *SOURCE-CODE* from its currently
     occupied position with the EXPECTED-STRING's content, the code
     producing on confirmation a ``boolean'' value of ``T'', while
     concomitantly advancing the *SOURCE-POSITION* cursor beyond the
     matched interval in the *SOURCE-CODE*; otherwise, the resulting
     program fragment responds with ``NIL'', without any further
     epiphenomenon's actuation."
    (declare (type string expected-string))
    `(the boolean
       (when (and *source-has-next-character-p*
                  (substring-starts-at-p
                    *source-code*
                    *source-position*
                    ,expected-string))
         (incf *source-position*
           (length ,expected-string))
         T))))

;;; -------------------------------------------------------

(defmacro match-content (expected-content)
  "Probes the input source's current character or characters for their
   compliance with the EXPECTED-CONTENT, returning a generalized boolean
   \"true\" value upon its confirmation, otherwise \"false\"."
  (generate-content-match-code expected-content))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type matching code generator.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-type-match-code (expected-type target-place)
  "Generates and returns a Common Lisp code tmema which probes whether
   the current *SOURCE-CHARACTER* complies with the EXPECTED-TYPE, on
   confirmation storing the probed character in the TARGET-PLACE,
   while advancing to the next position in the *SOURCE-CODE*, finally
   returning a ``boolean'' value of ``T''; otherwise responds with
   ``NIL'' without any epiphenomena's actuation."
  (declare (type T expected-type))
  (declare (type T target-place))
  `(the boolean
     (when (and *source-has-next-character-p*
                (typep *source-character* ',expected-type))
       (setf ,target-place *source-character*)
       (incf *source-position*)
       T)))

;;; -------------------------------------------------------

(defmacro match-type (expected-type target-place)
  "Probes the input source's current character for its compliance with
   the EXPECTED-TYPE, storing on confirmation the probed character in
   the TARGET-PLACE, while returning a generalized boolean \"true\"
   value; otherwise responds with a \"false\" sentinel."
  (generate-type-match-code expected-type target-place))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression parser.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compile-expression (expression)
  (:documentation
    "Generates and returns a Common Lisp code tmema which matches the
     EXPRESSION against the current input source state, and which on a
     match returns a generalized boolean value of \"true\", otherwise
     ``NIL''.")
  
  (:method ((expression character))
    "Generates and returns a Common Lisp code tmema which matches the
     EXPRESSION character against the current input source character,
     and which upon its owelty advances to the next position in the
     source, while returning a generalized boolean \"true\" value;
     otherwise, the resulting code abstains from the position cursor's
     advancement, instead returning ``NIL''."
    (declare (type character expression))
    `(the boolean
       (match-content ,expression)))
  
  (:method ((expression string))
    "Generates and returns a Common Lisp code tmema which matches the
     EXPRESSION string against the input source characters commencing
     at its current position, and which upon an owelty advances the
     position cursor beyond the matching source segment, while returning
     a generalized boolean \"true\" value; otherwise, the resulting code
     abstains from the position cursor's advancement, instead returning
     ``NIL''."
    (declare (type string expression))
    `(the boolean
       (match-content ,expression)))
  
  (:method ((expression META-Object))
    "Generates and returns a Common Lisp code tmema which represents the
     META EXPRESSION's functionality, and which for a match responds
     with a generalized boolean \"true\" value, otherwise with ``NIL''."
    (declare (type META-Object expression))
    (the T
      (compile-meta-object expression))))

;;; -------------------------------------------------------

(defmacro match-expression (expression)
  "Matches the EXPRESSIONS against the *SOURCE-CODE*'s contemporaneous
   state, returning on its compliance a generalized boolean \"true\",
   otherwise \"false\"."
  (compile-expression expression))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of reader macros.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Register the reader macro for the ``META-Alternative'' expression.
;; This macro consumes a series of Common Lisp forms until the closing
;; "}" token is encountered, and supply these forms to a freshly created
;; META alternative representation.
(set-macro-character #\{
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Alternative
        (make-meta-alternative
          (read-delimited-list #\} stream T)))))

;;; -------------------------------------------------------

;; Communicate the equivalency of the "}" token and the list terminator
;; symbol ")", which imposes a requisite for META alternative
;; expressions to wist the termination criterion during their collection
;; of forms, instigated via a "{" character.
(set-macro-character #\}
  (get-macro-character #\) NIL))

;;; -------------------------------------------------------

;; Register the reader macro for the ``META-Sequence'' expression.
;; This macro consumes a series of Common Lisp forms until the closing
;; "]" token is encountered, and supply these forms to a freshly created
;; META sequence representation.
(set-macro-character #\[
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Sequence
        (make-meta-sequence
          (read-delimited-list #\] stream T)))))

;;; -------------------------------------------------------

;; Communicate the equivalency of the "]" token and the list terminator
;; symbol ")", which imposes a requisite for META sequence expressions
;; to wist the termination criterion during their collection of forms,
;; instigated via a "[" character.
(set-macro-character #\]
  (get-macro-character #\) NIL))

;;; -------------------------------------------------------

;; Register the reader macro for the ``META-Repetition'' expression.
;; This macro consumes the next Common Lisp form and supply thilk to a
;; freshly created META repetition representation.
(set-macro-character #\$
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Repetition
        (make-meta-repetition
          (read stream)))))

;;; -------------------------------------------------------

;; Register the reader macro for the ``META-Evaluation'' expression.
;; This macro consumes the next Common Lisp form and supply thilk to a
;; freshly created META evaluation representation.
(set-macro-character #\!
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Evaluation
        (make-meta-evaluation
          (read stream)))))

;;; -------------------------------------------------------

;; Register the reader macro for the ``META-Type-Test'' expression.
;; This macro consumes the next Common Lisp form, expected to constitute
;; a list of two elements, construes the first sub-form as a type
;; specifier and the second as a place for data storage, and supplies
;; this twissel to a freshly created META type test representation.
(set-macro-character #\@
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (destructuring-bind (expected-type receiving-place)
          (read stream)
        (declare (type T expected-type))
        (declare (type T receiving-place))
        (the META-Type-Test
          (make-meta-type-test expected-type receiving-place)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular META parser operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-saved-position
    ((saved-position-variable restore-function-name)
     &body body)
  "Memorizes the *SOURCE-POSITION* by storing thilk in a local variable
   norned by the SAVED-POSITION-VARIABLE, defines a local function
   whose agnomination resolves to the RESTORE-FUNCTION-NAME, and to whom
   is apportioned the wike of accompassing the *SOURCE-POSITION*'s
   restoration to the SAVED-POSITION-VARIABLE upon its invocation,
   evaluates the BODY forms, and returns the desinent form's results.
   ---
   The function amenable to the RESTORE-FUNCTION-NAME accepts one
   optional argument, the same specifies its returns value, defaulting
   to ``NIL''."
  `(let ((,saved-position-variable *source-position*))
     (declare (type fixnum ,saved-position-variable))
     (flet ((,restore-function-name (&key (return-value NIL))
              "Restores the *SOURCE-POSITION* to the index stored in the
               SAVED-POSITION-VARIABLE and responds with the
               RETURN-VALUE, the same defaults to ``NIL''."
              (declare (type T return-value))
              (setf *source-position* ,saved-position-variable)
              (the T return-value)))
       ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of VarStack command classes.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface serves in a firmament's establishment upon
   which shall be edified all classes pursuing the telos of a VarStack
   instruction's representation.")

;;; -------------------------------------------------------

(defstruct (Create-Stack-Command
  (:include Command))
  "The ``Create-Stack-Command'' class implements the VarStack new stack
   creation operation, following the forbisen
   \"@stackName:initialElements\"."
  (stack            (error "Missing stack name.")
                    :type      simple-string
                    :read-only T)
  (initial-elements (error "Missing initial elements.")
                    :type      simple-string
                    :read-only T))

;;; -------------------------------------------------------

(defstruct (Push-Command
  (:include Command))
  "The ``Push-Command'' class implements the VarStack element insertion
   command operation \"$stackName:newElement\"."
  (new-element (error "Missing new element.")
               :type      character
               :read-only T)
  (stack       (error "Missing stack name.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Pop-Command
  (:include Command))
  "The ``Pop-Command'' class implements the VarStack stack top element
   removal operation, following the forbisen \"!stackName\"."
  (stack (error "Missing stack name.")
         :type      simple-string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Literal-Command
  (:include Command))
  "The ``Output-Literal-Command'' class implements the VarStack literal
   print operation, following the forbisen \"'stackName\"."
  (argument (error "Missing print argument.")
            :type      simple-string
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Stack-Command
  (:include Command))
  "The ``Output-Stack-Command'' class implements the VarStack stack
   content print operation, following the forbisen \"'stackName\"."
  (stack (error "Missing stack name.")
         :type      simple-string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Input-Command
  (:include Command))
  "The ``Input-Command'' class implements the VarStack user input
   reception operation, following the forbisen \"?stackName\"."
  (stack (error "Missing stack name.")
         :type      simple-string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Jump-If-Equals-Literal-Command
  (:include Command))
  "The ``Jump-If-Equals-Literal-Command'' class implements the VarStack
   conditional jump operation which relies on an affirmative
   juxtaposition with a literal character, following the forbisen
   \"#stackName=guard:destination\"."
  (stack       (error "Missing stack.")
               :type      simple-string
               :read-only T)
  (guard       (error "Missing guard value.")
               :type      character
               :read-only T)
  (destination (error "Missing destination label.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Jump-If-Does-Not-Equal-Literal-Command
  (:include Command))
  "The ``Jump-If-Does-Not-Equal-Literal-Command'' class implements the
   VarStack conditional jump operation which relies on a negative
   juxtaposition with a literal character, following the forbisen
   \"!stackName=guard:destination\"."
  (stack       (error "Missing stack.")
               :type      simple-string
               :read-only T)
  (guard       (error "Missing guard value.")
               :type      character
               :read-only T)
  (destination (error "Missing destination label.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Jump-If-Equals-Stack-Command
  (:include Command))
  "The ``Jump-If-Equals-Stack-Command'' class implements the VarStack
   conditional jump operation which relies on an affirmative
   juxtaposition betwixt two stack's top elements, following the
   forbisen \"%firstStackName=secondStackName:destination\"."
  (first-stack  (error "Missing first stack.")
                :type      simple-string
                :read-only T)
  (second-stack (error "Missing second stack.")
                :type      simple-string
                :read-only T)
  (destination (error "Missing destination label.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Swap-Command
  (:include Command))
  "The ``Swap-Command'' class implements the VarStack top stack elements
   swapping operation, following the forbisen \"~stackName\"."
  (stack (error "Missing stack name.")
         :type      simple-string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Reverse-Command
  (:include Command))
  "The ``Reverse-Command'' class implements the VarStack stack elements'
   reversal operation, following the forbisen \"&stackName\"."
  (stack (error "Missing stack name.")
         :type      simple-string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Top-Relocation-Command
  (:include Command))
  "The ``Top-Relocation-Command'' class implements the VarStack top
   stack element to bottom relocation operation, following the forbisen
   \"^stackName\"."
  (stack (error "Missing stack name.")
         :type      simple-string
         :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Statement".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Statement
  "The ``Statement'' interface serves in the furnishment of a substratum
   for all classes intended in representation of VarStack statements.")

;;; -------------------------------------------------------

(defstruct (Standard-Statement
  (:include Statement))
  "The ``Standard-Statement'' type is apportioned the wike of a VarStack
   line's modeling in a coherent guise, its componency intrining the
   membership of the identifying line label, the command to execute, and
   the subsequent destination label whose linkage to seek.
   ---
   A VarStack statement, or line, ostends, in its most abstract
   principle's diorism, a conformation intrining the components
     lineLabel;command;destinationLabel
   The the interstitial second parcel, the \"command\", please consult
   the ``Command'' interface and its multifarious implementations."
  (line-label  (error "Missing line label for statement.")
               :type      simple-string
               :read-only T)
  (command     (error "Missing command for statement.")
               :type      Command
               :read-only T)
  (destination (error "Missing destination for statement.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (NOP-Statement
  (:include Statement))
  "The ``NOP-Statement'' class serves in the representation of a blank
   statement line extracted from a parsed VarStack program.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-nop-statements (statements)
  "Removes, contingently by destructive modification, all
   ``Nop-Statement'' objects from the STATEMENTS list and returns the
   resulting list.
   ---
   Please heed that the STATEMENTS may or may not be destructively
   modified by this operation; as a consectary, the resulting list may
   either represent the STATEMENTS list itself or a freshly created
   one, depending on the respective Common Lisp implementation."
  (declare (type (list-of Statement) statements))
  (the (list-of Statement)
    (delete-if #'nop-statement-p statements)))

;;; -------------------------------------------------------

(defun make-varstack-program (statements)
  "Creates and returns a fresh VarStack ``program'' from the list of
   STATEMENTS."
  (declare (type (list-of Statement) statements))
  (the program
    (coerce statements
      '(simple-array Statement (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of character 2) +SPACE-CHARACTERS+))
(declaim (type (list-of character 4) +NEWLINE-CHARACTERS+))

;;; -------------------------------------------------------

(defparameter +SPACE-CHARACTERS+
  (list (code-char 9) (code-char 32))
  "Enumerates the characters capacitated to segregate words.")

(defparameter +NEWLINE-CHARACTERS+
  (list (code-char 10) (code-char 11) (code-char 12) (code-char 13))
  "Enumerates the characters capacitated to terminate a line.")

;;; -------------------------------------------------------

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal tab
   character, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate +SPACE-CHARACTERS+ :test #'char=))))

;;; -------------------------------------------------------

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a character covenable for
   the accommodation of a linebreak, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate +NEWLINE-CHARACTERS+ :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-simple-string (source)
  "Creates and returns a simple string representation of the SOURCE
   string."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents an empty string, such is
   imparted the diorism of a zero-length character sequence, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (get-boolean-value-of
      (zerop
        (length source)))))

;;; -------------------------------------------------------

(defun remove-surrounding-spaces (source)
  "Returns a fresh simple string based on the SOURCE with all
   surrounding spacing characters, which entails in its diorism the
   traditional space and the horizontal tab, ejected."
  (declare (type simple-string source))
  (the simple-string
    (convert-into-simple-string 
      (string-trim +SPACE-CHARACTERS+ source))))

;;; -------------------------------------------------------

(defun string-contains-spaces-p (source)
  "Determines whether the SOURCE string contains one or more spacing
   character, entailing in this diorism the traditional space as well as
   the horizontal tab, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (get-boolean-value-of
      (plusp
        (count-if #'space-character-p source)))))

;;; -------------------------------------------------------

(defun convert-string-into-list (source)
  "Creates and returns a fresh list comprehending the SOURCE string's
   characters in their specified order."
  (declare (type simple-string source))
  (the (list-of character)
    (coerce source 'list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types, implementation of type predicates.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type identifier-character (candidate)
  "The ``identifier-character'' type defines a character admissible as
   a constituent in a label or command name."
  (and
    candidate
    (characterp candidate)
    (not (newline-character-p candidate))
    (not (member candidate '(#\: #\= #\;) :test #'char=))))

;;; -------------------------------------------------------

(define-predicated-type newline-character (candidate)
  "The ``newline-character'' type defines a character whose membership
   is subscribed to a linebreaking purpose."
  (and
    candidate
    (characterp          candidate)
    (newline-character-p candidate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier validation operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-label-name (name)
  "Determines whether the NAME is invested with sufficient concinnity
   regarding its employment as a label identifier, on confirmation
   returning the same in its ipissima verba designment; otherwise
   signals an error of an unspecified type."
  (declare (type simple-string name))
  (let ((trimmed-name (remove-surrounding-spaces name)))
    (declare (type simple-string trimmed-name))
    (cond
      ((string-is-empty-p trimmed-name)
        (error "A label name must not be empty: ~s." trimmed-name))
      ((string-contains-spaces-p trimmed-name)
        (error "A label name must not contains spaces: ~s."
          trimmed-name))
      (T
        trimmed-name))))

;;; -------------------------------------------------------

(defun validate-stack-name (name)
  "Determines whether the NAME is invested with sufficient concinnity
   regarding its employment as a stack identifier, on confirmation
   returning the same in its ipissima verba designment; otherwise
   signals an error of an unspecified type."
  (declare (type simple-string name))
  (the simple-string
    (or (and (not (string-is-empty-p name))
             name)
        (error "Invalid stack name: ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of dedicated VarStack parsing operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces ()
  "Proceeding from the current position into the *SOURCE-CODE*, skips a
   catena composed of zero or more accolent spaces, and consistently
   returns a ``boolean'' value of ``T''."
  (match-expression ${#\Space #\Tab})
  (the boolean T))

;;; -------------------------------------------------------

(defun skip-newlines ()
  "Proceeding from the current position into the *SOURCE-CODE*, skips a
   catena composed of zero or more accolent newline characters, and
   consistently returns a ``boolean'' value of ``T''."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (declare (ignorable                current-character))
    (match-expression $@(newline-character current-character)))
  (the boolean T))

;;; -------------------------------------------------------

(defun read-identifier ()
  "Proceeding from the current *SOURCE-POSITION*, consumes an identifier
   compact of zero or more eligible characters, and returns a fresh
   simple string representation of the result."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (the simple-string
      (convert-into-simple-string
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (match-expression
            $[@(identifier-character current-character)
              !(write-char current-character identifier)]))))))

;;; -------------------------------------------------------

(defun parse-label-name ()
  "Proceeding from the current *SOURCE-POSITION*, consumes an
   identifier, validates its concinnity with the label designation
   stipulations, and returns upon its confirmation the unmodified
   identifier as a simple string; otherwise, for an invalid
   constitution, signals an error of an unspecified type."
  (the simple-string
    (validate-label-name
      (read-identifier))))

;;; -------------------------------------------------------

(defun parse-stack-name ()
  "Proceeding from the current *SOURCE-POSITION*, consumes an
   identifier, validates its concinnity with the stack designation
   stipulations, and returns upon its confirmation the unmodified
   identifier as a simple string; otherwise, for an invalid
   constitution, signals an error of an unspecified type."
  (the simple-string
    (validate-stack-name
      (read-identifier))))

;;; -------------------------------------------------------

(defun parse-create-stack-command ()
  "Attempts to parse a VarStack new stack creation command, following
   the forbisen \"@stackName:initialElements\", and returning on
   confirmation a fresh ``Create-Stack-Command'' representation thereof;
   otherwise responds with ``NIL''."
  (the (or null Create-Stack-Command)
    (let ((stack-name       NIL)
          (initial-elements NIL))
      (declare (type (or null simple-string) stack-name))
      (declare (type (or null simple-string) initial-elements))
      (and
        (match-expression
          [#\@
           !(setf stack-name       (parse-stack-name))
           #\:
           !(setf initial-elements (read-identifier))])
        (make-create-stack-command
          :stack            stack-name
          :initial-elements initial-elements)))))

;;; -------------------------------------------------------

(defun parse-push-command ()
  "Attempts to parse a VarStack new element insertion command, following
   the forbisen \"$newElement:stackName\", and returning on confirmation
   a fresh ``Push-Command'' representation thereof; otherwise responds
   with ``NIL''."
  (the (or null Push-Command)
    (let ((new-element     NIL)
          (receiving-stack NIL))
      (declare (type (or null character)     new-element))
      (declare (type (or null simple-string) receiving-stack))
      (and
        (match-expression
          [#\$
           @(identifier-character new-element)
           !(skip-spaces)
           #\:
           !(setf receiving-stack (parse-stack-name))])
        (make-push-command
          :new-element new-element
          :stack       receiving-stack)))))

;;; -------------------------------------------------------

(defun parse-pop-command ()
  "Attempts to parse a VarStack stack element removal command, following
   the forbisen \"!stackName\", and returning on confirmation a fresh
   ``Pop-Command'' representation thereof; otherwise responds with
   ``NIL''."
  (the (or null Pop-Command)
    (let ((source-stack NIL))
      (declare (type (or null simple-string) source-stack))
      (and
        (match-expression
          [#\!
           !(setf source-stack (parse-stack-name)) ])
        (make-pop-command :stack source-stack)))))

;;; -------------------------------------------------------

(defun parse-output-literal-command ()
  "Attempts to parse a VarStack literal printing command, following
   the forbisen \"\"message\", and returning on confirmation a fresh
   ``Output-Literal-Command'' representation thereof; otherwise responds
   with ``NIL''."
  (the (or null Output-Literal-Command)
    (let ((argument NIL))
      (declare (type (or null simple-string) argument))
      (and
        (match-expression
          [#\"
           !(setf argument (read-identifier))])
        (make-output-literal-command :argument argument)))))

;;; -------------------------------------------------------

(defun parse-output-stack-command ()
  "Attempts to parse a VarStack stack printing command, following the
   forbisen \"'stackName\", and returning on confirmation a fresh
   ``Output-Stack-Command'' representation thereof; otherwise responds
   with ``NIL''."
  (the (or null Output-Stack-Command)
    (let ((stack NIL))
      (declare (type (or null simple-string) stack))
      (and
        (match-expression
          [#\'
           !(setf stack (parse-stack-name))])
        (make-output-stack-command :stack stack)))))

;;; -------------------------------------------------------

(defun parse-input-command ()
  "Attempts to parse a VarStack input request command, following the
   forbisen \"?stackName\", and returning on confirmation a fresh
   ``Input-Command'' representation thereof; otherwise responds with
   ``NIL''."
  (the (or null Input-Command)
    (let ((receiving-stack NIL))
      (declare (type (or null simple-string) receiving-stack))
      (and
        (match-expression
          [#\?
           !(setf receiving-stack (parse-stack-name))])
        (make-input-command :stack receiving-stack)))))

;;; -------------------------------------------------------

(defun parse-jump-if-equals-literal-command ()
  "Attempts to parse a VarStack conditional jumping command based on the
   equality to a specific character literal, following the forbisen
   \"#stackName=guard:destination\", and returning on confirmation a
   fresh ``Jump-If-Equals-Literal-Command'' representation thereof;
   otherwise responds with ``NIL''."
  (the (or null Jump-If-Equals-Literal-Command)
    (let ((stack       NIL)
          (guard       NIL)
          (destination NIL))
      (declare (type (or null simple-string) stack))
      (declare (type (or null character)     guard))
      (declare (type (or null simple-string) destination))
      (and
        (match-expression
          [#\#
           !(setf stack (parse-stack-name))
           #\=
           @(identifier-character guard)
           #\:
           !(setf destination (parse-label-name))])
        (make-jump-if-equals-literal-command
          :stack       stack
          :guard       guard
          :destination destination)))))

;;; -------------------------------------------------------

(defun parse-jump-if-does-not-equal-literal-command ()
  "Attempts to parse a VarStack conditional jumping command based on the
   inequality to a specific character literal, following the forbisen
   \"!stackName=guard:destination\", and returning on confirmation a
   fresh ``Jump-If-Does-Not-Equal-Literal-Command'' representation
   thereof; otherwise responds with ``NIL''."
  (the (or null Jump-If-Does-Not-Equal-Literal-Command)
    (let ((stack       NIL)
          (guard       NIL)
          (destination NIL))
      (declare (type (or null simple-string) stack))
      (declare (type (or null character)     guard))
      (declare (type (or null simple-string) destination))
      (with-saved-position (old-position reset-position)
        (or
          (and
            (match-expression
              [#\!
               !(setf stack (parse-stack-name))
               #\=
               @(identifier-character guard)
               #\:
               !(setf destination (parse-label-name))])
            (make-jump-if-does-not-equal-literal-command
              :stack       stack
              :guard       guard
              :destination destination))
          (reset-position :return-value NIL))))))

;;; -------------------------------------------------------

(defun parse-jump-if-equals-stack-command ()
  "Attempts to parse a VarStack conditional jumping command based on the
   equality betwixt two stacks' top elements, following the forbisen
   \"%firstStackName=secondStackName:destination\", and returning on
   confirmation a fresh ``Jump-If-Equals-Stack-Command'' representation
   thereof; otherwise responds with ``NIL''."
  (the (or null Jump-If-Equals-Stack-Command)
    (let ((first-stack  NIL)
          (second-stack NIL)
          (destination  NIL))
      (declare (type (or null simple-string) first-stack))
      (declare (type (or null simple-string) second-stack))
      (declare (type (or null simple-string) destination))
      (and
        (match-expression
          [#\%
           !(setf first-stack (parse-stack-name))
           #\=
           !(setf second-stack (parse-stack-name))
           #\:
           !(setf destination (parse-label-name))])
        (make-jump-if-equals-stack-command
          :first-stack  first-stack
          :second-stack second-stack
          :destination  destination)))))

;;; -------------------------------------------------------

(defun parse-swap-command ()
  "Attempts to parse a VarStack stack top elements exchange command,
   following the forbisen \"~stackName\", and returning on confirmation
   a fresh ``Swap-Command'' representation thereof; otherwise responds
   with ``NIL''."
  (the (or null Swap-Command)
    (let ((affected-stack NIL))
      (declare (type (or null simple-string) affected-stack))
      (and
        (match-expression
          [#\~
           !(setf affected-stack (parse-stack-name))])
        (make-swap-command :stack affected-stack)))))

;;; -------------------------------------------------------

(defun parse-reverse-command ()
  "Attempts to parse a VarStack stack reversal command, following the
   forbisen \"~stackName\", and returning on confirmation a fresh
   ``Reverse-Command'' representation thereof; otherwise responds
   with ``NIL''."
  (the (or null Reverse-Command)
    (let ((affected-stack NIL))
      (declare (type (or null simple-string) affected-stack))
      (and
        (match-expression
          [#\&
           !(setf affected-stack (parse-stack-name))])
        (make-reverse-command :stack affected-stack)))))

;;; -------------------------------------------------------

(defun parse-top-relocation-command ()
  "Attempts to parse a VarStack stack top element to bottom relocation
   command, following the forbisen \"^stackName\", and returning on
   confirmation a fresh ``Top-Relocation-Command'' representation
   thereof; otherwise responds with ``NIL''."
  (the (or null Top-Relocation-Command)
    (let ((affected-stack NIL))
      (declare (type (or null simple-string) affected-stack))
      (and
        (match-expression
          [#\^
           !(setf affected-stack (parse-stack-name))])
        (make-top-relocation-command :stack affected-stack)))))

;;; -------------------------------------------------------

;; Given a line (statement) compact of:
;;   lineLabel;command;destination
;;             *******
;; Parses:
;;   command
(defun parse-command ()
  "Parses a VarStack command returns a covenable ``Command''
   representation of this effort; otherwise, upon its disrespondency,
   signals an error of an unspecified type."
  (the Command
    (or
      (match-expression
        [!(skip-spaces)
         {!(parse-jump-if-equals-literal-command)
          !(parse-jump-if-does-not-equal-literal-command)
          !(parse-jump-if-equals-stack-command)
          !(parse-create-stack-command)
          !(parse-push-command)
          !(parse-pop-command)
          !(parse-output-literal-command)
          !(parse-output-stack-command)
          !(parse-input-command)
          !(parse-swap-command)
          !(parse-reverse-command)
          !(parse-top-relocation-command)}])
      (error "No command found at position ~d, introducing the ~
              source code tmema ~s."
        *source-position*
        (subseq *source-code* *source-position*)))))

;;; -------------------------------------------------------

(defun end-of-line-follows-p ()
  "Determines whether the next character in the *SOURCE-CODE* represents
   a newline entity, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (the boolean
    (let ((current-character NIL))
      (declare (type (or null character) current-character))
      (declare (ignorable                current-character))
      (or
        (not *source-has-next-character-p*)
        (with-saved-position (old-position reset-position)
          (and
            (match-expression @(newline-character current-character))
            (reset-position :return-value T)))))))

;;; -------------------------------------------------------

(defun parse-blank-line ()
  "Attempts to parse a blank code line, homologating in this
   conformation solely spaces, on success returning a ``NOP-Statement''
   representation; otherwise responds with the ``NIL'' value."
  (the (or null NOP-Statement)
    (with-saved-position (old-position reset-position)
      (or
        (and
          (match-expression
            [!(skip-spaces)
             !(end-of-line-follows-p)])
          (make-nop-statement))
        (reset-position :return-value NIL)))))

;;; -------------------------------------------------------

(defun parse-effective-statement ()
  "Attempts to parse a VarStack standard statement, including in its
   componency a line label, a command, and a destination label, on
   success returning a covenable ``Standard-Statement'' representation
   thereof; otherwise responds with the ``NIL'' value."
  (the (or null Standard-Statement)
    (let ((line-label  NIL)
          (command     NIL)
          (destination NIL))
      (declare (type (or null simple-string) line-label))
      (declare (type (or null Command)       command))
      (declare (type (or null simple-string) destination))
      (and
        (match-expression
          [!(setf line-label  (parse-label-name))
           #\;
           !(setf command     (parse-command))
           #\;
           !(setf destination (parse-label-name))])
        (make-standard-statement
          :line-label  line-label
          :command     command
          :destination destination)))))

;;; -------------------------------------------------------

(defun parse-line ()
  "Parses a VarStack statement or a blank line and returns a covenable
   ``Statement'' representation thereof.
   ---
   A failure in the regulated statement conformation's perception will
   instigate the signaling of an error of an unspecified type."
  (the Statement
    (or
      (parse-blank-line)
      (parse-effective-statement)
      (error "Invalid statement line."))))

;;; -------------------------------------------------------

(defun expect-statement-sepiment ()
  "Attempts to parse a single newline character in the form of a
   statement terminator or a merist betwixt two such specimens,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (declare (ignorable                current-character))
    (the boolean
      (match-expression
        @(newline-character current-character)))))

;;; -------------------------------------------------------

(defun expect-end-of-source ()
  "Proceeding from the current position into the *SOURCE-CODE*, expects
   the same to furnish no characters, except for the contingency for
   spaces, returning on confirmation a ``boolean'' value of ``T'';
   otherwise signals an error of an unspecified type."
  (the boolean
    (or
      (match-expression
        [!(skip-spaces)
         !(not *source-has-next-character-p*)])
      (error "Expected the end of the program."))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Parses a VarStack program and returns a ``program'' representation
   of its ensconced statement lines their specified order."
  (the program
    (let ((statements        NIL)
          (current-statement NIL))
      (declare (type (list-of Statement) statements))
      (declare (type (or null Statement) current-statement))
      (match-expression
        [
          !(setf current-statement (parse-line))
          !(push current-statement statements)
          
          $[!(expect-statement-sepiment)
            !(setf current-statement (parse-line))
            !(push current-statement statements)]
          
          !(expect-end-of-source)
        ])
      (make-varstack-program
        (remove-nop-statements
          (nreverse statements))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of simple-string fixnum)
    :documentation "Associates the line labels with their zero-based
                    positions into the parsed VarStack program."))
  (:documentation
    "The ``Label-Table'' class applies itself to a VarStack program
     labels' castaldy, affiliating in this wike the identifying names
     with their zero-based positions into the ensconcing program."))

;;; -------------------------------------------------------

(defun prepare-empty-label-table ()
  "Creates and returns a fresh and initially empty ``Label-Table''."
  (the Label-Table
    (make-instance 'Label-Table)))

;;; -------------------------------------------------------

(defun label-with-name-exists-p (labels probed-name)
  "Determines whether a label amenable to the PROBED-NAME is present in
   the LABELS, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table   labels))
  (declare (type simple-string probed-name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash probed-name
          (slot-value labels 'entries))))))

;;; -------------------------------------------------------

(defun register-label (labels label-name line-number)
  "Associates the LABEL-NAME with its zero-based LINE-NUMBER in the
   respective VarStack program, registers the affiliation in the LABELS
   table, and returns no value.
   ---
   Upon the presence of an entry for the LABEL-NAME, an error of an
   unspecified type is signaled."
  (declare (type Label-Table   labels))
  (declare (type simple-string label-name))
  (declare (type fixnum        line-number))
  (if (label-with-name-exists-p labels label-name)
    (error "The label name ~s has already been assigned." label-name)
    (with-slots (entries) labels
      (declare (type (hash-table-of simple-string fixnum) entries))
      (setf (gethash label-name entries) line-number)))
  (values))

;;; -------------------------------------------------------

(defun locate-label (labels label-name)
  "Returns the zero-based position of the label amenable to the
   LABEL-NAME inside of the ensconcing VarStack program, as registered
   at the LABELS table; or responds with ``NIL'' upon its absence."
  (declare (type Label-Table   labels))
  (declare (type simple-string label-name))
  (the (or null fixnum)
    (gethash label-name
      (slot-value labels 'entries))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of statement label collector.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-statement-labels (program)
  "Creates and returns a fresh ``Label-Table'' whose definitions are
   founded upon the VarStack PROGRAM's statements."
  (declare (type program program))
  (let ((labels (prepare-empty-label-table)))
    (declare (type Label-Table labels))
    (loop
      for current-statement   of-type Statement across program
      and current-line-number of-type fixnum    from   0
      when (standard-statement-p current-statement) do
        (register-label labels
            (standard-statement-line-label current-statement)
            current-line-number))
    (the Label-Table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory stack.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack ()
  ((elements
    :initarg       :elements
    :initform      NIL
    :accessor      stack-elements
    :type          (list-of character)
    :documentation "The elements as a singly linked list of
                    characters, with the first position tantamount to
                    the stack top, and the desinent one signifying the
                    bottom."))
  (:documentation
    "The ``Stack'' class furnishes an implementation of the VarStack
     program memory's stack, its firmament that of a singly linked list,
     admitting merely characters to its membership."))

;;; -------------------------------------------------------

(defun create-empty-stack ()
  "Creates and returns a fresh and initially empty ``Stack''."
  (the Stack
    (make-instance 'Stack)))

;;; -------------------------------------------------------

(defun create-stack-initialized-with (initial-elements)
  "Creates and returns a fresh ``Stack'' whose inchoate state is defined
   by the INITIAL-ELEMENTS.
   ---
   Please heed that the newly created stack's order will reflect the
   INITIAL-ELEMENTS' membership in a verbatim fashion, that is, the
   first element desumed from the list will constitute the stack's top
   item, the desinent input sequence member the recipient's bottom."
  (declare (type (list-of character) initial-elements))
  (the Stack
    (make-instance 'Stack :elements
      (copy-list initial-elements))))

;;; -------------------------------------------------------

(defun stack-is-empty-p (stack)
  "Determines whether the STACK is destitute of elements, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Stack stack))
  (the boolean
    (null
      (stack-elements stack))))

;;; -------------------------------------------------------

(defun stack-contains-at-least-two-elements-p (stack)
  "Determines whether the STACK's componency enumerates at least two
   elements, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Stack stack))
  (the boolean
    (with-slots (elements) stack
      (declare (type (list-of character) elements))
      (get-boolean-value-of
        (and elements
             (rest elements))))))

;;; -------------------------------------------------------

(defun push-onto-stack (stack new-element)
  "Inserts the NEW-ELEMENT at the STACK's top position and returns no
   value."
  (declare (type Stack     stack))
  (declare (type character new-element))
  (push new-element
    (stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun peek-into-stack (stack)
  "Returns without removing the STACK's top element, or, upon its
   vacancy, responds with ``NIL''."
  (declare (type Stack stack))
  (the (or null character)
    (first
      (stack-elements stack))))

;;; -------------------------------------------------------

(defun pop-from-stack (stack)
  "Removes and returns the STACK's top element, or, upon its vacancy,
   responds with ``NIL''."
  (declare (type Stack stack))
  (the (or null character)
    (pop
      (stack-elements stack))))

;;; -------------------------------------------------------

(defun swap-top-stack-elements (stack)
  "Swaps the positions of the STACK's two topmost elements and returns
   no value.
   ---
   If the STACK is empty or entails an aefauld element only, no further
   causatum is accompassed."
  (declare (type Stack stack))
  (when (stack-contains-at-least-two-elements-p stack)
    (with-slots (elements) stack
      (declare (type (list-of character) elements))
      (rotatef
        (first  elements)
        (second elements))))
  (values))

;;; -------------------------------------------------------

(defun reverse-stack (stack)
  "Reverses the order of the STACK's elements and returns no value."
  (declare (type Stack stack))
  (when (stack-contains-at-least-two-elements-p stack)
    (setf (stack-elements stack)
      (nreverse
        (stack-elements stack))))
  (values))

;;; -------------------------------------------------------

(defun relocate-stack-top-to-bottom (stack)
  "Relocates the STACK's top element to the bottom position and returns
   no value.
   ---
   If the STACK is empty or entails an aefauld element onlz, no further
   causatum is accompassed."
  (declare (type Stack stack))
  (when (stack-contains-at-least-two-elements-p stack)
    (with-slots (elements) stack
      (declare (type (list-of character) elements))
      (let ((bottom-element (first (last elements))))
        (declare (type character bottom-element))
        (setf elements
          (nbutlast elements))
        (push bottom-element elements))))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((stack Stack) (stream T))
  (declare (type Stack       stack))
  (declare (type destination stream))
  (format stream "(Stack [top>~{ ~c~^,~} <bottom])"
    (stack-elements stack)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of stack table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack-Table ()
  ((stacks
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of simple-string  Stack)
    :documentation "Affiliates the registered stacks with their unique
                    identification strings."))
  (:documentation
    "The ``Stack-Table'' class applies itself to the castaldy of stacks,
     their adit founded upon an amenability to their identifying
     names."))

;;; -------------------------------------------------------

(defun prepare-empty-stack-table ()
  "Creates and returns a fresh and initially empty ``Stack-Table''."
  (the Stack-Table
    (make-instance 'Stack-Table)))

;;; -------------------------------------------------------

(defun stack-with-name-exists-p (table name)
  "Determines whether a stack amenable to the NAME exists in the stack
   TABLE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Stack-Table   table))
  (declare (type simple-string name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash name
          (slot-value table 'stacks))))))

;;; -------------------------------------------------------

(defun register-stack (table name new-stack)
  "Affiliates the NEW-STACK with the NAME in the stack TABLE and returns
   no value."
  (declare (type Stack-Table   table))
  (declare (type simple-string name))
  (declare (type Stack         new-stack))
  (if (stack-with-name-exists-p table name)
    (error "A stack with the name ~s already exists." name)
    (with-slots (stacks) table
      (declare (type (hash-table-of simple-string Stack) stacks))
      (setf (gethash name stacks) new-stack)))
  (values))

;;; -------------------------------------------------------

(defun get-stack-with-name (table name)
  "Returns the stack registered under the NAME at the stack TABLE; or
   responds with ``NIL'' upon its absence."
  (declare (type Stack-Table   table))
  (declare (type simple-string name))
  (the (or null Stack)
    (gethash name
      (slot-value table 'stacks))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ()
  (:documentation
    "The ``Halt-Condition'' condition type serves in the communication
     of a behest airted at an executed VarStack program's immediate
     cessation."))

;;; -------------------------------------------------------

(defun halt-program ()
  "Signals a ``Halt-Condition''."
  (signal 'Halt-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :type          program
    :documentation "The VarStack statements to evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The currently processed, zero-based line number.")
   (labels
    :initform      (prepare-empty-label-table)
    :type          Label-Table
    :documentation "Maintains the statement labels in an assocation
                    betwixt their unique names and their zero-based
                    statements' indices into the PROGRAMS.")
   (jump-destination
    :initform      NIL
    :type          (or null simple-string)
    :documentation "The jump destination label name specified by certain
                    conditional goto operations, if desiderated.
                    ---
                    Upon this value's equality with ``NIL'', the
                    interpreter assumes that no explicit jump has been
                    intended, in which case the current statement's
                    destination label will furnish the next statement to
                    execute.")
   (stacks
    :initform      (prepare-empty-stack-table)
    :type          Stack-Table
    :documentation "Maintains a collection of stacks, amenable to
                    unique names."))
  (:documentation
    "The ``Interpreter'' class is apportioned is wike of imbuing a
     parsed VarStack program with actual efficacy."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``program'' to the local
   symbol macro ``<program>'', ``ip'' to ``<ip>'', ``labels'' to
   ``<labels>'', ``jump-destination'' to ``<jump-destination>'', and
   ``stacks'' to ``<stacks>'', evaluates the BODY forms, and returns
   the desinent form's results.
   ---
   An apercu's furnishment anent the extant bindings shall facilitate
   their enjoyment:
     ------------------------------------------------------------------
     Definition         | Purpose
     -------------------+----------------------------------------------
     <program>          | The parsed VarStack program to execute as a
                        | vector of ``Statement'' objects.
     ..................................................................
     <ip>               | The currently evaluated statement's
                        | zero-based index into the <PROGRAM> vector.
     ..................................................................
     <labels>           | Maintains the recognized statement label
                        | names as a mapping from their unique names to
                        | their zero-based indices into the <PROGRAM>.
     ..................................................................
     <jump-destination> | The optional explicitly set next statement
                        | label name to execute as a consequence of a
                        | conditional command's successful activation.
                        | If ``NIL'', the currently processed
                        | statement's personal destination label name
                        | will be sojourned instead.
     ..................................................................
     <stacks>           | Maintains the created stack objects as a
                        | mapping from their unique names to
                        | representative ``Stack'' instances.
     ------------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (with-slots ((<program>          program)
                    (<ip>               ip)
                    (<labels>           labels)
                    (<jump-destination> jump-destination)
                    (<stacks>           stacks))
           ,evaluated-interpreter
         (declare (type program                 <program>)
                  (ignorable                    <program>))
         (declare (type fixnum                  <ip>)
                  (ignorable                    <ip>))
         (declare (type Label-Table             <labels>)
                  (ignorable                    <labels>))
         (declare (type (or null simple-string) <jump-destination>)
                  (ignorable                    <jump-destination>))
         (declare (type Stack-Table             <stacks>)
                  (ignorable                    <stacks>))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Collects the statement labels associated with the INTERPRETER's
   program, stores the same in the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf <labels>
      (collect-statement-labels <program>)))
  (values))

;;; -------------------------------------------------------

(defun get-current-statement (interpreter)
  "Returns the statement located at the INTERPRETER's current
   instruction pointer (IP) position."
  (declare (type Interpreter interpreter))
  (the Statement
    (with-interpreter (interpreter)
      (aref <program> <ip>))))

;;; -------------------------------------------------------

(defun halt-if-label-does-not-exist (interpreter probed-label-name)
  "Determines whether a label with the PROBED-LABEL-NAME exists in the
   INTERPRETER's label table, on confirmation returning no value;
   otherwise signals a ``Halt-Condition''."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string probed-label-name))
  (with-interpreter (interpreter)
    (unless (label-with-name-exists-p <labels> probed-label-name)
      (halt-program)))
  (values))

;;; -------------------------------------------------------

(defun redirect-to-destination-label (interpreter destination-label)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   zero-based position into its program affiliated with the
   DESTINATION-LABEL, or, upon its disrespondency, signals a
   ``Halt-Condition''."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string destination-label))
  (halt-if-label-does-not-exist interpreter destination-label)
  (with-interpreter (interpreter)
    (setf <ip>
      (locate-label <labels> destination-label)))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-statement (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the next
   statement as specified by the current statement's destination label,
   or, upon its disrespondency, signals a ``Halt-Condition''."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (redirect-to-destination-label interpreter
      (if <jump-destination>
        (prog1 <jump-destination>
          (setf <jump-destination> NIL))
        (standard-statement-destination
          (get-current-statement interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun halt-if-stack-does-not-exist (interpreter probed-stack-name)
  "Determines whether a stack with the PROBED-STACK-NAME exists in the
   INTERPRETER's stack table, on confirmation returning no value;
   otherwise signals a ``Halt-Condition''."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string probed-stack-name))
  (with-interpreter (interpreter)
    (unless (stack-with-name-exists-p <stacks> probed-stack-name)
      (halt-program)))
  (values))

;;; -------------------------------------------------------

(defun halt-if-stack-is-empty (interpreter probed-stack-name)
  "Determines whether the stack amenable to the PROBED-STACK-NAME in the
   INTERPRETER's stack table is not empty, on confirmation returning no
   value; otherwise signals a ``Halt-Condition''."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string probed-stack-name))
  (with-interpreter (interpreter)
    (when (stack-is-empty-p
            (get-stack-with-name <stacks> probed-stack-name))
      (halt-program)))
  (values))

;;; -------------------------------------------------------

(defun halt-if-stack-cannot-be-accessed (interpreter probed-stack-name)
  "Determines whether the stack amenable to the PROBED-STACK-NAME in the
   INTERPRETER's stack table exists and is not empty, on confirmation
   returning no value; otherwise signals a ``Halt-Condition''."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string probed-stack-name))
  (halt-if-stack-does-not-exist interpreter probed-stack-name)
  (halt-if-stack-is-empty       interpreter probed-stack-name)
  (values))

;;; -------------------------------------------------------

(defun push-onto-stack-with-name (interpreter stack-name new-element)
  "Pushes the NEW-ELEMENT onto the stack entalented with an amenability
   to the STACK-NAME in the INTERPRETER and returns no value.
   ---
   If no affiliation for the STACK-NAME can be detected, this operation
   signals a ``Halt-Condition'' in order to terminate the program."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (declare (type character     new-element))
  (halt-if-stack-does-not-exist interpreter stack-name)
  (with-interpreter (interpreter)
    (push-onto-stack
      (get-stack-with-name <stacks> stack-name)
      new-element))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack-with-name (interpreter stack-name)
  "Pops and returns the top element from the stack entalented with an
   amenability to the STACK-NAME in the INTERPRETER.
   ---
   If no affiliation for the STACK-NAME can be detected, or if such a
   stack exists, but is incapable of responding to the request because
   of its empty state, this operation signals a ``Halt-Condition'' in
   order to terminate the program."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (halt-if-stack-cannot-be-accessed interpreter stack-name)
  (the character
    (with-interpreter (interpreter)
      (pop-from-stack
        (get-stack-with-name <stacks> stack-name)))))

;;; -------------------------------------------------------

(defun peek-into-stack-with-name (interpreter stack-name)
  "Returns without removing the top element from the stack entalented
   with an amenability to the STACK-NAME in the INTERPRETER.
   ---
   If no affiliation for the STACK-NAME can be detected, or if such a
   stack exists, but is incapable of responding to the request because
   of its empty state, this operation signals a ``Halt-Condition'' in
   order to terminate the program."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (halt-if-stack-cannot-be-accessed interpreter stack-name)
  (the character
    (with-interpreter (interpreter)
      (peek-into-stack
        (get-stack-with-name <stacks> stack-name)))))

;;; -------------------------------------------------------

(defun get-elements-of-stack-with-name (interpreter stack-name)
  "Returns without removing all elements from the stack entalented
   with an amenability to the STACK-NAME in the INTERPRETER.
   ---
   If no affiliation for the STACK-NAME can be detected, or if such a
   stack exists, but is incapable of responding to the request because
   of its empty state, this operation signals a ``Halt-Condition'' in
   order to terminate the program."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (halt-if-stack-does-not-exist interpreter stack-name)
  (the (list-of character)
    (with-interpreter (interpreter)
      (stack-elements
        (get-stack-with-name <stacks> stack-name)))))

;;; -------------------------------------------------------

(defun swap-top-of-stack-with-name (interpreter stack-name)
  "Exchanges the positions of the two top elements commorant in the
   stack addressed by the STACK-NAME in the INTERPRETER and returns no
   value.
   ---
   If no affiliation for the STACK-NAME can be detected, this operation
   signals a ``Halt-Condition'' in order to terminate the program.
   ---
   If the desiderated stack's existency constitutes a compernage to the
   fact of its content's destitution, this operation signals a
   ``Halt-Condition'' in order to terminate the program.
   ---
   If the stack's existency resides as a concomitant to its cardinality
   as an aefauld member's salvatory, no epiphenomenon is accompassed."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (halt-if-stack-cannot-be-accessed interpreter stack-name)
  (with-interpreter (interpreter)
    (swap-top-stack-elements
      (get-stack-with-name <stacks> stack-name)))
  (values))

;;; -------------------------------------------------------

(defun reverse-stack-with-name (interpreter stack-name)
  "Reverses the order of the elements partaking of the stack amenable
   to the STACK-NAME in the INTERPRETER and returns no value.
   ---
   If no affiliation for the STACK-NAME can be detected, this operation
   signals a ``Halt-Condition'' in order to terminate the program.
   ---
   If the referenced stack's existency can be vouched, but its
   cardinality amounts to zero (0), no epiphenomenon is accompassed."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (halt-if-stack-does-not-exist interpreter stack-name)
  (with-interpreter (interpreter)
    (reverse-stack
      (get-stack-with-name <stacks> stack-name)))
  (values))

;;; -------------------------------------------------------

(defun relocate-top-of-stack-with-name (interpreter stack-name)
  "Relocates the top element on the stack capable of being accosted by
   the STACK-NAME in the INTERPRETER to the stack's bottom position and
   returns no value.
   ---
   If no affiliation for the STACK-NAME can be detected, this operation
   signals a ``Halt-Condition'' in order to terminate the program.
   ---
   If the desiderated stack's existency constitutes a compernage to the
   fact of its content's destitution, this operation signals a
   ``Halt-Condition'' in order to terminate the program.
   ---
   If the stack's existency resides as a concomitant to its cardinality
   as an aefauld member's salvatory, no epiphenomenon is accompassed."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string stack-name))
  (halt-if-stack-cannot-be-accessed interpreter stack-name)
  (with-interpreter (interpreter)
    (relocate-stack-top-to-bottom
      (get-stack-with-name <stacks> stack-name)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the VarStack COMMAND in the INTERPRETER's context and
     returns no value."))

;;; -------------------------------------------------------

(defmacro define-command-processor (command-class &body body)
  "Defines an implementation of the generic function
   ``process-command'', stevening the first argument ``<interpreter>''
   and dispatching on the ``Interpreter'' class, and the second argument
   ``<command>'', while dispatching on the COMMAND-CLASS, evaluates
   the BODY forms, and returns no value.
   ---
   An apercu adduced, the following definitions are furnished:
     ------------------------------------------------------------------
     Definition    | Purpose
     --------------+---------------------------------------------------
     <interpreter> | The ``Interpreter'' inwith whose context the
                   | <COMMAND> shall be evaluated.
     ..................................................................
     <command>     | The evaluated ``Command''.
     ------------------------------------------------------------------"
  `(defmethod process-command ((<interpreter> Interpreter)
                               (<command>     ,command-class))
     (declare (type Interpreter    <interpreter>)
              (ignorable           <interpreter>))
     (declare (type ,command-class <command>)
              (ignorable           <command>))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-command-processor Create-Stack-Command
  (with-interpreter (<interpreter>)
    (register-stack <stacks>
      (create-stack-command-stack <command>)
      (create-stack-initialized-with
        (convert-string-into-list
          (create-stack-command-initial-elements <command>))))))

;;; -------------------------------------------------------

(define-command-processor Push-Command
  (halt-if-stack-does-not-exist <interpreter>
    (push-command-stack <command>))
  (with-interpreter (<interpreter>)
    (push-onto-stack
      (get-stack-with-name <stacks>
        (push-command-stack <command>))
      (push-command-new-element <command>))))

;;; -------------------------------------------------------

(define-command-processor Pop-Command
  (halt-if-stack-cannot-be-accessed <interpreter>
    (pop-command-stack <command>))
  (with-interpreter (<interpreter>)
    (pop-from-stack-with-name <interpreter>
      (pop-command-stack <command>))))

;;; -------------------------------------------------------

(define-command-processor Output-Literal-Command
  (format *standard-output* "~a"
    (output-literal-command-argument <command>)))

;;; -------------------------------------------------------

(define-command-processor Output-Stack-Command
  (format *standard-output* "~{~c~}"
    (get-elements-of-stack-with-name <interpreter>
      (output-stack-command-stack <command>))))

;;; -------------------------------------------------------

(define-command-processor Input-Command
  (format        *standard-output* "~&>> ")
  (finish-output *standard-output*)
  (push-onto-stack-with-name <interpreter>
    (input-command-stack <command>)
    (read-char *standard-input* NIL #\Null))
  (clear-input *standard-input*))

;;; -------------------------------------------------------

(define-command-processor Jump-If-Equals-Literal-Command
  (with-interpreter (<interpreter>)
    (when (char=
            (peek-into-stack-with-name <interpreter>
              (jump-if-equals-literal-command-stack <command>))
            (jump-if-equals-literal-command-guard <command>))
      (setf <jump-destination>
        (jump-if-equals-literal-command-destination <command>)))))

;;; -------------------------------------------------------

(define-command-processor Jump-If-Does-Not-Equal-Literal-Command
  (with-interpreter (<interpreter>)
    (when (char/=
            (peek-into-stack-with-name <interpreter>
              (jump-if-does-not-equal-literal-command-stack <command>))
            (jump-if-does-not-equal-literal-command-guard <command>))
      (setf <jump-destination>
        (jump-if-does-not-equal-literal-command-destination
          <command>)))))

;;; -------------------------------------------------------

(define-command-processor Jump-If-Equals-Stack-Command
  (with-interpreter (<interpreter>)
    (when (char=
            (peek-into-stack-with-name <interpreter>
              (jump-if-equals-stack-command-first-stack <command>))
            (peek-into-stack-with-name <interpreter>
              (jump-if-equals-stack-command-second-stack <command>)))
      (setf <jump-destination>
        (jump-if-equals-stack-command-destination <command>)))))

;;; -------------------------------------------------------

(define-command-processor Swap-Command
  (with-interpreter (<interpreter>)
    (swap-top-of-stack-with-name <interpreter>
      (swap-command-stack <command>))))

;;; -------------------------------------------------------

(define-command-processor Reverse-Command
  (with-interpreter (<interpreter>)
    (reverse-stack-with-name <interpreter>
      (reverse-command-stack <command>))))

;;; -------------------------------------------------------

(define-command-processor Top-Relocation-Command
  (with-interpreter (<interpreter>)
    (relocate-top-of-stack-with-name <interpreter>
      (top-relocation-command-stack <command>))))

;;; -------------------------------------------------------

(defun process-statement (interpreter statement)
  "Processes the STATEMENT in the INTERPRETER's context, relocates the
   instruction pointer (IP) to the subsequent statement as specified by
   the evaluated one's destination label, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Statement   statement))
  (process-command interpreter
    (standard-statement-command statement))
  (advance-to-next-statement interpreter)
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the VarStack program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (handler-case
        (when (plusp (length <program>))
          (loop do
            (process-command interpreter
              (standard-statement-command
                (get-current-statement interpreter)))
            (advance-to-next-statement interpreter)))
      (Halt-Condition ()
        NIL)))
  (values))

;;; -------------------------------------------------------

(defun interpret-VarStack (code)
  "Interprets the piece of VarStack source CODE and returns no value."
  (declare (type string code))
  (set-source-code code)
  (execute-program
    (make-instance 'Interpreter :program (parse-program)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output.
(interpret-VarStack
  "hello;\"Hello, World!;halt")

;;; -------------------------------------------------------

;; Repeating cat program.
(interpret-VarStack
  "createStack;@X:;requestInput
   requestInput;?X;printInput
   printInput;'X;removeInput
   removeInput;!X;requestInput")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-VarStack
  "start;@X:0;input
   input;?X;if
   if;#X=0:print0;print1
   print1;\"1;print1
   print0;\"0;halt")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language
;; "Bitwise Cyclic Tag".
;; 
;; Please proceed in the following manner:
;; 
;;   (1) PROGRAM-STRING input:
;;       (1.1) Upon each input request, commit either a "0" or "1"
;;             character, corresponding to the desiderated bit value to
;;             append to the program-string, in the intended order.
;;       (1.2) If no further program-string bit shall be appended,
;;             input the "@" character. This will advance the program
;;             state to the next tier, -> (2) DATA-STRING INPUT.
;;   
;;   (2) DATA-STRING input:
;;       (2.1) Upon each input request, commit either a "0" or "1"
;;             character, corresponding to the desiderated bit value to
;;             append to the data-string, in the intended order.
;;       (2.2) If no further data-string bit shall be appended, input
;;             the character "@". This will advance the program to the
;;             subsequent interpretation stages.
;; 
(interpret-VarStack
  "
  initprogram;@code:0;initprogramfinish
  initprogramfinish;!code;initdata
  initdata;@data:0;initdatafinish
  initdatafinish;!data;inputcode1
  inputcode1;?code;inputcode2
  inputcode2;#code=@:inputcode3;inputcode1
  inputcode3;!code;inputcode4
  inputcode4;&code;inputdata1
  inputdata1;?data;inputdata2
  inputdata2;#data=@:inputdata3;inputdata1
  inputdata3;!data;inputdata4
  inputdata4;&data;interpret
  interpret;#code=0:remove;if1
  remove;!data;move
  if1;#data=1:nextinstruction;move
  nextinstruction;^code;reverse
  reverse;&data;enq
  enq;#code=0:enq0;enq1
  enq0;$0:data;endenq
  enq1;$1:data;endenq
  endenq;&data;interpret
  move;^code;interpret
  ")
