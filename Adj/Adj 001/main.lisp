;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Adj", invented by the Esolang user "BestCoder" and
;; presented on February 20th, 2024, its proprium's woning the
;; realization of its capabilities per procurationem of a single
;; forbisen, malleated via a triad of operands, during its engagement
;; with a trisulk of signed integer variables.
;; 
;; 
;; Concept
;; =======
;; The Adj programming language employs, among its faculties'
;; preponderance, the "ADJ" keyword in conjunction with three operands,
;; their concrete values the affiliated epiphenomena's merist.
;; 
;; == "Adj": AN INTIMATION OF THE MOST PEISANT LANGUAGE KEYWORD ==
;; The language's agnomination, empight upon "Adj", bewrays an allusion
;; to its most emolumental polymechany, "[AD]d a to b, then [J]ump to c"
;; principle, whence the moult capacities enjoy their emergence.
;; 
;; == ADJ: "ADJ" + THREE OPERANDS ==
;; The Adj programming language's firmament ostends, in a paravaunt
;; manner, an edification upon an aefauld instruction's multum in parvo
;; ramosity, its nevening "ADJ", the forbisens of an operative triad in
;; its communicated plasmatures serving as indicia to the meiny of
;; obtainable causata.
;; 
;; Entalented with the curule dignity, the "ADJ" operation's most
;; potent impact's fons et origo parlays of a variable's incrementation
;; by the second operand's value, ere an optional jumping action, the
;; fact and mode of its participation the third argument's bailiwick,
;; may transpire.
;; 
;; == CONTROL FLOW: LABELS AND LINE NUMBERS ==
;; This leilty to the "one instruction set computer" (OISC) species'
;; tenets merely tholes an adulteration in the syntactical contingency
;; for label definitions, thilk filst, as one among a jumelle's
;; possibility, in the control flow duction's governail; the patration's
;; promachos patefying in a one-based line number relocation.
;; 
;; == THE MEMORY: THREE SIGNED INTEGER VARIABLES: "a", "b", "c" ==
;; The program memory does not entertain a salvatory with a potential
;; aboon a trisulk of variables, the agnominations such as to maintain
;; the imposition of a minuscular designation: "a", "b", "c".
;; 
;; Each specimen enjoys the parcery in a capacity of a signed integer
;; datum, the mickleness of whom does not wist of a natural bourne
;; along both axes.
;; 
;; 
;; Syntax
;; ======
;; One conspectuity's exercise bewrays the Adj language's syntaxis as a
;; catena of zero or more lines, each such either an "ADJ" command
;; variation, forming in its operands a trisulc; or a label definition,
;; utible for subsequent navigation.
;; 
;; The sepiment to each token manifests in one or more spaces; while the
;; lines themselves are physically eloigned by one or more newline
;; characters.
;; 
;; == VARIABLE NAMES ==
;; The mountenance vouchsafe to variables does not rise ayond a trisulc,
;; identified by the Latin minuscules "a", "b", and "c".
;; 
;; == LABEL DESIGNATORS ==
;; A label identifier's conformation ensues from any non-empty content,
;; inwith whose diorism does not inhere the potential for a confoundment
;; with an integer literal, succeeded by zero or more symbols admitting
;; any content except for the colon (":") and whitespaces.
;; 
;; Adj language keywords, such as "ADJ" and "X", ostend an eloignment
;; from an reappropration's homologation.
;; 
;; The capacitation for navigations through label name, in conjunction
;; with Adj's dioristic syntaxis, concludes in the gendrure of a
;; particular nomothesia concerning the homologations in the label
;; agnominations' assignments:
;; 
;;   ------------------------------------------------------------------
;;   Designator    | Causatum
;;   --------------+---------------------------------------------------
;;   label name    | Navigates to the line comprehending the label's
;;                 | definition.
;;   ..................................................................
;;   variable name | Navigates to the line number signified by the
;;                 | value stored in the respective variable name, the
;;                 | compass of which amplects the specimens "a", "b",
;;                 | and "c".
;;   ..................................................................
;;   integer       | Navigates to the line number concurring with the
;;                 | literal integer value.
;;   ------------------------------------------------------------------
;; 
;; The choice of label name is subjected to a certain necessity
;; concerning the legislature:
;; 
;;   ------------------------------------------------------------------
;;   Inadmissible | Rationale
;;   content      | 
;;   -------------+----------------------------------------------------
;;   empty        | The characters' carency cannot be distinguished
;;                | from surrounding spaces.
;;   ..................................................................
;;   "ADJ"        | Introduces an instruction.
;;   ..................................................................
;;   ":"          | Naited to conclude a label definition's name.
;;   ..................................................................
;;   space        | Naited for the segregation of tokens.
;;   ..................................................................
;;   newline      | Naited for the segregation of instructions.
;;   ------------------------------------------------------------------
;; 
;; == GRAMMAR ==
;; An enhaused mete of lucidity's dation anent the language's donet
;; shall be the following Extended Backus-Naur Form (EBNF) description's
;; hyle:
;; 
;;   program              := { innerLine } , [ terminalLine ] ;
;;   terminalLine         := [ command ]                      ;
;;   innerLine            := [ command ] , newline            ;
;;   
;;   command              := addAndJumpCommand
;;                        |  addOnlyCommand
;;                        |  jumpOnlyCommand
;;                        |  outputAndJumpCommand
;;                        |  outputOnlyCommand
;;                        |  inputAndJumpCommand
;;                        |  inputOnlyCommand
;;                        |  labelDefinition
;;                        ;
;;   addAndJumpCommand    := ADJ , variable , operand  , labelName ;
;;   addOnlyCommand       := ADJ , variable , operand  , X         ;
;;   jumpOnlyCommand      := ADJ , X        , X        , labelName ;
;;   outputAndJumpCommand := ADJ , "0"      , operand  , labelName ;
;;   outputOnlyCommand    := ADJ , "0"      , operand  , X         ;
;;   inputAndJumpCommand  := ADJ , "1"      , variable , labelName ;
;;   inputOnlyCommand     := ADJ , "1"      , variable , X         ;
;;   
;;   labelDefinition      := labelName , ":" ;
;;   labelName            := potentialLabelName - invalidLabelNames ;
;;   potentialLabelName   := labelCharacter , { labelCharacter } ;
;;   labelCharacter       := character - ( whitespace | ":" ) ;
;;   invalidLabelNames    := ADJ | X | variable | integer ;
;;   
;;   ADJ                  := "ADJ" ;
;;   X                    := "X"   ;
;;   
;;   operand              := integerLiteral | variable ;
;;   variable             := "a" | "b" | "c" ;
;;   integerLiteral       := digit , { digit } ;
;;   digit                := "0" | "1" | "2" | "3" | "4"
;;                        |  "5" | "6" | "7" | "8" | "9"
;;                        ;
;;   whitespace           := newline | space ;
;;   newline              := "\n" ;
;;   space                := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; A twyforked variety's governail is realized in the Adj programming
;; language's instruction set: imprimis, employing the "ADJ" pattern in
;; order to actuate the preponderance among its capacitations; in a
;; parhedral dation's guise, the contingency for label definitions
;; homologates navigations across the code according to several avenues.
;; 
;; == OVERVIEW ==
;; An apercu, entalented with the dever of the requisite nortelry's
;; adhibition concerning the operative amenities, shall be produced
;; alow.
;; 
;; Please heed the demarcation of succedaneous tmemata by adminiculum of
;; catenas edified upon asterisks ("*"), intended for their ultimate
;; substitution by actual Adj code in the program.
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ==================================================================
;;   ADDITION
;;   ------------------------------------------------------------------
;;   ADJ augend addend label | Adds the {addend} to the {augend}
;;       ****** ****** ***** | variable, modifies the {augend} to the
;;                           | resulting sum, and relocates the
;;                           | instruction pointer (IP) to the line
;;                           | designated by the {label}.
;;                           |----------------------------------------- 
;;                           | {augend} must constitute the name of a
;;                           | member from the variables' trisulk: "a",
;;                           | "b", or "c".
;;                           |-----------------------------------------
;;                           | {addend} must subsume into one of these
;;                           | species:
;;                           |   (a) A literal signed or unsigned
;;                           |       integer number, the same is
;;                           |       directly added to the {augend}.
;;                           |   (b) A variable name from the trisulk
;;                           |       "a", "b", and "c", the value
;;                           |       stored in whom is added to the
;;                           |       {augend}.
;;                           |-----------------------------------------
;;                           | {label} must resolve to any of these:
;;                           |   (a) A literal or unsigned integer
;;                           |       number, which per saltum specifies
;;                           |       the one-based line number to
;;                           |       sojourn to.
;;                           |   (b) A variable name from the trisulk
;;                           |       "a", "b", and "c", the value
;;                           |       stored in whom designaes the
;;                           |       one-based line number to navigate
;;                           |       to.
;;                           |   (c) The name of a label, the position
;;                           |       of which shall be occupied.
;;                           |   (d) The sentinel "X", which designates
;;                           |       that no jumping shall transpire;
;;                           |       instead, the next command is
;;                           |       sojourned.
;;   ==================================================================
;;   OUTPUT
;;   ------------------------------------------------------------------
;;   ADJ 0      source label | Prints the {source} to the standard
;;              ****** ***** | output conduit and relocates the
;;                           | instruction pointer (IP) to the line
;;                           | designated by the {label}.
;;                           |-----------------------------------------
;;                           | {source} must subsume into one of these
;;                           | species:
;;                           |   (a) A literal signed or unsigned
;;                           |       integer number, the same is
;;                           |       directly added to the {augend}.
;;                           |   (b) A variable name from the trisulk
;;                           |       "a", "b", and "c", the value
;;                           |       stored in whom is added to the
;;                           |       {augend}.
;;                           |-----------------------------------------
;;                           | {label} must resolve to any of these:
;;                           |   (a) A literal or unsigned integer
;;                           |       number, which per saltum specifies
;;                           |       the one-based line number to
;;                           |       sojourn to.
;;                           |   (b) A variable name from the trisulk
;;                           |       "a", "b", and "c", the value
;;                           |       stored in whom designaes the
;;                           |       one-based line number to navigate
;;                           |       to.
;;                           |   (c) The name of a label, the position
;;                           |       of which shall be occupied.
;;                           |   (d) The sentinel "X", which designates
;;                           |       that no jumping shall transpire;
;;                           |       instead, the next command is
;;                           |       sojourned.
;;   ==================================================================
;;   INPUT
;;   ------------------------------------------------------------------
;;   ADJ 1      target label | Queries the standard input conduit for a
;;              ****** ***** | signed or unsigned integer number,
;;                           | stores thilk in the {target} variable,
;;                           | and relocates the instruction pointer
;;                           | (IP) to the line designated by the
;;                           | {label}.
;;                           |-----------------------------------------
;;                           | {target} must constitute the name of a
;;                           | member from the variables' trisulk: "a",
;;                           | "b", or "c".
;;                           |-----------------------------------------
;;                           | {label} must resolve to any of these:
;;                           |   (a) A literal or unsigned integer
;;                           |       number, which per saltum specifies
;;                           |       the one-based line number to
;;                           |       sojourn to.
;;                           |   (b) A variable name from the trisulk
;;                           |       "a", "b", and "c", the value
;;                           |       stored in whom designaes the
;;                           |       one-based line number to navigate
;;                           |       to.
;;                           |   (c) The name of a label, the position
;;                           |       of which shall be occupied.
;;                           |   (d) The sentinel "X", which designates
;;                           |       that no jumping shall transpire;
;;                           |       instead, the next command is
;;                           |       sojourned.
;;   ==================================================================
;;   LABEL DEFINITION
;;   ------------------------------------------------------------------
;;   labelName:              | Defines a label amenable to the name
;;   *********               | {labelName}.
;;                           |-----------------------------------------
;;                           | If a label with the {labelName} has
;;                           | already been defined at a prevenient
;;                           | location in the program, an error of the
;;                           | type "DuplicateLabelError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; == "ADJ" COMMAND FORBISENS ==
;; An listing whose circumference is dignified in all possible operand
;; combinations' vouchsafement for the "ADJ" statement species, the
;; mountance a 20 members' accompt, shall be adduced alow.
;; 
;; Ere these forbisens' enumeration, an epexegesis ordained to the
;; imperative symbols' participation in the same as succedanea shall be
;; a cursory tabulation's cynosure:
;; 
;;   ------------------------------------------------------
;;   Symbol | Denotation
;;   -------+----------------------------------------------
;;   var    | A variable name, that is, "a", "b", or "c".
;;   ......................................................
;;   var1   | A variable name, that is, "a", "b", or "c".
;;   ......................................................
;;   var2   | A variable name, that is, "a", "b", or "c".
;;   ......................................................
;;   var3   | A variable name, that is, "a", "b", or "c".
;;   ======================================================
;;   lit    | A literal signed or unsigned integer number.
;;   ......................................................
;;   lit1   | A literal signed or unsigned integer number.
;;   ......................................................
;;   lit2   | A literal signed or unsigned integer number.
;;   ======================================================
;;   label  | A label name.
;;   ------------------------------------------------------
;; 
;; Avaunting from the aboon keys presentation, the patterns themselves
;; shall be ostended, the placeholders signified by underlining
;; asterisks ("*"):
;; 
;;   ------------------------------------------------------------------
;;   Command pattern     | Causatum
;;   ==================================================================
;;   ADDITION BY VARIABLE
;;   ------------------------------------------------------------------
;;   ADJ var1 var2 X     | Increment {var1} by {var2} and do not jump.
;;       **** ****       | 
;;   ..................................................................
;;   ADJ var1 var2 label | Increment {var1} by {var2} and jump to
;;       **** **** ***** | {label} name.
;;   ..................................................................
;;   ADJ var1 var2 var3  | Increment {var1} by {var2} and jump to
;;       **** **** ****  | one-based line number {var3}.
;;   ..................................................................
;;   ADJ var1 var2 lit   | Increment {var1} by {var2} and jump to
;;       **** **** ***   | one-based line number {lit}.
;;   ==================================================================
;;   ADDITION BY LITERAL
;;   ------------------------------------------------------------------
;;   ADJ var  lit  X     | Increment {var} by {lit} and do not jump.
;;       ***  ***        | 
;;   ..................................................................
;;   ADJ var  lit  label | Increment {var1} by {lit} and jump to
;;       ***  ***  ***** | {label} name.
;;   ..................................................................
;;   ADJ var1 lit  var2  | Increment {var1} by {lit} and jump to
;;       **** ***  ****  | one-based line number {var2}.
;;   ..................................................................
;;   ADJ var  lit1 lit2  | Increment {var1} by {lit12} and jump to
;;       ***  **** ****  | one-based line number {lit2}.
;;   
;;   ==================================================================
;;   OUTPUT OF VARIABLE
;;   ------------------------------------------------------------------
;;   ADJ 0    var  X     | Output {var} and do not jump.
;;            ***        | 
;;   ..................................................................
;;   ADJ 0    var  label | Output {var} and jump to {label} name.
;;            ***  ***** | 
;;   ..................................................................
;;   ADJ 0    var1 var2  | Output {var1} and jump to one-based line
;;            **** ****  | number {var2}.
;;   ..................................................................
;;   ADJ 0    var  lit   | Output {var} and jump to one-based line
;;            ***  ***   | {lit}.
;;   ==================================================================
;;   OUTPUT OF LITERAL
;;   ------------------------------------------------------------------
;;   ADJ 0    lit  X     | Output {lit} and do not jump.
;;            ***        | 
;;   ..................................................................
;;   ADJ 0    lit  label | Output {lit} and jump to {label} name.
;;            ***  ***** | 
;;   ..................................................................
;;   ADJ 0    lit  var   | Output {lit} and jump to one-based line
;;            ***  ***   | number {var}.
;;   ..................................................................
;;   ADJ 0    lit1 lit2  | Output {lit1} and jump to one-based line
;;            **** ****  | {lit2}.
;;   ==================================================================
;;   INPUT INTO VARIABLE
;;   ------------------------------------------------------------------
;;   ADJ 1    var  X     | Input into {var} and do not jump.
;;            ***        | 
;;   ..................................................................
;;   ADJ 1    var  label | Input into {var} and jump to {label} name.
;;            ***  ***** | 
;;   ..................................................................
;;   ADJ 1    var1 var2  | Input into {var1} and jump to one-based line
;;            **** ****  | number {var2}.
;;   ..................................................................
;;   ADJ 1    var  lit   | Intput into {var} and jump to one-based line
;;            ***  ***   | {lit}.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The patefaction of this interpreter has been actuated in the
;; programming language Common Lisp, the execution a bipartite gestion,
;; inchoating in the Adj program's operative lines' transcription into
;; representative statements, filsting as parasceuastic molds, ere their
;; subsequent evaluation.
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
;;       Maugre the versatility whose inherence in META redes the same
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
;;       this implementation peracts a curtailment in the operative
;;       multitude, whence merely two indurated routines enjoy an
;;       admission. For instance, both the "matchit" and matchtype"
;;       macros are dispelled in favor of their epiphenomena's
;;       incorporation in the respective "compile-expression" functions.
;; 
;; 
;; == META'S TELOS: A PARSER OSTENDING A COMPENDIOUS SYNTAX ==
;; The telos comprising META's encheson of pursuit appertains to a
;; scannerless parser, one whose lexical analyzer (lexer) establishes
;; an item of the conjoined haecceity, the efforts of which capacitate
;; the same with its wikes' attendance, while a compendiousness limning
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
;;     (and (or (current-character-equals-p #\+)
;;              (and (current-character-equals-p #\-)
;;                   (setf sign -1))
;;              T)
;;          (current-character-is-of-type-p digit current-character))
;;     
;;     (print (list sign current-character)))
;; 
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
;; 
;; == META'S CAPABILITIES ==
;; A septuple membership exhausts the parser's foundry concerning those
;; operations on the source string that needs-cost ought to subsume
;; into the provision for its realization, and which, ensuing from a
;; higher mete of diligence's adhibition, differentiate into "atomic"
;; actions, operating with immediacy's puissance on the objects of
;; deliberation, such encompass the characters and strings, and a
;; paregal tier of facilities, incolants of a superimposed meta-level,
;; inwith whose bailiwick is integrated the combination of atomic and
;; meta expressions into a superior formation of complexes.
;; 
;; The following enumeration's dever shall be the seven constituents'
;; presentation and explication:
;; 
;;   (1) CHARACTER EQUALITY TEST
;;       > Purpose:
;;         Advancement to the next character in the source, without
;;         the probed element's capture, upon its equality to a
;;         specified character.
;;       > Procedure:
;;         Juxtaposes the source's current character with a guard
;;         character.
;;         If both are equal, advances the position cursor to the
;;         source's subsequent character, without a concomitant storage
;;         of the successfully probed element, and returns a Boolean
;;         value of "true".
;;         If the characters differ, no advancement transpires, and a
;;         Boolean "false" value is delivered.
;;   
;;   (2) STRING EQUALITY TEST
;;       > Purpose:
;;         Consumption of a string tmema in the source, without a
;;         capture, upon its equality to a specified string.
;;       > Procedure:
;;         Juxtaposes the characters in the source commencing with the
;;         current one with a specified guard string.
;;         If both are equal, advances the position cursor to the
;;         location in the source immediately succeeding the matched
;;         section, without a concomitant storage of the probed
;;         character sequence, and returns a Boolean value of "true".
;;         If the strings differ, no advancement transpires, and a
;;         Boolean "false" value is delivered.
;;   
;;   (3) CHARACTER TYPE TEST
;;       > Purpose:
;;         Assignment of a character to a place (variable) upon its
;;         compliance with a specified type.
;;       > Procedure:
;;         Determines whether the source's current character complies
;;         with a given type, represented by a type specifier.
;;         If a type compatibility can be ascertained, stores the
;;         successfully probed character in a place (variable), advances
;;         the position cursor to the next character in the source, and
;;         returns a Boolean value of "true".
;;         Upon a mismatch in the character's type, neither an
;;         assignment nor an advancement transpire, and a Boolean
;;         "false" value is delivered.
;;   
;;   (4) SEQUENTIAL MATCHING
;;       > Purpose:
;;         Definition of a finite sequence of characters, string, or
;;         META expression thilk must match in their specified order and
;;         in their entirety.
;;       > Procedure:
;;         Establishes an "AND"-combination of the sequence's members,
;;         where each such element is matched via its personal
;;         "compile-expression" operation invocation's result.
;;   
;;   (5) ALTERNATIVE MATCHING
;;       > Purpose:
;;         Definition of an ordered list of choices, each such either
;;         a character, a string, or a META expression, where at least
;;         one alternative must match.
;;       > Procedure:
;;         Establishes an "OR"-combination of the alternative's members,
;;         where each such option is matched via its personal
;;         "compile-expression" operation invocation's result.
;;   
;;   (6) REPETITION
;;       > Purpose:
;;         Repeated matching of the same character, string, or a META
;;         expression for zero or more times.
;;       > Procedure:
;;         Repeatedly invokes the form's "compile-expression" operation,
;;         as long as the same does not return a Boolean "false" result.
;;         In the case of siccan negative response terminates the
;;         iterance, and returns a Boolean value of "true".
;;   
;;   (7) EVALUATION
;;       > Purpose:
;;         Execution of an arbitrary form, including the contingency
;;         for epiphenomena.
;;       > Procedure:
;;         Evaluates the form and returns its result. A Boolean value of
;;         "true" is construed as a successful match's signification,
;;         while a "false" response amounts to a mismatch.
;; 
;; 
;; == META'S CAPABILITIES MAPPED TO OBJECTS OF DELIBERATION ==
;; The kithe's vouchsafement anenst the avails for character
;; perquisitions and those facilities devoted to more potent services
;; shall constitute a tabular exposition's hypostasis, whence will
;; derive an equiparation betwixt the requisite devers and their
;; representatives as constituting the responsibilities' recipients:
;; 
;;   ------------------------------------------------------------------
;;   # | Task                    | Object of deliberation
;;   --+-------------------------+-------------------------------------
;;   1 | Character equality test | character
;;   ..................................................................
;;   2 | String equality test    | string
;;   ..................................................................
;;   3 | Character type test     | META type test
;;   ..................................................................
;;   4 | Sequential matching     | META sequence
;;   ..................................................................
;;   5 | Alternative matching    | META alternative
;;   ..................................................................
;;   6 | Repetition              | META repetition
;;   ..................................................................
;;   7 | Code evaluation         | META evaluation
;;   ------------------------------------------------------------------
;; 
;; 
;; == A PROEM: META'S SYNTAXIS AND COMPETENCES ==
;; An inchoation of gnarity's conveyance concerning META's syntax and
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
;; 
;; == META IS RANKED ALOW THE POTENTIALS OF REGULAR EXPRESSIONS ==
;; Maugre its status as one blemished by an eloigment from the entire
;; competences embodied in regular expressions, META's capacitation lays
;; its amplection around a rather extensively measured set of solutions
;; to common problems, including, without an exhaustive potential's
;; claim, the parsing of integer and floating-point numbers and Common
;; Lisp's own lambda lists.
;; 
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
;; 
;; Avaunting from the foundational wikes apportioned to the objects of
;; deliberation, as referenced by an acquaintance in the section
;; "META'S CAPABILITIES MAPPED TO OBJECTS OF DELIBERATION", which, upon
;; an optation's pique please consult aboon, an equiparation betwixt the
;; responsibilities and the objects which form the "compile-expression"
;; function's aefauld parameter, represented by the class names, shall
;; be the following adduction's hyle:
;; 
;;   ------------------------------------------------------------------
;;   # | Task                    | "compile-expression" parameter
;;   --+-------------------------+-------------------------------------
;;   1 | Character equality test | character
;;   ..................................................................
;;   2 | String equality test    | string
;;   ..................................................................
;;   3 | Character type test     | META-Type-Test
;;   ..................................................................
;;   4 | Sequential matching     | META-Sequence
;;   ..................................................................
;;   5 | Alternative matching    | META-Alternative
;;   ..................................................................
;;   6 | Repetition              | META-Repetition
;;   ..................................................................
;;   7 | Code evaluation         | META-Evaluation
;;   ------------------------------------------------------------------
;; 
;; 
;; == META'S OPERATIONS: A FUNCTION IN 7 VARIATIONS, AND ONE MACRO ==
;; META's original implementation, a treatise capable of consultation in
;; [baker1991prag], wists of a champarty partaking in by both macros and
;; functions, in which case the formers enjoy a more peisant parcery's
;; recipience in both significance and tally.
;; 
;; Maugre its foundational principles' retention, the solution pursued
;; in this project inclines towards an athwart partage's commitment,
;; inwith whom the devers' allocation favor the functional agents, while
;; concrediting an aefauld macro with an actual participation.
;; 
;; 
;; == "COMPILE-EXPRESSION" FUNCTION AND "MATCH-EXPRESSION" MACRO ==
;; An octuple contingent for operative warklumes, its partage that of
;; a septuple appropriation inwith a single function's variation, its
;; nimiety in ramosity a perclose begotten from the moult species of
;; objects conceivable to entrepart therein, and an aefauld macro whose
;; exclusive contribution registers the aforementioned function's
;; invocation, homologating, by its very nature, a more comfortable
;; syntaxis in the invocation, exhausts the META parser's foundational
;; interface.
;; 
;; The function, specialized on its sole argument to match a septuple of
;; contingencies, the "expression", is nemned:
;; 
;;   compile-expression (expression)
;; 
;; The dependent macro appropriates the agnomination:
;; 
;;   match-expression (expression)
;; 
;; An apercu concerning the function's seven forms shall be adduced
;; alow, entertaining as an additament the cognate macro:
;; 
;; compile-expression (expression : character)
;; compile-expression (expression : string)
;; compile-expression (expression : META-Alternative)
;; compile-expression (expression : META-Evaluation)
;; compile-expression (expression : META-Repetition)
;; compile-expression (expression : META-Sequence)
;; compile-expression (expression : META-Type-Test)
;;   A function which generates the Common Lisp code for testing the
;;   current input source character, or a sequence of character
;;   commencing at the current position into the source, against a
;;   desired character or string; or which, in the case of a
;;   META-Object, probes its compliance with the source content
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
;; 
;; == AN INCHOATE SPECIFICATION OF "COMPILE-EXPRESSION" ==
;; == AND "MATCH-EXPRESSION"                            ==
;; The following treatise's hyle shall accept the "compile-expression"
;; function seven-fold contingency and the sole macro, which in two
;; tiers attain their collaborative entelechy, proceeding from the
;; top-level conspection to the lower stratum.
;; 
;; --------------------------------------------------------------------
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
;;     (a) If the expression represents a character, the generated code
;;         probes the current source character for its presence and
;;         concomitant equality with the expression, itself being a
;;         character.
;;         
;;         Upon the owelty's affirmation, the source's position cursor
;;         is advanced to the next character, while concomitantly a
;;         Boolean value of "true" is returned.
;;         
;;         In the case of the source character's absence or its mismatch
;;         with the expression character, no further causata, except
;;         for a responding with a Boolean "false" sentinel, is
;;         actuated.
;;     
;;     (b) If the expression represents a string, the generated code
;;         probes the source string characters commencing with the
;;         current one for their equality with the expression string.
;;         
;;         Upon the owelty's affimration, the source's position cursor
;;         is advanced to the location in the source immediately
;;         succeeding the matched segment, while concomitantly a Boolean
;;         value of "true" is returned.
;;         
;;         In the case of a mismatch with the expression string, no
;;         further causata, except for a responding with a Boolean
;;         "false", is actuated.
;;   
;;     (c) If the expression represents a "META-Alternative", the
;;         generated code constitutes an OR-combination of the
;;         "META-Alternative" options' produced code fragments, each
;;         such yielded by an invocation of the option "op" in
;;         "compile-expression(op)". Please heed that an empty option
;;         set always fails; this circumstance may be harnessed to
;;         produce a logical contradiction.
;;     
;;     (d) If the expression represents a "META-Evaluation", the
;;         generated code is yielded by simply invoking the contained
;;         form.
;;     
;;     (e) If the expression represents a "META-Repetition", the
;;         generated code constitutes a loop which perpetuates as long
;;         as its contained form "f", in its evaluated guise, obtained
;;         via "compile-expression(f)", yields a generalized boolean
;;         "true" value, with the loop code finally returning a
;;         generalized boolean value of "true" itself.
;;     
;;     (f) If the expression represents a "META-Sequence", the
;;         generated code constitutes an AND-combination of the
;;         "META-Sequence" elements' produced code fragments, each
;;         such yielded by an invocation of the element "elem" in
;;         "compile-expression(elem)". Please heed that an empty
;;         sequence always matches; this circumstance may be harnessed
;;         to produce a logical tautology.
;;     
;;     (g) If the expression represents a "META-Type-Test", the
;;         generated code probes the current source character for its
;;         presence and concomitant compatibility with the type
;;         specifier imposed by the "META-Type-Test".
;;         
;;         Upon this antecedent's satisfaction, the code assigns the
;;         successfully equiparated character to the META object's
;;         target place, advances the source's position cursor to the
;;         next character, and finally returns a Boolean value of
;;         "true".
;;         
;;         In the case of the character's absence or its inconcinnity
;;         with the optated type, no further causata, except for the
;;         responding with a Boolean "false" sentinel, is actuated.
;;     
;;     (h) Any other expression cannot be evaluated, and thus
;;         instigates an error reaction.
;; 
;; --------------------------------------------------------------------
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
;; 
;; == A CURSORY LISTING OF THE "COMPILE-EXPRESSION" VARIANTS ==
;; The following discussion's dation shall patefy in a cursory mete of
;; specification concerning the "compile-expression(...)" function
;; variants in response to their arguments' concrete species.
;; 
;; A treatise endowed with enhaused puissance in its epexegetical
;; designment will constitute the subsequent section's intext.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (expected-content : character)
;;   A function which generates the Common Lisp code for testing the
;;   the current input source character against a desired character or
;;   string.
;;   
;;   Upon the equality's confirmation, this code advances beyond the
;;   probed character in the source, while returning a generalized
;;   boolean value of "true".
;;   
;;   If no match could be confirmed, the source's position cursor does
;;   not move, while the generated code delivers a generalized boolean
;;   value of "false".
;;   
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (expected-content : string)
;;   A function which generates the Common Lisp code for testing the
;;   the current input source characters, commencing from the current
;;   one, against a desired character or string.
;;   
;;   Upon the equality's confirmation, this code advances beyond the
;;   probed character sequence in the source, while returning a
;;   generalized boolean value of "true".
;;   
;;   If no match could be confirmed, the source's position cursor does
;;   not move, while the generated code delivers a generalized boolean
;;   value of "false".
;;   
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (meta : Meta-Alternative)
;;   A function which generates the Common Lisp code for testing several
;;   options for their eligibility with regard to the current input
;;   source state.
;;   
;;   Each option is probed in the specified order, the first
;;   successfully matched specimen's result produces the positive
;;   generalized boolean response, immediately terminating the
;;   compound's perquisition.
;;   
;;   If no option succeeds, a generalized boolean value of "false" is
;;   yielded, signifying a failure.
;;   
;;   An empty option list always fails, producing a logical
;;   contradiction.
;;   
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (meta : Meta-Evaluation)
;;   A function which simply evaluates a form and returns its results.
;;   
;;   The evaluated result is construed as a match's determinator, with
;;   a generalized boolean value of "false" serving to signify a
;;   failure, and a "true" response construed as the success' delegate.
;;   
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (meta : Meta-Repetition)
;;   A function which generates the Common Lisp code for repeatedly
;;   matching a form, while its "compile-expression(...)" invocation
;;   yields a generalized boolean value of "true".
;;   
;;   This function always returns a Boolean value of "true", regardless
;;   of its successful matches' accompt, thus limining a tantamount to
;;   a "Kleene star" in a regular expression.
;;   
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (meta : Meta-Sequence)
;;   A function which generates the Common Lisp code which mandates that
;;   all ensconced expressions match in their specified order.
;;   
;;   In its haecceity's patefaction, the sequence applies an
;;   AND-combination to the comprising forms, each such represented by
;;   their "compile-expression(...)" result in order to contribute
;;   either a success or failure designator.
;;   
;;   If any of the member forms fails, the entire sequence complex
;;   tholes a mismatch's imputation; otherwise, for an enker
;;   satisfaction of sere constituents, the function responds with a
;;   desinent form's return values.
;;   
;;   An empty list of members always succeeds, engendering a logical
;;   tautology's plasmature.
;; 
;; --------------------------------------------------------------------
;; 
;; compile-expression (meta : Meta-Type-Test)
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
;;   This function is utilized by the higher-level "match-expression"
;;   macro, which please see.
;; 
;; 
;; == A DETAILED ACCOUNT ON META'S FUNCTIONS AND MACROS ==
;; The seven subsequent sections' cynosure shall be satisfied in a more
;; liberal and lucid epexegesis' vouchsafement concerning the
;; "compile-expression" variants.
;; 
;; 
;; == "COMPILE-EXPRESSION(CHARACTER)": MATCHING ONE CHARACTER ==
;; Matches the current source character against an expected character.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given a character, "expected-content", a function equiparating with
;; its "compile-expression" causatum may be expressed in the following:
;; 
;; function currentCharacterEquals (expectedContent : character)
;;   if source.currentCharacter = expectedContent then
;;     source.advance()
;;     return true
;;   else
;;     return false
;;   end if
;; end function
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Generates a Common Lisp code fragment which tests whether the input
;; source's current character matches the "expected-content"
;; character, on confirmation advancing to the next position in the
;; input source and returning a generalized boolean "true" value;
;; otherwise produces "false" without modulation of the input source
;; position cursor.
;; 
;; 
;; == "COMPILE-EXPRESSION(STRING): MATCHING A CHARACTER SEQUENCE ==
;; Matches a substring from the source's current position against an
;; expected character sequence.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given a string, "expected-content", a function equiparating with its
;; "compile-expression" causatum may be expressed in the following:
;; 
;; function currentCharactersEqual (expectedContent : string)
;;   let startPosition <- source.position
;;   for each character c in expectedContent do
;;     if c = source.currentCharacter then
;;       source.advance()
;;     else
;;       source.position <- startPosition
;;       return false
;;     end if
;;     return true
;; end function
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Generates a Common Lisp code fragment which tests whether the input
;; source's characters, commencing at the inclusive current position
;; into the same, match the "expected-content" string, on confirmation
;; advancing beyond the successfully probed tmema in the input source
;; and returning a generalized boolean "true" value; otherwise
;; produces "false" without modulation of the input source position
;; cursor.
;; 
;; 
;; == "COMPILE-EXPRESSION(META-ALTERNATIVE)": SELECTION FROM OPTIONS ==
;; Succeeds if any of its options matches, returning the first
;; successfully result.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given the "Meta-Alternative" class, compact of the instance field
;; 
;;   options : (list-of lisp-form)
;; 
;; a function equiparating with its "compile-expression" causatum may
;; be expressed in the following code tmema:
;; 
;; function anyFormMatches (options : list<lispForm>)
;;   for each option opt in options do
;;     let compiledResult <- compileExpression(opt)
;;     if compiledResult matches then
;;       return compiledResult
;;     else
;;       return false
;;     end if
;;   end for
;; end form
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Generates a Common Lisp code fragment which tests the "options",
;; a list of Common Lisp forms, in their specified order for their
;; matching, returning for the first successfully applied member its
;; obtained result, construed as a generalized boolean "true" value.
;; Upon all options' exhaustion without a positive response, a Boolean
;; "false" output is produced.
;; 
;; The thus assembled code establishes an ``or''-combination of the
;; "options", each such represented by the code yielded through its
;; personal "compile-expression(...)" function's invocation.
;; 
;; Please heed that an empty "options" list always fails, engendering
;; a logical contradiction.
;; 
;; 
;; == "COMPILE-EXPRESSION(META-EVALUATION)": EXECUTE A LISP FORM ==
;; Evaluates an arbitrary Common Lisp form, returning its result, with
;; a non-``NIL'' response tantamount to a successful match.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given the "Meta-Evaluation" class, compact of the instance field
;; 
;;   form : lisp-form
;; 
;; a function equiparating with its "compile-expression" causatum may
;; be expressed in the following code tmema:
;; 
;; function evaluateForm (form : lispForm)
;;   let evaluationResult <- evaluate(lispForm)
;;   return evaluationResult.
;; end form
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Evaluates the communicated "form" and returns its result.
;; 
;; If the executed "form"'s primary return value constitutes a
;; generalized boolean "true" value, the META expression is supputated
;; as successfully matched; a ``NIL'' response, on the other hand,
;; conflates with a Boolean "false", and a subsequent failure.
;; 
;; 
;; == "COMPILE-EXPRESSION(META-REPETITION)": MATCH ZERO OR MORE TIMES ==
;; Always succeeds, matching a specified form zero or more times.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given the "Meta-Repetition" class, compact of the instance field
;; 
;;   form : lisp-form
;; 
;; a function equiparating with its "compile-expression" causatum may
;; be expressed in the following code tmema:
;; 
;; function matchesZeroOrMoreTimes (form : lispForm)
;;   repeat do
;;     compileExpression(form)
;;   end repeat
;;   return true
;; end form
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Generates a Common Lisp code fragment which matches the ensconced
;; "form" zero or more times, each time invoking the requisite
;; "compile-expression(form)", and perpetuating this cycle until a
;; generalized boolean response of "false" yields; finally, a Boolean
;; value of "true" is returned, regardless of the mountance of
;; successful invocations.
;; 
;; The represented construct limns a perfect owelty to the "Kleene star"
;; in regular expressions.
;; 
;; 
;; == "COMPILE-EXPRESSION(META-SEQUENCE)": MATCH ALL ==
;; Succeeds if all sequence members, in their specified order, succeed.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given the "Meta-Sequence" class, compact of the instance field
;; 
;;   elements : (list-of lisp-form)
;; 
;; a function equiparating with its "compile-expression" causatum may
;; be expressed in the following code tmema:
;; 
;; function allFormsMatch (elements : list<lispForm>)
;;   let lastCompiledResult <- nil
;;   for each element elem in options do
;;     lastCompiledResult <- compileExpression(elem)
;;     if lastCompiledResult does not match then
;;       return false
;;     end if
;;   end for
;;   return lastCompiledResult
;; end form
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Generates a Common Lisp code fragment which tests the "elements",
;; a list of Common Lisp forms, in their specified order for their
;; matching, succeeding if and only if all members, adhering to this
;; ordonnance, confirm their eligibility, on confirmation returning the
;; desinent form's result, construed as a generalized boolean value.
;; Upon any "element" member's failure, the entire META sequence tholes
;; a rejection, producing a Boolean "false" response.
;; 
;; The thus assembled code establishes an ``and''-combination of the
;; "elements", each such represented by the code yielded through its
;; personal "compile-expression(...)" function's invocation.
;; 
;; Please heed that an empty "elements" list always succeeds,
;; engendering a logical tautology.
;; 
;; 
;; == "COMPILE-EXPRESSION(META-TYPE-TEST)": MATCHING A TYPE ==
;; Succeeds if the source's current character satisfies a type
;; specifier's impositions, in the positive case assigning the probed
;; character to a place, such as a variable.
;; 
;; OPERATIVE DEFINITION
;; --------------------
;; Given the "Meta-Type-Test" class, compact of the instance fields
;; 
;;   expected-type : typeSpecifier
;;   target        : place
;; 
;; a function equiparating with its "compile-expression" causatum may
;; be expressed in the following:
;; 
;; function currentCharacterIsOfType (expectedType : typeSpecifier,
;;                                    target       : place)
;;   if matchesType(source.currentCharacter, expectedType) then
;;     source.advance()
;;     return true
;;   else
;;     return false
;;   end if
;; end function
;; 
;; DETAILED ELUCIDATION
;; --------------------
;; Generates a Common Lisp code fragment which tests whether the
;; input source's current character conforms to the "expected-type",
;; on confirmation storing the just probed character in the place
;; "target", ere advancing the input source's position cursor to the
;; next character, and ultimately returning a generalized boolean
;; "true" value; otherwise produces "false" without the input source
;; position cursor's modulation, or any assignment's involvement.
;; 
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
;;    +--[META-Type-Test]----> |            << function >>             |
;;    |                        |           (typep ... ...)             |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[character]---------> |            << function >>             |
;;    |                        |           (char= ... ...)             |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[string]------------> |            << function >>             |
;;    |                        |          (string= ...  ...)           |
;;    |                        +---------------------------------------+
;;    |
;;    |
;;    |                        +---------------------------------------+
;;    +--[otherwise]---------> |                 error                 |
;;                             +---------------------------------------+
;;                                                 |
;;                                                 V
;;                                                (X)
;; 
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
;;         (declaim (type string    *source-string*))
;;         (declaim (type fixnum    *current-position*))
;;         (declaim (type character *current-character*))
;;         
;;         (defparameter *source-string*    "sparrow")
;;         (defparameter *current-position* 0)
;;         
;;         (define-symbol-macro *current-character*
;;           (the character
;;             (char *source-string* *current-position*)))
;;         
;;         ;; Generate and return, but do not yet execute, code which
;;         ;; matches the EXPECTED-CHARACTER against the
;;         ;; *CURRENT-CHARACTER*.
;;         (defun compile-expression (expected-character)
;;           (declare (type character expected-character))
;;           `(the boolean
;;              (when (and (< *current-position*
;;                            (length *source-string*))
;;                         (char= *current-character*
;;                                ,expected-character))
;;                (incf *current-position*)
;;                T)))
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
;; Date:   2025-12-21
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
;;   [esolang:2024:Adj]
;;   The Esolang contributors, "Adj", February 20th, 2024
;;   URL: "https://esolangs.org/wiki/Adj"
;;   Notes:
;;     - Specification of the "Adj" programming language.
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
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines an ordered list composed of zero or
   more elements, each member of which complies with the ELEMENT-TYPE,
   for the same is imposed the generic sentinel ``*'' as the default
   configuration."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   imposes zero or more entries, each such a composition of a key
   adhering to the KEY-TYPE and a value of the VALUE-TYPE, both being
   governed by the generic sentinel ``*'' as the default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Adj program as a
   one-dimensional simple array of ``Statement'' objects."
  '(simple-array Statement (*)))

;;; -------------------------------------------------------

(deftype variable-set-entry ()
  "The ``variable-set-entry'' type defines an entry partaking in a
   set of mappings atwixen a variable name and its integral value,
   norned also a ``variable-map'', as a cons whose first compartment
   lends a salvatory to the variable identifier, this constituting a
   simple string of one character's componency, and whose second moeity
   ensconces the integer datum."
  '(cons (simple-string 1) integer))

;;; -------------------------------------------------------

(deftype variable-map ()
  "The ``variable-map'' type defines the mappings atwixen variable names
   and their integral states as an association list, or alist, the
   entries being furnished in the plasmature of ``variable-set-entry''
   objects, that is, conses of simple strings and integers."
  '(list-of variable-set-entry))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean paregal thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the source for the META parser.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source*))
(declaim (type fixnum        *current-position*))
(declaim (type character     *current-character*))
(declaim (type boolean       *more-characters-follow-p*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The string to analyze and parse.")

(defparameter *current-position* 0
  "The current index into the ``*SOURCE*'' string.")

(define-symbol-macro *current-character*
  (the character
    (schar *source* *current-position*)))

(define-symbol-macro *more-characters-follow-p*
  (the boolean
    (resolve-to-a-boolean-value
      (array-in-bounds-p *source* *current-position*))))

;;; -------------------------------------------------------

(defun current-character-equals-p (expected-character)
  "Determines whether the source's current character equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character expected-character))
  (the boolean
    (resolve-to-a-boolean-value
      (and *more-characters-follow-p*
        (char= *current-character* expected-character)))))

;;; -------------------------------------------------------

(defun current-characters-match-p (expected-string)
  "Determines whether the source characters commencing at the current
   position into the same replicate the EXPECTED-STRING, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string expected-string))
  (the boolean
    (resolve-to-a-boolean-value
      (string= *source* expected-string
        :start1 *current-position*
        :end1   (min (+ *current-position*
                        (length expected-string))
                     (length *source*))))))

;;; -------------------------------------------------------

(defun current-character-satisfies-p (antecedent)
  "Determines whether the current source character satisfies the
   ANTECEDENT function's impositions, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (function (character) *) antecedent))
  (the boolean
    (resolve-to-a-boolean-value
      (and *more-characters-follow-p*
        (funcall antecedent *current-character*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the META expression classes.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Expression)
  "The ``META-Expression'' interface establishes a common foundry for
   all classes in their pursuit of a META expression's representation.")

;;; -------------------------------------------------------

(defstruct (META-Alternative
  (:include META-Expression))
  "The ``META-Alternative'' class serves in the definition of a META
   expression which matches upon any of its options' success."
  (options (error "Missing options.")
           :type      (list-of T)
           :read-only T))

;;; -------------------------------------------------------

(defstruct (META-Evaluation
  (:include META-Expression))
  "The ``META-Evaluation'' class serves in the definition of a META
   expression whose responsibility appertains to the evaluation of an
   arbitrary Common Lisp, and whose success constitutes a dependency
   upon its result's equiparation with a non-``NIL'' response."
  (form (error "Missing form.")
        :type      T
        :read-only T))

;;; -------------------------------------------------------

(defstruct (META-Repetition
  (:include META-Expression))
  "The ``META-Repetition'' class serves in the definition of a META
   expression whose success renders a tautology, matching an optated
   Common Lisp form zero or more times."
  (form (error "Missing form.")
        :type      T
        :read-only T))

;;; -------------------------------------------------------

(defstruct (META-Sequence
  (:include META-Expression))
  "The ``META-Sequence'' class serves in the definition of a META
   expression which matches if and only if the entirety of its forms,
   in their specified order, succeed."
  (forms (error "Missing forms.")
         :type      (list-of T)
         :read-only T))

;;; -------------------------------------------------------

(defstruct (META-Type-Test
  (:include META-Expression))
  "The ``META-Type-Test'' class serves in the definition of a META
   expression founded upon a character's docimasy for its compliance
   with an imperative type specifier and, upon its covenableness'
   ascertainment, the probed character's assignment to a desiderated
   place."
  (antecedent      (error "Missing antecedent function.")
                   :type      T
                   :read-only T)
  (receiving-place (error "Missing receiving place.")
                   :type      T
                   :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the expression compilation operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compile-expression (expression)
  (:documentation
    "Creates and returns a Common Lisp code tmema which matches the
     EXPRESSION against the source string, and which on a successful
     application returns a \"generalized boolean\" value of \"true\";
     otherwise, upon a mismatch, responds with a \"false\", or ``NIL'',
     sentinel."))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression character))
  "Creates and returns a Common Lisp code tmema which juxtaposes the
   source string's current character with the expected character
   EXPRESSION, and which, upon its equiparation's affirmation, advances
   the position cursor to the next character in the probed source, while
   concomitantly returning a ``boolean'' value of ``T''; but which, for
   a failure in the owelty, administers no causata, except for the
   responding with a ``boolean'' ``NIL'' sentinel."
  (declare (type character expression))
  `(the boolean
     (when (current-character-equals-p ,expression)
       (incf *current-position*)
       T)))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression string))
  "Creates and returns a Common Lisp code tmema which determines whether
   the source string's content, commencing from its current character,
   replicate the string imposed by the string EXPRESSION, and which,
   upon the confluency's affirmation, advances the position cursor to
   the location immediately succeeding the successfully matched section
   in the source, while concomitantly returning a ``boolean'' value of
   ``T''; but which, for a mismatch in any character, abstains from an
   epiphenomenon's adhibition, except for the responding with a
   ``boolean'' ``NIL'' sentinel."
  (declare (type string expression))
  `(the boolean
     (when (current-characters-match-p ,expression)
       (incf *current-position*
         (length ,expression))
        T)))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression META-Alternative))
  "Creates and returns a Common Lisp code tmema which represents the
   attempted matching of any of the META alternative EXPRESSION's
   options, probed in their specified order by an ``or''-concatenation
   of their compiled forms, and which, for the first successful
   application, returns the affirmative member's result; otherwise, in
   the case of all options' mismatch, produces the ``NIL'' sentinel."
  (declare (type META-Alternative expression))
  `(the T
     (or
       ,@(mapcar #'compile-expression
           (meta-alternative-options expression)))))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression META-Evaluation))
  "Evaluates the META evaluation EXPRESSION's ensconced form and returns
   its result."
  (declare (type META-Evaluation expression))
  (the T
    (meta-evaluation-form expression)))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression META-Repetition))
  "Creates and returns a Common Lisp code tmema which repeatedly
   matches the form communicated in the META repetition EXPRESSION,
   aborting only upon the production of a ``NIL'' result, and which
   always produces a ``boolean'' value of ``T'' in order to signify
   zero or more successful applications."
  (declare (type META-Repetition expression))
  `(the boolean
     (loop
       while
         ,(compile-expression
            (meta-repetition-form expression))
       finally
         (return T))))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression META-Sequence))
  "Creates and returns a Common Lisp code tmema which represents the
   attempted matching of the META sequence EXPRESSION's elements in
   their specified order by an ``and''-concatenation of their compiled
   forms, and which, for a successful application of these entire
   elements, reponds with the desinent member's result; otherwise, for
   any membership's failure, produces the ``NIL'' sentinel."
  (declare (type META-Sequence expression))
  `(the T
     (and
       ,@(mapcar #'compile-expression
           (meta-sequence-forms expression)))))

;;; -------------------------------------------------------

(defmethod compile-expression ((expression META-Type-Test))
  "Generates and returns a Common Lisp code tmema which probes the
   source string's current character for its satisfaction of the
   predicate communicated in the META type test EXPRESSION, and which on
   confirmation stores the successfully probed character in the
   place designated in the EXPRESSION, if such does not resolve to the
   ``NIL'' sentinel, while advancing the position cursor to the next
   character into the indagated source, finally returning a ``boolean''
   value of ``T''; but which, upon a mismatch in the character-predicate
   relationship, abstains from any causatum, except for a responding
   with the ``boolean'' ``NIL'' sentinel."
  (declare (type META-Type-Test expression))
  `(the boolean
     (when (current-character-satisfies-p
             #',(meta-type-test-antecedent expression))
       ,(when (meta-type-test-receiving-place expression)
          `(setf ,(meta-type-test-receiving-place expression)
                 *current-character*))
       (incf *current-position*)
       T)))

;;; -------------------------------------------------------

(defmacro match-expression (expression)
  "Matches the EXPRESSION against the source string and returns the
   desinent EXPRESSION form's results, with a \"generalized boolean\"
   value of \"true\" assigned the construe as the success' affirmation;
   while a \"false\", or ``NIL'', response serves in the failure's
   signification."
  (the T
    (compile-expression expression)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of the reader macros.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-macro-character #\{
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Alternative
        (make-meta-alternative :options
          (read-delimited-list #\} stream)))))

;;; -------------------------------------------------------

(set-macro-character #\[
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Sequence
        (make-meta-sequence :forms
          (read-delimited-list #\] stream)))))

;;; -------------------------------------------------------

(set-macro-character #\@
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (destructuring-bind (antecedent &optional (receiving-place NIL))
          (read stream)
        (declare (type T antecedent))
        (declare (type T receiving-place))
        (the META-Type-Test
          (make-meta-type-test
            :antecedent      antecedent
            :receiving-place receiving-place)))))

;;; -------------------------------------------------------

(set-macro-character #\$
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Repetition
        (make-meta-repetition :form
          (read stream)))))

;;; -------------------------------------------------------

(set-macro-character #\!
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the META-Evaluation
        (make-meta-evaluation :form
          (read stream)))))

;;; -------------------------------------------------------

(set-macro-character #\}
  (get-macro-character #\)))

;;; -------------------------------------------------------

(set-macro-character #\]
  (get-macro-character #\)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the adminicular META parser operations.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-the-current-position-saved
    ((&optional (entry-position-variable 'saved-position)
                (reset-function-name     'reset-the-position))
     &body body)
  "Stores the ``*CURRENT-POSITION*'' in a local variable whose
   agnomination is desumed from the ENTRY-POSITION-VARIABLE, while
   concomitantly defining a local function, its stevening appropriated
   from the RESET-FUNCTION-NAME, the invocation of which restores the
   ``*CURRENT-POSITION*'' to its state assumed at the instant of this
   macro's invocation, preserved in the ENTRY-POSITION-VARIABLE, ere
   returning a desiderated value; the macro further evaluating the BODY
   forms, and returning the desinent form's results.
   ---
   This macro's purpose wones in a commodity's dation, intended to be
   employed during the META parser's operations in cases where a
   back-tracking imposes a requisitum."
  `(let ((,entry-position-variable *current-position*))
     (declare (type fixnum ,entry-position-variable))
     (declare (ignorable   ,entry-position-variable))
     (flet ((,reset-function-name (&optional (return-value NIL))
             "Sets the ``*CURRENT-POSITION*'' to the index present at
              this ``with-the-current-position-saved'' macro's
              invocation and responds with the RETURN-VALUE."
             (declare (type T return-value))
             (setf *current-position* ,entry-position-variable)
             (the T return-value)))
       ,@body)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command operand classes.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface establishes a firmament entreparted by all
   classes in their agency as representatives of an Adj command
   argument, the trisulk of which forms a united compound.")

;;; -------------------------------------------------------

(defstruct (Numeric-Operand
  (:include Operand))
  "The ``Numeric-Operand'' interface serves in the vouchsafement of a
   specialized Adj command operand expected to produce, either in a
   per saltum or an intermediate avenue, an integral object.")

;;; -------------------------------------------------------

(defstruct (Input-Operand
  (:include Numeric-Operand))
  "The ``Input-Operand'' class serves in the furnishment of a sentinel
   acting in the agency as an input request in an operand's guise.")

;;; -------------------------------------------------------

(defstruct (Label-Operand
  (:include     Operand)
  (:constructor make-label-operand (name)))
  "The ``Label-Operand'' class serves in the ensconcement of a label
   name in a jump reference's agency, in an operand's guise."
  (name (error "No label name specified.")
        :type      simple-string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Literal-Operand
  (:include     Numeric-Operand)
  (:constructor make-literal-operand (value)))
  "The ``Literal-Operand'' class serves in the ensconcement of an
   integral literal in an operand's guise."
  (value (error "No literal value specified.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Operand
  (:include     Numeric-Operand)
  (:constructor make-variable-operand (name)))
  "The ``Variable-Operand'' class serves in the ensconcement of a
   variable in an operand's guise."
  (name (error "No variable name specified.")
        :type      (simple-string 1)
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Operand
  (:include Operand))
  "The ``Output-Operand'' class serves in the furnishment of a sentinel
   acting in the agency as an output issuance in an operand's guise.")

;;; -------------------------------------------------------

(defstruct (Omitted-Operand
  (:include Operand))
  "The ``Omitted-Operand'' class furnishes a representation of the
   command constituent omission sentinel, signified by the majuscular
   letter \"X\", in an operand's guise.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the statement classes.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Statement)
  "The ``Statement'' interface is apportioned the wike a common
   firmament's furnishment for all classes in a pursuit of an Adj
   statement's incarnation.")

;;; -------------------------------------------------------

(defstruct (Command
  (:include     Statement)
  (:constructor make-command (first-operand
                              second-operand
                              third-operand)))
  "The ``Command'' class serves in the incarnation of an Adj statement
   adhering to the default plasmature, its inchoacy the \"ADJ\" keyword,
   establishing the parasceve to a trisulc champarty of operands, their
   telos the actuation of addition and control flow duction."
  (first-operand  (error "No first operand specified.")
                  :type      Operand
                  :read-only T)
  (second-operand (error "No second operand specified.")
                  :type      Operand
                  :read-only T)
  (third-operand  (error "No third operand specified.")
                  :type      Operand
                  :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Definition
  (:include     Statement)
  (:constructor make-label-definition (name)))
  "The ``Label-Definition'' class furnishes a palpable plasmature for
   statements dedicated to a program label's definition, its compass
   exhausted by the optated label name."
  (name (error "No label name specified.")
        :type      simple-string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (NOP-Statement
  (:include     Statement)
  (:constructor make-a-nop-statement))
  "The ``NOP-Statement'' limns the wike's pernor that imposed upon it
   the communication of a no-operative statement (NOP), a paregal to a
   blank line in an Adj program.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the operands' global constants and variables.  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Input-Operand   +INPUT-OPERAND+))
(declaim (type Output-Operand  +OUTPUT-OPERAND+))
(declaim (type Omitted-Operand +OMITTED-OPERAND+))

;;; -------------------------------------------------------

(defparameter +INPUT-OPERAND+
  (make-input-operand)
  "The ``+INPUT-OPERAND+'' global constant represents the singleton
   instance of an ``Input-Operand'', its solitude's vindication
   emerging from the lacuna and superfluity of any configurations
   applicable to the class' objects.")

(defparameter +OUTPUT-OPERAND+
  (make-output-operand)
  "The ``+OUTPUT-OPERAND+'' global constant represents the singleton
   instance of an ``Output-Operand'', its solitude's vindication
   emerging from the lacuna and superfluity of any configurations
   applicable to the class' objects.")

(defparameter +OMITTED-OPERAND+
  (make-omitted-operand)
  "The ``+OMITTED-OPERAND+'' global constant represents the singleton
   instnce of an ``Omitted-Operand'', its solitude's vindication
   emerging from the lacuna and superfluity of any configurations
   applicable to the class' objects.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the statements' global constants.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type NOP-Statement +NOP-STATEMENT+))

;;; -------------------------------------------------------

(defparameter +NOP-STATEMENT+
  (make-a-nop-statement)
  "The global singleton instance of the ``NOP-Statement'' class, serving
   in the representation of a blank line at any location except for the
   ``*SOURCE*'' string's desinence.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the specialized parsing operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of fixnum) +SPACE-CHARACTER-CODES+))
(declaim (type (list-of fixnum) +NEWLINE-CHARACTER-CODES+))

;;; -------------------------------------------------------

(defparameter +SPACE-CHARACTER-CODES+
  '(9 32)
  "Enumerates the ASCII codes of the spacing characters.")

(defparameter +NEWLINE-CHARACTER-CODES+
  '(10 11 12 13)
  "Enumerates the ASCII codes of the linebreak characters.")

;;; -------------------------------------------------------

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a spacing character,
   which includes in its diorism the traditional space, the horzontal
   tab, and the vertical tabulation, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (find candidate +SPACE-CHARACTER-CODES+
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline character,
   returning on confirmation a ``boolan'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (find candidate +NEWLINE-CHARACTER-CODES+
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a variable identifier's
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not
      (or
        (space-character-p   candidate)
        (newline-character-p candidate)
        (find candidate ":" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Converts the SOURCE into a simple string, either by returning a
   freshly created object of the optated type, upon the SOURCE's failure
   to comply with its stipulations, or, if the SOURCE already subsumes
   into the ``simple-string'' species, by delivering the SOURCE itself.
   ---
   The SOURCE will not be modulated in any case."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents a \"null string\", or empty
   string, returnign on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (resolve-to-a-boolean-value
      (zerop
        (length source)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the identifier validation operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-variable-name-p (identifier)
  "Determines whether the IDENTIFIER represents an admissible variable
   name, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string identifier))
  (the boolean
    (resolve-to-a-boolean-value
      (find identifier '("a" "b" "c") :test #'string=))))

;;; -------------------------------------------------------

(defun valid-label-name-p (identifier)
  "Determines whether the IDENTIFIER represents an admissible label
   name, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string identifier))
  (the boolean
    (resolve-to-a-boolean-value
      (and
        (not (string-is-empty-p identifier))
        (string/=           identifier "ADJ")
        (string/=           identifier "X")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Adj program operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-a-program-from-the-statements (statements)
  "Creates and returns a fresh simple one-dimensional array amplecting
   the list of Adj STATEMENTS."
  (declare (type (list-of Statement) statements))
  (the program
    (coerce statements
      '(simple-array Statement (*)))))

;;; -------------------------------------------------------

(defun remove-all-nop-statements-from-the-program (program)
  "Removes all instances of the global ``NOP-STATEMENT'' from the
   PROGRAM, contingently destructively, and returns the thus purged
   PROGRAM."
  (declare (type program program))
  (the program
    (delete +NOP-STATEMENT+ program :test #'eq)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the specialized parsing operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun optional-spaces ()
  "Parses a catena enumerating zero or more attiguous space characters
   and returns in a Procrustean mode always a ``boolean'' value of
   ``T''."
  (the boolean
    (prog1 T
      (match-expression
        $@(space-character-p)))))

;;; -------------------------------------------------------

(defun token-merist ()
  "Attempts to parse a catena composed of one or more attiguous space
   characters, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (resolve-to-a-boolean-value
      (or
        (match-expression
          [@(space-character-p)
           $@(space-character-p)])
        (error "One or more spaces were expected commencing at the ~
                position ~d, but encountered the ~
                ~:[source exhausted~;character \"~c\" instead~]."
          *current-position*
          *more-characters-follow-p*
          (when *more-characters-follow-p*
            *current-character*))))))

;;; -------------------------------------------------------

(defun probe-for-the-end-of-the-line ()
  "Expecting the ``*CURRENT-POSITION*'' pointer to be located ayond an
   instruction, if not occupying a no-operation line, determines whether
   the coda complies with the expected conformation, scilicet, the
   subsequent intext ostends, besides the homologation of spaces, no
   further content, returning on confirmation a ``boolean'' value of
   ``T''; otherwise signaling an error of an unspecified type."
  (the boolean
    (match-expression
      [
        $@(space-character-p)
        {
          !(not *more-characters-follow-p*)
          @(newline-character-p)
          !(error "The line contains invalid content commencing at ~
                   the position ~d."
             *current-position*)
        }
      ])))

;;; -------------------------------------------------------

(defun read-an-identifier ()
  "Proceeding from the ``*CURRENT-POSITION*'' into the ``*SOURCE*'',
   reads a catena of zero or more identifier characters and returns a
   fresh simple string representation of the thus consumed content."
  (the simple-string
    (convert-into-a-simple-string
      (let ((current-character NIL))
        (declare (type (or null character) current-character))
        (with-output-to-string (name)
          (declare (type string-stream name))
          (match-expression
            $[@(identifier-character-p current-character)
              !(write-char             current-character name)]))))))

;;; -------------------------------------------------------

(defun parse-a-variable-operand ()
  "Attempts to parse a variable name, returning on a match the fresh
   ``Variable-Operand'' representation thereof; otherwise produces the
   ``NIL'' sentinel."
  (the (or null Variable-Operand)
    (with-the-current-position-saved ()
      (let ((variable-name (read-an-identifier)))
        (declare (type string variable-name))
        (if (valid-variable-name-p variable-name)
          (make-variable-operand variable-name)
          (reset-the-position))))))

;;; -------------------------------------------------------

(defun parse-an-output-operand ()
  "Attempts to parse an output operand, designated by the decimal digit
   \"0\", returning on a match the global ``+OUTPUT-OPERAND+'' instance;
   otherwise produces the ``NIL'' sentinel."
  (the (or null Output-Operand)
    (and
      (match-expression #\0)
      +OUTPUT-OPERAND+)))

;;; -------------------------------------------------------

(defun parse-an-input-operand ()
  "Attempts to parse an input operand, designated by the decimal digit
   \"1\", returning on a match the global ``+INPUT-OPERAND+'' instance;
   otherwise produces the ``NIL'' sentinel."
  (the (or null Input-Operand)
    (and
      (match-expression #\1)
      +INPUT-OPERAND+)))

;;; -------------------------------------------------------

(defun parse-an-integer-number ()
  "Attempts to parse a signed or unsigned integer number, returning on
   success the thus yielded integral object; otherwise produces the
   ``NIL'' sentinel."
  (the (or null integer)
    (with-the-current-position-saved ()
      (or
        (ignore-errors
          (parse-integer
            (read-an-identifier)))
        (reset-the-position)))))

;;; -------------------------------------------------------

(defun parse-a-literal-operand ()
  "Attempts to parse a signed or unsigned integer literal, returning on
   a match a fresh ``Literal-Operand'' representation thereof; otherwise
   produces the ``NIL'' sentinel."
  (the (or null Literal-Operand)
    (let ((parsed-number (parse-an-integer-number)))
      (declare (type (or null integer) parsed-number))
      (when parsed-number
        (make-literal-operand parsed-number)))))

;;; -------------------------------------------------------

(defun parse-a-label-operand ()
  "Attempts to parse a label, returning on a match a fresh
   ``Label-Operand'' representation of the obtained identification;
   otherwise responds with the ``NIL'' value."
  (the (or null Label-Operand)
    (with-the-current-position-saved ()
      (let ((label (read-an-identifier)))
        (declare (type string label))
        (if (valid-label-name-p label)
          (make-label-operand label)
          (reset-the-position))))))

;;; -------------------------------------------------------

(defun parse-an-omitted-operand ()
  "Attempts to parse a sentinel signifying an operand's omission, thilk
   conflates in its designment to the \"X\" character, returning on a
   match the global ``Omitted-Operand'' instance; otherwise responds
   with the ``NIL'' value."
  (the (or null Omitted-Operand)
    (and
      (match-expression #\X)
      +OMITTED-OPERAND+)))

;;; -------------------------------------------------------

(defun signal-an-operand-error (operand-position)
  "Signals an error of an unspecified type apprizing about the character
   at the OPERAND-POSITION into the source engaging in no affiliation
   with a recognized operand's identifier and returns no value."
  (declare (type string operand-position))
  (error "Invalid ~a operand commencing at position ~d: \"~c...\"."
    operand-position *current-position* *current-character*)
  (values))

;;; -------------------------------------------------------

(defun attempt-to-parse-an-adj-command ()
  "Attempts to parse an Adj statement inchoating in the \"ADJ\" keyword,
   returning on confirmation a connable ``Command'' representation of
   the extracted operands; otherwise responds with the ``NIL''
   sentinel."
  (the (or null Command)
    (let ((first-operand  NIL)
          (second-operand NIL)
          (third-operand  NIL))
      (declare (type (or null Operand) first-operand))
      (declare (type (or null Operand) second-operand))
      (declare (type (or null Operand) third-operand))
      (and
        (match-expression
          [
            "ADJ"
            !(token-merist)
            
            ;; Parse the first argument.
            {
              !(setf first-operand (parse-an-output-operand))
              !(setf first-operand (parse-an-input-operand))
              !(setf first-operand (parse-a-variable-operand))
              !(setf first-operand (parse-an-omitted-operand))
              !(signal-an-operand-error "first")
            }
            
            !(token-merist)
            
            ;; Parse the second argument.
            {
              !(setf second-operand (parse-a-literal-operand))
              !(setf second-operand (parse-a-variable-operand))
              !(setf second-operand (parse-an-omitted-operand))
              !(signal-an-operand-error "second")
            }
            
            !(token-merist)
            
            ;; Parse the third argument.
            {
              !(setf third-operand (parse-a-literal-operand))
              !(setf third-operand (parse-a-variable-operand))
              !(setf third-operand (parse-a-label-operand))
              !(setf third-operand (parse-an-omitted-operand))
              !(signal-an-operand-error "third")
            }
          ])
        (probe-for-the-end-of-the-line)
        (make-command first-operand second-operand third-operand)))))

;;; -------------------------------------------------------

(defun attempt-to-parse-an-empty-line ()
  "Attempts to parse a blank line, but forming no incolant of the Adj
   source code's desinence, returning on confirmation a
   ``NOP-Statement'' representation; otherwise responds with the ``NIL''
   sentinel."
  (the (or null NOP-Statement)
    (and
      (match-expression
        [
          !*more-characters-follow-p*
          !(newline-character-p *current-character*)
          !(incf *current-position*)
        ])
      +NOP-STATEMENT+)))

;;; -------------------------------------------------------

(defun attempt-to-parse-a-label-definition ()
  "Attempts to parse a label definition line, returning on confirmation
   a ``Label-Definition'' representation thereof; otherwise responds
   with the ``NIL'' sentinel."
  (the (or null Label-Definition)
    (let ((label-name ""))
      (declare (type simple-string label-name))
      (and
        (match-expression
          [
            !(setf label-name (read-an-identifier))
            !(optional-spaces)
            #\:
            !(probe-for-the-end-of-the-line)
          ])
        (if (valid-label-name-p label-name)
          (make-label-definition label-name)
          (error "The name ~s cannot be used to define a label."
            label-name))))))

;;; -------------------------------------------------------

(defun probe-for-an-invalid-line ()
  "Expected to be invoked after the failure to locate an Adj statement
   in a prevenient stage, determines whether the current ``*SOURCE*''
   line contains any character, on confirmation signaling an error of
   an unspecified type, otherwise returning a ``boolean'' value of
   ``NIL'', which attests to the line's conformity to the Adj language
   syntaxis."
  (the boolean
    (match-expression
      [!*more-characters-follow-p*
       !(error "Invalid content has been detected commencing at the ~
                position ~d."
          *current-position*)])))

;;; -------------------------------------------------------

(defun parse-the-next-line ()
  "Parses the next line from the ``*SOURCE*'' and either returns a
   conable ``Statement'' representation thereof, or produces the ``NIL''
   sentinel in order to signify its content's exhaustion."
  (optional-spaces)
  (the (or null Statement)
    (or
      (attempt-to-parse-an-adj-command)
      (attempt-to-parse-an-empty-line)
      (attempt-to-parse-a-label-definition)
      (probe-for-an-invalid-line))))

;;; -------------------------------------------------------

(defun parse-the-program (source)
  "Parses the SOURCE as an Adj program and returns a conable ``program''
   representation of its entailed statements."
  (psetf
    *source*           (convert-into-a-simple-string source)
    *current-position* 0)
  (let ((statements     NIL)
        (next-statement NIL))
    (declare (type (list-of Statement) statements))
    (declare (type (or null Statement) next-statement))
    (match-expression
      $[!(setf next-statement (parse-the-next-line))
        !(push next-statement statements)])
    (the program
      (remove-all-nop-statements-from-the-program
        (assemble-a-program-from-the-statements
          (nreverse statements))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Adj-Error (error)
  ()
  (:documentation
    "The ``Adj-Error'' conditon type furnishes a common foundry
     entreparted by all conditions dedicated to the apprizal about
     anomalous circumstances' transpirations in any of an Adj program's
     stages."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (Adj-Error)
  ((name
    :initarg       :name
    :initform      (error "No offending name has been specified.")
    :reader        duplicate-label-error-name
    :type          simple-string
    :documentation "The label name whose attempted reassigned to a new
                    position has instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Error condition))
      (declare (type stream                stream))
      (format stream "A label with the name ~s has already been ~
                      defined at a prevenient position in the program."
        (duplicate-label-error-name condition))))
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology emerges from the
     attempt to define a label by an agnomination already established
     at a prevenient position in the same Adj program."))

;;; -------------------------------------------------------

(define-condition Unknown-Label-Error (Adj-Error)
  ((name
    :initarg       :name
    :initform      (error "No offending name has been specified.")
    :reader        unknown-label-error-name
    :type          simple-string
    :documentation "The label name whose location's request, maugre
                    the carency of its definition, has served to
                    instigate this error."))
  (:report
    (lambda (condition stream)
      (declare (type Unknown-Label-Error condition))
      (declare (type stream              stream))
      (format stream "No label with the name ~s could be detected."
        (unknown-label-error-name condition))))
  (:documentation
    "The ``Unknown-Label-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology emerges from the
     attempt to navigate to a label whose definition tholes a carency
     in the ensconcing Adj program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the label table.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of simple-string fixnum)
    :documentation "Maps the defined label names to their zero-based
                    positions into a ``program'' vector."))
  (:documentation
    "The ``Label-Table'' class is apportioned that dever thilk
     requires its castaldy of an Adj program's label definitions, the
     vincula such as alligate the name string to their zero-based
     line numbers in the code."))

;;; -------------------------------------------------------

(defun prepare-an-empty-label-table ()
  "Creates and returns a fresh and initially vacant ``Label-Table''."
  (the Label-Table
    (make-instance 'Label-Table)))

;;; -------------------------------------------------------

(defun label-with-the-name-exists-p (labels name)
  "Determines whether the LABELS table wists of an entry amenable to the
   NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table   labels))
  (declare (type simple-string name))
  (the boolean
    (resolve-to-a-boolean-value
      (nth-value 1
        (gethash name
          (slot-value labels 'entries))))))

;;; -------------------------------------------------------

(defun register-the-label (labels name position)
  "Associates the label NAME with its zero-based POSITION into the
   program, stores the vinculum in the LABELS table, and returns no
   value.
   ---
   Upon the participation of an entry with the NAME among the LABELS,
   an error of the type ``Duplicate-Label-Error'' is signaled."
  (declare (type Label-Table   labels))
  (declare (type simple-string name))
  (declare (type fixnum        position))
  (if (label-with-the-name-exists-p labels name)
    (error 'Duplicate-Label-Error :name name)
    (setf (gethash name (slot-value labels 'entries))
          position))
  (values))

;;; -------------------------------------------------------

(defun collect-the-program-labels (program)
  "Generates and returns a fresh ``Label-Table'' associating the Adj
   PROGRAM's label definitions with their zero-based positions into the
   same."
  (declare (type program program))
  (let ((labels (prepare-an-empty-label-table)))
    (declare (type Label-Table labels))
    (loop
      for current-statement of-type Statement across program
      and current-position  of-type fixnum    from   0 by 1
      when (label-definition-p current-statement) do
        (register-the-label labels
          (label-definition-name current-statement)
          current-position))
    (the Label-Table labels)))

;;; -------------------------------------------------------

(defun locate-the-label (labels name)
  "Returns the zero-based position associated with the label NAME in
   the LABELS table; or, upon its disrespondency, signals an error of
   the type ``Unknown-Label-Error''."
  (declare (type Label-Table   labels))
  (declare (type simple-string name))
  (the fixnum
    (or (gethash name (slot-value labels 'entries))
        (error 'Unknown-Label-Error :name name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the variable set.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Set ()
  ((entries
    :initform      (list (cons "a" 0) (cons "b" 0) (cons "c" 0))
    :type          variable-map
    :documentation "An association list (alist) which maps the trisulc
                    of recognized variable names to their signed integer
                    states."))
  (:documentation
    "The ``Variable-Set'' class is concredited with the castaldy of an
     Adj program's trisulc variable set, establishing the vincula
     atwixen the variable names and their integer values."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-variable-set ()
  "Creates and returns a fresh ``Variable-Set'' whose entries' entirety
   assume the default state of zero (0)."
  (the Variable-Set
    (make-instance 'Variable-Set)))

;;; -------------------------------------------------------

(defun retrieve-the-variable-entry (variables name)
  "Returns for the variable amenable to the NAME among the VARIABLES
   set's membership the representative entry as a cons cell; or, upon
   its carency, responds with the ``NIL'' sentinel."
  (declare (type Variable-Set  variables))
  (declare (type simple-string name))
  (the (or null variable-set-entry)
    (assoc name
      (slot-value variables 'entries)
      :test #'string=)))

;;; -------------------------------------------------------

(defun validate-the-variable-name (variables name)
  "Determines whether the NAME references a variable in the VARIABLES
   set, returning on confirmation the associated entry as a cons cell;
   otherwise, signals an error of an unspecified type."
  (declare (type Variable-Set  variables))
  (declare (type simple-string name))
  (the variable-set-entry
    (or (retrieve-the-variable-entry variables name)
        (error "The name ~s does not designate a valid variable."
          name))))

;;; -------------------------------------------------------

(defun variable-value (variables name)
  "Returns the value stored in the variable amenable to the NAME among
   the VARIABLES set's definitions."
  (declare (type Variable-Set  variables))
  (declare (type simple-string name))
  (the integer
    (cdr (validate-the-variable-name variables name))))

;;; -------------------------------------------------------

(defun (setf variable-value) (new-value variables name)
  "Changes the value of the variable amenable to the NAME in the
   VARIABLES set to the NEW-VALUE and returns no value."
  (declare (type integer       new-value))
  (declare (type Variable-Set  variables))
  (declare (type simple-string name))
  (let ((entry (validate-the-variable-name variables name)))
    (declare (type variable-set-entry entry))
    (setf (cdr entry) new-value))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "No program has been specified.")
    :reader        interpreter-program
    :type          program
    :documentation "The Adj program to execute.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          integer
    :documentation "The zero-based position of the instruction pointer
                    (IP) among the PROGRAM's commands.")
   (labels
    :reader        program-labels
    :type          Label-Table
    :documentation "Associates the defined label names with their
                    zero-based positions into the program.")
   (variables
    :initform      (prepare-a-pristine-variable-set)
    :accessor      program-variables
    :type          Variable-Set
    :documentation "Maintains the variables in a name-value mapping."))
  (:documentation
    "The ``Interpreter'' class applies itself to the furnishment of an
     entity responsible for the adhibition of actual efficacy to an Adj
     program supplied in the guise of a statement vector."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Registers the label definitions commorant in the INTERPRETER's Adj
   program, stores these in the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (labels program) interpreter
    (declare (type Label-Table labels))
    (declare (type program     program))
    (setf labels
      (collect-the-program-labels program)))
  (values))

;;; -------------------------------------------------------

(defun prepare-an-interpreter-for (program)
  "Creates and returns a fresh ``Interpreter'', concredited with the
   castaldy and execution of the Adj PROGRAM."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the foundational operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun advance-to-the-next-statement (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   statement in its underlying program and returns no value."
  (declare (type Interpreter interpreter))
  (incf (program-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun assign-the-value-to-the-variable (interpreter target new-value)
  "Assigns the NEW-VALUE to the variable, consigned to the INTERPRETER's
   castaldy and specified by the operand TARGET, and returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type Variable-Operand target))
  (declare (type integer          new-value))
  (with-slots (variables) interpreter
    (declare (type Variable-Set variables))
    (setf
      (variable-value variables
        (variable-operand-name target))
      new-value))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the operand resolution operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric resolve-the-operand-value (interpreter operand)
  (:documentation
    "Returns the value stored in the OPERAND, either as a consequence of
     its immediate datum's inquisition, or per procurationem of its
     content in a variable's case, as construed in the INTERPRETER's
     context.")
  
  (:method ((interpreter Interpreter) (operand Input-Operand))
    (declare (type Interpreter   interpreter)
             (ignore             interpreter))
    (declare (type Input-Operand operand))
    (format T "~&>> ")
    (finish-output)
    (prog1
      (parse-integer
        (read-line NIL NIL "0"))
      (clear-input)))
  
  (:method ((interpreter Interpreter) (operand Literal-Operand))
    (declare (type Interpreter     interpreter)
             (ignore               interpreter))
    (declare (type Literal-Operand operand))
    (the integer
      (literal-operand-value operand)))
  
  (:method ((interpreter Interpreter) (operand Variable-Operand))
    (declare (type Interpreter      interpreter))
    (declare (type Variable-Operand operand))
    (the integer
      (variable-value
        (program-variables interpreter)
        (variable-operand-name operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command's jump parcel.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric jump-to-the-line-at (interpreter destination)
  (:documentation
    "Relocates the INTERPRETER's instruction pointer (IP) to the
     location in its underlying program designated by the DESTINATION
     operand and returns no value.")
  
  (:method ((interpreter Interpreter) (destination Label-Operand))
    (declare (type Interpreter   interpreter))
    (declare (type Label-Operand destination))
    (setf (program-ip interpreter)
      (locate-the-label
        (program-labels     interpreter)
        (label-operand-name destination)))
    (values))
  
  (:method ((interpreter Interpreter) (destination Literal-Operand))
    (declare (type Interpreter     interpreter))
    (declare (type Literal-Operand destination))
    (setf (program-ip interpreter)
      (1- (resolve-the-operand-value interpreter destination)))
    (values))
  
  (:method ((interpreter Interpreter) (destination Omitted-Operand))
    (declare (type Interpreter     interpreter))
    (declare (type Omitted-Operand destination)
             (ignore               destination))
    (advance-to-the-next-statement interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (destination Variable-Operand))
    (declare (type Interpreter      interpreter))
    (declare (type Variable-Operand destination))
    (setf (program-ip interpreter)
      (1- (resolve-the-operand-value interpreter destination)))
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command's addition parcel.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-an-addition-dispatch
    ((first-operand-class second-operand-class)
     &body body)
  "Defines an implementation of the generic function
   ``actuate-the-addition'', its first formal parameter's agnomination
   fixated as ``%interpreter'', specializing on the ``Interpreter''
   class, the second, norned ``%1'' dispatching on the
   FIRST-OPERAND-CLASS, and the third, ``%2'', on the
   SECOND-OPERAND-CLASS, evaluates the BODY forms, and returns no
   value."
  `(defmethod actuate-the-addition
       ((%interpreter Interpreter)
        (%1           ,first-operand-class)
        (%2           ,second-operand-class))
     (declare (type Interpreter           %interpreter)
              (ignorable                  %interpreter))
     (declare (type ,first-operand-class  %1)
              (ignorable                  %1))
     (declare (type ,second-operand-class %2)
              (ignorable                  %2))
     ,@body
     (values)))

;;; -------------------------------------------------------

(defgeneric actuate-the-addition (interpreter
                                  first-operand
                                  second-operand)
  (:documentation
    "Evaluates the jumelle's coefficiency of the FIRST-OPERAND and the
     SECOND-OPERAND, commonly a specification of the \"addition\"
     component, which also enhalses the input and output capacitations,
     in the INTERPRETER's context and returns no value.")
  
  (:method ((interpreter    Interpreter)
            (first-operand  T)
            (second-operand T))
    "The default, or \"fallback\", configuration assigns to the
     combination involving the FIRST-OPERAND and SECOND-OPERAND, no
     homologation, thus ignoring the INTERPRETER, while signaling an
     error of an unspecified type, and ultimately returning no value."
    (declare (type T first-operand))
    (declare (type T second-operand))
    (error "The combination of the operand twain (~a, ~a) fails ~
            to comply with any presently admissible expression of ~
            nomothesia."
      (class-of first-operand)
      (class-of second-operand))
    (values)))

;;; -------------------------------------------------------

(define-an-addition-dispatch (Variable-Operand Literal-Operand)
  (assign-the-value-to-the-variable %interpreter %1
    (+ (resolve-the-operand-value %interpreter %1)
       (resolve-the-operand-value %interpreter %2))))

;;; -------------------------------------------------------

(define-an-addition-dispatch (Variable-Operand Variable-Operand)
  (assign-the-value-to-the-variable %interpreter %1
    (+ (resolve-the-operand-value %interpreter %1)
       (resolve-the-operand-value %interpreter %2))))

;;; -------------------------------------------------------

(define-an-addition-dispatch (Omitted-Operand Omitted-Operand))

;;; -------------------------------------------------------

(define-an-addition-dispatch (Output-Operand Literal-Operand)
  (format T "~&~d"
    (resolve-the-operand-value %interpreter %2)))

;;; -------------------------------------------------------

(define-an-addition-dispatch (Output-Operand Variable-Operand)
  (format T "~&~d"
    (resolve-the-operand-value %interpreter %2)))

;;; -------------------------------------------------------

(define-an-addition-dispatch (Input-Operand Variable-Operand)
  (assign-the-value-to-the-variable %interpreter %2
    (resolve-the-operand-value %interpreter %1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the statement execution operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-the-statement (interpreter statement)
  (:documentation
    "Evaluates the STATEMENT in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter) (command Command))
    (declare (type Interpreter interpreter))
    (declare (type Command     command))
    (actuate-the-addition interpreter
      (command-first-operand  command)
      (command-second-operand command))
    (jump-to-the-line-at interpreter
      (command-third-operand command))
    (values))
  
  (:method ((interpreter Interpreter) (definition Label-Definition))
    (declare (type Interpreter      interpreter))
    (declare (type Label-Definition definition)
             (ignore                definition))
    (advance-to-the-next-statement interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (nop NOP-Statement))
    (declare (type Interpreter   interpreter))
    (declare (type NOP-Statement nop)
             (ignore             nop))
    (advance-to-the-next-statement interpreter)
    (values)))

;;; -------------------------------------------------------

(defun execute-the-current-statement (interpreter)
  "Executes the statement currently selected from the INTERPRETER's
   Adj program and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (process-the-statement interpreter
      (aref program ip)))
  (values))

;;; -------------------------------------------------------

(defun program-has-halted-p (interpreter)
  "Determines whether the Adj program consigned to the INTERPRETER's
   castaldy has been executed in its entirety, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (program ip) interpreter
      (declare (type program program))
      (declare (type fixnum  ip))
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun start-the-interpreter (interpreter)
  "Launches the INTERPRETER's operations, executing the Adj program
   concredited to its castaldy, and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-has-halted-p interpreter) do
    (execute-the-current-statement interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpretation operation.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-adj-code (code)
  "Interprets the piece of Adj source CODE and returns no value."
  (declare (type string code))
  (start-the-interpreter
    (prepare-an-interpreter-for
      (parse-the-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add one (1) to one (1) and print the sum (2 = 1 + 1).
(interpret-the-adj-code
  "ADJ a 1 X
   ADJ b 1 X
   ADJ a b X
   ADJ 0 a X")

;;; -------------------------------------------------------

;; Query the user for two integer numbers and print the sum.
(interpret-the-adj-code
  "ADJ 1 a X
   ADJ 1 b X
   ADJ a b X
   ADJ 0 a X")

;;; -------------------------------------------------------

;; Query for a line number to navigate to and jump thither.
(interpret-the-adj-code
  "ADJ 1 a X
   ADJ X X a")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-adj-code
  "ADJ 1 a X
   ADJ b 7 X
   ADJ b a X
   ADJ b a X
   ADJ 0 a X
   ADJ X X b
   case_of_zero:
   ADJ X X 11
   case_of_one:
   ADJ 0 a case_of_one")
