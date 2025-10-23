;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "DerpScrp", invented by the Esolang user "A" and presented
;; on July 3rd, 2019, the commorancy of its diorism is located in a
;; derivation from "DerpText" by the Esolang user "Fuckerturd", with a
;; concomitant curtailment in the complexity, while yet retaining the
;; self-modifying program capacity of the source code, maintained by a
;; dedicated variable among the infinite tally's standard treble.
;; 
;; 
;; Concept
;; =======
;; The DerpScrp programming language subsumes into the species of that
;; esoteric programming language whose potential for self-modification
;; homologates the program's dynamic modulation during the execution
;; phase; in this particular case registering its entelechy in a member
;; of the standard variables' gleek, nevened as "C", in the compernage
;; of its twain "A" and "B", as well as the contingency for an infinite
;; tally of siclike string-valued succedanea defined by the programmer.
;; 
;; == DERPSCRP PROGRAM'S HOMOLOGATE THEIR MODIFICATION ==
;; Begotten by a derivation from the language DerpText, DerpScrp
;; appropriates the kenspeckle proprium of the warklume's accoutrement
;; for the source code's immediate modulation.
;; 
;; This faculty's firmament emerges from the provenance of its conjoined
;; data and code principles as adjustable strings. The program,
;; alligated into a consanguinity with the console and text variables,
;; participates in an overt access of its content for perquisitions and
;; alterations, the latter species of adit engenders the dynamic
;; conduction of the runtime deportment.
;; 
;; == EXECUTION: COMMAND DETECTION, EXTRACTION, EVALUATION, ITERANCE ==
;; A DerpScrp program's execution complies with the following ordered
;; tesseratomy:
;; 
;;   (1) COMMAND DETECTION:
;;       Proceeding from the start position into the command string,
;;       curated under the castaldy of the variable "C", the leading
;;       operation's detection and selection, in the intent's pursuit
;;       of a subsequent execution, is performed.
;;   
;;   (2) COMMAND EXCISION:
;;       The just delineated command tmema is subjected to an excision
;;       from the program string's front; as a consectary and a
;;       concomitant, during the command's following execution, the
;;       prevenient state escapes the apprehension in favor of the
;;       newly curtailed one. This modification, of course, applies to
;;       the value of the "C" variable in an epiphenomenal fashion.
;;   
;;   (3) COMMAND EXECUTION:
;;       The selected command's execution is actuated; this procession
;;       cannot rely on the already excised command's own code portion.
;;   
;;   (4) REPETITION:
;;       If no vacancy inflicts the program string, stored in the
;;       command variable "C", the process perpetuates its iterance by
;;       a recurrence in the step -> (1); otherwise, for a now empty
;;       source code, the cycle's termination conflates with a cessation
;;       in the program.
;; 
;; == VARIABLES MAP IDENTIFIERS TO STRINGS ==
;; The paravaunt mode of data castaldy is apportioned to the variable
;; registry's bailiwick, where an arbitrary accompt of placeholders,
;; in addition to the autochthonous triple of "A", "B", and "C",
;; partakes of the salvatories' accommodation for string objects.
;; 
;; == VARIABLE NAMES COMPREHEND ZERO OR MORE CHARACTERS ==
;; A variable's agnomination lays its amplection around zero or more
;; characters as the identifying component, with a homologation that
;; generally wists of no impositions in the haecceity; however, no
;; variable name's conformation may tolerate the inclusion of symbols
;; necessitated for a command's assemblage.
;; 
;; For instance, the value exchange/assignment command
;; 
;;   =leftArgument=rightArgument=
;; 
;; eschews the incorporation of the equality sign "=" in any of its
;; argument jumelles "leftArgument" and "rightArgument", forecause an
;; infliction with ambivalency emerges from siccan combination.
;; 
;; == INTRINSIC VARIABLES ==
;; The compass of autochthonous succedanea is restricted to a pair
;; royal in membership, and shall be a cursory ilk of elucidation's
;; cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Variable | Internal name  | Role
;;   ---------+----------------+---------------------------------------
;;   A        | output string  | Represents the console content.
;;            |                | This text is printed perpetually, with
;;            |                | a prerogation of 0.5 seconds betwixt
;;            |                | any two cycles, ere the screen's
;;            |                | clearance.
;;            |                |---------------------------------------
;;            |                | It is homologated to be empty.
;;   ..................................................................
;;   B        | text string    | Is initialized by the user in a step
;;            |                | prevenient to the program's
;;            |                | inchoation.
;;            |                |---------------------------------------
;;            |                | It is homologated to remain empty.
;;   ..................................................................
;;   C        | command string | Contains the program source code.
;;            |                | During their processing, the commands
;;            |                | are being deleted from this string's
;;            |                | beginning.
;;   ------------------------------------------------------------------
;; 
;; == DISTINGUISHMENT BETWIXT VARIABLES AND CONSTANTS ==
;; In the face of the syntactical carency concerning a merist's
;; assignment to the construe of variables in counterdistinguishment
;; from constants, the nomothesia's imposition holds that any identifier
;; lacking a registration as a variable name by default attains a
;; literal's role.
;; 
;; Such a succedaneum binding's establishment ensues from the
;; contingency of the exchange/assignment behest
;; 
;;   =leftArgument=rightArgument=
;; 
;; where the sinistral "leftArgument" may, if not already defined in
;; this term, enjoy the "rightArgument" content's reception.
;; 
;; == CONSOLE OUTPUT UPDATES IN CYCLES OF HALF-SECONDS ==
;; DerpScrp's output visualization entertains a gendrure from the
;; champarty of the executing environment's textual console and the
;; output variable "A", governed in this aspect by a lealty to the
;; following principle:
;; 
;;   (1) The console content, stored in the variable "A", is issued to
;;       the console's output conduit.
;;   
;;   (2) The display remains unaltered for 0.5 seconds.
;;   
;;   (3) As the delay's sequela, the console is cleared, seguing into
;;       an iterum application of the first step -> (1).
;; 
;; 
;; Instructions
;; ============
;; A mere quintuple contingency accomplishes the instruction set's
;; exhaustion, a few specimens among this membership entalented with a
;; variation on the deportment as an ultimity of the concrete arguments'
;; nature.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall contribute the necessary nortelry
;; concerning the language's operative avails.
;; 
;; Please heed the delineation of succedaneous tmemata by catenas of
;; asterisks ("*"), intended for their supplanting by actual DerpScrp
;; code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command               | Effect
;;   ----------------------+-------------------------------------------
;;   =variable1=variable2= | Exchanges the content of the variable
;;    ********* *********  | {variable1} with that of {variable2}, and
;;                         | vice versa.
;;                         |-------------------------------------------
;;                         | {variable1} must be a variable name, its
;;                         | agnomination admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | equality sign "=".
;;                         |-------------------------------------------
;;                         | {variable2} must be a variable name, its
;;                         | agnomination admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | equality sign "=".
;;                         |-------------------------------------------
;;                         | In a pseudocode diction, it holds:
;;                         |   let value1 <- valueOf(variable1)
;;                         |   variable1  <- valueOf(variable2)
;;                         |   variable2  <- value1
;;   ..................................................................
;;   =variable=constant=   | Sets the content of the variable
;;    ******** ********    | {variable} to the value {constant}.
;;                         |-------------------------------------------
;;                         | {variable} must be a variable name, its
;;                         | agnomination admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | equality sign "=".
;;                         |-------------------------------------------
;;                         | {constant} must be a character sequence,
;;                         | its agnomination admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | equality sign "=".
;;                         |-------------------------------------------
;;                         | In a pseudocode diction, it holds:
;;                         |   variable1 <- constant
;;   ..................................................................
;;   >variable>constant>   | Removes from the content of the variable
;;    ******** ********    | {variable} all occurrencies of the
;;                         | {constant}.
;;                         |-------------------------------------------
;;                         | {variable} must be a variable name, its
;;                         | agnomination admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | greater than sign ">".
;;                         |-------------------------------------------
;;                         | {constant} must be a character sequence,
;;                         | its componency admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | greater than sign ">".
;;   ..................................................................
;;   <variable<constant<   | Removes from the content of the variable
;;    ******** ********    | {variable} all characters except those
;;                         | equal to the {constant}.
;;                         |-------------------------------------------
;;                         | {variable} must be a variable name, its
;;                         | agnomination admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | less than sign "<".
;;                         |-------------------------------------------
;;                         | {constant} must be a character sequence,
;;                         | its componency admitting as constituents
;;                         | zero or more characters, exempting the
;;                         | less than sign "<".
;;   ..................................................................
;;   &expression&          | Appends the content of the {expression} to
;;    **********           | the output console.
;;                         |-------------------------------------------
;;                         | This operation, with the inherent variable
;;                         | "A" appertaining to the output console,
;;                         | modifies same variable's content.
;;                         |-------------------------------------------
;;                         | {expression} must be a variable name or a
;;                         | constant value; in the former cases
;;                         | desisting in its agnomination from the
;;                         | inclusion of the ampersand sign "&", in
;;                         | the latter case avoiding thilk in its
;;                         | character sequence.
;;   ..................................................................
;;   +expression+          | Appends the content of the {expression} to
;;    **********           | the program string.
;;                         |-------------------------------------------
;;                         | This operation, with the inherent variable
;;                         | "C" appertaining to the program string,
;;                         | modifies same variable's content.
;;                         |-------------------------------------------
;;                         | {expression} must be a variable name or a
;;                         | constant value; in the former cases
;;                         | desisting in its agnomination from the
;;                         | inclusion of the plus sign "+", in the
;;                         | latter case avoiding thilk in its
;;                         | character sequence.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The project at hand has been implemented in the programming language
;; Common Lisp, its operative causata's adhibition a sequela from the
;; immediate DerpScrp source code's consumption, whence the most
;; recently detected instruction's reformulation as a dedicated object
;; furnishes a parasceve to its operative causata's adhibition.
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
;; Date:   2025-03-11
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
;;   [esolang2019DerpScrp]
;;   The Esolang contributors, "DerpScrp", December 25th, 2019
;;   URL: "https://esolangs.org/wiki/DerpScrp"
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
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   admits zero or more entries, everichon among this membership's
   compass allying a key of the KEY-TYPE to a value complying with the
   VALUE-TYPE, for both is imposed the generic sentinel ``*'' as a
   default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (or
              (and (eq key-type   '*)
                   (eq value-type '*))
              (loop
                for current-key
                  of-type T
                  being the hash-keys in (the hash-table candidate)
                using
                  (hash-value current-value)
                always
                  (and
                    (or (eq    key-type '*)
                        (typep current-key key-type))
                    (or (eq    value-type '*)
                        (typep current-value value-type))))))))
    `(satisfies ,predicate)))



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
;; -- Implementation of string operations for META parser.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substring-starts-at-p (haystack start-index-into-haystack needle)
  "Determines whether the NEEDLE occurs entirely in the HAYSTACK,
   commencing at the inclusive START-INDEX-INTO-HAYSTACK, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string haystack))
  (declare (type fixnum        start-index-into-haystack))
  (declare (type simple-string needle))
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

(defstruct (META)
  "The ``META'' interface establishes a common foundry upon which the
   edifications of the concrete META expressions are manifested.")

;;; -------------------------------------------------------

(defgeneric compile-meta (meta)
  (:documentation
    "Generates and returns a Common Lisp code fragment which probes
     the META expression for its compatibility with the *SOURCE-CODE*'s
     currens state and which responds upon confirmation a \"generalized
     boolean\" true value, otherwise false."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Alternative".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Alternative
  (:include     META)
  (:constructor make-meta-alternative (alternatives)))
  "The ``META-Alternative'' class represents a META expression which
   succeeds if any of its forms, probed in their specified order,
   match.
   ---
   An empty list of alternatives is expected to always fail."
  (alternatives (error "Missing META alternatives.")
                :type      list
                :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta ((meta META-Alternative))
  "Generates and returns a piece of Common Lisp source code which
   succeeds if at least one of the META alternative option forms, probed
   in their specified order, matches.
   ---
   An empty form sequence always fails."
  (declare (type META-Alternative meta))
  `(the T
     (or ,@(mapcar #'compile-expression
             (meta-alternative-alternatives meta)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Alternative".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Evaluation
  (:include     META)
  (:constructor make-meta-evaluation (form)))
  "The ``META-Evaluation'' class serves in the representation of a META
   expression whose dedication appertains to the unconditional
   evaluation of its ensconced form, in its diorism amplected the
   potential for side effects."
  (form (error "Missing META evaluation form.")
        :type      T
        :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta ((meta META-Evaluation))
  "Evaluates the META evaluation's ensconced form and returns the
   results produced by the same."
  (declare (type META-Evaluation meta))
  (the T
    (meta-evaluation-form meta)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "META-Repetition".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Repetition
  (:include     META)
  (:constructor make-meta-repetition (form)))
  "The ``META-Repetition'' class furnishes the manifestation of an
   expression which always succeeds, probing the ensconced Common Lisp
   form zero or more times until its failure to comply with the imposed
   rules."
  (form (error "Missing META repetition form.")
        :type      T
        :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta ((meta META-Repetition))
  "Generates and returns a piece of Common Lisp source code which
   perpetually tests the META repetition's ensconced form for its
   success, its cessation eventuated by the first form mismatch,
   returning in any case, even for a lacuna of a single match, a
   ``boolean'' value of ``T''."
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
  (:include     META)
  (:constructor make-meta-sequence (forms)))
  "The ``META-Sequence'' class represents a META expression which
   succeeds if all of its forms, probed in their specified order,
   match.
   ---
   An empty list of forms is expected to always succeed."
  (forms (error "Missing META sequence forms.")
         :type      list
         :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta ((meta META-Sequence))
  "Generates and returns a piece of Common Lisp source code which
   succeeds if all the META sequence forms in their specified order
   match.
   ---
   An empty form sequence always matches."
  (declare (type META-Sequence meta))
  `(the T
     (and ,@(mapcar #'compile-expression
              (meta-sequence-forms meta)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of clas "META-Type-Test".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (META-Type-Test
  (:include     META)
  (:constructor make-meta-type-test (expected-type receiving-place)))
  "The ``META-Evaluation'' class establishes an expression that probes
   a character for its membership regarding a type specifier, on
   confirmation storing the candidate in a specified place, commonly a
   variable."
  (expected-type   (error "Missing expected type.")
                   :type      T
                   :read-only T)
  (receiving-place (error "Missing receiving place.")
                   :type      T
                   :read-only T))

;;; -------------------------------------------------------

(defmethod compile-meta ((meta META-Type-Test))
  "Generates and returns a piece of Common Lisp source code which
   determines whether the current *SOURCE-CHARACTER* complies to the
   META type test's type specifier, returning on confirmation a
   ``boolean'' of ``T'', while concomitantly storing the probed object
   in the META instance's receiving place, and advancing beyond the
   *SOURCE-POSITION*; otherwise responds with ``NIL''."
  (declare (type META-Type-Test meta))
  `(the boolean
     (match-type
       ,(meta-type-test-expected-type   meta)
       ,(meta-type-test-receiving-place meta))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-simple-string (source)
  "Returns a fresh simple string representation of the SOURCE."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defmacro with-output-to-simple-string ((stream-variable) &body body)
  "Creates a string output stream, binds it to a variable agnominated by
   the STREAM-VARIABLE, evaluates the BODY forms, and returns a fresh
   simple string representation of the stream's result."
  `(the simple-string
     (convert-into-simple-string
       (with-output-to-string (,stream-variable)
         (declare (type string-stream ,stream-variable))
         ,@body))))

;;; -------------------------------------------------------

(defun concatenate-strings (first-source second-source)
  "Appends the SECOND-SOURCE to the FIRST-SOURCE and returns a fresh
   string representing this concatenation.
   ---
   Neither of the input objects are subjected to modifications."
  (declare (type simple-string first-source))
  (declare (type simple-string second-source))
  (the simple-string
    (convert-into-simple-string
      (format NIL "~a~a" first-source second-source))))

;;; -------------------------------------------------------

(defun string-is-empty-p (candidate)
  "Determines whether the CANDIDATE represents the null string, that is,
   a character sequence of length zero (0), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string candidate))
  (the boolean
    (get-boolean-value-of
      (zerop
        (length candidate)))))

;;; -------------------------------------------------------

(defun copy-substring-into-stream (destination
                                   source source-start source-end)
  "Copies the tmema from the SOURCE string, inchoating at the inclusive
   SOURCE-START index and terminating in the exclusive SOURCE-END
   position, to the DESTINATION stream and returns no value."
  (declare (type string-stream destination))
  (declare (type simple-string source))
  (declare (type fixnum        source-start))
  (declare (type fixnum        source-end))
  (write-string source destination :start source-start :end source-end)
  (values))

;;; -------------------------------------------------------

(defun locate-token (needle haystack haystack-start)
  "Searches for the NEEDLE in the HAYSTACK, commencing in the latter at
   the inclusive HAYSTACK-START index, and returns the position into
   the HAYSTACK of the NEEDLE's first character; otherwise, upon the
   detection's failure, responds with the HAYSTACK's length."
  (declare (type simple-string needle))
  (declare (type simple-string haystack))
  (declare (type fixnum haystack-start))
  (the fixnum
    (or (search needle haystack :start2 haystack-start :test #'char=)
        (length haystack))))

;;; -------------------------------------------------------

(defun replace-all-occurrences-in-string (source eject substitute)
  "If the EJECT does not represent an empty string, creates and returns
   a fresh string which entails the SOURCE's characters, but
   supersedes all of the EJECT's occurrencies by the SUBSTITUTE;
   otherwise, for an EJECT destitute of any characters, responds with
   the SOURCE itself."
  (declare (type simple-string source))
  (declare (type simple-string eject))
  (declare (type simple-string substitute))
  (the simple-string
    (if (string-is-empty-p eject)
      source
      (with-output-to-simple-string (output)
        (loop
          for search-start-point
            of-type fixnum
            =       0
            then    (+ eject-start-point (length eject))
          
          for eject-start-point
            of-type fixnum
            =       (locate-token eject source search-start-point)
          
          do
            (copy-substring-into-stream
              output
              source
              search-start-point
              eject-start-point)
          
          ;; Repeat while the eject has been detected.
          while (< eject-start-point (length source)) do
            (format output "~a" substitute))))))

;;; -------------------------------------------------------

(defun remove-all-occurrences-in-string (source eject)
  "Creates and returns a fresh string which retains from the SOURCE all
   content except for the EJECT's occurrencies."
  (declare (type simple-string source))
  (declare (type simple-string eject))
  (the simple-string
    (replace-all-occurrences-in-string source eject "")))

;;; -------------------------------------------------------

(defun retain-all-occurrences-in-string (source desideratum)
  "Creates and returns a fresh string which removes from the SOURCE all
   content except for the occurrencies of the DESIDERATUM.
   ---
   If the DESIDERATUM represents the empty string, the result will per
   force entail no characters at all."
  (declare (type simple-string source))
  (declare (type simple-string desideratum))
  (the simple-string
    (cond
      ((string-is-empty-p source)      "")
      ((string-is-empty-p desideratum) "")
      (T
        (with-output-to-simple-string (output)
          (loop
            for search-start-point
              of-type fixnum
              =       0
              then    (+ desideratum-start-point
                         (length desideratum))
            
            for desideratum-start-point
              of-type fixnum
              =       (locate-token
                        desideratum
                        source
                        search-start-point)
            
            while (< desideratum-start-point (length source)) do
              (format output "~a" desideratum)))))))



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

(define-symbol-macro *source-length*
  (the fixnum
    (length *source-code*)))

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
  "Sets the program *SOURCE-CODE* to the NEW-CODE, resets the
   appertaining state variables, and returns no value."
  (declare (type simple-string new-code))
  (psetf *source-code*     new-code
         *source-position* 0)
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
    "Generates and returns a piece of Common Lisp source code which
     probes whether the current *SOURCE-CHARACTER* matches the
     EXPECTED-CHARACTER, and which on confirmation returns a ``boolean''
     value of ``T'', while concomitantly advancing the *SOURCE-POSITION*
     cursor to the next character in the *SOURCE-CODE*; otherwise the
     code produces ``NIL''."
    (declare (type character expected-character))
    `(the boolean
       (when (and *source-has-next-character-p*
                  (char= *source-character* ,expected-character))
         (incf *source-position*)
         T)))
  
  (:method ((expected-string string))
    "Generates and returns a piece of Common Lisp source code which
     probes whether the *SOURCE-CODE* characters, commencing at the
     *SOURCE-POSITION*, replicate the EXPECTED-STRING, and which on
     confirmation returns a ``boolean'' value of ``T'', while
     concomitantly advancing the *SOURCE-POSITION* cursor beyond the
     matched tmema; otherwise the code produces ``NIL''."
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
  "Determines whether the EXPECTED-CONTENT, represented by an aefauld
   character or sequence of such, is replicated in the *SOURCE-CODE*'s
   current state, returning on confirmation a ``boolean'' value of
   ``T'', while concomitantly advancing the *SOURCE-POSITION* cursor
   beyond the matched tmema; otherwise produces ``NIL''."
  (generate-content-match-code expected-content))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type matching code generator.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-type-match-code (expected-type output-variable)
  "Generates and returns a Common Lisp code tmema which probes whether
   the current *SOURCE-CHARACTER* complies with the EXPECTED-TYPE, on
   confirmation storing the probed character in the OUTPUT-VARIABLE,
   while advancing to the next position in the *SOURCE-CODE*, finally
   returning a ``boolean'' value of ``T''; otherwise responds with
   ``NIL'' without any epiphenomena's actuation."
  (declare (type T      expected-type))
  (declare (type symbol output-variable))
  `(the boolean
     (when (and *source-has-next-character-p*
                (typep *source-character* ',expected-type))
       (setf ,output-variable *source-character*)
       (incf *source-position*)
       T)))

;;; -------------------------------------------------------

(defmacro match-type (expected-type output-variable)
  "Determines whether the current *SOURCE-CHARACTER* complies with the
   EXPECTED-TYPE specifier, on confirmation storing the probed object
   in the place designated by the OUTPUT-VARIABLE, while concomitantly
   advancing the *SOURCE-POSITION* cursor to the next character, and
   finally returning a ``boolean'' value of ``T''; otherwise produces
   ``NIL''."
  (generate-type-match-code expected-type output-variable))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression parser.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compile-expression (expression)
  (:documentation
    "Generates and returns a Common Lisp tmema which engages in the
     assessment of the EXPRESSION's conformity with the current
     *SOURCE-CODE* state, returning on the compliance's confirmation a
     ``boolean'' value of ``T'', contingently attended by epiphenomenal
     causata; otherwise the code produces ``NIL''.")
  
  (:method ((expression character))
    "Determines whether the current *SOURCE-CHARACTER* conflates with
     the notion of the EXPRESSION symbol, returning on confirmation a
     ``boolean'' value of ``T'', while concomitantly advancing the
     *SOURCE-POSITION* cursor to the next index; otherwise respond with
     ``NIL''."
    (declare (type character expression))
    `(the boolean
       (match-content ,expression)))
  
  (:method ((expression string))
    "Proceeding from the *SOURCE-POSITION* into the *SOURCE-CODE*,
     determines whether the subsequent characters replicate the
     EXPRESSION, returning on confirmation a ``boolean'' value of ``T'',
     while concomitantly advancing the *SOURCE-POSITION* cursor beyond
     the matched tmema in the *SOURCE-CODE*; otherwise responds with
     ``NIL''."
    (declare (type string expression))
    `(the boolean
       (match-content ,expression)))
  
  (:method ((expression META))
    "Determines whether the *SOURCE-CODE*'s state complies with the
     META EXPRESSION's stipulations, returning on confirmation a
     ``boolean'' value of ``T'', otherwise ``NIL''."
    (declare (type META expression))
    (the T
      (compile-meta expression))))

;;; -------------------------------------------------------

(defmacro match-expression (expression)
  "Attempts to match the EXPRESSION against the *SOURCE-CODE*'s current
   state, returning in the case of a successful application a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (compile-expression expression))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of reader macros.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-macro-character #\{
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the Meta-Alternative
        (make-meta-alternative
          (read-delimited-list #\} stream T)))))

;;; -------------------------------------------------------

(set-macro-character #\[
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the Meta-Sequence
        (make-meta-sequence
          (read-delimited-list #\] stream T)))))

;;; -------------------------------------------------------

(set-macro-character #\$
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the Meta-Repetition
        (make-meta-repetition
          (read stream)))))

;;; -------------------------------------------------------

(set-macro-character #\!
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (the Meta-Evaluation
        (make-meta-evaluation
          (read stream)))))

;;; -------------------------------------------------------

(set-macro-character #\@
  #'(lambda (stream macro-character)
      (declare (type stream    stream))
      (declare (type character macro-character))
      (declare (ignore         macro-character))
      (destructuring-bind (expected-type receiving-place)
          (read stream)
        (declare (type T expected-type))
        (declare (type T receiving-place))
        (the Meta-Type-Test
          (make-meta-type-test expected-type receiving-place)))))

;;; -------------------------------------------------------

(set-macro-character #\}
  (get-macro-character #\) NIL))

;;; -------------------------------------------------------

(set-macro-character #\]
  (get-macro-character #\) NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command classes.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface serves the dever of a common firmament's
   establishment dedicated to all classes intent on the representation
   of DerpScrp instructions.")

;;; -------------------------------------------------------

(defstruct (Abstract-Command
  (:include   Command)
  (:conc-name command-))
  "The ``Abstract-Command'' abstract class applies itself to the
   encapsulation of those pieces of information requisite to and
   entreparted by all ``Command'' interface implementations."
  (start-position (error "Missing start position.")
                  :type      fixnum
                  :read-only T)
  (end-position   (error "Missing end position.")
                  :type      fixnum
                  :read-only T))

;;; -------------------------------------------------------

(defstruct (Exchange-Command
  (:include Abstract-Command))
  "The ``Exchange-Command'' class serves in the encapsulation of a
   steven to either exchange two variables' contents, or such to
   replace one variable's string by a constant, manifesting in the
   DerpScrp programming language as the forbisen
   \"=variable1=variable2orConstant=\"."
  (variable-1   (error "Missing first variable.")
                :type      simple-string
                :read-only T)
  (expression-2 (error "Missing second variable or expression.")
                :type      simple-string
                :read-only T))

;;; -------------------------------------------------------

(defstruct (Remove-Command
  (:include Abstract-Command))
  "The ``Remove-Command'' class serves in the encapsulation of a steven
   to remove from a variable's content a constant, manifesting in the
   DerpScrp programming language as the forbisen
   \">variable>constant>\"."
  (haystack (error "Missing haystack variable.")
            :type      simple-string
            :read-only T)
  (needle   (error "Missing needle constant.")
            :type      simple-string
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Keep-Command
  (:include Abstract-Command))
  "The ``Keep-Command'' class serves in the encapsulation of a steven
   to retaim in a variable's content a constant, manifesting in the
   DerpScrp programming language as the forbisen
   \"<variable<constant<\"."
  (haystack (error "Missing haystack variable.")
            :type      simple-string
            :read-only T)
  (needle   (error "Missing needle constant.")
            :type      simple-string
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Append-To-Console-Command
  (:include Abstract-Command))
  "The ``Append-To-Console-Command'' class serves in the encapsulation
   of a steven to append a string to the output console, manifesting in
   the DerpScrp programming language as the forbisen \"&text&\"."
  (text (error "Missing text.") :type simple-string :read-only T))

;;; -------------------------------------------------------

(defstruct (Append-To-Program-Command
  (:include Abstract-Command))
  "The ``Append-To-Program-Command'' class serves in the encapsulation
   of a steven to append a string to the program, manifesting in the
   DerpScrp programming language as the forbisen
   \"+variableOrConstant+\"."
  (expression (error "Missing expression.")
              :type      simple-string
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun non-ampersand-character-p (candidate)
  "Determines whether the CANDIDATE represents any character except for
   the ampersand (\"&\"), returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (char/= candidate #\&))))

;;; -------------------------------------------------------

(defun non-plus-symbol-character-p (candidate)
  "Determines whether the CANDIDATE represents any character except for
   the \"plus\" symbol (\"+\"), returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (char/= candidate #\+))))

;;; -------------------------------------------------------

(defun non-equal-sign-character-p (candidate)
  "Determines whether the CANDIDATE represents any character except for
   the \"equals\" sign (\"=\"), returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (char/= candidate #\=))))

;;; -------------------------------------------------------

(defun non-less-than-symbol-character-p (candidate)
  "Determines whether the CANDIDATE represents any character except for
   the \"less than\" symbol (\"<\"), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (char/= candidate #\<))))

;;; -------------------------------------------------------

(defun non-greater-than-symbol-character-p (candidate)
  "Determines whether the CANDIDATE represents any character except for
   the \"greater than\" symbol (\">\"), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (char/= candidate #\>))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace entity,
   enhalsing in its circumference the space character, the horizontal
   tab, as well as the newline, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(#\Linefeed #\Newline #\Space #\Tab)
        :test #'char=))))

;;; -------------------------------------------------------

(deftype non-ampersand-character ()
  "The ``non-ampersand-character'' type defines a character except for
   the ampersand \"&\"."
  '(satisfies non-ampersand-character-p))

;;; -------------------------------------------------------

(deftype non-plus-symbol-character ()
  "The ``non-plus-symbol-character'' type defines a character except for
   the plus symbol \"+\"."
  '(satisfies non-plus-symbol-character-p))

;;; -------------------------------------------------------

(deftype non-equal-sign-character ()
  "The ``non-equal-sign-character'' type defines a character except for
   the \"equals\" sign \"=\"."
  '(satisfies non-equal-sign-character-p))

;;; -------------------------------------------------------

(deftype non-less-than-symbol-character ()
  "The ``non-less-than-symbol-character'' type defines a character
   except for the \"less than\" symbol \"<\"."
  '(satisfies non-less-than-symbol-character-p))

;;; -------------------------------------------------------

(deftype non-greater-than-symbol-character ()
  "The ``non-greater-than-symbol-character'' type defines a character
   except for the \"greater than\" symbol \">\"."
  '(satisfies non-greater-than-symbol-character-p))

;;; -------------------------------------------------------

(deftype whitespace-character ()
  "The ``whitespace-character'' type defines a whitespace character."
  '(satisfies whitespace-character-p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of DerpScrp parsing operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces ()
  "Proceeding from the current *SOURCE-POSITION*, skips a sequence of
   zero or more accolent whitespaces, updates *SOURCE-POSITION* cursor
   to the first index immediately succeeding the matched tmema, and
   returns a ``boolean'' value of ``T''."
  (setf *source-position*
    (or (position-if-not #'whitespace-character-p *source-code*
          :start *source-position*)
        *source-length*))
  (the boolean T))

;;; -------------------------------------------------------

(defun read-non-ampersand-identifier ()
  "Proceeding from the current position into the *SOURCE-CODE*, reads
   an identifier compact of zero or more characters, except for the
   ampersand symbol \"&\", and returns a fresh string representation
   thereof."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (the simple-string
      (with-output-to-simple-string (identifier)
        (match-expression
          $[@(non-ampersand-character current-character)
            !(write-char current-character identifier)])))))

;;; -------------------------------------------------------

(defun read-non-plus-symbol-identifier ()
  "Proceeding from the current position into the *SOURCE-CODE*, reads
   an identifier compact of zero or more characters, except for the
   plus symbol \"+\", and returns a fresh string representation
   thereof."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (the simple-string
      (with-output-to-simple-string (identifier)
        (match-expression
          $[@(non-plus-symbol-character current-character)
            !(write-char current-character identifier)])))))

;;; -------------------------------------------------------

(defun read-non-equal-sign-identifier ()
  "Proceeding from the current position into the *SOURCE-CODE*, reads
   an identifier compact of zero or more characters, except for the
   equality sign \"=\", and returns a fresh string representation
   thereof."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (the simple-string
      (with-output-to-simple-string (identifier)
        (match-expression
          $[@(non-equal-sign-character current-character)
            !(write-char current-character identifier)])))))

;;; -------------------------------------------------------

(defun read-non-greater-than-symbol-identifier ()
  "Proceeding from the current position into the *SOURCE-CODE*, reads
   an identifier compact of zero or more characters, except for the
   \"greater than\" symbol \">\", and returns a fresh string
   representation thereof."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (the simple-string
      (with-output-to-simple-string (identifier)
        (match-expression
          $[@(non-greater-than-symbol-character current-character)
            !(write-char current-character identifier)])))))

;;; -------------------------------------------------------

(defun read-non-less-than-symbol-identifier ()
  "Proceeding from the current position into the *SOURCE-CODE*, reads
   an identifier compact of zero or more characters, except for the
   \"less than\" symbol \"<\", and returns a fresh string representation
   thereof."
  (let ((current-character NIL))
    (declare (type (or null character) current-character))
    (the simple-string
      (with-output-to-simple-string (identifier)
        (match-expression
          $[@(non-less-than-symbol-character current-character)
            !(write-char current-character identifier)])))))

;;; -------------------------------------------------------

(defun probe-exchange-command ()
  "Proceeding from the current position into the *SOURCE-CODE*, attempts
   to match a DerpScrp exchange behest, molded into the forbisen
   \"=variable1=variable2OrConstant=\", returning on success an
   ``Exchange-Command'' encapsulation of its pertinent data, while
   concomitantly advancing the *SOURCE-POSITION* cursor to the location
   immediately succeeding the matched tmema; otherwise responds with
   ``NIL''."
  (let ((first-parameter  NIL)
        (second-parameter NIL)
        (old-index        *source-position*))
    (declare (type (or null string) first-parameter))
    (declare (type (or null string) second-parameter))
    (declare (type fixnum           old-index))
    (the (or null Exchange-Command)
      (or
        (and
          (match-expression
            [!(skip-whitespaces)
             #\=
             !(prog1 T
                (setf first-parameter
                  (read-non-equal-sign-identifier)))
             #\=
             !(prog1 T
                (setf second-parameter
                  (read-non-equal-sign-identifier)))
             #\=])
          (make-exchange-command
            :start-position old-index
            :end-position   *source-position*
            :variable-1     first-parameter
            :expression-2   second-parameter))
        (and
          (setf *source-position* old-index)
          NIL)))))

;;; -------------------------------------------------------

(defun probe-remove-command ()
  "Proceeding from the current position into the *SOURCE-CODE*, attempts
   to match a DerpScrp removal behest, molded into the forbisen
   \">variable>constant>\", returning on success a ``Remove-Command''
   encapsulation of its pertinent data, while concomitantly advancing
   the *SOURCE-POSITION* cursor to the location immediately succeeding
   the matched tmema; otherwise responds with ``NIL''."
  (let ((first-parameter  NIL)
        (second-parameter NIL)
        (old-index        *source-position*))
    (declare (type (or null string) first-parameter))
    (declare (type (or null string) second-parameter))
    (declare (type fixnum           old-index))
    (the (or null Remove-Command)
      (or
        (and
          (match-expression
            [!(skip-whitespaces)
             #\>
             !(prog1 T
                (setf first-parameter
                  (read-non-greater-than-symbol-identifier)))
             #\>
             !(prog1 T
                (setf second-parameter
                  (read-non-greater-than-symbol-identifier)))
             #\>])
          (make-remove-command
            :start-position old-index
            :end-position   *source-position*
            :haystack       first-parameter
            :needle         second-parameter))
        (and
          (setf *source-position* old-index)
          NIL)))))

;;; -------------------------------------------------------

(defun probe-keep-command ()
  "Proceeding from the current position into the *SOURCE-CODE*, attempts
   to match a DerpScrp retention behest, molded into the forbisen
   \"<variable<constant<\", returning on success a ``Keep-Command''
   encapsulation of its pertinent data, while concomitantly advancing
   the *SOURCE-POSITION* cursor to the location immediately succeeding
   the matched tmema; otherwise responds with ``NIL''."
  (let ((first-parameter  NIL)
        (second-parameter NIL)
        (old-index        *source-position*))
    (declare (type (or null string) first-parameter))
    (declare (type (or null string) second-parameter))
    (declare (type fixnum           old-index))
    (the (or null Keep-Command)
      (or
        (and
          (match-expression
            [!(skip-whitespaces)
             #\<
             !(prog1 T
                (setf first-parameter
                  (read-non-less-than-symbol-identifier)))
             #\<
             !(prog1 T
                (setf second-parameter
                  (read-non-less-than-symbol-identifier)))
             #\<])
          (make-keep-command
            :start-position old-index
            :end-position   *source-position*
            :haystack       first-parameter
            :needle         second-parameter))
        (and
          (setf *source-position* old-index)
          NIL)))))

;;; -------------------------------------------------------

(defun probe-append-to-console-command ()
  "Proceeding from the current position into the *SOURCE-CODE*, attempts
   to match a DerpScrp appendage to the console behest, molded into the
   forbisen \"&text&\", returning on success an
   ``Append-To-Console-Command'' encapsulation of its pertinent data,
   while concomitantly advancing the *SOURCE-POSITION* cursor to the
   location immediately succeeding the matched tmema; otherwise responds
   with ``NIL''."
  (let ((variable  NIL)
        (old-index *source-position*))
    (declare (type (or null string) variable))
    (declare (type fixnum           old-index))
    (the (or null Append-To-Console-Command)
      (or
        (and
          (match-expression
            [!(skip-whitespaces)
             #\&
             !(prog1 T
                (setf variable
                  (read-non-ampersand-identifier)))
             #\&])
          (make-append-to-console-command
            :start-position old-index
            :end-position   *source-position*
            :text           variable))
        (and
          (setf *source-position* old-index)
          NIL)))))

;;; -------------------------------------------------------

(defun probe-append-to-program-command ()
  "Proceeding from the current position into the *SOURCE-CODE*, attempts
   to match a DerpScrp appendage to the program behest, molded into the
   forbisen \"+text+\", returning on success an
   ``Append-To-Program-Command'' encapsulation of its pertinent data,
   while concomitantly advancing the *SOURCE-POSITION* cursor to the
   location immediately succeeding the matched tmema; otherwise responds
   with ``NIL''."
  (let ((argument  NIL)
        (old-index *source-position*))
    (declare (type (or null string) argument))
    (declare (type fixnum           old-index))
    (the (or null Append-To-Program-Command)
      (or
        (and
          (match-expression
            [!(skip-whitespaces)
             #\+
             !(prog1 T
                (setf argument
                  (read-non-plus-symbol-identifier)))
             #\+])
          (make-append-to-program-command
            :start-position old-index
            :end-position   *source-position*
            :expression     argument))
        (and
          (setf *source-position* old-index)
          NIL)))))

;;; -------------------------------------------------------

(defun has-next-command-p ()
  "Determines whether the *SOURCE-CODE* entails at least one further
   non-whitespace character, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (skip-whitespaces)
  (the boolean *source-has-next-character-p*))

;;; -------------------------------------------------------

(defun signal-failure-to-parse-command ()
  "Signals an error of an unspecified type whose etiology conflates with
   the impotence to construe the currently processed *SOURCE-CODE*'s
   tmema as a DerpScrp command."
  (error "No command detected at position ~d in source ~s."
    *source-position* *source-code*))

;;; -------------------------------------------------------

(defun get-next-command ()
  "Returns the next DerpScrp instruction encapsulated in a ``Command''
   object; or, upon a failure to detect thilk, signals an error of an
   unspecified type."
  (the Command
    (or (probe-exchange-command)
        (probe-remove-command)
        (probe-keep-command)
        (probe-append-to-console-command)
        (probe-append-to-program-command)
        (signal-failure-to-parse-command))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of simple-string simple-string) *variables*))

;;; -------------------------------------------------------

(defparameter *variables*
  (make-hash-table :test #'equal)
  "Associates with the recognized variable names their respective
   string values.")

;;; -------------------------------------------------------

(defun query-variable (name)
  "Returns the value stored in the *VARIABLES* table under the NAME,
   or responds with a fresh empty string upon its disresponency."
  (declare (type simple-string name))
  (the simple-string
    (gethash name *variables* "")))

;;; -------------------------------------------------------

(defun set-variable (name value)
  "Stores the VALUE in the variable amenable to the NAME in the
   *VARIABLES* table, on necessity declaring thilk in a prevenient step,
   and returns no value."
  (declare (type simple-string name))
  (declare (type simple-string value))
  (setf (gethash name *variables*) value)
  (values))

;;; -------------------------------------------------------

(defun variable-with-name-exists-p (name)
  "Determines whether a variable amenable to the NAME exists in the
   *VARIABLES* table, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type simple-string name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash name *variables*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command and program string operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun append-content-to-source-code (new-content)
  "Appends the NEW-CONTENT to the *SOURCE-CODE* and returns no value."
  (declare (type simple-string new-content))
  (setf *source-code*
    (concatenate-strings *source-code* new-content))
  (values))

;;; -------------------------------------------------------

(defun update-source-code ()
  "Copies the content of the command string \"C\" into the *SOURCE-CODE*
   and returns no value."
  (psetf *source-code*     (query-variable "C")
         *source-position* 0)
  (values))

;;; -------------------------------------------------------

(defun excise-command-from-string (source command)
  "Removes the parcel occupied by the COMMAND from the SOURCE string
   and a fresh string comprehending the remaining segment.
   ---
   The SOURCE string will not be subjected to alterations."
  (declare (type simple-string source))
  (declare (type Command       command))
  (the simple-string
    (subseq source
      (command-end-position command))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of console operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 0 *) *console-height*))
(declaim (type (real    0 *) *console-update-rate*))

;;; -------------------------------------------------------

(defparameter *console-height* 32
  "The height of the console in number of lines.
   ---
   An adjustment to the personal environment's measure constitutes a
   subject of reasonable investment in order to accommodate for the
   DerpScrp programs' concinnity in operation.")

(defparameter *console-update-rate* 0.5
  "The prerogation betwixt two clearances and redrawings of the console
   in seconds.")

;;; -------------------------------------------------------

(defun print-to-console (format-control &rest format-arguments)
  "Generates an interpolated representation of the FORMAT-CONTROL
   utilizing the FORMAT-ARGUMENTS, prints the same to the standard
   output conduit, and returns no value."
  (declare (type string format-control))
  (declare (type list   format-arguments))
  (format T "~?" format-control format-arguments)
  (values))

;;; -------------------------------------------------------

(defun clear-console ()
  "Simulates the purge of the console by printing a tally of newlines to
   the standard output equal to the *CONSOLE-HEIGHT* and returns no
   value."
  (print-to-console "~v%" *console-height*)
  (values))

;;; -------------------------------------------------------

(defun redraw-console ()
  "Waits the *CONSOLE-UPDATE-RATE* in milliseconds, clears the console,
   prints its content, educed by the program variable \"A\", to the
   standard output, and returns no value."
  (sleep *console-update-rate*)
  (clear-console)
  (print-to-console "~a"
    (query-variable "A"))
  (values))

;;; -------------------------------------------------------

(defun flush-console ()
  "Purges the console's associated output stream, in the course of this
   action printing any remaining content to this conduit, and returns
   no value."
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defun query-line-input-from-console ()
  "Queries the console's associated standard input for a line of
   characters, purges the conduit, and returns a fresh string
   comprehending the response, destitute of the terminating linebreak
   entity.
   ---
   Upon the standard input conduit's disrepondency, a fresh empty string
   designates the delivery."
  (the simple-string
    (convert-into-simple-string
      (prog1
        (read-line NIL NIL "")
        (clear-input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-content (content)
  "If a variable amenable to the name CONTENT exists in the *VARIABLES*
   table, returns its associated value; otherwise responds with the
   CONTENT in its ipsissima verba form."
  (declare (type simple-string content))
  (the simple-string
    (or (and (variable-with-name-exists-p content)
             (query-variable content))
        content)))

;;; -------------------------------------------------------

(defgeneric execute-command (command)
  (:documentation
    "Executes the DerpScrp COMMAND and returns no value.")
  
  (:method ((command Exchange-Command))
    "Implements the DerpScrp operation \"=var1=var2OrConstant=\", which
     either exchanges two variables' values or assigns to a variable a
     constant's datum, the participating jumelle being communicated in
     the COMMAND, and returns no value."
    (declare (type Exchange-Command command))
    (set-variable
      (exchange-command-variable-1 command)
      (resolve-content
        (exchange-command-expression-2 command)))
    (values))
  
  (:method ((command Remove-Command))
    "Implements the DerpScrp operation \">variable>constant>\", which
     removes from a variable all occurrencies of a specific constant,
     the participating jumelle being communicated in the COMMAND, and
     returns no value."
    (declare (type Remove-Command command))
    (set-variable
      (remove-command-haystack command)
      (remove-all-occurrences-in-string
        (query-variable
          (remove-command-haystack command))
        (remove-command-needle command)))
    (values))
  
  (:method ((command Keep-Command))
    "Implements the DerpScrp operation \"<variable<constant<\", which
     retains in a variable all occurrencies of a specific constant,
     the participating jumelle being communicated in the COMMAND, and
     returns no value."
    (declare (type Keep-Command command))
    (set-variable
      (keep-command-haystack command)
      (retain-all-occurrences-in-string
        (query-variable
          (keep-command-haystack command))
        (keep-command-needle command)))
    (values))
  
  (:method ((command Append-To-Console-Command))
    "Implements the DerpScrp operation \"&text&\", which appends to the
     output console, and, as an epiphenomenon, to the variable \"A\"
     maintaining a vinculum to the efferent conduit, the string
     communicated in the COMMAND, and returns no value."
    (declare (type Append-To-Console-Command command))
    (set-variable "A"
      (concatenate-strings
        (query-variable "A")
        (resolve-content
          (append-to-console-command-text command))))
    (values))
  
  (:method ((command Append-To-Program-Command))
    "Implements the DerpScrp operation \"+variableOrConstant+\", which
     appends to the program, and, as an epiphenomenon, to the variable
     \"C\" maintaining a vinculum to the source code, the variable value
     or literal string communicated in the COMMAND, and returns no
     value."
    (declare (type Append-To-Program-Command command))
    (append-content-to-source-code
      (resolve-content
        (append-to-program-command-expression command)))
    (set-variable "C" *source-code*)
    (values)))

;;; -------------------------------------------------------

(defun initialize-text-string ()
  "Queries the standard input for a line of characters, stores thilk in
   the text variable \"B\", and returns no value."
  (print-to-console "~&Please input the text string: ")
  (flush-console)
  (set-variable "B"
    (query-line-input-from-console))
  (values))

;;; -------------------------------------------------------

(defun initialize-standard-variables ()
  "Initializes the DerpScrp standard variables \"A\", \"B\" and \"C\" to
   reasonable default states and returns no value.
   ---
   Please heed that the command string variable \"C\" draws its content
   from the global *SOURCE-CODE* variable; as a corollary, the DerpScrp
   program intended for execution ought to be transmitted to
   *SOURCE-CODE* ere this operation's invocation."
  (set-variable "A" "")
  (set-variable "B" "")
  (set-variable "C" *source-code*)
  (initialize-text-string)
  (values))

;;; -------------------------------------------------------

(defun prepare-program-for-command-execution (command)
  "Removes the tmema occupied by the extracted COMMAND from the
   *SOURCE-CODE*'s front, updates the connected program variable \"C\",
   and returns no value."
  (declare (type Command command))
  (set-variable "C"
    (excise-command-from-string
      (query-variable "C")
      command))
  (update-source-code)
  (values))

;;; -------------------------------------------------------

(defun interpret-DerpScrp (program)
  "Interprets the DerpScrp PROGRAM and returns no value."
  (declare (type string program))
  (set-source-code
    (convert-into-simple-string program))
  (initialize-standard-variables)
  (loop while (has-next-command-p) do
    (let ((command (get-next-command)))
      (declare (type Command command))
      (prepare-program-for-command-execution command)
      (execute-command                       command)
      (redraw-console)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Endeictic example which assays all language facilities.
(interpret-DerpScrp
  "&Hello&
   =A=B=
   =A=123=
   >A>12>
   <A<12<
   +A+")

;;; -------------------------------------------------------

;; Query the user for their name and print a message leal to the
;; forbisen
;; 
;;   Hello, {B}!
;; 
;; where {B} represent the text variable "B", initialized with the
;; response, to the console.
(interpret-DerpScrp "&Hello, & &B& &!&")

;;; -------------------------------------------------------

;; Append the text output behest "&B&", which references the user's
;; text string, stored in the variable "B", and the literal "!" to the
;; program's rear, and execute the resulting code,
;; 
;;   &Hello, & &B& &!&
;; 
;; thilk produces the message
;; 
;;   Hello, {B}!
;; 
;; If provided with a name for the text string variable "B" at the
;; program's inchoating via the standard input conduit, a greeting is
;; simulated, presenting the forbisen
;; 
;;   Hello, {userInput}!
(interpret-DerpScrp
  "+&B&+
   +&!&+
   &Hello, &")

;;; -------------------------------------------------------

;; Curtail the console output "sparrow" to "arrow" by expunging all
;; occurrencies of the substring "sp".
(interpret-DerpScrp "&sparrow& >A>sp>")

;;; -------------------------------------------------------

;; Distill the adulterated message "&HxExLxLxO&>A>x>" to the reasonable
;; text "HELLO" by expungement of all "x" occurrences.
(interpret-DerpScrp "&HxExLxLxO&>A>x>")
