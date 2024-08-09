;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deadfish TM", invented by the Esolang user "BoundedBeans"
;; and presented on August 6th, 2022, is foundry ensues from the
;; nuptials betwixt Jonathan Todd Skinner's "Deadfish" and the abstract
;; Turing machine concepts, desuming from the latter's diorism the
;; state machine --- here in its entelechy relating to integer-valued
;; states --- and a bilaterally infinite tape of characters expected to
;; commit to certain stipulations, while the actual operative haecceity
;; constitutes an appropriation from Deadfish's instruction set,
;; amplified in its competences' circumference by additional faculties.
;; 
;; 
;; Concept
;; =======
;; The Deadfish TM progamming language constitutes a variation on and
;; enhancement of Deadfish, the adscititious potentials enjoyed as a
;; dation from the abstract Turing machine principles, operating
;; simultaneously on a state machine of integer-valued states and a
;; bilaterally infinite tape of symbols by adminiculum of several
;; conditional and one default rule.
;; 
;; == THE ACCUMULATOR'S EVOLUTION TO THE STATE ==
;; The Deadfish TM Turing machine's state machine registers as its
;; ancestry the Deadfish accumulator, a signed integer object of no
;; natural bournes. For the machine to perpetuate its processing onus,
;; the state, however, must occupy the integral range [0, 255]; a
;; transcendence beyond these limits inflicts the program with an
;; immediate cessation.
;; 
;; == THE TAPE A BILATERALLY INFINITE DISPANSION OF CHARACTERS ==
;; The second constituent respnosible for the Turing machine's patration
;; appertains to the tape, a bilaterally infinite expansion of cells,
;; everichon's capacity meted by an aefauld character, desumed from a
;; particular set.
;; 
;; == THE TAPE ADMITS CERTAIN CHARACTERS ONLY ==
;; A symbol's concinnity with the tape, and its consectaneous admission
;; to the salvatory, describes a four stipulations' succedent:
;; 
;;   (a) PRINTABLE:
;;       The character ought to subsume into the printable vale.
;;   
;;   (b) NO CONTROL CHARACTERS:
;;       The character is prohibited from an affiliation with the set
;;       of control characters.
;;   
;;   (c) UNICODE RANGE OF HEXADECIMAL 0000--FFFF
;;       The character's code point must be a commorant of the closed
;;       Unicode range from inclusive hexadecimal 0000 to inclusive
;;       hexadecimal FFFF.
;;   
;;   (d) NO WHITESPACE OR HASH SIGN ("#")
;;       The character is interdicted to constitute a whitespace, which
;;       rine the diorism of spaces, horizontal tabs, newlines, and
;;       cognate entities, or the hash sign "#", the same is allotted
;;       an operational bailiwick as a Deadfish command.
;; 
;; == THE TAPE HEAD: THE CURRENT CELL MARKER ==
;; The currently active cell's tiver is accoutred by the tape head, a
;; motile species of cursor, empight at the program's inchoation at the
;; first cell, and capacitated in its mobility to translate in a
;; stillatitious mode of locomotion either sinistrally or dextrally as
;; a successful rule execution's consequence.
;; 
;; The active cell constitutes the exclusive cynosure of the machine
;; during the reading and writing stages, both actuated by the rules'
;; agons.
;; 
;; == A RULE'S CONFORMATION: LINE OF ANTECEDENT, LINE OF CONSEQUENT ==
;; With the default case's involvement as the aefauld exemption, a
;; Deadfish TM rule's conformation spans exactly two lines: the "case",
;; an antecedent which imposes the combination of integer-valued states
;; and character-valued symbols that in coefficiency act as the clause's
;; activation condition, and the "transition", a consequent to execute
;; only in the circumstance of the rule case's eligibility.
;; 
;; In an abstract apercu, the following two parcels define a rule,
;; encompassing two lines in immediate succession:
;; 
;;   case
;;   transition
;; 
;; The case pattern's structure exhibits a twissel of components,
;; scilicet, the state matching predicate and the activating symbols,
;; the former appertains to the Turing machine's state machine, while
;; the latter applies itself to the juxtaposition with the machine's
;; character tape.
;; 
;; Siclike, the transition compartment is distributed across a
;; quadruple componency, which states the Deadfish instructions to
;; execute upon the rule's instigation, the new symbol to place in the
;; machine's current tape cell, the airt into the same to translate the
;; tape head by one step, and a flag accommodated for the
;; halting/continuation of the program and the tape's contingent
;; output.
;; 
;; An amplification in the expressive puissance will be lend to the
;; following elucidation of the case and transition structures:
;; 
;;   ------------------------------------------------------------------
;;   Part       | Conformation pattern
;;   -----------+------------------------------------------------------
;;   Case       | statePredicate symbolPredicate
;;   ..................................................................
;;   Transition | deadfishCode symbolToWrite headDirection haltOption
;;   ------------------------------------------------------------------
;; 
;; The default case, establishing the mandatory minimum requisite for
;; a Deadfish TM program's existency, entirely eschews the case line,
;; resolving in its specification's entirety to the transition row,
;; forecause its instigation is exercised automatically if none of the
;; extant conditional rules match in their case the Turing machine's
;; probed configuration.
;; 
;; == THE RULE CASE: A CONDITION FOR A RULE'S SELECTION ==
;; That compartment of a conditional rule attending to its selection
;; predicate is stevened the case in Deadfish TM, its componency
;; enumerating by the significant parcels of a Turing machine's
;; perquisition, scilicet, the activating states and the activating
;; symbols.
;; 
;; In the circumstance of the program's state machine state being
;; amplected by a rule's impositions, and the tape's current symbol's
;; conformance with the expected rule characters, an activation, and
;; subsequent execution, will materialize.
;; 
;; A precis of this twifold division shall be delivered to one's
;; attestation:
;; 
;;   ------------------------------------------------------------------
;;   No. | Part               | Role
;;   ----+--------------------+----------------------------------------
;;    1  | Activating states  | Defines the set of states which, if
;;       |                    | intersecting with the machine's current
;;       |                    | state, may activate the rule.
;;   ..................................................................
;;    2  | Activating symbols | Defines the set of characters which,
;;       |                    | if intersecting with the tape's current
;;       |                    | symbol, may activate the rule.
;;   ------------------------------------------------------------------
;; 
;; == THE CASE STATE: A PREDICATE FOR THE STATE MACHINE ==
;; The case's first element is realized in terms of the mandatory state
;; or states for a rule's involvement. A triad of options for such a
;; specification exist:
;; 
;;   ------------------------------------------------------------------
;;   Predicate | Pattern                   | Satisfied if
;;   ----------+---------------------------+---------------------------
;;   Single    | state                     | The machine state equals
;;             | *****                     | the specified {state}.
;;   ..................................................................
;;   List      | state1,state2,...,stateN  | The machine state equals
;;             | ****** ******     ******  | any of the states {state1}
;;             |                           | through {stateN}.
;;   ..................................................................
;;   Range     | minimumState-maximumState | The machine state is
;;             | ************ ************ | greater than or equal to
;;             |                           | the {minimumState} and
;;             |                           | less than or equal to the
;;             |                           | {maximumState}.
;;   ------------------------------------------------------------------
;; 
;; Invested with greater magnanimity in verbs, the antecedent state
;; stipulation intrines the contingencies for a single guard, an
;; unordered list thereof, or a closed range:
;; 
;;   - SINGLE STATE:
;;     A single state's imposition is realized by the aefauld choice
;;     itself, as ostended alow:
;;     
;;       state
;;       *****
;;     
;;     With the {state} constituting an integer-valued state diorism.
;;   
;;   - STATE LIST:
;;     An antecedent which shall provide an unordered list of states as
;;     alternatives, whence a single concurrency with the Turing
;;     machine's current state suffices for this case moeity's
;;     eligibility, ensues from the following pattern:
;;     
;;       state1,state2,...,stateN
;;       ****** ****** *** ******
;;     
;;     Where {state1}, {state2}, through {stateN} define the possible
;;     choices. The integer-valued states must be segregated via a
;;     single comma each, with no further medioxumous content, not even
;;     spaces. A forbisen shall be administered in
;;     
;;       4,91,2
;;     
;;     which satisfies the antecedent if any of the states 4, 91, or 2
;;     comport with the machine's contemporaneous state.
;;   
;;   - STATE RANGE:
;;     In a closed interval of states shall be engaged in the
;;     equiparation, a state range proffers its serves, obeying this
;;     formula:
;;     
;;       minimumState-maximumState
;;       ************ ************
;;     
;;     Both {minimumState} and {maximumState} ought to be integer-valued
;;     state specifications, where the {minimumState} is strictly less
;;     than {maximumState}, and both items are applied the construe as
;;     inclusive bournes. The twissel's separation proceeds by
;;     adminiculum of a single hyphen ("-"), or minus sign, with no
;;     further content's inclusion, which also bars spaces. As an
;;     example, the range
;;     
;;       8-104
;;     
;;     comports with any state starting from inclusive 8 to inclusive
;;     104.
;; 
;; == THE CASE SYMBOLS: A PREDICATE FOR THE TAPE ==
;; A concomitant to the state equiparation, a case's symbol list
;; produces a complementing antecedent for the Turing machine tape.
;; Specified as a sequence of one or more characters, desumed in
;; compliance with the admissible tape content, this moeity of the rule
;; condition matches if the current tape cell symbol concurs with any
;; of the imposed characters.
;; 
;; An aefauld donet applies to the rule's symbolic expectations,
;; specifying the required characters in immediate succession, destitute
;; of any sepiments, and tallying at least one member.
;; 
;; For instance, the activating symbols
;; 
;;   aN8Z
;; 
;; will be satisfied if the current tape cell either entails the entity
;; "a", "N", "8", or "Z".
;; 
;; == THE DEFAULT CASE: A FALLBACK FOR FAILURE ==
;; Empight on the incipient position of a Deadfish TM program, the
;; default case appropriates merely the transition diorism, conspicable
;; in the case line's absence, as this rule's activation segues into
;; entelechy in the circumstance of any conditional rules' failure to
;; answer to the current Turing machine configuration.
;; 
;; == THE RULE TRANSITION: A CONSEQUENCE OF A RULE'S SELECTION ==
;; An ultimity begotten by either a conditional rule's activation, or
;; upon any such candidates' submission to failure, the default rule's
;; involvement, the consequent moeity partakes of efficacy: the
;; transition; this being an aggregate of the Deadfish instructions to
;; execute, the new symbol to write into the current tape cell, the
;; direction into to translate the tape head, and, at the desinence, a
;; decision about halting/continuing the program and printing the
;; tape's content.
;; 
;; The quadruple compartments providing a rule transition's edification
;; shall be limned in a cursory fashion:
;; 
;;   ------------------------------------------------------------------
;;   No. | Part               | Role
;;   ----+--------------------+----------------------------------------
;;    1  | Deadfish code      | A sequence of one or more augmented
;;       |                    | Deadfish instructions to perform.
;;   ..................................................................
;;    2  | New symbol         | Communicates the new symbol to write
;;       |                    | into the cell under the tape's head.
;;   ..................................................................
;;    3  | Head direction     | Specifies the airt into which to move
;;       |                    | the tape head by one step: either left
;;       |                    | or right.
;;   ..................................................................
;;    4  | Halt and/or output | Determines whether the program shall
;;       |                    | halt and whether the tape shall be
;;       |                    | printed.
;;   ------------------------------------------------------------------
;; 
;; == THE RULE'S EXECUTION COMMENCES WITH DEADFISH INSTRUCTIONS ==
;; The premier succedent to a rule's actuation is realized in the
;; Deadfish code accommodated for immediate execution, itself a sequence
;; of one or more augmented Deadfish instructions.
;; 
;; A very cursory exposition of the instructions shall be ostended in
;; the following; please refer to a discussion of superior lucidity
;; in the "Instructions" section.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increment state.
;;   ..................................................................
;;   d       | Decrement state.
;;   ..................................................................
;;   s       | Squares state.
;;   ..................................................................
;;   o       | Print state as number.
;;   ..................................................................
;;   a       | Print state as Unicode character.
;;   ..................................................................
;;   c       | Input character into current tape cell.
;;   ..................................................................
;;   #       | No-operation (no-op).
;;   ------------------------------------------------------------------
;; 
;; As counterdistinguished from the original Deadfish's wont, an invalid
;; token does not instil a single linebreak's replication on the
;; standard output, and instead instigates an error.
;; 
;; == THE CELL CONTENT'S SUPERSESSION BY A NEW SYMBOL ==
;; In compliance with the abstract Turing machine's notions, a rule's
;; adhibition constitutes the vaunt-courier to a recently probed cell's
;; modification, in the course of this process superseding the extant
;; character by a new symbol of covenable designation.
;; 
;; The succedaneum's perimeter concurs perfectly with the stipulations
;; levied against any symbol during its admission to the tape.
;; 
;; == TAPE HEAD MOVEMENT ==
;; Proceeding from the symbol substitution stage, the transition's
;; ensuing action manifests in the relocation of the tape head, just
;; employed for the pertinent cell's modulation, now either translated
;; by one step in a sinistral or dextral airt, as depending on the
;; communicated identification:
;; 
;;   ------------------------------------------------------------------
;;   Movement identifier | Effect
;;   --------------------+---------------------------------------------
;;   L                   | Moves the tape head one cell to the left.
;;   ..................................................................
;;   R                   | Moves the tape head one cell to the right.
;;   ------------------------------------------------------------------
;; 
;; Given the tape's vastness, wisting of no bournes' coercion along its
;; two lateralities, this exercise of mobility always succeeds.
;; 
;; == THE HALT/OUTPUT: HALT PROGRAM OR NOT, PRINT TAPE OR NOT ==
;; The halt/output flag may assume one of the values from the integral
;; set {0, 1, 2, 3}, the discrepancies withal appertaining to whether
;; the program shall be halted during the rule's execution and whether
;; the tape in its entirety --- or at least a pertinent excerpt
;; thereof --- as an epiphenomenon shall be printed to the standard
;; output.
;; 
;; An apercu's reification shall be delivered to one's conspectuity in
;; the alow tabulation:
;; 
;;   ------------------------------------------------------------------
;;   Halt code | Effect
;;   ----------+-------------------------------------------------------
;;   0         | Continues the program without printing the tape.
;;   ..................................................................
;;   1         | Halts the program, but does not print the tape.
;;   ..................................................................
;;   2         | Halts the program and prints the tape to the standard
;;             | output.
;;   ..................................................................
;;   3         | Continues the program and prints the tape to the
;;             | standard output.
;;   ------------------------------------------------------------------
;; 
;; Amplified stringency in the pattern of halting and output policies
;; shall be limned by the following variation on the aboon table:
;; 
;;   -----------------------------------------
;;   Halt code | Halts program? | Prints tape?
;;   ----------+----------------+-------------
;;   0         | No             | No
;;   .........................................
;;   1         | Yes            | No
;;   .........................................
;;   2         | Yes            | Yes
;;   .........................................
;;   3         | No             | Yes
;;   -----------------------------------------
;; 
;; == PROGRAMS: INITIALIZATION, RULE SELECTION, REPETITION ==
;; A Deadfish TM program's inchoation is realized by the transferral of
;; user input characters, succeeded by a repeated selection of a conable
;; or the default rule, based on the Turing machine state and tape
;; symbol, until the program is explicitly halted or the state departs
;; from the valid range of [0, 255].
;; 
;; With augmented precision, a program's incarnation adheres to the
;; following principles:
;; 
;;   (1) TAPE INITIALIZATION:
;;       The program queries the standard input for a string and
;;       inserts all eligible content into the tape. To do so, the
;;       the tape head is empight on the incipial cell; the input's
;;       characters are iterated, ignoring invalid symbols, while a
;;       character administered eligibility is written to the cell under
;;       the tape's head, concomitantly advancing the same one step to
;;       the right. Finally, the tape head returns to the initial
;;       position.
;;   
;;   (2) RULE SELECTION:
;;       Iterate through all conditional rules in their specified order;
;;       if the rule's case matches, which means that its activating
;;       state predicate satisfies the Turing machine's current state
;;       and its symbol predicate satisfies the machine tape's current
;;       symbol, executes the rules transition.
;;       
;;       (2.a) DEADFISH CODE EXECUTION:
;;             The Deadfish instructions are executed. If new machine
;;             state transcends the valid bournes of [0, 255], the
;;             program is halted; otherwise the remaining transition
;;             actions are performed.
;;       
;;       (2.b) NEW SYMBOL:
;;             The transition symbol is written to the tape's current
;;             cell.
;;       
;;       (2.c) HEAD MOTION:
;;             The tape head is translated one cell into the airt
;;             imposed by the transition.
;;       
;;       (2.d) HALT/OUTPUT ACTION:
;;             Depending on the halt/output flag, halts or continues
;;             the program and/or prints the tape's content.
;;   
;;   (3) DEFAULT RULE APPLICATION:
;;       If no rule conforms to the machine configurations, the default
;;       rule's transition is executed.
;;       
;;       (3.a) DEADFISH CODE EXECUTION:
;;             The Deadfish instructions are executed. If new machine
;;             state transcends the valid bournes of [0, 255], the
;;             program is halted; otherwise the remaining transition
;;             actions are performed.
;;       
;;       (3.b) NEW SYMBOL:
;;             The transition symbol is written to the tape's current
;;             cell.
;;       
;;       (3.c) HEAD MOTION:
;;             The tape head is translated one cell into the airt
;;             imposed by the transition.
;;       
;;       (3.d) HALT/OUTPUT ACTION:
;;             Depending on the halt/output flag, halts or continues
;;             the program and/or prints the tape's content.
;;   
;;   (4) If the program has not been halted, repeats the process with
;;       the stage -> (2).
;; 
;; A more stringent delineation shall be limned by this pseudocode
;; formulation:
;; 
;;   Given:
;;     tape:          The tape, initially beset with "!" symbols.
;;     tape[head]:    The tape's currently active cell, selected via
;;                    the "head" pointer.
;;     head:          The tape's mobile head pointer, utilized for
;;                    selecting the currently active cell.
;;     state:         The current state, initially set to 0.
;;     programHalted: A flag which determines whether the program
;;                    should terminate.
;;   
;;   function executeDeadfishCode (code)
;;     for token in code do
;;       if token = "i" then
;;         increment tape[head] by 1
;;       else if token = "d" then
;;         decrement tape[head] by 1
;;       else if token = "s" then
;;         set tape[head] <- tape[head] * tape[head]
;;       else if token = "o" then
;;         print tape[head] in its numeric form
;;       else if token = "a" then
;;         print Unicode character corresponding to tape[head]
;;       else if token = "c" then
;;         let characterInput <- query Unicode character
;;         set tape[head] <- Unicode code point of characterInput
;;       else if token = "#" then
;;         no effect
;;       else
;;         error: Invalid token
;;       end if
;;     end
;;   end function
;;   
;;   { Initialize the tape with user input. }
;;   let lineInput <- query for a line of input
;;   
;;   for inputSymbol in lineInput do
;;     if inputSymbol is valid tape symbol then
;;       tape[head] <- inputSymbol
;;       move head right
;;     end if
;;     reset tape head to initial position
;;   end for
;;   
;;   repeat until programHalted do
;;     for rule in conditionalRules do
;;       let ruleCase          <- case(rule)
;;       let activatingStates  <- activatingStates(case)
;;       let activatingSymbols <- activatingSymbols(case)
;;       
;;       if (state in activatingStates) and
;;          (tape[head] in activatingSymbols) then
;;         let transition   <- transition(rule)
;;         let deadfishCode <- deadfishCode(transition)
;;         let newSymbol    <- newSymbol(transition)
;;         let headMotion   <- headMotion(transition)
;;         let haltFlag     <- haltFlag(transition)
;;         
;;         executeDeadfishCode (deadfishCode)
;;         
;;         write newSymbol to tape[head]
;;         
;;         if headMotion = "L" then
;;           move head left
;;         else if headMotion = "R" then
;;           move head right
;;         else
;;           error: Invalid headMotion
;;         end if
;;         
;;         if haltFlag = 0 then
;;           simply continue without printing the tape
;;         else if haltFlag = 1 then
;;           programHalted <- true
;;         else if haltFlag = 2 then
;;           programHalted <- true
;;           print tape
;;         else if haltFlag = 3 then
;;           print tape
;;         else
;;           error: Invalid haltFlag
;;         end if
;;       end if
;;     end for
;;     
;;     if (state < 0) or (state > 255) then
;;      programHalted <- true
;;     end if
;;   end repeat
;; 
;; 
;; Syntax
;; ======
;; A Deadfish TM program's conformation is defined in terms of one or
;; more lines, the incipient bearing the default rule's transition,
;; those in subsequence established a jumelles of rule case and
;; transition.
;; 
;; == THE CASE: A TWISSEL OF INTEGRAL STATES AND ARBITRARY SYMBOLS ==
;; A case's compass amplects either a single integral object, a
;; comma-separated list thereof, or a closed interval whose extrema are
;; ligated via hyphen "-". Ensuing thereof, by one or more spaces'
;; mediation, the a sequence of one or more characters unfolds.
;; 
;; == THE TRANSITION: INSTRUCTIONS, SYMBOL, DIRECTION, AND FLAG ==
;; The transition line measures four components, each twissel's
;; distinguishment iterum accommodated by at least one space. The first
;; constituent lists the mandatory Deadfish instructions, prevenient to
;; the singular tape symbol specification, seguing into a head direction
;; identifier, and concluding in the numeric halt/output flag.
;; 
;; == COMMENTS ==
;; Any operative line, including both rule case and transition
;; specimens, may adduce, following the mandatory content, a space
;; succeeded by arbitrary content, whose neglected status acts in a
;; commentary agency.
;; 
;; == GRAMMAR ==
;; The donat's conformation shall be administered a superior grade of
;; formality by the following Extended Backus-Naur Form (EBNF)
;; description:
;; 
;;   program             := transition
;;                       ,  [ newline , { innerRule } , [ lastRule ] ]
;;                       ;
;;   innerRule           := rule , newline ;
;;   lastRule            := rule , [ newline ] ;
;;   rule                := case , newline , transition ;
;;   
;;   transition          := deadfishCode
;;                       ,  space
;;                       ,  symbol
;;                       ,  space
;;                       ,  direction
;;                       ,  space
;;                       ,  haltOption
;;                       ,  optionalComment
;;                       ;
;;   deadfishCode        := deadfishInstruction
;;                       ,  { deadfishInstruction }
;;                       ;
;;   deadfishInstruction := "i" | "d" | "s" | "o" | "a" | "c" | "#" ;
;;   direction           := "L" | "R" ;
;;   haltOption          := "0" | "1" | "2" | "3" ;
;;   
;;   case                := caseState
;;                       ,  space
;;                       ,  caseSymbols
;;                       ,  optionalComment
;;                       ;
;;   caseSymbols         := symbol , { symbol } ;
;;   optionalComment     := space , { ( character - newline ) } ;
;;   symbol              := printableSymbol - ( whitespace | "#" ) ;
;;   printableSymbol     := printableCharacter
;;                       |  UnicodeCodePointRange_0000_through_FFFF
;;                       ;
;;   
;;   caseState           := singletonState | stateList | stateRange ;
;;   singletonState      := state ;
;;   stateList           := state , "," , state , { "," , state } ;
;;   stateRange          := state , "-" , state ;
;;   state               := digit , [ digit , [ digit ] ] ;
;;   
;;   digit               := "0" | "1" | "2" | "3" | "4"
;;                       |  "5" | "6" | "7" | "8" | "9"
;;                       ;
;;   whitespace          := space | horizontalTab | newline ;
;;   horizontalTab       := "\t" ;
;;   space               := " " ;
;;   newline             := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Deadfish TM's actual instruction component, incorporated into the
;; rules' transition part, and amplecting a cleronomy from Deadfish's
;; original competences, in conjunction with an augmentation thereof,
;; enumerates a septuple membership.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer a cursory mete of gnarity
;; concerning the operative elements:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the machine state by one (1).
;;   ..................................................................
;;   d       | Decrements the machine state by one (1).
;;   ..................................................................
;;   s       | Squares the machine state.
;;   ..................................................................
;;   o       | Prints the machine state in its verbatim numeric form
;;           | to the standard output.
;;   ..................................................................
;;   a       | Prints the character whose Unicode code point
;;           | corresponds to the machine state to the standard output.
;;   ..................................................................
;;   c       | Queries a character from the standard input and stores
;;           | its Unicode code point in the current tape cell.
;;   ..................................................................
;;   #       | No-operation; exercises no causatum.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the protolog's extensive elucidations, a few inroads of
;; ambiguity retain their existency, whence a subset shall be the coming
;; disquisition's cynosure.
;; 
;; == DOES A PROGRAM HALTING ACCOMPASS IMMEDIATE EFFICACY? ==
;; The contingency for a program's termination arrives in a twifaced
;; avenue, the explicit case's manifestation in the transition's halt
;; flags; while the tacit entelechy sustains its provenance by
;; adminiculum of the machine state, the same, upon transcending the
;; valid bournes of [0, 255] through a Deadfish instruction's exercise
;; vanquishes the perpetuation. As this code segment acts as the
;; prevenient action of a transition, the question intrudes whether the
;; transgression shall be an immediate causatum, aborting any further
;; efforts.
;; 
;; It has been adjudged, in the face of complications' absence, that
;; the rule's remaining transition actions, that is, the current tape
;; symbol overwriting, the head translation, and the halt flag's output
;; contingency --- but not its continuation option ---, shall be
;; executed, ere the rule concludes the program.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation experiences its reification in the
;; programming language Common Lisp, employing an emarginate species of
;; lexer and parser, with a distinct interpretation entity.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-08-07
;; 
;; Sources:
;;   [esolang2022DeadfishTM]
;;   The Esolang contributors, "Deadfish TM", August 6th, 2022
;;   URL: "https://esolangs.org/wiki/Deadfish_TM"
;;   
;;   [mullins2012turingmachinexampleprogs]
;;   Robert Mullins, "Section 4: Turing Machine Example Programs", 2012
;;   URL: "https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/
;;         turing-machine/four.html"
;;   Notes:
;;     - Presents several Turing machine example programs, including a
;;       binary counter and a 3-state busy beaver.
;;   
;;   [mullins2012whatisturingmachine]
;;   Robert Mullins, "What is a Turing machine?", 2012
;;   URL: "https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/
;;         turing-machine/one.html"
;;   Notes:
;;     - Description of the Turing machine model, with a special focus
;;       on its binary tape design.
;;   
;;   [tuteja2023turingmachine]
;;   Sonal Tuteja, "Turing Machine in TOC", 22 Feb, 2023
;;   URL: "https://www.geeksforgeeks.org/turing-machine-in-toc/"
;;   Notes:
;;     - Description of the Turing machine model, encompassing a
;;       formal diorism.
;;   
;;   [tutorialspoint2023turingmachineintro]
;;   The Tutorials Point contributors, "Turing Machine Introduction",
;;                                     2023 
;;   URL: "https://www.tutorialspoint.com/automata_theory/
;;         turing_machine_introduction.htm"
;;   Notes:
;;     - Description of the Turing machine model, encompassing a
;;       formal diorism.
;;     - Presents a simple example.
;;   
;;   [wikipedia2023turingmachine]
;;   The Wikipedia contributors, "Turing machine", 2023
;;   URL: "https://en.wikipedia.org/wiki/Turing_machine"
;;   Notes:
;;     - Throughout description of the Turing machine model,
;;       encompassing a formal diorism.
;;     - Demonstrates the 3-state busy beaver problem as an example.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type by adminiculum of the ``deftype''
   infrastructure in conjunction with the ``satisfies'' type specifier,
   deriving its agnomination from the TYPE-NAME and deploying the
   LAMBDA-LIST in its verbatim form as formal parameters, the subject
   of the docimasy acquiring its identity from the CANDIDATE-NAME,
   evaluates the BODY forms with access to the CANDIDATE-NAME, and
   expects for a communication of the subject's eligibility a
   generalized boolean \"true\" value in the desinent form's primary
   result, otherwise a \"false\" sentinel for its incompatibility with
   the thus declared specifier.
   ---
   The first body form, if resolving to a string object, is construed
   as the derived type's documentation string and reappropriated for
   this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table the componency of
   which enumerates a set of zero or more entries, its keys obeying the
   KEY-TYPE, answering to the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for    key of-type T being the hash-keys in candidate
      using  (hash-value value)
      always (and (typep key key-type) (typep value value-type)))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which complies to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      candidate)))

;;; -------------------------------------------------------

(deftype deadfish-instruction ()
  "The ``deadfish-instruction'' type enumerates the recognized variation
   on Deadfish operations available to the \"Deadfish TM\" programming
   language."
  '(member
    :increment
    :decrement
    :square
    :output-number
    :output-character
    :input-character
    :no-op))

;;; -------------------------------------------------------

(deftype deadfish-code ()
  "The ``deadfish-code'' type defines the Deadfish instructions segment
   of a \"Deadfish TM\" transition as an ordered list of zero or more
   ``deadfish-instruction'' objects."
  '(list-of deadfish-instruction))

;;; -------------------------------------------------------

(deftype head-direction ()
  "The ``head-direction'' type enumerates the recognized airts along
   which the tape head may be translated."
  '(member :left :right))

;;; -------------------------------------------------------

(deftype halt-action ()
  "The ``halt-action'' type enumerates the recognized actions
   appertaining to the halt and output stage during a rule's execution."
  '(member
    :halt-without-output
    :continue-without-output
    :halt-with-output
    :continue-with-output))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   specifies of which nevens, among others, the functions ``format'' and
   ``write-char'' as its compernage."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\",
   returning a ``boolean'' value of ``T'' for a non-``NIL'' input,
   otherwise responding with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid symbol in a
   \"Deadfish TM\" program, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and
        (graphic-char-p candidate)
        (char/= candidate #\Space)
        (char/= candidate #\#)
        (<= #x0000 (char-code candidate) #xFFFF)))))

;;; -------------------------------------------------------

(defun deadfish-identifier-p (candidate)
  "Determines whether the CANDIDATE represents a Deadfish instruction
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "idsoac#" :test #'char=))))

;;; -------------------------------------------------------

(defun parse-deadfish-instruction (identifier)
  "Parses the Deadfish instruction IDENTIFIER and returns a
   ``deadfish-instruction'' representation thereof."
  (declare (type character identifier))
  (the deadfish-instruction
    (case identifier
      (#\i :increment)
      (#\d :decrement)
      (#\s :square)
      (#\o :output-number)
      (#\a :output-character)
      (#\c :input-character)
      (#\# :no-op)
      (otherwise
        (error "Invalid Deadfish instruction identifier: ~s."
          identifier)))))

;;; -------------------------------------------------------

(defun head-direction-identifier-p (candidate)
  "Determines whether the CANDIDATE designates a recognized tape head
   motion direction identifier, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\L)
          (char= candidate #\R)))))

;;; -------------------------------------------------------

(defun parse-head-direction (identifier)
  "Parses the tape head direction IDENTIFIER and returns the
   corresponding ``head-direction'' representation thereof."
  (declare (type character identifier))
  (the head-direction
    (case identifier
      (#\L :left)
      (#\R :right)
      (otherwise
        (error "Invalid tape head direction identifier: \"~c\"."
          identifier)))))

;;; -------------------------------------------------------

(defun halt-action-identifier-p (candidate)
  "Determines whether the CANDIDATE designates a recognized halt action
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "0123" :test #'char=))))

;;; -------------------------------------------------------

(defun parse-halt-action (identifier)
  "Parses the halt action IDENTIFIER and returns the corresponding
   ``halt-action'' representation thereof."
  (declare (type character identifier))
  (the halt-action
    (case identifier
      (#\0 :continue-without-output)
      (#\1 :halt-without-output)
      (#\2 :halt-with-output)
      (#\3 :continue-with-output)
      (otherwise
        (error "Invalid halt action identifier: \"~c\"."
          identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of state range operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-state-range-bournes (minimum maximum)
  "Determines whether the MINIMUM state range is strictly less than the
   MAXIMUM, returning on confirmation no value; otherwise signals an
   error of an unspecified type."
  (declare (type integer minimum))
  (declare (type integer maximum))
  (when (>= minimum maximum)
    (error "Invalid state range: ~d >= ~d. The minimum state must ~
            be strictly less than the maximum."
      minimum maximum))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (number)
  "Squares the NUMBER by multiplication of the same by itself and
   returns the product."
  (declare (type integer number))
  (the (integer 0 *)
    (* number number)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of state predicates.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (State-Predicate)
  "The ``State-Predicate'' interface introduces a diorism for such
   classes which pursue the representation of state sets for contingent
   juxtapositions.")

;;; -------------------------------------------------------

(defstruct (Singleton-State-Predicate
  (:include State-Predicate))
  (state (error "Missing state.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (State-List-Predicate
  (:include State-Predicate))
  "The ``State-List-Predicate'' class represents an aggregate of states
   ligated into a union by mediation of an ordered list comprehending
   a membership tallying zero or more participants."
  (states (error "Missing states.")
          :type      list
          :read-only T))

;;; -------------------------------------------------------

(defstruct (State-Range-Predicate
  (:include State-Predicate))
  "The ``State-Range-Predicate'' class establishes a conglomeration of
   states founded upon a closed interval proceeding from an inclusive
   lower bourne towards an inclusive upper march."
  (minimum-state (error "Missing minimum state.")
                 :type      integer
                 :read-only T)
  (maximum-state (error "Missing maximum state.")
                 :type      integer
                 :read-only T))

;;; -------------------------------------------------------

(defgeneric contains-state-p (predicate candidate)
  (:documentation
    "Determines whether the state PREDICATE admits the CANDIDATE state,
     returning on confirmation a ``boolean'' value of ``T'', otherwise
     ``NIL''.")
  
  (:method ((predicate Singleton-State-Predicate) (candidate integer))
    (declare (type Singleton-State-Predicate predicate))
    (declare (type integer                   candidate))
    (the boolean
      (get-boolean-value-of
        (= candidate
           (singleton-state-predicate-state predicate)))))
  
  (:method ((predicate State-List-Predicate) (candidate integer))
    (declare (type State-List-Predicate predicate))
    (declare (type integer              candidate))
    (the boolean
      (get-boolean-value-of
        (member candidate
          (state-list-predicate-states predicate)
          :test #'=))))
  
  (:method ((predicate State-Range-Predicate) (candidate integer))
    (declare (type State-Range-Predicate predicate))
    (declare (type integer               candidate))
    (the boolean
      (get-boolean-value-of
        (<= (state-range-predicate-minimum-state predicate)
            candidate
            (state-range-predicate-maximum-state predicate))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule cases.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Rule-Case
  "The ``Rule-Case'' interface furnishes a common substratum for all
   classes in their pursuit of a rule case's incarnation.")

;;; -------------------------------------------------------

(defstruct (Conditional-Rule-Case
  (:include Rule-Case))
  "The ``Conditional-Rule-Case'' class models a conditional case for a
   \"Deadfish TM\" rule, its componency exhausted by the twissel of the
   activating state and symbol."
  (activating-states  (error "Missing activating state.")
                      :type      State-Predicate
                      :read-only T)
  (activating-symbols (error "Missing activating symbol.")
                      :type      (list-of character)
                      :read-only T))

;;; -------------------------------------------------------

(defstruct (Default-Rule-Case
  (:include Rule-Case))
  "The ``Default-Rule-Case'' class models a \"Deadfish TM\" program's
   default rule, its amenability to inquisitions intended in such
   circumstances which fail in one of the conditional rule's
   activation.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule transition.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Rule-Transition
  "The ``Rule-Transition'' class serves in the encapsulation of a
   \"Deadfish TM\" rule's transition component, the circumference of
   its lococession amplecting the quadruple componency of the Deadfish
   instructions to perform, the new symbol to write to the current tape
   cell, a direction into which to translate the tape head, and, in its
   desinence, the halting and output actions."
  (deadfish-code  (error "Missing Deadfish code.")
                  :type      deadfish-code
                  :read-only T)
  (new-symbol     (error "Missing new symbol.")
                  :type      character
                  :read-only T)
  (head-direction (error "Missing head direction.")
                  :type      head-direction
                  :read-only T)
  (halt-action    (error "Missing halt action.")
                  :type      halt-action
                  :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Rule
  "The ``Rule'' class is apportioned the dever of expressing a
   \"Deadfish TM\" program's rule, this being a composition of the
   antecedent case and the consequent transition."
  (case       (error "Missing rule case.")
              :type      Rule-Case
              :read-only T)
  (transition (error "Missing rule transition.")
              :type      Rule-Transition
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule set.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Rule-Set ()
  ((default-rule
    :initarg       :default-rule
    :initform      NIL
    :reader        get-default-rule
    :type          (or null Rule)
    :documentation "The default rule, intended for the selection upon
                    the CONDITIONAL-RULES' disrespondency.")
   (conditional-rules
    :initarg       :conditional-rules
    :initform      NIL
    :reader        get-conditional-rules
    :type          (list-of Rule)
    :documentation "An ordered list of conditional rules."))
  (:documentation
    "The ``Rule-Set'' class applies itself to the castaldy of zero or
     more \"Deadfish TM\" rules."))

;;; -------------------------------------------------------

(defun empty-rule-set-p (rules)
  "Determines whether the set of RULES is devoid of any entries, that
   is, neither endowed with a default rule nor conditional specimens,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Rule-Set rules))
  (the boolean
    (get-boolean-value-of
      (and (null (slot-value rules 'default-rule))
           (null (slot-value rules 'conditional-rules))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Scanner ()
  ((source
    :initarg       :source
    :initform      (error "Missing scanner source.")
    :type          string
    :documentation "The piece of \"Deadfish TM\" source code to
                    analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The zero-based index into the SOURCE's current
                    location.")
   (character
    :initform      #\Null
    :type          character
    :documentation "The character at the contemporaneous POSITION into
                    the SOURCE.")
   (source-exhausted-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the SOURCE is exhausted, which
                    constitutes a tantamount to the POSITION cursor's
                    transcendence of the former's dextral bourne."))
  (:documentation
    "The ``Scanner'' class serves in the provision of a lexical
     analyzer, the amenability invested into whom homologates the
     helming of its faculties for the retrieval of information from the
     string source under perquisition."))

;;; -------------------------------------------------------

(defmacro with-scanner ((scanner) &body body)
  "Evaluates the SCANNER, binds its slot ``source'' to the local symbol
   macro ``$source'', the ``position'' to ``$position'', the
   ``character'' to ``$character'', and the flag ``source-exhausted-p''
   to ``$source-exhausted-p'', evaluates the BODY forms, and returns the
   desinent form's results."
  (let ((evaluated-scanner (gensym)))
    (declare (type symbol evaluated-scanner))
    `(let ((,evaluated-scanner ,scanner))
       (declare (type Scanner ,evaluated-scanner))
       (declare (ignorable    ,evaluated-scanner))
       (symbol-macrolet
           (($source
             (the string
               (slot-value ,evaluated-scanner 'source)))
            ($position
              (the fixnum
                (slot-value ,evaluated-scanner 'position)))
            ($character
              (the character
                (slot-value ,evaluated-scanner 'character)))
            ($source-exhausted-p
              (the boolean
                (slot-value ,evaluated-scanner 'source-exhausted-p))))
         (declare (type string    $source))
         (declare (ignorable      $source))
         (declare (type fixnum    $position))
         (declare (ignorable      $position))
         (declare (type character $character))
         (declare (ignorable      $character))
         (declare (type boolean   $source-exhausted-p))
         (declare (ignorable      $source-exhausted-p))
         ,@body))))

;;; -------------------------------------------------------

(defun update-scanner-state (scanner)
  "Updates the SCANNER's state with respect to the current character and
   the exhaustion flag, and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (if (array-in-bounds-p $source $position)
      (setf $character          (char $source $position))
      (setf $source-exhausted-p T)))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((scanner Scanner) &key)
  "Empights the SCANNER into its inchoate state and returns no value."
  (declare (type Scanner scanner))
  (update-scanner-state scanner)
  (values))

;;; -------------------------------------------------------

(defun make-scanner (source)
  "Creates and returns a fresh ``Scanner'' whose onus wones in the
   evaluation of the \"Deadfish TM\" SOURCE."
  (declare (type string source))
  (the Scanner
    (make-instance 'Scanner :source source)))

;;; -------------------------------------------------------

(defun advance-scanner (scanner)
  "Proceeding from the current position into the SCANNER's source,
   advances to the next character and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (setf $position
      (min (1+ $position) (length $source))))
  (update-scanner-state scanner)
  (values))

;;; -------------------------------------------------------

(defun skip-spaces (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more accolent spaces and horizontal tabs and
   returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop
      while (and (not $source-exhausted-p)
                 (or (char= $character #\Space)
                     (char= $character #\Tab)))
      do    (advance-scanner scanner)))
  (values))

;;; -------------------------------------------------------

(defun expect-space (scanner)
  "Determines whether the SCANNER's current character represents a
   space, on confirmation skipping the same and any accolent subsequent
   spaces, while returning no value; otherwise signals an error of an
   unspecified type."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (cond
      ($source-exhausted-p
        (error "Expected a space at position ~d, but found the ~
                source exhausted."
          $position))
      ((char/= $character #\Space)
        (error "Expected a space at position ~d, but encountered the ~
                character \"~c\"."
          $position $character))
      (T
        (skip-spaces scanner))))
  (values))

;;; -------------------------------------------------------

(defun skip-linebreaks (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more accolent linebreaks and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop
      while (and (not $source-exhausted-p)
                 (char= $character #\Newline))
      do    (advance-scanner scanner)))
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more whitespaces, a diorims whose circumference
   embraces both spaces, horizontal tabs, and newline entities, and
   returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop until $source-exhausted-p do
      (case $character
        ((#\Space #\Tab)
          (skip-spaces scanner))
        (#\Newline
          (skip-linebreaks scanner))
        (otherwise
          (loop-finish)))))
  (values))

;;; -------------------------------------------------------

(defun expect-linebreak (scanner)
  "Determines whether the SCANNER's current character represents a
   linebreak, on confirmation skipping the same and any accolent
   newlines, while returning no value; otherwise signals an error of an
   unspecified type."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (cond
      ($source-exhausted-p
        (error "Expected a linebreak at position ~d, but found the ~
                source exhausted."
          $position))
      ((char/= $character #\Newline)
        (error "Expected a linebreak at position ~d, but encountered ~
                the character \"~c\"."
          $position $character))
      (T
        (skip-linebreaks scanner))))
  (values))

;;; -------------------------------------------------------

(defun expect-character (scanner expected-character)
  "Determines whether the SCANNER's current character constitutes the
   EXPECTED-CHARACTER, consuming the same on confirmation, while
   returning no value; otherwise signals an error of an unspecified
   type."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (cond
      ($source-exhausted-p
        (error "Expected the character \"~c\" at position ~d, ~
                but found the source exhausted."
          expected-character $position))
      ((char/= $character expected-character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\"."
          expected-character $position $character))
      (T
        (advance-scanner scanner))))
  (values))

;;; -------------------------------------------------------

(defun read-optional-comment (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   an optional comment section and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (unless $source-exhausted-p
      (case $character
        (#\Space
          (loop until $source-exhausted-p do
            (case $character
              (#\Newline
                (loop-finish))
              (otherwise
                (advance-scanner scanner)))))
        (#\Newline
          NIL)
        (otherwise
          (error "Expected a space introducing a comment section ~
                  or a linebreak at position ~d, but encountered the ~
                  character \"~c\"."
            $position $character)))))
  (values))

;;; -------------------------------------------------------

(defun read-single-state (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a single state and returns the same."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the integer
      (cond
        ($source-exhausted-p
          (error "Expected a numeric state at position ~d, but found ~
                  the source exhausted."
            $position))
        ((digit-char-p $character)
          (parse-integer
            (with-output-to-string (state)
              (declare (type string-stream state))
              (loop
                while (and (not $source-exhausted-p)
                           (digit-char-p $character))
                do
                  (write-char $character state)
                  (advance-scanner scanner)))))
        (T
          (error "Expected a numeric state at position ~d, but ~
                  encountered the character \"~c\"."
            $position $character))))))

;;; -------------------------------------------------------

(defun read-state-range (scanner start-state)
  "Proceeding from the current position into the SCANNER's source, reads
   a range of states, the minimum bourne already being contributed via
   the START-STATE, and returns a ``State-Range-Predicate''
   encapsulation thereof."
  (declare (type Scanner scanner))
  (declare (type integer start-state))
  (expect-character scanner #\-)
  (let ((end-state (read-single-state scanner)))
    (declare (type integer end-state))
    (validate-state-range-bournes start-state end-state)
    (the State-Range-Predicate
      (make-state-range-predicate
        :minimum-state start-state
        :maximum-state end-state))))

;;; -------------------------------------------------------

(defun read-state-list (scanner first-state)
  "Proceeding from the current position into the SCANNER's source, reads
   a list of states, the incipient member already being contributed via
   the FIRST-STATE, and returns a ``State-List-Predicate'' encapsulation
   thereof."
  (declare (type Scanner scanner))
  (declare (type integer first-state))
  (with-scanner (scanner)
    (the State-List-Predicate
      (loop
        while (and (not $source-exhausted-p)
                   (char= $character #\,))
        collect
          (progn
            (expect-character  scanner #\,)
            (read-single-state scanner))
          into
            state-list
        finally
          (return
            (make-state-list-predicate :states
              (cons first-state state-list)))))))

;;; -------------------------------------------------------

(defun read-state (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a state predicate and returns a ``State-Predicate'' encapsulation
   thereof."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (let ((first-state (read-single-state scanner)))
      (declare (type integer first-state))
      (the State-Predicate
        (if $source-exhausted-p
          (make-singleton-state-predicate :state first-state)
          (case $character
            (#\-
              (read-state-range scanner first-state))
            (#\,
              (read-state-list scanner first-state))
            (otherwise
              (make-singleton-state-predicate :state first-state))))))))

;;; -------------------------------------------------------

(defun read-symbol (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a tape symbol and returns the same in its character form."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the character
      (cond
        ($source-exhausted-p
          (error "Expected a symbol at position ~d, but found the ~
                  source exhausted."
            $position))
        ((symbol-character-p $character)
          (prog1 $character
            (advance-scanner scanner)))
        (T
          (error "Expected a symbol at position ~d, but encountered ~
                  the inadmissible character \"~c\"."
            $position $character))))))

;;; -------------------------------------------------------

(defun read-symbols (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   an ordered list of one or more tape symbols and returns a list
   comprehending these."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (let ((first-symbol (read-symbol scanner)))
      (declare (type character first-symbol))
      (the (list-of character)
        (loop
          while (and (not $source-exhausted-p)
                     (symbol-character-p $character))
          collect (read-symbol scanner)
            into  subsequent-symbols
          finally
            (return
              (cons first-symbol subsequent-symbols)))))))

;;; -------------------------------------------------------

(defun read-case (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a \"Deadfish TM\" rule's case and returns a ``Conditional-Rule-Case''
   representation thereof."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (the Conditional-Rule-Case
    (make-conditional-rule-case
      :activating-states
        (prog1
          (read-state   scanner)
          (expect-space scanner))
      :activating-symbols
        (prog1
          (read-symbols         scanner)
          (read-optional-comment scanner)))))

;;; -------------------------------------------------------

(defun read-deadfish-instruction (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a Deadfish command identifier and returns a ``deadfish-instruction''
   representation thereof."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the deadfish-instruction
      (cond
        ($source-exhausted-p
          (error "Expected a Deadfish instruction at position ~d, ~
                  but found the source exhausted."
            $position))
        ((deadfish-identifier-p $character)
          (prog1
            (parse-deadfish-instruction $character)
            (advance-scanner scanner)))
        (T
          (error "Expected a Deadfish instruction at position ~d,
                  but encountered the character \"~c\"."
            $position $character))))))

;;; -------------------------------------------------------

(defun read-deadfish-code (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a sequence of one or more Deadfish instructions and returns a
   ``deadfish-code'' representation thereof, maintaining its lealty to
   the specification ordonnance."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (let ((first-command (read-deadfish-instruction scanner)))
      (declare (type deadfish-instruction first-command))
      (the deadfish-code
        (loop
          while (and (not $source-exhausted-p)
                     (deadfish-identifier-p $character))
          collect (read-deadfish-instruction scanner)
            into commands
          finally
            (return
              (cons first-command commands)))))))

;;; -------------------------------------------------------

(defun read-head-direction (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a tape head, or \"pointer\", motion direction and returns a
   ``head-direction'' representation thereof."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the head-direction
      (cond
        ($source-exhausted-p
          (error "Expected a tape head motion direction at position ~d,
                  but found the source exhausted."
            $position))
        ((head-direction-identifier-p $character)
          (prog1
            (parse-head-direction $character)
            (advance-scanner      scanner)))
        (T
          (error "Expected a tape head motion direction at ~
                  position ~d, but encountered the character \"~c\"."
            $position $character))))))

;;; -------------------------------------------------------

(defun read-halt-action (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a halt action and returns a ``halt-action'' representation thereof."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the halt-action
      (cond
        ($source-exhausted-p
          (error "Expected a halt action at position ~d, but found ~
                  the source exhausted."
            $position))
        ((halt-action-identifier-p $character)
          (prog1
            (parse-halt-action $character)
            (advance-scanner   scanner)))
        (T
          (error "Expected a halt action at position ~d, but ~
                  encountered the character \"~c\"."
            $position $character))))))

;;; -------------------------------------------------------

(defun read-transition (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a transition and returns a ``Rule-Transition'' representation
   thereof."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (the Rule-Transition
    (make-rule-transition
      :deadfish-code
        (prog1
          (read-deadfish-code scanner)
          (expect-space       scanner))
      :new-symbol
        (prog1
          (read-symbol  scanner)
          (expect-space scanner))
      :head-direction
        (prog1
          (read-head-direction scanner)
          (expect-space        scanner))
      :halt-action
        (prog1
          (read-halt-action      scanner)
          (read-optional-comment scanner)))))

;;; -------------------------------------------------------

(defun read-default-rule (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   the default rule and returns a covenable representation thereof."
  (declare (type Scanner scanner))
  (the Rule
    (make-rule
      :case       (make-default-rule-case)
      :transition (read-transition scanner))))

;;; -------------------------------------------------------

(defun read-conditional-rule (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a conditional rule and returns a ``Rule'' representation thereof."
  (declare (type Scanner scanner))
  (the Rule
    (make-rule
      :case
        (prog1
          (read-case        scanner)
          (expect-linebreak scanner)
          (skip-whitespaces scanner))
      :transition
        (read-transition scanner))))

;;; -------------------------------------------------------

(defun read-rules (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a sequence of zero or more \"Deadfish TM\" rules and returns an
   ordered list of ``Rule'' representations."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (with-scanner (scanner)
    (the Rule-Set
      (loop
        for first-rule-p of-type boolean = T then NIL
        until $source-exhausted-p
        collect
          (prog1
            (if first-rule-p
              (read-default-rule scanner)
              (read-conditional-rule scanner))
            (skip-whitespaces scanner))
          into collected-rules
        finally
          (return
            (make-instance 'Rule-Set
              :default-rule      (first collected-rules)
              :conditional-rules (rest  collected-rules)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Tape character) (values))
                set-current-symbol))
(declaim (ftype (function (Tape) (values))
                move-tape-head-right))

;;; -------------------------------------------------------

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer character)
    :documentation "A sparse vector of characters, infinite in its
                    extent and amenable to signed integer subscripts.")
   (head
    :initform      0
    :type          integer
    :documentation "Selects at any instant the currently active cell
                    by mediation of its index into the CELLS table.")
   (lowest-cell-index
    :initform      0
    :type          integer
    :documentation "The minimum index of the explicitly modified cell
                    in the CELLS table.")
   (highest-cell-index
    :initform      0
    :type          integer
    :documentation "The maximum index of the explicitly modified cell
                    in the CELLS table."))
  (:documentation
    "The ``Tape'' class implements the \"Deadfish TM\" memory,
     manifesting in a tape of characters on which a mobile head
     operates in order to select at any instant the currently active
     unit."))

;;; -------------------------------------------------------

(defun make-empty-tape ()
  "Creates and returns a fresh ``Tape'' in its pristine state."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun make-initialized-tape (initial-symbols)
  "Creates and returns a fresh ``Tape'' which, proceeding from the
   inclusive START-INDEX of the first cell to prepare, inserts the
   INITIAL-SYMBOLS in a strictly dextral expansion, while retaining the
   head's default location at the incipial position.
   ---
   Please heed that merely admissible characters are transferred from
   the INITIAL-SYMBOLS.
   ---
   Please heed that the tape head, while homologated to relocate itself
   during the operation's process, will stringently be empight on the
   incipial first cell at the function's conclusion."
  (declare (type string  initial-symbols))
  (let ((new-tape (make-empty-tape)))
    (declare (type Tape new-tape))
    (loop
      for current-symbol of-type character across initial-symbols
      and first-symbol-p of-type boolean   = T then NIL
      when (symbol-character-p current-symbol) do
        (unless first-symbol-p
          (move-tape-head-right new-tape))
        (set-current-symbol new-tape current-symbol)
      finally
        (setf (slot-value new-tape 'head) 0))
    (the Tape new-tape)))

;;; -------------------------------------------------------

(defun update-tape-bournes-with-respect-to-head (tape)
  "Updates the TAPE's minimum and maximum cell indices in relation to
   the current TAPE head position and returns no value."
  (declare (type Tape tape))
  (with-slots (lowest-cell-index highest-cell-index head) tape
    (declare (type integer lowest-cell-index))
    (declare (type integer highest-cell-index))
    (declare (type integer head))
    (psetf lowest-cell-index  (min lowest-cell-index  head)
           highest-cell-index (max highest-cell-index head)))
  (values))

;;; -------------------------------------------------------

(defun update-tape-bournes-with-respect-to-index (tape index)
  "Updates the TAPE's minimum and maximum cell indices in relation to
   the specified cell INDEX and returns no value."
  (declare (type Tape tape))
  (with-slots (lowest-cell-index highest-cell-index) tape
    (declare (type integer lowest-cell-index))
    (declare (type integer highest-cell-index))
    (psetf lowest-cell-index  (min lowest-cell-index  index)
           highest-cell-index (max highest-cell-index index)))
  (values))

;;; -------------------------------------------------------

(defun get-symbol-at (tape index)
  "Returns the symbol stored in the TAPE cell amenable to the INDEX."
  (declare (type Tape    tape))
  (declare (type integer index))
  (the character
    (gethash index
      (slot-value tape 'cells)
      #\!)))

;;; -------------------------------------------------------

(defun get-current-symbol (tape)
  "Returns the currently active TAPE cell's symbol."
  (declare (type Tape tape))
  (the character
    (get-symbol-at tape
      (slot-value tape 'head))))

;;; -------------------------------------------------------

(defun set-symbol-at (tape index new-symbol)
  "Replaces the symbol in the TAPE's cell amenable to the INDEX by the
   NEW-SYMBOL and returns no value."
  (declare (type Tape      tape))
  (declare (type integer   index))
  (declare (type character new-symbol))
  (setf (gethash index (slot-value tape 'cells))
        new-symbol)
  (update-tape-bournes-with-respect-to-index tape index)
  (values))

;;; -------------------------------------------------------

(defun set-current-symbol (tape new-symbol)
  "Replaces the symbol in the TAPE's active cell by the NEW-SYMBOL and
   returns no value."
  (declare (type Tape      tape))
  (declare (type character new-symbol))
  (set-symbol-at tape (slot-value tape 'head) new-symbol)
  (update-tape-bournes-with-respect-to-head tape)
  (values))

;;; -------------------------------------------------------

(defun move-tape-head-left (tape)
  "Translates the TAPE head one step to the left and returns no value."
  (declare (type Tape tape))
  (decf (slot-value tape 'head))
  (update-tape-bournes-with-respect-to-head tape)
  (values))

;;; -------------------------------------------------------

(defun move-tape-head-right (tape)
  "Translates the TAPE head one step to the right and returns no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'head))
  (update-tape-bournes-with-respect-to-head tape)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((tape Tape) (stream T))
  (declare (type Tape        tape))
  (declare (type destination stream))
  (loop
    for current-index
      of-type integer
      from    (slot-value tape 'lowest-cell-index)
      to      (slot-value tape 'highest-cell-index)
    do
      (format stream "~c"
        (get-symbol-at tape current-index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Turing machine.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Turing-Machine ()
  ((state
    :initform      0
    :type          integer
    :documentation "The machine's current state, or accumulator value.")
   (tape
    :initarg       :tape
    :initform      (make-empty-tape)
    :documentation "The machine's tape, composed of a bilaterally
                    infinite tally of character-valued cells.")
   (halted-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the Turing machine has ceased
                    its operation."))
  (:documentation
    "The ``Turing-Machine'' class serves in the edification of a
     Turing machine model accommodated for the haecceity to whom the
     commorancy in the \"Deadfish TM\" programming language is
     allotted, its componency enumerating a state machine, representing
     in signed integer numbers the traditional Deadfish accumulator's
     simulacrum in a new grade of conspectuity; as well as, its
     contribution the patration by the second moeity's facette, a
     bilaterally infinite tape of character-valued cells, operated upon
     by a motile head, or \"pointer\", as the contemporaneously active
     unit's designator among this dispansion's entirey."))

;;; -------------------------------------------------------

(defun make-default-turing-machine ()
  "Creates and returns a ``Turing-Machine'' whose configurations resort
   to their defaults."
  (the Turing-Machine
    (make-instance 'Turing-Machine)))

;;; -------------------------------------------------------

(defun make-turing-machine-with-tape (initial-tape)
  "Creates and returns a fresh ``Turing-Machine'' whose tape is
   appropriated from the INITIAL-TAPE and retained as a reference.
   ---
   Please heed that from the retention as a reference the corollary
   ensues that the INITIAL-TAPE object will be directly modified by the
   Turing Machine in response to the program rule's application."
  (declare (type Tape initial-tape))
  (the Turing-Machine
    (make-instance 'Turing-Machine :tape initial-tape)))

;;; -------------------------------------------------------

(defun get-turing-machine-state (machine)
  "Returns the Turing MACHINE's current state or accumulator value."
  (declare (type Turing-Machine machine))
  (the integer
    (slot-value machine 'state)))

;;; -------------------------------------------------------

(defun set-turing-machine-state (machine new-state)
  "Changes the Turing MACHINE into the NEW-STATE and returns no value."
  (declare (type Turing-Machine machine))
  (declare (type integer        new-state))
  (with-slots (state halted-p) machine
    (declare (type integer state))
    (declare (type boolean halted-p))
    (setf state new-state)
    (unless (<= 0 state 255)
      (setf halted-p T)))
  (values))

;;; -------------------------------------------------------

(defun get-turing-machine-tape (machine)
  "Returns the Turing MACHINE's tape."
  (declare (type Turing-Machine machine))
  (the Tape
    (slot-value machine 'tape)))

;;; -------------------------------------------------------

(defun turing-machine-halted-p (machine)
  "Determines whether the Turing MACHINE has halted its execution,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Turing-Machine machine))
  (the boolean
    (slot-value machine 'halted-p)))

;;; -------------------------------------------------------

(defun halt-turing-machine (machine)
  "Halts the Turing MACHINE and returns no value."
  (declare (type Turing-Machine machine))
  (setf (slot-value machine 'halted-p) T)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule selection.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rule-case-matches-p (case machine)
  (:documentation
    "Determines whether the rule CASE matches the Turing MACHINE's
     current configuration, returning on confirmation a ``boolean''
     value of ``T'', otherwise ``NIL''.")
  
  (:method ((case Conditional-Rule-Case) (machine Turing-Machine))
    (declare (type Conditional-Rule-Case case))
    (declare (type Turing-Machine        machine))
    (the boolean
      (get-boolean-value-of
        (and
          (contains-state-p
            (conditional-rule-case-activating-states case)
            (get-turing-machine-state machine))
          (member
            (get-current-symbol
              (get-turing-machine-tape machine))
            (conditional-rule-case-activating-symbols case)
            :test #'char=)))))
  
  (:method ((case Default-Rule-Case) (machine Turing-Machine))
    (declare (type Default-Rule-Case case))
    (declare (ignore                 case))
    (declare (type Turing-Machine    machine))
    (declare (ignore                 machine))
    (the boolean T)))

;;; -------------------------------------------------------

(defun rule-matches-p (rule machine)
  "Determines whether the RULE's case matches the Turing MACHINE's
   contemporaneous configurations, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Rule           rule))
  (declare (type Turing-Machine machine))
  (the boolean
    (get-boolean-value-of
      (rule-case-matches-p
        (rule-case rule)
        machine))))

;;; -------------------------------------------------------

(defun find-conditional-rule (rules machine)
  "Searches in the set of RULES for a conditional specimen whose
   activating states and symbols comply with the Turing MACHINE's
   contemporaneous configurations, returning on confirmation the first
   detected ``Rule'', otherwise responding with ``NIL''."
  (declare (type Rule-Set       rules))
  (declare (type Turing-Machine machine))
  (the (or null Rule)
    (find-if
      #'(lambda (current-rule)
          (declare (type Rule current-Rule))
          (rule-matches-p current-rule machine))
      (get-conditional-rules rules))))

;;; -------------------------------------------------------

(defun find-rule (rules machine)
  "Searches in the set of RULES for a rule to execute, probing in an
   incipient stage the conditional specimens, and resorting to the
   default upon their failure to comply with the Turing MACHINE's
   contemporaneous configurations, returning in the case of success the
   selected rule; otherwise, if none could be ascertained, signals an
   error of an unspecified type."
  (declare (type Rule-Set       rules))
  (declare (type Turing-Machine machine))
  (the Rule
    (or (find-conditional-rule rules machine)
        (get-default-rule      rules)
        (error "No rule detected for the current state ~d in ~
                conjunction with the current tape symbol \"~c\"."
          (get-turing-machine-state machine)
          (get-current-symbol
            (get-turing-machine-tape machine))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule execution.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric execute-deadfish-instruction (instruction machine)
  (:documentation
    "Executes the Deadfish INSTRUCTION utilizing the Turing MACHINE as
     the contingently target of its efficacy, and returns no value.")
  
  (:method ((instruction (eql :increment))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (set-turing-machine-state machine
      (1+ (get-turing-machine-state machine)))
    (values))
  
  (:method ((instruction (eql :decrement))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (set-turing-machine-state machine
      (1- (get-turing-machine-state machine)))
    (values))
  
  (:method ((instruction (eql :square))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (set-turing-machine-state machine
      (square
        (get-turing-machine-state machine)))
    (values))
  
  (:method ((instruction (eql :output-number))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (format T "~&~d"
      (get-turing-machine-state machine))
    (values))
  
  (:method ((instruction (eql :output-character))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (format T "~c"
      (code-char
        (get-turing-machine-state machine)))
    (values))
  
  (:method ((instruction (eql :input-character))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (format T "~&>> ")
    (finish-output)
    (let ((input (read-char NIL NIL #\!)))
      (declare (type character input))
      (set-current-symbol
        (get-turing-machine-tape machine)
        (or (and (symbol-character-p input)
                 input)
            #\!)))
    (clear-input)
    (values))
  
  (:method ((instruction (eql :no-op))
            (machine     Turing-Machine))
    (declare (type deadfish-instruction instruction))
    (declare (ignore                    instruction))
    (declare (type Turing-Machine       machine))
    (declare (ignore                    machine))
    (values)))

;;; -------------------------------------------------------

(defun execute-deadfish-code (deadfish-code machine)
  "Executes the instructions comprising the DEADFISH-CODE on the
   Turing MACHINE and returns no value."
  (declare (type deadfish-code  deadfish-code))
  (declare (type Turing-Machine machine))
  (dolist (instruction deadfish-code)
    (declare (type deadfish-instruction instruction))
    (execute-deadfish-instruction instruction machine))
  (values))

;;; -------------------------------------------------------

(defun handle-head-motion (head-direction machine)
  "Exercises the causatum woning in the tape HEAD-DIRECTION to the
   Turing MACHINE and returns no value."
  (declare (type head-direction head-direction))
  (declare (type Turing-Machine machine))
  (case head-direction
    (:left
      (move-tape-head-left
        (get-turing-machine-tape machine)))
    (:right
      (move-tape-head-right
        (get-turing-machine-tape machine)))
    (otherwise
      (error "Invalid tape head direction: ~s." head-direction)))
  (values))

;;; -------------------------------------------------------

(defun handle-halt-action (halt-action machine)
  "Exercises the causatum woning in the HALT-ACTION's principle to the
   Turing MACHINE and returns no value."
  (declare (type halt-action    halt-action))
  (declare (type Turing-Machine machine))
  (case halt-action
    (:halt-without-output
      (halt-turing-machine machine))
    (:halt-with-output
      (format T "~&~a"
        (get-turing-machine-tape machine))
      (halt-turing-machine machine))
    (:continue-without-output
      NIL)
    (:continue-with-output
      (format T "~&~a"
        (get-turing-machine-tape machine)))
    (otherwise
      (error "Invalid halt action: ~s." halt-action)))
  (values))

;;; -------------------------------------------------------

(defun execute-transition (transition machine)
  "Executes the rule TRANSITION on the Turing MACHINE and returns no
   value."
  (declare (type Rule-Transition transition))
  (declare (type Turing-Machine  machine))
  (execute-deadfish-code
    (rule-transition-deadfish-code transition)
    machine)
  (set-current-symbol
    (get-turing-machine-tape    machine)
    (rule-transition-new-symbol transition))
  (handle-head-motion
    (rule-transition-head-direction transition)
    machine)
  (handle-halt-action
    (rule-transition-halt-action transition)
    machine)
  (values))

;;; -------------------------------------------------------

(defun execute-rule (rule machine)
  "Executes the RULE's transition in the Turing MACHINE's context and
   returns no value."
  (declare (type Rule           rule))
  (declare (type Turing-Machine machine))
  (execute-transition
    (rule-transition rule)
    machine)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Deadfish TM program.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((machine
    :initform      (make-default-turing-machine)
    :type          Turing-Machine
    :documentation "The underlying Turing machine.")
   (rules
    :initarg       :rules
    :initform      (error "Missing program rules.")
    :type          Rule-Set
    :documentation "The rule set which operates on the MACHINE."))
  (:documentation
    "The ``Program'' class serves in the encapsulation of a
     \"Deadfish TM\" program, its diorism's entelechy that of a
     Turing machine variant, itself a composition of a character-valued
     tape and a integer-valued state machine, and a set of rule for
     operation on the compound."))

;;; -------------------------------------------------------

(defun initialize-program-tape (program)
  "Queries the standard input for a line of characters, creates a tape
   with the valid portion of the input symbols, stores a new Turing
   machine with this tape in the PROGRAM, and returns no value."
  (declare (type Program program))
  (format T "~&Please input the initial tape symbols: ")
  (finish-output)
  (setf (slot-value program 'machine)
    (make-turing-machine-with-tape
      (make-initialized-tape
        (read-line NIL NIL ""))))
  (values))

;;; -------------------------------------------------------

(defun execute-program (program)
  "Executes the \"Deadfish TM\" program consigned to the PROGRAM's
   castaldy, perpetuating its actions until the underlying Turing
   machine segues into a halting state, and returns no value."
  (declare (type Program program))
  (with-slots (machine rules) program
    (declare (type Turing-Machine machine))
    (declare (type Rule-Set       rules))
    (initialize-program-tape program)
    (loop until (turing-machine-halted-p machine) do
      (execute-rule
        (find-rule rules machine)
        machine)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Deadfish-TM (code)
  "Creates a \"Deadfish TM\" interpreter, its tape initialized with a
   line of symbols provided via the standard nput, interprets the piece
   of \"Deadfish TM\" source CODE, and returns no value."
  (declare (type string code))
  (execute-program
    (make-instance 'Program :rules
      (read-rules
        (make-scanner code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello world!".
(interpret-Deadfish-TM
  "# ! L 1
   0 !
   iiiiiiiisiiiiiiiia ! L 0
   72 !
   iiiiiiiiiiiiiiiiiiiiiiiiiiiiia ! L 0
   101 !
   iiiiiiia ! L 0
   108 !
   ai ! L 0
   109 !
   iia ! L 0
   111 !
   ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddda ! L 0
   32 !
   ddddddddddddddddddddddsiiiiiiiiiiiiiiiiiiia ! L 0
   119 !
   ddddddddai ! L 0
   112 !
   iia ! L 0
   114 !
   ddddddaii ! L 0
   110 !
   dddddddddda ! L 0
   100 !
   ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddda ! L 1")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Deadfish-TM
  "# ! L 1
   0 0
   o 0 L 1
   0 1
   iisiiiisddddddddddddddd 1 L 0
   49 !
   a ! L 0")

;;; -------------------------------------------------------

;; Unary adder: The program initializes the tape at its inchoation
;; stage, expecting the augend and addend operands in unary basis, each
;; number being a catena of "1" digits, the tally tantamount to the
;; magnitude. The operand twissel is segregated by a single "0".
;; 
;; This machine actually concatenates the separate "1" sequences by
;; relocating the "0" sepiment to the position immediately succeeding
;; the spliced "1" sequences.
(interpret-Deadfish-TM
  "# ! L 1
   0 1
   # 1 R 0
   0 0
   i 0 R 0
   1 !
   # ! L 2
   1 1
   i 0 L 0
   2 0
   dd 1 R 0")
