;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Esofish", invented by the Esolang user
;; "Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
;; and presented on September 28th, 2024, the woning of its kenspeckle
;; nature found in an extension of Jonathan Todd Skinner's "Deadfish"
;; language, which, as a supererogative instruction, offers a facility
;; to execute subprograms in the "5" programming language and its
;; various derivatives, their entirety, iterum, a vouchsafement provided
;; by the creative stithy of
;; "Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff".
;; 
;; 
;; Concept
;; =======
;; The Esofish programming language, in all other aspects an ipsissima
;; verba appropriation of Deadfish concepts, adhibits an aefauld
;; advenient operation yclept "r", whose dation manifest in the
;; execution of a "5" programming language subprogram, the actual
;; dialect selected from a confluency with the Esofish accumulator's
;; contemporaneous state.
;; 
;; The following treatise's ordination shall be a twifold enterprise,
;; inwith whose perimeter the inchoation remains a reserved
;; vouchsafement to the Esofish programming language's pertinence; while
;; the second moiety pledges its efforts to the parergal specimen, the
;; "5" trisulk and its subordinate variations.
;; 
;; 
;; Esofish
;; =======
;; The Esofish programming language limns a simulacrum of Deadfish, its
;; stock-father's mournival in competences valorized by an aefauld
;; facility whose quantitative meekness bewrays its qualitative dation
;; as a multum in parvo, involving the homologation of "5" subprograms'
;; evaluation in the context of every committed program line.
;; 
;; == ESOFISH: PROGRAMS OPERATE IN AN OLAMIC CYCLE ==
;; As with its genesis' provenance, an Esofish program's advancement
;; is edified upon an eviternally repeating process of program code
;; request and acquisition, its evaluation, and the cycle's propagation:
;; 
;;   (1) PROGRAM ACQUISITION:
;;       A request is airted towards the standard input conduit, its
;;       prevenience the signifying prompt message
;;       
;;         >> 
;;       
;;       and its consectary the thus delivered character sequences'
;;       obtention; the conformation homologating zero or more symbols.
;;   
;;   (2) PROGRAM EVALUATION:
;;       A consumption applies to its character from the most recent
;;       code string acquisition, whence ensues the affiliated action's
;;       eventuation. No-operation tokens instigate one newline
;;       character's issuance each.
;;   
;;   (3) REPETITION:
;;       The control flow, in its olamic iterance, returns to the
;;       inchoate state -> (1), which please consult.
;; 
;; == ESOFISH: ERRORS ARE CONSIGNED TO THE VALE OF MYTHS ==
;; Adherent to the ejusdem generis approach of liberal appreciation,
;; Esofish, ostending a consanguinity with its Deadfish cleronomy,
;; consigns its respondency to unrecognized tokens, in lieu of an
;; error's infliction, to a single newline character's issuance on the
;; standard output conduit.
;; 
;; == ESOFISH: THE MEMORY IS DEFINED BY AN INTEGER ACCUMULATOR ==
;; Among the preponderance of its haecceity's formulation, Esofish's
;; memory model derives with immediacy from Deadfish's incus,
;; enumerating a componency to whom an aefauld membership, a signed
;; integer-valued accumulator of no bournes along both axes' mickleness
;; is a fact of adhibition, and in thilk's inchoation the value zero
;; (0) furnishes the default constitution.
;; 
;; The aspiration for the entheus' mimicry does not wisth of an
;; eloignment's status by the kenspeckle "normalization" nomothesia:
;; If the accumulator state equals -1 or 256, the value relapses to its
;; incipiency's designment in the acquisition of the inchoate
;; zero-valued content.
;; 
;; 
;; 5
;; =
;; The "5" programming language, a gleek of dialects, in its diorisms'
;; endowment assigned the highest mete in the eloignment of the digit
;; one (1) for five (5), ostends an edification upon an integer-valued
;; accumulator, a queue, and a "constrol stack", or call stack, the
;; former twissel poses as the general data castaldy subjects, whereas
;; the latter's devoation is such of the instruction pointer (IP)
;; positions' membrance.
;; 
;; == "5": 5 + 15 + 35 = 5? ==
;; The "5" programming language actually serves in an aggregate's
;; agency, limning the provenance of concamerations that intrine the
;; eponymous and foundational "5", as well as the more sophisticated
;; brethrens "15" and "35" as dialects.
;; 
;; == "5": ONE EQUALS FIVE ==
;; The most kenspeckle species of nomothesia partaking in the "5" and
;; "15" language dialects is begotten in the "1=5" rule, a stipulation
;; in concord with whose realization all "1" digits shall be committed
;; into a bartery for "5" symbols.
;; 
;; Related in a concrete diction, a vergent instruction processing
;; cyle's desinent action comprises the substitution of each one (1)
;; digit by a five (5) in both the accumulator and the instruction
;; pointer (IP).
;; 
;; This imperative adhibition does not wist of a default stipulation in
;; the "35" variant, however, where both accumulator and instruction
;; pointer attain a state of immunity from this odd modulation ---
;; except, of course, for an explicit application in the form of a "\"
;; operation, inwith whose demesne is located the diorism's temporary
;; activation.
;; 
;; == "5": ACCUMULATOR + QUEUE = MEMORY ==
;; A twifold componency's demense is registered in the subject of the
;; "5" programming language's memory model, the paravaunt participant
;; an integer-valued accumulator, its parhedral compernage associated
;; as a queue whose elements consigned to the selfsame numeric realm.
;; 
;; == "5": THE ACCUMULATOR AS A SCALAR INTEGER STORAGE ==
;; Parvipotent only in its quantitative circumference, the accumulator's
;; accoutrement constitutes that of an aefauld signed integer number's
;; admission, its ken deprived of any bournes' imposition concerning
;; the mickleness along both airts of polarity.
;; 
;; Maugre its, provisional, tolerance's adhibition towards negative
;; values, a kenspeckle ilk of respondency appertains to this state:
;; Upon the accumulator's transgression of the threshold of zero (0),
;; a removal of the queue's is actuated, ere its assignment to the
;; accumulator as the new datum.
;; 
;; == "5": THE QUEUE AS A SEQUENCE OF INTEGERS ==
;; A parergon entalented with superior capacity in constrast to the
;; accumulator's singleton apprehension, the memory's queue establishes
;; a warklume of castaldy conferred upon an arbitrary accompt of
;; signed integer elements.
;; 
;; Its firmament the eponymous first-in first-out data structure,
;; extant constituents' eloignments occur at the front, while the
;; membership's curtailment limns an encheson at the rear spatiality.
;; 
;; Several implements of supererogation serve in the valorization
;; concerning the fundamental queue notion, chiefly siccan nuncupated
;; to the content's rearrangement.
;; 
;; == "5": THE CALL STACK MEMORIZES THE INSTRUCTION POINTER POSITIONS ==
;; A warklume to whose supputation a high grade of potential ought to be
;; avered appertains to the "control stack", or call stack, its wike the
;; instruction pointer (IP) positions' governance as an express behest's
;; ultimity, whence an emergence in the control flow's redirections may
;; be attested.
;; 
;; Upon a more meticulous conspection's adhibition, this data structure
;; bewrays its euonymous agnomination, operating as a last-in first-out
;; salvatory, where unsigned integer-valued indices into the "5" program
;; entertain their castaldy.
;; 
;; == "5": SINGLE SYMBOLS REPRESENT INSTRUCTIONS ==
;; The syntactical conformation involved in a "5" program always
;; proceeds from a single symbol's assignment to any of the language's
;; operative competences.
;; 
;; Characters accompting for a tholance of carency in any such
;; affiliation receive a no-operation role, deprived of epiphenomenal
;; impact, yet in the usufructure of the interpreter's tolerance.
;; 
;; == "5": COMMENTS ==
;; The provision for comments eludes the basic "5" variant of the
;; language, but participates as a contubernal element in the "15" and
;; "35" extensions' standard.
;; 
;; A commentary block's inchoation is realized in the left parenthesis,
;; "(", its cessation in a right parenthesis ")", ensconcing an
;; arbitary account of symbols accommodated by this dispansion. Such
;; blocks do not wist of any nesting, that is, as a forbisen's
;; adduction, the construct
;; 
;;   (ab(c)d)
;; 
;; limns a tantamount meted with patration to
;; 
;;   (abcd)
;; 
;; As a parasceve to a "15" or "35" program's execution, all comments'
;; expungement must be ascertained, the code remnant establishing the
;; actual source code to process.
;; 
;; 
;; Instructions
;; ============
;; The status of the "5" programming language family and its dependent
;; Esofish as one designated by a contubernal relationship imposes upon
;; this treatise a comprehensiveness in both moeities' elucidation;
;; whence the superior constituent, the Esofish language, shall become
;; the inchoate cynosure's occupant, succeeded by its parhedral
;; companionship in the immediacy of a sequela.
;; 
;; == ESOFISH: A QUINTUPLE INSTRUCTION SET'S DEMESNE ==
;; A scion of Deadfish's notions, Esofish's acquisitions desume the
;; cleronomy's entire mournival of hereditaments, the circumference
;; engendered from this modest heritage registers basic arithmetics and
;; a numeric output construct.
;; 
;; A valorization whose edification is exerted upon Deadfish's basic
;; infrastructure, Esofish's sole instance of supererogation, the "r"
;; command, natheless introduces a powerful warklume capacitating a
;; Turing-complete language's installment.
;; 
;; == OVERVIEW: ESOFISH COMMANDS ==
;; The following apercu's cynosure shall be realized in a sufficient
;; mete of nortelry's dation concerning the Esofish programming
;; language's competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   d       | Decrements the Esofish accumulator by one (1).
;;           |---------------------------------------------------------
;;           | If the new accumulator value equals -1 or 256, the state
;;           | is restored to the inchoate zero (0).
;;   ..................................................................
;;   i       | Increments the Esofish accumulator by one (1).
;;           |---------------------------------------------------------
;;           | If the new accumulator value equals -1 or 256, the state
;;           | is restored to the inchoate zero (0).
;;   ..................................................................
;;   s       | Squares the Esofish accumulator, that is, multiplies
;;           | its value by itself.
;;           |---------------------------------------------------------
;;           | If the new accumulator value equals 256, the state is
;;           | restored to the inchoate zero (0).
;;   ..................................................................
;;   o       | Prints the Esofish accumulator value in its verbatim
;;           | numeric form to the standard output.
;;   ..................................................................
;;   r       | Construes the remaining content of the current Esofish
;;           | program, immediately succeeding this "r" symbol, as a
;;           | "5" program, the concrete variant being chosen as a
;;           | tantamount of the contemporaneous Esofish accumulator
;;           | value, executes the resulting "5" code.
;;           |---------------------------------------------------------
;;           | The following correlation is stipulated betwixt the
;;           | Esofish accumulator state and the deployed "5"
;;           | programming language variant:
;;           | 
;;           |   ------------------------------------------------------
;;           |   Accumulator | "5" language variant
;;           |   ------------+-----------------------------------------
;;           |   5           | "5"
;;           |   ......................................................
;;           |   15          | "15"
;;           |   ......................................................
;;           |   35          | "35"
;;           |   ......................................................
;;           |   Otherwise   | No causatum; the remaining code is
;;           |               | simply ignored.
;;           |   ------------------------------------------------------
;;   ------------------------------------------------------------------
;; 
;; == "5": 19 THROUGH 24 FACILITIES ==
;; The 5 programming language, at the moment of this documentation's
;; preparation, intrines a foundational set of 19 instructions, as well
;; as a quadruple in facilities restricted to the "15" and "35"
;; specimens, and siccan aefauld membership recluded to the desinent
;; variation only, the total accompt of which engenderes the
;; supputation of 24 elements.
;; 
;; == OVERVIEW: COMMON OPERATIONS OF ALL "5" VARIANTS ==
;; The following apercu's cynosure shall be established in those
;; operations of commonality to all variants of the 5 programming
;; language, scilicet, the 5, 15, and 35 specimens.
;; 
;; Please heed the demarcation of succedaneous parcels by an underlining
;; catena of percentage signs, "%", whose ultimate purpose resolves to
;; their substitution by actual 5 code fragments in the final program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   number  | Sets the accumulator to the integral {number}.
;;   %%%%%%  |---------------------------------------------------------
;;           | The {number} must be a catena enumerating one or more
;;           | decimal digits, forming an unsigned integer number.
;;   ..................................................................
;;   «       | Performs a digit-wise decimal left shift of the
;;           | accumulator by one (1) position, which is tantamount to
;;           | its multiplication by a factor of ten (10).
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   accumulator <- accumulator * 10
;;   ..................................................................
;;   »       | Performs a digit-wise decimal right shift of the
;;           | accumulator by one (1) position, which is tantamount to
;;           | its division by a factor of ten (10) and subsequent
;;           | rounding down, or, equivalently, a floor operation
;;           | administered to the unrounded quotient.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   accumulator <- floor(accumulator / 10)
;;   ..................................................................
;;   +       | Increments the accumulator value by one (1).
;;   ..................................................................
;;   -       | Decrements the accumulator value by one (1).
;;           |---------------------------------------------------------
;;           | If the new accumulator state descends alow the minimum
;;           | of zero (0), the memory queue's front element is
;;           | removed and transferred in the accumulator as its new
;;           | value.
;;   ..................................................................
;;   ²       | Squares the accumulator value, that is, multiplies its
;;           | state by its own value, or, equivalently, raises the
;;           | value by a power of two (2).
;;   ..................................................................
;;   ³       | Cubes the accumulator value, that is, multiplies its
;;           | state twice by its own value, or, equivalently, raises
;;           | the value by a power of three (3).
;;   ..................................................................
;;   ~       | Sets the accumulator to its bitwise NOT.
;;   ..................................................................
;;   ,       | Sets the accumulator to the tally of the occurrences of
;;           | the digit five (5) in its current state.
;;   ..................................................................
;;   ?       | Inserts the accumulator value at the memory queue's rear
;;           | position.
;;   ..................................................................
;;   *       | Relocates the memory queue's front element to its rear
;;           | position.
;;           |---------------------------------------------------------
;;           | This process constitutes limns an owelty to the front
;;           | element's dequeuing with a subsequent enqueuing at the
;;           | tail, as elucidated in this pseudocode tmema:
;;           | 
;;           |   let frontElement <- dequeue()
;;           |   enqueue(frontElement)
;;           |---------------------------------------------------------
;;           | If the queue is empty at the instant of this operation's
;;           | invocation, no causatum is accompassed.
;;   ..................................................................
;;   ^       | Relocates the memory queue's rear element to its front
;;           | position.
;;           |---------------------------------------------------------
;;           | This process constitutes limns an owelty to the rear
;;           | element's deletion with a subsequent insertion at the
;;           | front, as elucidated in this pseudocode tmema:
;;           | 
;;           |   let rearElement <- removeLast()
;;           |   insertAtFront(rearElement)
;;           |---------------------------------------------------------
;;           | If the queue is empty at the instant of this operation's
;;           | invocation, no causatum is accompassed.
;;   ..................................................................
;;   {       | Pushes the current instruction pointer (IP) position
;;           | onto the call stack's top position.
;;   ..................................................................
;;   }       | Pops the top call stack element and relocates the
;;           | instruction pointer (IP) to the same.
;;   ..................................................................
;;   |       | Pops the top call stack element and discards the same.
;;   ..................................................................
;;   ;       | Skips the next instruction.
;;   ..................................................................
;;   !       | If the accumulator value equals zero (0), skips the
;;           | next instruction; otherwise accompasses no causatum.
;;   ..................................................................
;;   [       | Moves the instruction pointer (IP) forward to the
;;           | matching "]" token.
;;   ..................................................................
;;   ]       | Serves as a destination point for a "[" instruction.
;;           |---------------------------------------------------------
;;           | Please heed that this operation does not move the
;;           | instruction pointer (IP) back to the matching "[" token.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW: COMMANDS EXCLUSIVE TO THE "15" AND "35" VARIATIONS ==
;; A set of additional adjectitious operations serves to eek the
;; capacitation of the language versions 15 and 35, to whose symbolic
;; expression no epiphenomenal potential's parcery enjoys an assignment
;; in the basic 5 language.
;; 
;; Please iterum probe your conspection on the convention involving
;; succedaneous tmemata's signification via an underlining of "%"
;; symbols.
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   `chars` | Prints the {chars} to the standard output conduit,
;;    %%%%%  | succeeded by a single newline character. Upon this
;;           | character sequence's vacuity, that is, the ensconcement
;;           | of an empty string, a single newline character is
;;           | printed.
;;           |---------------------------------------------------------
;;           | If no terminating "`" character can be detected, an
;;           | error of the type "UnterminatedStringError" is signaled.
;;   ..................................................................
;;   #       | Prints the content of the accumulator in its verbatim
;;           | numeric form to the standard output conduit, succeeded
;;           | by a single newline character.
;;   ..................................................................
;;   $       | Prints the character whose Unicode code point
;;           | corresponds to the accumulator's value to the standard
;;           | output conduit, appending no newline or other
;;           | adscititious appendage.
;;           |---------------------------------------------------------
;;           | If the accumulator value cannot be mapped to a Unicode
;;           | code point, the consequent behavior is undefined.
;;   ..................................................................
;;   @       | Queries the standard input conduit for a line of
;;           | characters and stores the result in the accumulator.
;;           | In a concrete diction, the following nomothesia's
;;           | governance extends across the response's interpretation:
;;           | 
;;           |   (a) If the input represents an unsigned integer
;;           |       number, its parsed integral value is transferred
;;           |       into the accumulator.
;;           |   
;;           |   (b) If the input represents a single character, its
;;           |       Unicode code point is transferred into the
;;           |       accumulator.
;;           |   
;;           |   (c) In any other case, an error of the type
;;           |       "InvalidInputError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW: COMMANDS EXCLUSIVE TO THE "35" VARIATION ==
;; The following valorization appertains to the 35 language variant
;; only, and retains in any other rendition the aquainted carency of
;; causata:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   `       | Applies the "1=5" rule to the accumulator, that is,
;;           | substitutes every occurrency of the digit one (1) by the
;;           | digit five (5) in the same.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Esofish protolog, curtailed to some mete into a rather nimious
;; distillation, ostends the tholance under a few ambivalencies'
;; cumbrances; a subset desumed from this unfortunate vale shall limn
;; the following sections' cynosure.
;; 
;; == WHAT MEASURES APPERTAIN TO UNDEFINED ACCUMULATOR SUBPROGRAMS? ==
;; The conceivable contingency for siccan attendance accords of a
;; trisulc ordination:
;; 
;;   (1) COMPLETE OMISSION:
;;       The mateotechny's emergence from the incongruency reverberates
;;       in the skipping of the entire tmema succeeding the instigating
;;       the "r" instruction, concluding the current program line in a
;;       premature manner without further epiphenomenal investment.
;;   
;;   (2) NO-OPERATION:
;;       Akin to any unrecognized symbol's involvement in a Deadfish
;;       program, and its homoiousian adhibition in the alligating
;;       cleronomy in Esofish's recipiency, a signification of the
;;       anomalous circumstance in the form of a single newline
;;       character's issuance to the standard output conduit is
;;       eventuated.
;;   
;;   (3) INTERPRETATION AS ESOFISH CODE:
;;       The carency in an adscititous construe as a "5" program in all
;;       of its multifarious formats installs the ultimity of all
;;       subsequent characters' processing as Esofish tokens. This
;;       deportment's rational consectary includes the homologation of
;;       a further "r" instruction's recognition.
;; 
;; The adjudgment's entertain has been settled upon to impose the
;; first option (1) as a sequela to an undefined subprogram in response
;; to the accumulator state, whence ensues the admittance of the
;; respective Esofish code tmema's presence as a gaisan subject.
;; 
;; == HOW SHALL ESOFISH REACT TO ANOMALIES IN A "5" PROGRAMS? ==
;; An obbligato to the contingency for abortive anomalies in a "5"
;; program, a forbisen desumed from these conceivable enchesons being
;; an unterminated string literal, the superimposed Esofish specimen's
;; proprium, in the consideration of its dation from Deadfish's cavalier
;; deportment in such circumstances, remains in its autochthonous
;; potentials eloigned from cessations, relocating the apodosis to a
;; mere newline issuance. The conflicting consequences' participation
;; levies the inquisition into the Esofish interpreter's response to
;; an error encounted in a relayed "5" program.
;; 
;; It has been adjudged, ensuing from the conspection of the peisant
;; rank of the intrinsic "5" interpreter in the Esofish language, to
;; impute a propagation and consequent cessation in the error behavior.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been accomplished in the
;; programming language Common Lisp, the efforts' explication one of
;; intricacy by its twifold componency's champarty, such is nemned in
;; the Esofish language and its parhedral "5" dependency.
;; 
;; == COMPLEX PROJECT CASTALDY AS A CYNOSURE ==
;; As a parhedral objective, peisant natheless in its influence on the
;; overall application structure, this program's purpose assigns itself
;; to the demonstration of a complex project's castaldy in Common Lisp's
;; realm.
;; 
;; This constitution's warkloom in its manifestation receives its
;; reified guise in packages, these serving as a threefold merist in
;; partage into the "5" specimen's interpreter, an equivalency for the
;; Esofish compartment, and a confluency betokened in a main tmema,
;; inwith those context the latter's programs may be invoked.
;; 
;; The treble package distribution shall be the alow exposition's hyle:
;; 
;;   ------------------------------------------------------------------
;;   Package name           | Role
;;   -----------------------+------------------------------------------
;;   5-programming-language | Comprehends the files thilk constitute
;;                          | "5" interpreter's services.
;;                          | The dever assigned to the nimious
;;                          | additament "-programming-language" is
;;                          | vindicated in the language's curtailed
;;                          | and generic agnomination, engendering an
;;                          | identifier less kenspeckle in conspection
;;                          | than, say, "Esofish".
;;   ..................................................................
;;   esofish                | Entails the Esofish interpreter's files,
;;                          | itself a dependent upon the package
;;                          | "5-programming-language".
;;   ..................................................................
;;   main                   | Furnishes a context's edification for the
;;                          | comfortable usance of the Esofish
;;                          | interpreter depending with immediacy on
;;                          | the "esofish" package and, eloigned but
;;                          | substantive, on thilk's
;;                          | "5-programming-language" reference.
;;                          |------------------------------------------
;;                          | This package, located in the "main.lisp"
;;                          | entry file and activated by the same,
;;                          | involves the capacitation, a fortiori, of
;;                          | the operation "interpret-esofish", and
;;                          | the several extant test programs.
;;   ------------------------------------------------------------------
;; 
;; Please heed that a listing of the project files is assigned to the
;; wike of the "Appendices" section, concretely the tmema
;; "APPENDIX A: PROJECT FILES", whose consultation is reded for the
;; interested party.
;; 
;; 
;; Appendices
;; ==========
;; The following sections shall provide a series of additaments in
;; reference to such topics that ostend a mete of pertinence
;; intermediate betwixt a vindication of its attendance and a docimasy
;; producing no stringency to incorporate in the primary writ.
;; 
;; == APPENDIX A: PROJECT FILES ==
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
;; enjoying --- a more serious enterprise certainly would be assayed in
;; an athwart airt.
;; 
;;   ------------------------------------------------------------------
;;   No. | File                      | Role
;;   ==================================================================
;;   PACKAGE "5-programming-language"
;;   ------------------------------------------------------------------
;;    1  | 5.packages.lisp           | Defines the "5" programming
;;       |                           | language's package, graced with
;;       |                           | the euonym
;;       |                           | "5-programming-language".
;;   ..................................................................
;;    2  | 5.types.lisp              | Ensconces the type definitions
;;       |                           | utible across the "5" parcel of
;;       |                           | this project.
;;   ..................................................................
;;    3  | 5.logical-operations.lisp | Offers operations desumed from
;;       |                           | the realm of Boolean logic.
;;   ..................................................................
;;    4  | 5.jump-table.lisp         | Defines the jump table as a
;;       |                           | data structure whose bailiwick
;;       |                           | wones in the connection of a "5"
;;       |                           | program's jump points, mediated
;;       |                           | via their positions.
;;   ..................................................................
;;    5  | 5.queue.lisp              | Provides an implementation of
;;       |                           | the "5" language memory's queue
;;       |                           | component.
;;   ..................................................................
;;    6  | 5.call-stack.lisp         | Realizes the call stack,
;;       |                           | ordained to the castaldy of
;;       |                           | instruction pointer (IP)
;;       |                           | positions.
;;   ..................................................................
;;    7  | 5.string-operations.lisp  | Applies itself to the handling
;;       |                           | of strings.
;;   ..................................................................
;;    8  | 5.numeric-operations.lisp | Provides functionality for the
;;       |                           | manipulation of numbers.
;;   ..................................................................
;;    9  | 5.input-output.lisp       | Handles communications along the
;;       |                           | input and output conduits.
;;   ..................................................................
;;   10  | 5.comment-excision.lisp   | Furnishes the capability of
;;       |                           | extracting commentary tmemata
;;       |                           | from strings.
;;   ..................................................................
;;   11  | 5.interpreter.lisp        | Implements the interpreter
;;       |                           | dedicated to the execution of
;;       |                           | "5" programs.
;;   ..................................................................
;;   12  | 5.condition-types.lisp    | Defines the condition types
;;       |                           | appertaining to all stages of
;;       |                           | the "5" programming language.
;;   ..................................................................
;;   13  | esofish.package.lisp      | Defines the package inwith whose
;;       |                           | context shall be defined the
;;       |                           | Esofish interpreter.
;;   ==================================================================
;;   PACKAGE "esofish"
;;   ------------------------------------------------------------------
;;   14  | esofish.interpreter.lisp  | Provides the Esofish
;;       |                           | interpreter.
;;   ..................................................................
;;   15  | esofish.tests.lisp        | Establishes the test cases and
;;       |                           | example programs, chevishing as
;;       |                           | latreutical agents as affedavits
;;       |                           | in the proof of this Esofish
;;       |                           | interpreter's correct
;;       |                           | operations.
;;   ==================================================================
;;   PACKAGE "main"
;;   ------------------------------------------------------------------
;;   16  | main.lisp                 | Defines the point of ingress
;;       |                           | into this application; in
;;       |                           | particular loading the various
;;       |                           | Common Lisp project files.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-10-22
;; 
;; Sources:
;;   [esolang2024Esofish]
;;   The Esolang contributors, "Esofish", October 2nd, 2024
;;   URL: "https://esolangs.org/wiki/Esofish"
;;   
;;   [goodrich2006datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 120--127 describe the doubly linked list.
;;       o The page 120 presents an implementation of a doubly linked
;;         list node in Java, norned "DNode".
;;       o The pages 125--127 present an implementation of a doubly
;;         linked list in Java.
;;     
;;     - The pages 213--216 describe the double-ended queue, or deque,
;;       abstract data type (ADT).
;;       o The pages 215--216 present a partial implementation in Java
;;         utilizing a doubly linked list.
;;     
;;     - The pages 231-241 describe the node list abstract data type
;;       (ADT).
;;       o This data type utilizes the notion of "positions" in order
;;         to furnish an abstraction of nodes for its elements' access.
;;       o The pages 234--235 describe an interface for the node list
;;         ADT, nevened "PositionList".
;;       o The page 235 mentions the equivalency of the node list
;;         operations and the deque counterparts.
;;       o The pages 236--241 present an implementation of the node list
;;         ADT via a doubly linked list, the product being yclept the
;;         "NodePositionList".
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 132--137 describe the concept and an implementation
;;       of the doubly linked list in the Java programming language.
;;       o The pages 135--137 furnish the implementation.
;;     
;;     - The pages 248--251 describe the concept and implementation of
;;       the double-ended queue, or deque, abstract data type (ADT).
;;       o The pages 250--251 describe the implementation of a deque via
;;         a doubly linked list.
;;     
;;     - The pages 276--280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype file-source ()
  "The ``file-source'' type defines a source for a file's obtention,
   utible, in a particular mete of supputation, for loading such
   external resources into a unified project."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global project variables and constants.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "The ``+PROJECT-DIRECTORY+'' global constant defines the commorancy
   of this interpreter's Common Lisp project files.
   ---
   Please substitute this global variable's content by the actual
   directory on your system which contains the Esofish interpreter's
   source files.
   ---
   Several facilities are offered by the Common Lisp standard library
   for engaging in such an activity, enumerating, for instance:
   
     -------------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+-------------------------------------------
     make-pathname    | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                 \"Users\"
                      |                 \"Kaveh\"
                      |                 \"Esofish\"
                      |                 \"Esofish_001\"))
     .............................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/Esofish/Esofish_001/\")
     -------------------------------------------------------------")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the project file management operations.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-the-source-file (source-file)
  "Loads the Common Lisp SOURCE-FILE, its woning accommodated below the
   +PROJECT-DIRECTORY+, and returns no value."
  (declare (type file-source source-file))
  (load
    (merge-pathnames +PROJECT-DIRECTORY+ source-file))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of the project files.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the "5" programming language project files.
(import-the-source-file "5.package.lisp")
(import-the-source-file "5.types.lisp")
(import-the-source-file "5.logical-operations.lisp")
(import-the-source-file "5.jump-table.lisp")
(import-the-source-file "5.queue.lisp")
(import-the-source-file "5.call-stack.lisp")
(import-the-source-file "5.string-operations.lisp")
(import-the-source-file "5.numeric-operations.lisp")
(import-the-source-file "5.input-output.lisp")
(import-the-source-file "5.comment-excision.lisp")
(import-the-source-file "5.interpreter.lisp")
(import-the-source-file "5.condition-types.lisp")

;; Load the "Esofish" programming language project files.
(import-the-source-file "esofish.package.lisp")
(import-the-source-file "esofish.interpreter.lisp")
(import-the-source-file "esofish.tests.lisp")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the main package.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :main
  (:use
    :cl
    :esofish))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of main package.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :main)
