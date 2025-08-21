;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter as well as converters for the
;; esoteric programming language "GERMAN", invented by the Esolang user
;; "Domi382" and presented on December 9th, 2014, its dioristic
;; contribution manifested in a cambistry applied to Urban Mueller's
;; brainfuck whose octuple instruction set's membership enjoys a
;; supersession by descriptive terms from the German language.
;; 
;; 
;; Concept
;; =======
;; The GERMAN programming language's haecceity wones in its conception
;; as a trivial brainfuck substitution, edifying an alternative syntaxis
;; to the entheus' octuple instruction identifiers in a guise which
;; desumes its diction from the German language.
;; 
;; == GERMAN: AN AGNOMINATION AS ITS CONCEPT'S AFFEDAVIT ==
;; The GERMAN programming language's equiparance to brainfuck solely
;; disengages in its dispansion from the stock-father's tents by a
;; discrepancy in the donat, its instruction identifers' provenance the
;; Almain tongue's dation.
;; 
;; == THE MEMORY: AN INFINITE DISPANSION OF UNSIGNED BYTES ==
;; Its perfect congruency with brainfuck in any mete of conspection
;; liberated from the syntactical guise entalents GERMAN with the
;; brainfuck memory model.
;; 
;; This data castaldy appertains to a bilaterally infinite tape of
;; unsigned byte-valued cells, each such a salvatory with the capacity
;; of an integral number from the closed interval [0, 255].
;; 
;; Upon any of its two bournes' transgression, the cell automatically
;; wraps the value around to the athwart border.
;; 
;; A cell pointer governs the onus of the currently active unit's
;; designation, its status the aefauld member contemporaneously invested
;; with an amenability to perquisitions and modulations. The mobile
;; nature of this cursor homologates a stillatim locomotion across the
;; tape pursuing both airts.
;; 
;; 
;; Instructions
;; ============
;; Its status as a descendant of brainfuck imparts GERMAN's competences
;; with a perfect congruency to the stock-father's haeceity, its account
;; enumerating the selfsame octuple membership.
;; 
;; == OVERVIEW ==
;; The eight operative warklumes shall be a tabulation's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command         | Effect
;;   ----------------+-------------------------------------------------
;;   LINKS           | Translates the cell pointer one step to the
;;                   | left.
;;   ..................................................................
;;   RECHTS          | Translates the cell pointer one step to the
;;                   | right.
;;   ..................................................................
;;   ADDITION        | Increments the current cell value by one (1).
;;                   | If the new state transgresses the upper bourne
;;                   | of 255, the value wraps around to the minimum of
;;                   | zero (0).
;;   ..................................................................
;;   SUBTRAKTION     | Decrements the current cell value by one (1).
;;                   | If the new state transgresses the lower bourne
;;                   | of zero (0), the value wraps around to the
;;                   | maximum of 255.
;;   ..................................................................
;;   EINGABE         | Queries the standard input conduit for a
;;                   | character and stores its ASCII code in the
;;                   | current cell.
;;   ..................................................................
;;   AUSGABE         | Prints the character whose ASCII code concurs
;;                   | with the current cell value to the standard
;;                   | output conduit.
;;   ..................................................................
;;   SCHLEIFENANFANG | If the current cell value equals zero (0), moves
;;                   | the instruction pointer (IP) forward to the
;;                   | position immediately succeeding the matching
;;                   | "SCHLEIFENENDE" token; otherwise proceeds as
;;                   | usual.
;;   ..................................................................
;;   SCHLEIFENENDE   | If the current cell value does not equal
;;                   | zero (0), moves the instruction pointer (IP)
;;                   | back to the position immediately succeeding the
;;                   | matching "SCHLEIFENANFANG" token; otherwise
;;                   | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == AN EQUIPARATION OF GERMAN AND BRAINFUCK ==
;; Its capacity of equiparation in the facilities' quantity and causata
;; homologates a juxtaposition of the GERMAN programming language and
;; brainfuck in its donat's aspect, as afforded in this tabulation,
;; the same is extended by a column serving in some sense as a delectus,
;; at least for our particular compass and context:
;; 
;;   --------------------------------------------------
;;   GERMAN          | brainfuck | English translation
;;   ----------------+-----------+---------------------
;;   LINKS           | <         | left
;;   ..................................................
;;   RECHTS          | >         | right
;;   ..................................................
;;   ADDITION        | +         | addition
;;   ..................................................
;;   SUBTRAKTION     | -         | subtraction
;;   ..................................................
;;   EINGABE         | ,         | input
;;   ..................................................
;;   AUSGABE         | .         | output
;;   ..................................................
;;   SCHLEIFENANFANG | [         | loop start
;;   ..................................................
;;   SCHLEIFENENDE   | ]         | loop end
;;   --------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The GERMAN language's perfect equiparation with brainfuck serves in
;; any inroads for ambivalencies' extinguishment.
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been entertained in the programming language
;; Common Lisp, the project's cynosure that of a didascalic endeavor,
;; whence shall be begotten a perquisition into design patterns ---
;; concretely, the "state" pattern, and, as a parhedral participant,
;; the "prototype" solution.
;; 
;; The presence of nimiety and its mete shall both not be subjected to
;; an arraignment anent their sensibility in the face of the GERMAN
;; programming language's simplistic conception.
;; 
;; == THE TELOS: REPRESENTING INSTRUCTIONS BY STATES ==
;; The paravaunt objective of this project, and the etiology for its
;; convolute nature, wones in the application of the "state" design
;; pattern, providing the advent of a telos whose formation prescribes
;; for each among the octuple GERMAN instructions a dedicated state,
;; concomitantly amplecting in its wike's compass the transition to the
;; subsequent one, while operating in the interpreter entity as its
;; superimposed ensconcing context.
;; 
;; == THE STATE DESIGN PATTERN: STATES DEFINE BEHAVIORS ==
;; The "state" design pattern subsumes into the behavioral species,
;; such is assigned the onus of facilitating the interaction betwixt
;; objects in a complex system.
;; 
;; The state species as particular forbisen emerges from the intricacies
;; to whom a system tholes its affliction if the deportment depends on
;; its internal configuration at a certain instant in time. Most
;; frequently, siccan physignomy's ostention registers a multiplicity in
;; internally processed conditional statements, or "if"-constructs.
;; 
;; == AN ANAGOGE: CONDITIONALS CAN BE REPLACED BY SUBCLASSES ==
;; The anagnorisis to an extant aggregate of conditionals' reformulation
;; is instigated by mediation of the polymorphic principle: Subclasses
;; of a common interface or class may react to an eponymous method
;; invocation in distinct manners. This accoutrement with a succedaneum
;; for catenas of "if" statements in dedicated classes identifies the
;; claviger of a system's dynamic behavior's partage into indepdent
;; units.
;; 
;; == COMPLEX SYSTEM = CONTEXT (DATA) + STATE MACHINE (BEHAVIOR) ==
;; The state pattern's approach to this circumstance's alleviation
;; expresses itself in the high-level segregation of the system into
;; a "context", to whom the commonly utible data castaldy and a
;; reference to the active state object is apportioned, and the "state"
;; moeity, defining a state machine, while constituting the recipient
;; of that parcery responsible for implementing the software's logic,
;; as well as transitioning between the subsequently necessitated
;; states.
;; 
;; Adduced as a purlicue, the context should, in a rather kenspeckle
;; mete, remain devoid of sophistication is operative value, while the
;; several states entrepart the functionality in a manner rendering
;; each entity from this species sufficiently competent and potent for
;; a well-defined subset of devers.
;; 
;; == THE CONTEXT: SERVANT TO THE CLIENT, CLIENT OF THE STATES ==
;; The "Context" defines the service component, thilk's exposure to the
;; client begets the operative expectations.
;; 
;; The kenspeckle attribute of the state design pattern harbors its
;; commorancy in the alienation of the operative logic from the context
;; into a series of linked states, everichon among these wisting in its
;; responsibilities of both the instructions to perform and the next
;; state to assign.
;; 
;; This delegation of responsibilities to an unspecific account of
;; states, each a subset of competences' salvatory, experiences its
;; manifestation in a field maintaining a reference to the currently
;; active state. A subsequent stage to its inchoation with an incipial
;; state, the transitions themselves elude the context's private
;; efforts; in lieu of this castaldy's encumbrance, the concrete states
;; themselves, their diorisms please behold alow, apply themselves to
;; the segues by the current state field's modulation.
;; 
;; == THE STATE: AN INTERFACE FOR THE ACTUALLY OPERATIVE UNITS ==
;; In the "State" interface is realized the firmament of the implicitly
;; defined state machine. In the most common manifestation, an aefauld
;; operation's imposition delineates the slim covenant to whom the
;; context's trust is arranged, and inwith whose circumference the
;; conditional case represented by the state subclass advances its
;; entelechia, producing some causatum, ere progressing into the
;; consectaneous next state.
;; 
;; == THE CONCRETE STATES: IMPLEMENTATION OF PARCELS OF LOGIC ==
;; Every concrete state implements the "State" interface in its pursuit
;; to produce a specific parcel of functionality from the "Context"
;; object's offered services.
;; 
;; In an actual apprehension, the concrete state's competences
;; bifurcates into a twissel of devers:
;; 
;;   (1) The expected causata's actuation; that is, the realization of
;;       the logic.
;;   
;;   (2) A transition to the next state in this implicitly defined
;;       state machine; its reification ensuing from the modification of
;;       the "Context" object's managed current state reference.
;; 
;; The following approximated simulacrum of a UML class diagram's dation
;; shall contribute a visual apercu concerning the participating
;; entities in such a state-centric solution.
;; 
;; Please heed that the public method "request()" of the "Context" class
;; simply invokes its currently active state's "handle()" operation,
;; effectively delegating the main onus to the concrete "State" subclass
;; implementation.
;; 
;;      +-------------------------------------+
;;      |               Context               |
;;      |-------------------------------------|
;;      | - currentState : State              |
;;      |-------------------------------------|
;;      | + setCurrentState (newState) : void |
;;      | + request ()                 : any  |
;;      +-------------------------------------+
;;                         |
;;                         |
;;                         |
;;                         V
;;               +-------------------+
;;               |   <<interface>>   |
;;               |       State       |
;;               |-------------------|
;;               |-------------------|
;;               | + handle () : any |
;;               +-------------------+
;;                         ^
;;                         |
;;                         | implements
;;                         |
;;             +-----------+----------+
;;             |                      |
;;             |                      |
;;             |                      |
;;   +-------------------+   +-------------------+
;;   |  ConcreteState1   |   |  ConcreteState2   |
;;   |-------------------|   |-------------------|
;;   |-------------------|   |-------------------|
;;   | + handle () : any |   | + handle () : any |
;;   +-------------------+   +-------------------+
;; 
;; == A TWISSEL OF DESIGN PATTERNS PARTAKE OF THIS PROJECT ==
;; An enhaused offering of tangability shall not be this treatise's
;; default, as the nexus to the GERMAN interpreter's response to the
;; pattern stimulation vindicates further expositions.
;; 
;; == THE COMING NOTATIONS FOLLOW COMMON OBJECT-ORIENTED STANDARDS ==
;; Maugre the language's central significance in this implementation,
;; the Common Lisp syntax' diorism shall not adulterate the wont of
;; UML description standards; whence ensues a local forisfamiliation
;; from the realizing warklume towards the acquainted syntactical
;; habits. Several consectaries' accouchements, are capacitated in their
;; extrication from the alow listing:
;; 
;;   (1) HYPHENS ARE ESCHEWED
;;       Common Lisp' consuetude prescribing an compound identifier
;;       name's entreparted words' conjunction via hyphens ("-"),
;;       assigned in popular parlance the agnomination "kebab case",
;;       yields to the more ketly naited distinguishment via majuscules
;;       in the interstices, usually norned "camel case" or "Pascal
;;       "case", depending on the first letter's designment.
;;   
;;   (2) CLASS NAMES IN FUNCTIONS ARE EJECTED
;;       Programming languages cognate in their invocation principles
;;       to UML by their imputation conferring a method's ownership to
;;       the ensconcing classes or objects profit from the consequent
;;       syntactical disambiguating puissance; Common Lisp, on the
;;       other hand, eludes in its ken this proprietorship, equiparating
;;       "methods" and functions as disassociated from classes and
;;       objects in terms of their identifiers. A remedy to the
;;       potential inroad of duplicate function names wones in the
;;       targeted class' installation inwith the operation's
;;       agnomination.
;;       
;;       As a forbisen, a class stevened "Person" that possedes a field
;;       or slot "age", if utilized with an instance "myPerson", in the
;;       programming language Java would emerge as the indagating
;;       invocation
;;       
;;         myPerson.age
;;       
;;       In Common Lisp, a popular wont prescribes
;;       
;;         (person-age my-person)
;;       
;;       The following treatise, and especially the diagrams, shall
;;       comply with the first design.
;; 
;; == THIS IMPLEMENTATION: PROGRAM STATES AND INTERPRETER AS CONTEXT ==
;; In a few aspect's aberrant from its entheus, the implementation at
;; hand wists of an adscititious establishment in the "ProgramContext"
;; interface, rather than an immediate and concrete "Context" class, the
;; aefauld extant subclass' realization relates to the "Interpreter",
;; into whose collaboration the states, defined as subclasses of the
;; abstract "AbstractProgramState", and, ultimately, descendants of the
;; "ProgramState" interface occupying the loftiest echolon, submit.
;; 
;; This project's conformation in its specific form shall be limned by
;; the alow UML class diagram:
;; 
;;   +--------------------------------------------------------+
;;   |                     <<interface>>                      |
;;   |                     ProgramContext                     |
;;   |--------------------------------------------------------|
;;   |--------------------------------------------------------|
;;   | + setCurrentState     (newState : ProgramState) : void |
;;   | + executeCurrentState () : void                        |
;;   +--------------------------------------------------------+
;;                               ^
;;                               |
;;                               | implements
;;                               |
;;   +--------------------------------------------------------+
;;   |                      Interpreter                       |
;;   |--------------------------------------------------------|
;;   | - initialState        : ProgramStartState              |
;;   | - currentState        : ProgramState                   |
;;   | - tape                : Tape                           |
;;   | - hasProgramCompleted : boolean                        |
;;   |--------------------------------------------------------|
;;   | + setCurrentState     (newState : ProgramState) : void |
;;   | + executeCurrentState ()                        : void |
;;   | + getTape             ()                        : Tape |
;;   | + setProgramCompleted (completed : boolean)     : void |
;;   +--------------------------------------------------------+
;;     |                                                  |
;;     |                                                  |
;;     |                                                  V
;;     |                                              +------+
;;     |                                              | Tape |
;;     |                                              |------|
;;     |                                              |------|
;;     |                                              +------+
;;     |
;;     |
;;     V
;;   +-------------------------------------------------------+
;;   |                     <<interface>>                     |
;;   |                     ProgramState                      |
;;   |-------------------------------------------------------|
;;   |-------------------------------------------------------|
;;   | + handle (context : ProgramContext) : void            |
;;   | + getNextState () : ProgramState[0..1]                |
;;   | + setNextState (newState : ProgramState[0..1]) : void |
;;   +-------------------------------------------------------+
;;     ^
;;     |
;;     | implements
;;     |
;;   +-------------------------------------------------------+
;;   |                 AbstractProgramState                  |
;;   |                      {abstract}                       |
;;   |-------------------------------------------------------|
;;   | # nextState : ProgramState[0..1]                      |
;;   |-------------------------------------------------------|
;;   | + getNextState () : ProgramState[0..1]                |
;;   | + setNextState (newState : ProgramState[0..1]) : void |
;;   +-------------------------------------------------------+
;;     ^
;;     |
;;     | extends
;;     |
;;     |    +-----------------------------------------+
;;     +----|            ProgramStartState            |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|             ProgramEndState             |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|             DecrementState              |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|             IncrementState              |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|              MoveLeftState              |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|             MoveRightState              |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|               InputState                |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +-----------------------------------------+
;;     +----|               OutputState               |
;;     |    |-----------------------------------------|
;;     |    |-----------------------------------------|
;;     |    | + handle (context : Interpreter) : void |
;;     |    +-----------------------------------------+
;;     |    
;;     |    +----------------------------------------------------+
;;     +----|                     JumpState                      |
;;          |----------------------------------------------------|
;;          | - destination : JumpState                          |
;;          |----------------------------------------------------|
;;          | + getDestination ()                    : JumpState |
;;          | + setDestination (newDest : JumpState) : void      |
;;          +----------------------------------------------------+
;;            ^
;;            |
;;            | extends
;;            |
;;            |    +-----------------------------------------+
;;            +----|            ForwardJumpState             |
;;            |    |-----------------------------------------|
;;            |    |-----------------------------------------|
;;            |    | + handle (context : Interpreter) : void |
;;            |    +-----------------------------------------+
;;            |    
;;            |    +-----------------------------------------+
;;            +----|              BackJumpState              |
;;                 |-----------------------------------------|
;;                 |-----------------------------------------|
;;                 | + handle (context : Interpreter) : void |
;;                 +-----------------------------------------+
;; 
;; == A PARHEDRAL TELOS: A TABLE WHICH PRODUCES FRESH STATE OBJECTS ==
;; A further ultimity begotten, as an adventive epiphenomenon, relates
;; to the prototype pattern; scilicet, the requisitum involving the
;; copying of extant "pristine" state objects in order to modulate the
;; independent "clones".
;; 
;; == EACH GERMAN INSTRUCTION TOKEN MAPS TO A RESPONSIBLE STATE ==
;; Upon a recognized GERMAN instruction token's detection, the dedicated
;; state representation's selection constitutes a consectaneous
;; necessity. For any specimen from the octuple instruction set exists a
;; "Program-State" implementation whose gnarity permits its respective
;; duty's patration.
;; 
;; == EVERY INSTRUCTION REQUIRES A FRESH STATE ==
;; Concomitant to their Procrustean application of the operative logic,
;; siccan state instances, as a corollary of the concrete program's
;; instruction sequence, requires a bespoke configuration in its
;; connection to the subsequent and, for the case of partaking jump
;; instructions, their opposite destination point. The identical state
;; object, hence, may neither be reused inside of one program, nor, a
;; fortiori, across several segregated parsing courses.
;; 
;; Kete in retaining the benisons of a tabular identifier-state
;; castaldy, a solution to this predicament is extricated from the
;; request of a commensurately neutral instance of the respective
;; state implementation, whose copy, without remaining vincula to the
;; template object, constitutes the delivered response to the inquirying
;; side. The foundational desigment is nevened in the popular literature
;; as the "prototype design pattern".
;; 
;; == THE PROTOTYPE DESIGN PATTERN: CLASSES FURNISH THEIR OWN CLONES ==
;; The "prototype" design pattern's realm is comprised of the creational
;; specimens, thilk apply themselves to the predicaments involved with
;; the origination of objects; which in the case of this particular
;; forbisen consigns a fresh instance's gendrure, derived from an extant
;; member, to the class' own labor, expecting a "clone()" method's
;; furnishment, whence a partially or commensurately independent
;; instance of the specific type is begotten.
;; 
;; The following UML class diagram shall accoutre the reader with an
;; illustration the same limns both the involved participants and their
;; relational vincula:
;; 
;;   +------------------------+           +----------------------+
;;   |     <<interface>>      |<----------|        Client        |
;;   |       Prototype        |           |----------------------|
;;   |------------------------|           |----------------------|
;;   |------------------------|           | # operation () : any |
;;   | + clone () : Prototype |           +----------------------+
;;   +------------------------+
;;               ^
;;               |
;;               | implements
;;               |
;;               |    +---------------------------------+
;;               +----|       ConcretePrototype1        |
;;               |    |---------------------------------|
;;               |    |---------------------------------|
;;               |    | + clone () : ConcretePrototype1 |
;;               |    +---------------------------------+
;;               |
;;               |
;;               |    +---------------------------------+
;;               +----|       ConcretePrototype2        |
;;                    |---------------------------------|
;;                    |---------------------------------|
;;                    | + clone () : ConcretePrototype2 |
;;                    +---------------------------------+
;; 
;; == THE SOLUTION: QUERY A PLAIN TEMPLATE STATE, DEPLOY ITS CLONE ==
;; These formalized notions' reification for our project are molded into
;; a hash table, edified upon an octuple componency which concurs in
;; being equinumerant with the instruction set's cardinality, and whose
;; keys bear the GERMAN identifier names, each committed to a champarty
;; with a template instance begotten from a class implementing the
;; "Program-State".
;; 
;; Upon the connable language identifier's recognition, the quesited
;; state instance yields a fresh copy of the pristine forbisen, its
;; obtention resulting from the invocation of the class' "clone-state"
;; method.
;; 
;; == COMMON LISP HOMOLOGATES A SIMPLIFIED CLONING OF SUBCLASSES ==
;; A remark aboon an apostille as a deviation from the basic concept,
;; this implementation fails in its lealty to the idea of each prototype
;; subclass' personal investment in the "clone()" or "clone-state"
;; method's definition. Norned as this discrepancy's encheson is Common
;; Lisp's dioristic capacitation in instantiating objects of classes by
;; their symbol class identifier's adminiculum; in a diction entalented
;; with concreteness and compendiousness, an autochthonous function
;; 
;;   (make-instance {class-name} {options})
;; 
;; enumerates among the language's faculties, which, furnished with a
;; symbol specifying the {class-name}, and an optional list of the
;; slots' name-value pairs, {options}, yields a thus assembled instance.
;; Proffered as a forbisen, for a given class "Increment-State", the
;; summons of
;; 
;;   (make-instance 'Increment-State)
;; 
;; limns an equivalency to a fictitious Java constructor
;; 
;;   new IncrementState ()
;; 
;; The thus referenced class' existency during the compilation phase
;; abstains from imposing a requisitum; merely the runtime object
;; creation request ought to be construed as a mandate.
;; 
;; A corollary of this elevating potential, the generic function
;; "clone-state" experiences its aefauld implementation, or, in Common
;; Lisp's terminology, its "specialization", on the "Program-State"
;; interface, thilk, upon the concrete state instance's reception,
;; requests the desideratum's class, ere assembling a fresh copy.
;; The abstractness of the tongue's disquisition shall enjoy a more
;; relatable expression in the actual code tmema, desumed immediately
;; from this source file's viscerals:
;; 
;;   (defmethod clone-state ((state Program-State))
;;     (declare (type Program-State state))
;;     (the Program-State
;;       (make-instance
;;         (class-of state))))
;; 
;; == THE STATE MACHINE BUILDER CONNECTS THE STATE OBJECTS ==
;; The exercise's adhibition that produces from the GERMAN tokens a
;; state machine is consigned to a specifically invented
;; "State-Machine-Builder" agent's investments, inwith whose bailiwick
;; is included the instruction identifiers' obtention from a lexical
;; analyzer (lexer), their supersession by "Program-State" objects, and
;; their linkage.
;; 
;; In the special case of forward and back jump behests, the
;; traditionally linear catena's designment elevates to a parergal
;; reference establishment: Each jumelle of matching jump points enjoys
;; its moeities' conjoinment by an adjectitious, bidirectionally
;; composed ligation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-08-16
;; 
;; Sources:
;;   [esolang2023GERMAN]
;;   The Esolang contributors, "GERMAN", June 29th, 2023
;;   URL: "https://esolangs.org/wiki/GERMAN"
;;   
;;   [gofpatterns2025prototypepattern]
;;   GofPatterns, "Prototype Pattern (Create customized Objects)", 2025
;;   URL: "https://www.gofpattern.com/creational/patterns/
;;         prototype-pattern.php"
;;   Notes:
;;     - Elucidates the "prototype" design pattern.
;;   
;;   [gofpatterns2025statepattern]
;;   GofPatterns, "State Pattern (Localize state-specific behavior)",
;;     2025
;;   URL: "https://www.gofpattern.com/behavioral/patterns/
;;         state-pattern.php"
;;   Notes:
;;     - Elucidates the "state" design pattern.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type (type-name
                               (candidate-name &rest lambda-list)
                               &body body)
  "Defines a derived type whose provenance of the agnomination's
   appropration constitutes the CANDIDATE-NAME's dation, while its
   formal parameters derive from the LAMBDA-LIST, and which probes its
   subject via the stevening established in the CANDIDATE-NAME,
   evaluating the BODY forms, and construing the desinent form's primary
   result as the docimasy's conclusion, with a generalized boolean value
   of \"true\" tantamount to the candidate's compliance with the type's
   stipulations, and a \"false\" response its incompatibility's
   apprizal.
   ---
   If the first BODY form resolves to a string, thilk its administered
   the interpretation as the type definition's documentation string,
   and is consequently appropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
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

(define-bespoke-type hash-table-of (candidate
                                    &optional (key-type   '*)
                                              (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   lays its amplection around zero or more entries, for everichon from
   these is specified the KEY-TYPE as the key's member, while the
   VALUE-TYPE imposes a species for the affiliated value, both utilizing
   the generic sentinel ``*'' as the default."
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

(deftype state-table ()
  "The ``state-table'' type defines a mapping betwixt a GERMAN operation
   identifier and a representative program state, its manifestation that
   of a hash table, the same allies the name strings to
   ``Program-State'' instances."
  '(hash-table-of simple-base-string Program-State))

;;; -------------------------------------------------------

(define-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list whose conformation stipulates
   zero or more members, each such complying to the ELEMENT-TYPE, for
   which is defined the generic sentinel ``*'' as the default."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte object, enumerating in
   its componency a sequence of eight accolent bits, and thus producing
   an occupant of the closed integral interval [0, 255].
   ---
   The ``octet'' type in its paravaunt duty participates in the memory
   tape's context, ligated to the onus of a cell state's delineation."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype encoding-table ()
  "The ``encoding-table'' type defines a mapping from the space of
   brainfuck instruction identifiers to the equivalent GERMAN operation
   names, molded into a hash table's guise, the same allies the octuple
   standard characters representing the brainfuck moeity to simple base
   strings nevening the GERMAN tantamounts."
  '(hash-table-of standard-char simple-base-string))

;;; -------------------------------------------------------

(deftype decoding-table ()
  "The ``decoding-table'' type defines a mapping from the space of
   GERMAN instruction identifiers to the equivalent brainfuck operation
   symbols, molded into a hash table's guise, the same allies the
   octuple simple base strings representing the GERMAN moiety to the
   standard characters nevening the brainfuck tantamounts."
  '(hash-table-of simple-base-string standard-char))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of which amplects, among other specimens, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Booleant tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate
        '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-simple-base-string (source)
  "Creates and returns a simple base string representation of the SOURCE
   string."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))

;;; -------------------------------------------------------

(defun convert-to-simple-string (source)
  "Creates and returns a simple string representation of the SOURCE
   string."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun locate-start-of-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, returns
   the index of the nearest word's first character, or, upon its
   absence, responds with the SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE,
   locates and returns the index immediately succeeding the end of the
   nearest following word."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-bournes-of-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, locates
   the nearest following word and returns two values:
     (1) The index of the detected word's first character, or, upon its
         absence, the SOURCE's length.
     (2) The index immediately succeeding the detected word's desinent
         character, or, upon its absence, the SOURCE's length.
   ---
   Please heed that a conflation of the first and second return values
   is unambiguously capacitated to identify the absence of a next word
   in the SOURCE."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the (values fixnum fixnum)
    (let ((word-start (locate-start-of-next-word source start)))
      (declare (type fixnum word-start))
      (values
        word-start
        (locate-end-of-next-word source word-start)))))

;;; -------------------------------------------------------

(defun string-is-empty-p (subject)
  "Determines whether the SUBJECT represents an empty string,
   enumerating exactly zero characters, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string subject))
  (the boolean
    (get-boolean-value-of
      (zerop
        (length subject)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Lexer" class.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          simple-string
    :documentation "The piece of GERMAN source code to segregate into
                    its words.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE."))
  (:documentation
    "The ``Lexer'' class provides a lexical analyzer, the same enjoys
     the capacitation of a piece of GERMAN source code's segregation
     into its words, or tokens, and their concomitant delivery upon
     requests."))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a fresh ``Lexer'' whose analyzation's provenance
   is desumed from the SOURCE, experiencing a prevenience which converts
   thilk into a simple string for internal employment."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer
      :source (convert-to-simple-string source))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next word from the LEXER.
   ---
   Upon its source's exhaustion the LEXER responds to any request with
   a fresh empty string."
  (declare (type Lexer lexer))
  (the simple-string
    (with-slots (source position) lexer
      (declare (type simple-string source))
      (declare (type fixnum        position))
      (multiple-value-bind (word-start word-end)
          (locate-bournes-of-next-word source position)
        (declare (type fixnum word-start))
        (declare (type fixnum word-end))
        (prog1
          (convert-to-simple-string
            (subseq source word-start word-end))
          (setf position word-end))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of "Program-Context" interface.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-Context ()
  ()
  (:documentation
    "The ``Program-Context'' interface furnishes the firmament upon
     which are edified the basic and common services appertaining to
     all state machine context species."))

;;; -------------------------------------------------------

(defgeneric set-current-context-state (context new-state)
  (:documentation
    "Sets the program CONTEXT's active state to the NEW-STATE and
     returns no value.
     ---
     The ``NIL'' value as the NEW-STATE constitutes a homologated use
     case."))

;;; -------------------------------------------------------

(defgeneric execute-current-context-state (context)
  (:documentation
    "Executes the causata associated with the program CONTEXT's active
     state and returns no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of "Program-State" interface.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-State ()
  ()
  (:documentation
    "The ``Program-State'' interface accountres the substratum upon
     which all concrete classes shall be edified that optate to
     represent GERMAN operations in a state's guise."))

;;; -------------------------------------------------------

(defgeneric handle-state-action (state context)
  (:documentation
    "Accompasses the causata affiliated with the program STATE in the
     specific CONTEXT and returns no value."))

;;; -------------------------------------------------------

(defgeneric get-next-state (state)
  (:documentation
    "Returns the state reached by actuating an egress from the given
     STATE."))

;;; -------------------------------------------------------

(defgeneric set-next-state (state new-next-state)
  (:documentation
    "Sets the program STATE's succeessor to the NEW-NEXT-STATE and
     returns no value."))

;;; -------------------------------------------------------

(defgeneric clone-state (state)
  (:documentation
    "Creates and returns a shallow copy of the specified STATE."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of "Abstract-Program-State" abstract class.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Abstract-Program-State (Program-State)
  ((next-state
    :initarg       :next-state
    :initform      NIL
    :type          (or null Program-State)
    :documentation "The optional successor state to this one."))
  (:documentation
    "The ``Abstract-Program-State'' abstract class applies itself to a
     parergon's furnishment in its implementation of the commonly
     necessary slots and methods for the ``Program-State'' interface
     covenants' attendance."))

;;; -------------------------------------------------------

(defmethod get-next-state ((state Program-State))
  (declare (type Program-State state))
  (the (or null Program-State)
    (slot-value state 'next-state)))

;;; -------------------------------------------------------

(defmethod set-next-state ((state          Program-State)
                           (new-next-state Program-State))
  (declare (type Program-State state))
  (declare (type Program-State new-next-state))
  (setf (slot-value state 'next-state) new-next-state)
  (values))

;;; -------------------------------------------------------

(defmethod clone-state ((state Program-State))
  (declare (type Program-State state))
  (the Program-State
    (make-instance
      (class-of state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program state classes.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-Start-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Program-Start-State'' class serves in the modeling of a
     program state within whose bailiwick enhalses the dever forinsecal
     and adscititious to the GERMAN programming language's instruction
     set, yet of managerial poderance, of establishing the parsed
     program's initial state, whence all other components ensue."))

;;; -------------------------------------------------------

(defclass Program-End-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Program-Start-State'' class serves in the modeling of a
     program state within whose bailiwick enhalses the dever forinsecal
     and adscititious to the GERMAN programming language's instruction
     set, yet of managerial poderance, of establishing the parsed
     program's terminal state, marking the operation sequence's
     conclusion."))

;;; -------------------------------------------------------

(defclass Move-Left-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Move-Left-State'' class serves in the modeling of a program
     state within whose bailiwick enhalses the dever of the memory
     tape cell pointer's sinistral translation."))

;;; -------------------------------------------------------

(defclass Move-Right-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Move-Right-State'' class serves in the modeling of a program
     state within whose bailiwick enhalses the dever of the memory
     tape cell pointer's dextral translation."))

;;; -------------------------------------------------------

(defclass Increment-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Increment-State'' class serves in the modeling of a program
     state within whose bailiwick enhalses the dever of the memory
     tape's current cell value's incrementation."))

;;; -------------------------------------------------------

(defclass Decrement-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Decrement-State'' class serves in the modeling of a program
     state within whose bailiwick enhalses the dever of the memory
     tape's current cell value's decrementation."))

;;; -------------------------------------------------------

(defclass Input-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Input-State'' class serves in the modeling of a program
     state within whose bailiwick enhalses the dever of character input
     reception."))

;;; -------------------------------------------------------

(defclass Output-State (Abstract-Program-State)
  ()
  (:documentation
    "The ``Output-State'' class serves in the modeling of a program
     state within whose bailiwick enhalses the dever of the memory
     tape's current cell 's output in its character form."))

;;; -------------------------------------------------------

(defclass Jump-State (Abstract-Program-State)
  ((destination
    :initarg       :destination
    :initform      NIL
    :accessor      jump-state-destination
    :type          (or null Jump-State)
    :documentation "The program state reached if skipping the
                    intermediately positioned instructions."))
  (:documentation
    "The ``Jump-State'' abstract class furnishes an extension of the
     ``Program-State'' notion in its aspect as the forward and back
     jump operations' foundation, including in its diorism a slot and
     an accessor which concur in their definition of the state reached
     by skipping a jump point towards its connected compernage."))

;;; -------------------------------------------------------

(defclass Forward-Jump-State (Jump-State)
  ()
  (:documentation
    "The ``Forward-Jump-State'' class serves in the modeling of a
     program state within whose bailiwick enhalses the dever involving
     a conditional forward jumping program navigation."))

;;; -------------------------------------------------------

(defclass Back-Jump-State (Jump-State)
  ()
  (:documentation
    "The ``Back-Jump-State'' class serves in the modeling of a
     program state within whose bailiwick enhalses the dever involving
     a conditional back jumping program navigation."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of state table.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type state-table +STATE-TABLE+))

;;; -------------------------------------------------------

(defparameter +STATE-TABLE+
  (make-hash-table :test #'equal :size 8)
  "Associates with the recognized GERMAN identifier names pristine
   ``Program-State'' instances thilk define the respective operations'
   molding into a state design, intended for their copying, the clone
   constituting the ultimate object to administer configurations for the
   assembled program's purposes.")

;;; -------------------------------------------------------

(defun define-identifier (name state-class)
  "Creates a fresh instance of the ``Program-State'' subclass signified
   by symbolic STATE-CLASS, associates thilk with the GERMAN identifier
   NAME in the ``+STATE-TABLE+'', and returns no value."
  (declare (type simple-string name))
  (declare (type symbol        state-class))
  (setf
    (gethash
      (convert-to-simple-base-string name)
      +STATE-TABLE+)
    (make-instance state-class))
  (values))

;;; -------------------------------------------------------

(define-identifier "LINKS"           'Move-Left-State)
(define-identifier "RECHTS"          'Move-Right-State)
(define-identifier "ADDITION"        'Increment-State)
(define-identifier "SUBTRAKTION"     'Decrement-State)
(define-identifier "EINGABE"         'Input-State)
(define-identifier "AUSGABE"         'Output-State)
(define-identifier "SCHLEIFENANFANG" 'Forward-Jump-State)
(define-identifier "SCHLEIFENENDE"   'Back-Jump-State)

;;; -------------------------------------------------------

(defun identifier-name-p (candidate)
  "Determines whether the CANDIDATE represents a valid GERMAN identifier
   name, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash candidate +STATE-TABLE+)))))

;;; -------------------------------------------------------

(defun get-state-for-name (name)
  "Returns a fresh instance of the program state corresponding to the
   identifier NAME; or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type string name))
  (let ((state (gethash name +STATE-TABLE+)))
    (declare (type (or null Program-State) state))
    (the Program-State
      (if state
        (clone-state state)
        (error "No state is associated with the identifier ~s."
          name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "State-Machine-Builder" class.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass State-Machine-Builder ()
  ((initial-state
    :initform      NIL
    :type          (or null Program-Start-State)
    :documentation "The initial state, which always assumes the guise of
                    a ``Program-Start-State''.")
   (previous-state
    :initform      NIL
    :type          (or null Program-State)
    :documentation "The preveniently created program state.")
   (forward-jump-states
    :initform      NIL
    :type          (list-of Program-State)
    :documentation "A stack dedicated to the castaldy of the encountered
                    forward jump points, each such represented by a
                    ``Forward-Jump-State'' object, and intended for its
                    ligation with a matching ``Back-Jump-State''."))
  (:documentation
    "The ``State-Machine-Builder'' class is apportioned the wike of
     assembling from a sequence of GERMAN program's tokens a state
     machine representative of the provenance's purposes.
     ---
     The resulting state machine, even if a consequence of an adhibition
     to a program destitute of any operative content, commences in an
     initial state, complying to the ``Program-Start-State'' species,
     and terminates in a ``Program-End-State'' instance, the former
     among this sentinel twissel establishes the builder application's
     result, homologating a traversal from its initial to the desinent
     state, while the capacitation's compass also embraces the
     ramosity contributed by the forward and back jump states.
     ---
     The connection logic underlying the state linkages ensues from the
     implementations of the generic function ``connect-states'', thilk
     in its specialization on jump states begets the non-linear goto
     facilities."))

;;; -------------------------------------------------------

(defgeneric connect-states (builder previous-state current-state)
  (:documentation
    "Establishes a vinculum from the PREVIOUS-STATE to the
     CURRENT-STATE, contingently employing the state machine BUILDER's
     services for the sake of the jump states' conjoinment, and returns
     no value."))

;;; -------------------------------------------------------

(defmethod connect-states ((builder        State-Machine-Builder)
                           (previous-state Program-State)
                           (current-state  Program-State))
  "Connects the PREVIOUS-STATE to the CURRENT-STATE, assigning no
   effective value to the state machine BUILDER's participiation, and
   returns no value."
  (declare (type State-Machine-Builder builder)
           (ignore                     builder))
  (declare (type Program-State         previous-state))
  (declare (type Program-State         current-state))
  ;; Connect the prevenient to the current state.
  (set-next-state previous-state current-state)
  (values))

;;; -------------------------------------------------------

(defmethod connect-states ((builder        State-Machine-Builder)
                           (previous-state Program-State)
                           (current-state  Forward-Jump-State))
  
  "Connects the PREVIOUS-STATE to the CURRENT-STATE, pushes the latter,
   this constituting a forward jump point, in the state machine BUILDER
   for latter bilateral vincula's establishment to the matching back
   jump state, and returns no value."
  (declare (type State-Machine-Builder builder))
  (declare (type Program-State         previous-state))
  (declare (type Forward-Jump-State    current-state))
  ;; Connect the prevenient to the current state.
  (set-next-state previous-state current-state)
  ;; Memorize this forward jump state for a future matching with a
  ;; back jump state.
  (with-slots (forward-jump-states) builder
    (declare (type (list-of Program-State) forward-jump-states))
    (push current-state forward-jump-states))
  (values))

;;; -------------------------------------------------------

(defmethod connect-states ((builder        State-Machine-Builder)
                           (previous-state Program-State)
                           (current-state  Back-Jump-State))
  "Connects the PREVIOUS-STATE to the CURRENT-STATE, stores a bilateral
   reference betwixt the latter, this constituting a back jump point,
   to the matching forward jump point expected to have been encountered
   in a prevenient stage and memorized in the state machine BUILDEER,
   and returns no value."
  (declare (type State-Machine-Builder builder))
  (declare (type Program-State         previous-state))
  (declare (type Back-Jump-State       current-state))
  ;; Connect the prevenient to the current state.
  (set-next-state previous-state current-state)
  ;; Connect the matching forward and back jump states.
  (with-slots (forward-jump-states) builder
    (declare (type (list-of Program-State) forward-jump-states))
    (if forward-jump-states
      (let ((forward-jump-state (pop forward-jump-states)))
        (declare (type Forward-Jump-State forward-jump-state))
        (psetf
          (jump-state-destination forward-jump-state)
            current-state
          (jump-state-destination current-state)
            forward-jump-state))
      (error "Unmatched back jump point.")))
  (values))

;;; -------------------------------------------------------

(defun prepare-initial-state (builder)
  "Prepares a ``Program-Start-State'' as the state BUILDER's initial
   one, sets the previous state to the same, and returns no value."
  (declare (type State-Machine-Builder builder))
  (with-slots (initial-state previous-state) builder
    (declare (type (or null Program-Start-State) initial-state))
    (declare (type (or null Program-State)       previous-state))
    (setf initial-state  (make-instance 'Program-Start-State))
    (setf previous-state initial-state))
  (values))

;;; -------------------------------------------------------

(defun clear-forward-jump-states (builder)
  "Removes all forward jump points from the state BUILDER's stack and
   returns no value."
  (declare (type State-Machine-Builder builder))
  (setf (slot-value builder 'forward-jump-states) NIL)
  (values))

;;; -------------------------------------------------------

(defun reset-state-machine-builder (builder)
  "Resets all state information governed by the state machine BUILDER's
   purview and returns no value."
  (declare (type State-Machine-Builder builder))
  (prepare-initial-state     builder)
  (clear-forward-jump-states builder)
  (values))

;;; -------------------------------------------------------

(defun build-state-machine (builder lexer)
  "Constructs a fresh state machine by the state machine BUILDER's
   adminiculum, receiving its requisite words from the LEXER, and
   returns the initial ``Program-State'' instance whence originates the
   entire machine's architecture."
  (declare (type State-Machine-Builder builder))
  (declare (type Lexer                 lexer))
  (with-slots (initial-state previous-state) builder
    (declare (type (or null Program-Start-State) initial-state))
    (declare (type (or null Program-State)       previous-state))
    
    (loop
      initially
        (reset-state-machine-builder builder)
      
      for current-token
        of-type string
        =       (get-next-token lexer)
      
      until (string-is-empty-p current-token)
      
      when (identifier-name-p current-token) do
        (let ((current-state (get-state-for-name current-token)))
          (declare (type Program-State current-state))
          
          (connect-states builder previous-state current-state)
          
          (setf previous-state current-state))
      
      finally
        (connect-states builder previous-state
          (make-instance 'Program-End-State)))
    
    (the Program-Start-State initial-state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Tape" class.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :accessor      tape-cells
    :type          (hash-table-of integer octet)
    :documentation "Associates with the signed integer-valued cell
                    indices the unsigned byte-valued cell states.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          integer
    :documentation "The cell pointer, realized as the currently selected
                    cell's key into the CELLS table."))
  (:documentation
    "The ``Tape'' class applies itself to a GERMAN program data's
     castaldy, its edification begotten from the firmament of a
     bidirectionally infinite dispansion of unsigned byte-valued cells,
     operating upon whose entirety remains a mobile cell pointer, thilk
     assumes the diorism of a cursor serving to select at any instant
     the currently amenable unit for perquisitions and modulations.
     ---
     This tape implementation manifests in a sparse vector of octets,
     the warklume of this operative capacitation a hash table which
     assigns to its signed integer-valued keys, the provide the cell
     indices, unsigned byte values as an entry's athwart moeity,
     accomplishing the cell states' modelling.
     ---
     Woning inside of this designment, the cell pointer merely ostends
     a resolution to a signed integer number, addressing the current
     cell value's index as the equiparating key into the hash table."))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value representing the TAPE's currently
   selected cell's state."
  (declare (type Tape tape))
  (the octet
    (gethash
      (tape-pointer tape)
      (tape-cells   tape)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by its state's wrapping into the admissible
   unsigned byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (gethash
      (tape-pointer tape)
      (tape-cells   tape)
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Interpreter" class.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter (Program-Context)
  ((initial-state
    :initarg       :initial-state
    :initform      (error "Missing program state machine.")
    :type          Program-Start-State
    :documentation "The parsed GERMAN program represented by its state
                    machine's initial state.")
   (current-state
    :type          Program-State
    :documentation "The currently active program state.")
   (tape
    :initform      (make-instance 'Tape)
    :type          Tape
    :documentation "The program's memory as a bilaterally infinite tape
                    of unsigned byte-valued cells.")
   (program-has-completed-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the program
                    has been processed in its entirety, this being
                    consonant with the desinent ``Program-End-State''
                    object's processing."))
  (:documentation
    "The ``Interpreter'' class implements a state machine context whose
     telos' diorism resolves to the accompassing of actual efficacy to
     a GERMAN program communicated in the form of states as responsible
     units for the operations' execution."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the INTERPRETER's current state to the initial one and
   returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'current-state)
    (slot-value interpreter 'initial-state))
  (values))

;;; -------------------------------------------------------

(defmethod set-current-context-state ((interpreter Interpreter)
                                      (new-state   Program-State))
  (declare (type Interpreter   interpreter))
  (declare (type Program-State new-state))
  (setf (slot-value interpreter 'current-state) new-state)
  (values))

;;; -------------------------------------------------------

(defmethod execute-current-context-state ((interpreter Interpreter))
  (declare (type Interpreter interpreter))
  (handle-state-action
    (slot-value interpreter 'current-state)
    interpreter)
  (values))

;;; -------------------------------------------------------

(defun get-tape (interpreter)
  "Returns the tape employed by the INTERPRETER as its program memory."
  (declare (type Interpreter interpreter))
  (the Tape
    (slot-value interpreter 'tape)))

;;; -------------------------------------------------------

(defun set-program-has-completed (interpreter has-completed-p)
  "Adjusts the INTERPRETER's program completion status, thilk determines
   whether its instruction sequence is exhausted, to the Boolean flag
   HAS-COMPLETED-P and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type boolean     has-completed-p))
  (setf (slot-value interpreter 'program-has-completed-p)
        has-completed-p)
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the GERMAN program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (slot-value interpreter 'program-has-completed-p) do
    (execute-current-context-state interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-german (code)
  "Interprets the piece of GERMAN source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-instance 'Interpreter :initial-state
      (build-state-machine
        (make-instance 'State-Machine-Builder)
        (make-lexer code))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program state actions.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-state-action ((state   Program-Start-State)
                                (context Interpreter))
  (declare (type Program-Start-State state))
  (declare (type Interpreter         context))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Program-End-State)
                                (context Interpreter))
  (declare (type Program-End-State state))
  (declare (type Interpreter       context))
  (set-program-has-completed context T)
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Move-Left-State)
                                (context Interpreter))
  (declare (type Move-Left-State state))
  (declare (type Interpreter     context))
  (move-cell-pointer-left
    (get-tape context))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Move-Right-State)
                                (context Interpreter))
  (declare (type Move-Right-State state))
  (declare (type Interpreter      context))
  (move-cell-pointer-right
    (get-tape context))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Increment-State)
                                (context Interpreter))
  (declare (type Increment-State state))
  (declare (type Interpreter     context))
  (incf
    (current-cell-value
      (get-tape context)))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Decrement-State)
                                (context Interpreter))
  (declare (type Decrement-State state))
  (declare (type Interpreter     context))
  (decf
    (current-cell-value
      (get-tape context)))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Input-State)
                                (context Interpreter))
  (declare (type Input-State state))
  (declare (type Interpreter context))
  (let ((tape (get-tape context)))
    (declare (type Tape tape))
    (format T "~&>> ")
    (finish-output)
    (setf (current-cell-value tape)
      (char-code
        (read-char NIL NIL #\Null)))
    (clear-input))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Output-State)
                                (context Interpreter))
  (declare (type Output-State state))
  (declare (type Interpreter  context))
  (format T "~c"
    (code-char
      (current-cell-value
        (get-tape context))))
  (set-current-context-state context
    (get-next-state state))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Forward-Jump-State)
                                (context Interpreter))
  (declare (type Forward-Jump-State state))
  (declare (type Interpreter        context))
  (let ((tape (get-tape context)))
    (declare (type Tape tape))
    (set-current-context-state context
      (if (zerop (current-cell-value tape))
        (jump-state-destination state)
        (get-next-state         state))))
  (values))

;;; -------------------------------------------------------

(defmethod handle-state-action ((state   Back-Jump-State)
                                (context Interpreter))
  (declare (type Back-Jump-State state))
  (declare (type Interpreter     context))
  (let ((tape (get-tape context)))
    (declare (type Tape tape))
    (set-current-context-state context
      (if (zerop (current-cell-value tape))
        (get-next-state         state)
        (jump-state-destination state))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of translation tables.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type encoding-table +ENCODING-TABLE+))
(declaim (type decoding-table +DECODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +ENCODING-TABLE+
  (make-hash-table :test #'eql :size 8)
  "Associates with the recognized brainfuck instruction symbols the
   corresponding GERMAN operation identifier names.")

(defparameter +DECODING-TABLE+
  (make-hash-table :test #'equal :size 8)
  "Associates with the recognized GERMAN operation identifier names the
   corresponding brainfuck instruction symbols.")

;;; -------------------------------------------------------

(defun define-translation (german-token brainfuck-symbol)
  "Associates the BRAINFUCK-TOKEN with the GERMAN-TOKEN in a
   bidirectional manner, assigning the affiliations to both the
   ``+ENCODING-TABLE+'' and the ``+DECODING-TABLE+'', and returns no
   value.
   ---
   Any entry in these table amenable to a key of either component will
   be tacitly superseded."
  (declare (type simple-string german-token))
  (declare (type standard-char brainfuck-symbol))
  (let ((prepared-german-token
          (convert-to-simple-base-string german-token)))
    (declare (type simple-base-string german-token))
    (psetf
      (gethash brainfuck-symbol +ENCODING-TABLE+)
        prepared-german-token
      (gethash prepared-german-token +DECODING-TABLE+)
        brainfuck-symbol))
  (values))

;;; -------------------------------------------------------

(define-translation "LINKS"           #\<)
(define-translation "RECHTS"          #\>)
(define-translation "ADDITION"        #\+)
(define-translation "SUBTRAKTION"     #\-)
(define-translation "EINGABE"         #\,)
(define-translation "AUSGABE"         #\.)
(define-translation "SCHLEIFENANFANG" #\[)
(define-translation "SCHLEIFENENDE"   #\])

;;; -------------------------------------------------------

(defun brainfuck-symbol-p (candidate)
  "Determines whether the CANDIDATE is associated with an operative
   purpose in the brainfuck programming language, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash candidate +ENCODING-TABLE+)))))

;;; -------------------------------------------------------

(defun translate-brainfuck-symbol (brainfuck-symbol)
  "Returns for the BRAINFUCK-SYMBOL the corresponding GERMAN token; or,
   upon its disrespondency, signals an error of an unspecified type."
  (declare (type standard-char brainfuck-symbol))
  (the simple-base-string
    (or (gethash brainfuck-symbol +ENCODING-TABLE+)
        (error "Unrecognized brainfuck symbol: ~s." brainfuck-symbol))))

;;; -------------------------------------------------------

(defun german-identifier-p (candidate)
  "Determines whether the CANDIDATE represents an operation identifier
   in the GERMAN programming language, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string candidate))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash candidate +DECODING-TABLE+)))))

;;; -------------------------------------------------------

(defun translate-german-token (german-token)
  "Returns for the GERMAN-TOKEN the corresponding brainfuck symbol; or,
   upon its disrespondency, signals an error of an unspecified type."
  (declare (type simple-string german-token))
  (the standard-char
    (or (gethash german-token +DECODING-TABLE+)
        (error "Unrecognized GERMAN token: ~s." german-token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from brainfuck to GERMAN.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-german (brainfuck-code
                                      &key (destination NIL))
  "Generates for the piece of BRAINFUCK-CODE an equivalent GERMAN
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for current-brainfuck-character
          of-type character
          across   brainfuck-code
        when (brainfuck-symbol-p current-brainfuck-character) do
          (format destination "~&~a"
            (translate-brainfuck-symbol current-brainfuck-character)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-brainfuck-to-german brainfuck-code
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from GERMAN to brainfuck.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-german-to-brainfuck (german-code
                                      &key (destination NIL))
  "Generates for the piece of GERMAN-CODE an equivalent brainfuck
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the output."
  (declare (type string      german-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((lexer (make-lexer german-code)))
        (declare (type Lexer lexer))
        (loop
          for current-german-token
            of-type simple-string
            =       (get-next-token lexer)
          until
            (string-is-empty-p current-german-token)
          when (german-identifier-p current-german-token) do
            (format destination "~c"
              (translate-german-token current-german-token))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-german-to-brainfuck german-code
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-german
  "EINGABE SCHLEIFENANFANG AUSGABE EINGABE SCHLEIFENENDE")

;;; -------------------------------------------------------

;; Translate a repeating cat program from brainfuck to its GERMAN
;; equivalency and execute thilk.
(interpret-german
  (translate-brainfuck-to-german ",[.,]"))

;;; -------------------------------------------------------

;; Translate the repeating cat prgoram from GERMAN to brainfuck,
;; yielding the piece of code
;;   ,[.,]
(translate-german-to-brainfuck
  "EINGABE SCHLEIFENANFANG AUSGABE EINGABE SCHLEIFENENDE")

;;; -------------------------------------------------------

;; Print the message "Hello, World!" to the standard output conduit.
(interpret-german
  "
  ADDITION
  SCHLEIFENANFANG
  SUBTRAKTION
  SUBTRAKTION
  RECHTS
  SUBTRAKTION
  SCHLEIFENANFANG
  RECHTS
  RECHTS
  ADDITION
  RECHTS
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  LINKS
  LINKS
  SCHLEIFENENDE
  LINKS
  SUBTRAKTION
  SUBTRAKTION
  LINKS
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  SCHLEIFENENDE
  RECHTS
  SUBTRAKTION
  AUSGABE
  RECHTS
  RECHTS
  RECHTS
  ADDITION
  AUSGABE
  RECHTS
  RECHTS
  AUSGABE
  AUSGABE
  ADDITION
  ADDITION
  ADDITION
  SCHLEIFENANFANG
  AUSGABE
  RECHTS
  SCHLEIFENENDE
  LINKS
  LINKS
  LINKS
  LINKS
  AUSGABE
  ADDITION
  ADDITION
  ADDITION
  AUSGABE
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  SUBTRAKTION
  AUSGABE
  LINKS
  LINKS
  SUBTRAKTION
  AUSGABE
  RECHTS
  RECHTS
  RECHTS
  RECHTS
  ADDITION
  AUSGABE
  ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-german
  "
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  RECHTS
  RECHTS
  EINGABE
  LINKS
  LINKS
  SCHLEIFENANFANG
  SUBTRAKTION
  RECHTS
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  LINKS
  SCHLEIFENENDE
  RECHTS
  ADDITION
  ADDITION
  ADDITION
  SCHLEIFENANFANG
  RECHTS
  SUBTRAKTION
  LINKS
  SUBTRAKTION
  SCHLEIFENENDE
  RECHTS
  RECHTS
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  SCHLEIFENANFANG
  RECHTS
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  ADDITION
  LINKS
  SUBTRAKTION
  SCHLEIFENENDE
  RECHTS
  SUBTRAKTION
  LINKS
  LINKS
  SCHLEIFENANFANG
  RECHTS
  RECHTS
  AUSGABE
  LINKS
  LINKS
  SCHLEIFENENDE
  RECHTS
  RECHTS
  SUBTRAKTION
  AUSGABE
  ")
