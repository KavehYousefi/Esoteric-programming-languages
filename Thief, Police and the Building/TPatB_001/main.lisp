;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Thief, Police and the Building", invented by the Esolang
;; user "Username1234" and presented on August 23rd, 2022, the subject
;; of which appertains to the simulation of a thief's larceny actuated
;; inside of a building, in particular, his traversal along the
;; premise's floor by an elevator's or stair room's adminicle, and his
;; intrusion and robbery of rooms, each such comprehends a single
;; character, which, ensuing from the police's arrival, are concatenated
;; into a single message and printed to the standard output.
;; 
;; 
;; Concept
;; =======
;; The "Thief, Police and the Building" programming language simulates
;; a thief operating in a building composed of one or more floors,
;; across which he navigates by aide of an elevator and a stair room,
;; gleaning in his bag characters from rooms, which, after the police's
;; arrival, are printed in the correct order to the standard output
;; conduit.
;; 
;; == THE THIEF MOVES IN A BUILDING ==
;; The thief's activities' simulation is founded upon the constraints of
;; the building inwith that the crime is perpetrated.
;; 
;; The premise's general conformation shall be the following adduction's
;; product:
;; 
;;                   ___
;;              _____| |__
;;             /          \
;;            /            \
;;           /______________\
;;   top     |              |   ^
;;           |              |   |
;;           |              |   | +  (positive numbers)
;;           |              |   |
;;   ground  |--------------|   o 0  (zero)
;;           |              |   | -  (negative numbers)
;;   bottom  |______________|   v
;; 
;; == CHARACTERS CONSTITUTE THE SOLE TOKENS OF OPERATIONS ==
;; Its lacuna of arithmetics, logics, and control flow mechanisms, as
;; well as its specialization on an aefauld printing facility, leaves
;; the language with characters as sole items of the data segment.
;; 
;; 
;; Instructions
;; ============
;; The language's instruction set bifurcates into a twain of sections,
;; following a hafted order, and commencing with a parasceuastic
;; initialization parcel inwith the same the thief's initial location,
;; the elevator and stair room climbing speed, as well as the building's
;; design enjoy their establishment; ere the second bailiwick apprehends
;; the actual instructions, the same in a preponderant tale engage in
;; the criminal subject's activities' description.
;; 
;; In a Procrustean adhibition of syntactical forms, each command ensues
;; from a natural language's approximation and is vouchsafed the
;; commorancy of its personal line.
;; 
;; == THE INITIALIZATION SECTION CONFIGURES PROGRAM AND BUILDING ==
;; The program's incipial moeity, the initialization section, its
;; componency tallies a sextuple capacity, encumbers itself with the
;; dever of describing the initial state, the elevator's and stair
;; room's celerity, and the building architecture.
;; 
;; == THE MOVEMENT SECTION DESCRIBES THIEF'S DEPORTMENT ==
;; A septuple cardinality governs the motive aspect of the instruction
;; set, embracing in its capacities the navigation across the floors,
;; the collection of characters in the output buffer, and the latter's
;; output and concomitant program termination.
;; 
;; == OVERVIEW: INITIALIZATION ==
;; The following apercu shall elucidate the initial configuration's
;; composition.
;; 
;; Please heed that succedaneum segments are limned by a jumelle of
;; braces at their bournes, demonstrating the design "{...}". Such
;; spatial accommodations are expected to be superseded by actual
;; "Thief, Police and the Building" code in the program's ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command                  | Configuration
;;   -------------------------+----------------------------------------
;;   A thief on {floor}/F     | Specifies the thief's initial floor
;;                            | occupied at the program's inchoation as
;;                            | {floor}.
;;                            |----------------------------------------
;;                            | {floor} must either be a signed integer
;;                            | number or the "G" ground floor
;;                            | sentinel. In any case, the referenced
;;                            | floor must be comprehended by the
;;                            | boundaries imposed via the top and
;;                            | bottom range, which please see below.
;;   ..................................................................
;;   Set SoE -> {duration}F/s | Sets the speed of the elevator's
;;                            | movement to the {duration} in seconds.
;;                            |----------------------------------------
;;                            | {duration} must be a non-negative
;;                            | integer greater or equal to zero (0).
;;   ..................................................................
;;   Set SoS -> {duration}F/s | Sets the speed of climbing the stair
;;                            | room to the {duration} in seconds.
;;                            |----------------------------------------
;;                            | {duration} must be a non-negative
;;                            | integer greater or equal to zero (0).
;;   ..................................................................
;;   top: {floor}-th floor    | Specifies that the building's top
;;                            | floor is designated by the {floor}
;;                            | number.
;;                            |----------------------------------------
;;                            | {floor} must either be a positive
;;                            | integer greater than or equal to
;;                            | one (1) or the "G" ground floor
;;                            | sentinel.
;;   ..................................................................
;;   btm: {floor}-th floor    | Specifies that the building's bottom
;;                            | floor is designated by the {floor}
;;                            | number.
;;                            |----------------------------------------
;;                            | {floor} must either be a negative
;;                            | integer less than or equal to one (1)
;;                            | or the "G" ground floor sentinel.
;;   ------------------------------------------------------------------
;; 
;; Concluding the configurations section, the building's conformation,
;; that is, the house plan, ought to be adduced, therein resides the
;; onus of allotting to each room on the premise a character whose
;; larceny ultimate produces an output message.
;; 
;; To this end, every floor's room require a specification in an aefauld
;; character's guise, with all floors expected to maintain an
;; equinumerant ordonnance in their cameration.
;; 
;; Given the top floor number "top" and the bottom floor number "btm",
;; the tally of floors "numFloors" issues from the supputation:
;; 
;;   numFloors = (top - btm) + 1
;; 
;; Each floor is represented by a line of its own, with its rooms'
;; characters segregated by one or more spaces. The assignment of the
;; floor specifications is adighted from the top to the bottom floor,
;; the rooms in a sinistrodextral airt.
;; 
;; A particular arrangement, the ground floor must be designated by the
;; indicial keyword "G/F" preceding its room characters, itself
;; separated by one or more spaces from the reserved symbols.
;; 
;; Proceeding from the aboon elucidated stipulations, the house layout
;; section ostends this designment, which, iterum, ensconces
;; succedaneous segments in braces ("{...}"):
;; 
;;       {room(top,   1)}  {room(top,   2)} ... {room(top,   numRooms)}
;;       {room(top-1, 1)}  {room(top-1, 2)} ... {room(top-1, numRooms)}
;;       ...
;;   G/F {room(gf,    1)}  {room(gf,    2)} ... {room(gf,    numRooms)}
;;       ...
;;       {room(btm+1, 1)}  {room(btm+1, 2)} ... {room(btm+1, numRooms)}
;;       {room(btm,   1)}  {room(btm,   2)} ... {room(btm,   numRooms)}
;; 
;; == OVERVIEW: MOVEMENTS ==
;; The seven motion instructions shall now be a cursory ilk of apercu's
;; cynosure.
;; 
;; Please heed that succedaneum segments are limned by a jumelle of
;; braces at their bournes, demonstrating the design "{...}". Such
;; spatial accommodations are expected to be superseded by actual
;; "Thief, Police and the Building" code in the program's ultimity.
;; 
;;   +---------+------------------------+
;;   | Purpose | Prepare elevator usage |
;;   +----------------------------------+------------------------------
;;   | Syntax  | He gets into the elevator and gets {direction}
;;   +-----------------------------------------------------------------
;;   | Effect  | Relocates the thief to the elevator and selects  the
;;   |         | {direction} for the impending motion.
;;   |         |.......................................................
;;   |         | {direction} must be one of the following options:
;;   |         |   ----------------------------------------------------
;;   |         |   Direction | Causatum
;;   |         |   ----------+-----------------------------------------
;;   |         |   up        | The thief will ascend to a higher floor.
;;   |         |   ....................................................
;;   |         |   down      | The thief will descend to a lower floor.
;;   |         |   ----------------------------------------------------
;;   +---------+-------------------------------------------------------
;;   
;;   +---------+--------------------------+
;;   | Purpose | Prepare stair room usage |
;;   +------------------------------------+----------------------------
;;   | Syntax  | He gets into the stair room and gets {direction}
;;   +-----------------------------------------------------------------
;;   | Effect  | Relocates the thief to the stair room and selects
;;   |         | the {direction} for the impending motion.
;;   |         |.......................................................
;;   |         | {direction} must be one of the following options:
;;   |         |   ----------------------------------------------------
;;   |         |   Direction | Causatum
;;   |         |   ----------+-----------------------------------------
;;   |         |   up        | The thief will ascend to a higher floor.
;;   |         |   ....................................................
;;   |         |   down      | The thief will descend to a lower floor.
;;   |         |   ----------------------------------------------------
;;   +---------+-------------------------------------------------------
;;   
;;   +---------+---------------------------------+
;;   | Purpose | Move up/down using the elevator |
;;   +-------------------------------------------+---------------------
;;   | Syntax  | He stays in the elevator for {duration}s
;;   +-----------------------------------------------------------------
;;   | Effect  | If the current elevator direction equals "up", moves
;;   |         | the thief up by the following tally of stories
;;   |         | relative to the current floor "currFloor", depending
;;   |         | upon the elevator speed "elevatorSpeed" specified in
;;   |         | the initialization section, following the formula
;;   |         |   currFloor = currFloor + (duration * elevatorSpeed)
;;   |         | 
;;   |         | If the current elevator direction equals "up", moves
;;   |         | the thief down by the following tally of stories
;;   |         | relative to the current floor "currFloor", depending
;;   |         | upon the elevator speed "elevatorSpeed" specified in
;;   |         | the initialization section, following the formula
;;   |         |   currFloor = currFloor - (duration * elevatorSpeed)
;;   |         |.......................................................
;;   |         | {duration} must be a non-negative integer number.
;;   +---------+-------------------------------------------------------
;;   
;;   +---------+-----------------------------------+
;;   | Purpose | Move up/down using the stair room |
;;   +---------------------------------------------+-------------------
;;   | Syntax  | He stays in the stair room for {duration}s
;;   +-----------------------------------------------------------------
;;   | Effect  | If the current stair room direction equals "up",
;;   |         | moves the thief up by the following tally of stories
;;   |         | relative to the current floor "currFloor", depending
;;   |         | upon the climb speed "climbSpeed" specified in the
;;   |         | initialization section, following the formula
;;   |         |   currFloor = currFloor + (duration * climbSpeed)
;;   |         | 
;;   |         | If the current stair room direction equals "down",
;;   |         | moves the thief down by the following tally of stories
;;   |         | relative to the current floor "currFloor", depending
;;   |         | upon the climb speed "climbSpeed" specified in the
;;   |         | initialization section, following the formula
;;   |         |   currFloor = currFloor - (duration * climbSpeed)
;;   |         |.......................................................
;;   |         | {duration} must be a non-negative integer number.
;;   +---------+-------------------------------------------------------
;;   
;;   +---------+----------------------------------+
;;   | Purpose | Leave the elevator or stair room |
;;   +--------------------------------------------+--------------------
;;   | Syntax  | He gets out
;;   +-----------------------------------------------------------------
;;   | Effect  | Commands the thief to leave the currently used
;;   |         | elevator or stair room, invaliding the selected
;;   |         | elevator or climb diretion.
;;   +---------+-------------------------------------------------------
;;   
;;   +---------+-----------------------------------------+
;;   | Purpose | Append a character to the output buffer |
;;   +---------------------------------------------------+-------------
;;   | Syntax  | He climbs into {roomNumber}-th room and steals
;;   +-----------------------------------------------------------------
;;   | Effect  | Appends the character answering to the room with the
;;   |         | {roomNumber} on the current floor to the output
;;   |         | buffer.
;;   |         |.......................................................
;;   |         | {roomNumber} must be a non-negative integer number
;;   |         | greater than or equal to one (1) and less than or
;;   |         | to the number of rooms per floor.
;;   +---------+-------------------------------------------------------
;;   
;;   +---------+-------------------------+
;;   | Purpose | Print the output buffer |
;;   +-----------------------------------+-----------------------------
;;   | Syntax  | The police have come
;;   +-----------------------------------------------------------------
;;   | Effect  | Prints the output buffer content to the standard
;;   |         | output conduit, purges the buffer, and terminates the
;;   |         | program.
;;   +---------+-------------------------------------------------------
;; 
;; == COMMAND EXECUTION IS A DEPENDENCY UPON THE PROGRAM STATE ==
;; During the execution of the program's movement segment, the
;; derivation of a command's feasibility derives from the program state
;; assumed at its invocation's instant.
;; 
;; In order to administer the mete's requisitum in nortelry, a state
;; machine illustration shall be delivered to the reader's dation, for
;; siccan occasion it holds that the commands are delegated to the roles
;; of events, while the program state ally with the machine states.
;; 
;; A quadruple of states exhausts the possible situations:
;; 
;;   ------------------------------------------------------------------
;;   State         | Description
;;   --------------+---------------------------------------------------
;;   in floor      | The thief moving freely inside of the building.
;;   ..................................................................
;;   in elevator   | The thief has entered the elevator and selected a
;;                 | motion direction, or is currently moving via this
;;                 | medium.
;;   ..................................................................
;;   in stair room | The thief has entered the stair room and selected
;;                 | a motion direction, or is currently climbing its
;;                 | stairs.
;;   ..................................................................
;;   in custody    | The police has arrived, which terminates the
;;                 | program execution.
;;   ------------------------------------------------------------------
;; 
;; The following equiparation betwixt the commands in the language and
;; the events in the state machine are maintained:
;; 
;;   ------------------------------------------------------------------
;;   Command                                  | State machine event
;;   -----------------------------------------+------------------------
;;   He gets into the elevator and gets ...   | enter elevator
;;   ..................................................................
;;   He gets into the stair room and gets ... | enter stair room
;;   ..................................................................
;;   He stays in the elevator for ...s        | operate elevator
;;   ..................................................................
;;   He stays in the stair room for ...s      | climb stair room
;;   ..................................................................
;;   He climbs into ...-th number and steals  | rob room
;;   ..................................................................
;;   The police have come                     | encounter police
;;   ------------------------------------------------------------------
;; 
;; Please heed that the state machine's incipial state is designated by
;; the initial pseudo-state, here limned as "(O)", while the execution
;; concludes in the final pseudo-state, visualized as "(X)".
;; 
;; Proceeding from the aboon diorisms, the following diagram imposes
;; its concinnity:
;; 
;;                         (O)
;;                          |     +-----+
;;                          |     |     | rob room
;;                          |     |     |
;;                          V     V     |
;;       enter elevator   +---------------+  enter stair room
;;     +------------------|               |------------------+
;;     |                  |   in floor    |                  |
;;     |           +----->|               |<-----+           |
;;     |           |      +---------------+      |           |
;;     |           |              |              |           |
;;     |     leave |              |              | leave     |
;;     |           |       encounter police      |           |
;;     |           |              |              |           |
;;     V           |              |              |           V
;;   +---------------+            |            +---------------+
;;   |               |            |            |               |
;;   |  in elevator  |            |            | in stair room |
;;   |               |            |            |               |
;;   +---------------+            |            +---------------+
;;       ^       |                V                ^       |
;;       |       |        +---------------+        |       |
;;       |       |        |               |        |       |
;;       +-------+        |  in custody   |        +-------+
;;   operate elevator     |               |      climb stair room
;;                        +---------------+
;;                                |
;;                                |
;;                                |
;;                                V
;;                               (X)
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of an encapsulated
;; program representation directly from the source code's characters.
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
;; 
;; Appendices
;; ==========
;; A particular bailiwick is empight on the jointure betwix the
;; conspicable significant topics and siccan specimens whom a parhedral
;; rank ought to be imparted. The following sections shall cast their
;; attention to these elements maintaining their abode in the crepuscle.
;; 
;; == APPENDIX A: PROJECT FILES ==
;; A certain mete of complexity's purview vindicates the project's
;; distribution among several files, their correct ordonnance, in
;; conjunction with a cursory apercu, shall be adduced in a tabular
;; form:
;; 
;;   ------------------------------------------------------------------
;;   No. | File             | Role
;;   ----+------------------+------------------------------------------
;;   1   | types.lisp       | Comprehends the definition of the bespoke
;;       |                  | types utilized throughout the project,
;;       |                  | encompassing such specimens as
;;       |                  | ``list-of'' and ``positive-integer''.
;;   ..................................................................
;;   2   | house-plan.lisp  | Implements the representation of the
;;       |                  | robbed building's architecture, including
;;       |                  | in its compass the top and bottom floors,
;;       |                  | as well as the rooms per floor.
;;   ..................................................................
;;   3   | commands.lisp    | Defines the languague's commands,
;;       |                  | everichon's delegate a dedicated class.
;;   ..................................................................
;;   4   | states.lisp      | Accoutres the program state machine, the
;;       |                  | dever apportioned to whom constitutes the
;;       |                  | recognized states during a program's
;;       |                  | evolution and the permissive transitions,
;;       |                  | instigated by the commands.
;;   ..................................................................
;;   5   | program.lisp     | Encapsulates a parse program, entailing
;;       |                  | the initial state, house plan, and
;;       |                  | command list.
;;   ..................................................................
;;   6   | parser.lisp      | Establishes the language parser, realized
;;       |                  | in terms of parser combinators, the same
;;       |                  | in their ultimity produce a particular
;;       |                  | program representation.
;;   ..................................................................
;;   7   | interpreter.lisp | Furnishes the interpreter, reponsible for
;;       |                  | the parsed program's evaluation.
;;   ..................................................................
;;   8   | tests.lisp       | Specifies the test cases as a docimasy
;;       |                  | applied to the interpreter.
;;   ..................................................................
;;   9   | main.lisp        | Establishes the starting point into this
;;       |                  | project, the paravaunt objective of which
;;       |                  | manifests in the loading of the project's
;;       |                  | source files.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-02-15
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
;;   [englishclub2024floors]
;;   The EnglishClub contributors, "Floors of a House", 2024
;;   URL: "https://www.englishclub.com/vocabulary/floors-house.php"
;;   Notes:
;;     - Visualizes the conceptual design of a house's floors.
;;     - Emphasizes the discrepancies in American and British English.
;;       o In particular, the floor at land level is stevened the
;;         "ground floor" in British English, while concomitantly the
;;         American English terminology assigns "first floor" to it.
;;       o The "Thief, Police and the Building" programming language
;;         complies with the British nomenclature.
;;   
;;   [esolang2023thiefpolbuild]
;;   The Esolang contributors, "Thief, Police and the Building",
;;     September 5th, 2023
;;   URL: "https://esolangs.org/wiki/Thief,_Police_and_the_Building"
;;   
;;   [goodrich214datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", sixth edition, 2014,
;;     pages 122--127
;;   Notes:
;;     - Describes the concept and an implementation of the singly
;;       linked list in the Java programming language.
;;     - The pages 276 through 280 describe the concept and an
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
;; -- Definition of package.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :thief-police-and-the-building
  (:use    :cl)
  (:shadow #:word
           #:speed))

;;; -------------------------------------------------------

(in-package :thief-police-and-the-building)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type pathname +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "The directory on the system accommodating the commorancy of the
   project's Common Lisp source files.
   ---
   For the maintainer or manipulator of this project: Please do not
   encumber yourself with the peccancy of a lapsus' inroad anenst the
   project directory's correct specification; its pointed location must
   resolve to the Common Lisp source files' woning on the executing
   system.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of import operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-project-file (file-name)
  "Loads the Common Lisp source file from the FILE-NAME, evaluates it,
   and returns no value."
  (declare (type pathname file-name))
  (load (merge-pathnames +PROJECT-DIRECTORY+ file-name))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of project files.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-project-file
  (make-pathname :name "types" :type "lisp"))

(import-project-file
  (make-pathname :name "house-plan" :type "lisp"))

(import-project-file
  (make-pathname :name "commands" :type "lisp"))

(import-project-file
  (make-pathname :name "states" :type "lisp"))

(import-project-file
  (make-pathname :name "program" :type "lisp"))

(import-project-file
  (make-pathname :name "parser" :type "lisp"))

(import-project-file
  (make-pathname :name "interpreter" :type "lisp"))

(import-project-file
  (make-pathname :name "tests" :type "lisp"))
