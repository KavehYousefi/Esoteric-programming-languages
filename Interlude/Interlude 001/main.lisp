;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Interlude", invented by the Esolang user
;; "PythonshellDebugwindow" and presented on March 8th, 2021, the
;; commorancy of its haecceity a two-dimensional grid of characters,
;; partitioned and dighted by vertical "interlude" lines into
;; rectangular parcels, norned "acts", that serve in the encapsulation
;; of instructions in serelepes units of coherency, while operating on
;; a queue of signed integer numbers.
;; 
;; 
;; Concept
;; =======
;; The Interlude programming language operates two-dimensional grid of
;; characters, horizontally segregated by vertical lines, the
;; "interludes", into rectangular areas stevened "acts", their entailed
;; instructions' dedication that of a signed integer-valued queue's
;; perquisition and manipulation.
;; 
;; == THE CODE: A TWO-DIMENSIONAL GRID, ENTREPARTED INTO ACTS ==
;; An Interlude program's conformation derives from a two-dimensional
;; character grid's partage into so called "acts", their merist an
;; aefauld vertical line ("|") commorant inside of each two accolent
;; acts' interstition, but not expected to demarcate the grid's
;; sinistral and dextral bournes. These sepiment lines, extending
;; from the vertically across the entire character grid, are nevend
;; "interludes", forming the rectangular act parcels.
;; 
;; A forbisen's elucidative potential shall be naited for the endeictic
;; purpose of the grid and acts' principles. Please heed that the
;; parcery of letters does not necessarily limn actual Interlude
;; instructions, but rather acts in the agency of succedanea for an
;; abstract visualization.
;; 
;;   AAAA|BB|CCCCC
;;   AAAA|BB|CCCCC
;;   AAAA|BB|CCCCC
;; 
;; The aboon illustration serves in the designment of three acts:
;; 
;;   (1) The first act, designated by "A"s, being composed of four
;;       columns and three lines, with the line count shared by all its
;;       compernage.
;;         AAAA
;;         AAAA
;;         AAAA
;;   
;;   (2) The second act, comprehending "B"s, edified by two columns and
;;       the natural three lines.
;;         BB
;;         BB
;;         BB
;;   
;;   (3) The third act, ostending "C"s, amplecting five columns and
;;       three lines.
;;         CCCCC
;;         CCCCC
;;         CCCCC
;; 
;; == ACTS FORM COLUMNS IN THE GRID ==
;; An act's spatial appropriation constitutes a column's simulacrum,
;; commencing in the upmost line of the code and dispanding until the
;; desinent row, segregated, however, sinistrally and dextrally by
;; either a near-dwelling act, or the program's natural boundaries.
;; 
;; == ACTS EXECUTE FROM THE TOP-LEFT TO THE BOTTOM-RIGHT CORNER ==
;; An act's establishment is that of an encapsulated instruction
;; sequence, intended to be processed from the top-left corner in a
;; sinistrodextral airt towards its bottom-right cell, in the course of
;; such anabasis wrapping around on the dextral bourne to the next lower
;; line.
;; 
;; An illustrative accoutrement, destitute of representative competences
;; according to the actual Interlude instruction set, the following
;; act, regareded serelepes, shall demonstrate the execution sequence:
;; 
;;   abc
;;   def
;;   ghi
;; 
;; A program theoretically ordered to this parcel's evaluation is
;; expected to traverse the symbols in this order:
;; 
;;   a, b, c, d, e, f, g, h, i
;; 
;; which, by adminiculum of enumerations, is tantamount to the
;; step numbers
;; 
;;   123
;;   456
;;   789
;; 
;; == THE PROGRAM HALTS WHEN EXHAUSTING AN ACT'S INSTRUCTIONS ==
;; At the program's inchoation, the instruction pointer (IP) partakes
;; of a commorancy in the first act's top-left cell, executing its
;; instructions during the incession towards the bottom-right bourne.
;; Upon this desinent cell processing's patration, the program
;; immediately halts.
;; 
;; == A VALID PROGRAM ENTERS AT LEAST TWO ACTS ==
;; During the program's execution, at least two acts ought to be
;; sojourned; otherwise, upon the program's termination, an error is
;; signaled appertaining to this covenant's infraction.
;; 
;; == THE PROGRAM MEMORY: A QUEUE OF INTEGERS ==
;; The castaldy of data subscribes to a queue's consignment, its
;; compass' attendance restricted to the species of integers, albeit
;; homologating any sign and mickleness.
;; 
;; Operations exist which, as a paravaunt causatum or an epiphenomenal
;; consequence, actuate the queue abstract data type's (ADT)
;; foundational services, scilicet:
;; 
;;   (a) The insertion at the queue's rear, that is, to "enqueue" an
;;       element.
;;   
;;   (b) The foremost element's removal, contingently with a concomitant
;;       deployment of the thus obtained datum; which enjoys the euonym
;;       to "dequeue" that member.
;;   
;;   (c) The peeking, or returning without deletion, of the foremost
;;       queue element.
;; 
;; The Interlude queue's diorism in the latter two regards, nominatim
;; the processes (a) and (b), imposes the prescription of a zero (0)
;; value response when dequeuing from or peeking into an empty queue.
;; 
;; 
;; Instructions
;; ============
;; The Interlude instruction set enumerates a sextuple account of
;; actual operations, comprehending in this entelechy the warklumes for
;; the program memory queue's castaldy, input reception and output
;; issuance, as well as control flow guidance facilities, such as
;; conditional execution and unconditional jumping betwixt acts.
;; 
;; As a perergon, alphanumeric characters and any non-space symbol
;; enjoy an epiphenomenal potential which yet eloigns them from the
;; consideration as veridicous instructions.
;; 
;; Spaces, as an exemption, may not partake of the Interlude source code
;; in any manner.
;; 
;; == ABSTRACT INSTRUCTION CAUSATA ==
;; A certain mete of complexity's woning is established in several of
;; Interlude's instructions, with respect to the concrete specimen
;; experiencing the actuation as a dependency upon the subsequent
;; characters or the memory queue's contemporaneous state.
;; 
;; An apercu concerning the commands and their general devers shall be
;; the below tabulation's furnishment in order to sicker some cohesion
;; in the comprehension:
;; 
;;   ------------------------------------------------------------------
;;   Command | Devers
;;   --------+---------------------------------------------------------
;;   v       | Act-internal line descending
;;   ..................................................................
;;   $       | Jumping among acts
;;   ..................................................................
;;   +       | Summation, output
;;   ..................................................................
;;   *       | Enqueing of literals, dequeing
;;   ..................................................................
;;   ?       | Input, conditional execution, queue clearance
;;   ..................................................................
;;   :       | Program invalidation, act padding
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; A tabular apercu's dation shall be a sufficient grade of nortelry's
;; communication anent the actual instructive components, ere the
;; parhedral operative aspects ensue in the subsequent section:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Dequeues an integer, "n", from the queue:
;;           |   (1) If n is greater than zero (0), dequeues n further
;;           |       elements, supputates their sum, and enqueues this
;;           |       result.
;;           |   (2) Otherwise, if n is less than or equal to zero (0),
;;           |       dequeues the next element and print the character
;;           |       whose ASCII code corresponds to thilk to the
;;           |       standard output.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction it holds:
;;           |   let frontElement <- dequeue ()
;;           |   if frontElement > 0 then
;;           |     let numberOfRepetitions <- dequeue ()
;;           |     let sum                 <- 0
;;           |     repeat numberOfRepetitions times do
;;           |       let currentFrontElement <- dequeue ()
;;           |       sum <- sum + currentFrontElement
;;           |     end repeat
;;           |     enqueue (sum)
;;           |   else
;;           |     let asciiCode <- dequeue ()
;;           |     print character for asciiCode
;;           |   end if
;;   ..................................................................
;;   *       | Reads the next character from the current act:
;;           |   (1) If this character constitutes a digit, enqueues
;;           |       its numeric value.
;;           |   (2) If the character constitutes a Latin letter,
;;           |       enqueues its ASCII code.
;;           |   (3) If the character constitutes a minus sign ("-"),
;;           |       reads the subsequent character "c":
;;           |       (3.a) If c constitutes a digit, enqueues the
;;           |             negative numeric value, -c, of c.
;;           |       (3.b) If c constitutes a Latin letter, enqueues
;;           |             the negative ASCII code of c.
;;           |       (3.c) Otherwise dequeues an element and enqueues
;;           |             zero.
;;           |   (4) Otherwise deques an element and discards it.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction it holds:
;;           |   let nextSymbol <- read next character in act
;;           |   if nextSymbol is a digit then
;;           |     let numericValue <- nextSymbol as number
;;           |     enqueue (numericValue)
;;           |   else if nextSymbol is alphabetic then
;;           |     let asciiCode <- ASCII code for nextSymbol
;;           |     enqueue (asciiCode)
;;           |   else if nextSymbol = "-" then
;;           |     let symbolAfterMinus <- read next character in act
;;           |     if symbolAfterMinus is digit then
;;           |       let numericValue <- symbolAfterMinus a number
;;           |       enqueue (0 - numericValue)
;;           |     else if symbolAfterMinus is alphabetic then
;;           |       let asciiCode <- ASCII code for symbolAfterMinus
;;           |       enqueue (0 - asciiCode)
;;           |     else
;;           |       dequeue ()
;;           |       enqueue (0)
;;           |     end if
;;           |   else
;;           |     dequeue ()
;;           |   end if
;;   ..................................................................
;;   ?       | Reads the next character from the current act:
;;           |   (1) If this character constitutes a period ("."),
;;           |       queries the standard input for a character and
;;           |       enqueues its ASCII code.
;;           |   (2) If the character constitutes an "explicit"
;;           |       Interlude command, present among the hexad of this
;;           |       tabular overview, checks the queue's front element
;;           |       without its removal:
;;           |       (2.a) If this element does not equal zero (0),
;;           |             skips the entire next command.
;;           |       (2.b) If the element equals zero (0), proceeds as
;;           |             usual.
;;           |   (3) Otherwise clears the entire queue.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction it holds:
;;           |   let nextSymbol <- read next character in act
;;           |   if nextSymbol = "." then
;;           |     let userInput <- query character
;;           |     let inputCode <- ASCII code for userInput
;;           |     enqueue (inputCode)
;;           |   else if nextSymbol designates explicit command then
;;           |     let frontElement <- peekQueue ()
;;           |     if frontElement != 0 then
;;           |       skip next command
;;           |     end if
;;           |   else
;;           |     clear queue
;;           |   end if
;;   ..................................................................
;;   $       | Dequeues an element, "n", and relocates the instruction
;;           | pointer (IP) to the top-left character in the act
;;           | amenable to the one-based index n.
;;   ..................................................................
;;   v       | Moves the instruction pointer (IP) to the leftmost
;;           | position of the line below the current one.
;;           | If the instruction pointer is already empight on the
;;           | last line, immediately terminates the program.
;;   ..................................................................
;;   :       | Signals an error and immediately terminates the program.
;;           |---------------------------------------------------------
;;           | This symbol, if not intended for actual usance, may be
;;           | naited as a padding character.
;;   ------------------------------------------------------------------
;; 
;; == "IMPLICIT" INSTRUCTIONS ==
;; The following stipulations appertain to symbols not ensconced among
;; the sextuple of the aboon command listing:
;; 
;;   (1) If the character represents an alphanumeric entity not
;;       reserved by the sextuple instruction table, that is, either
;;       one of the ten decimal digits or a Lattin letter except "v",
;;       thilk is printed in its verbatim form to the standard output.
;;   
;;   (2) Any other symbol, non-alphanumeric and non-space, incites the
;;       following complex deportment following this exact ordonnance:
;;       
;;         (a) The value 78 is enqueued in the memory queue.
;;         
;;         (b) The instruction pointer (IP) is relocated to the
;;             left-upper corner of a randomly chosen act.
;;         
;;         (c) If a line below the new instruction pointer (IP)
;;             position exists, the character immediately below this
;;             location is changed to the "v" symbol, concomitantly
;;             receiving the respective instruction's competences.
;;   
;;   (3) If the character represents a space --- corresponding to the
;;       ASCII code 32 ---, an error of the type "InvalidSymbolError"
;;       is actuated.
;; 
;; == EXPOSITION OF THE "*" INSTRUCTION ==
;; The convolute intrinsics of the "*" instruction shall enjoy a
;; dedicated exposition's adhibition.
;; 
;; Please heed that succedaneous segments are ensconced in a jumelle of
;; braces, "{" and "}", serving in the designation of the actual
;; Interlude program's content.
;; 
;;   ------------------------------------------------------------------
;;   Combination    | Causatum
;;   ---------------+--------------------------------------------------
;;   *  {digit}     | Enqueues the {digit}.
;;   ..................................................................
;;   *  {letter}    | Enqueues the ASCII code of the {letter}.
;;   ..................................................................
;;   *  {otherwise} | Dequeues an element and discards it.
;;   ..................................................................
;;   *- {digit}     | Enqueues the negated {digit}.
;;   ..................................................................
;;   *- {letter}    | Enqueues the negated ASCII code of the {letter}.
;;   ..................................................................
;;   *- {otherwise} | Dequeues an element, discard it, and enqueue 0.
;;   ------------------------------------------------------------------
;; 
;; == EXPOSITION OF THE "?" INSTRUCTION ==
;; Consanguinous in its versatility, but vested with less severity in
;; its ramosity, the "?" instruction's triune haecceity shall be a
;; nuncupated treatise's cynosure.
;; 
;; Please heed that succedaneous segments are ensconced in a jumelle of
;; braces, "{" and "}", serving in the designation of the actual
;; Interlude program's content. Please also remember that the "command"
;; designation as a succedaneum appertains to one of the sextuple
;; members listed in the instruction table further above, that is:
;; 
;;   +
;;   *
;;   ?
;;   $
;;   v
;;   :
;; 
;; Any other symbol desists from this "command" aspect indicium's
;; appropriation.
;; 
;;   ------------------------------------------------------------------
;;   Command       | Causatum
;;   --------------+---------------------------------------------------
;;   ? .           | Queries for an character, and enqueues its ASCII
;;                 | code.
;;   ..................................................................
;;   ? {command}   | If the front queue element is non-zero, skips the
;;                 | entire next instruction; otherwise executes it.
;;   ..................................................................
;;   ? {otherwise} | Clears the queue.
;;   ------------------------------------------------------------------
;; 
;; == COMMON TASKS ==
;; A peculiar telos' reification being the causatum of several
;; configurations, the following exposition shall contribute the
;; forbisens for their establishment, including the interactions betwixt
;; the instruction, its operands, and, upon a contingent necessitation,
;; the memory queue state.
;; 
;; Please heed the demarcation of succedaneous segments by adminiculum
;; of braces ("{...}").
;; 
;;   ------------------------------------------------------------------
;;   Intention            | Instruction              | Queue state
;;   ---------------------+--------------------------+-----------------
;;   Enqueue digit        | * {digit}                | -
;;   ..................................................................
;;   Enqueue negative     | * - {digit}              | -
;;   digit                |                          | 
;;   ..................................................................
;;   Enqueue ASCII code   | * {letter}               | -
;;   ..................................................................
;;   Enqueue negative     | * - {letter}             | -
;;   ASCII code           |                          | 
;;   ..................................................................
;;   Dequeue and discard  | * {character},           | -
;;                        |--------------------------| 
;;                        | where {character} is not | 
;;                        | a digit, a letter, or    | 
;;                        | the minus ("-") sign     | 
;;   ..................................................................
;;   Dequeue, discard,    | * - {character},         | -
;;   and enqueue zero (0) |--------------------------| 
;;                        | where {character} is not | 
;;                        | a digit or a letter      | 
;;   ..................................................................
;;   Print literal digit  | {digit or letter}        | -
;;   or letter            |                          | 
;;   ..................................................................
;;   Print front of queue | +                        | (n, ...),
;;                        |                          | where n <= 0
;;   ..................................................................
;;   Accumulate first n   | +                        | (n, ...),
;;   queue elements       |                          | where n > 0
;;   ..................................................................
;;   Jump to start of     | $                        | (n, ...),
;;   n-th act             |                          | where n != 0
;;   ..................................................................
;;   Jump to start of     | {character},             | -
;;   random act           | where {character} is not | 
;;                        | where {character} is not | 
;;                        | a digit, a letter, an    | 
;;                        | instruction identifier,  | 
;;                        | or a space (" ").        | 
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's reification originates in the programming language
;; Common Lisp, its operation, on a higher tier, a twifold enterprise,
;; the second moeity itself iterum capable of disintegration into
;; disjunct parcels of interoperable capacity.
;; 
;; In a listing producing its composites nominatim, the first stratum
;; comprises the twissel:
;; 
;;   (1) CODE INTO BOARD ARRANGEMENT
;;       The source code string is rearranged into a two-dimensional
;;       grid or board format.
;;   
;;   (2) INTERPRETATION
;;       The 2D board is evaluated and its ensconced instructions are
;;       executed.
;; 
;; == (1) THE PARASCEVE: A STRING TRANSMOGRIFIED INTO A BOARD ==
;; The facilitation of the Interlude code's evaluation is assigned as a
;; bailiwick to a parasceuastic effort, which transforms the
;; provenance's one-dimensional string format into a dedicated "Board"
;; class instance, entalented with the amenability of everichon among
;; its character-valued cells to a two-dimensional subscript twain of
;; column (x-coordinate) and row (y-coordinate).
;; 
;; == (2) INTERPRETATION: REPEATED INSTRUCTION EXTRACTION, EXECUTION ==
;; The multifarious tiers comprising the interpretation stage shall be
;; adhibited a structured enumeration:
;; 
;;   (1) The instruction pointer (IP) is empight on the left-upper
;;       corner of the first act.
;;       As a concomitant, the number of distinct visited acts is set
;;       to one (1), naited as a parasceve for the integrity docimasy
;;       at the program's cessation, which please deliver to your
;;       conspection under the point (5).
;;   
;;   (2) The character signified by the current instruction pointer (IP)
;;       location is subjected to a perquisition, whence yields an
;;       "Instruction" instance encapsulation of its type, content, as
;;       well as inclusive start point, concurring with the character's
;;       location itself, and the inclusive end point, designating the
;;       desinent location occupied by a more complex operation.
;;   
;;   (3) The thus extracted instruction is executed by the interpreter
;;       unit. It constitutes the instruction's epiphenomenal dever to
;;       relocate the instruction pointer (IP) to the correct new
;;       position in the current or a new act.
;;       If the new act realizes a sojourn's destination not yet
;;       attested during the execution process, the tally of distinct
;;       visited acts is incremented by one.
;;   
;;   (4) If the current act's desinent instruction has been processed,
;;       or its established bournes have been transgressed, the program
;;       terminates; otherwise the procedure continues with the
;;       step (2).
;;   
;;   (5) An ultimity actuated by the program's termination, the tally
;;       of distinct acts entered during the execution period is probed;
;;       if supputated as less than two, an error of the type
;;       "Too-Few-Entered-Acts-Error" is signaled; otherwise no further
;;       causatum precedes the interpreter's conclusion.
;; 
;; 
;; Appendices
;; ==========
;; A certain account of subjects' existency can be attested thilk does
;; not vindicate the respective items' installation inwith the main
;; documentation body; yet ostends a diorism and pertinence occupying
;; on the metewand a sufficient parcel as to rede a desistence from
;; its elision. These topics shall be the following appendix tmemata's
;; cynosure.
;; 
;; == APPENDIX A: CHARACTER OUTPUT GENERATION AND ISSUANCE ==
;; The kenspeckle concepts imparted a commorancy in the Interlude
;; programming language condition a rather harsh mete of intricacy in
;; the anticipated eath reproduction of an ASCII character on the
;; standard output.
;; 
;; An ultimity begotten by these predicaments shall be a dedicated
;; treatise on the subject involving such entity's character code
;; assemblage and its printing.
;; 
;; == PARTITION OF AN INTEGER NUMBER ==
;; The solution to the difficulty of generating large numbers on the
;; Interlude program queue relies in its amendment on the mathematical
;; concept of integer partition: the partage of a non-negative integer
;; number into the sum of one or more positive integer terms. The
;; foundational principles' generalization into the negative realm may
;; be harnessed with the telos of any signed integer's generation.
;; 
;; Given the desideratum "a", construed as the sum of "n" terms, or
;; sumands, with
;; 
;;   n >= 1
;; 
;; such that
;; 
;;   a = a[1] + a[2] + ... + a[n]
;; 
;; a restoration of "a" via these components proceeds by the enqueuing
;; the tally of summands "n", succeeded by the terms "a[1]" through
;; "a[n]" themselves:
;; 
;;   enqueue (n)
;;   enqueue (a[1])
;;   enqueue (a[2])
;;   enqueue (...)
;;   enqueue (a[n])
;; 
;; which ultimately yields the following structure on the queue's
;; pertinent tmema, limning the queue front on the sinistral laterality,
;; while the dextral comprehends the rear:
;; 
;;   n, a[1], a[2], ..., a[n]
;; 
;; If "a" constitutes a negative integer the selection of addends must
;; perforce incorporate one or more negative components.
;; 
;; == PARASCEVE FOR PRINTING ==
;; Ere the integer number's assemblage, that is, the summation of "a[1]"
;; through "a[n]" into the desired "a", the print behest's requisitum
;; ought to be accommodated. Siccan service is educed from the Interlude
;; "+" instruction as a response to the dequeuing of an element less
;; than or equal to zero (0) from the program queue. As a corollary, the
;; printing sentinel, here nevened "p", with
;; 
;;   p <= 0
;; 
;; must be appended to the summands, empight on the (n + 1)-th position:
;; 
;;   n, a[1], a[2], ..., a[n], p
;; 
;; Forecause "p" may assume any value less than or equal to zero (0), we
;; hithertoforth simplify the choice to
;; 
;;   p = 1
;; 
;; --- of course, any other value satisfying the stipulation may be
;; employed instead:
;; 
;;   n, a[1], a[2], ..., a[n], 0
;; 
;; == ASSEMBLE OF THE INTEGER FROM ITS SUMMANDS ==
;; The desiderated integer's, "a", production ensues from the addition
;; of its "n" summands, "a[i]" through "a[n]", actuated by a "+"
;; Interlude operation invocation, inciting, as further epiphenomena,
;; these "n" front elements' removal from the queue, concluded with an
;; insertion of the sum, "a", into its rear.
;; 
;; A purlicue begotten by these efforts, the queue fragment
;; 
;;   n, a[1], a[2], ..., a[n], 0
;; 
;; for
;; 
;;   a = a[1], a[2], ..., a[n]
;; 
;; transforms into
;;   
;;   0, a
;; 
;; == OUTPUT OF THE ASSEMBLED INTEGER OBJECT AS A CHARACTER ==
;; A second "+" instruction application, this time producing for the
;; discriminant zero (0) at the queue's front the causatum of an output
;; in lieu of a catena of additions, removes both elements at the front,
;; imprimis the sentinel 0, subsequently the integer number "a", while
;; concomitantly printing the character whose ASCII code corresponds to
;; "a".
;; 
;; == PURLICUE: PRINTING AN ASSEMBLED CHARACTER ==
;; The aboon principles' reiteration shall constitute an apercu's
;; cynosure in this section.
;; 
;; Given the integer number "a" to replicate, and its partition into
;; the "n" tally of integral constituents "a[1]" through "a[n]", the
;; following queue operations, limned as a pseudocode tmema, apply:
;; 
;;   enqueue (n)
;;   enqueue (a[1])
;;   enqueue (a[2])
;;   enqueue (...)
;;   enqueue (a[n])
;;   enqueue (0)
;;   
;;   invoke Interlude operation "+"
;;   invoke Interlude operation "+"
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-02-10
;; 
;; Sources:
;;   [esolang2021Interlude]
;;   The Esolang contributors, "Interlude", March 10th, 2021
;;   URL: "https://esolangs.org/wiki/Interlude"
;;   
;;   [goodrich2006datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 115--119 describe the singly linked list.
;;       o The page 116 presents a partial implementation in the Java
;;         programming language.
;;     - The pages 204--212 describe the queue abstract data type (ADT).
;;       o The pages 205--206 present the "Queue" interface.
;;       o The pages 206--209 furnish a fixed-capacity array-based queue
;;         implementation in the Java programming language.
;;       o The page 210 provides a fragmentary implementation of the
;;         queue via a generic singly linked list.
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 122--127 describe the concept and an
;;       implementation of the singly linked list in the Java
;;       programming language.
;;       o This establishes the most pertinent subject for our project.
;;       o The pages 126--127 furnish an implementation.
;;     - The pages 238--247 describe the queue abstract data type (ADT).
;;       o The pages 241--244 produce an array-based implementation.
;;       o The page 245 demonstrates an implementation via a singly
;;         linked list.
;;     - The pages 276--280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;;   
;;   [math.stackexchange2018q2812193]
;;   The Mathematics Stack Exchange contributors,
;;     "Is there a term analogous to factoring, for addition?",
;;     June 8th, 2018
;;   URL: "https://math.stackexchange.com/questions/2812193/
;;         is-there-a-term-analogous-to-factoring-for-addition"
;;   Notes:
;;     - Mentions the term "partition" as an additive analogue to the
;;       multiplicative factorization concept.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of whose diorism includes, among other specimens, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which complies with the KEY-TYPE and
   answers to a value of the VALUE-TYPE, for both holds the default of
   ``T''."
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
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype character-matrix ()
  "The ``character-matrix'' type defines a two-dimensional Cartesian
   arrangement of characters in a sparse mode, realized as a hash table,
   the keys of which are established via ``Location'' specifiers, while
   the values assume characters."
  '(hash-table-of Location character))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero more elements,
   each member of which complies to the ELEMENT-TYPE, for the same holds
   the default of the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype index-list ()
  "The ``index-list'' type defines a list whose conformation amplects
   zero or more act indices, desumed from the non-negative realm of
   integer numbers greater than or equal to one (1)."
  '(list-of (integer 1 *)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its \"generalized boolean\" aspects and
   produces a veridical Boolean tantamount thereof, returning for a
   non-``NIL'' input the ``T'' value; otherwise, for a ``NIL'' OBJECT,
   responding with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor make-node (element next)))
  "The ``Node'' class serves in the representation of a singly linked
   node, utible for its participation in a queue, and endowed with a
   twissel componency: imprimis, the ensconced element; further a
   pointer to a contingent successor node."
  (element (error "Missing node element.")
           :type      integer
           :read-only T)
  (next    (error "Missing node successor.")
           :type      (or null Node)
           :read-only NIL))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) (stream T))
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "(Node :element ~d)"
    (node-element node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Queue".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Queue ()
  ((head
    :initform      NIL
    :type          (or null Node)
    :documentation "The first node in the queue, if the same is not
                    empty.")
   (tail
    :initform      NIL
    :type          (or null Node)
    :documentation "The last node in the queue, if the same is not
                    empty.")
   (size
    :initform      0
    :type          (integer 0 *)
    :documentation "The number of elements in the queue."))
  (:documentation
    "The ``Queue'' class furnishes an implementation of the queue
     abstract data type (AST) by mediation of a singly linked list
     composed of nodes."))

;;; -------------------------------------------------------

(defun make-empty-queue ()
  "Creates and returns an empty ``Queue''."
  (the Queue
    (make-instance 'Queue)))

;;; -------------------------------------------------------

(defun empty-queue-p (queue)
  "Determines whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (get-boolean-value-of
      (zerop
        (slot-value queue 'size)))))

;;; -------------------------------------------------------

(defun queue-size (queue)
  "Returns the tally of elements partaking of this QUEUE."
  (declare (type Queue queue))
  (the (integer 0 *)
    (slot-value queue 'size)))

;;; -------------------------------------------------------

(defun enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the QUEUE's tail and returns no value."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (let ((new-node (make-node new-element NIL)))
    (declare (type Node new-node))
    (with-slots (head tail) queue
      (declare (type (or null Node) head))
      (declare (type (or null Node) tail))
      (if (empty-queue-p queue)
        (setf head             new-node)
        (setf (node-next tail) new-node))
      (setf tail new-node))
    (incf (slot-value queue 'size)))
  (values))

;;; -------------------------------------------------------

(defun dequeue (queue)
  "Removes and returns the QUEUE's front element, or responds with the
   default value of zero (0) upon its vacancy."
  (declare (type Queue queue))
  (the integer
    (if (empty-queue-p queue)
      0
      (with-slots (head tail size) queue
        (declare (type (or null Node) head))
        (declare (type (or null Node) tail))
        (declare (type (integer 0 *)  size))
        (prog1
          (node-element head)
          (setf head (node-next head))
          (decf size)
          (when (empty-queue-p queue)
            (setf tail NIL)))))))

;;; -------------------------------------------------------

(defun peek-queue (queue)
  "Returns without removing the QUEUE's front element, or responds with
   the default value of zero (0) upon its vacancy."
  (declare (type Queue queue))
  (the integer
    (if (empty-queue-p queue)
      0
      (node-element
        (slot-value queue 'head)))))

;;; -------------------------------------------------------

(defun clear-queue (queue)
  "Removes all elements from the QUEUE and returns no value."
  (declare (type Queue queue))
  (psetf (slot-value queue 'head) NIL
         (slot-value queue 'tail) NIL
         (slot-value queue 'size) 0)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((queue Queue) (stream T))
  (declare (type Queue       queue))
  (declare (type destination stream))
  (format stream "(Queue")
  (loop
    for current-node
      of-type (or null Node)
      =       (slot-value queue 'head)
      then    (node-next current-node)
    while current-node do
      (format stream " ~d"
        (node-element current-node)))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class represents a two-dimensional location defined
   by integer coordinates in the Cartesian space."
  (x 0 :type fixnum :read-only NIL)
  (y 0 :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defun set-location-coordinates (location new-x new-y)
  "Changes the LOCATION's x-coordinate to NEW-X and its y-coordinate to
   NEW-Y and returns no value."
  (declare (type Location location))
  (declare (type fixnum   new-x))
  (declare (type fixnum   new-y))
  (psetf (location-x location) new-x
         (location-y location) new-y)
  (values))

;;; -------------------------------------------------------

(defun copy-location-coordinates (location target)
  "Copies the coordinates of the TARGET to the LOCATION and returns no
   value."
  (declare (type Location location))
  (declare (type Location target))
  (psetf (location-x location) (location-x target)
         (location-y location) (location-y target))
  (values))

;;; -------------------------------------------------------

(defun move-location-left (location)
  "Moves the LOCATION one step to the left and returns the modified
   LOCATION."
  (declare (type Location location))
  (decf (location-x location))
  (the Location location))

;;; -------------------------------------------------------

(defun move-location-right (location)
  "Moves the LOCATION one step to the right and returns the modified
   LOCATION."
  (declare (type Location location))
  (incf (location-x location))
  (the Location location))

;;; -------------------------------------------------------

(defun move-location-down (location)
  "Moves the LOCATION one step down and returns the modified LOCATION."
  (declare (type Location location))
  (incf (location-y location))
  (the Location location))

;;; -------------------------------------------------------

(defun move-location-up (location)
  "Moves the LOCATION one step up and returns the modified LOCATION."
  (declare (type Location location))
  (decf (location-y location))
  (the Location location))

;;; -------------------------------------------------------

(defun get-location-below (location)
  "Creates and returns a new ``Location'' located one step below the
   LOCATION, while occupying the same horizontal space."
  (declare (type Location location))
  (the Location
    (make-location
      (location-x location)
      (1+ (location-y location)))))

;;; -------------------------------------------------------

(defun locations-concur-p (first-location second-location)
  "Determines whether the FIRST-LOCATION and the SECOND-LOCATION
   designate the same point in the two-dimensional Cartesian space,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Location first-location))
  (declare (type Location second-location))
  (the boolean
    (get-boolean-value-of
      (and
        (= (location-x first-location)
           (location-x second-location))
        (= (location-y first-location)
           (location-y second-location))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Board".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Board ()
  ((source
    :initarg       :source
    :initform      NIL
    :reader        board-source
    :type          string
    :documentation "The piece of Interlude source code to mold into a
                    veridical two-dimensional layout.")
   (width
    :initarg       :width
    :initform      0
    :accessor      board-width
    :type          fixnum
    :documentation "The number of columns.")
   (height
    :initarg       :height
    :initform      0
    :accessor      board-height
    :type          fixnum
    :documentation "The number of rows.")
   (cells
    :initarg       :cells
    :initform      (make-hash-table :test #'equalp)
    :accessor      board-cells
    :type          character-matrix
    :documentation "A sparse two-dimensional array of characters,
                    realized by a mapping of ``Location''s to character
                    objects."))
  (:documentation
    "The ``Board'' class provides a two-dimesional representation of an
     Interlude program where the non-linebreak characters are arranged
     in Cartesian order along columns and rows."))

;;; -------------------------------------------------------

(defun initialize-board-cell-at (board x y new-character)
  "Stores the NEW-CHARACTER in the BOARD cell located in the X-th column
   of the Y-th row, updates the BOARD's bournes, if necessitated by this
   modification, and returns no value."
  (declare (type Board     board))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (declare (type character new-character))
  (with-slots (cells width height) board
    (declare (type character-matrix cells))
    (declare (type fixnum           width))
    (declare (type fixnum           height))
    (psetf (gethash (make-location x y) cells) new-character
           width  (max width  (1+ x))
           height (max height (1+ y))))
  (values))

;;; -------------------------------------------------------

(defun populate-board (board)
  "Initalizes the BOARD's cells from its maintained source and returns
   no value."
  (declare (type Board board))
  (let ((x 0)
        (y 0))
    (declare (type fixnum x))
    (declare (type fixnum y))
    (loop
      for token    of-type character across (board-source board)
      and position of-type fixnum    from 0 by 1
      do
        (case token
          (#\Space
            (error "Space character encountered at position ~d."
              position))
          (#\Newline
            (setf x 0)
            (incf y 1))
          (otherwise
            (initialize-board-cell-at board x y token)
            (incf x 1)))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((board Board) &key)
  "Initializes the BOARD's cells with the symbols desumed from the
   supplied Interlude program and returns no value."
  (declare (type Board board))
  (populate-board board)
  (values))

;;; -------------------------------------------------------

(defun make-board (source)
  "Creates and returns a new ``Board'' representing a two-dimensional
   view on the piece of Interlude SOURCE code."
  (declare (type string source))
  (the Board
    (make-instance 'Board :source source)))

;;; -------------------------------------------------------

(defun board-at (board location)
  "Returns the character in the BOARD cell amenable to the LOCATION."
  (declare (type Board    board))
  (declare (type Location location))
  (the character
    (gethash location
      (board-cells board))))

;;; -------------------------------------------------------

(defun (setf board-at) (new-character board location)
  "Replaces the character in the BOARD cell amenable to the LOCATION by
   the NEW-CHARACTER and returns no value."
  (declare (type character new-character))
  (declare (type Board     board))
  (declare (type Location  location))
  (setf (gethash location (board-cells board))
        new-character)
  (values))

;;; -------------------------------------------------------

(defun empty-board-p (board)
  "Determines whether the BOARD is empty, which conflates with the
   diorism of its width, height, or both aspects being zero-valued,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Board board))
  (the boolean
    (get-boolean-value-of
      (or (zerop (board-width  board))
          (zerop (board-height board))))))

;;; -------------------------------------------------------

(defun valid-board-location-p (board probed-location)
  "Determines whether the PROBED-LOCATION answers to a valid position
   into the BOARD, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Board    board))
  (declare (type Location probed-location))
  (the boolean
    (get-boolean-value-of
      (with-slots (width height) board
        (declare (type fixnum width))
        (declare (type fixnum height))
        (and
          (<= 0 (location-x probed-location) (1- width))
          (<= 0 (location-y probed-location) (1- height)))))))

;;; -------------------------------------------------------

(defmethod print-object ((board Board) stream)
  (declare (type Board board))
  (with-slots (width height cells) board
    (declare (type fixnum     width))
    (declare (type fixnum     height))
    (declare (type hash-table cells))
    (format stream "~%~d x ~d board:" width height)
    (let ((current-location (make-location 0 0)))
      (declare (type Location current-location))
      (dotimes (row height)
        (declare (type fixnum row))
        (format stream "~%")
        (dotimes (column width)
          (declare (type fixnum column))
          (set-location-coordinates current-location column row)
          (format stream "~a"
            (gethash current-location cells)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Act".                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Act
  (:constructor make-act (board x-start x-end y-end)))
  "The ``Act'' class designs a column of a piece of Interlude code into
   which the total is split by mediation of vertical bars, and which
   defines an independent unit.
   ---
   An act's diorism amplects, besides the ensconcing the board, the
   inclusive index of the column commencing the act, the inclusive index
   of the desinent column submitted its occupancy, and the inclusive
   index of the bottom board row."
  (board   (error "Missing board.") :type Board  :read-only T)
  (x-start 0                        :type fixnum :read-only T)
  (x-end   0                        :type fixnum :read-only T)
  (y-end   0                        :type fixnum :read-only T))

;;; -------------------------------------------------------

(defun act-start-point (act)
  "Returns the ACT's start position."
  (declare (type Act act))
  (the Location
    (make-location
      (act-x-start act)
      0)))

;;; -------------------------------------------------------

(defun act-end-point (act)
  "Returns the ACT's desinent position."
  (declare (type Act act))
  (the Location
    (make-location
      (act-x-end act)
      (act-y-end act))))

;;; -------------------------------------------------------

(defun act-contains-point-p (act probed-point)
  "Determines whether the ACT's circumference comprehends the
   PROBED-POINT, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Act      act))
  (declare (type Location probed-point))
  (the boolean
    (get-boolean-value-of
      (and
        (<= (act-x-start act)
            (location-x  probed-point)
            (act-x-end   act))
        (<= 0
            (location-y  probed-point)
            (act-y-end   act))))))

;;; -------------------------------------------------------

(defun empty-act-p (act)
  "Determines whether the ACT is empty, that is, exhibits a column count
   of zero (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Act act))
  (the boolean
    (get-boolean-value-of
      (<= (act-x-end   act)
          (act-x-start act)))))

;;; -------------------------------------------------------

(defun empight-on-start-of-act (act location)
  "Relocates the LOCATION to the start coordinates of the ACT and
   returns no value."
  (declare (type Act      act))
  (declare (type Location location))
  (set-location-coordinates location
    (act-x-start act)
    0)
  (values))

;;; -------------------------------------------------------

(defun empight-on-start-of-act-line (act line-index location)
  "Relocates the LOCATION to the start coordinates of the ACT's row
   designated by the zero-based LINE-INDEX and returns no value."
  (declare (type Act      act))
  (declare (type fixnum   line-index))
  (declare (type Location location))
  (set-location-coordinates location
    (act-x-start act)
    line-index)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((act Act) (stream T))
  (declare (type Act         act))
  (declare (type destination stream))
  (format stream "(Act :x-start ~d :x-end ~d :y-end ~d)"
    (act-x-start act)
    (act-x-end   act)
    (act-y-end   act)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Act-List".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Act-List ()
  ((members
    :initarg       :members
    :initform      (error "Missing acts.")
    :reader        act-list-members
    :type          (simple-array Act (*))
    :documentation "A one-dimensional vector of the maintained acts.")
   (cursor
    :initform      0
    :accessor      act-list-cursor
    :type          fixnum
    :documentation "The currently selected act's zero-based position in
                    the MEMBERS vector."))
  (:documentation
    "The ``Act-List'' class is apportioned the dever of a random-access
     sequence of acts' castaldy."))

;;; -------------------------------------------------------

(defun make-act-list (acts)
  "Creates and returns a fresh ``Act-List'' whose members are specified
   by the ACTS."
  (declare (type (simple-array Act (*)) acts))
  (the Act-List
    (make-instance 'Act-List :members acts)))

;;; -------------------------------------------------------

(defun act-list-size (acts)
  "Returns the tally of members stored in the list of ACTS."
  (declare (type Act-List acts))
  (the fixnum
    (length (act-list-members acts))))

;;; -------------------------------------------------------

(defun has-next-act-p (acts)
  "Determines whether the cursor operating on the list of ACTS is
   capacitated to select the subsequent act, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-List acts))
  (the boolean
    (get-boolean-value-of
      (< (act-list-cursor acts)
         (1- (act-list-size acts))))))

;;; -------------------------------------------------------

(defun advance-to-next-act (acts)
  "Advances the ACTS list's cursor to the next act, if possible, and
   returns no value."
  (declare (type Act-List acts))
  (when (has-next-act-p acts)
    (incf (act-list-cursor acts)))
  (values))

;;; -------------------------------------------------------

(defun select-random-act (acts)
  "Empights the ACTS list's cursor on an aleatorily chosen act and
   returns no value."
  (declare (type Act-List acts))
  (setf (act-list-cursor acts)
    (random
      (act-list-size acts)))
  (values))

;;; -------------------------------------------------------

(defun select-act-at (acts index)
  "Empights the ACTS list's cursor on the act amenable to the one-based
   INDEX and returns no value."
  (declare (type Act-List acts))
  (declare (type fixnum   index))
  (if (plusp index)
    (setf (act-list-cursor acts)
      (1- index))
    (error "Cannot jump to the act at index ~d." index))
  (values))

;;; -------------------------------------------------------

(defun act-at-index (acts index)
  "Returns the act commorant at the INDEX in the list of ACTS."
  (declare (type Act-List acts))
  (declare (type fixnum   index))
  (the Act
    (aref (act-list-members acts) index)))

;;; -------------------------------------------------------

(defun current-act (acts)
  "Returns the currently selected member in the list of ACTS."
  (declare (type Act-List acts))
  (the Act
    (act-at-index acts
      (act-list-cursor acts))))

;;; -------------------------------------------------------

(defmethod print-object ((acts Act-List) (stream T))
  (declare (type Act-List    acts))
  (declare (type destination stream))
  (format stream "~&Act-List:")
  (dotimes (current-index (act-list-size acts))
    (declare (type fixnum current-index))
    (if (= current-index (act-list-cursor acts))
      (format stream "~&  > ")
      (format stream "~&    "))
    (format stream "[~d] ~a" current-index
      (act-at-index acts current-index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Act-Extractor".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Act-Extractor ()
  ((board
    :initarg       :board
    :initform      (error "Missing board.")
    :reader        extractor-board
    :type          Board
    :documentation "The character matrix whose acts shall be
                    extracted.")
   (location
    :initform      (make-location 0 0)
    :accessor      extractor-location
    :type          Location
    :documentation "The current position into the BOARD.")
   (completed-p
    :initform      NIL
    :accessor      extraction-completed-p
    :type          boolean
    :documentation "Determines whether the act extraction process is
                    finished, either because of the BOARD being empty,
                    or as an ultimity of its desinent cell's
                    traversal.")
   (act-bournes
    :initform      NIL
    :accessor      extractor-act-bournes
    :type          (list-of fixnum)
    :documentation "Collects the column indices designating the acts'
                    commencement and termination in the board."))
  (:documentation
    "The ``Act-Extractor'' class serves in the extraction of the
     entailed acts from an Interlude program's ``Board''
     representation, applying itself in a parhedral office to the
     validation of the thus established acts' conformation."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((extractor Act-Extractor) &key)
  "Determines whether the EXTRACTOR's evaluation is already finished by
   operating on an empty board, initializes its completion flag
   accordingly, and returns no value."
  (declare (type Act-Extractor extractor))
  (setf (extraction-completed-p extractor)
    (empty-board-p
      (extractor-board extractor)))
  (values))

;;; -------------------------------------------------------

(defun make-act-extractor (board)
  "Creates and returns a fresh ``Act-Extractor'' dedicated to the
   BOARD's evaluation."
  (declare (type Board board))
  (the Act-Extractor
    (make-instance 'Act-Extractor :board board)))

;;; -------------------------------------------------------

(defun currently-extracted-character (extractor)
  "Returns the act EXTRACTOR's currently processed character in its
   board."
  (declare (type Act-Extractor extractor))
  (the character
    (board-at
      (extractor-board    extractor)
      (extractor-location extractor))))

;;; -------------------------------------------------------

(defun separator-encountered-p (extractor)
  "Determines whether the act EXTRACTOR currently resides on an act
   separator character (\"|\") in its board, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Extractor extractor))
  (the boolean
    (get-boolean-value-of
      (char= (currently-extracted-character extractor) #\|))))

;;; -------------------------------------------------------

(defun extractor-location-x (extractor)
  "Returns the column index of the act EXTRACTOR's current position."
  (declare (type Act-Extractor extractor))
  (the fixnum
    (location-x
      (extractor-location extractor))))

;;; -------------------------------------------------------

(defun extractor-location-y (extractor)
  "Returns the row index of the act EXTRACTOR's current position."
  (declare (type Act-Extractor extractor))
  (the fixnum
    (location-y
      (extractor-location extractor))))

;;; -------------------------------------------------------

(defun move-extractor-to-next-row (extractor)
  "Advances the act EXTRACTOR to the incipiency of the new row of its
   evaluated board and returns no value."
  (declare (type Act-Extractor extractor))
  (with-slots (location) extractor
    (declare (type Location location))
    (setf (location-x location) 0)
    (move-location-down location))
  (values))

;;; -------------------------------------------------------

(defun extractor-has-completed-current-row-p (extractor)
  "Determines whether the act EXTRACTOR has finished its board's
   currently processed row, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Act-Extractor extractor))
  (the boolean
    (with-slots (location board) extractor
      (get-boolean-value-of
        (>= (extractor-location-x extractor)
            (board-width board))))))

;;; -------------------------------------------------------

(defun advance-extractor (extractor)
  "Advances the act EXTRACTOR to the next position in its board and
   returns no value."
  (declare (type Act-Extractor extractor))
  (move-location-right
    (extractor-location extractor))
  (when (extractor-has-completed-current-row-p extractor)
    (move-extractor-to-next-row extractor))
  (setf (extraction-completed-p extractor)
    (not
      (valid-board-location-p
        (extractor-board    extractor)
        (extractor-location extractor))))
  (values))

;;; -------------------------------------------------------

(defun extractor-resides-in-first-row-p (extractor)
  "Determines whether the act EXTRACTOR currently resides in its board's
   first row, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Act-Extractor extractor))
  (the boolean
    (get-boolean-value-of
      (zerop
        (extractor-location-y extractor)))))

;;; -------------------------------------------------------

(defun store-act-bourne (extractor column-index)
  "Appends the COLUMN-INDEX to the act EXTRACTOR's act bournes and
   returns no value."
  (declare (type Act-Extractor extractor))
  (declare (type fixnum        column-index))
  (push column-index
    (extractor-act-bournes extractor))
  (values))

;;; -------------------------------------------------------

(defun contains-act-bourne-p (extractor probed-column-index)
  "Determines whether the act EXTRACTOR entails in its maintained act
   bournes the PROBED-COLUMN-INDEX, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Extractor extractor))
  (declare (type fixnum        probed-column-index))
  (the boolean
    (get-boolean-value-of
      (find probed-column-index
        (extractor-act-bournes extractor)
        :test #'=))))

;;; -------------------------------------------------------

(defun finalize-act-bournes (extractor)
  "Concludes the act EXTRACTOR's bournes by inserting at the incipient
   and desinent positions the zero (0) index and the underlying board's
   width respectively and returns no value."
  (declare (type Act-Extractor extractor))
  (store-act-bourne extractor
    (board-width
      (extractor-board extractor)))
  (setf (extractor-act-bournes extractor)
    (nreverse (extractor-act-bournes extractor)))
  (push 0 (extractor-act-bournes extractor))
  (values))

;;; -------------------------------------------------------

(defun make-vector-of-acts (list-of-acts)
  "Creates and returns a fresh one-dimensional simple array of ``Act''
   objects desumed from the LIST-OF-ACTS."
  (declare (type (list-of Act) list-of-acts))
  (the (simple-array Act (*))
    (coerce list-of-acts
      '(simple-array Act (*)))))

;;; -------------------------------------------------------

(defun generate-acts-from-bournes (extractor)
  "Generates, proceeding from the gathered act column indices, the
   actual ``Act'' instances and returns the same in a fresh ``Act-List''
   instance."
  (declare (type Act-Extractor extractor))
  (let ((act-y-end (1- (board-height (extractor-board extractor)))))
    (declare (type fixnum act-y-end))
    (the Act-List
      (make-act-list
        (make-vector-of-acts
          (loop
            while (extractor-act-bournes extractor)
            
            for act-x-start
              of-type fixnum
              =       (pop (extractor-act-bournes extractor))
              then    (1+ act-x-end)
            
            for act-x-end
              of-type fixnum
              =       (pop (extractor-act-bournes extractor))
            
            collect
              (make-act
                (extractor-board extractor)
                act-x-start
                (1- act-x-end)
                act-y-end)))))))

;;; -------------------------------------------------------

(defun extract-acts (extractor)
  "Extracts from the board supplied to the act EXTRACTOR the entailed
   acts and returns an ``Act-List'' collection thereof."
  (declare (type Act-Extractor extractor))
  
  (loop until (extraction-completed-p extractor) do
    (cond
      ;; First board row?
      ;; => Collect act separator ("|") position.
      ((and (separator-encountered-p extractor)
            (extractor-resides-in-first-row-p extractor))
        (store-act-bourne extractor
          (extractor-location-x extractor)))
      
      ;; Separator in subsequent row detected?
      ;; => Probe if aligns with the recognized act bournes.
      ((separator-encountered-p extractor)
        (unless (contains-act-bourne-p extractor
                  (extractor-location-x extractor))
          (error "Unexpected act separator at position (x=~d, y=~d)."
            (extractor-location-x extractor)
            (extractor-location-y extractor))))
      
      ;; Act bourne encountered?
      ;; => Probe if expected separator ("|") continues in this column.
      ((and (contains-act-bourne-p extractor
              (extractor-location-x extractor))
            (not (separator-encountered-p extractor)))
        (error "Broken act separator at position (x=~d, y=~d)."
            (extractor-location-x extractor)
            (extractor-location-y extractor)))
      
      ;; Any other character is ignored.
      (T
        NIL))
      (advance-extractor extractor))
  
  (finalize-act-bournes extractor)
  
  (the Act-List
    (generate-acts-from-bournes extractor)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenates the list of LINES into a single string, each twissel's
   intermede conjoined by a single newline character, and returns the
   resulting string."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~a~^~%~}" lines)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Act-Navigator".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Act-Navigator)
  "The ``Act-Navigator'' class applies itself to the perambulation
   across a single act's enclosed area."
  (act      NIL                 :type (or null Act) :read-only NIL)
  (location (make-location 0 0) :type Location      :read-only T))

;;; -------------------------------------------------------

(defun act-navigator-left-margin (navigator)
  "Returns the x-coordinate demarcating the act NAVIGATOR act's
   sinistral march, or start."
  (declare (type Act-Navigator navigator))
  (the fixnum
    (act-x-start
      (act-navigator-act navigator))))

;;; -------------------------------------------------------

(defun act-navigator-right-margin (navigator)
  "Returns the x-coordinate demarcating the act NAVIGATOR act's dextral
   march, or end."
  (declare (type Act-Navigator navigator))
  (the fixnum
    (act-x-end
      (act-navigator-act navigator))))

;;; -------------------------------------------------------

(defun act-navigator-bottom-margin (navigator)
  "Returns the y-coordinate demarcating the act NAVIGATOR act's bottom
   march."
  (declare (type Act-Navigator navigator))
  (the fixnum
    (act-y-end
      (act-navigator-act navigator))))

;;; -------------------------------------------------------

(defun act-navigator-column-index (navigator)
  "Returns the zero-based index of the act column currently occupied by
   the act NAVIGATOR."
  (declare (type Act-Navigator navigator))
  (the fixnum
    (location-x
      (act-navigator-location navigator))))

;;; -------------------------------------------------------

(defun act-navigator-line-index (navigator)
  "Returns the zero-based index of the act row currently occupied by the
   act NAVIGATOR."
  (declare (type Act-Navigator navigator))
  (the fixnum
    (location-y
      (act-navigator-location navigator))))

;;; -------------------------------------------------------

(defun act-navigator-board (navigator)
  "Returns the board occupied the act NAVIGATOR's currently set act."
  (declare (type Act-Navigator navigator))
  (the (or null Board)
    (and
      (act-navigator-act navigator)
      (act-board
        (act-navigator-act navigator)))))

;;; -------------------------------------------------------

(defun change-act-navigator-act (navigator new-act)
  "Changes the act NAVIGATOR's cynosure to the NEW-ACT, resets its
   state, and returns no value."
  (declare (type Act-Navigator navigator))
  (declare (type Act           new-act))
  (setf (act-navigator-act navigator) new-act)
  (empight-on-start-of-act new-act
    (act-navigator-location navigator))
  (values))

;;; -------------------------------------------------------

(defun move-act-navigator-to-xy (navigator new-x new-y)
  "Relocates the act NAVIGATOR to the location specified by the NEW-X
   and NEW-Y coordinates and returns no value."
  (declare (type Act-Navigator navigator))
  (declare (type fixnum        new-x))
  (declare (type fixnum        new-y))
  (set-location-coordinates
    (act-navigator-location navigator)
    new-x new-y)
  (values))

;;; -------------------------------------------------------

(defun move-act-navigator-to-location (navigator new-location)
  "Relocates the act NAVIGATOR to the location specified by the
   NEW-LOCATION and returns no value.
   ---
   Please heed that the NEW-LOCATION's coordinates will be copied into
   the NAVIGATOR, discarding any reference to thilk."
  (move-act-navigator-to-xy navigator
    (location-x new-location)
    (location-y new-location))
  (values))

;;; -------------------------------------------------------

(defun move-act-navigator-forward (navigator)
  "Relocates the act NAVIGATOR one step to the right and returns no
   value."
  (declare (type Act-Navigator navigator))
  (move-location-right
    (act-navigator-location navigator))
  (values))

;;; -------------------------------------------------------

(defun has-next-act-cell-p (navigator)
  "Determines whether there remains for the act NAVIGATOR at least one
   cell in its traversed act in a forward direction, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Navigator navigator))
  (the boolean
    (act-contains-point-p
      (act-navigator-act      navigator)
      (act-navigator-location navigator))))

;;; -------------------------------------------------------

(defun shall-navigate-to-next-row-p (navigator)
  "Determines whether the act NAVIGATOR currently resides in its
   traversed act's most dextral column, which would necessitate a
   descent into the start of its act's next lower line, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Navigator navigator))
  (the boolean
    (get-boolean-value-of
      (>= (act-navigator-column-index navigator)
          (act-navigator-right-margin navigator)))))

;;; -------------------------------------------------------

(defun advance-act-navigator (navigator)
  "Advances the act NAVIGATOR to its act's next cell, if possible, and
   returns no value."
  (declare (type Act-Navigator navigator))
  (cond
    ((not (has-next-act-cell-p navigator))
      (error "Act navigator has reached the end of its act."))
    ((shall-navigate-to-next-row-p navigator)
      (move-act-navigator-to-xy navigator
        (act-navigator-left-margin navigator)
        (1+ (act-navigator-line-index navigator))))
    (T
      (move-act-navigator-forward navigator)))
  (values))

;;; -------------------------------------------------------

(defun current-token (navigator)
  "Returns the character stored in the act NAVIGATOR's currently
   selected cell."
  (declare (type Act-Navigator navigator))
  (the character
    (board-at
      (act-navigator-board    navigator)
      (act-navigator-location navigator))))

;;; -------------------------------------------------------

(defun current-token-equals-p (navigator expected-character)
  "Determines whether the act NAVIGATOR's currently selected token
   equals the EXPECTED-CHARACTER, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Navigator navigator))
  (declare (type character     expected-character))
  (the boolean
    (get-boolean-value-of
      (char= (current-token navigator) expected-character))))

;;; -------------------------------------------------------

(defun current-token-satisfies-p (navigator predicate)
  "Determines whether the act NAVIGATOR's currently selected token
   satisfies the PREDICATE, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''.
   ---
   The PREDICATE ought to establish a monadic function which accepts as
   its aefauld input the character in the current NAVIGATOR cell, and
   responds with a \"generalized boolean\" value of \"true\" if the
   probed character was capacitated to satisfy the PREDICATE; otherwise
   returning \"false\"."
  (declare (type Act-Navigator            navigator))
  (declare (type (function (character) *) predicate))
  (the boolean
    (get-boolean-value-of
      (funcall predicate (current-token navigator)))))

;;; -------------------------------------------------------

(defun current-token-is-alphanumeric-p (navigator)
  "Determines whether the act NAVIGATOR's currently selected token
   represents an alphanumeric character, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Navigator navigator))
  (the boolean
    (current-token-satisfies-p navigator #'alphanumericp)))

;;; -------------------------------------------------------

(defun current-token-designates-instruction-p (navigator)
  "Determines whether the act NAVIGATOR's currently selected token
   introduces an Interlude instruction, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Act-Navigator navigator))
  (the boolean
    (get-boolean-value-of
      (find (current-token navigator) "v$+*?:" :test #'char=))))

;;; -------------------------------------------------------

(defun can-move-down-in-act-p (navigator)
  "Determines whether the act NAVIGATOR is capacitated to relocate into
   the line below the current row, returning on confirmation a
   ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type Act-Navigator navigator))
  (the boolean
    (get-boolean-value-of
      (< (location-y (act-navigator-location navigator))
         (act-navigator-bottom-margin navigator)))))

;;; -------------------------------------------------------

(defun move-down-in-act (navigator)
  "Relocates the act NAVIGATOR to the leftmost position of the line
   below the current row in its act and returns no value."
  (declare (type Act-Navigator navigator))
  (empight-on-start-of-act-line
    (act-navigator-act navigator)
    (1+ (act-navigator-line-index navigator))
    (act-navigator-location navigator))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of act travel book.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Travel-Book
  (:constructor make-travel-book ()))
  "The ``Travel-Book'' class serves in the memorization of a program's
   visited acts by mediation of their one-based indices; in this agency
   capacitating the validation of the Interlude programming language's
   aspect that imposes a minimum tally of two sojourned acts during the
   execution."
  (visited-indices NIL :type index-list :read-only NIL))

;;; -------------------------------------------------------

(defun register-visited-act (travel-book visited-act-index)
  "Memorizes the VISITED-ACT-INDEX in the TRAVEL-BOOK, if not having
   been acquainted yet, and returns no value."
  (declare (type Travel-Book   travel-book))
  (declare (type (integer 1 *) visited-act-index))
  (setf (travel-book-visited-indices travel-book)
    (adjoin visited-act-index
      (travel-book-visited-indices travel-book)
      :test #'=))
  (values))

;;; -------------------------------------------------------

(defun count-visited-acts (travel-book)
  "Returns the tally of distinct visited acts according to the
   TRAVEL-BOOK."
  (declare (type Travel-Book travel-book))
  (the (integer 0 *)
    (length
      (travel-book-visited-indices travel-book))))

;;; -------------------------------------------------------

(defun has-visited-at-least-two-acts-p (travel-book)
  "Determines whether the TRAVEL-BOOK contains at least two distinct
   visited acts, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Travel-Book travel-book))
  (the boolean
    (get-boolean-value-of
      (>= (count-visited-acts travel-book)
          2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Too-Few-Entered-Acts-Error (error)
  ((number-of-acts
    :initarg       :number-of-acts
    :initform      (error "Missing number of entered acts.")
    :reader        too-few-entered-acts-error-number-of-acts
    :type          (integer 0 1)
    :documentation "The illegal tally of entered acts."))
  (:report
    (lambda (condition stream)
      (declare (type Too-Few-Entered-Acts-Error condition))
      (declare (type destination                stream))
      (format stream "Too few acts have been entered in the course ~
                      of the program: Expected two or more, but ~
                      tallied merely ~d."
        (too-few-entered-acts-error-number-of-acts condition))))
  (:documentation
    "The ``Too-Few-Entered-Acts-Error'' condition type serves in the
     apprizal about a program's conclusion with a tally of entered acts
     below the imposed minimum."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction)
  "The ``Instruction'' abstract class furnishes a common foundry for all
   concrete representations of Interlude instructions in an organized
   composite, their commonality realized in their two-dimensional
   inclusive start location inside of the ensconcing Interlude program
   grid and their inclusive end position."
  (start-point (error "Missing start point.")
               :type      Location
               :read-only T)
  (end-point   (error "Missing end point.")
               :type      Location
               :read-only T))

;;; -------------------------------------------------------

;; *  {digit}
;; *  {letter}
;; *- {digit}
;; *- {letter}
(defstruct (Enqueue-Instruction
  (:include     Instruction)
  (:constructor make-enqueue-instruction (value start-point end-point)))
  "The ``Input-Instruction'' class serves in the representation of a
   variety of the Interlude operation \"*\" dedicated to the enqueuing
   of a numeric value into the memory queue."
  (value (error "Missing value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

;; *  {non-alphanumeric}
;; *- {non-alphanumeric}
(defstruct (Dequeue-Instruction
  (:include     Instruction)
  (:constructor make-dequeue-instruction (enqueue-zero-p
                                          start-point
                                          end-point)))
  "The ``Dequeue-Instruction'' class serves in the representation of a
   variety of the Interlude operation \"*\" dedicated to the dequeuing
   of the memory queue's front element, optionally succeeded by an
   enqueuing of the value zero (0)."
  (enqueue-zero-p (error "Missing enqueue zero flag.")
                  :type      boolean
                  :read-only T))

;;; -------------------------------------------------------

;; ?.
(defstruct (Input-Instruction
  (:include     Instruction)
  (:constructor make-input-instruction (start-point end-point)))
  "The ``Input-Instruction'' class serves in the representation of a
   variety of the Interlude operation \"?\" dedicated to the reception
   of user input in character form.")

;;; -------------------------------------------------------

;; ? {command-token}
(defstruct (Skip-Instruction
  (:include     Instruction)
  (:constructor make-skip-instruction (start-point end-point)))
  "The ``Skip-Instruction'' class serves in the representation of a
   variety of the Interlude operation \"?\" dedicated to the conditional
   omission of the subsequent instruction.")

;;; -------------------------------------------------------

;; ? {non-period-and-non-command-token}
(defstruct (Purge-Instruction
  (:include     Instruction)
  (:constructor make-purge-instruction (start-point end-point)))
  "The ``Purge-Instruction'' class serves in the representation of a
   variety of the Interlude operation \"?\" dedicated to the clearance
   of the memory queue.")

;;; -------------------------------------------------------

;; +
(defstruct (Sum/Print-Instruction
  (:include     Instruction)
  (:constructor make-sum/print-instruction (start-point end-point)))
  "The ``Sum/Print-Instruction'' class serves in the representation of
   the Interlude operation \"+\", nuncupated to the twifaced service of
   either a specific memory queue tmema's summation, or, alternatively,
   one of its element's printing.")

;;; -------------------------------------------------------

;; $
(defstruct (Jump-Instruction
  (:include     Instruction)
  (:constructor make-jump-instruction (start-point end-point)))
  "The ``Jump-Instruction'' class serves in the representation of the
   Interlude operation \"$\", nuncupated to the service of the
   instruction pointer's (IP) relocation to the inchoation of an act
   determined by the memory queue's front element.")

;;; -------------------------------------------------------

;; v
(defstruct (Move-Down-Instruction
  (:include     Instruction)
  (:constructor make-move-down-instruction (start-point end-point)))
  "The ``Move-Down-Instruction'' class serves in the representation of
   the Interlude operation \"v\", nuncupated to the service of the
   instruction pointer's (IP) downward relocation to the next lower
   act line's incipiency, or, upon the act bourne's transcendence, the
   program execution's cessation.")

;;; -------------------------------------------------------

;; :
(defstruct (Error-Instruction
  (:include     Instruction)
  (:constructor make-error-instruction (start-point end-point)))
  "The ``Error-Instruction'' class serves in the representation of
   the Interlude operation \":\", nuncupated to the service of an
   error's instigation.")

;;; -------------------------------------------------------

;; {non-command-alphanumeric-character}
(defstruct (Literal-Instruction
  (:include     Instruction)
  (:constructor make-literal-instruction (token
                                          start-point
                                          end-point)))
  "The ``Literal-Instruction'' class serves in the representation of
   an Interlude statement incited by an alphanumeric character, serving
   to print its verbatim form to the standard output."
  (token (error "Missing token.") :type character :read-only T))

;;; -------------------------------------------------------

;; {non-command-and-non-alphanumeric-character}
(defstruct (Random-Instruction
  (:include     Instruction)
  (:constructor make-random-instruction (token
                                         start-point
                                         end-point)))
  "The ``Random-Instruction'' class serves in the representation of
   an Interlude statement incited by a non-command and non-alphanumeric
   character, enqueue the number 78 into the memory queue, relocate the
   instruction pointer (IP) to the inchoation of a randomly selected
   act, and modify the cell immediately alow this new IP position to the
   downward movement instruction token \"v\"."
  (token (error "Missing token.") :type character :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-numeric-value-of-token (token)
  "Returns the numeric value appertaining to the TOKEN, producing for
   a digit character the respective decimal number, and for a Latin
   letter the corresponding ASCII code; otherwise signals an error of
   an unspecified type."
  (declare (type character token))
  (the fixnum
    (or
      (digit-char-p token)
      (and (alpha-char-p token)
           (char-code    token))
      (error "The character \"~c\" cannot be interpreted as a digit."
        token))))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((board
    :initarg       :board
    :initform      (error "Missing board.")
    :documentation "The board providing the acts to process.")
   (acts
    :type          Act-List
    :documentation "An ordered sequence of the acts comprising the
                    BOARD.")
   (navigator
    :type          Act-Navigator
    :documentation "The currently selected act's navigator.")
   (execution-completed-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the program has been completed
                    as a consectary of its current act's exhaustion.")
   (memory
    :initform      (make-empty-queue)
    :type          Queue
    :documentation "The program memory as a queue object.")
   (travel-book
    :initform      (make-travel-book)
    :type          travel-book
    :documentation "A set comprehending the one-based indices of the
                    acts hitherto traversed during the program's
                    execution; serving to determine whether the least
                    account of two acts has been sojourned at the
                    instant of the program's cessation."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of accompassing
     actual efficacy to an Interlude program supplied in the static form
     of a one-dimensional string."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots, evaluates the BODY forms,
   and returns the desinent form's results.
   ---
   The following affiliations betwixt the INTERPRETER's slot names and
   tantamount symbol macros are reified in their establishment:
     -----------------------------------------------
     Slot name             | Symbol macro
     ----------------------+------------------------
     acts                  | $acts
     ...............................................
     board                 | $board
     ...............................................
     execution-completed-p | $execution-completed-p
     ...............................................
     memory                | $memory
     ...............................................
     navigator             | $navigator
     ...............................................
     travel-book           | $travel-book
     -----------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($board
              (the Board
                (slot-value ,evaluated-interpreter 'board)))
            ($acts
              (the Act-List
                (slot-value ,evaluated-interpreter 'acts)))
            ($navigator
              (the Act-Navigator
                (slot-value ,evaluated-interpreter 'navigator)))
            ($execution-completed-p
              (the boolean
                (slot-value ,evaluated-interpreter
                  'execution-completed-p)))
            ($memory
              (the Queue
                (slot-value ,evaluated-interpreter 'memory)))
            ($travel-book
              (the Travel-Book
                (slot-value ,evaluated-interpreter 'travel-book))))
         (declare (type Board         $board)
                  (ignorable          $board))
         (declare (type Act-List      $acts)
                  (ignorable          $acts))
         (declare (type Act-Navigator $navigator)
                  (ignorable          $navigator))
         (declare (type boolean       $execution-completed-p)
                  (ignorable          $execution-completed-p))
         (declare (type Queue         $memory)
                  (ignorable          $memory))
         (declare (type Travel-Book   $travel-book)
                  (ignorable          $travel-book))
         ,@body))))

;;; -------------------------------------------------------

(defun memorize-current-act (interpreter)
  "Memorizes the one-based index of the INTERPRETER's currently
   processed act and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (register-visited-act $travel-book
      (1+ (act-list-cursor $acts))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the INTERPRETER's act list and its navigator, exercises
   the parasceves for the requisite random number generator, and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (cond
      ((empty-board-p $board)
        (error 'Too-Few-Entered-Acts-Error :number-of-acts 0))
      (T
        (psetf
          $acts
            (extract-acts
              (make-act-extractor $board))
          $navigator
            (make-act-navigator))
        (change-act-navigator-act $navigator
          (current-act $acts))
        (memorize-current-act interpreter))))
  (setf *random-state*
    (make-random-state T))
  (values))

;;; -------------------------------------------------------

(defun current-location (interpreter)
  "Returns the position in the INTERPRETER's board currently occupied
   by its navigator."
  (declare (type Interpreter interpreter))
  (the Location
    (with-interpreter (interpreter)
      (act-navigator-location $navigator))))

;;; -------------------------------------------------------

(defun copy-of-current-location (interpreter)
  "Returns the position in the INTERPRETER's board currently occupied
   by its navigator."
  (declare (type Interpreter interpreter))
  (the Location
    (with-interpreter (interpreter)
      (copy-location
        (act-navigator-location $navigator)))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's navigator to the next cell in the
   currently active act, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (advance-act-navigator $navigator))
  (values))

;;; -------------------------------------------------------

(defun move-past-current-location (interpreter)
  "Advances the INTERPRETER's navigator to the next cell in the
   currently active act, if possible, otherwise designates the program
   execution as completed, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (if (has-next-act-cell-p $navigator)
      (advance-program interpreter)
      (setf $execution-completed-p T)))
  (values))

;;; -------------------------------------------------------

(defun move-past-instruction (interpreter instruction)
  "Relocates the INTERPRETER's instruction pointer (IP) to the position
   immediately succeeding the INSTRUCTION's end point, concomitantly,
   if eventuating the current act's exhaustion, designating the
   Interlude program as completed, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (with-interpreter (interpreter)
    (move-act-navigator-to-location $navigator
      (instruction-end-point instruction))
    (if (has-next-act-cell-p $navigator)
      (advance-program interpreter)
      (setf $execution-completed-p T)))
  (values))

;;; -------------------------------------------------------

(defgeneric extract-instruction (interpreter identifier)
  (:documentation
    "Evaluates the IDENTIFIER character in the INTERPRETER's context,
     detecting, proceeding from its current instruction pointer (IP)
     location, the nearest instruction, and returns an ``Instruction''
     representation of its content.
     ---
     The instruction pointer (IP), embraced in the INTERPRETER's
     ``Act-Navigator'', at this operation's conclusion, must be
     relocated to its state at the invocation's inchoation."))

;;; -------------------------------------------------------

(defun extract-next-instruction (interpreter)
  "Evaluates the INTERPRETER's currently selected character and returns
   an ``Instruction'' representation of the thus introduced Interlude
   operation."
  (declare (type Interpreter interpreter))
  (the Instruction
    (with-interpreter (interpreter)
      (extract-instruction interpreter
        (current-token $navigator)))))

;;; -------------------------------------------------------

(defmacro define-instruction-extractor (identifier (interpreter-name)
                                        &body body)
  "Defines an implementation of the generic function
   ``extract-instruction'', the first formal parameter's agnomination
   derives from the INTERPRETER-NAME, specializing on the
   ``Interpreter'', while the second assumes an automatically generated
   agnomination, its specialization an ``eql''-specimen on the
   IDENTIFIER character, the method body ensconcing the BODY forms in
   an implicitly accommodated ``with-interpreter'' macro invocation,
   returning the desinent form's results."
  (let ((identifier-name (gensym)))
    (declare (type symbol identifier-name))
    `(defmethod extract-instruction
         ((,interpreter-name Interpreter)
          (,identifier-name  (eql ,identifier)))
       (declare (type Interpreter ,interpreter-name))
       (declare (ignorable        ,interpreter-name))
       (declare (type character   ,identifier-name))
       (declare (ignore           ,identifier-name))
       (with-interpreter (,interpreter-name)
         ,@body))))

;;; -------------------------------------------------------

(define-instruction-extractor #\v (interpreter)
  (let ((start-point (copy-of-current-location interpreter)))
    (declare (type Location start-point))
    (the Move-Down-Instruction
      (make-move-down-instruction start-point start-point))))

;;; -------------------------------------------------------

(define-instruction-extractor #\$ (interpreter)
  (let ((start-point (copy-of-current-location interpreter)))
    (declare (type Location start-point))
    (the Jump-Instruction
      (make-jump-instruction start-point start-point))))

;;; -------------------------------------------------------

(define-instruction-extractor #\+ (interpreter)
  (let ((start-point (copy-of-current-location interpreter)))
    (declare (type Location start-point))
    (the Sum/Print-Instruction
      (make-sum/print-instruction start-point start-point))))

;;; -------------------------------------------------------

(define-instruction-extractor #\* (interpreter)
  (let ((start-point (copy-of-current-location interpreter)))
    (declare (type Location start-point))
    (advance-act-navigator $navigator)
    (prog1
      (cond
        ;; * {end-of-act}
        ((not (has-next-act-cell-p $navigator))
          (error "Missing operand for the \"*\" instruction at ~
                  position ~a."
            (current-location interpreter)))
        
        ;; * {digit}
        ;; * {letter}
        ((current-token-is-alphanumeric-p $navigator)
          (make-enqueue-instruction
            (get-numeric-value-of-token
              (current-token $navigator))
            start-point
            (copy-of-current-location interpreter)))
        
        ;; *- [...]
        ((current-token-equals-p $navigator #\-)
          (advance-act-navigator $navigator)
          (cond
            ;; *- {end-of-act}
            ((not (has-next-act-cell-p $navigator))
              (error "Missing operand for the \"*\" instruction at ~
                      position ~a."
                (current-location interpreter)))
            
            ;; *- {digit}
            ;; *- {letter}
            ((current-token-is-alphanumeric-p $navigator)
              (make-enqueue-instruction
                (- (get-numeric-value-of-token
                     (current-token $navigator)))
                start-point
                (copy-of-current-location interpreter)))
            
            ;; *- {non-alphanumeric-character}
            (T
              (make-dequeue-instruction T start-point
                (copy-of-current-location interpreter)))))
        
        ;; * {non-alphanumeric-or-minus-character}
        (T
          (make-dequeue-instruction NIL start-point
            (copy-of-current-location interpreter))))
      
      (move-act-navigator-to-location $navigator start-point))))

;;; -------------------------------------------------------

(define-instruction-extractor #\? (interpreter)
  (the (or Input-Instruction Purge-Instruction Skip-Instruction)
    (let ((start-point (copy-of-current-location interpreter)))
      (declare (type Location start-point))
      (advance-program interpreter)
      
      (cond
        ;; ? {end-of-act}
        ((not (has-next-act-cell-p $navigator))
          (error "Missing operand for the \"?\" instruction at ~
                  position ~a."
            (current-location interpreter)))
        
        ;; ?.
        ((current-token-equals-p $navigator #\.)
          (prog1
            (make-input-instruction start-point
              (copy-of-current-location interpreter))
            (move-act-navigator-to-location $navigator start-point)))
        
        ;; ? {command}
        ((current-token-designates-instruction-p $navigator)
          (prog1
            (make-skip-instruction start-point
              (instruction-end-point
                (extract-next-instruction interpreter)))
            (move-act-navigator-to-location $navigator start-point)))
        
        ;; ? {non-dot-and-non-command-character}
        (T
          (prog1
            (make-purge-instruction start-point
              (copy-of-current-location interpreter))
            (move-act-navigator-to-location
              $navigator
              start-point)))))))

;;; -------------------------------------------------------

(define-instruction-extractor #\: (interpreter)
  (let ((start-point (copy-of-current-location interpreter)))
    (declare (type Location start-point))
    (the Error-Instruction
      (make-error-instruction start-point start-point))))

;;; -------------------------------------------------------

(defmethod extract-instruction ((interpreter Interpreter)
                                (identifier  character))
  "Evaluates a non-command IDENTIFIER in the INTERPRETER's context and
   returns either a ``Literal-Instruction'' representation for an
   alphanumeric character, or a ``Random-Instruction'' in the case of
   any other symbol."
  (declare (type Interpreter interpreter))
  (declare (type character   identifier))
  (let ((start-point (copy-of-current-location interpreter)))
    (declare (type Location start-point))
    (with-interpreter (interpreter)
      (the (or Literal-Instruction Random-Instruction)
        (cond
          ((char= identifier #\Space)
            (error "Invalid space character at position ~a."
              (current-location interpreter)))
          ((alphanumericp identifier)
            (make-literal-instruction identifier
              start-point start-point))
          (T
            (make-random-instruction
              identifier
              start-point
              start-point)))))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor (instruction-class &body body)
  "Defines an implementation of the generic function
   ``process-instruction'', its first formal parameter's agnomination
   fixated as ``$interpreter'', specializing on the ``Interpreter''
   class, the second the ``$instruction'' identifier's dation,
   specializing and dispatching on the INSTRUCTION-CLASS, the method
   implementation ensconcing the BODY forms in an implicitly supplied
   ``with-interpreter'' context, returning no value."
  `(defmethod process-instruction
       (($interpreter Interpreter)
        ($instruction ,instruction-class))
     (declare (type Interpreter        $interpreter))
     (declare (type ,instruction-class $instruction)
              (ignorable               $instruction))
     (with-interpreter ($interpreter)
       ,@body)
     (values)))

;;; -------------------------------------------------------

;; v
(define-instruction-processor Move-Down-Instruction
  (if (can-move-down-in-act-p $navigator)
    (move-down-in-act $navigator)
    (setf $execution-completed-p T)))

;;; -------------------------------------------------------

;; $
(define-instruction-processor Jump-Instruction
  (select-act-at $acts
    (dequeue $memory))
  (change-act-navigator-act $navigator
    (current-act $acts))
  (memorize-current-act $interpreter))

;;; -------------------------------------------------------

;; +
(define-instruction-processor Sum/Print-Instruction
  (let ((discriminator (dequeue $memory)))
    (declare (type integer discriminator))
    (cond
      ((plusp discriminator)
        (enqueue $memory
          (loop
            repeat discriminator
            sum    (dequeue $memory))))
      (T
        (format T "~c"
          (code-char
            (dequeue $memory)))
        (finish-output))))
  (move-past-instruction $interpreter $instruction))

;;; -------------------------------------------------------

;; *  {digit}
;; *  {letter}
;; *- {digit}
;; *- {letter}
(define-instruction-processor Enqueue-Instruction
  (enqueue $memory
    (enqueue-instruction-value $instruction))
  (move-past-instruction $interpreter $instruction))

;;; -------------------------------------------------------

;; *  {non-alphanumeric}
;; *- {non-alphanumeric}
(define-instruction-processor Dequeue-Instruction
  (dequeue $memory)
  (when (dequeue-instruction-enqueue-zero-p $instruction)
    (enqueue $memory 0))
  (move-past-instruction $interpreter $instruction))

;;; -------------------------------------------------------

;; ?.
(define-instruction-processor Input-Instruction
  (format T "~&>> ")
  (finish-output)
  (enqueue $memory
    (char-code
      (read-char)))
  (clear-input)
  (move-past-instruction $interpreter $instruction))

;;; -------------------------------------------------------

;; ? {command-token}
(define-instruction-processor Skip-Instruction
  (if (zerop (peek-queue $memory))
    (move-past-current-location $interpreter)
    (move-past-instruction $interpreter $instruction)))

;;; -------------------------------------------------------

;; ? {non-period-and-non-command-token}
(define-instruction-processor Purge-Instruction
  (clear-queue $memory)
  (move-past-instruction $interpreter $instruction))

;;; -------------------------------------------------------

;; :
(define-instruction-processor Error-Instruction
  (error "Encountered a \":\" instruction at position ~a."
    (instruction-start-point $instruction)))

;;; -------------------------------------------------------

;; {non-command-alphanumeric-character}
(define-instruction-processor Literal-Instruction
  (format T "~c"
    (literal-instruction-token $instruction))
  (finish-output)
  (move-past-instruction $interpreter $instruction))

;;; -------------------------------------------------------

;; {non-command-and-non-alphanumeric-character}
(define-instruction-processor Random-Instruction
  (enqueue $memory 78)
  (select-random-act $acts)
  (change-act-navigator-act $navigator
    (current-act $acts))
  (memorize-current-act $interpreter)
  
  (when (can-move-down-in-act-p $navigator)
    (setf
      (board-at
        (act-navigator-board $navigator)
        (get-location-below
          (current-location $interpreter)))
      #\v)))

;;; -------------------------------------------------------

(defun validate-number-of-visited-acts (interpreter)
  "Determines whether the program consigned to the INTERPRETER's
   castaldy has hitherto visited two or more acts, returning on
   confirmation no value; otherwise signals an error of the type
   ``Too-Few-Entered-Acts-Error''."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (unless (has-visited-at-least-two-acts-p $travel-book)
      (error 'Too-Few-Entered-Acts-Error :number-of-acts
        (count-visited-acts $travel-book))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Interlude program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (loop
      for next-instruction
        of-type Instruction
        =       (extract-next-instruction interpreter)
      do
        (process-instruction interpreter next-instruction)
      until
        (or (not (has-next-act-cell-p $navigator))
            $execution-completed-p)
      finally
        (validate-number-of-visited-acts interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Interlude (code)
  "Interprets the piece of Interlude source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-instance 'Interpreter :board
      (make-board code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-Interlude
  "Hello*5*9*9*9*9*8*0++*4*9*9*9*5*0++world*4*9*9*9*6*0++*2$|v")

;;; -------------------------------------------------------

;; Print a random integer number from the closed interval [1, 6].
(interpret-Interlude "&|1|2|3|4|5|6")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Interlude
  (concatenate-lines
    "*4*-A*9*8?.+?v?_*2$|?_1*2$|v"
    "*:0*3$:::::::::::::|::::::|v"))

;;; -------------------------------------------------------

;; Perpetual cat program:
(interpret-Interlude
  (concatenate-lines
    "*0?.+*2$|*1$"))
