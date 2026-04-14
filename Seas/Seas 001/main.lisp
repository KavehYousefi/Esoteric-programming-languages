;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Seas", invented by the Esolang user "AmNow" and presented
;; on December 11th, 2021, the incarnated diorism of which wones in its
;; programs' representation as infinite two-dimensional grids, the
;; operative parcels of which limn a water area's simulacrum by
;; adminiculum of, at least partially, topical Unicode characters, with
;; an upper echolon's conflation with the surface level in a catena of
;; "🌊" symbols, imposing a uniform width to this particular cadastre.
;; 
;; 
;; Concept
;; =======
;; The Seas programming language subsumes into the two-dimensional
;; species of esoteric produces, its entheus the mimicry --- with a
;; very liberal conspectuity's approach --- of the sea, imposing as the
;; water surface's demarcation a horizontal catena enumerating zero or
;; more "🌊" characters, alow whom the tmema dispands into the actual
;; value of operative entelechy, the subjects of the manipulations a
;; scalar integer-valued accumulator and a stack whose elements concur
;; with the same realm of numeric currency.
;; 
;; == THE PROGRAM GRID: AN EXPLICIT AND AN IMPLICIT MOEITY ==
;; Anent the program's conformation, a bifurcation applies, ensuing from
;; whom a dimidation conduces the gendrure of an explicit and an
;; implicit area, the former the actual operative partition, the latter
;; an infinite dimense's simulacrum.
;; 
;; == THE EXPLICIT AREA: THE EFFECTIVE PROGRAM CADASTRE ==
;; The explicitly defined tmema amplects the effective grid cells, a
;; rectangle plasmature whose specification must ensure a lealty to a
;; common width and height.
;; 
;; The former dimension, the horizontal dispansion, derives from the
;; tally of waves, molded into the "🌊" character's guise, an accompt
;; to whom the lower bourne of one (1), but no maximum column dispansion
;; wists of an imposition, this serving as the topmost program row.
;; 
;; Any subsequent line, empight alow this surface level, must ostend the
;; exact length of the "🌊" symbols' forbisen; otherwise, an abortive
;; error's inroad terminates the execution.
;; 
;; == THE IMPLICIT AREA: RANDOMIZED ASCII CHARACTERS ==
;; Outwith the explicit program, an infinite dispansion of characters
;; entertains its woning, disencumbered from any efficacy, and chosen
;; in an aleatory fashion from the range of printable ASCII character,
;; the integral codes of which constitute incolants of the closed
;; interval [32, 126], assuming a probability of 1/5 (= 20%) for a
;; non-space character's dimensum's chevisance.
;; 
;; == OUR CYNOSURE: THE EXPLICIT SEA ==
;; The conflation of bourneless procession and carency in epiphonemal
;; vallidom shall found the vindication's furnishment for the
;; hithertoforth exclusive adhibition of our conspectuity upon the
;; explicit program segment.
;; 
;; == THE SEA: A TWO-DIMENSIONAL CARTESIAN GRID OF CHARACTERS ==
;; Given a program's reticulation encompassing a width of "w" columns
;; and a height of "h" cells, adhibiting to the former axis' divisions
;; an enumeration scheme employing integer subscripts commorant in the
;; closed interval [0, m], where
;; 
;;   m = w - 1
;; 
;; and the latter, vertical axis such occupying the range [0, n], where
;; 
;;   n = h - 1
;; 
;; the following designment's visualization may be limned:
;; 
;;   +---------------------------------------+
;;   | 🌊       | 🌊       |   ...   | 🌊       |
;;   | (0, -1) | (1, -1) |   ...   | (m, -1) |
;;   |         |         |         |         |
;;   |---------+---------+---------+---------|
;;   |🔳       🔳|         |         |         |
;;   | (0,  0) | (1,  0) |   ...   | (m,  0) |
;;   |🔳       🔳|         |         |         |
;;   |---------+---------+---------+---------|
;;   |         |         |         |         |
;;   | (0,  1) | (1,  1) |   ...   | (m,  1) |
;;   |         |         |         |         |
;;   |---------+---------+---------+---------|
;;   |         |         |         |         |
;;   |   ...   |   ...   |   ...   |   ...   |
;;   |         |         |         |         |
;;   |---------+---------+---------+---------|
;;   |         |         |         |         |
;;   | (0,  n) | (1,  n) |   ...   | (m,  n) |
;;   |         |         |         |         |
;;   +---------------------------------------+
;; 
;; Please heed the upper echolon's assignment comprised exclusively of
;; "🌊" symbols, and indexed with a row designation of -1; forecause
;; the instruction pointer's (IP) incipial location, expressly stated
;; in the language standard, occupies the coordinates (0, 0),
;; immediately alow the left-upper surface "🌊" instance, and visually
;; signified via the "🔳" symbol in each of the cell's quadruple
;; corners' occupancy.
;; 
;; == THE PROGRAM'S TOP LINE: A CATENA OF "🌊" SYMBOLS ==
;; The inchoacy of a Seas program's conformation is empight in the
;; topmost line, admitting as its exclusive members a catena comprising
;; zero or more "🌊" characters, with even spaces tholing an eloignment
;; from their validity in this horizontal dispansion.
;; 
;; == THE "🌊" LINE IMPOSES THE COLUMN COUNT FOR SUBSEQUENT LINES ==
;; A peisant perclose from this "surface level" line is betokened in
;; the subsequent rows, the same must ostend an exact cardinality in
;; characters equal to "🌊" accompt.
;; 
;; == THE INSTRUCTION POINTER COMMENCES BELOW THE LEFTMOST "🌊" ==
;; At a program's inchoate moment, the instruction pointer (IP) occupies
;; the grid cell immediately alow the leftmost "🌊" instance, the
;; ponibility of this origin point specified as (x=0, y=0); the cursor's
;; incipial airt being aligned with a sinistrodextral orientation.
;; 
;; Avaunting from this spatial prerequisite, the instruction pointer's
;; peragration athwart the sea grid furnishes the operative foundry for
;; the program execution, each encountered symbol, if affiliated with an
;; instruction, experiencing its evaluation.
;; 
;; == THE PROGRAM HALTS IF A "🌊" SYMBOL IN THE TOP LINE IS MET ==
;; The sea's status as an infinite dispansion along all axes, occupied
;; outwith the explicitly defined area with spaces or random printable
;; ASCII characters, conditions a dioristic program termination
;; criterion's establishment, thilk is realized in the pointer's
;; collision with the "🌊" character commorant in the top line.
;; 
;; == THE MEMORY: A TWISSEL OF INTEGER-VALUED ACCUMULATOR AND STACK ==
;; The Seas's programming language's architecture limns a dimidation's
;; governail, a conception inwith whose perimeter the champarty of an
;; accumulator, concredited with a scalar integer's castaldy, the datum
;; wisting of no mear in both the polarity and the magnitude, and a
;; stack endowed with an arbitrary accompt of the same object species.
;; 
;; 
;; Instructions
;; ============
;; Seas's instruction set tallies a membership of the cardinality 39,
;; the thus appropriated bailiwicks dispanding athwart navigational
;; operations, warklumes for the indagation and manipulation of the
;; memory's accumulator and stack, as well as basic arithmetics.
;; 
;; Any symbol to whom no vinculum to a causatum constitutes the
;; haecceity's pernancy abstains from a complication's inroad, as much
;; as thilk submits an apprizal aboon a mateotenchy's patefaction.
;; 
;; == OVERVIEW ==
;; The following apercu's onus shall be defined in a fundamental mete of
;; kith's communication concerning the Seas programming language's
;; operative facilities.
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   🌊       | Immediately terminates the program.
;;           |---------------------------------------------------------
;;           | This command may only occur at the program's topmost
;;           | row and must encompass the entire width, thus imposing
;;           | in a concomitant manner the code grid's admissible and
;;           | mandatory column count.
;;           |---------------------------------------------------------
;;           | If the topmost program line entails any other character
;;           | except for the "🌊" symbol, an error of the type
;;           | "InvalidSurfaceError" is signaled.
;;           |---------------------------------------------------------
;;           | If the "🌊" character partakes at any position other
;;           | than the topmost line, an error of the type
;;           | "InvalidCharacterError" is signaled.
;;   ..................................................................
;;   🟦       | Accompasses no causatum; that is, represents a
;;           | no-operation, or NOP.
;;   ==================================================================
;;   NAVIGATIONAL OPERATIONS
;;   ------------------------------------------------------------------
;;   🔼       | Redirects the instruction pointer (IP) upwards.
;;   ..................................................................
;;   🔽       | Redirects the instruction pointer (IP) downwards.
;;   ..................................................................
;;   ◀️      | Redirects the instruction pointer (IP) leftwards.
;;   ..................................................................
;;   ▶️      | Redirects the instruction pointer (IP) rightwards.
;;   ..................................................................
;;   🖋       | Reflects the instruction pointer (IP) on a mirror
;;           | ascending when perquired in a sinistro-dextral airt,
;;           | limned by an abbozzo's vehicle as "/", the subsequent
;;           | direction being subjected to the following nomothesia:
;;           | 
;;           |   ----------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+---------------
;;           |   left              | down
;;           |   ..................................
;;           |   right             | up
;;           |   ..................................
;;           |   up                | right
;;           |   ..................................
;;           |   down              | left
;;           |   ----------------------------------
;;   ..................................................................
;;   ✒️      | Reflects the instruction pointer (IP) on a mirror
;;           | descending when perquired in a sinistro-dextral airt,
;;           | limned by an abbozzo's vehicle as "\", the subsequent
;;           | direction being subjected to the following nomothesia:
;;           | 
;;           |   ----------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+---------------
;;           |   left              | up
;;           |   ..................................
;;           |   right             | down
;;           |   ..................................
;;           |   up                | left
;;           |   ..................................
;;           |   down              | right
;;           |   ----------------------------------
;;           |---------------------------------------------------------
;;           | Please heed that on some platforms, such as several
;;           | contemporary operating systems released by the company
;;           | Microsoft, the nib character ostends a sinistrodextral
;;           | ascent, rather than a descent, conflating its designment
;;           | in this aspect with the "🖋" icon.
;;   ..................................................................
;;   📍       | Pops the top stack element; if the same equals zero
;;           | (0), redirects the instruction pointer (IP) upwards;
;;           | otherwise downwards.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let discriminator <- stack.pop()
;;           |   
;;           |   if discriminator = 0 then
;;           |     ip.direction <- up
;;           |   else
;;           |     ip.direction <- down
;;           |   end if
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   📏       | Pops the top stack element; if the same equals zero
;;           | (0), redirects the instruction pointer (IP) leftwards;
;;           | otherwise rightwards.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let discriminator <- stack.pop()
;;           |   
;;           |   if discriminator = 0 then
;;           |     ip.direction <- left
;;           |   else
;;           |     ip.direction <- right
;;           |   end if
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ==================================================================
;;   NUMERICAL OPERATIONS
;;   ------------------------------------------------------------------
;;   0️⃣     | Pushes the digit zero (0) onto the stack.
;;   ..................................................................
;;   1️⃣     | Pushes the digit one (1) onto the stack.
;;   ..................................................................
;;   2️⃣     | Pushes the digit two (2) onto the stack.
;;   ..................................................................
;;   3️⃣     | Pushes the digit three (3) onto the stack.
;;   ..................................................................
;;   4️⃣     | Pushes the digit four (4) onto the stack.
;;   ..................................................................
;;   5️⃣     | Pushes the digit five (5) onto the stack.
;;   ..................................................................
;;   6️⃣     | Pushes the digit six (6) onto the stack.
;;   ..................................................................
;;   7️⃣     | Pushes the digit seven (7) onto the stack.
;;   ..................................................................
;;   8️⃣     | Pushes the digit eight (8) onto the stack.
;;   ..................................................................
;;   9️⃣     | Pushes the digit nine (9) onto the stack.
;;   ==================================================================
;;   ARITHMETICS
;;   ------------------------------------------------------------------
;;   ➕       | Pops the top stack element, here nevened "x", from the
;;           | stack, succeeded by a removal of the new top element,
;;           | "y", supputates the sum of x + y, and pushes this result
;;           | onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x   <- stack.pop()
;;           |   let y   <- stack.pop()
;;           |   let sum <- x + y
;;           |   
;;           |   stack.push(sum)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation, an error of the type
;;           | "EmptyStackError" is signaled at the instant of the
;;           | illicit removal trial.
;;   ..................................................................
;;   ➖       | Pops the top stack element, here nevened "x", from the
;;           | stack, succeeded by a removal of the new top element,
;;           | "y", supputates the difference of x - y, and pushes this
;;           | result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x          <- stack.pop()
;;           |   let y          <- stack.pop()
;;           |   let difference <- x - y
;;           |   
;;           |   stack.push(difference)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation, an error of the type
;;           | "EmptyStackError" is signaled at the instant of the
;;           | illicit removal trial.
;;   ..................................................................
;;   ✖️      | Pops the top stack element, here nevened "x", from the
;;           | stack, succeeded by a removal of the new top element,
;;           | "y", supputates the product of x * y, and pushes this
;;           | result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x       <- stack.pop()
;;           |   let y       <- stack.pop()
;;           |   let product <- x + y
;;           |   
;;           |   stack.push(product)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation, an error of the type
;;           | "EmptyStackError" is signaled at the instant of the
;;           | illicit removal trial.
;;   ..................................................................
;;   ➗       | Pops the top stack element, here nevened "x", from the
;;           | stack, succeeded by a removal of the new top element,
;;           | "y", supputates the quotient (x / y), truncates the same
;;           | in order to retain its integral moeity, and pushes this
;;           | result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x        <- stack.pop()
;;           |   let y        <- stack.pop()
;;           |   let quotient <- truncate(x / y)
;;           |   
;;           |   stack.push(quotient)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation, an error of the type
;;           | "EmptyStackError" is signaled at the instant of the
;;           | illicit removal trial.
;;   ==================================================================
;;   ACCUMULATOR MANIPULATION
;;   ------------------------------------------------------------------
;;   🅿️      | Pops the top stack element, here nevened "x", from the
;;           | stack, succeeded by a removal of the new top element,
;;           | "y", interprets "x" as a column index and "y" as a row
;;           | designator, requests the grid cell character amenable to
;;           | the zero-based x-th column of the zero-based y-th row,
;;           | copies its Unicode code point to the accumulator, while
;;           | writing the character whose code point equals the
;;           | accumulator's erstwhile value to thus located cell.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x                <- stack.pop()
;;           |   let y                <- stack.pop()
;;           |   let cellCharacter    <- grid[x, y]
;;           |   let cellValue        <- codePointOf(cellCharacter)
;;           |   let accumulatorValue <- accumulator.value
;;           |   let accumulatorChar  <- characterFor(accumulatorValue)
;;           |   
;;           |   accumulator.value <- cellValue
;;           |   grid[x, y]        <- accumulatorChar
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation, an error of the type
;;           | "EmptyStackError" is signaled at the instant of the
;;           | illicit removal trial.
;;   ..................................................................
;;   🧾       | Swaps the value stored in the top stack element with
;;           | the accumulator, and vice versa.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let erstwhileAccumulator <- accumulator.value
;;           |   
;;           |   accumulator.value <- stack.pop()
;;           |   stack.push(erstwhileAccumulator)
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   🌡       | Resets the accumulator value to its inchoate state of
;;           | zero (0).
;;   ==================================================================
;;   STACK MANIPULATION
;;   ------------------------------------------------------------------
;;   🔑       | Clears the stack, removing its entire content.
;;   ..................................................................
;;   🖍       | Reverses the stack.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | no causatum is adhibited.
;;   ..................................................................
;;   📗       | Relocates the stack's bottom element to its top
;;           | position.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   📙       | Relocates the stack's top element to its bottom
;;           | position.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   ✉️      | Pops the top stack element and discards thilk.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   ✂️      | Swaps the two top stack elements' positions.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements
;;           | during this operation, an error of the type
;;           | "EmptyStackError" is signaled at the instant of the
;;           | illicit removal trial.
;;   ..................................................................
;;   🧻       | Duplicates the top stack element.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   🥤       | Pops the top stack element, supputates the logical
;;           | NOT of the same, and pushes the result onto the stack.
;;           | As a consectary, if the top element equalled zero (0)
;;           | ere this operation's invocation, its new value assumes
;;           | one (1); otherwise, if the prevenient state amounted to
;;           | a non-zero value, the new state constitutes zero (0).
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x <- stack.pop()
;;           |   
;;           |   if x = 0 then
;;           |     stack.push(1)
;;           |   else
;;           |     stack.push(0)
;;           |   end if
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ==================================================================
;;   INPUT AND OUTPUT COMMUNICATION
;;   ------------------------------------------------------------------
;;   💡       | Queries the standard input conduit for a signed or
;;           | unsigned integer number and pushes thilk onto the
;;           | stack.
;;   ..................................................................
;;   📖       | Queries the standard input conduit for a Unicode
;;           | character and pushes its Unicode code point onto the
;;           | stack.
;;   ..................................................................
;;   🗳       | Pops the top stack value and prints thilk in its
;;           | verbatim numeric form to the standard output conduit,
;;           | succeeded by a single newline character.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   📅       | Pops the top stack value and prints the character
;;           | whose Unicode code point corresponds to the same to the
;;           | standard output conduit, neither preceded nor succeeded
;;           | by any adscititious content.
;;           |---------------------------------------------------------
;;           | If the stack is empty at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Appendices
;; ==========
;; A few topics' participation exist in the documentation's hyle that
;; cannot be supputated with an owelty to the paravaunt subjects'
;; gravity, nor does their pertinence anent the entirety's nortelry
;; disrank the same to an apprizal lower than a paregon. Such hyle shall
;; edify the following appendices' cynosure.
;; 
;; == APPENDIX A: UNICODE ==
;; The imperative begotten by the Seas programming language's dependency
;; upon Unicode characters limns a gendrure's concomitant in this
;; bailiwick's submergence.
;; 
;; Please heed that, as a corollary of the Unicode standard's intricate
;; haecceity, the tmemata extricated for an epexegetical adduction do
;; not rise aboon a cursory mete of gnarity's dation, the choice one
;; to rectify what knowledge is tharfed in this respect.
;; 
;; == COMBINING CHARACTERS: CHARACTER DESCRIPTORS AND DECORATORS ==
;; The requisite of certain character specification's coalition with
;; information advenient to the symbols' aefauld construct in a pursuit
;; of their verbatim replication segues into the Unicode concept of
;; combining characters, items in a catena which operate in conjunction
;; with the basic glyph targeting its modification.
;; 
;; Symbols may be succeeded by one or more such combining entities for
;; their design and effect's manipulation; as a forbisen, in
;; desiderating the expression of a decimal digit one (1) decked as a
;; keyboard button, that is, "1️⃣ ", the following triad of code points
;; may be employed in coefficiency:
;; 
;;   49, 65039, 8419
;; 
;; The componency's analysis into its participants instigates the
;; affiliated characters' epexegetical gendrure:
;; 
;;   ------------------------------------------------------------------
;;   Number | Code point | Character description
;;   -------+------------+---------------------------------------------
;;   1      | 49         | The decimal "Digit 1". Please note its
;;          |            | Unicode code point's confluence with the
;;          |            | ASCII code (49) for the same entity.
;;   ..................................................................
;;   2      | 65039      | The "Variation Selector 16", a hint to the
;;          |            | rendering of the combination as an emoji.
;;   ..................................................................
;;   3      | 8419       | The "Combining Enclosing Keycap", which
;;          |            | designates the wish to display the character
;;          |            | in a form which resembles a physical
;;          |            | keyboard key or a virtual button, usually
;;          |            | via a rectangular or square outline.
;;   ------------------------------------------------------------------
;; 
;; Having admnistered this cursory grade of gnarity, a listing of the
;; significant combinators shall be supplemented in a compendious
;; exposition:
;; 
;;   ------------------------------------------------------------------
;;   Name                       | Code point | Purpose
;;   ---------------------------+------------+-------------------------
;;   Combining Enclosing Keycap | 8419       | Specifies the prevenient
;;                              |            | character's rendering in
;;                              |            | the circumference of a
;;                              |            | physical keyboard key or
;;                              |            | virtual button.
;;   ..................................................................
;;   Variation Selector 16      | 65039      | Specifies the prevenient
;;                              |            | character's rendering
;;                              |            | as an emoji.
;;   ------------------------------------------------------------------
;; 
;; == LISTING OF DEPLOYED SYMBOLS ==
;; No encumberance with the wite of a lapsus in Unicode symbols'
;; replication, which partake of the Seas programming language, shall
;; befall this documentation; hence, the following tabulation ostends
;; the characters, their official names according to the standard, and
;; their code points in the decimal system's plasmature.
;; 
;;   ------------------------------------------------------------------
;;   Symbol | Unicode name                     | Codepoints (decimal)
;;   -------+----------------------------------+-----------------------
;;   🌊      | Water Wave                       | 127754
;;   ..................................................................
;;   🟦      | Large Blue Square                | 128998
;;   ..................................................................
;;   0️⃣    | Keycap Digit Zero                | 48, 65039, 8419
;;   ..................................................................
;;   1️⃣    | Keycap Digit One                 | 49, 65039, 8419
;;   ..................................................................
;;   2️⃣    | Keycap Digit Two                 | 50, 65039, 8419
;;   ..................................................................
;;   3️⃣    | Keycap Digit Three               | 51, 65039, 8419
;;   ..................................................................
;;   4️⃣    | Keycap Digit Four                | 52, 65039, 8419
;;   ..................................................................
;;   5️⃣    | Keycap Digit Five                | 53, 65039, 8419
;;   ..................................................................
;;   6️⃣    | Keycap Digit Six                 | 54, 65039, 8419
;;   ..................................................................
;;   7️⃣    | Keycap Digit Seven               | 55, 65039, 8419
;;   ..................................................................
;;   8️⃣    | Keycap Digit Eight               | 56, 65039, 8419
;;   ..................................................................
;;   9️⃣    | Keycap Digit Nine                | 57, 65039, 8419
;;   ..................................................................
;;   🅿️     | Negative Squared Latin Capital   | 127359, 65039
;;          | Letter P                         |
;;   ..................................................................
;;   🧾      | Receipt                          | 129534
;;   ..................................................................
;;   🌡      | Thermometer                      | 127777
;;   ..................................................................
;;   🔑      | Key                              | 128273
;;   ..................................................................
;;   🖍      | Lower Left Crayon                | 128397
;;   ..................................................................
;;   📗      | Green Book                       | 128215
;;   ..................................................................
;;   📙      | Orange Book                      | 128217
;;   ..................................................................
;;   🖋      | Lower Left Fountain Pen          | 128395
;;   ..................................................................
;;   ✒️     | Black Nib                        | 10002, 65039
;;   ..................................................................
;;   📍      | Round Pushpin                    | 128205
;;   ..................................................................
;;   📏      | Straight Ruler                   | 128207
;;   ..................................................................
;;   ➕      | Heavy Plus Sign                  | 10133
;;   ..................................................................
;;   ➖      | Heavy Minus Sign                 | 10134
;;   ..................................................................
;;   ✖️     | Heavy Multiplication X           | 10006, 65039
;;   ..................................................................
;;   ➗      | Heavy Division Sign              | 10135
;;   ..................................................................
;;   🔼      | Up-Pointing Small Red Triangle   | 128316
;;   ..................................................................
;;   🔽      | Down-Pointing Small Red Triangle | 128317
;;   ..................................................................
;;   ◀️     | Black Left-Pointing Triangle     | 9664, 65039
;;   ..................................................................
;;   ▶️     | Black Right-Pointing Triangle    | 9654, 65039
;;   ..................................................................
;;   💡      | Electric Light Bulb              | 128161
;;   ..................................................................
;;   📖      | Open Book                        | 128214
;;   ..................................................................
;;   🗳      | Ballot Box with Ballot           | 128499
;;   ..................................................................
;;   📅      | Calendar                         | 128197
;;   ..................................................................
;;   ✉️     | Envelope                         | 9993, 65039
;;   ..................................................................
;;   ✂️     | Black Scissors                   | 9986, 65039
;;   ..................................................................
;;   🧻      | Roll of Paper                    | 129531
;;   ..................................................................
;;   🥤      | Cup with Straw                   | 129380
;;   ..................................................................
;;   🔳      | White Square Button              | 128307
;;   ------------------------------------------------------------------
;; 
;; == THE SYMBOLS AND THEIR PURPOSES ==
;; A more chrestomathic treatise's vouchsafement shall be entertained in
;; the following tabulation, the telos incorporated in its apportionment
;; such as to produce an equiparation atwixen the pertinent language
;; symbols and their alligated operative epiphenomena:
;; 
;;   ------------------------------------------------------------------
;;   Symbol | Signification in code
;;   -------+----------------------------------------------------------
;;   🌊      | End of program (surface level)
;;   ..................................................................
;;   🟦      | No-operation (NOP)
;;   ..................................................................
;;   0️⃣    | Push number 0
;;   ..................................................................
;;   1️⃣    | Push number 1
;;   ..................................................................
;;   2️⃣    | Push number 2
;;   ..................................................................
;;   3️⃣    | Push number 3
;;   ..................................................................
;;   4️⃣    | Push number 4
;;   ..................................................................
;;   5️⃣    | Push number 5
;;   ..................................................................
;;   6️⃣    | Push number 6
;;   ..................................................................
;;   7️⃣    | Push number 7
;;   ..................................................................
;;   8️⃣    | Push number 8
;;   ..................................................................
;;   9️⃣    | Push number 9
;;   ..................................................................
;;   🅿️     | Exchange program character with accumulator value
;;   ..................................................................
;;   🧾      | Exchange stack top and accumulator value
;;   ..................................................................
;;   🌡      | Reset accumulator
;;   ..................................................................
;;   🔑      | Clear stack
;;   ..................................................................
;;   🖍      | Reverse stack
;;   ..................................................................
;;   📗      | Move stack bottom to top
;;   ..................................................................
;;   📙      | Move stack top to bottom
;;   ..................................................................
;;   🖋      | Ascending direction mirror
;;   ..................................................................
;;   ✒️     | Descending direction mirror
;;   ..................................................................
;;   📍      | Conditional up/down redirection
;;   ..................................................................
;;   📏      | Conditional left/right redirection
;;   ..................................................................
;;   ➕      | Addition
;;   ..................................................................
;;   ➖      | Subtraction
;;   ..................................................................
;;   ✖️     | Multiplication
;;   ..................................................................
;;   ➗      | Division
;;   ..................................................................
;;   🔼      | Upward redirection
;;   ..................................................................
;;   🔽      | Downward redirection
;;   ..................................................................
;;   ◀️     | Sinistral redirection
;;   ..................................................................
;;   ▶️     | Dextral redirection
;;   ..................................................................
;;   💡      | Input number
;;   ..................................................................
;;   📖      | Input character
;;   ..................................................................
;;   🗳      | Print number
;;   ..................................................................
;;   📅      | Print character
;;   ..................................................................
;;   ✉️     | Pop from stack
;;   ..................................................................
;;   ✂️     | Swap stack top
;;   ..................................................................
;;   🧻      | Duplicate stack top
;;   ..................................................................
;;   🥤      | Logical NOT of stack top
;;   ..................................................................
;;   🔳      | Used in documentation to emphasize program start point
;;   ------------------------------------------------------------------
;; 
;; == APPENDIX B: PROJECT FILES ==
;; The convolute nature encumbering the project's requisita establishes
;; the necessity for its source code's distribution across a rather
;; wide array of files.
;; 
;; The following table applies itself to these components' enumeration,
;; complementing the agnomination and the agency by the concrete order
;; of the file's loading.
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
;;   No. | File            | Purpose
;;   ----+-----------------+-------------------------------------------
;;    1  | types.lisp      | Defines the custom types deployed in
;;       |                 | the program and utilized by the
;;       |                 | subsequent project files, including,
;;       |                 | among others, ``list-of'', ``octet'',
;;       |                 | etc.
;;   ..................................................................
;;    2  | boolean.lisp    | Furnishes the operation for converting an
;;       |                 | object as a "generalized boolean" into a
;;       |                 | veridicous "boolean" truth value.
;;   ..................................................................
;;    3  | strings.lisp    | Establishes the general string operations.
;;   ..................................................................
;;    4  | icons.lisp      | Responsible for the warklumes' furnishment
;;       |                 | thilk capacitate the handling of Unicode
;;       |                 | characters and, in particular, the
;;       |                 | recognition of emojis as compounds of
;;       |                 | such, forming the ``Icon'' objects.
;;   ..................................................................
;;    5  | location.lisp   | The salvatory for routines appertaining to
;;       |                 | the conspection and manipulation of
;;       |                 | two-dimensional positions and the
;;       |                 | directional representations.
;;   ..................................................................
;;    6  | submarine.lisp  | Implements the instruction pointer (IP),
;;       |                 | here yclept the "submarine".
;;   ..................................................................
;;    7  | sea.lisp        | Serves in the handling of the
;;       |                 | two-dimensional program grid, comprised of
;;       |                 | "Icon" cells, and norned here the "sea".
;;   ..................................................................
;;    8  | stack.lisp      | Accoutres one moiety of the program memory
;;       |                 | in the integer-valued stack.
;;   ..................................................................
;;    9  | travel-log.lisp | Implements the Seas program interpreter,
;;       |                 | its agnomination chosen as the "travel
;;       |                 | log".
;;   ..................................................................
;;   10  | conditions.lisp | Defines the bespoke condition types.
;;   ..................................................................
;;   11  | tests.lisp      | Accoutres the interpreter test cases.
;;   ..................................................................
;;   12  | main.lisp       | Establishes an entry point into the
;;       |                 | project by loading the requisite
;;       |                 | Common Lisp source files elucidated
;;       |                 | above in their correct order.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort peracted in
;; the programming language Common Lisp, the chevisance itself two
;; tiers' coefficiency; imprimis, involving the Seas source string's
;; plasmation, thilk wists of a single axis' dimense only, into a
;; veridicous two-dimensional guise, inwith whose bournes are coalesced
;; multipartite characters into "icons"; concluding with the desinent
;; stage which operates on this accommodated reformulation.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen:2013:lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-11-22
;; 
;; Sources:
;;   [christensen:2013:lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang:2025:Seas]
;;   The Esolang contributors, "Seas", September 7th, 2025
;;   URL: "https://esolangs.org/wiki/Seas"
;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype file-source ()
  "The ``file-source'' type defines a valid source for an external
   file's obtention, which comprehends a ``pathname'' object, a stream
   connected to a file, or a string representation of a file path."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global variables and constants.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +PROJECT-PATH+))

;;; -------------------------------------------------------

(defparameter +PROJECT-PATH+
  (make-pathname)
  "Specifies the directory on the executing machine the same contains
   this Seas interpreter project's Common Lisp source files, and whence
   the separate file designations are derived.
   ---
   Please substitute the +PROJECT-PATH+ by your personal directory thilk
   accommodates a commorancy to the requisite Common Lisp source files
   for the interpreter.
   ---
   Several facilities are offered by the Common Lisp standard library
   for engaging in such an activity, enumerating, for instance:
   
     ------------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+------------------------------------------
     make-path-name   | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                 \"Users\"
                      |                 \"Kaveh\"
                      |                 \"Seas\"
                      |                 \"Seas_001\"))
     ............................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/Seas/Seas_001/\")
     ------------------------------------------------------------")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the project file handling operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-the-project-file (source-file)
  "Loads and evaluates the Common Lisp SOURCE-FILE, its commorancy
   expected to be, immediately or through indirections, inwith the path
   specified by the ``+PROJECT-PATH+'', and returns no value."
  (declare (type file-source source-file))
  (load
    (merge-pathnames +PROJECT-PATH+ source-file)
    :external-format :utf-8)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of the project files.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-the-project-file "types.lisp")
(import-the-project-file "boolean.lisp")
(import-the-project-file "strings.lisp")
(import-the-project-file "icons.lisp")
(import-the-project-file "location.lisp")
(import-the-project-file "submarine.lisp")
(import-the-project-file "sea.lisp")
(import-the-project-file "stack.lisp")
(import-the-project-file "travel-log.lisp")
(import-the-project-file "conditions.lisp")
(import-the-project-file "tests.lisp")
