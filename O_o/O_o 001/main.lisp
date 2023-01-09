;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "O_o", presented by the Esolang user "BoundedBeans" in the
;; year 2022, derived from Urban Mueller's "brainfuck", kenspeckle in
;; representing the source code as lines of the raised eyebrows emoji
;; "O_o".
;; 
;; 
;; Concept
;; =======
;; The O_o programming language tallies among the brainfuck derivatives,
;; extending the same by a stack per cell and the relating operations,
;; and encoding each triple of brainfuck command twain in conjunction
;; with a stack manipulation into a separate line either comprehending
;; an "O_o" or "0_o" pattern.
;; 
;; 
;; Decoding
;; ========
;; An O_o program's interpretation enumerates as its paravaunt
;; requisitum the decoding of an "O_o" or "0_o" line pattern into the
;; three constituents, the premier two brainfuck commands, the desinent
;; one a stack accessor.
;; 
;; == EACH PROGRAM LINE CONTAINS TWO OR THREE INSTRUCTIONS ==
;; An O_o program consists of zero or more lines, each of which encodes
;; two or three instructions, with the former case encompassing one
;; brainfuck command, followed by one stack operation, whereas the
;; latter collocates two brainfuck commands in succession to the single
;; stack companion. The one-brainfuck command can be considered as
;; special case of the triad variant, with the first operation being
;; designated as a no-operation (NOP) item.
;; 
;; == THE EMOJIS O_o AND 0_o COMPOUND INTO A PROGRAM LINE ==
;; A line in the language is expressed as a simulacrum of the raised
;; eyebrow emoji, compact of two lateralities, aligning on the left
;; compartment the big eyes, juxtaposed with the small visual warklumes
;; on the dextral moeity, both separated by a mouth. The syntaxis
;; assigns the sinistral parcel's design either to a series of
;; upper-case "O"s or a single zero "0", followed by the underscore "_"
;; mouth, and perfected using at least one minuscular "o" for the
;; right-hand side eyes.
;; 
;; == THE 8-BIT LINE CODE CONTAINS INSTRUCTION ENCODINGS ==
;; In a general diction, the two sides contribute in champarty the bits
;; to the 8-bit line code. In the case of the "O_o" pattern, both eye
;; variants encode four bits each, contained in the one to sixteen
;; symbols. The special "0_o" case attributes to the left eyes three
;; zero-valued bits (000), whereas the one to thirty-two right-hand "o"s
;; provides the five remaining positions.
;; 
;; An "O_o" style line serves to ensconce three instructions, specified
;; by their unique binary codes. The three most significant bits
;; appertain to the first brainfuck command's encoding, succeeded by
;; another treble that provides the second brainfuck command key, and
;; terminated with the last two bits specifying the single stack
;; operation.
;; 
;; Concretely, the following association betwixt a 3-bit binary code and
;; its affiliated brainfuck command holds:
;; 
;;   ------------------------------------------------------------------
;;   3-bit binary code | brainfuck command
;;   ------------------+-----------------------------------------------
;;   000               | >
;;   ..................................................................
;;   001               | <
;;   ..................................................................
;;   010               | +
;;   ..................................................................
;;   011               | -
;;   ..................................................................
;;   100               | .
;;   ..................................................................
;;   101               | ,
;;   ..................................................................
;;   110               | [
;;   ..................................................................
;;   111               | ]
;;   ------------------------------------------------------------------
;; 
;; For the 2-bit binary code and stack operation juxtaposition, please
;; query this table:
;; 
;;   ------------------------------------------------------------------
;;   2-bit binary code | Stack operation
;;   ------------------+-----------------------------------------------
;;   00                | No-operation
;;   ..................................................................
;;   01                | Push value to stack
;;   ..................................................................
;;   10                | Pop stack to value
;;   ..................................................................
;;   11                | Pop stack to neighbor stack
;;   ------------------------------------------------------------------
;; 
;; The "0_o" style variant is steadable for the conveyance of a single
;; brainfuck instruction and a singular stack operation. The three most
;; significant bits are simply neglected; the next three positions
;; supply the only brainfuck command code, with the bit twain at the
;; desinence again accommodating the stack part.
;; 
;; As an apercu, the following holds for the line code and its embedded
;; instruction encodings:
;; 
;;   ------------------------------------------------------------------
;;   Pattern | 8-bit line code   | Keys
;;   --------+-------------------+-------------------------------------
;;           | 7 6 5  4 3 2  1 0 | c1: 3-bit first brainfuck command.
;;   O_o     | -----  -----  --- | c2: 3-bit second brainfuck command.
;;           |   c1     c2    c3 | c3: 2-bit stack operation.
;;   ..................................................................
;;           | 7 6 5  4 3 2  1 0 | c1: 3-bit ignored section.
;;   0_o     | -----  -----  --- | c2: 3-bit brainfuck command.
;;           |   c1     c2    c3 | c3: 2-bit stack operation.
;;   ------------------------------------------------------------------
;; 
;; == TWO LINE FORMATS EXIST ==
;; An instruction line in O_o may be subsumed into one of two possible
;; formats, the "bilateral" and the "unilateral" species, the former of
;; which expects two brainfuck commands to act as a concluding stack
;; operation's compernage, whereas the unilateral type relinquishes the
;; first of the brainfuck twain, thus homologating an odd account of
;; such instructions.
;; 
;; == THE BILATERAL FORMAT: TWO BRAINFUCK + ONE STACK OPERATION ==
;; The standard or "bilateral" format comprehends an equipoised design,
;; with the sinistral compartment composed of between one (1) and
;; sixteen (16) majuscular "O"s, simulating the "big eyes".
;; 
;; The mouth part, established using a single underscore "_", segregates
;; the left from the dextral moeity.
;; 
;; This second part embraces a sequence of also one (1) up to sixteen
;; (16) "o" minuscles, a simulacrum of the "small eyes".
;; 
;;   OOO...O_ooo...o  | (L) --- One to sixteen majuscular "O"s.
;;   ^-----^ ^-----^  | (M) --- Separating underscore "_".
;;     (L) (M) (R)    | (R) --- One to sixteen minuscular "o"s.
;; 
;; Given that each non-empty program line must be transcripted into a
;; sequence of exactly eight bits, equivalent to one byte, the big "O"
;; segment encodes the most significant four bits --- the high nybble.
;; Its decoding applies by counting these big eyes, subtracting thereof
;; the value one (1) --- as a single "O" instance always occurs as a
;; guarantee to obviate the lack of a "left eye" in the case of a
;; zero-valued high nybble --- and construing it in its four-bit binary
;; representation. This deduction translates the big-eyes tally range
;; [1, 16] into the nybble gamut [0, 15].
;; 
;; As an adminiculum for the nybble formation principle, the following
;; juxtaposition betwixt the tally of big eyes and their binary
;; expression shall be produced:
;; 
;;   ------------------------------------------------------------------
;;   Left side        | # Os | Decimal value | Binary form (nybble)
;;   -----------------+------+---------------+-------------------------
;;   O                |   1  |       0       | 0000
;;   ..................................................................
;;   OO               |   2  |       1       | 0001
;;   ..................................................................
;;   OOO              |   3  |       2       | 0010
;;   ..................................................................
;;   OOOO             |   4  |       3       | 0011
;;   ..................................................................
;;   OOOOO            |   5  |       4       | 0100
;;   ..................................................................
;;   OOOOOO           |   6  |       5       | 0101
;;   ..................................................................
;;   OOOOOOO          |   7  |       6       | 0110
;;   ..................................................................
;;   OOOOOOOO         |   8  |       7       | 0111
;;   ..................................................................
;;   OOOOOOOOO        |   9  |       8       | 1000
;;   ..................................................................
;;   OOOOOOOOOO       |  10  |       9       | 1001
;;   ..................................................................
;;   OOOOOOOOOOO      |  11  |      10       | 1010
;;   ..................................................................
;;   OOOOOOOOOOOO     |  12  |      11       | 1011
;;   ..................................................................
;;   OOOOOOOOOOOOO    |  13  |      12       | 1100
;;   ..................................................................
;;   OOOOOOOOOOOOOO   |  14  |      13       | 1101
;;   ..................................................................
;;   OOOOOOOOOOOOOOO  |  15  |      14       | 1110
;;   ..................................................................
;;   OOOOOOOOOOOOOOOO |  16  |      15       | 1111
;;   ------------------------------------------------------------------
;; 
;; In a similiter fashion, the right-hand side, compact of its one to
;; sixteen "small eyes", each a minuscular "o", ought to be transformed
;; into a nybble, iterum by succeeding the occurrences' tally with a
;; deduction of the single advenient "o", and forming the [0, 15]-ranged
;; decimal's four-bit binary equivalent. The thus yielded bit quartet
;; participates in the line's byte as the low nybble.
;; 
;; Again, the vinculum betwixt small eyes and nybble representation,
;; with the intermittent gnarity, shall be adduced:
;; 
;;   ------------------------------------------------------------------
;;   Right side       | # os | Decimal value | Binary form (nybble)
;;   -----------------+------+---------------+-------------------------
;;   o                |   1  |       0       | 0000
;;   ..................................................................
;;   oo               |   2  |       1       | 0001
;;   ..................................................................
;;   ooo              |   3  |       2       | 0010
;;   ..................................................................
;;   oooo             |   4  |       3       | 0011
;;   ..................................................................
;;   ooooo            |   5  |       4       | 0100
;;   ..................................................................
;;   oooooo           |   6  |       5       | 0101
;;   ..................................................................
;;   ooooooo          |   7  |       6       | 0110
;;   ..................................................................
;;   oooooooo         |   8  |       7       | 0111
;;   ..................................................................
;;   ooooooooo        |   9  |       8       | 1000
;;   ..................................................................
;;   oooooooooo       |  10  |       9       | 1001
;;   ..................................................................
;;   ooooooooooo      |  11  |      10       | 1010
;;   ..................................................................
;;   oooooooooooo     |  12  |      11       | 1011
;;   ..................................................................
;;   ooooooooooooo    |  13  |      12       | 1100
;;   ..................................................................
;;   oooooooooooooo   |  14  |      13       | 1101
;;   ..................................................................
;;   ooooooooooooooo  |  15  |      14       | 1110
;;   ..................................................................
;;   oooooooooooooooo |  16  |      15       | 1111
;;   ------------------------------------------------------------------
;; 
;; A recapitulation will now apply itself to an augmented elucidation of
;; the convolute process:
;; 
;;                             OOO...O_ooo...o
;;                             ------- -------
;;                                |       |
;;                                V       V  
;;                -----------------       -----------------
;;                Count the 1 to 16       Count the 1 to 16
;;                big eyes.               small eyes.
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------       -----------------
;;                Reduce them by 1        Reduce them by 1
;;                into the range          into the range
;;                [0, 15].                [0, 15].
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------       -----------------
;;                Convert them into       Convert them into
;;                a 4-bit binary.         a 4-bit binary.
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------       -----------------
;;                This produces the       This produces the
;;                8-bit line code's       8-bit line code's
;;                high nybble.            low nybble.
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------------------------------
;;                Concatenate the high nybble and the low
;;                nybble into the 8-bit line code.
;;                -----------------------------------------
;; 
;; The two nybbles' obtention segues into the line code analyzation's
;; bailiwick, relating to the objective to extract from the octet the
;; two brainfuck commands and the singular stack operation.
;; 
;; Please refer to the following diagram, which lists the eight line
;; code bits in decrementing order of position significant, from 7 down
;; to 0, for the interpretation of the embraced operations:
;; 
;;    MSB            LSB
;;   ...................
;;   7 6 5 | 4 3 2 | 1 0
;;   -----   -----   ---
;;     |       |      |
;;     |       |     2-bit stack operation code.
;;     |       |
;;     |      3-bit second brainfuck command code.
;;     |
;;    3-bit first brainfuck command code.
;; 
;; == THE UNILATERAL FORMAT: ONE BRAINFUCK + ONE STACK OPERATION ==
;; A relief concerning the two-brainfuck-commands-per-line requirement,
;; the unilateral format exists that barters the sinistral moeity,
;; comprehending the one to sixteen big eyes ("O"s) for a single zero
;; digit "0", thus eschewing the first brainfuck member.
;; 
;; A ramification issuing from the modified pattern, the overthwart
;; laterality now comprehends one (1) to thirty-two (32) "o" minuscles,
;; providing five in lieu of the previous four least significant bits
;; comprising the octet line code. The three most significant positions,
;; delineating the redesigned left part's contribution, steadily assume
;; the three bits "000".
;; 
;; The deviating unilateral design shall be explicated in a more
;; illustrative manner:
;; 
;;                                   0_ooo...o
;;                             ------- -------
;;                                |       |
;;                                V       V  
;;                -----------------       -----------------
;;                Always a single 0       Count the 1 to 32
;;                eye exists.             small eyes.
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------       -----------------
;;                                        Reduce them by 1
;;                                        into the range
;;                                        [0, 31].
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------       -----------------
;;                Use the 3-bit           Convert them into
;;                binary 000.             a 5-bit binary.
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------       -----------------
;;                This produces the       This produces the
;;                8-bit line code's       8-bit line code's
;;                three highest           five lowest bits.
;;                bits.
;;                -----------------       -----------------
;;                                |       |
;;                                V       V
;;                -----------------------------------------
;;                Concatenate the three 0-bits and the five
;;                low bits into the 8-bit line code.
;;                -----------------------------------------
;; 
;; The procession from a distinct airt does not affect the instruction
;; extraction foundation: The assembled 8-bit line encoding must be
;; subjected to an indagation in order to produce the contained
;; ingredients. The unilateral format, waiving the one brainfuck
;; command's participation, does not rely on the first three most
;; significant bits, either ignoring them or, a tantamount in effect,
;; applying the construe as a no-operation (NOP). The succeeding steps
;; equal exactly the bilateral case, with the next three bits specifying
;; a brainfuck command, and the two-bit remnant relating to a stack
;; operation's code.
;; 
;; Please refer to the following diagram, which lists the eight line
;; code bits in decrementing order of position significant, from 7 down
;; to 0, for the interpretation of the embraced operations:
;; 
;;    MSB            LSB
;;   ...................
;;   7 6 5 | 4 3 2 | 1 0
;;   -----   -----   ---
;;     |       |      |
;;     |       |     2-bit stack operation code.
;;     |       |
;;     |      3-bit brainfuck command code.
;;     |
;;    3-bit ignored no-operation code.
;; 
;; 
;; Encoding
;; ========
;; The encoding manifests in the coalescing of three instructions, each
;; a compound of two brainfuck and one stack operation, which are
;; converted into an 8-bit line code, whence either the number of "O"s
;; and "o"s is calculated in order to produce an "O_o" pattern, or the
;; "o" component is preceded by a "0" to generate the "0_o" design.
;; 
;; == BASIC CONCEPTS ==
;; A symmetrical exercise, the encoding process' inchoation arrives with
;; the specification of zero or more instruction triples, each such
;; compact of
;; 
;;   (1) a first brainfuck command, or the sentinel "no-operation" (NOP)
;;       to communicate its absence
;;   (2) a second brainfuck command that may not be a "no-operation"
;;   (3) a stack operation.
;; 
;; Depending upon the first member's (1) nature, a line code composed of
;; eight bits issues. For a non-NOP first command, the three most
;; significant bits assume the brainfuck operation code, succeeded by
;; the code of its peer. The table alow shall adhibit the requisite
;; nortelry anest the brainfuck tokens and their 3-bit binary encodings:
;; 
;;   ------------------------------------------------------------------
;;   brainfuck command | 3-bit binary code
;;   ------------------+-----------------------------------------------
;;   >                 | 000
;;   ..................................................................
;;   <                 | 001
;;   ..................................................................
;;   +                 | 010
;;   ..................................................................
;;   -                 | 011
;;   ..................................................................
;;   .                 | 100
;;   ..................................................................
;;   ,                 | 101
;;   ..................................................................
;;   [                 | 110
;;   ..................................................................
;;   ]                 | 111
;;   ------------------------------------------------------------------
;; 
;; The two least significant bits are specified by the stack operation's
;; encoding, purveyed in the next tabular illustration:
;; 
;;   ------------------------------------------------------------------
;;   Stack operation             | 2-bit binary code
;;   ----------------------------+-------------------------------------
;;   No-operation                | 00
;;   ..................................................................
;;   Push value to stack         | 01
;;   ..................................................................
;;   Pop stack to value          | 10
;;   ..................................................................
;;   Pop stack to neighbor stack | 11
;;   ------------------------------------------------------------------
;; 
;; The eight-bit line code is then bisected into its two 4-bit halves,
;; this entailing the most significant moeity known as the high nybble,
;; accompanied by the low nybble.
;; 
;; Given the high nybble's decimal value n and the low nybble's m, a
;; sequence of n+1 "O" majuscles is written to the O_o code, followed by
;; an underscore "_", and concluded with a series of m+1 "o" minuscles.
;; 
;; Every instruction triplet's "O_o" encoding is allotted a line of its
;; own.
;; 
;; If, on the other hand, the first brainfuck command assumes the
;; omission sentinel NOP (no-operation), the 8-bit line code's three
;; most significiant positions are ignored. The second brainfuck command
;; and the stack operation retain their roles verbatim, modifying the
;; five least significiant bits as produced by the aboon elucidations.
;; 
;; This 5-bit sequence's decimal value p is employed for the O_o code
;; generation, proceeding by writing a single zero digit "0", appending
;; the underscore "_", and complementing the output using a sequence of
;; p+1 minuscular "o" letters.
;; 
;; Again, a linebreak follows for the next treble instruction group.
;; 
;; == PSEUDOCODE ==
;; The encoding process shall be formulated in the following piece of
;; pseudocode.
;; 
;; However, as a parasceve, please note these facts:
;; 
;;   (a) The binary numbers are prefixed with a minuscular "b",
;;       followed by one or more bits.
;;   
;;   (b) All binary values are expressed in their sinistrodextral
;;       arrangement with the left position constituting the most
;;       significant bit (MSB), proceeding rightwards to the least
;;       significant bit (LSB).
;;         Producing a forbisen regarding this convention, the binary
;;       number
;;         b110
;;       encodes via the constituents
;;         MSB | 1 = 1 * 2^2 = 4
;;             | 1 = 1 * 2^1 = 2
;;         LSB | 0 = 0 * 2^0 = 0
;;       the decimal integer
;;         6 (= 4 + 2 + 0).
;;   
;;   (c) The bit positions are enumerated with a one-based subscripting
;;       system, establishing the number one (1) relating to the least
;;       significant element (LSB). The highest bit of an octet thus
;;       would be designated using the subscript eight (8).
;;         As a consequence of this specification, in conjuction with
;;       the point (b), subscripts proceed by mentioning the highest
;;       index first, preceding the also inclusive lower boundary:
;;         bitSequence[highBitIndex, lowBitIndex]
;;       As an example, the bit sequence
;;         myBits <- b11011000
;;       when consigned to the subsequence designator
;;         myBits[7,4]
;;       would return
;;         b1011
;;       forecause:
;;         bits:        1 1 0 1 1 0 0 0
;;         subscripts:  8 7 6 5 4 3 2 1
;;         subsequence:   *******
;; 
;; These apostils furnished, the pseudocode comprehends:
;; 
;;   function getBrainfuckCommandCode (brainfuckCommand)
;;     if brainfuckCommand = ">" then
;;       return b000
;;     else if brainfuckCommand = "<" then
;;       return b001
;;     else if brainfuckCommand = "+" then
;;       return b010
;;     else if brainfuckCommand = "-" then
;;       return b011
;;     else if brainfuckCommand = "." then
;;       return b100
;;     else if brainfuckCommand = "," then
;;       return b101
;;     else if brainfuckCommand = "[" then
;;       return b110
;;     else if brainfuckCommand = "]" then
;;       return b111
;;     else
;;       signal error: "Invalid brainfuck command: {brainfuckCommand}."
;;     end if
;;   end function
;;   
;;   function getStackOperationCode (stackOperation)
;;     if stackOperation = "no-operation" then
;;       return b00
;;     else if stackOperation = "push-value-to-stack" then
;;       return b01
;;     else if stackOperation = "pop-stack-to-value" then
;;       return b10
;;     else if stackOperation = "pop-stack-to-next-stack" then
;;       return b11
;;     else
;;       signal error: "Invalid stack operation: {stackOperation}."
;;     end if
;;   end function
;;   
;;   procedure encodeLine (firstBrainfuckCommand,
;;                         secondBrainfuckCommand,
;;                         stackOperation)
;;     let lineCode <- b00000000
;;     
;;     if firstBrainfuckCommand is no-operation
;;       lineCode[5,3] <- getBrainfuckCommandCode(secondBrainfuckCommand)
;;       lineCode[2,1] <- getStackOperationCode(stackOperation)
;;       
;;       let numberOfSmallEyes <- decimalValueOf(lineCode[5,1]) + 1
;;       
;;       write "0"
;;       write "_"
;;       write numberOfSmallEyesTimes the letter "o"
;;     else
;;       lineCode[8,6] <- getBrainfuckCommandCode(firstBrainfuckCommand)
;;       lineCode[5,3] <- getBrainfuckCommandCode(secondBrainfuckCommand)
;;       lineCode[2,1] <- getStackOperationCode(stackOperation)
;;       
;;       let highNybble <- lineCode[8,5]
;;       let lowNybble  <- lineCode[4,1]
;;       
;;       let numberOfBigEyes   <- decimalValueOf(highNybble) + 1
;;       let numberOfSmallEyes <- decimalValueOf(lowNybble)  + 1
;;       
;;       write numberOfBigEyes times the letter "O"
;;       write the underscore character "_"
;;       write numberOfSmallEyes times the letter "o"
;;     end if
;;   end procedure
;; 
;; 
;; Architecture
;; ============
;; A scion of brainfuck, O_o deploys a tape-like memory structure, a
;; composition of a theoretically infinite number of integer-valued
;; cells, amenable to a pointer that selects the currently active
;; instance. The language's status as an extension of this
;; stock-father's principles, natheless, incites the dation's
;; enhancement by allotting to each cell a private stack capable of
;; holding integer objects.
;; 
;; == THE TAPE: AN INFINITE AMOUNT OF ORDERED CELLS ==
;; A bilaterally infinite extension governs the memory, whose cells are
;; aligned in a linear fashion.
;; 
;; == THE CELL POINTER: A MARKER FOR THE CURRENTLY ACTIVE CELL ==
;; A cell pointer selects at any instant the currently active cell,
;; answering as the sole entity to the various indagation and
;; manipulation purposes of an O_o program. The cell pointer itself may
;; be translated in sinistral and dextral airt by two operations, being
;; empight at the start of the program on the first unit.
;; 
;; == THE CELL VALUE: A SCALAR INTEGER TO OPERATE UPON ==
;; Each cell lends harborage to two pieces of data, the first and more
;; traditionally anchored constituted by a scalar value, also known as
;; the "cell value", an integer of unbounded magnitude and any sign,
;; initalized to zero (0) at the program's inchoation.
;; 
;; This datum's indagation and manipulation appropriates the cynosure of
;; most brainfuck facilities, including incrementing and deduction, as
;; well as the jump constructs.
;; 
;; == THE CELL STACK: AN INTEGER-VALUED LIFO STORAGE ==
;; O_o's cell architecture, in a manner parallel to its entire
;; conception, administers some ampliation to the brainfuck cell
;; compassment. The retention of the scalar integer value conjoins with
;; an act of antilibration in a stack's additional affiliation.
;; 
;; This last-in-first-out (LIFO) data structure's dedication relates to
;; the storage of an arbitrary tally of signed integers. A manifestation
;; of the stack abstract data structure (ADT), this instance embraces
;; the two essential operations "push" and "pop", the former of which
;; inserts a new element unto the top, whereas the latter removes and
;; returns the item from this exact position. A kenspeckle attribute of
;; this implementation, popping from an empty stack does not eventuate
;; an error; instead the value zero (0) will be issued as a response.
;; 
;; == THE CELL STACK CONSTITUTES A COMMUNICATIVE PROPERTY ==
;; Concomitant to the brainfuck operation's unmodified behavior, which
;; vouches for the cell value's unwavering significance, three advenient
;; stack operations extend the operative faculties, capacitating the
;; bidirectional communication from the stack to the scalar datum, as
;; as well as a transfer originating from the current cell's stack to
;; that of its immedate successor in the memory.
;; 
;; 
;; Data Types
;; ==========
;; O_o's genesis from brainfuck's strain conditions its more liberal
;; notion's appropriation, augmented, however, by an integer-valued
;; stack per cell to accompany the unit's signed integer scalar, woning
;; on a linear memory that cooperates with a pointer in order to
;; designate the currently active cell. The similitude in the two
;; language's competence incites an amplexation of the character type as
;; an inferior constituent.
;; 
;; == INTEGERS DEFINE THE CELL VALUE ==
;; An acquainted element inherited from brainfuck, each cell lends a
;; salvatory to a scalar integer of any sign and magnitude.
;; 
;; == THE CELL STACK: MANY MORE INTEGERS ==
;; The most kenspeckle attribute of its augmentation, when apposed to
;; its stock-father, accounts for the introduction of a stack capable of
;; storing an arbitrary amount of unbounded signed integers, and
;; affiliated with a particular cell.
;; 
;; An attempt at removing the top element from an empty stack does not
;; produce an error, instead returning the value zero (0).
;; 
;; The stack's participation in the language does not alleviate its rank
;; from being puisne as equiparated to the preponderate cell value.
;; 
;; == CHARACTERS ==
;; The character type's commorancy is located at the paravail stratum of
;; the language, operating exclusively on the communication interfaces,
;; that is, in the query for input and the issuing of output.
;; 
;; 
;; Syntax
;; ======
;; In its syntactical matter, an O_o program consists of zero or more
;; lines, either empty or composed of "O_o" or "0_o" variations, the
;; first mode extending on both "O"/"o" expressions into between one and
;; sixteen repetitions, whereas the latter design retains a single zero,
;; expanding on the dextral "o" part into up to thirty-two occurrences.
;; 
;; == INSTRUCTIONS ==
;; Instructions in O_o appear in the forms of twains or triads, with
;; both categories allotted a line of their own, actually compact of a
;; single unbroken sequence of characters that resemble an emoji.
;; 
;; The constituents of this embrace four possible symbols: the majuscule
;; "O", the minsucle "o", the digit "0", and the underscore "_", the
;; latter apportioned a sepiment's onus.
;; 
;; In its standard format, the sinistral parcel contributes exactly a
;; moiety of the octet, namely four bits, or one nybble; a consectary
;; thereof, the representable range amounts to [0, 15], covered by the
;; minimum of zero (0) and the inclusive upper bound 15 (= 2^4). The
;; language's requirement to ascertain at least one majuscular "O" on
;; this laterality imposes an additional instance thereof. Destitute of
;; this rule, the left side could maintain zero to fifteen "O"s,
;; capacitating odd statements such as
;; 
;;   _ooo
;; 
;; which would pose a dissimilarity from an actual facial expression.
;; 
;; 
;; The augmentation by one shifts the representable range from [0, 15]
;; to the final [1, 16], conditioning that one to sixteen "O"s may
;; appear left from the separating underscore.
;; 
;; The same encheson prescribes one to sixteen minuscle "o"s for the
;; dextral part.
;; 
;; Its unilateral variant, depending upon a private diorism, relays the
;; discarded three bits for the first non-command brainfuck portion to a
;; single zero digit in lieu of the up to sixteen big eyes ("O"). The
;; dextral compartment may now seize five instead of only four bits,
;; thus permitting a length of one (1) to 32 (= 2^5) "o"s.
;; 
;; == NEWLINES ==
;; A piece of O_o source code's mandatory ingredient, any twain or triad
;; of encoded operations is segregated from its neighbors by a
;; linebreak. Its occurrence as a component of the character sequence
;; itself constitutes a prohibition.
;; 
;; Empty lines, destitute of effect, may be present. This class embraces
;; both lines lacking characters as well as such composed of spaces
;; only.
;; 
;; == SPACES ==
;; Spaces betwixt the "O_o" and "0_o" constituents are not tolerated,
;; but may be presented in the locations preceding or succeeding such a
;; sequence. Empty lines may incorporate any tally of such.
;; 
;; == COMMENTS ==
;; The current language iteration lacks provisions for comments.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation applies to
;; the O_o programming language:
;; 
;;   program     := [ linebreaks ]
;;               ,  [ ( commandLine  emptyLine ) ]
;;               ,  { linebreaks , ( commandLine | emptyLine ) }
;;               ,  [ linebreaks ]
;;               ;
;;   commandLine := [ spaces ] , command , [ spaces ] ;
;;   emptyLine   := [ spaces ] ;
;;   command     := O_o
;;               |  O_oo
;;               |  O_ooo
;;               |  O_oooo
;;               |  O_ooooo
;;               |  O_oooooo
;;               |  O_ooooooo
;;               |  O_oooooooo
;;               |  O_ooooooooo
;;               |  O_oooooooooo
;;               |  O_ooooooooooo
;;               |  O_oooooooooooo
;;               |  O_ooooooooooooo
;;               |  O_oooooooooooooo
;;               |  O_ooooooooooooooo
;;               |  OO_o
;;               |  OO_oo
;;               |  OO_ooo
;;               |  OO_oooo
;;               |  OO_ooooo
;;               |  OO_oooooo
;;               |  OO_ooooooo
;;               |  OO_oooooooo
;;               |  OO_ooooooooo
;;               |  OO_oooooooooo
;;               |  OO_ooooooooooo
;;               |  OO_oooooooooooo
;;               |  OO_ooooooooooooo
;;               |  OO_oooooooooooooo
;;               |  OO_ooooooooooooooo
;;               |  OOO_o
;;               |  OOO_oo
;;               |  OOO_ooo
;;               |  OOO_oooo
;;               |  OOO_ooooo
;;               |  OOO_oooooo
;;               |  OOO_ooooooo
;;               |  OOO_oooooooo
;;               |  OOO_ooooooooo
;;               |  OOO_oooooooooo
;;               |  OOO_ooooooooooo
;;               |  OOO_oooooooooooo
;;               |  OOO_ooooooooooooo
;;               |  OOO_oooooooooooooo
;;               |  OOO_ooooooooooooooo
;;               |  OOOO_o
;;               |  OOOO_oo
;;               |  OOOO_ooo
;;               |  OOOO_oooo
;;               |  OOOO_ooooo
;;               |  OOOO_oooooo
;;               |  OOOO_ooooooo
;;               |  OOOO_oooooooo
;;               |  OOOO_ooooooooo
;;               |  OOOO_oooooooooo
;;               |  OOOO_ooooooooooo
;;               |  OOOO_oooooooooooo
;;               |  OOOO_ooooooooooooo
;;               |  OOOO_oooooooooooooo
;;               |  OOOO_ooooooooooooooo
;;               |  OOOOO_o
;;               |  OOOOO_oo
;;               |  OOOOO_ooo
;;               |  OOOOO_oooo
;;               |  OOOOO_ooooo
;;               |  OOOOO_oooooo
;;               |  OOOOO_ooooooo
;;               |  OOOOO_oooooooo
;;               |  OOOOO_ooooooooo
;;               |  OOOOO_oooooooooo
;;               |  OOOOO_ooooooooooo
;;               |  OOOOO_oooooooooooo
;;               |  OOOOO_ooooooooooooo
;;               |  OOOOO_oooooooooooooo
;;               |  OOOOO_ooooooooooooooo
;;               |  OOOOOO_o
;;               |  OOOOOO_oo
;;               |  OOOOOO_ooo
;;               |  OOOOOO_oooo
;;               |  OOOOOO_ooooo
;;               |  OOOOOO_oooooo
;;               |  OOOOOO_ooooooo
;;               |  OOOOOO_oooooooo
;;               |  OOOOOO_ooooooooo
;;               |  OOOOOO_oooooooooo
;;               |  OOOOOO_ooooooooooo
;;               |  OOOOOO_oooooooooooo
;;               |  OOOOOO_ooooooooooooo
;;               |  OOOOOO_oooooooooooooo
;;               |  OOOOOO_ooooooooooooooo
;;               |  OOOOOOO_o
;;               |  OOOOOOO_oo
;;               |  OOOOOOO_ooo
;;               |  OOOOOOO_oooo
;;               |  OOOOOOO_ooooo
;;               |  OOOOOOO_oooooo
;;               |  OOOOOOO_ooooooo
;;               |  OOOOOOO_oooooooo
;;               |  OOOOOOO_ooooooooo
;;               |  OOOOOOO_oooooooooo
;;               |  OOOOOOO_ooooooooooo
;;               |  OOOOOOO_oooooooooooo
;;               |  OOOOOOO_ooooooooooooo
;;               |  OOOOOOO_oooooooooooooo
;;               |  OOOOOOO_ooooooooooooooo
;;               |  OOOOOOOO_o
;;               |  OOOOOOOO_oo
;;               |  OOOOOOOO_ooo
;;               |  OOOOOOOO_oooo
;;               |  OOOOOOOO_ooooo
;;               |  OOOOOOOO_oooooo
;;               |  OOOOOOOO_ooooooo
;;               |  OOOOOOOO_oooooooo
;;               |  OOOOOOOO_ooooooooo
;;               |  OOOOOOOO_oooooooooo
;;               |  OOOOOOOO_ooooooooooo
;;               |  OOOOOOOO_oooooooooooo
;;               |  OOOOOOOO_ooooooooooooo
;;               |  OOOOOOOO_oooooooooooooo
;;               |  OOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOO_o
;;               |  OOOOOOOOO_oo
;;               |  OOOOOOOOO_ooo
;;               |  OOOOOOOOO_oooo
;;               |  OOOOOOOOO_ooooo
;;               |  OOOOOOOOO_oooooo
;;               |  OOOOOOOOO_ooooooo
;;               |  OOOOOOOOO_oooooooo
;;               |  OOOOOOOOO_ooooooooo
;;               |  OOOOOOOOO_oooooooooo
;;               |  OOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOOO_o
;;               |  OOOOOOOOOO_oo
;;               |  OOOOOOOOOO_ooo
;;               |  OOOOOOOOOO_oooo
;;               |  OOOOOOOOOO_ooooo
;;               |  OOOOOOOOOO_oooooo
;;               |  OOOOOOOOOO_ooooooo
;;               |  OOOOOOOOOO_oooooooo
;;               |  OOOOOOOOOO_ooooooooo
;;               |  OOOOOOOOOO_oooooooooo
;;               |  OOOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOOOO_o
;;               |  OOOOOOOOOOO_oo
;;               |  OOOOOOOOOOO_ooo
;;               |  OOOOOOOOOOO_oooo
;;               |  OOOOOOOOOOO_ooooo
;;               |  OOOOOOOOOOO_oooooo
;;               |  OOOOOOOOOOO_ooooooo
;;               |  OOOOOOOOOOO_oooooooo
;;               |  OOOOOOOOOOO_ooooooooo
;;               |  OOOOOOOOOOO_oooooooooo
;;               |  OOOOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOOOOO_o
;;               |  OOOOOOOOOOOO_oo
;;               |  OOOOOOOOOOOO_ooo
;;               |  OOOOOOOOOOOO_oooo
;;               |  OOOOOOOOOOOO_ooooo
;;               |  OOOOOOOOOOOO_oooooo
;;               |  OOOOOOOOOOOO_ooooooo
;;               |  OOOOOOOOOOOO_oooooooo
;;               |  OOOOOOOOOOOO_ooooooooo
;;               |  OOOOOOOOOOOO_oooooooooo
;;               |  OOOOOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOOOOOO_o
;;               |  OOOOOOOOOOOOO_oo
;;               |  OOOOOOOOOOOOO_ooo
;;               |  OOOOOOOOOOOOO_oooo
;;               |  OOOOOOOOOOOOO_ooooo
;;               |  OOOOOOOOOOOOO_oooooo
;;               |  OOOOOOOOOOOOO_ooooooo
;;               |  OOOOOOOOOOOOO_oooooooo
;;               |  OOOOOOOOOOOOO_ooooooooo
;;               |  OOOOOOOOOOOOO_oooooooooo
;;               |  OOOOOOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOOOOOOO_o
;;               |  OOOOOOOOOOOOOO_oo
;;               |  OOOOOOOOOOOOOO_ooo
;;               |  OOOOOOOOOOOOOO_oooo
;;               |  OOOOOOOOOOOOOO_ooooo
;;               |  OOOOOOOOOOOOOO_oooooo
;;               |  OOOOOOOOOOOOOO_ooooooo
;;               |  OOOOOOOOOOOOOO_oooooooo
;;               |  OOOOOOOOOOOOOO_ooooooooo
;;               |  OOOOOOOOOOOOOO_oooooooooo
;;               |  OOOOOOOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOOOOOOO_ooooooooooooooo
;;               |  OOOOOOOOOOOOOOO_o
;;               |  OOOOOOOOOOOOOOO_oo
;;               |  OOOOOOOOOOOOOOO_ooo
;;               |  OOOOOOOOOOOOOOO_oooo
;;               |  OOOOOOOOOOOOOOO_ooooo
;;               |  OOOOOOOOOOOOOOO_oooooo
;;               |  OOOOOOOOOOOOOOO_ooooooo
;;               |  OOOOOOOOOOOOOOO_oooooooo
;;               |  OOOOOOOOOOOOOOO_ooooooooo
;;               |  OOOOOOOOOOOOOOO_oooooooooo
;;               |  OOOOOOOOOOOOOOO_ooooooooooo
;;               |  OOOOOOOOOOOOOOO_oooooooooooo
;;               |  OOOOOOOOOOOOOOO_ooooooooooooo
;;               |  OOOOOOOOOOOOOOO_oooooooooooooo
;;               |  OOOOOOOOOOOOOOO_ooooooooooooooo
;;               |  0_o
;;               |  0_oo
;;               |  0_ooo
;;               |  0_oooo
;;               |  0_ooooo
;;               |  0_oooooo
;;               |  0_ooooooo
;;               |  0_oooooooo
;;               |  0_ooooooooo
;;               |  0_oooooooooo
;;               |  0_ooooooooooo
;;               |  0_oooooooooooo
;;               |  0_ooooooooooooo
;;               |  0_oooooooooooooo
;;               |  0_ooooooooooooooo
;;               |  0_oooooooooooooooo
;;               |  0_ooooooooooooooooo
;;               |  0_oooooooooooooooooo
;;               |  0_ooooooooooooooooooo
;;               |  0_oooooooooooooooooooo
;;               |  0_ooooooooooooooooooooo
;;               |  0_oooooooooooooooooooooo
;;               |  0_ooooooooooooooooooooooo
;;               |  0_oooooooooooooooooooooooo
;;               |  0_ooooooooooooooooooooooooo
;;               |  0_oooooooooooooooooooooooooo
;;               |  0_ooooooooooooooooooooooooooo
;;               |  0_oooooooooooooooooooooooooooo
;;               |  0_ooooooooooooooooooooooooooooo
;;               |  0_oooooooooooooooooooooooooooooo
;;               |  0_ooooooooooooooooooooooooooooooo
;;               ;
;;   linebreaks  := linebreak , { linebreak } ;
;;   linebreak   := "\n" ;
;;   spaces      := space , { space } ;
;;   space       := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; O_o's legacy comprehends the verbatim appropriation of brainfuck's
;; octuple instruction set, as well as an augmented roster of four
;; stack operations.
;; 
;; == OVERVIEW OF BRAINFUCK COMMANDS ==
;; With its derivation from brainfuck, the memory's operations as well
;; as the cells' scalar value compartment are not eloigned in the least
;; from the legacy, as demonstrated alow:
;; 
;;   ------------------------------------------------------------------
;;   Code | Plain token | Command
;;   -----+-------------+----------------------------------------------
;;   000  | >           | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   001  | <           | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   010  | +           | Increments the current cell value by one.
;;   ..................................................................
;;   011  | -           | Decrements the current cell value by one.
;;   ..................................................................
;;   100  | .           | Outputs the character corresponding to the
;;        |             | current cell value interpreted as an ASCII
;;        |             | code.
;;   ..................................................................
;;   101  | ,           | Queries for an ASCII character input and
;;        |             | stores its character code in the current
;;        |             | cell's value.
;;   ..................................................................
;;   110  | [           | If the current cell value equals zero (0),
;;        |             | moves the instruction pointer forward to the
;;        |             | position immediately succeeding the matching
;;        |             | closing bracket "]". Otherwise proceeds as
;;        |             | usual.
;;   ..................................................................
;;   111  | ]           | If the current cell value does not equal zero
;;        |             | (0), moves the instruction pointer back to
;;        |             | the position immediately succeeding the
;;        |             | matching opening bracket "[". Otherwise
;;        |             | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW OF STACK OPERATIONS ==
;; Siclike to the cleronomy from brainfuck's instructions, the
;; operations appertaining to the stack, its designment solely reserved
;; for a cell's LIFO storage, shall be elucidated:
;; 
;;   ------------------------------------------------------------------
;;   Code | Stack operation
;;   -----+------------------------------------------------------------
;;   00   | No effect (no-operation, NOP).
;;   ..................................................................
;;   01   | Pushes the value of the current cell unto the cell's stack.
;;   ..................................................................
;;   10   | Pops the top element from the current cell's stack and
;;        | stores it in the cell's value.
;;        | If the stack is empty, the default quantity of zero (0)
;;        | will copied into the cell value.
;;   ..................................................................
;;   11   | Pops the top element of the current cell's stack, and
;;        | pushes it unto the stack of the cell to the right of the
;;        | cell pointer.
;;        | If the current cell's stack is empty, the default quantity
;;        | of zero (0) will be copied to the neighboring stack.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The O_o language's pellucidity in elucidation and demonstration
;; permits torely any ambiguity's inroad. Merely two points may be
;; adduced for disquisition, concerning the spaces and vacant lines'
;; participation.
;; 
;; == ARE CIRCUMAMBIENT SPACES IN AN "O_o" PATTERN HOMOLOGATED? ==
;; Involved in the proterolog's material, an instruction line's design
;; is consigned to a very precise diorism. It is, however, not expressed
;; whether spaces may precede or succeed the "O_o" or "0_o" pattern.
;; 
;; In the face of such elements' innocuous nature, spaces, which embrace
;; the space and horizontal tab character, have been adjudged a
;; tolerated content if not disturbing an "O_o" or "0_o" segment.
;; 
;; == MAY EMPTY LINES EXIST? ==
;; It is not stated whether empty lines, whose compass circumscribes
;; both rows without content and such compact of spaces only, may apply
;; themselves as participants in an O_o program.
;; 
;; It has been deemed that such vacant extents do not infringe on the
;; foundational concepts of the O_o language; as a consectary, their
;; introduction is homologated.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter, decoder, encoder and converters are implemented in
;; the Common Lisp programming language.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-01-04
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/O_o"
;;   -> "https://en.wikipedia.org/wiki/Units_of_information"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tally ()
  "The ``tally'' type defines a non-negative integer suitable for
   representing a counting result."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype dibit ()
  "The ``dibit'' type defines an unsigned two-bit compound."
  '(unsigned-byte 2))

;;; -------------------------------------------------------

(deftype tribit ()
  "The ``tribit'' type defines an unsigned three-bit compound."
  '(unsigned-byte 3))

;;; -------------------------------------------------------

(deftype nybble ()
  "The ``nybble'' type defines an unsigned four-bit compound."
  '(unsigned-byte 4))

;;; -------------------------------------------------------

(deftype pentad ()
  "The ``pentad'' type defines an unsigned five-bit compound."
  '(unsigned-byte 5))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype brainfuck-command ()
  "The ``brainfuck-command'' type enumerates a subset of the recognized
   O_o instructions whichs relate to brainfuck capabilities, augmented
   by a sentinel ``:nop'' (no-operation) member."
  '(member
    :nop
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype stack-command ()
  "The ``stack-command'' type enumerates a subset of the recognized O_o
   instructions which relates to the stack manipulation."
  '(member
    :stack-nop
    :push-cell-value-unto-stack
    :pop-to-cell-value
    :pop-to-next-stack))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' enumerates the recognized O_o instruction types."
  '(or brainfuck-command
       stack-command))

;;; -------------------------------------------------------

(deftype line-format ()
  "The ``line-format'' type enumerates the possible formats in which an
   O_o instruction line may appear.
   ---
   Two members can be distinguished:
   
     ------------------------------------------------------------------
     Line format | Description
     ------------+-----------------------------------------------------
     :bilateral  | The common format which bears a sequence of one to
                 | sixteen 'O' majuscles on the left-hand side,
                 | accompanied by one to sixteen 'o' minuscles on the
                 | athwart laterality.
     ..................................................................
     :unilateral | The special format which accommodates a single zero
                 | digit ('0') on the left-hand side, accompanied by
                 | one to thirty-two 'o' minuscles on the athwart
                 | laterality.
     ------------------------------------------------------------------"
  '(member :bilateral :unilateral))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T) size)
  "The ``list-of'' type defines a list of either SIZE, or if omitted,
   zero or more elements, each member of which conforms to the
   ELEMENT-TYPE, defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (eq size '*)
                (= (the tally (length (the list candidate)))
                   (the tally size)))
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype o_o-program ()
  "The ``o_o-program'' type defines an O_o program as a vector of zero
   or more ``command''s."
  '(vector command *))

;;; -------------------------------------------------------

(deftype instruction-triplet ()
  "The ``instruction-triplet'' type defines a list of three commands,
   commonly employed in the representation of an O_o-encoded line's
   treble operations, expected to be composed of two brainfuck commands
   and one stack operation."
  '(list-of command 3))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype jump-table ()
  "The ``jump-table'' type defines an affiliation of forward jump
   instruction positions in an instruction vector to the matching back
   jump locations, and vice versa, implemented as a hash table with
   fixnum keys mapped to fixnum values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based last-in-first-out (LIFO)
   datastructure intended for the maintenance of signed integer
   objects."
  '(list-of integer))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse collection of cells,
   represented as a hash table whose signed integer keys model the cell
   indices, affiliated with ``Cell'' objects."
  '(hash-table-of integer Cell))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   embracing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of decoder.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-brainfuck-command (code)
  "Returns the brainfuck command associated with the three-bit CODE, or
   signals an error of an unspecified type upon a disrespondency."
  (declare (type tribit code))
  (the brainfuck-command
    (case code
      (#b000 :move-right)
      (#b001 :move-left)
      (#b010 :increment)
      (#b011 :decrement)
      (#b100 :output)
      (#b101 :input)
      (#b110 :jump-forward)
      (#b111 :jump-back)
      (otherwise
        (error "No brainfuck command is associated with the three ~
                bits ~3,'0b."
          code)))))

;;; -------------------------------------------------------

(defun decode-stack-command (code)
  "Returns the stack operation associated with the two-bit CODE, or
   signals an error of an unspecified type upon a disrespondency."
  (declare (type dibit code))
  (the stack-command
    (case code
      (#b00 :stack-nop)
      (#b01 :push-cell-value-unto-stack)
      (#b10 :pop-to-cell-value)
      (#b11 :pop-to-next-stack)
      (otherwise
        (error "No stack operation is associated with the two ~
                bits ~2,'0b."
          code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character functions.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Checks whether the CANDIDATE represents a space character, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Line".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Line ()
  ((source
    :initarg       :source
    :initform      NIL
    :type          (or null string)
    :documentation "The line characters to analyze.")
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
                    SOURCE.")
   
   (format
    :initarg       :format
    :initform      :bilateral
    :type          line-format
    :documentation "Determines the line's structure, that is, whether
                    the standard O-o design
                      O..._o...
                    holds or the special zero-o pattern
                      0_o...")
   (number-of-big-eyes
    :initarg       :number-of-big-eyes
    :initform      0
    :type          tally
    :documentation "The tally of majuscular 'O' letters on the left, or
                    zeroes ('0') on that laterality.")
   (high-nybble
    :initarg       :high-nybble
    :initform      0
    :type          nybble
    :documentation "Contains the three or four most significant bits of
                    the line's 8-bit binary representation, obtained by
                    analyzing the left-hand side, composed either of
                    majuscular 'O's or a single zero ('0').")
   (number-of-small-eyes
    :initarg       :number-of-small-eyes
    :initform      0
    :type          tally
    :documentation "The number of minuscular 'o' letters on the right.")
   (low-nybble
    :initarg       :low-nybble
    :initform      0
    :type          (or nybble pentad)
    :documentation "Contains the four or five least significant bits of
                    the line's 8-bit binary representation, obtained by
                    analyzing the right-hand side, composed either of
                    up to 16 minuscular 'o's, in the case of the
                    left-hand side's 'O' format, or up to 32 'o'
                    minuscules, if the sinistral compartment entails a
                    single zero ('0').")
   (line-code
    :initarg       :line-code
    :initform      0
    :type          octet
    :documentation "The line's 8-bit binary representation, obtained by
                    splicing the HIGH-NYBBLE and the LOW-NYBBLE into a
                    single amalgam, and containing in its highest three
                    bits the first brainfuck command's code, in the next
                    three the second brainfuck command code, and in the
                    desinent two the stack operation's.")
   
   (first-brainfuck-command
    :initarg       :first-brainfuck-command
    :initform      NIL
    :type          (or null brainfuck-command)
    :documentation "The brainfuck command extracted from the line's
                    left side, containing the majuscular 'O's.")
   (second-brainfuck-command
    :initarg       :second-brainfuck-command
    :initform      NIL
    :type          (or null brainfuck-command)
    :documentation "The brainfuck command extracted from the line's
                    right side, containing the minuscular 'o's.")
   (stack-command
    :initarg       :stack-command
    :initform      NIL
    :type          (or null stack-command)
    :documentation "The stack operation extracted from the line's
                    right side, containing the minuscular 'o's."))
  (:documentation
    "The ``Line'' class encapsulates the pertinent information gleaned
     by analyzing a line string's characters."))

;;; -------------------------------------------------------

(defun make-line ()
  "Creates and returns a new empty ``Line''."
  (the Line
    (make-instance 'Line)))

;;; -------------------------------------------------------

(defun line-set-source (line new-source)
  "Sets the LINE's content to the NEW-SOURCE, resets its state, and
   returns the modified LINE."
  (declare (type Line   line))
  (declare (type string new-source))
  (with-slots (source position character) line
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf source   new-source)
    (setf position 0)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (setf (slot-value line 'format)                   :bilateral)
  (setf (slot-value line 'number-of-big-eyes)       0)
  (setf (slot-value line 'high-nybble)              0)
  (setf (slot-value line 'number-of-small-eyes)     0)
  (setf (slot-value line 'low-nybble)               0)
  (setf (slot-value line 'line-code)                0)
  (setf (slot-value line 'first-brainfuck-command)  NIL)
  (setf (slot-value line 'second-brainfuck-command) NIL)
  (setf (slot-value line 'stack-command)            NIL)
  (the Line line))

;;; -------------------------------------------------------

(defun line-empty-p (line)
  "Checks whether a LINE is empty, that is, either contains no
   characters or is composed of spaces only, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line line))
  (the boolean
    (not (null
      (every #'space-character-p
        (slot-value line 'source))))))

;;; -------------------------------------------------------

(defun line-advance (line)
  "Moves the LINE's position cursor to the next character in its source,
   if possible, updates its state, and returns the modified LINE."
  (declare (type Line line))
  (with-slots (source position character) line
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Line line))

;;; -------------------------------------------------------

(defun line-skip-spaces (line)
  "Starting at the current position in the LINE, skips a sequence of
   zero or more adjacent spaces and returns the modified LINE."
  (declare (type Line line))
  (with-slots (character) line
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (line-advance line)))
  (the Line line))

;;; -------------------------------------------------------

(defun line-determine-format (line)
  "Inquires into the LINE's current character and, based upon its value,
   concludes the line format, memorizing the same, and returning the
   modified LINE."
  (declare (type Line line))
  (with-slots (character position format) line
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (type line-format         format))
    (setf format
      (case character
        (#\O :bilateral)
        (#\0 :unilateral)
        (otherwise
          (error "Invalid incipient character ~s on the left ~
                  at the position ~d."
            character position)))))
  (the Line line))

;;; -------------------------------------------------------

(defun line-count-character (line expected-character)
  "Starting at the current position into the LINE's source, consumes
   zero or more adjacent occurrences of the EXPECTED-CHARACTER and
   returns their tally."
  (declare (type Line      line))
  (declare (type character expected-character))
  (with-slots (character) line
    (declare (type (or null character) character))
    (the tally
      (loop
        while (and character (char= character expected-character))
        do    (line-advance line)
        count 1))))

;;; -------------------------------------------------------

(defun line-signal-format-error (line)
  "Signals an error of an unspecified type imparting intelligence about
   the LINE maintaining an invalid format."
  (declare (type Line line))
  (error "Invalid line format: ~s." (slot-value line 'format)))

;;; -------------------------------------------------------

(defun line-expect-zero-character (line)
  "Checks whether the character at the LINE's current position
   constitutes a zero digit ('0'), on confirmation advancing the cursor
   beyond it and returning the modified LINE, otherwise signaling an
   error of an unspecified type."
  (declare (type Line line))
  (with-slots (character position) line
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (if (and character (char= character #\0))
      (line-advance line)
      (error "Expected a zero character (\"0\") at position ~d, ~
              but encountered ~s."
        position character)))
  (the Line line))

;;; -------------------------------------------------------

(defun line-check-number-of-big-eyes (line)
  "Checks whether the number of eyes on the left-hand side, depending on
   the LINE's format, constitutes a valid value, on confirmation simply
   returning the LINE, otherwise signaling an error of an unspecified
   type."
  (declare (type Line line))
  (with-slots (number-of-big-eyes format) line
    (declare (type tally       number-of-big-eyes))
    (declare (type line-format format))
    (case format
      (:bilateral
        (unless (<= 1 number-of-big-eyes 16)
          (error "Invalid number of \"O\"s: ~d." number-of-big-eyes)))
      (:unilateral
        (unless (= number-of-big-eyes 1)
          (error "Invalid number of big eyes: ~d." number-of-big-eyes)))
      (otherwise
        (line-signal-format-error line))))
  (the Line line))

;;; -------------------------------------------------------

(defun line-count-big-eyes (line)
  "Starting at the current position into the LINE's source, depending on
   the line format, either tallies the number of adjacent minuscular 'O'
   characters or expects a single zero ('0') character, relocates the
   cursor beyond the segment, stores the number, and returns the
   modified LINE."
  (declare (type Line line))
  (with-slots (format number-of-big-eyes) line
    (declare (type line-format format))
    (declare (type tally       number-of-big-eyes))
    (case format
      (:bilateral
        (setf number-of-big-eyes (line-count-character line #\O)))
      (:unilateral
        (line-expect-zero-character line)
        (setf number-of-big-eyes 1))
      (otherwise
        (line-signal-format-error line))))
  (line-check-number-of-big-eyes line)
  (the Line line))

;;; -------------------------------------------------------

(defun line-extract-high-nybble (line)
  "Calculates from number of big eyes on the left-hand side the line
   code's most significant bits, stores it, and returns the modified
   LINE."
  (declare (type Line line))
  (with-slots (number-of-big-eyes high-nybble) line
    (declare (type tally  number-of-big-eyes))
    (declare (type nybble high-nybble))
    (setf high-nybble (1- number-of-big-eyes)))
  (the Line line))

;;; -------------------------------------------------------

(defun line-expect-underscore (line)
  "Checks whether LINE's current character represents an underscore
   ('_'), on confirmation advancing the position cursor and returning
   the modified LINE, otherwise signaling an error of an unspecified
   type."
  (declare (type Line line))
  (with-slots (character position) line
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (if (and character (char= character #\_))
      (line-advance line)
      (error "Expected an underscore (\"_\") at position ~d, ~
              but encountered ~s."
        position character)))
  (the Line line))

;;; -------------------------------------------------------

(defun line-check-number-of-small-eyes (line)
  "Checks whether the number of eyes on the right-hand side, depending
   on the LINE's format, constitutes a valid value, on confirmation
   simply returning the LINE, otherwise signaling an error of an
   unspecified type."
  (declare (type Line line))
  (with-slots (number-of-small-eyes format) line
    (declare (type tally       number-of-small-eyes))
    (declare (type line-format format))
    (case format
      (:bilateral
        (unless (<= 1 number-of-small-eyes 16)
          (error "Invalid number of \"o\"s: ~d." number-of-small-eyes)))
      (:unilateral
        (unless (<= 1 number-of-small-eyes 32)
          (error "Invalid number of \"o\"s: ~d." number-of-small-eyes)))
      (otherwise
        (line-signal-format-error line))))
  (the Line line))

;;; -------------------------------------------------------

(defun line-count-small-eyes (line)
  "Starting at the current position into the LINE's source, tallies the
   number of adjacent minuscular 'o' characters, relocates the cursor
   beyond these, stores the number, and returns the modified LINE."
  (declare (type Line line))
  (setf (slot-value line 'number-of-small-eyes)
    (line-count-character line #\o))
  (line-check-number-of-small-eyes line)
  (the Line line))

;;; -------------------------------------------------------

(defun line-extract-low-nybble (line)
  "Determines the nybble or pentad represented by the minuscular 'o's on
   the LINE's dextral laterality following the underscore ('_'), stores
   it, and returns the modified LINE."
  (declare (type Line line))
  (setf (slot-value line 'low-nybble)
    (1- (slot-value line 'number-of-small-eyes)))
  (the Line line))

;;; -------------------------------------------------------

(defun line-build-line-code (line)
  "Based upon the LINE's two analyzed parts, assembles a single 8-bit
   binary encoding ensconcing the two brainfuck commands and the one
   stack operation, stores this octet in the LINE, and returns the
   modified LINE."
  (declare (type Line line))
  (with-slots (line-code low-nybble format) line
    (declare (type octet              line-code))
    (declare (type (or nybble pentad) low-nybble))
    (declare (type line-format        format))
    (setf line-code
      (case format
        (:bilateral
          (dpb (slot-value line 'high-nybble) (byte 4 4) low-nybble))
        (:unilateral
          low-nybble)
        (otherwise
          (line-signal-format-error line)))))
  (the Line line))

;;; -------------------------------------------------------

(defun line-extract-commands (line)
  "Extracts from the LINE's binary representation the two entailed
   brainfuck commands and the single stack operation, stores them, and
   returns the modified LINE."
  (declare (type Line line))
  ;; The three most significant bits encode the first brainfuck command.
  (setf (slot-value line 'first-brainfuck-command)
    (case (slot-value line 'format)
      (:bilateral
        (decode-brainfuck-command
          (ldb (byte 3 5)
            (slot-value line 'line-code))))
      (:unilateral
        :nop)
      (otherwise
        (line-signal-format-error line))))
  ;; The next three bits encode the second brainfuck command.
  (setf (slot-value line 'second-brainfuck-command)
    (decode-brainfuck-command
      (ldb (byte 3 2)
        (slot-value line 'line-code))))
  ;; The desinent two bits encode the stack operation.
  (setf (slot-value line 'stack-command)
    (decode-stack-command
      (ldb (byte 2 0)
        (slot-value line 'line-code))))
  (the Line line))

;;; -------------------------------------------------------

(defun line-expect-end-of-line (line)
  "Checks whether the LINE's source, starting at its current position,
   is either exhausted or composed of spaces only, on confirmation
   consuming this homologated content and returning the modified LINE,
   otherwise signaling an error of an unspecified type."
  (declare (type Line line))
  (line-skip-spaces line)
  (with-slots (character) line
    (declare (type (or null character) character))
    (unless (null character)
      (error "Expected end of line, but encountered ~c." character)))
  (the Line line))

;;; -------------------------------------------------------

(defun line-evaluate (line)
  "Analyzes the content of the LINE and returns the modified LINE."
  (declare (type Line line))
  (line-skip-spaces         line)
  (line-determine-format    line)
  (line-count-big-eyes      line)
  (line-extract-high-nybble line)
  (line-expect-underscore   line)
  (line-count-small-eyes    line)
  (line-extract-low-nybble  line)
  (line-build-line-code     line)
  (line-extract-commands    line)
  (line-expect-end-of-line  line)
  (the Line line))

;;; -------------------------------------------------------

(defun line-get-first-brainfuck-command (line)
  "Returns the first decoded brainfuck command extracted from the LINE."
  (declare (type Line line))
  (the command
    (slot-value line 'first-brainfuck-command)))

;;; -------------------------------------------------------

(defun line-get-second-brainfuck-command (line)
  "Returns the second decoded brainfuck command extracted from the
   LINE."
  (declare (type Line line))
  (the command
    (slot-value line 'second-brainfuck-command)))

;;; -------------------------------------------------------

(defun line-get-stack-command (line)
  "Returns the decoded stack operation extracted from the LINE."
  (declare (type Line line))
  (the command
    (slot-value line 'stack-command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of O_o decoder.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-O_o (code)
  "Decodes the piece of O_o source CODE and returns a one-dimensional
   simple array containing its instructions."
  (declare (type string code))
  (let ((line (make-line)))
    (declare (type Line line))
    (let ((instructions NIL))
      (declare (type (list-of command) instructions))
      (flet ((collect-instruction (instruction)
              (declare (type command instruction))
              (push instruction instructions)
              (values)))
        (with-input-from-string (input code)
          (declare (type string-stream input))
          (loop
            for line-string
              of-type (or null string)
              =       (read-line input NIL NIL)
            while line-string
            do
              (line-set-source line line-string)
              (unless (line-empty-p line)
                (line-evaluate line)
                (collect-instruction
                  (line-get-first-brainfuck-command line))
                (collect-instruction
                  (line-get-second-brainfuck-command line))
                (collect-instruction
                  (line-get-stack-command line))))))
      (the (simple-array command (*))
        (coerce (nreverse instructions)
          '(simple-array command (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-cell ()))
  "The ``Cell'' class represents a cell in the program memory, defined
   by its scalar integer value and a stack of integers."
  (value 0   :type integer)
  (stack NIL :type stack))

;;; -------------------------------------------------------

(defun cell-pop (cell)
  "Pops and returns from the CELL the top element, or responds with a
   value of zero (0) upon its vacancy."
  (declare (type Cell cell))
  (the integer
    (or (pop (cell-stack cell))
        0)))

;;; -------------------------------------------------------

(defun cell-push (cell new-element)
  "Pushes unto the CELL's stack the NEW-ELEMENT and returns the modified
   CELL."
  (declare (type Cell    cell))
  (declare (type integer new-element))
  (push new-element (cell-stack cell))
  (the Cell cell))

;;; -------------------------------------------------------

(defun cell-push-value-unto-stack (cell)
  "Pushes the CELL value unto its own stack and returns the modified
   CELL."
  (declare (type Cell cell))
  (cell-push cell (cell-value cell))
  (the Cell cell))

;;; -------------------------------------------------------

(defun cell-pop-from-stack-to-value (cell)
  "Pops from the CELL's stack the top element, or, upon its vacancy,
   employs a value of zero (0), and stores it in the CELL's value,
   returning the modified CELL."
  (declare (type Cell cell))
  (setf (cell-value cell) (cell-pop cell))
  (the Cell cell))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents a bilaterally infinite linear
   ordonnance of cells, with the active unit being selected by a cell
   pointer."
  (cells   (make-hash-table :test #'eql) :type cell-table)
  (pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun memory-ensure-cell-at (memory index)
  "Ensures the existence of the MEMORY cell at the INDEX by creating and
   registering such if not yet present, in any case returning the
   MEMORY."
  (declare (type Memory  memory))
  (declare (type integer index))
  (unless (nth-value 1 (gethash index (memory-cells memory)))
    (setf (gethash index (memory-cells memory))
          (make-cell)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-cell-at (memory index)
  "Returns the MEMORY cell at the INDEX."
  (declare (type Memory  memory))
  (declare (type integer index))
  (memory-ensure-cell-at memory index)
  (the Cell
    (gethash index
      (memory-cells memory))))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell."
  (declare (type Memory memory))
  (the Cell
    (memory-cell-at memory
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun memory-current-cell-value (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the integer
    (cell-value (memory-current-cell memory))))

;;; -------------------------------------------------------

(defun (setf memory-current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns the
   modified MEMORY."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (cell-value (memory-current-cell memory)) new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell value by one and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (incf (cell-value (memory-current-cell memory)) 1)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell value by one and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (decf (cell-value (memory-current-cell memory)) 1)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-cell-zero-p (memory)
  "Checks whether the MEMORY's current cell contains the value zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (null
      (zerop (cell-value (memory-current-cell memory)))))))

;;; -------------------------------------------------------

(defun memory-next-cell (memory)
  "Returns the MEMORY cell immediately succeeding the current one."
  (declare (type Memory memory))
  (the Cell
    (memory-cell-at memory
      (1+ (memory-pointer memory)))))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (incf (memory-pointer memory) 1)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (decf (memory-pointer memory) 1)
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Computes and returns a jump table for the INSTRUCTIONS, associating
   with each forward jump instruction's location in the same the
   position of the matching back jump operation, and vice versa."
  (declare (type o_o-program instructions))
  (let ((jump-table    (make-hash-table :test #'eql))
        (forward-jumps NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jumps))
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0 by 1
      do
        (case instruction
          (:jump-forward
            (push position forward-jumps))
          (:jump-back
            (if forward-jumps
              (let ((start-position (pop forward-jumps))
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (setf (gethash start-position jump-table) end-position)
                (setf (gethash end-position jump-table) start-position))
              (error "Unmatched back jump instruction at position ~d."
                position)))
          (otherwise
            NIL)))
    (when forward-jumps
      (error "Unmatched forward jump instructions at ~
              positions ~{~d~^, ~}."
        forward-jumps))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the O_o INSTRUCTIONS and returns no value."
  (declare (type o_o-program instructions))
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (jump-table  (build-jump-table instructions))
          (memory      (make-memory)))
      (declare (type fixnum            ip))
      (declare (type (or null command) instruction))
      (declare (type jump-table        jump-table))
      (declare (type Memory            memory))
      
      (flet ((advance ()
              "Advances the instruction pointer IP to the next element
               in the INSTRUCTIONS vector, if possible, updates the
               current INSTRUCTION, and returns no value."
              (setf instruction
                (when (array-in-bounds-p instructions (1+ ip))
                  (aref instructions (incf ip))))
              (values))
             
             (jump-to-opposite-boundary ()
              "Expecting to be located as a jump forward or back
               instruction, relocates the instruction pointer IP to the
               opposite boundary, updates the current INSTRUCTION, and
               returns no value."
              (setf ip (gethash ip jump-table))
              (setf instruction
                (when (array-in-bounds-p instructions ip)
                  (aref instructions ip)))
              (values)))
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:nop
              NIL)
            
            (:move-right
              (memory-move-right memory))
            
            (:move-left
              (memory-move-left memory))
            
            (:increment
              (memory-increment memory))
            
            (:decrement
              (memory-decrement memory))
            
            (:output
              (write-char
                (code-char
                  (memory-current-cell-value memory))))
            
            (:input
              (format T "~&Please input an ASCII character: ")
              (setf (memory-current-cell-value memory)
                    (char-code (read-char)))
              (clear-input))
            
            (:jump-forward
              (when (memory-current-cell-zero-p memory)
                (jump-to-opposite-boundary)))
            
            (:jump-back
              (unless (memory-current-cell-zero-p memory)
                (jump-to-opposite-boundary)))
            
            (:stack-nop
              NIL)
            
            (:push-cell-value-unto-stack
              (cell-push-value-unto-stack
                (memory-current-cell memory)))
            
            (:pop-to-cell-value
              (cell-pop-from-stack-to-value
                (memory-current-cell memory)))
            
            (:pop-to-next-stack
              (cell-push
                (memory-next-cell memory)
                (cell-pop
                  (memory-current-cell memory))))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                instruction ip)))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-O_o (code)
  "Interprets the piece of O_o source CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (decode-O_o code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of O_o encoder.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-p (command)
  "Checks whether the COMMAND represents a brainfuck instruction,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type command command))
  (the boolean
    (not (null
      (typep command 'brainfuck-command)))))

;;; -------------------------------------------------------

(defun stack-command-p (command)
  "Checks whether the COMMAND represents a stack instruction, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type command command))
  (the boolean
    (not (null
      (typep command 'stack-command)))))

;;; -------------------------------------------------------

(defun valid-instruction-triplet-p (instructions)
  "Checks whether the INSTRUCTIONS represent a valid triplet, compact of
   two brainfuck instructions in succession, with only the first
   homologated to bear the ``:nop'' sentinel, followed by a single stack
   ooperation, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type instruction-triplet instructions))
  (destructuring-bind (first-command second-command third-command)
      instructions
    (declare (type command first-command))
    (declare (type command second-command))
    (declare (type command third-command))
    (the boolean
      (not (null
        (and
          ;; The first instruction must be any brainfuck command.
          (brainfuck-command-p first-command)
          ;; The second instruction must be a brainfuck command, ...
          (brainfuck-command-p second-command)
          ;; ... but it may not be the no-operation instance.
          (not (eq second-command :nop))
          ;; The third instruction must be a stack operation.
          (stack-command-p     third-command)))))))

;;; -------------------------------------------------------

(defun validate-instruction-triplet (instructions)
  "Checks whether the INSTRUCTIONS represent a valid triplet, compact of
   two brainfuck instructions in succession, with only the first
   homologated to bear the ``:nop'' sentinel, followed by a single stack
   ooperation, returning on the triplet itself; otherwise signaling an
   error of an unspecified type."
  (declare (type instruction-triplet instructions))
  (unless (valid-instruction-triplet-p instructions)
    (error "Invalid triplet of instructions: ~s." instructions))
  (the instruction-triplet instructions))

;;; -------------------------------------------------------

(defun encode-brainfuck-command (brainfuck-command)
  "Returns for the BRAINFUCK-COMMAND the three-bit code, representing
   the ``:nop'' sentinel by the zero-bits triple 000; or signals an
   error of an unspecified type upon a disrespondency."
  (declare (type brainfuck-command brainfuck-command))
  (the tribit
    (case brainfuck-command
      (:move-right    #b000)
      (:move-left     #b001)
      (:increment     #b010)
      (:decrement     #b011)
      (:output        #b100)
      (:input         #b101)
      (:jump-forward  #b110)
      (:jump-back     #b111)
      (:nop           #b000)
      (otherwise
        (error "No recognized brainfuck command: ~s."
          brainfuck-command)))))

;;; -------------------------------------------------------

(defun encode-stack-command (stack-command)
  "Returns for the STACK-COMMAND the two-bit code, or signals an error
   of an unspecified type upon a disrespondency."
  (declare (type stack-command stack-command))
  (the dibit
    (case stack-command
      (:stack-nop                  #b00)
      (:push-cell-value-unto-stack #b01)
      (:pop-to-cell-value          #b10)
      (:pop-to-next-stack          #b11)
      (otherwise
        (error "No recognized stack command: ~s." stack-command)))))

;;; -------------------------------------------------------

(defun extract-number-of-eyes (line-format instructions)
  "Returns for the INSTRUCTIONS triplet, based upon the LINE-FORMAT, two
   values:
     (1) the number of big eyes on the left-hand side, greater than or
         equal zero (0), with the lower boundary representing a special
         case of the ``:nop'' brainfuck instruction
     (2) the number of small eyes on the right-hand side, greater than
         or equal to one (1)."
  (declare (type line-format         line-format))
  (declare (type instruction-triplet instructions))
  (destructuring-bind (first-brainfuck-command
                       second-brainfuck-command
                       stack-command)
      instructions
    (declare (type brainfuck-command first-brainfuck-command))
    (declare (type brainfuck-command second-brainfuck-command))
    (declare (type stack-command     stack-command))
    (declare (ignorable              first-brainfuck-command))
    (the (values tally tally)
      (case line-format
        (:bilateral
          (let ((line-code #b00000000))
            (declare (type octet line-code))
            (setf (ldb (byte 3 5) line-code)
                  (encode-brainfuck-command first-brainfuck-command))
            (setf (ldb (byte 3 2) line-code)
                  (encode-brainfuck-command second-brainfuck-command))
            (setf (ldb (byte 2 0) line-code)
                  (encode-stack-command stack-command))
            (values
              (1+ (ldb (byte 4 4) line-code))
              (1+ (ldb (byte 4 0) line-code)))))
        (:unilateral
          (let ((line-code #b00000))
            (declare (type pentad line-code))
            (setf (ldb (byte 3 2) line-code)
                  (encode-brainfuck-command second-brainfuck-command))
            (setf (ldb (byte 2 0) line-code)
                  (encode-stack-command stack-command))
            (values 0 (1+ line-code))))
        (otherwise
          (error "Invalid line format: ~s." line-format))))))

;;; -------------------------------------------------------

(defun write-character-sequence (character tally destination)
  "If the TALLY is greater than zero, writes the CHARACTER a TALLY of
   times to the DESTINATION, otherwise outputs a single zero digit
   character \"0\", in any case returning no value."
  (declare (type character   character))
  (declare (type tally       tally))
  (declare (type destination destination))
  (cond
    ((zerop tally)
      (format destination "0"))
    ((plusp tally)
      (loop repeat tally do
        (format destination "~c" character)))
    (T
      (error "Invalid tally: ~d." tally)))
  (values))

;;; -------------------------------------------------------

(defun determine-line-format (instructions)
  "Returns the line format for the specified INSTRUCTIONS triplet."
  (declare (type instruction-triplet instructions))
  (the line-format
    (case (first instructions)
      (:nop      :unilateral)
      (otherwise :bilateral))))

;;; -------------------------------------------------------

(defun write-O_o-line (instructions destination)
  "Writes an O_o-encoded string representation of the INSTRUCTIONS to
   the DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise responds with a fresh string containing the result."
  (declare (type instruction-triplet instructions))
  (declare (type destination         destination))
  (multiple-value-bind (left-side right-side)
      (extract-number-of-eyes
        (determine-line-format instructions)
        instructions)
    (declare (type tally left-side))
    (declare (type tally right-side))
    (write-character-sequence #\O left-side destination)
    (format destination "_")
    (write-character-sequence #\o right-side destination)))

;;; -------------------------------------------------------

(defun encode-O_o-program (instructions &key (destination NIL))
  "Encodes the O_o instructions into lines of the matching patterns,
   writing the same into the DESTINATION, returning for a non-``NIL''
   DESTINATION value ``NIL'', otherwise responds with a fresh stringing
   containing the output.
   ---
   The INSTRUCTIONS sequence must conform to a particular structure,
   with each three consecutive items assuming a coherent unit, the first
   element of which ought to be a brainfuck instruction, permitting the
   ``:nop'' sentinel, followed a second non-``:nop'' brainfuck
   instruction, and concluded with a stack operation. A corollary of
   this, any non-empty INSTRUCTIONS vector must exhibit a length that is
   a multiple of three. A failure to ascertain this forbisen will result
   in an error of an unspecified type."
  (declare (type o_o-program instructions))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((ip                     0)
            (number-of-instructions (length instructions)))
        (declare (type fixnum ip))
        (declare (type tally  number-of-instructions))
        (if (zerop (mod number-of-instructions 3))
          (labels
              ((has-more-instructions ()
                "Checks whether more instructions follow, starting at
                 the current instruction pointer IP, returning on
                 confirmation a ``boolean'' value of ``T'', otherwise
                 ``NIL''."
                (the boolean
                  (not (null
                    (< ip (1- number-of-instructions))))))
               
               (probe-next-instruction ()
                "Checks whether the instruction pointer IP resides at
                 an instruction, on confirmation returning the same,
                 while concomitantly advancing the instruction
                 pointer; otherwise returning the ``NIL'' value."
                (the (or null command)
                  (when (< ip (length instructions))
                    (prog1
                      (aref instructions ip)
                      (incf ip)))))
               
               (load-next-instruction ()
                "Checks whether the instruction pointer IP resides at
                 an instruction, on confirmation returning the same,
                 while concomitantly advancing the instruction
                 pointer; otherwise signaling an error of an
                 unspecified type."
                (the command
                  (or (probe-next-instruction)
                      (error "Missing instruction."))))
               
               (load-next-three-instructions ()
                "Returns the next three instructions from the
                 INSTRUCTIONS vector, or signals an error of an
                 unspecified if an insufficient number of items
                 remains."
                (the instruction-triplet
                  (list
                    (load-next-instruction)
                    (load-next-instruction)
                    (load-next-instruction)))))
            
            (loop
              while (has-more-instructions)
              for first-line-p
                of-type boolean
                =       T
                then    NIL
              do
                (unless first-line-p
                  (format destination "~%"))
                (write-O_o-line
                  (validate-instruction-triplet
                    (load-next-three-instructions))
                  destination)))
          
          (error "Invalid number of instructions: ~d. ~
                  Must be a multiple of tree."
            number-of-instructions)))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (encode-O_o-program instructions :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-O_o-converter.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for-brainfuck-token (brainfuck-token)
  "Returns the command answering to the BRAINFUCK-TOKEN, or the sentinel
   ``:nop'' upon a disrespondency."
  (declare (type character brainfuck-token))
  (the brainfuck-command
    (case brainfuck-token
      (#\>       :move-right)
      (#\<       :move-left)
      (#\+       :increment)
      (#\-       :decrement)
      (#\.       :output)
      (#\,       :input)
      (#\[       :jump-forward)
      (#\]       :jump-back)
      (otherwise :nop))))

;;; -------------------------------------------------------

(defun brainfuck-command-token-p (token)
  "Checks whether the TOKEN represents a brainfuck command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (find token "><+-.,[]" :test #'char=)))))

;;; -------------------------------------------------------

(defun find-next-brainfuck-command (brainfuck-code start)
  "Proceeding from the START position in the piece of BRAINFUCK-CODE,
   seeks the next brainfuck command token, on success returning two
   values:
     (1) the ``command'' associated with the detected token
     (2) the position immediately following this token.
   On failure, responds with the ``NIL'' value."
  (declare (type string brainfuck-code))
  (declare (type fixnum start))
  (let ((end (position-if
               #'brainfuck-command-token-p brainfuck-code
               :start start)))
    (declare (type (or null fixnum) end))
    (the (values (or null character) (or null fixnum))
      (when end
        (values
          (get-command-for-brainfuck-token
            (char brainfuck-code end))
          (1+ end))))))

;;; -------------------------------------------------------

(defun get-next-brainfuck-command-pair (brainfuck-code start)
  "Proceeding from the START position, seeks and returns the next two
   brainfuck commands in the BRAINFUCK-CODE, returning three values:
     (1) if at least one brainfuck command token could be found, the
         ``command'' object answering to the first detection, otherwise
         ``NIL''
     (2) if two brainfuck command tokens could be attested, the
         ``command'' object answering to the second detection, otherwise
         ``NIL''
     (3) if at least one brainfuck command token could be found, the
         position in the BRAINFUCK-CODE immediately succeeding the last
         of the two possible token detections, otherwise ``NIL''."
  (declare (type string brainfuck-code))
  (declare (type fixnum start))
  (when (< start (length brainfuck-code))
    (let ((first-command  NIL)
          (second-command NIL)
          (position       NIL))
      (declare (type (or null brainfuck-command) first-command))
      (declare (type (or null brainfuck-command) second-command))
      (declare (type (or null fixnum)            position))
      
      ;; Attempt to detect the first brainfuck command token.
      (multiple-value-setq (first-command position)
        (find-next-brainfuck-command brainfuck-code start))
      
      ;; If tenable, probe for a second brainfuck command token.
      (when position
        (multiple-value-setq (second-command position)
          (find-next-brainfuck-command brainfuck-code position)))
      
      (the (values (or null brainfuck-command)
                   (or null brainfuck-command)
                   (or null fixnum))
        (values first-command
                second-command
                position)))))

;;; -------------------------------------------------------

(defun convert-brainfuck-into-O_o-program (brainfuck-code)
  "Creates and returns for the BRAINFUCK-CODE a one-dimensional simple
   array of instructions equivalent to the input program in effect."
  (declare (type string brainfuck-code))
  (let ((instructions   NIL)
        (position       0)
        (first-command  NIL)
        (second-command NIL))
    (declare (type (list-of command)           instructions))
    (declare (type (or null fixnum)            position))
    (declare (type (or null brainfuck-command) first-command))
    (declare (type (or null brainfuck-command) second-command))
    (flet ((collect-instruction (instruction)
            "Prepends the INSTRUCTION to the INSTRUCTIONS list and
             returns no value."
            (declare (type command instruction))
            (push instruction instructions)
            (values)))
      
      (loop do
        (multiple-value-setq (first-command second-command position)
          (get-next-brainfuck-command-pair brainfuck-code position))
        
        (cond
          ;; Two brainfuck commands found?
          ((and first-command second-command)
            (collect-instruction first-command)
            (collect-instruction second-command)
            (collect-instruction :stack-nop))
          
          ;; Only a single brainfuck command found?
          ((and first-command (null second-command))
            (collect-instruction :nop)
            (collect-instruction first-command)
            (collect-instruction :stack-nop))
          
          ;; Only a single brainfuck command found?
          ;; This anomalous case should never occur, but will be handled
          ;; nonetheless equivalently to the previous one.
          ((and (null first-command) second-command)
            (collect-instruction :nop)
            (collect-instruction second-command)
            (collect-instruction :stack-nop))
          ;; No brainfuck commands found?
          (T
            (loop-finish)))))
    
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))

;;; -------------------------------------------------------

(defun interpret-brainfuck (brainfuck-code)
  "Encode the piece of BRAINFUCK-CODE, evaluates it, and returns no
   value."
  (declare (type string brainfuck-code))
  (process-instructions
    (convert-brainfuck-into-O_o-program brainfuck-code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on an input of the null
;; character (ASCII code of zero).
(interpret-O_o
  "
  OOOOOOOOOOOO_ooooooooo
  OOOOOOOOOO_ooooo
  0_ooooooooooooooooooooooooooooo
  ")

;;; -------------------------------------------------------

;; Print "Hello World!" using negative cell values.
(interpret-O_o
  "
  O_ooooooooo
  OOOOO_ooooooooo
  OOOOO_ooooooooo
  OOOOO_ooooooooo
  OOOOOO_ooooooooo
  OOOOOOO_ooooo
  OOOOO_ooooooooo
  OOOOO_ooooooooo
  OOOOO_ooooooooo
  OOOOO_ooooooooo
  OOOOO_o
  OOOOOOOOOOOOOOO_ooooo
  OOOOOOOOO_o
  O_ooooooooo
  O_ooooooooooooo
  OOOOOOOOOOOOO_ooooooooo
  OOOOOOOOOOOOOOO_ooooooooo
  OOOOO_o
  OOOOO_ooooooooo
  O_ooooooooo
  OOOOO_ooooooooo
  OOOOOOOOOOOOO_o
  OOOOOOOOOOOOO_ooooooooooooo
  O_ooooooooo
  OOOOO_ooooooooo
  OOO_ooooo
  OOOOO_ooooooooo
  OOOOO_o
  OOOOOOOOOOOOOOO_ooooo
  OOOO_ooooooooooooo
  O_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOOOO_o
  OOOOOOO_o
  OOOOO_ooooooooo
  OOOOOO_o
  OOOOOOOOO_ooooooooo
  OOOOO_ooooooooo
  OOOOOOOOO_o
  OOOOOOOO_o
  OOO_ooooo
  OOOOOO_ooooooooo
  OO_ooooooooo
  OOOOO_o
  OOOOOO_ooooooooooooo
  O_o
  OOOOOOOOOOOOOOO_ooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOOOO_o
  OO_o
  OOOOO_ooooooooo
  OOOOOO_o
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOO_ooooooooooooo
  OOOOOOOO_o
  O_ooooooooo
  OOOOOOOOO_o
  OOOOOO_o
  ")

;;; -------------------------------------------------------

;; Encode the repeating cat program's instructions and write the print
;; the resulting string to the standard output.
;; The result constitutes
;; 
;;   OOOOOOOOOOOO_ooooooooo
;;   OOOOOOOOOO_ooooo
;;   0_ooooooooooooooooooooooooooooo
(encode-O_o-program
  (coerce
    '(:input
      :jump-forward
      :stack-nop
      :output
      :input
      :stack-nop
      :nop
      :jump-back
      :stack-nop)
    'o_o-program)
  :destination T)

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program, which uses negative
;; cell values, into a sequence of O_o instructions and write their
;; encoded form into a new string.
(encode-O_o-program
  (convert-brainfuck-into-O_o-program
    ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->
     +++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."))

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program, which uses negative
;; cell values, into a vector of O_o instructions.
(convert-brainfuck-into-O_o-program
  ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->
   +++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+.")

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program, which uses negative
;; cell values, into O_o instructions and execute these.
(interpret-brainfuck
  ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->
   +++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+.")
