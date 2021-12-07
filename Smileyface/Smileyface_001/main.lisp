;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language ":)" --- nevened "Smileyface" in cases of the
;; original name's symbols being impediments to its illustration ---,
;; invented by the Esolang user "Martsadas".
;; 
;; Concept
;; =======
;; The :) ("Smileyface") programming language constitutes an esoteric
;; specimen, the programs of which operate on a tape-like memory,
;; responding to instructions solely composed of variants of smileys.
;; Numbers are expressed in the base-4 (quaternary) number system.
;; 
;; == SIX SMILEYS DESCRIBE A PROGRAM ==
;; The language's diorism resides in its syntax: All non-whitespace
;; tokens are defined in terms of smileys, a subspecies of emoticons,
;; and most frequently associated with the specimen ":)", eponymous with
;; the subject of our disquisition.
;; 
;; A program in :) establishes a composition of six different types of
;; smileys, also called "tokens" in the specification. In the following,
;; an official designation is established in order to distinguish these
;; entities. The similarity of their purpose and the inherently
;; subjective nature of pareidolia interlocked in the construe of such
;; subtlety encumbers the task of defining a sane terminology. The
;; agnomination and its justification are juxtaposed in the below table:
;; 
;;   Token | Name      | Description
;;   ------+-----------+-----------------------------------------------
;;    :)   | smiling   | A smiling face.
;;         |           | The mien, expressive of bliss, more prominently
;;         |           | than any other serves to display this mood,
;;         |           | exceeding mere happiness through extroversion.
;;   ..................................................................
;;    :P   | cheeky    | A facial expression with a drooping tongue.
;;         |           | The tongue's adduction intimidates a certain
;;         |           | mete of mischief or cavalier disposition,
;;         |           | which most frequently amounts to impudence.
;;   ..................................................................
;;    :]   | satisfied | A physiognomy of an angular smile.
;;         |           | The modest curvature administers to the
;;         |           | exposition more restraint than a smile, while
;;         |           | yet admitting satisfaction, happiness, or
;;         |           | relief. The modicum of introversion serves in
;;         |           | discriminating it from the common smiley ":)".
;;   ..................................................................
;;    :>   | delighted | An enrapt mien.
;;         |           | A conspicuous bashfulness enlivens the smile,
;;         |           | such that its joviality partakes of an
;;         |           | admixture with rapture, yielding delight.
;;   ..................................................................
;;    :D   | grinning  | A face bearing a rictus.
;;         |           | The distance of the lips can be construed
;;         |           | either as an open mouth in positivy or as a
;;         |           | grin.
;;   ..................................................................
;;    :O   | surprised | A countenance with open mouth.
;;         |           | The mouth's outline betokens an uneasy or
;;         |           | anxious state occupied mostly in surprise or
;;         |           | shock. A forced or involuntary air accompanies
;;         |           | the effigy as opposed to a debonair grin.
;; 
;; The admission, role and interpretation of any of these tokens depends
;; on the context, developing from this proposition the faculty to
;; represent instructions and their components, including numeric
;; arguments, their separators, and the command terminator, by a
;; combination of a very restricted symbol set, which in turn typifies
;; the language utilization thereof as its key to a multum in parvo.
;; 
;; == :) USES A QUATERNARY NUMBER SYSTEM ==
;; As opposed to most programming languages, :) represents its numerals
;; in the form of a quaternary number system, that is, one to the base
;; or radix four (4). A consectary of this, only the four digits 0, 1,
;; 2, and 3 are permissive as constituents of such objects. In the face
;; of the peculiarity by which all data is represented in this language
;; each such digit must perforce be expressed in a smiley. The following
;; associations hold:
;; 
;;   Token | Name      | Represented quaternary digit
;;   ------+-----------+------------------------------
;;    :)   | smiling   | 0
;;   .................................................
;;    :P   | cheeky    | 1
;;   .................................................
;;    :]   | satisfied | 2
;;   .................................................
;;    :D   | grinning  | 3
;; 
;; == DATA IS STORED IN AN INFINITE MEMORY ==
;; Very similar to the esoteric programming language "brainfuck", :)
;; subscribes in its data management to a tape-like memory of cells,
;; with a pointer memorizing the currently selected one. A differential
;; to brainfuck, the tape size is neither fixed, as in the basic
;; rendition of that language, nor bilaterally infinite in its extent
;; after the extended version; instead, its boundless extension proceeds
;; only along the positive dimension, with the minimum cell position
;; equaling one (1).
;; 
;; == TWO SMILEYS DESCRIBE AN INSTRUCTION ==
;; :) manifests its functionality exclusively in the form of
;; instructions, lacking variables, explicit iteration facilities, and
;; more luxurious amenities. An instruction is invoked by aid of two
;; smilies in a compound, the former of which selects the category, the
;; latter specifies the concrete command. This kenspeckle trait shall be
;; discussed in the section "Instructions", which see.
;; 
;; 
;; Architecture
;; ============
;; :) bases its architecture upon a memory of infinite expansion towards
;; the positive axis, and aided by a pointer marking the active cell.
;; 
;; == THE MEMORY: A TAPE OF INFINITE CELLS ==
;; The memory is defined as a tape composed of cells, enumerated by
;; non-negative integer subscripts or indices starting at one (1), that
;; is, occupying the interval [1, +infinity]. This data structure is
;; capable of random access with respect to a cell by avail of exactly
;; these indices, expected to answer to requests submitted by certain
;; instructions.
;; 
;; == A POINTER MAINTAINS THE ACTIVE CELL ==
;; A pointer exists in order to memorize the currently active cell,
;; initially resolving to that at the index 1. While the memory
;; enumerates its cells from 1 upward, the special index zero (0)
;; constitutes a valid subscript,  always corresponding to the selected
;; cell designated by this pointer. Ultimately, the range of permitted
;; indices into the memory amounts to [0, +infinity]. This cursor is
;; endowed with the capacity to navigate along the memory by aid of two
;; instructions, and, while retaining fidelity to the infinite extent
;; along the positive axis, its movement is, of course, impeded to the
;; left if it has been located at the first memory cell at the index 1.
;; 
;; Each cell holds a single integer number of arbitrary constitution,
;; initially set to zero.
;; 
;; 
;; Data Types
;; ==========
;; The language operates mainly on integer data, stored in an ordered
;; collection, resorting to characters merely for the sake of input and
;; output facilities.
;; 
;; == INTEGERS CONSTITUTE THE MAIN OPERATIVE DATA ==
;; :) relies on integer numbers of unbounded magnitude, maintained as
;; elements in the memory's cells, for the preponderance of its
;; faculties. The language employs the quaternary number system, which
;; means that it is base-4. While negative values are not encumbered
;; with intolerance, number literals, as vianded by instruction
;; arguments, always resolve to non-negative integers greater than or
;; equal to zero. However, the various commands enable the manipulation
;; of cells in a manner to yield arbitrary integer results.
;; 
;; == CHARACTERS OPERATE ON THE INTERFACES ==
;; While the internal data management and all involved operations
;; participate in the integer realm, input and output facilities proffer
;; in addition to the numeric type support for characters as objects in
;; the user-machine communication. Depending on the concrete
;; instruction, the user might enter a number or a character;
;; analogously, the display of a cell value might proceed verbatim by
;; outputting the numeric data, or by printing the associated ASCII
;; character for the integer code. In any case, regardless of input or
;; output, any character datum is converted into an integer object for
;; the sake of persistence. Characters thus constitute a mere
;; alternative in the perspective on the numbers in the storage.
;; 
;; == AN INFINITE MEMORY MAINTAINS THE DATA ==
;; As explicated in the "Concept" section, the complete program data is
;; maintained exclusively as integer numbers in a tape-like memory whose
;; cells start at the position one (1), but may extend infinitely toward
;; the positive axis. Any cell, extant or not, may be inquired for its
;; value by its unique position or index; cells not yet defined
;; explicitly respond by the default value of zero (0). The special
;; index zero (0) designates the active cell, the maintenance of which
;; is embraced as the bailiwick of the memory pointer.
;; 
;; 
;; Syntax
;; ======
;; :) programs are built from six variants of smileys, separated by
;; zero or more whitespaces, which includes the space and tab character,
;; but not a linebreak. Newlines are permissive only after a complete
;; instruction description or at the end of a comment.
;; 
;; == COMMENTS ==
;; Comments are introduced by a single semicolon (";") and extend to the
;; end of the line; they are terminated by the linebreak.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) diagram describes the
;; structure of a :) program:
;; 
;;   program     := { comment | instruction } ;
;;   comment     := ";" , { character } , linebreak ;
;;   instruction := smiley , smiley , [ number , { ":>" , number } ] , ":O" ;
;;   linebreak   := "\n" ;
;;   number      := digit , { digit } ;
;;   digit       := ":)" | ":P" | ":]" | ":D" ;
;;   smiley      := ":)" | ":P" | ":]" | ":>" | ":D" | ":O" ;
;; 
;; 
;; Instructions
;; ============
;; :) utilizes a peculiar mode of command statement, where the first
;; token selects one of five instruction categories, the second
;; designating the concrete instruction, followed by zero or more
;; arguments, and terminated by the end-of-line symbol ":O".
;; 
;; The language's parsimony in the circumference of its symbols or
;; tokens is alleviated by the context sensitivity of their construe.
;; Depending upon a token's position relative to its kins, its
;; interpretation partakes of a well-defined resolution.
;; 
;; == COMMANDS COME IN CATEGORIES ==
;; The invocation of instructions, the only means of operating code,
;; introduces a further peculiarization into the language in addition
;; to its token design: Each instruction is preceded by a symbol which
;; serves as the "instruction modification parameter" (IMP) --- a
;; a concept borrowed from the esoteric programming language
;; "Whitespaces". The command set is distinguished into five categories,
;; with the membership composed of the actual instructions, each itself
;; being described by another smiley, optionally followed by arguments.
;; The IMP avails in selecting the category, with the following
;; associations holding:
;; 
;;   IMP token | Name      | Instruction category
;;   ----------+-----------+-----------------------------------
;;    :)       | smiling   | I/O
;;   ..........................................................
;;    :P       | cheeky    | Cells
;;   ..........................................................
;;    :]       | satisfied | Arithmetic
;;   ..........................................................
;;    :>       | delighted | Repeat the last instruction's IMP
;;   ..........................................................
;;    :D       | grinning  | Flow control
;; 
;; == SELECT A CATEGORY, THEN CHOOSE AN INSTRUCTION ==
;; Having stated the intended category by the incipient token, the next
;; smiley determines the instruction to call. The jumelle of IMP and
;; instruction symbol establishes an unambiguous invocation. The number
;; of arguments, if any, depends upon the concrete instance. In the
;; table below, the possible IMP-token combinations and an apercu of
;; their effect are enumerated:
;; 
;;   IMP | Instr. token | Effect
;;   ----+--------------+----------------------------------------------
;;    :) | :)           | Print cell value as ASCII character.
;;   ..................................................................
;;    :) | :P           | Print cell value as number.
;;   ..................................................................
;;    :) | :]           | Store user input number.
;;   ..................................................................
;;    :) | :>           | Print constant as ASCII character.
;;   ..................................................................
;;    :) | :D           | Store user input character.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :P | :)           | Set cell value from argument.
;;   ..................................................................
;;    :P | :P           | Copy cell value from another cell.
;;   ..................................................................
;;    :P | :]           | Move cell pointer left.
;;   ..................................................................
;;    :P | :>           | Move cell pointer right.
;;   ..................................................................
;;    :P | :D           | Store cell pointer in cell.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :] | :)           | Add two cells and store in a third one.
;;   ..................................................................
;;    :] | :P           | Subtract two cells and store in a third one.
;;   ..................................................................
;;    :] | :]           | Multiply two cells and store in a third one.
;;   ..................................................................
;;    :] | :>           | Divide two cells and store in a third one.
;;   ..................................................................
;;    :] | :D           | Store modulus of two cell in a third one.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :> | :)           | Simulate previous IMP with ":)" instruction.
;;   ..................................................................
;;    :> | :P           | Simulate previous IMP with ":P" instruction.
;;   ..................................................................
;;    :> | :]           | Simulate previous IMP with ":]" instruction.
;;   ..................................................................
;;    :> | :>           | Simulate previous IMP with ":>" instruction.
;;   ..................................................................
;;    :> | :D           | Simulate previous IMP with ":D" instruction.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :D | :)           | Create a new label.
;;   ..................................................................
;;    :D | :P           | Unconditionally jump to a label.
;;   ..................................................................
;;    :D | :]           | Jump to a label if two cells are equal.
;;   ..................................................................
;;    :D | :>           | Jump to a label if one cell exceeds another.
;;   ..................................................................
;;    :D | :D           | Halt the program.
;; 
;; It is patent from the list that the instruction set, being a pairing
;; of two tokens, constitutes an exhaustive enumeration of the available
;; symbols as twains, accommodating no room for extensions without the
;; dation of new symbols in a future iteration of the language.
;; 
;; == ARGUMENTS ==
;; Some but not all instructions depend on a set of required arguments,
;; each such composed of one or more quaternary digits, represented in
;; the source code by the tokens ":)", ":P", "]", and ":D". If more than
;; one argument is necessitated, the ":>" token as as a sepiment. Any
;; instruction, including the parameterized ones, terminates with a
;; single ":O". The list below states the tokens' roles in an argument
;; list:
;; 
;;   Token | Name      | Role in instruction argument list
;;   ------+-----------+-----------------------------------
;;    :)   | smiling   | quaternary digit 0
;;    :P   | cheeky    | quaternary digit 1
;;    :]   | satisfied | quaternary digit 2
;;    :D   | grinning  | quaternary digit 3
;;    :>   | delighted | parameter separator
;;    :O   | surprised | end of line/statement
;; 
;; == OVERVIEW ==
;; The following tabular exposition lists the commands of :).
;; Utilized as placeholders, the arguments in the following descriptions
;; are enclosed in curly parenthesis, "{" and "}", ensconcing the
;; argument name.
;; 
;; In addition to the :) syntax and its description an interface
;; definition in the form of pseudocode is supplied in order to
;; facilitate their comprehension. The interface defines the following
;; two data types:
;; 
;;   index
;;     A non-negative integer number greater or equal to zero, intended
;;     to be employed as a cell index into the memory.
;;   
;;   unsignedInteger
;;     An non-negative integer number greater or equal to zero, stated
;;     without any particular purpose. In corollary, the object may
;;     be either an ASCII character code, a position in the source
;;     code, or an arbitrary other one.
;; 
;;   Instruction | Description
;;   ------------+-----------------------------------------------------
;;    :) :)      | Syntax:
;;               |   :) :) {cellIndex} :O
;;               | Description:
;;               |   Prints the value of the cell designated by the
;;               |   "cellIndex" as the associated ASCII character.
;;               | Interface:
;;               |   printCellAsCharacter (cellIndex : index)
;;   ..................................................................
;;    :) :P      | Syntax:
;;               |   :) :P {cellIndex} :O
;;               | Description:
;;               |   Prints the value of the cell designated by the
;;               |   "cellIndex" as a number.
;;               | Interface:
;;               |   printCellAsNumber (cellIndex : index)
;;   ..................................................................
;;    :) :]      | Syntax:
;;               |   :) :] {cellIndex} :O
;;               | Description:
;;               |   Prompts the user for a number and stores it in
;;               |   the cell at the "cellIndex".
;;               | Interface:
;;               |   inputNumber (cellIndex : index)
;;   ..................................................................
;;    :) :>      | Syntax:
;;               |   :) :> {asciiCode} :O
;;               | Description:
;;               |   Prints the ASCII character corresponding to the
;;               |   numeric "asciiCode".
;;               | Interface:
;;               |   printCharacter (asciiCode : unsignedInteger)
;;   ..................................................................
;;    :) :D      | Syntax:
;;               |   :) :D {cellIndex} :O
;;               | Description:
;;               |   Prompts the user for a character and stores its
;;               |   ASCII code in the cell at the "cellIndex".
;;               | Interface:
;;               |   inputCharacter (cellIndex : index)
;;   :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :P :)      | Syntax:
;;               |   :P :) {cellIndex} :> {newValue} :O
;;               | Description:
;;               |   Sets the value of the cell at the "cellIndex" to
;;               |   the "newValue".
;;               | Interface:
;;               |   setCellValue (cellIndex : index,
;;               |                 newValue  : unsignedInteger)
;;   ..................................................................
;;    :P :P      | Syntax:
;;               |   :P :P {destinationCellIndex} :> {sourceCellIndex} :O
;;               | Description:
;;               |   Copies the value of the cell at the
;;               |   "sourceCellIndex" into the cell at the
;;               |   "destinationCellIndex".
;;               | Interface:
;;               |   copyCellValue (destinationCellIndex : index,
;;               |                  sourceCellIndex      : index)
;;   ..................................................................
;;    :P :]      | Syntax:
;;               |   :P :] :O
;;               | Description:
;;               |   Moves the memory pointer one cell to the left.
;;               |   This instruction is ineffectuous if the pointer
;;               |   resides at the cell index one (1).
;;               | Interface:
;;               |   moveCellPointerLeft ()
;;   ..................................................................
;;    :P :>      | Syntax:
;;               |   :P :> :O
;;               | Description:
;;               |   Moves the memory pointer one cell to the right.
;;               | Interface:
;;               |   moveCellPointerRight ()
;;   ..................................................................
;;    :P :D      | Syntax:
;;               |   :P :D {cellIndex} :O
;;               | Description:
;;               |   Sets the value of the cell at the "cellIndex" to
;;               |   the memory pointer's position.
;;               | Interface:
;;               |   setCellValueToPointerPosition (cellIndex : index)
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :] :)      | Syntax:
;;               |   :] :) {augendIndex} :> {addendIndex} :> {resultIndex} :O
;;               | Description:
;;               |   Adds the value of the cell at the "addendIndex" to
;;               |   that at the "augendIndex" and stores the sum in
;;               |   the cell at the "resultIndex".
;;               | Interface:
;;               |   addCells (augendIndex : index,
;;               |             addendIndex : index,
;;               |             resultIndex : index)
;;   ..................................................................
;;    :] :P      | Syntax:
;;               |   :] :P {minuendIndex} :> {subtrahendIndex} :> {resultIndex} :O
;;               | Description:
;;               |   Subtracts the value of the cell at the
;;               |   "subtrahendIndex" from that at the "minuendIndex"
;;               |   and stores the difference in the cell at the
;;               |   "resultIndex".
;;               | Interface:
;;               |   subtractCells (minuendIndex    : index,
;;               |                  subtrahendIndex : index,
;;               |                  resultIndex     : index)
;;   ..................................................................
;;    :] :]      | Syntax:
;;               |   :] :] {multiplierIndex} :> {multiplicandIndex} :> {resultIndex} :O
;;               | Description:
;;               |   Multiplies the value of the cell at the
;;               |   "multiplierIndex" by that at the
;;               |   "multiplicandIndex" and stores the product in the
;;               |   cell at the "resultIndex".
;;               | Interface:
;;               |   multiplyCells (multiplierIndex   : index
;;               |                  multiplicandIndex : index,
;;               |                  resultIndex       : index)
;;   ..................................................................
;;    :] :>      | Syntax:
;;               |   :] :> {dividendIndex} :> {divisorIndex} :> {resultIndex} :O
;;               | Description:
;;               |   Divides the value of the cell at the
;;               |   "dividendIndex" by that at the "divisorIndex" and
;;               |   stores the quotient in the cell at the
;;               |   "resultIndex".
;;               | Interface:
;;               |   divideCells (dividendIndex : index,
;;               |                divisorIndex  : index,
;;               |                resultIndex   : index)
;;   ..................................................................
;;    :] :D      | Syntax:
;;               |   :] :D {dividendIndex} :> {divisorIndex} :> {resultIndex} :O
;;               | Description:
;;               |   Divides the value of the cell at the
;;               |   "dividendIndex" by that at the "divisorIndex" and
;;               |   stores the rest in the cell at the "resultIndex".
;;               | Interface:
;;               |   modulusOfCells (dividendIndex : index,
;;               |                   divisorIndex  : index,
;;               |                   resultIndex   : index)
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :> :)      | Syntax:
;;               |   :> :) {argumentList...} :O
;;               | Description:
;;               |   Invokes the instruction which is associated with
;;               |   the last IMP and the instruction name ":)", using
;;               |   the "argumentList" appropriate for the represented
;;               |   instruction.
;;               | Interface:
;;               |   repeatImp (":)", argumentList : unsignedInteger[0..*])
;;   ..................................................................
;;    :> :P      | Syntax:
;;               |   :> :P {argumentList...} :O
;;               | Description:
;;               |   Invokes the instruction which is associated with
;;               |   the last IMP and the instruction name ":P", using
;;               |   the "argumentList" appropriate for the represented
;;               |   instruction.
;;               | Interface:
;;               |   repeatImp (":P", argumentList : unsignedInteger[0..*])
;;   ..................................................................
;;    :> :]      | Syntax:
;;               |   :> :] {argumentList...} :O
;;               | Description:
;;               |   Invokes the instruction which is associated with
;;               |   the last IMP and the instruction name ":]", using
;;               |   the "argumentList" appropriate for the represented
;;               |   instruction.
;;               | Interface:
;;               |   repeatImp (":]", argumentList : unsignedInteger[0..*])
;;   ..................................................................
;;    :> :>      | Syntax:
;;               |   :> :> {argumentList...} :O
;;               | Description:
;;               |   Invokes the instruction which is associated with
;;               |   the last IMP and the instruction name ":>", using
;;               |   the "argumentList" appropriate for the represented
;;               |   instruction.
;;               | Interface:
;;               |   repeatImp (":>", argumentList : unsignedInteger[0..*])
;;   ..................................................................
;;    :> :D      | Syntax:
;;               |   :> :D {argumentList...} :O
;;               | Description:
;;               |   Invokes the instruction which is associated with
;;               |   the last IMP and the instruction name ":D", using
;;               |   the "argumentList" appropriate for the represented
;;               |   instruction.
;;               | Interface:
;;               |   repeatImp (":D", argumentList : unsignedInteger[0..*])
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    :D :)      | Syntax:
;;               |   :D :) {labelID} :O
;;               | Description:
;;               |   Creates a label which associates the "labelID"
;;               |   with the position in the source code immediately
;;               |   following this instruction.
;;               | Interface:
;;               |   createLabel (labelID : unsignedInteger)
;;   ..................................................................
;;    :D :P      | Syntax:
;;               |   :D :P {labelID} :O
;;               | Description:
;;               |   Jumps to the position in the source code
;;               |   associated with the "labelID".
;;               | Interface:
;;               |   jumpToLabel (labelID : unsignedInteger)
;;   ..................................................................
;;    :D :]      | Syntax:
;;               |   :D :] {labelID} :> {leftCellIndex} :> {rightCellIndex} :O
;;               | Description:
;;               |   Jumps to the position in the source code
;;               |   associated with the "labelID" if the value of the
;;               |   cell at the "leftCellIndex" is equal to that at
;;               |   the "rightCellIndex".
;;               | Interface:
;;               |   jumpToLabelIfEqual (labelID        : unsignedInteger,
;;               |                       leftCellIndex  : index,
;;               |                       rightCellIndex : index)
;;   ..................................................................
;;    :D :>      | Syntax:
;;               |   :D :> {labelID} :> {leftCellIndex} :> {rightCellIndex} :O
;;               | Description:
;;               |   Jumps to the position in the source code
;;               |   associated with the "labelID" if the value of the
;;               |   cell at the "leftCellIndex" is greater than that
;;               |   at the "rightCellIndex".
;;               | Interface:
;;               |   jumpToLabelIfGreaterThan (labelID        : unsignedInteger,
;;               |                             leftCellIndex  : index,
;;               |                             rightCellIndex : index)
;;   ..................................................................
;;    :D :D      | Syntax:
;;               |   :D :D :O
;;               | Description:
;;               |   Terminates the program.
;;               | Interface:
;;               |   haltProgram ()
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A very well composed writ, the :) specification imposes merely a
;; scintilla of apertures. A list of the same shall be adduced.
;; 
;; == DO END OF LINE CHARACTERS TERMINATE COMMANDS? ==
;; The dedicated token ":O" embraces as its exclusive purpose the
;; proclamation of the end of a line. The definition, however, leaves
;; the utilization's details nebulous. In particular one inquisition
;; interferes: Does this end-of-line token ":O" serve as a simple
;; statement separator? All examples produce a linebreak immediately
;; following the symbol, albeit no ambiguity issues from appending
;; further statements to the same line. This specification thus chooses
;; to not enforce a new line as a requisite to the ":O" token,
;; homologating several instructions to share one horizontal extent.
;; 
;; 
;; Implementation
;; ==============
;; Simplicity embues this interpreter, with few indagations into and
;; safeguards for sanity.
;; 
;; == THE MEMORY IS IMPLEMENTED BY A HASH TABLE ==
;; The infinitely expanding memory with its amenability to random access
;; by numeric indices manifests as a hash table, the entries of which
;; represent the cell information. This involves each key designating
;; an index in the form of a non-negative integer number, associated
;; with an arbitrary integer cell value. The concept of infinity amounts
;; to responding to an index lacking existence in the table by the
;; default value of zero --- without the creation of an entry. A
;; key-value pair is only created if an operation explicity requires
;; such. Migrations of the pointer along undeveloped space, also, do not
;; establish new entries. The pointer itself resolves to a plain
;; variable of the non-negative integer type.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-04
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Smileyface"
;;   -> "https://en.wikipedia.org/wiki/Emoticon"
;;       o Describes emoticons and their history.
;;   -> "https://en.wikipedia.org/wiki/List_of_emoticons"
;;       o Offers a list of emoticons.
;;   -> "https://theasciicode.com.ar/"
;;       o Displays the ASCII character codes.
;;   -> "https://www.mathsisfun.com/definitions/quaternary.html"
;;       o Describes the quaternary number system.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype smiley ()
  '(member
    :smiling      ;; :)
    :cheeky       ;; :P
    :satisfied    ;; :]
    :delighted    ;; :>
    :grinning     ;; :D
    :surprised))  ;; :O

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type provides a definition of valid indices for
   cells in the memory, which range in [0, +infinity].
   ---
   The cells in the memory data structure are enumerated from one (1)
   upwards, with zero (0) established as a sentinel signifying the cell
   currently selected by the memory pointer."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype quaternary-digit ()
  "The type ``quaternary-digit'' describes a base-4 compatible digit,
   that is, an integer number in the range [0, 3]."
  '(integer 0 3))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type ))
  "The ``hash-table-of'' type defines a hash table whose keys all
   conform to the KEY-TYPE and whose values to the VALUE-TYPE, both of
   which default to the comprehensive ``T'' type specifier."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Smileyface (code)
  "Interprets the piece of :) CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (token     NIL))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type (or null smiley)    token))
      
      (let ((memory  (make-hash-table :test #'eql))
            (pointer 1)
            ;; Instruction modification parameter (IMP).
            (imp     NIL)
            (labels  (make-hash-table :test #'eql)))
        (declare (type (hash-table-of cell-index integer)   memory))
        (declare (type (integer 1 *)                        pointer))
        (declare (type (hash-table-of (integer 0 *) fixnum) labels))
        
        (labels
            ((advance ()
              "Moves the POSITION to the next character in the CODE, if
               possible, updating the current CHARACTER in the process,
               and returning no value."
              (if (< position (1- (length code)))
                (setf character (char code (incf position)))
                (setf character NIL))
              (values))
             
             (read-token ()
              "Starting at the current POSITION, reads a token, stores it
               in the TOKEN, and returns no value."
              (advance)
              (setf token
                (case character
                  (#\) :smiling)
                  (#\P :cheeky)
                  (#\] :satisfied)
                  (#\> :delighted)
                  (#\D :grinning)
                  (#\O :surprised)
                  (otherwise (error "Invalid token: :~c." character))))
              (advance)
              (values))
             
             (whitespace-character-p (candidate)
              "Checks whether the CANDIDATE is a whitespace character,
               returning a generalized Boolean which is true if the
               predicate can be confirmed, otherwise ``NIL''."
              (declare (type character candidate))
              (member candidate '(#\Space #\Tab) :test #'char=))
             
             (skip-whitespaces ()
              "Starting at the current POSITION, skips zero or more
               whitespace, relocates the POSITION to the first
               non-whitespace character in the CODE, and returns no value."
              (loop
                while (and character (whitespace-character-p character))
                do    (advance))
              (values))
             
             (skip-comment ()
              "Starting at the current POSITION, skips a comment line,
               sets the POSITION to the start of the next line, and
               returns no value."
              (loop
                while (and character (not (char= character #\Newline)))
                do    (advance))
              (advance)
              (values))
             
             (get-next-token ()
              "Starting at the current POSITION, searches for the next
               token, consuming, storing and returning it if found,
               otherwise signaling an error.
               ---
               This function, relying on ``read-token'', exhibits the
               following diorisms:
                 (1) It seeks the token itself, instead of expecting to
                     be positioned at its start, thus being of good
                     service for repeated inquiry for subsequent tokens.
                 (2) It signals an error if no token could be found.
                     ``read-token'', if handled correctly, always consumes
                     a smiley-like token, throwing an error only in the
                     case of an invalid design in its retrieved content.
                 (3) It returns the consumed and updated token, rendering
                     it suitable for use in binding expressions, in
                     particular variable assignments."
              (loop do
                (cond
                  ((null character)
                    (setf token NIL)
                    (loop-finish))
                  ((whitespace-character-p character)
                    (skip-whitespaces))
                  ((char= character #\:)
                    (read-token)
                    (loop-finish))
                  (T
                    (error "Cannot read next token."))))
              (the (or null smiley) token))
             
             (get-token-value (token)
              "Returns the base-4 numeric value of the TOKEN as a
               ``quaternary-digit'', that is, an integer in [0, 3]."
              (the quaternary-digit
                (case token
                  (:smiling  0)
                  (:cheeky   1)
                  (:satisfied    2)
                  (:grinning 3)
                  (otherwise (error "Cannot get the value of the token ~s." token)))))
             
             (read-integer ()
              "Consumes one or more tokens representing quaternary digits,
               parses these as a single number, and returns their decimal
               positive integer equivalent."
              (the (integer 0 *)
                (parse-integer
                  (with-output-to-string (digits)
                    (declare (type string-stream digits))
                    (loop
                      until
                        (member (get-next-token)
                          '(:surprised :delighted) :test #'eq)
                      do
                        (write-char (digit-char (get-token-value token)) digits)))
                  :radix 4)))
             
             (read-argument ()
              "Starting at the current POSITION, reads an argument as a
               sequence of one or more tokens, terminating when
               encountering a parameter separator (':>') or instruction
               terminator (':O'), and returning a non-negative integer
               number."
              (let ((argument 0))
                (declare (type (integer 0 *) argument))
                (loop do
                  (cond
                    ((null character)
                      (loop-finish))
                    ((whitespace-character-p character)
                      (skip-whitespaces))
                    ((char= character #\:)
                      (setf argument (read-integer))
                      (loop-finish))
                    (T
                      (error "Cannot read next token."))))
                (the (integer 0 *) argument)))
             
             
             (cell-at (index)
              "Returns the value of the cell designated by the INDEX.
               ---
               An INDEX of zero signifies the cell at the memory POINTER."
              (declare (type cell-index index))
              (the integer
                (if (zerop index)
                  (gethash pointer memory 0)
                  (gethash index   memory 0))))
             
             ((setf cell-at) (new-value index)
              "Sets the value of the cell designated by the INDEX to the
               NEW-VALUE and returns no value.
               ---
               An INDEX of zero signifies the cell at the memory POINTER."
              (declare (type integer    new-value))
              (declare (type cell-index index))
              (if (zerop index)
                (setf (gethash pointer memory 0) new-value)
                (setf (gethash index   memory 0) new-value))
              (values))
             
             (move-pointer-left ()
              "Moves the memory POINTER one cell to the left and returns
               no value.
               ---
               This function silently fails if the POINTER is located at
               the minimum index one (1)."
              (when (> pointer 1)
                (decf pointer))
              (values))
             
             (move-pointer-right ()
              "Moves the memory POINTER one cell to the right and returns
               no value."
              (incf pointer)
              (values))
             
             
             (create-label (label-name)
              "Creates a new label with associated the LABEL-NAME with
               the current POSITION in the CODE and returns no value.
               ---
               If the LABEL-NAME is already in use, it is overwritten."
              (declare (type (integer 0 *) label-name))
              (setf (gethash label-name labels) position)
              (values))
             
             (go-to-label (label-identifier)
              "Moves the POSITION cursor to the position in the CODE of
               the label associated by the LABEL-IDENTIFIER and returns
               no value.
               ---
               An error occurs if the LABEL-IDENTIFIER cannot be
               retrieved."
              (declare (type (integer 0 *) label-identifier))
              (multiple-value-bind (label-position contains-identifier)
                  (gethash label-identifier labels)
                (declare (type (or null fixnum) label-position))
                (declare (type T                contains-identifier))
                (unless contains-identifier
                  (error "No label with identifier ~s detected." label-identifier))
                (setf position  label-position)
                (setf character (char code position)))))
          
          (loop do
            (cond
              ((null character)
                (loop-finish))
              
              ((char= character #\;)
                (skip-comment))
              
              ((whitespace-character-p character)
                (skip-whitespaces))
              
              ((char= character #\:)
                (read-token)
                
                ;; Set the instruction modification parameter (IMP).
                (case token
                  ((NIL)
                    (error "No instruction modification parameter found."))
                  (:delighted
                    (unless imp
                      (error "Cannot assume the previous instruction ~
                              modification parameter, as no such exists.")))
                  (T
                    (setf imp token)))
                
                (case imp
                  ;; :) ...
                  (:smiling
                    (let ((instruction (get-next-token)))
                      (declare (type smiley instruction))
                      
                      (case instruction
                        
                        ;; :) :) {cellIndex} :O
                        ;; printCellAsCharacter (cellIndex : quaternaryDigit[1..*])
                        (:smiling
                          (let ((cell-index (read-argument)))
                            (declare (type cell-index cell-index))
                            (write-char (code-char (cell-at cell-index)))))
                        
                        ;; :) :P {cellIndex} :O
                        ;; printCellAsNumber (cellIndex : quaternaryDigit[1..*])
                        (:cheeky
                          (let ((cell-index (read-argument)))
                            (declare (type cell-index cell-index))
                            (format T "~d" (cell-at cell-index))))
                        
                        ;; :) :] {cellIndex} :O
                        ;; inputNumber (cellIndex : quaternaryDigit[1..*])
                        (:satisfied
                          (format T "~&Please input a number: ")
                          (let ((user-input (read)))
                            (declare (type integer user-input))
                            (clear-input)
                            (let ((cell-index (read-argument)))
                              (declare (type cell-index cell-index))
                              (setf (cell-at cell-index) user-input))))
                        
                        ;; :) :> {asciiCode} :O
                        ;; printCharacter (asciiCode : quaternaryDigit[1..*])
                        (:delighted
                          (write-char (code-char (read-argument))))
                        
                        ;; :) :D {cellIndex} :O
                        ;; inputCharacter (cellIndex : quaternaryDigit[1..*])
                        (:grinning
                          (let ((cell-index (read-argument)))
                            (declare (type cell-index cell-index))
                            (format T "~&Please input a character: ")
                            (let ((user-input (read-char)))
                              (declare (type character user-input))
                              (clear-input)
                              (setf (cell-at cell-index)
                                    (char-code user-input)))))
                        
                        (otherwise
                          (error "Invalid instruction ~a while in IMP ~a."
                            instruction imp)))))
                  
                  ;; :P
                  (:cheeky
                    (let ((instruction (get-next-token)))
                      (declare (type smiley instruction))
                      
                      (case instruction
                        
                        ;; :P :) {cellIndex} :> {newValue} :O
                        ;; setCellValue (cellIndex : quaternaryDigit[1..*], newValue : quaternaryDigit[1..*])
                        (:smiling
                          (let ((cell-index (read-argument))
                                (new-value  (read-argument)))
                            (declare (type cell-index    cell-index))
                            (declare (type (integer 0 *) new-value))
                            (setf (cell-at cell-index) new-value)))
                        
                        ;; :P :P {destinationCellIndex} :> {sourceCellIndex} :O
                        ;; copyCellValue (destinationCellIndex : quaternaryDigit[1..*],
                        ;;                sourceCellIndex      : quaternaryDigit[1..*])
                        (:cheeky
                          (let ((destination-cell-index (read-argument))
                                (source-cell-index      (read-argument)))
                            (declare (type cell-index destination-cell-index))
                            (declare (type cell-index source-cell-index))
                            (setf (cell-at destination-cell-index)
                                  (cell-at source-cell-index))))
                        
                        ;; :P :] :O
                        ;; moveCellPointerLeft ()
                        (:satisfied
                          (move-pointer-left))
                        
                        ;; :P :> :O
                        ;; moveCellPointerRight ()
                        (:delighted
                          (move-pointer-right))
                        
                        ;; :P :D {cellIndex} :O
                        ;; setCellValueToPointerPosition (cellIndex : quaternaryDigit[1..*])
                        (:grinning
                          (let ((cell-index (read-argument)))
                            (declare (type cell-index cell-index))
                            (setf (cell-at cell-index) pointer)))
                        
                        (otherwise
                          (error "Invalid instruction ~a while in IMP ~a."
                            instruction imp)))))
                  
                  ;; :]
                  (:satisfied
                    (let ((instruction (get-next-token)))
                      (declare (type smiley instruction))
                      
                      (case instruction
                        
                        ;; :] :) {augendCellIndex} :> {addendCellIndex} :> {resultCellIndex} :O
                        ;; addCells (augendCellIndex : quaternaryDigit[1..*],
                        ;;           addendCellIndex : quaternaryDigit[1..*],
                        ;;           resultCellIndex : quaternaryDigit[1..*])
                        (:smiling
                          (let ((augend-cell-index (read-argument))
                                (addend-cell-index (read-argument))
                                (result-cell-index (read-argument)))
                            (declare (type cell-index augend-cell-index))
                            (declare (type cell-index addend-cell-index))
                            (declare (type cell-index result-cell-index))
                            (setf (cell-at result-cell-index)
                                  (+ (cell-at augend-cell-index)
                                     (cell-at addend-cell-index)))))
                        
                        ;; :] :P {minuendCellIndex} :> {subtrahendCellIndex} :> {resultCellIndex} :O
                        ;; subtractCells (minuendCellIndex    : quaternaryDigit[1..*]
                        ;;               subtrahendCellIndex : quaternaryDigit[1..*]
                        ;;               resultCellIndex     : quaternaryDigit[1..*])
                        (:cheeky
                          (let ((minuend-cell-index    (read-argument))
                                (subtrahend-cell-index (read-argument))
                                (result-cell-index     (read-argument)))
                            (declare (type cell-index minuend-cell-index))
                            (declare (type cell-index subtrahend-cell-index))
                            (declare (type cell-index result-cell-index))
                            (setf (cell-at result-cell-index)
                                  (- (cell-at minuend-cell-index)
                                     (cell-at subtrahend-cell-index)))))
                        
                        ;; :] :] {multiplierCellIndex} :> {multiplicandCellIndex} :> {resultCellIndex} :O
                        ;; multiplyCells (multiplierCellIndex   : quaternaryDigit[1..*],
                        ;;                multiplicandCellIndex : quaternaryDigit[1..*],
                        ;;                resultCellIndex       : quaternaryDigit[1..*])
                        (:satisfied
                          (let ((multiplier-cell-index   (read-argument))
                                (multiplicand-cell-index (read-argument))
                                (result-cell-index       (read-argument)))
                            (declare (type cell-index multiplier-cell-index))
                            (declare (type cell-index multiplicand-cell-index))
                            (declare (type cell-index result-cell-index))
                            (setf (cell-at result-cell-index)
                                  (* (cell-at multiplier-cell-index)
                                     (cell-at multiplicand-cell-index)))))
                        
                        ;; :] :> {dividendCellIndex} :> {divisorCellIndex} :> {resultCellIndex} :O
                        ;; divideCells (dividendCellIndex : quaternaryDigit[1..*],
                        ;;              divisorCellIndex  : quaternaryDigit[1..*],
                        ;;              resultCellIndex   : quaternaryDigit[1..*])
                        (:delighted
                          (let ((dividend-cell-index (read-argument))
                                (divisor-cell-index  (read-argument))
                                (result-cell-index   (read-argument)))
                            (declare (type cell-index dividend-cell-index))
                            (declare (type cell-index divisor-cell-index))
                            (declare (type cell-index result-cell-index))
                            (setf (cell-at result-cell-index)
                                  (round (cell-at dividend-cell-index)
                                         (cell-at divisor-cell-index)))))
                        
                        ;; :] :D {dividendCellIndex} :> {divisorCellIndex} :> {resultCellIndex} :O
                        ;; modulusOfCells (dividendCellIndex : quaternaryDigit[1..*],
                        ;;                 divisorCellIndex  : quaternaryDigit[1..*],
                        ;;                 resultCellIndex   : quaternaryDigit[1..*])
                        (:grinning
                          (let ((dividend-cell-index (read-argument))
                                (divisor-cell-index  (read-argument))
                                (result-cell-index   (read-argument)))
                            (declare (type cell-index dividend-cell-index))
                            (declare (type cell-index divisor-cell-index))
                            (declare (type cell-index result-cell-index))
                            (setf (cell-at result-cell-index)
                                  (mod (cell-at dividend-cell-index)
                                       (cell-at divisor-cell-index)))))
                        
                        (otherwise
                          (error "Invalid instruction ~a while in IMP ~a."
                            instruction imp)))))
                  
                  ;; :D
                  (:grinning
                    (let ((instruction (get-next-token)))
                      (declare (type smiley instruction))
                      (case instruction
                        ;; :D :) {labelIdentifier} :O
                        ;; createLabel (labelIdentifier : quaternaryDigit[1..*])
                        (:smiling
                          (create-label (read-argument)))
                        
                        ;; :D :P {labelIdentifier} :O
                        ;; jumpToLabel (labelIdentifier : quaternaryDigit[1..*])
                        (:cheeky
                          (let ((label-identifier (read-argument)))
                            (declare (type (integer 0 *) label-identifier))
                            (go-to-label label-identifier)))
                        
                        ;; :D :] {labelIdentifier} :> {leftCellIndex} :> {rightCellIndex} :O
                        ;; jumpToLabelIfEqual (labelIdentifier : quaternaryDigit[1..*],
                        ;;                     leftCellIndex   : quaternaryDigit[1..*],
                        ;;                     rightCellIndex  : quaternaryDigit[1..*])
                        (:satisfied
                          (let ((label-identifier (read-argument))
                                (left-cell-index  (read-argument))
                                (right-cell-index (read-argument)))
                            (declare (type (integer 0 *) label-identifier))
                            (declare (type cell-index    left-cell-index))
                            (declare (type cell-index    right-cell-index))
                            (when (= (cell-at left-cell-index)
                                     (cell-at right-cell-index))
                              (go-to-label label-identifier))))
                        
                        ;; :D :> {labelIdentifier} :> {leftCellIndex} :> {rightCellIndex} :O
                        ;; jumpToLabelIfGreaterThan (labelIdentifier : quaternaryDigit[1..*],
                        ;;                           leftCellIndex   : quaternaryDigit[1..*],
                        ;;                           rightCellIndex  : quaternaryDigit[1..*])
                        (:delighted
                          (let ((label-identifier (read-argument))
                                (left-cell-index  (read-argument))
                                (right-cell-index (read-argument)))
                            (declare (type (integer 0 *) label-identifier))
                            (declare (type cell-index    left-cell-index))
                            (declare (type cell-index    right-cell-index))
                            (when (> (cell-at left-cell-index)
                                     (cell-at right-cell-index))
                              (go-to-label label-identifier))))
                        
                        ;; :D :D :O
                        ;; haltProgram ()
                        (:grinning
                          (loop-finish))
                        
                        (otherwise
                          (error "Invalid instruction ~a while in IMP ~a."
                            instruction imp)))))))
              
              ((char= character #\Newline)
                (advance))
              
              (T
                (error "Invalid character ~s at position ~d."
                  character position))))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!"
(interpret-Smileyface "
:) :> :P :) :] :) :O ; H
:) :> :P :] :P :P :O ; e
:) :> :P :] :D :) :O ; l
:) :> :P :] :D :) :O ; l
:) :> :P :] :D :D :O ; o
:) :> :] :D :) :O    ; ,
:) :> :] :) :) :O    ; [space]
:) :> :P :P :P :D :O ; W
:) :> :P :] :D :D :O ; o
:) :> :P :D :) :] :O ; r
:) :> :P :] :D :) :O ; l
:) :> :P :] :P :) :O ; d
:) :> :] :) :P :O    ; !
")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Smileyface
  ":) :] :D :O             ; Get input
   :P :) :P :> :P :O       ; Set another cell to 1
  
   :D :) :] :O             ; Make a new label
   :) :P :D :O             ; Print the input
   :D :] :] :> :D :> :P :O ; Jump to the label if the input is equal to 1
   :D :D :O                ; Halt the program")

;;; -------------------------------------------------------

;; 99 bottles of beer.
(interpret-Smileyface
  "
  ; Set cell at index 1 to 99.
  :P :) :P :> :P :] :) :D :O
  ; Set cell at index 2 to 1.
  :P :) :] :> :P :O

  ; Create a label with the ID 2.
  :D :) :] :O

  ; Print the value of the cell at index 1 as a number.
  :) :P :P :O

  ; Print ' bottles of beer on the wall'
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :D :) :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :D :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :P :] :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :P :P :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :] :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :D :] :O
  :) :> :] :) :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :] :) :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :P :D :P :D :O
  :) :> :P :] :) :P :O
  :) :> :P :] :D :) :O
  :) :> :P :] :D :) :O
  :) :> :] :] :O

  ; Print the value of the cell at index 1 as a number.
  :) :P :P :O

  ; Print ' bottles of beer'
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :D :) :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :D :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :P :] :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :P :P :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :] :O
  :) :> :] :] :O

  ; Print 'take 1 down'
  :) :> :P :D :P :) :O
  :) :> :P :] :) :P :O
  :) :> :P :] :] :D :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :D :) :P :O
  :) :> :] :) :) :O
  :) :> :P :] :P :) :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :D :O
  :) :> :P :] :D :] :O
  :) :> :] :] :O

  ; Print 'pass it around'
  :) :> :P :D :) :) :O
  :) :> :P :] :) :P :O
  :) :> :P :D :) :D :O
  :) :> :P :D :) :D :O
  :) :> :] :) :) :O
  :) :> :P :] :] :P :O
  :) :> :P :D :P :) :O
  :) :> :] :) :) :O
  :) :> :P :] :) :P :O
  :) :> :P :D :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :P :O
  :) :> :P :] :D :] :O
  :) :> :P :] :P :) :O
  :) :> :] :] :O
  :) :> :] :] :O

  ; Decrement counter cell[1] by subtracting cell[1] by cell[2] (= 1) and
  ; storing the result back in cell[1], that is:
  ;      cell[1] = cell[1] - cell[2]
  ;   => cell[1] = cell[1] - 1
  :] :P :P :> :] :> :P :O

  ; Jump to label 2 if cell[1] > cell[2].
  :D :> :] :> :P :> :] :O

  ; Print '1 bottle of beer on the wall
  ;        1 bottle of beer
  ;        take 1 down
  ;        pass it around'

  :) :> :D :) :P :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :D :) :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :P :] :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :P :P :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :] :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :D :] :O
  :) :> :] :) :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :] :) :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :P :D :P :D :O
  :) :> :P :] :) :P :O
  :) :> :P :] :D :) :O
  :) :> :P :] :D :) :O
  :) :> :] :] :O
  :) :> :D :) :P :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :D :) :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :P :] :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :P :P :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :] :O
  :) :> :] :] :O
  :) :> :P :D :P :) :O
  :) :> :P :] :) :P :O
  :) :> :P :] :] :D :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :D :) :P :O
  :) :> :] :) :) :O
  :) :> :P :] :P :) :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :D :O
  :) :> :P :] :D :] :O
  :) :> :] :] :O
  :) :> :P :D :) :) :O
  :) :> :P :] :) :P :O
  :) :> :P :D :) :D :O
  :) :> :P :D :) :D :O
  :) :> :] :) :) :O
  :) :> :P :] :] :P :O
  :) :> :P :D :P :) :O
  :) :> :] :) :) :O
  :) :> :P :] :) :P :O
  :) :> :P :D :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :P :O
  :) :> :P :] :D :] :O
  :) :> :P :] :P :) :O
  :) :> :] :] :O
  :) :> :] :] :O
  :) :> :D :) :) :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :D :D :O
  :) :> :P :D :P :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :D :) :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :D :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :P :] :O
  :) :> :] :) :) :O
  :) :> :P :] :) :] :O
  :) :> :P :] :P :P :O
  :) :> :P :] :P :P :O
  :) :> :P :D :) :] :O
  :) :> :] :) :) :O
  :) :> :P :] :D :D :O
  :) :> :P :] :D :] :O
  :) :> :] :) :) :O
  :) :> :P :D :P :) :O
  :) :> :P :] :] :) :O
  :) :> :P :] :P :P :O
  :) :> :] :) :) :O
  :) :> :P :D :P :D :O
  :) :> :P :] :) :P :O
  :) :> :P :] :D :) :O
  :) :> :P :] :D :) :O

  ; Halt
  :D :D :O
  ")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Smileyface
  ":D :) :) :O  ; Create a label with the ID 0.
   :) :D :P :O  ; Prompt the user for a character to store in cell 1.
   :) :) :P :O  ; Print the user character in cell 1.
   :D :P :) :O  ; Return to the label with the ID 0.")

;;; -------------------------------------------------------

;; Infinitely repeat the letters "el" using jumps.
(interpret-Smileyface
  ":) :> :P :) :] :) :O     ; Print 'H'.
   :D :) :) :O              ; Create a label with the ID 0.
   :) :> :P :] :P :P :O     ; Print 'e'.
   :) :> :P :] :D :) :O     ; Print 'l'.
   :D :] :) :> :) :> :) :O  ; If 0 = 0, then jump to label with ID 0.")

;;; -------------------------------------------------------

;; Print "Hel" using the instruction modification parameter (IMP) ":>",
;; which repeats the previous IMP.
(interpret-Smileyface "
:) :> :P :) :] :) :O ; H
:> :> :P :] :P :P :O ; e")
