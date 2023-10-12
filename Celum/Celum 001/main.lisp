;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Celum", invented by the Esolang user "Zzo38" and presented
;; on October 21st, 2006, based upon two conspicuous characteristics,
;; the first constituting the line-based nature of the syntax and the
;; resulting capabilities of conditional jumping betwixt these, the
;; second being its infinite tape whose bit-valued cells may only be
;; manipulated by applying an elementary cellular automaton unto their
;; content.
;; 
;; 
;; Concept
;; =======
;; The Celum programming language is founded upon a sequence of lines,
;; each prefixed with a bit, followed by an optional label name, and
;; concluding in a series of instructions, the same operate on an
;; infinite tape of bit-valued cells. Most conspicuously, these
;; facilities incorporate an elementary cellular automaton application
;; specimen, which, in the face of direct lacking cell manipulations,
;; such can be found in Urban Mueller's "brainfuck", for instance,
;; alters the memory's design.
;; 
;; == CELUM PROGRAMS: COMPOSITIONS OF LINES ==
;; In the general case, a Celum program line's composition adheres to
;; the forbisen
;; 
;;   prefix:label:commands
;;   ****** ***** ********
;; 
;; The prefix expects a bit value from the numeric set {0, 1}, whereas
;; the label establishes an optional entity for control flow mechanisms,
;; the liberty of which is exemplified by the capacity to submit an
;; an empty character sequence, as well as the homologation of duplicate
;; identifiers across the program. The commands comprehend a series
;; of zero or more operations defining the line's effect.
;; 
;; A particular species of line, a commentary exemplary, may be inserted
;; at any location by the prefix' substitution with the "c" or "C"
;; symbol:
;; 
;;   C:commentText
;;   c:commentText
;; 
;; Such apostils do not contribute any causata and are subjected to
;; neglect.
;; 
;; == PREFIX BITS: NAVIGATIONAL ANCHORS ==
;; The prefix bit's telos manifests in the contingency for navigational
;; exercises, perpetuated by a twain of operations which alter the
;; tape's central bit as a parasceve to the search for the next line in
;; the specified direction whose prefix matches the new tape center.
;; 
;; == LINE LABELS: GOTO TARGETS ==
;; As an important instance of supererogation from the prefix bit, the
;; line label names may be employed for the control flow's helming. Such
;; a goto attempt, seeking downwards from the current line, and wrapping
;; around until having either matched the identifier, or alternatively
;; reached the provenance line without success, upon ascertainment of
;; its telos' accomplishment, relocates the instruction pointer to the
;; start of the first eligible line; otherwise, the program is
;; terminated in immediacy.
;; 
;; == THE TAPE: AN INFINITE TAPE OF BITS ==
;; The paravaunt component of Celum's memory, a tape of infinite expanse
;; along both lateralities, and composed of a bits only, accommodates
;; the necessary data storage capability.
;; 
;; The central bit appropriates a cynosure's rank, as a preponderance
;; among the available operations is accompassed in regard to this unit.
;; 
;; The dioristic indicium commorant in the language, direct cell
;; manipulations are inflicted with a lacuna; instead, a reliance upon
;; an elementary cellular automaton rule for the entire tape's
;; recomputation is necessitated, or a command twissel, dedicated to the
;; central bit's inversion and subsequent search for a line prefixed
;; with the new bit value, ought to be issued.
;; 
;; For a cursory ilk of treatise on the topic involving elementary
;; cellular automata, please refer to the subsection
;; "Appendix A: ELEMENTARY CELLULAR AUTOMATA", subsumed into the
;; "Appendices" parent section.
;; 
;; == THE BIT FLAG: A GOTO ADMINICULUM ==
;; A second memory component, albeit conspicable parvipotent in
;; juxtaposition with the tape, is realized in the program's independent
;; bit flag, a bit-valued scalar sentinel whose manipulation ensues from
;; an attempted control flow redirection via label names, and whose
;; potential's entelechy exercises influence with the conditional
;; command skipping instruction, the same omits the current line's
;; remaining commands upon the bit flag's assumption of the value zero
;; (0).
;; 
;; == INPUT AND OUTPUT PROCEED BITWISE AND BUFFERED ==
;; A further element entalented with a kenspeckle demarcation relates to
;; the input and output facilities, accompassing their capacities with a
;; mode of guttation: based upon bits and buffering. Both components'
;; entelech issues in an airt from the least significant bit (LSB)
;; towards the most significant position (MSB).
;; 
;; == THE INPUT BUFFER: CHARACTER BITS GRADUALLY WRITTEN TO THE TAPE ==
;; A proprium of the input buffer manifests in its position cursor,
;; at the inchoation empighted on the least significant location, its
;; course being airted in gradual progression towards the higher-valued
;; bits.
;; 
;; An input command invocation provides an assessment's impetus: For a
;; vacant buffer, the the standard input conduit is queried for a single
;; ASCII character; the input buffer subsequently commits the received
;; character's ASCII code in its binary form to its own salvatory.
;; 
;; Such an initialized input buffer's reaction, commencing immediately
;; with the character's reception, reproduces the bit located at its
;; cursor in the tape's central position, while as an epiphenomenon
;; concomitantly advancing the same one step towards the most
;; significant bit place. No further interaction's necessitation ensues
;; until this highest bit's transport unto the tape. The transcendence's
;; ultimity, the desinent bit's transfer purges the buffer, resetting
;; its status to an empty sequence, and its position cursor to the least
;; significant location. A subsequently issued input operation will
;; iterum instigate the character reception and evaluation explicated
;; aboon.
;; 
;; == THE OUTPUT BUFFER: A TAPE'S BIT GATHERED INTO A BYTE ==
;; Its similitude to the input compernage ligates the output buffer to
;; a siclike stillatitious comportment.
;; 
;; At its in incipiency, the output buffer's position cursor registers
;; its occupancy at the least significant bit location, responding to a
;; print request by a transfer of the tape's middle bit to the cursor
;; locality, while advancing this marker one step towards the opposing
;; laterality. A display command does not elicit an unconditional output
;; reaction: Only if the most significant bit in the buffer has been
;; set, the completed octet's transcription into the corresponding ASCII
;; character and its commission to the standard output conduit is
;; accompassed, followed by the output buffer's purging, the same
;; renders its state tantamount to its pristine configuration.
;; 
;; 
;; Architecture
;; ============
;; Celum's architecture is delineated by a twain of components: the
;; bilaterally infinite bit-valued tape, and an independent bit flag.
;; 
;; == THE TAPE: AN INFINITE BIT VECTOR ==
;; The paravaunt material of deliberations in the context of the program
;; memory is imposed by the tape, an bilaterally unbounded expanse of
;; cells, each such a salvatory to an aefauld bit. Maugre its
;; innumerable mickleness, the central or middle bit appropriates a
;; dioristic significance, as a majority among the operations relay to
;; its perquisition or manipulation, incorporated into this set, the
;; kenspeckle elementary cellular automaton application.
;; 
;; == THE BIT FLAG: AN COMMAND OMISSION SWITCH ==
;; A paravail echolon's commorant, the bit flag, inchoated with a value
;; of zero (0), and inverted in its content by a line label search, may
;; be exploited in order to conditionally skip a line's remaining
;; commands, acting in this agency if the zero (0) state concurs with
;; the skipping command "?".
;; 
;; 
;; Data Types
;; ==========
;; Celum's type hierarchy wists of two species only: bits as the most
;; important contributors, and ASCII character which operate on the
;; communication channels.
;; 
;; 
;; Syntax
;; ======
;; A Celum program is compact of lines, each one designated by a
;; bit-valued prefix, an optional label, and a conditory to zero or more
;; instructions.
;; 
;; == LINE PATTERN ==
;; A particular forbisen's adhibition determines the format of any
;; effective line, rendering it a tripartite construct:
;; 
;;   (a) A mandatory bit value, equal either to 0 or 1, determines a
;;       potential jump target, followed by a colon.
;;   (b) A contingently empty label name, whose characters must be
;;       commorants of the ASCII code range [33, 126], excluding the
;;       colon (":"), as a second variant of jump target follows,
;;       succeeded by a further colon.
;;   (c) A sequence of zero or more instructions manifests the line's
;;       actual effects.
;; 
;; == LINEBREAKS ==
;; The installation of linebreaks constitutes a significant element in
;; the Celum source code, forecause its programs distribute along rows
;; amenable to be targeted by several jump instructions.
;; 
;; Tolerance is administer to vacant lines.
;; 
;; == SPACES ==
;; In all aspects, instances of the space character may be introduced
;; liberally without an influence upon the code's construe.
;; 
;; == COMMENTS ==
;; The language circumference attends to comments by lines whose prefix
;; bit is substituted by the character "c" or "C", followed by a colon
;; (":"), and any content to whom the respective line's entirety is
;; allotted, concluding, as any row, with the newline character or the
;; end of the program.
;; 
;; == GRAMMAR==
;; The following Extended Backus-Naur Form (EBNF) serves in the
;; delineation of the language's donat:
;; 
;;   program      := { innerLine } , [ lastLine ] ;
;;   
;;   innerLine    := lineContent , linebreak ;
;;   lastLine     := lineContent ;
;;   lineContent  := commandLine | commentLine | emptyLine ;
;;   
;;   emptyLine    := optionalSpaces ;
;;   commentLine  := ( "c" | "C" ) , colon , { character } ;
;;   commandLine  := bit
;;                ,  colon , [ labelName ]
;;                ,  colon , commands
;;                ; 
;;   
;;   commands     := { optionalSpaces , innerCommand , optionalSpaces }
;;                ,  [ flipFlagAndJump ]
;;                ;
;;   innerCommand := executeCellularAutomaton
;;                |  skipIfZero
;;                |  flipBitAndSearchUpwards
;;                |  flipBitAndSearchDownwards
;;                |  input
;;                |  output
;;                ;
;;   
;;   executeCellularAutomaton  := hexDigit , hexDigit ;
;;   skipIfZero                := "?" ;
;;   flipBitAndSearchUpwards   := "{" ;
;;   flipBitAndSearchDownwards := "}" ;
;;   flipFlagAndJump           := "!" , optionalSpaces , [ labelName ] ;
;;   input                     := "i" ;
;;   output                    := "o" ;
;;   
;;   labelName      := labelCharacter , { labelCharacter } ;
;;   labelCharacter := "!" | '"' | "#" | "$" | "%" | "&" | "'" | "("
;;                  |  ")" | "*" | "+" | "," | "-" | "." | "/"
;;                  |  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7"
;;                  |  "8" | "9"
;;                  |  ";" | "<" | "=" | ">" | "?" | "@"
;;                  |  "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H"
;;                  |  "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P"
;;                  |  "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X"
;;                  |  "Y" | "Z"
;;                  |  "[" | "\" | "]" | "^" | "_" | "`"
;;                  |  "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h"
;;                  |  "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p"
;;                  |  "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
;;                  |  "y" | "z"
;;                  |  "{" | "|" | "}" | "~"
;;                  ;
;;   linebreak      := "\n" ;
;;   colon          := ":" ;
;;   hexDigit       := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7"
;;                  |  "8" | "9" | "a" | "A" | "b" | "B" | "c" | "C"
;;                  |  "d" | "D" | "e" | "E" | "f" | "F"
;;                  ;
;;   bit            := "0" | "1" ;
;;   
;;   optionalSpaces := { space } ;
;;   spaces         := space , { space } ;
;;   space          := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; The compass of Celum's instruction set embraces seven members, the
;; most kenspeckle of which being the elementary cellular
;; automaton-based tape cell modifier. Further operations for input and
;; output are complemented by navigation specimens, switching to program
;; lines by adminiculum of some condition.
;; 
;; == INSTRUCTION TYPES ==
;; Celum's instructions may be regarded with augmented cohesion by
;; applying a subsumption unto its members into three possible tiers:
;; 
;;   - Cell modifying operations
;;     o The language relies on a very dioristic warklume for its tape
;;       cells' modification by its desistence for direct manipulation
;;       in favor of the application of an elementary cellular automaton
;;       rule on the whole infinite memory.
;;   - Input/Output facilities
;;     o The language incorporates a command for the prompting of a bit
;;       from the user, as well as a counterpart for a bit's printing to
;;       to the standard output.
;;     o Both operations appertaining to the central cell on the tape.
;;   - Navigational instructions
;;     o The preponderance of the instruction set targets the relocation
;;       of the line pointer in respondency to some criterion.
;;     o Two jump instruction variants can be distinguished:
;;       (i)  Bit-prefix based jumping: A line prefixed by a specified
;;            bit shall be detected.
;;       (ii) Label-based jumping: A line with a specified label shall
;;            be detected.
;;     o In addition, an instruction skipping operation exists which
;;       relies on the state of the Boolean flag.
;; 
;; == OVERVIEW ==
;; The septuple instruction set shall be subjected to a cursory
;; indagation:
;; 
;;   ------------------------------------------------------------------
;;   Command |Effect
;;   --------+---------------------------------------------------------
;;   hh      | Converts the two hexadecimal digits {hh} into a decimal
;;   **      | value and executes the elementary cellular automaton
;;           | rule corresponding with the same on the whole tape.
;;   ..................................................................
;;   i       | If the input buffer is empty, queries the standard input
;;           | conduit for an ASCII character and stores its bits in
;;           | the input buffer.
;;           | In any case, proceeding from the least significant bit
;;           | (LSB) to the most significant one (MSB), copies the bit
;;           | at the buffer's cursor position to the central cell of
;;           | the tape, while advancing the cursor one step towards
;;           | the most significant bit.
;;           | If the last bit has been thus processed, purges the
;;           | input buffer, rendering it empty, and resets the cursor
;;           | to the first, least significant, position.
;;   ..................................................................
;;   o       | Proceeding from the least significant bit (LSB) of the
;;           | output buffer towards the most significant one (MSB),
;;           | stores the tape's central bit in the in buffer at its
;;           | current position cursor, and advances the same one step
;;           | into the direction of the most significant bit.
;;           | If this highest location has been set, the output
;;           | buffer's content is interpreted as a decimal integer,
;;           | the corresponding ASCII character is printed to the
;;           | standard output, the buffer is purged, and its position
;;           | cursor is reset to the least significant position.
;;   ..................................................................
;;   ?       | If the Boolean flag is set to zero, skips the rest of
;;           | the current line. Otherwise, proceeds as usual.
;;   ..................................................................
;;   {       | Inverts the bit in the center of the tape, and searches,
;;           | starting from the current line, upwards in the
;;           | program for a line prefixed with the new central tape
;;           | bit value, ultimately executing that line, if detected.
;;           | Otherwise, an error of the type "MissingPrefixLineError"
;;           | is signaled.
;;   ..................................................................
;;   }       | Inverts the bit in the center of the tape, and searches,
;;           | starting from the current line, downwards in the
;;           | program for a line prefixed with the new central tape
;;           | bit value, ultimately executing that line, if detected.
;;           | Otherwise, an error of the type "MissingPrefixLineError"
;;           | is signaled.
;;   ..................................................................
;;   !label  | Inverts the program's bit flag, and, searches, starting
;;    *****  | with the line immediately below the current one, forward
;;           | in the program for a line whose label name matches the
;;           | specified {label}, potentially wrapping around if
;;           | passing the desinent line. Upon such a line's detection,
;;           | the same is executed.
;;           | If no line with the {label} can be found, the program
;;           | is terminated immediately.
;;           | This instruction must be the desinent one on its line.
;;   ------------------------------------------------------------------
;; 
;; == APPLY ELEMENTARY CELLULAR AUTOMATON RULE ("hh") ==
;; Applies the elementary cellular automaton rule identified by the
;; decimal integer value of the hexadecimal digit pair to the entire
;; tape.
;; 
;; Signature:
;;   hh
;;   **
;; 
;; Parameters:
;;   hh: Two hexadecimal digits, in either character case, whose
;;       unsigned decimal integer equivalent identifies the elementary
;;       cellular automaton rule to apply.
;; 
;; Interface:
;;   applyElementaryCellularAutomatonRule (rule : integer in [0, 255])
;;   : void
;; 
;; Pseudocode:
;;   Input:
;;     rule --- decimal value of hexadecimal digit pair hh.
;;   
;;   let newTape <- empty tape
;;   
;;   for each cell in tape do
;;     let leftNeighborCell  <- getLeftNeighbor(cell)
;;     let rightNeighborCell <- getRightNeighbor(cell)
;;     let bitKey            <- (leftNeighborCell,
;;                               cell,
;;                               rightNeighborCell)
;;     let ruleOutput        <- applyRule (rule, bitKey)
;;     newTape.appendCell(ruleOutput)
;;   end for
;;   
;;   tape <- newTape
;; 
;; Side effects:
;;   - The entire tape is subjected to the selected elementary cellular
;;     automaton rule.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == INPUT BIT ("i") ==
;; If the input buffer is empty, queries the standard input for an ASCII
;; character whose bits are transferred into the buffer. In any case,
;; proceeding from the least significant bit (LSB) towards the most
;; significant bit position (MSB), copies the current bit to the tape's
;; middle cell, and increments the input buffer's bit cursor one step
;; towards the most significant position. If this upper bourne has been
;; transcended, the input buffer is purged and reset, amenable to a
;; character request on the subsequent invocation of this command.
;; 
;; Signature:
;;   i
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   input () : void
;; 
;; Pseudocode:
;;   if inputBuffer is empty then
;;     let userInput <- query for ASCII character
;;     store userInput bits in inputBuffer
;;   end if
;;   
;;   setCenterBit(tape, inputBuffer.getNextBit())
;;   
;;   if inputBuffer is exhausted then
;;     set inputBuffer to empty
;;     reset bit cursor of inputBuffer
;;   end if
;; 
;; Side effects:
;;   - The tape's central cell value is modified by the input buffer's
;;     currently active bit.
;;   - If the input buffer is empty or has been purged since the last
;;     inquisition, the standard input conduit is queried for an ASCII
;;     character.
;; 
;; Exceptional situations:
;;   - An error of an unspecified type is signaled if the standard input
;;     conduit fails to respond to the query for an ASCII character.
;; 
;; == OUTPUT BIT ("o") ==
;; Copies the tape's central bit into the current output buffer cursor
;; position, and advances the same one step towards the most significant
;; bit (MBS) location. If this march has been transgressed, a status
;; which signifies the buffer's patration, its bit content are
;; transliterated into their decimal equivalent, and the character whose
;; ASCII code corresponds to this base-10 value is issued to the
;; standard output, while subsequently the output buffer is purged and
;; its position cursor reset to the least significant bit (LSB) state.
;; 
;; Signature:
;;   o
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   output () : void
;; 
;; Pseudocode:
;;   if outputBuffer is complete then
;;     let asciiCode      <- get outputBuffer as decimal integer
;;     let asciiCharacter <- get character for asciiCode
;;     print asciiCharacter
;;     reset position cursor of outputBuffer
;;   end if
;;   
;;   outputBuffer.setNextBit(getCenterBit(tape))
;; 
;; Side effects:
;;   - If the output buffer's eight-bit character code is completed, the
;;     corresponding ASCII character is printed to the standard output
;;     conduit.
;; 
;; Exceptional situations:
;;   - An error of an unspecified type is signaled if the output conduit
;;     does not respond to the query for output buffer character's
;;     display.
;; 
;; == SKIP LINE COMMANDS IF FLAG IS ZERO ("?") ==
;; If the program's bit flag equals zero, skips the currently processed
;; line's commands; otherwise proceeds as usual.
;; 
;; Signature:
;;   ?
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   skipLineIfZero () : void
;; 
;; Pseudocode:
;;   if bitFlag = 0 then
;;     currentLine <- getLineAfter(currentLine)
;;   end if
;; 
;; Side effects:
;;   - None.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == GO TO BIT ABOVE ("{") ==
;; Inverts the bit in the tape's center and relocates the instruction
;; pointer to the first line above the current line whose prefix bit
;; matches the new tape center bit.
;; 
;; Signature:
;;   {
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   flipBitAndSearchUpwards () : void
;; 
;; Pseudocode:
;;   flipCenterBit(tape)
;;   
;;   let hasFoundLine <- false
;;   let probedLine   <- current line in program
;;   
;;   while hasLineBefore(probedLine) do
;;     probedLine <- getLineBefore(probedLine)
;;     if getPrefixBit(probedLine) = setCenterBit(tape) then
;;       hasFoundLine <- true
;;       relocate instruction pointer to probedLine
;;       break
;;     end if
;;   end while
;;   
;;   if not hasFoundLine then
;;     relocate instruction pointer to probedLine
;;   else
;;     signal MissingPrefixLineError
;;   end if
;; 
;; Side effects:
;;   - The tape's central cell value is inverted.
;; 
;; Exceptional situations:
;;   - If no line prefixed with the expected bit value can be detected,
;;     an error of the type "MissingPrefixLineError" is signaled.
;; 
;; == GO TO BIT BELOW ("}") ==
;; Inverts the bit in the tape's center and relocates the instruction
;; pointer to the first line below the current line whose prefix bit
;; matches the new tape center bit.
;; 
;; Signature:
;;   }
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   flipBitAndSearchDownwards() : void
;; 
;; Pseudocode:
;;   flipCenterBit(tape)
;;   
;;   let hasFoundLine <- false
;;   let probedLine   <- current line in program
;;   
;;   while hasLineAfter(probedLine) do
;;     probedLine <- getLineAfter(probedLine)
;;     if getPrefixBit(probedLine) = setCenterBit(tape) then
;;       hasFoundLine <- true
;;       relocate instruction pointer to probedLine
;;       break
;;     end if
;;   end while
;;   
;;   if not hasFoundLine then
;;     relocate instruction pointer to probedLine
;;   else
;;     signal MissingPrefixLineError
;;   end if
;; 
;; Side effects:
;;   - The tape's central cell value is inverted.
;; 
;; Exceptional situations:
;;   - If no line prefixed with the expected bit value can be detected,
;;     an error of the type "MissingPrefixLineError" is signaled.
;; 
;; == GO TO LABEL ("!") ==
;; Inverts the Boolean flag, ere searching for a label whose name
;; matches that designated by this command, seeking forward in the
;; program and wrapping around at its desinence. If a line containing
;; this label exists, the instruction pointer is relocated to the same,
;; otherwise the program is immediately terminated.
;; 
;; Signature:
;;   !label
;;    *****
;; 
;; Parameters:
;;   label --- a string denoting the line label to search for. It may
;;             be empty, which designates the homologated empty label
;;             name.
;; 
;; Interface:
;;   flipFlagAndGoToLabel (label : string) : void
;; 
;; Pseudocode:
;;   Input:
;;     label --- the name of the label to jump to.
;;   
;;   flipBitFlag()
;;   
;;   let matchingLine <- nil
;;   let probedLine   <- getLineBelow(current line in program)
;;   
;;   while probedLine != current line in program do
;;     if getLabel(probedLine) = {label} then
;;       matchingLine <- probedLine
;;       break
;;     end if
;;     
;;     if hasLineAfter(probedLine) then
;;       probedLine <- getLineAfter(probedLine)
;;     else
;;       probedLine <- get first line in program
;;     end if
;;   end while
;;   
;;   if matchingLine != nil then
;;     relocate instruction pointer to the matchineLine
;;   else
;;     terminate program
;;   end if
;; 
;; Side effects:
;;   - The program's bit flag is inverted.
;;   - If no line endowed with an amenability to the search {label}
;;     identifier can be detected, the program will be immediately
;;     terminated.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == OPERATIONS SUBSUMED BY CONTEXTS ==
;; The variety of operations, interrelations and intersections with
;; the Celum programming language's ambits shall be unraveled by the
;; following sections' adminiculum, the same dedicate themselves to the
;; language's logical components.
;; 
;; == TAPE OPERATIONS ==
;; Tape operations are appropriated for all causes appertaining to the
;; indagation and manipulation of the program's infinite tape, compact
;; of bit-valued cells, whose central unit enjoys a particular
;; significance's apportionment.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   hh      | Applies the elementary cellular automaton rule
;;   **      | identified by the decimal equivalent of the hexadecimal
;;           | digit pair {hh} to the entire tape.
;;   ..................................................................
;;   i       | Queries the standard input for a bit, which is
;;           | subsequently transferred into the tape's central cell.
;;   ..................................................................
;;   o       | Outputs the bit stored in the tape's center cell.
;;   ..................................................................
;;   {       | Toggles the bit in the middle of the tape, ere searching
;;           | upwards from the current line for a line whose prefix
;;           | bit equals the new tape center bit value.
;;   ..................................................................
;;   }       | Toggles the bit in the middle of the tape, ere searching
;;           | downwards from the current line for a line whose prefix
;;           | bit equals the new tape center bit value.
;;   ------------------------------------------------------------------
;; 
;; == PREFIX BIT OPERATIONS ==
;; A twissel of operations appertain to the lines' prefix bits'
;; handling:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   {       | Toggles the bit in the middle of the tape, ere searching
;;           | upwards from the current line for a line whose prefix
;;           | bit equals the new tape center bit value.
;;   ..................................................................
;;   }       | Toggles the bit in the middle of the tape, ere searching
;;           | downwards from the current line for a line whose prefix
;;           | bit equals the new tape center bit value.
;;   ------------------------------------------------------------------
;; 
;; == BIT FLAG OPERATIONS ==
;; The bit flag, a binary sentinel operating independently of the lines
;; as a program's adminicular warklume, its diorism the conditional
;; omission of commands, ostends an amenability to a special instruction
;; twain:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   !label  | Unconditionally toggles the bit flag's value, ere
;;    *****  | searching for a line agnominated by the {label} name.
;;   ..................................................................
;;   ?       | If the bit flag's value equals zero (0), skips all
;;           | remaining commands on the current line.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The rather curt circumference administered to its explications, in
;; champarty with the a dearth in examples, inflicts the Celum
;; specification with an etiology for ambiguities, whence a subset shall
;; be desumed for more meticulous perquisition.
;; 
;; == HOW SHALL A PROGRAM REACT TO A MISSING PREFIX BIT? ==
;; A discrepancy from the label-based line searching operation, the
;; ultimity of a prefix bit's failed inquisition has not been specified
;; by the original language standard.
;; 
;; A treble of possible tendance variants rede their acquisition:
;; 
;;   (1) ERROR ELICITATION:
;;       A failure to locate the desired bit on a line instigates an
;;       error of the type "MissingPrefixLineError", the fatality woning
;;       inside of its ponderance immediately terminates the program in
;;       an anomalous and uncontrolled manner.
;;       This species of amenability ostends a patent concinnity with
;;       the Boolfuck programming language, ligated by consanguinity in
;;       many aspects, embracing also a failure on a mismatched jump or
;;       loop boundary.
;;   
;;   (2) TERMINATIION:
;;       The missing prefix bit's ascertainment immediately terminates
;;       the program in a regulated manner.
;;       Such a deportment would introduce a vinculum betwixt the
;;       bit-based search commodity and the label matching compernage.
;;   
;;   (3) NEGLECT:
;;       The instruction pointer simply ignores the failed control flow
;;       attempt and advances to the next command on the current line.
;; 
;; It has been adjudged, proceeding from the Boolfuck equivalency, to
;; impute the first (1) behavior as the canonical response, its presence
;; a mimicry of the stringency commorant in common iteration bournes
;; matching.
;; 
;; == HOW SHALL INPUT AND OUTPUT BE ACTUATED? ==
;; Celum's protolog, in its command listing's perimeter, relates of the
;; input and output commodities as simply issuing or receiving the
;; central bit from the tape; its section dedicated to the equiparation
;; with the Boolfuck programming language, natheless, intimates a more
;; complex buffering deportment.
;; 
;; It has been adjudged that the entheus from Boolfuck most probably
;; specifies the communication principles, employing buffers for this
;; wike. A consectary yielded by this alternative capacitates Celum
;; programs with character-based input and output competences, as
;; counterdistinguished from the athwart restriction to binary digits.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, realized by intrining the stages of lexical analyzation,
;; parsing, and actual interpretation, with the former twissel conflated
;; into an nearly inextricable union.
;; 
;; 
;; Appendices
;; ==========
;; Those subjects whose eligibility has been not been avered by this
;; project author's exercised act of docimasy a thing of essential
;; contribution, yet whom some peisant mete's commorancy attributes an
;; at least cursory ilk of perquisition, shall be the coming sections'
;; cynosure.
;; 
;; == APPENDIX A: ELEMENTARY CELLULAR AUTOMATA ==
;; The elementary cellular automata establish a specific species in the
;; cellular automaton realm, restricted in their foundational structure
;; to a single dimension, a line compact of a series of bit-valued
;; cells, which, employing three-bit neighborhoods, obey to a set of
;; 256 rules, enumerated by the integer values in the range [0, 255],
;; the same, when expressed in base-2, prescribe two-state patterns.
;; 
;; The three-bit neighborhood acts in the agency of a key, selecting
;; from the chosen rule's binary pattern an aefauld bit to harness, for
;; the anagenesis' subsequent generation, as the new cell state. New
;; generations usually do not veridically supersede their ancestors, and
;; are instead empighted on a fresh line below, the resulting chronicle
;; exhibiting a dioristic texture as a dependency of time.
;; 
;; The following visualization shall serve an endeictic wike in the
;; reproduction of the rule 126 across 24 generations. Please note that
;; the complete composition proceeded from a single bit in the topmost
;; line, with any subsequent row having been born out of the previous
;; one by the rule's adhibition.
;; 
;;                          #                        
;;                         ###                       
;;                        ## ##                      
;;                       #######                     
;;                      ##     ##                    
;;                     ####   ####                   
;;                    ##  ## ##  ##                  
;;                   ###############                 
;;                  ##             ##                
;;                 ####           ####               
;;                ##  ##         ##  ##              
;;               ########       ########             
;;              ##      ##     ##      ##            
;;             ####    ####   ####    ####           
;;            ##  ##  ##  ## ##  ##  ##  ##          
;;           ###############################         
;;          ##                             ##        
;;         ####                           ####       
;;        ##  ##                         ##  ##      
;;       ########                       ########     
;;      ##      ##                     ##      ##    
;;     ####    ####                   ####    ####   
;;    ##  ##  ##  ##                 ##  ##  ##  ##  
;;   ################               ################ 
;; 
;; == THIS TREATISE'S STRUCTURE: FROM THE BASICS TO THE NUCLEUS ==
;; The following treatise applies itself to a very rudimentary ilk of
;; its pursued nortelry's dispersion, at its inchoation administering a
;; parasceuastic apercu regarding the binary numeral system and its
;; contribution to the elementary cellular automaton rules, ere the
;; actual vincula's exposition ensues.
;; 
;; == EVERY BINARY SEQUENCE CORRESPONDS TO EXACTLY ONE DECIMAL NUMBER ==
;; The binary system establishes an equivalency to its decimal
;; counterpart, the latter of which employs the ten digits
;; 
;;   0, 1, 2, 3, 4, 5, 6, 7, 8, 9
;; 
;; whereas the former, maugre its equipollence, relies on a twissel of
;; symbols only:
;; 
;;   0, 1
;; 
;; Each member from this twain is also norned a "bit".
;; 
;; For our purposes, the infinite set of integer values is restricted
;; to the unsigned byte, or octet, portion that covers the closed
;; interval [0, 255]. The following table shall adhibit a foundational
;; piece of gnarity concerning the binary representation of all elements
;; from this restricted integral selection:
;; 
;;   ---------------------------------
;;   Binary (8 bits)  | Decimal number
;;   -----------------+---------------
;;   00000000         | 0
;;   .................................
;;   00000001         | 1
;;   .................................
;;   00000010         | 2
;;   .................................
;;   00000011         | 3
;;   .................................
;;   00000100         | 4
;;   .................................
;;   00000101         | 5
;;   .................................
;;   00000110         | 6
;;   .................................
;;   00000111         | 7
;;   .................................
;;   00001000         | 8
;;   .................................
;;   00001001         | 9
;;   .................................
;;   00001010         | 10
;;   .................................
;;   00001011         | 11
;;   .................................
;;   00001100         | 12
;;   .................................
;;   00001101         | 13
;;   .................................
;;   00001110         | 14
;;   .................................
;;   00001111         | 15
;;   .................................
;;   00010000         | 16
;;   .................................
;;   00010001         | 17
;;   .................................
;;   00010010         | 18
;;   .................................
;;   00010011         | 19
;;   .................................
;;   00010100         | 20
;;   .................................
;;   00010101         | 21
;;   .................................
;;   00010110         | 22
;;   .................................
;;   00010111         | 23
;;   .................................
;;   00011000         | 24
;;   .................................
;;   00011001         | 25
;;   .................................
;;   00011010         | 26
;;   .................................
;;   00011011         | 27
;;   .................................
;;   00011100         | 28
;;   .................................
;;   00011101         | 29
;;   .................................
;;   00011110         | 30
;;   .................................
;;   00011111         | 31
;;   .................................
;;   00100000         | 32
;;   .................................
;;   00100001         | 33
;;   .................................
;;   00100010         | 34
;;   .................................
;;   00100011         | 35
;;   .................................
;;   00100100         | 36
;;   .................................
;;   00100101         | 37
;;   .................................
;;   00100110         | 38
;;   .................................
;;   00100111         | 39
;;   .................................
;;   00101000         | 40
;;   .................................
;;   00101001         | 41
;;   .................................
;;   00101010         | 42
;;   .................................
;;   00101011         | 43
;;   .................................
;;   00101100         | 44
;;   .................................
;;   00101101         | 45
;;   .................................
;;   00101110         | 46
;;   .................................
;;   00101111         | 47
;;   .................................
;;   00110000         | 48
;;   .................................
;;   00110001         | 49
;;   .................................
;;   00110010         | 50
;;   .................................
;;   00110011         | 51
;;   .................................
;;   00110100         | 52
;;   .................................
;;   00110101         | 53
;;   .................................
;;   00110110         | 54
;;   .................................
;;   00110111         | 55
;;   .................................
;;   00111000         | 56
;;   .................................
;;   00111001         | 57
;;   .................................
;;   00111010         | 58
;;   .................................
;;   00111011         | 59
;;   .................................
;;   00111100         | 60
;;   .................................
;;   00111101         | 61
;;   .................................
;;   00111110         | 62
;;   .................................
;;   00111111         | 63
;;   .................................
;;   01000000         | 64
;;   .................................
;;   01000001         | 65
;;   .................................
;;   01000010         | 66
;;   .................................
;;   01000011         | 67
;;   .................................
;;   01000100         | 68
;;   .................................
;;   01000101         | 69
;;   .................................
;;   01000110         | 70
;;   .................................
;;   01000111         | 71
;;   .................................
;;   01001000         | 72
;;   .................................
;;   01001001         | 73
;;   .................................
;;   01001010         | 74
;;   .................................
;;   01001011         | 75
;;   .................................
;;   01001100         | 76
;;   .................................
;;   01001101         | 77
;;   .................................
;;   01001110         | 78
;;   .................................
;;   01001111         | 79
;;   .................................
;;   01010000         | 80
;;   .................................
;;   01010001         | 81
;;   .................................
;;   01010010         | 82
;;   .................................
;;   01010011         | 83
;;   .................................
;;   01010100         | 84
;;   .................................
;;   01010101         | 85
;;   .................................
;;   01010110         | 86
;;   .................................
;;   01010111         | 87
;;   .................................
;;   01011000         | 88
;;   .................................
;;   01011001         | 89
;;   .................................
;;   01011010         | 90
;;   .................................
;;   01011011         | 91
;;   .................................
;;   01011100         | 92
;;   .................................
;;   01011101         | 93
;;   .................................
;;   01011110         | 94
;;   .................................
;;   01011111         | 95
;;   .................................
;;   01100000         | 96
;;   .................................
;;   01100001         | 97
;;   .................................
;;   01100010         | 98
;;   .................................
;;   01100011         | 99
;;   .................................
;;   01100100         | 100
;;   .................................
;;   01100101         | 101
;;   .................................
;;   01100110         | 102
;;   .................................
;;   01100111         | 103
;;   .................................
;;   01101000         | 104
;;   .................................
;;   01101001         | 105
;;   .................................
;;   01101010         | 106
;;   .................................
;;   01101011         | 107
;;   .................................
;;   01101100         | 108
;;   .................................
;;   01101101         | 109
;;   .................................
;;   01101110         | 110
;;   .................................
;;   01101111         | 111
;;   .................................
;;   01110000         | 112
;;   .................................
;;   01110001         | 113
;;   .................................
;;   01110010         | 114
;;   .................................
;;   01110011         | 115
;;   .................................
;;   01110100         | 116
;;   .................................
;;   01110101         | 117
;;   .................................
;;   01110110         | 118
;;   .................................
;;   01110111         | 119
;;   .................................
;;   01111000         | 120
;;   .................................
;;   01111001         | 121
;;   .................................
;;   01111010         | 122
;;   .................................
;;   01111011         | 123
;;   .................................
;;   01111100         | 124
;;   .................................
;;   01111101         | 125
;;   .................................
;;   01111110         | 126
;;   .................................
;;   01111111         | 127
;;   .................................
;;   10000000         | 128
;;   .................................
;;   10000001         | 129
;;   .................................
;;   10000010         | 130
;;   .................................
;;   10000011         | 131
;;   .................................
;;   10000100         | 132
;;   .................................
;;   10000101         | 133
;;   .................................
;;   10000110         | 134
;;   .................................
;;   10000111         | 135
;;   .................................
;;   10001000         | 136
;;   .................................
;;   10001001         | 137
;;   .................................
;;   10001010         | 138
;;   .................................
;;   10001011         | 139
;;   .................................
;;   10001100         | 140
;;   .................................
;;   10001101         | 141
;;   .................................
;;   10001110         | 142
;;   .................................
;;   10001111         | 143
;;   .................................
;;   10010000         | 144
;;   .................................
;;   10010001         | 145
;;   .................................
;;   10010010         | 146
;;   .................................
;;   10010011         | 147
;;   .................................
;;   10010100         | 148
;;   .................................
;;   10010101         | 149
;;   .................................
;;   10010110         | 150
;;   .................................
;;   10010111         | 151
;;   .................................
;;   10011000         | 152
;;   .................................
;;   10011001         | 153
;;   .................................
;;   10011010         | 154
;;   .................................
;;   10011011         | 155
;;   .................................
;;   10011100         | 156
;;   .................................
;;   10011101         | 157
;;   .................................
;;   10011110         | 158
;;   .................................
;;   10011111         | 159
;;   .................................
;;   10100000         | 160
;;   .................................
;;   10100001         | 161
;;   .................................
;;   10100010         | 162
;;   .................................
;;   10100011         | 163
;;   .................................
;;   10100100         | 164
;;   .................................
;;   10100101         | 165
;;   .................................
;;   10100110         | 166
;;   .................................
;;   10100111         | 167
;;   .................................
;;   10101000         | 168
;;   .................................
;;   10101001         | 169
;;   .................................
;;   10101010         | 170
;;   .................................
;;   10101011         | 171
;;   .................................
;;   10101100         | 172
;;   .................................
;;   10101101         | 173
;;   .................................
;;   10101110         | 174
;;   .................................
;;   10101111         | 175
;;   .................................
;;   10110000         | 176
;;   .................................
;;   10110001         | 177
;;   .................................
;;   10110010         | 178
;;   .................................
;;   10110011         | 179
;;   .................................
;;   10110100         | 180
;;   .................................
;;   10110101         | 181
;;   .................................
;;   10110110         | 182
;;   .................................
;;   10110111         | 183
;;   .................................
;;   10111000         | 184
;;   .................................
;;   10111001         | 185
;;   .................................
;;   10111010         | 186
;;   .................................
;;   10111011         | 187
;;   .................................
;;   10111100         | 188
;;   .................................
;;   10111101         | 189
;;   .................................
;;   10111110         | 190
;;   .................................
;;   10111111         | 191
;;   .................................
;;   11000000         | 192
;;   .................................
;;   11000001         | 193
;;   .................................
;;   11000010         | 194
;;   .................................
;;   11000011         | 195
;;   .................................
;;   11000100         | 196
;;   .................................
;;   11000101         | 197
;;   .................................
;;   11000110         | 198
;;   .................................
;;   11000111         | 199
;;   .................................
;;   11001000         | 200
;;   .................................
;;   11001001         | 201
;;   .................................
;;   11001010         | 202
;;   .................................
;;   11001011         | 203
;;   .................................
;;   11001100         | 204
;;   .................................
;;   11001101         | 205
;;   .................................
;;   11001110         | 206
;;   .................................
;;   11001111         | 207
;;   .................................
;;   11010000         | 208
;;   .................................
;;   11010001         | 209
;;   .................................
;;   11010010         | 210
;;   .................................
;;   11010011         | 211
;;   .................................
;;   11010100         | 212
;;   .................................
;;   11010101         | 213
;;   .................................
;;   11010110         | 214
;;   .................................
;;   11010111         | 215
;;   .................................
;;   11011000         | 216
;;   .................................
;;   11011001         | 217
;;   .................................
;;   11011010         | 218
;;   .................................
;;   11011011         | 219
;;   .................................
;;   11011100         | 220
;;   .................................
;;   11011101         | 221
;;   .................................
;;   11011110         | 222
;;   .................................
;;   11011111         | 223
;;   .................................
;;   11100000         | 224
;;   .................................
;;   11100001         | 225
;;   .................................
;;   11100010         | 226
;;   .................................
;;   11100011         | 227
;;   .................................
;;   11100100         | 228
;;   .................................
;;   11100101         | 229
;;   .................................
;;   11100110         | 230
;;   .................................
;;   11100111         | 231
;;   .................................
;;   11101000         | 232
;;   .................................
;;   11101001         | 233
;;   .................................
;;   11101010         | 234
;;   .................................
;;   11101011         | 235
;;   .................................
;;   11101100         | 236
;;   .................................
;;   11101101         | 237
;;   .................................
;;   11101110         | 238
;;   .................................
;;   11101111         | 239
;;   .................................
;;   11110000         | 240
;;   .................................
;;   11110001         | 241
;;   .................................
;;   11110010         | 242
;;   .................................
;;   11110011         | 243
;;   .................................
;;   11110100         | 244
;;   .................................
;;   11110101         | 245
;;   .................................
;;   11110110         | 246
;;   .................................
;;   11110111         | 247
;;   .................................
;;   11111000         | 248
;;   .................................
;;   11111001         | 249
;;   .................................
;;   11111010         | 250
;;   .................................
;;   11111011         | 251
;;   .................................
;;   11111100         | 252
;;   .................................
;;   11111101         | 253
;;   .................................
;;   11111110         | 254
;;   .................................
;;   11111111         | 255
;;   ---------------------------------
;; 
;; == THE DECIMAL NUMBERS ENUMERATE THE BINARY PATTERNS ==
;; The bijective nature inherent to the binary and decimal mapping ---
;; every binary sequence corresponds to exactly one decimal number, and
;; vice versa ---, permits an equiparation of the aboon ostended
;; amphichiral table:
;; 
;;   --------------------------------
;;   Decimal number | Binary (8 bits)
;;   ---------------+----------------
;;   0              | 00000000
;;   ................................
;;   1              | 00000001
;;   ................................
;;   2              | 00000010
;;   ................................
;;   3              | 00000011
;;   ................................
;;   4              | 00000100
;;   ................................
;;   5              | 00000101
;;   ................................
;;   6              | 00000110
;;   ................................
;;   7              | 00000111
;;   ................................
;;   8              | 00001000
;;   ................................
;;   9              | 00001001
;;   ................................
;;   10             | 00001010
;;   ................................
;;   11             | 00001011
;;   ................................
;;   12             | 00001100
;;   ................................
;;   13             | 00001101
;;   ................................
;;   14             | 00001110
;;   ................................
;;   15             | 00001111
;;   ................................
;;   16             | 00010000
;;   ................................
;;   17             | 00010001
;;   ................................
;;   18             | 00010010
;;   ................................
;;   19             | 00010011
;;   ................................
;;   20             | 00010100
;;   ................................
;;   21             | 00010101
;;   ................................
;;   22             | 00010110
;;   ................................
;;   23             | 00010111
;;   ................................
;;   24             | 00011000
;;   ................................
;;   25             | 00011001
;;   ................................
;;   26             | 00011010
;;   ................................
;;   27             | 00011011
;;   ................................
;;   28             | 00011100
;;   ................................
;;   29             | 00011101
;;   ................................
;;   30             | 00011110
;;   ................................
;;   31             | 00011111
;;   ................................
;;   32             | 00100000
;;   ................................
;;   33             | 00100001
;;   ................................
;;   34             | 00100010
;;   ................................
;;   35             | 00100011
;;   ................................
;;   36             | 00100100
;;   ................................
;;   37             | 00100101
;;   ................................
;;   38             | 00100110
;;   ................................
;;   39             | 00100111
;;   ................................
;;   40             | 00101000
;;   ................................
;;   41             | 00101001
;;   ................................
;;   42             | 00101010
;;   ................................
;;   43             | 00101011
;;   ................................
;;   44             | 00101100
;;   ................................
;;   45             | 00101101
;;   ................................
;;   46             | 00101110
;;   ................................
;;   47             | 00101111
;;   ................................
;;   48             | 00110000
;;   ................................
;;   49             | 00110001
;;   ................................
;;   50             | 00110010
;;   ................................
;;   51             | 00110011
;;   ................................
;;   52             | 00110100
;;   ................................
;;   53             | 00110101
;;   ................................
;;   54             | 00110110
;;   ................................
;;   55             | 00110111
;;   ................................
;;   56             | 00111000
;;   ................................
;;   57             | 00111001
;;   ................................
;;   58             | 00111010
;;   ................................
;;   59             | 00111011
;;   ................................
;;   60             | 00111100
;;   ................................
;;   61             | 00111101
;;   ................................
;;   62             | 00111110
;;   ................................
;;   63             | 00111111
;;   ................................
;;   64             | 01000000
;;   ................................
;;   65             | 01000001
;;   ................................
;;   66             | 01000010
;;   ................................
;;   67             | 01000011
;;   ................................
;;   68             | 01000100
;;   ................................
;;   69             | 01000101
;;   ................................
;;   70             | 01000110
;;   ................................
;;   71             | 01000111
;;   ................................
;;   72             | 01001000
;;   ................................
;;   73             | 01001001
;;   ................................
;;   74             | 01001010
;;   ................................
;;   75             | 01001011
;;   ................................
;;   76             | 01001100
;;   ................................
;;   77             | 01001101
;;   ................................
;;   78             | 01001110
;;   ................................
;;   79             | 01001111
;;   ................................
;;   80             | 01010000
;;   ................................
;;   81             | 01010001
;;   ................................
;;   82             | 01010010
;;   ................................
;;   83             | 01010011
;;   ................................
;;   84             | 01010100
;;   ................................
;;   85             | 01010101
;;   ................................
;;   86             | 01010110
;;   ................................
;;   87             | 01010111
;;   ................................
;;   88             | 01011000
;;   ................................
;;   89             | 01011001
;;   ................................
;;   90             | 01011010
;;   ................................
;;   91             | 01011011
;;   ................................
;;   92             | 01011100
;;   ................................
;;   93             | 01011101
;;   ................................
;;   94             | 01011110
;;   ................................
;;   95             | 01011111
;;   ................................
;;   96             | 01100000
;;   ................................
;;   97             | 01100001
;;   ................................
;;   98             | 01100010
;;   ................................
;;   99             | 01100011
;;   ................................
;;   100            | 01100100
;;   ................................
;;   101            | 01100101
;;   ................................
;;   102            | 01100110
;;   ................................
;;   103            | 01100111
;;   ................................
;;   104            | 01101000
;;   ................................
;;   105            | 01101001
;;   ................................
;;   106            | 01101010
;;   ................................
;;   107            | 01101011
;;   ................................
;;   108            | 01101100
;;   ................................
;;   109            | 01101101
;;   ................................
;;   110            | 01101110
;;   ................................
;;   111            | 01101111
;;   ................................
;;   112            | 01110000
;;   ................................
;;   113            | 01110001
;;   ................................
;;   114            | 01110010
;;   ................................
;;   115            | 01110011
;;   ................................
;;   116            | 01110100
;;   ................................
;;   117            | 01110101
;;   ................................
;;   118            | 01110110
;;   ................................
;;   119            | 01110111
;;   ................................
;;   120            | 01111000
;;   ................................
;;   121            | 01111001
;;   ................................
;;   122            | 01111010
;;   ................................
;;   123            | 01111011
;;   ................................
;;   124            | 01111100
;;   ................................
;;   125            | 01111101
;;   ................................
;;   126            | 01111110
;;   ................................
;;   127            | 01111111
;;   ................................
;;   128            | 10000000
;;   ................................
;;   129            | 10000001
;;   ................................
;;   130            | 10000010
;;   ................................
;;   131            | 10000011
;;   ................................
;;   132            | 10000100
;;   ................................
;;   133            | 10000101
;;   ................................
;;   134            | 10000110
;;   ................................
;;   135            | 10000111
;;   ................................
;;   136            | 10001000
;;   ................................
;;   137            | 10001001
;;   ................................
;;   138            | 10001010
;;   ................................
;;   139            | 10001011
;;   ................................
;;   140            | 10001100
;;   ................................
;;   141            | 10001101
;;   ................................
;;   142            | 10001110
;;   ................................
;;   143            | 10001111
;;   ................................
;;   144            | 10010000
;;   ................................
;;   145            | 10010001
;;   ................................
;;   146            | 10010010
;;   ................................
;;   147            | 10010011
;;   ................................
;;   148            | 10010100
;;   ................................
;;   149            | 10010101
;;   ................................
;;   150            | 10010110
;;   ................................
;;   151            | 10010111
;;   ................................
;;   152            | 10011000
;;   ................................
;;   153            | 10011001
;;   ................................
;;   154            | 10011010
;;   ................................
;;   155            | 10011011
;;   ................................
;;   156            | 10011100
;;   ................................
;;   157            | 10011101
;;   ................................
;;   158            | 10011110
;;   ................................
;;   159            | 10011111
;;   ................................
;;   160            | 10100000
;;   ................................
;;   161            | 10100001
;;   ................................
;;   162            | 10100010
;;   ................................
;;   163            | 10100011
;;   ................................
;;   164            | 10100100
;;   ................................
;;   165            | 10100101
;;   ................................
;;   166            | 10100110
;;   ................................
;;   167            | 10100111
;;   ................................
;;   168            | 10101000
;;   ................................
;;   169            | 10101001
;;   ................................
;;   170            | 10101010
;;   ................................
;;   171            | 10101011
;;   ................................
;;   172            | 10101100
;;   ................................
;;   173            | 10101101
;;   ................................
;;   174            | 10101110
;;   ................................
;;   175            | 10101111
;;   ................................
;;   176            | 10110000
;;   ................................
;;   177            | 10110001
;;   ................................
;;   178            | 10110010
;;   ................................
;;   179            | 10110011
;;   ................................
;;   180            | 10110100
;;   ................................
;;   181            | 10110101
;;   ................................
;;   182            | 10110110
;;   ................................
;;   183            | 10110111
;;   ................................
;;   184            | 10111000
;;   ................................
;;   185            | 10111001
;;   ................................
;;   186            | 10111010
;;   ................................
;;   187            | 10111011
;;   ................................
;;   188            | 10111100
;;   ................................
;;   189            | 10111101
;;   ................................
;;   190            | 10111110
;;   ................................
;;   191            | 10111111
;;   ................................
;;   192            | 11000000
;;   ................................
;;   193            | 11000001
;;   ................................
;;   194            | 11000010
;;   ................................
;;   195            | 11000011
;;   ................................
;;   196            | 11000100
;;   ................................
;;   197            | 11000101
;;   ................................
;;   198            | 11000110
;;   ................................
;;   199            | 11000111
;;   ................................
;;   200            | 11001000
;;   ................................
;;   201            | 11001001
;;   ................................
;;   202            | 11001010
;;   ................................
;;   203            | 11001011
;;   ................................
;;   204            | 11001100
;;   ................................
;;   205            | 11001101
;;   ................................
;;   206            | 11001110
;;   ................................
;;   207            | 11001111
;;   ................................
;;   208            | 11010000
;;   ................................
;;   209            | 11010001
;;   ................................
;;   210            | 11010010
;;   ................................
;;   211            | 11010011
;;   ................................
;;   212            | 11010100
;;   ................................
;;   213            | 11010101
;;   ................................
;;   214            | 11010110
;;   ................................
;;   215            | 11010111
;;   ................................
;;   216            | 11011000
;;   ................................
;;   217            | 11011001
;;   ................................
;;   218            | 11011010
;;   ................................
;;   219            | 11011011
;;   ................................
;;   220            | 11011100
;;   ................................
;;   221            | 11011101
;;   ................................
;;   222            | 11011110
;;   ................................
;;   223            | 11011111
;;   ................................
;;   224            | 11100000
;;   ................................
;;   225            | 11100001
;;   ................................
;;   226            | 11100010
;;   ................................
;;   227            | 11100011
;;   ................................
;;   228            | 11100100
;;   ................................
;;   229            | 11100101
;;   ................................
;;   230            | 11100110
;;   ................................
;;   231            | 11100111
;;   ................................
;;   232            | 11101000
;;   ................................
;;   233            | 11101001
;;   ................................
;;   234            | 11101010
;;   ................................
;;   235            | 11101011
;;   ................................
;;   236            | 11101100
;;   ................................
;;   237            | 11101101
;;   ................................
;;   238            | 11101110
;;   ................................
;;   239            | 11101111
;;   ................................
;;   240            | 11110000
;;   ................................
;;   241            | 11110001
;;   ................................
;;   242            | 11110010
;;   ................................
;;   243            | 11110011
;;   ................................
;;   244            | 11110100
;;   ................................
;;   245            | 11110101
;;   ................................
;;   246            | 11110110
;;   ................................
;;   247            | 11110111
;;   ................................
;;   248            | 11111000
;;   ................................
;;   249            | 11111001
;;   ................................
;;   250            | 11111010
;;   ................................
;;   251            | 11111011
;;   ................................
;;   252            | 11111100
;;   ................................
;;   253            | 11111101
;;   ................................
;;   254            | 11111110
;;   ................................
;;   255            | 11111111
;;   --------------------------------
;; 
;; This alternative prospect's application endows us with a further
;; construe for the decimal-binary relationship, namely, that the
;; decimal values *enumerate* the binary patterns: Any of the 256
;; available decimal integers in the range [0, 255] produces a unique
;; sequence of eight bits, a unique bit pattern or binary pattern.
;; 
;; == A BYTE OSTENDS TWO ENDS ==
;; A byte's octuple constituents are enumerated in conformance with the
;; following forbisen:
;;  
;;   +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;   | 7 | | 6 | | 5 | | 4 | | 3 | | 2 | | 1 | | 0 |
;;   +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;; 
;; Proceeding in a dextraosinistral airt, that is, from the bit at the
;; right-hand end, the position zero (0), to the leftmost bit at the
;; position seven (7), the value encoded by the bit ascends in its
;; magnitude:
;; 
;;   ----------------------------
;;   Bit position | Decimal value
;;   -------------+--------------
;;   0            | 2^0 = 1
;;   ............................
;;   1            | 2^1 = 2
;;   ............................
;;   2            | 2^2 = 4
;;   ............................
;;   3            | 2^3 = 8
;;   ............................
;;   4            | 2^4 = 16
;;   ............................
;;   5            | 2^5 = 32
;;   ............................
;;   6            | 2^6 = 64
;;   ............................
;;   7            | 2^7 = 128
;;   ----------------------------
;; 
;; Resorting to our acquainted diagram, an equiparation to the aboon
;; table governs:
;; 
;;                +-----+ +----+ +----+ +----+ +---+ +---+ +---+ +---+
;;   Bit position |  7  | |  6 | |  5 | |  4 | | 3 | | 2 | | 1 | | 0 |
;;                +-----+ +----+ +----+ +----+ +---+ +---+ +---+ +---+
;;   Decimal val. | 128 | | 64 | | 32 | | 16 | | 8 | | 4 | | 2 | | 1 |
;;                +-----+ +----+ +----+ +----+ +---+ +---+ +---+ +---+
;; 
;; A corollary thereof, the bit producing the smallest value, empight at
;; the position zero (0), is nevend the least significant bit, or,
;; abbreviated, LSB; On the obverse bourne, that bit whose produce
;; contributes the greatest number, that at the position seven (7),
;; bears the agnomination of the most significiant bit, or MSB.
;; 
;; We thus note:
;; 
;;   most significant bit (MSB)
;;     V
;;   +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;   | 7 | | 6 | | 5 | | 4 | | 3 | | 2 | | 1 | | 0 |
;;   +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;                                               ^
;;                       least significant bit (LSB)
;; 
;; The already introduced bit position-value table may be augmented by
;; this little piece of information to:
;; 
;;   ----------------------------
;;   Bit position | Decimal value
;;   -------------+--------------
;;   0 (LSB)      | 2^0 = 1
;;   ............................
;;   1            | 2^1 = 2
;;   ............................
;;   2            | 2^2 = 4
;;   ............................
;;   3            | 2^3 = 8
;;   ............................
;;   4            | 2^4 = 16
;;   ............................
;;   5            | 2^5 = 32
;;   ............................
;;   6            | 2^6 = 64
;;   ............................
;;   7 (MBS)      | 2^7 = 128
;;   ----------------------------
;; 
;; == EVERY OCTET'S BIT CAN BE ADDRESSED BY AN INTEGER IN [0, 7] ==
;; No particular denouement's vallidom, yet a significant fact's
;; reiteration, every octet's bit is capacitated to be addressed by a
;; decimal number in the integral range [0, 7].
;; 
;; == A RULE EXAMPLE: THE DECIMAL NUMBER 30 ==
;; As a prospectus, an elementary cellular automaton rule simple answers
;; to the following inqusition:
;; 
;;   Given a group of three consecutive bits, which single bit value is
;;   produced?
;; 
;; The rule 30, as an example, represents the decimal number 30. This
;; number's binary paregal is tantamount to:
;; 
;;   --------------------------------
;;   Decimal number | Binary (8 bits)
;;   ---------------+----------------
;;   30             | 00011110
;;   --------------------------------
;; 
;; Please note the binary display format which apportions to the most
;; significant bit (MSB), at position 7, the sinistral laterality, while
;; the least significant bit (LBS), with the position 0, resides at the
;; dextral post:
;; 
;;   most significant bit (MSB)
;;     V
;;   +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;   | 0 | | 0 | | 0 | | 1 | | 1 | | 1 | | 1 | | 0 |
;;   +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;                                               ^
;;                       least significant bit (LSB)
;; 
;; A juxtaposing diagram, the same empights the bit indices on the top,
;; the bit values alow, shall be produced:
;; 
;;                most significant bit (MSB)
;;                  V
;;                +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;   Bit position | 7 | | 6 | | 5 | | 4 | | 3 | | 2 | | 1 | | 0 |
;;                +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;   Bit value    | 0 | | 0 | | 0 | | 1 | | 1 | | 1 | | 1 | | 0 |
;;                +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+
;;                                                            ^
;;                                    least significant bit (LSB)
;; 
;; In a tabular apercu, we yield an equivalency:
;; 
;;   ----------------------------------------------
;;   Bit position | Bit value (8 bits) of number 30
;;   -------------+--------------------------------
;;   0 (LSB)      | 0
;;   ..............................................
;;   1            | 1
;;   ..............................................
;;   2            | 1
;;   ..............................................
;;   3            | 1
;;   ..............................................
;;   4            | 1
;;   ..............................................
;;   5            | 0
;;   ..............................................
;;   6            | 0
;;   ..............................................
;;   7 (MSB)      | 0
;;   ----------------------------------------------
;; 
;; == EVERY BIT INDEX FROM THE RANGE [0,7] CORRESPONDS TO THREE BITS ==
;; A binary pattern is comprised of eight (8) bits, and thus eight
;; positions exist, enumerated with the indices zero (0) through
;; inclusive seven (7) --- the integral range [0, 7].
;; 
;; Every position, that is, every of these eight decimal integers in the
;; range [0, 7], naturally, corresponds to a 3-bit binary number:
;; 
;;   -------------------------------
;;   Decimal number | Binary (3 bit)
;;   ---------------+---------------
;;   0              | 000
;;   ...............................
;;   1              | 001
;;   ...............................
;;   2              | 010
;;   ...............................
;;   3              | 011
;;   ...............................
;;   4              | 100
;;   ...............................
;;   5              | 101
;;   ...............................
;;   6              | 110
;;   ...............................
;;   7              | 111
;;   -------------------------------
;; 
;; This postulates that one may address every bit of an 8-bit binary
;; number either by a decimal integer in the range [0, 7] or the
;; equivalent binary form comprehending three bits.
;; 
;; == THE BIT KEY: A DECIMAL BIT INDEX ENCODED AS THREE BITS ==
;; For our encheson, the notions of the decimal value, a commorant in
;; the range [0, 7], and bit positions, siclike spanning this interval
;; [0, 7], constitute an equivalent, begetting the extended table
;; 
;;   -----------------------------------------------------------------
;;   Bit position | Bit pos. as binary | Bit of decimal 30 at position
;;   -------------+--------------------+------------------------------
;;   0            | 000                | 0
;;   .................................................................
;;   1            | 001                | 1
;;   .................................................................
;;   2            | 010                | 1
;;   .................................................................
;;   3            | 011                | 1
;;   .................................................................
;;   4            | 100                | 1
;;   .................................................................
;;   5            | 101                | 0
;;   .................................................................
;;   6            | 110                | 0
;;   .................................................................
;;   7            | 111                | 0
;;   -----------------------------------------------------------------
;; 
;; Its consanguinity inherent in the binary principles embues the
;; bit key with a structure cognate to the 8-bit binary pattern:
;; 
;;    the most significant bit (MSB)
;;     V
;;   +---+ +---+ +---+
;;   | 2 | | 1 | | 0 |
;;   +---+ +---+ +---+
;;                 ^
;;                the least significant bit (LSB)
;; 
;; == THE BIT KEY: THREE BITS TO ADDRESS AN 8-BIT PATTERN'S BIT ==
;; We are capable of the sinistral column's, "Bit position", elision
;; without the loss of information, as the center file,
;; "Bit pos. as binary", replicates the former's decimal objects in a
;; 3-bit binary format. Purged of this supererogative first compartment,
;; we obtain:
;; 
;;   --------------------------------------------------
;;   Bit pos. as binary | Bit of decimal 30 at position
;;   -------------------+------------------------------
;;   000                | 0
;;   ..................................................
;;   001                | 1
;;   ..................................................
;;   010                | 1
;;   ..................................................
;;   011                | 1
;;   ..................................................
;;   100                | 1
;;   ..................................................
;;   101                | 0
;;   ..................................................
;;   110                | 0
;;   ..................................................
;;   111                | 0
;;   --------------------------------------------------
;; 
;; From this vista, every bit of the decimal integer 30's 8-bit binary
;; representation can be retrieved unambiguously by a sequence of three
;; bits, each such triad actually the binary representation of one
;; of its eight positions, that is, integers in the interval [0, 7].
;; 
;;   ---------------------------------------
;;   Bit key | Bit of decimal 30 at position
;;   --------+------------------------------
;;   000     | 0
;;   .......................................
;;   001     | 1
;;   .......................................
;;   010     | 1
;;   .......................................
;;   011     | 1
;;   .......................................
;;   100     | 1
;;   .......................................
;;   101     | 0
;;   .......................................
;;   110     | 0
;;   .......................................
;;   111     | 0
;;   ---------------------------------------
;; 
;; Please note how we have simply bartered the acquainted column label
;; "Bit pos. as binary" for the more specific term "Bit key".
;; 
;; A visual tantamount shall ostend the decimal bit position specifier,
;; its 3-bits bit key equivalency, and the addressed bit by any of the
;; twain's mediation in the exemplary binary pattern forming the decimal
;; integer number 30:
;; 
;;             most significant bit (MSB)
;;               V
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;   Bit pos. |  7  | |  6  | |  5  | |  4  | |  3  | |  2  | |  1  | |  0  |
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;   Bit key: | 111 | | 110 | | 101 | | 100 | | 011 | | 010 | | 001 | | 000 |
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;   Bit val. |  0  | |  0  | |  0  | |  1  | |  1  | |  1  | |  1  | |  0  |
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;                                                                       ^
;;                                                least significant bit (LSB)
;; 
;; Our cognoscence about the paregal fungability relation betwixt the
;; bit's index as a decimal integer in the range [0, 7] and its 3-bit
;; key, the bit key, entalents us with the potential to excise the
;; superfluous former element's influence from the aboon presented
;; illustration, iterum defined for the binary pattern of the number 30:
;; 
;;             most significant bit (MSB)
;;               V
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;   Bit key: | 111 | | 110 | | 101 | | 100 | | 011 | | 010 | | 001 | | 000 |
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;   Bit val. |  0  | |  0  | |  0  | |  1  | |  1  | |  1  | |  1  | |  0  |
;;            +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
;;                                                                       ^
;;                                                least significant bit (LSB)
;; 
;; Every bit in a decimal number, here: 30, may be addressed by a
;; combination of three bits, the bit key, actually binary-encoding a
;; decimal index from the interval [0, 7].
;; 
;; == A PARLECUE: FROM BIT INDICES TO BIT KEYS ==
;; The vincula betwixt a decimal number's 8-bit binary representation,
;; its single bit's selection via a decimal index in the range [0, 7],
;; and the 3-bit binary encoding of this index designator, nevend here
;; the "bit key", shall be explicated with stark compendiousness in a
;; precis:
;; 
;;   (1) Three consecutive bits define an integral value in the range
;;       [0, 7].
;;   (2) Every integer value in the range [0, 255] can be represented
;;       by eight bits.
;;   (3) Each bit in this 8-bit binary representation answers to an
;;       index, or position designator, in the range [0, 7].
;;   (4) Thus, each bit of an 8-bit binary sequence is capable of being
;;       addressed by either this [0, 7]-decimal index, or the
;;       tantamount 3-bit equivalent.
;;   (5) Such a 3-bit encoding of the [0, 7]-index shall be norned the
;;       8-bit pattern's "bit key".
;;   (6) In conclusion: A bit key constitutes three bits, encoding a
;;       decimal number in the range [0, 7], the latter represents the
;;       position, or index, of a bit in an 8-bit sequence or pattern.
;; 
;; == ELEMENTARY CELLULAR AUTOMATON RULES: A PARASCEVE ==
;; A parasceuastic linkage betwixt the concept of elementary cellular
;; automaton rules and the insights hitherto derived from our treatise
;; shall yark the reader for coming, more detailed paragraphs:
;; 
;;   (1) An elementary cellular automaton rule, or simply "rule", is a
;;       unique binary pattern composed of eight (8) bits.
;;   (2) Such an 8-bit binary representation maps naturally to an
;;       integer number in the decimal range [0, 255].
;;   (3) The decimal value of an 8-bit binary pattern constitutes a
;;       rule's name. This is known as as a "Wolfram code".
;;   (4) Every bit in the 8-bit binary pattern answers to an index in
;;       the decimal range [0, 7].
;;   (5) Any among these [0, 7]-indices corresponds to a 3-bit binary
;;       number, the bit key.
;;   (6) In conclusion: Every bit of an 8-bit binary pattern, produced
;;       by the rule's decimal name, in fact specifies what happens if
;;       three bits in a particular combination are supplied ---
;;       concretely, which bit will be returned by the rule.
;; 
;; == A RULE PRODUCES FOR A TRIAD OF BITS AN OUTPUT BIT ==
;; Every decimal integer number in the range [0, 255] actually defines a
;; rule: It produces a distinct 8-bit binary pattern. Every bit in this
;; pattern can be addressed by an index, or position, in the range
;; [0, 7]. Every such decimal index can be replicated by a combination
;; of three bits: the bit key. As a corollary, every bit in the 8-bit
;; pattern answers unambiguously to a 3-bit bit key.
;; 
;; Given a rule, designated by the name "rule", where rule is an integer
;; number in the range [0, 255], and a bit key, composed of three bits,
;; the rule responds with a bit of its underlying 8-bit binary pattern.
;; In a more formal diction:
;; 
;;   applyRule (rule : integer in [0, 255], bitKey : bit[3]) => bit
;; 
;; An example shall illustrate this concept:
;; 
;;   Given
;;     - the automaton rule 30, which represents the decimal number 30,
;;       and assumes the binary pattern 00011110,
;;     - and a key bitKey = 101, the same addresses the bit at index 5,
;;   we obtain 0, as it holds:
;;   
;;      Binary value of decimal 30: 00011110
;;                                    ^
;;                                    |
;;                                   The bit at index 5 equals 0.
;;   
;;   A conspection of this result's cause can be derived from the
;;   acquainted bit key table for the decimal number 30, here extended
;;   by the bit position as the decimal equivalent of the 3-bit key.
;;   
;;   ------------------------------------------------------
;;   Bit key | Bit position | Bit of decimal 30 at position
;;   --------+--------------+------------------------------
;;   000     | 0            | 0
;;   ......................................................
;;   001     | 1            | 1
;;   ......................................................
;;   010     | 2            | 1
;;   ......................................................
;;   011     | 3            | 1
;;   ......................................................
;;   100     | 4            | 1
;;   ......................................................
;;   101     | 5            | 0    <--- The output of rule 30
;;                          |           for the bit key 101.
;;   ......................................................
;;   110     | 6            | 0
;;   ......................................................
;;   111     | 7            | 0
;;   ------------------------------------------------------
;;   
;;   The significant line has been emphasized.
;; 
;; Applying such a rule-bitKey combination to some series of input bit
;; triads, a new pattern of bits would be generated. Interestingly, the
;; result displays in the most frequent cases a very attractive weftage.
;; 
;; == A RULE APPLIED TO A SEQUENCE OF LINES PRODUCES A PATTERN ==
;; The fact of a rule's corresponds to three bits, the bit key, with a
;; new bit does neither imbue us with mazement, afflatus, nor a soupcon
;; of thaumaturgy; its most conspectible encheson for alliciency wones
;; in the visual causata to whom it lends a vehicle.
;; 
;; In more concrete diction, if applied to a row of zero-valued bits,
;; the extent of which founds upon one's personal delectation, employing
;; a sliding window of three bits as the bit keys, the thus modified
;; line carries a distinct texture --- produced entirely as a response
;; to the selected automaton rule and the initial line's pattern.
;; 
;; A fortiori, if, in lieu of the incipient line's direct modification,
;; one appends a new zero-valued bit row below the extant one, and
;; enters the rule application results formed by the first line's
;; traversal into this new section, subsequently repeating the process
;; with this second line as the new template for a third expanse, et
;; cetera, the thus spanned rectangular grid ostends a very kenspeckle
;; picture, unique for any of the 256 possible rules.
;; 
;; This process, molded into the superior stringency and formality of a
;; gradual explication, shall be limned in the following:
;; 
;;   (0) SELECT BIT SOURCE AND ELEMENTARY CELLULAR AUTOMATON RULE:
;;       (0.1) Prepare a line composed of N bits, the cells initialized
;;             to some inchoate state from the set of bits {0, 1}. Most
;;             commonly, the center cell is set to 1, while all
;;             surrounding units assume the value 0. As a forbisen, a
;;             horizontal expanse compact of five cells (N=5), would
;;             assume the guise:
;;               0 0 1 0 0
;;                   ^
;;                   |_ Only the middle cells contains 1, all others 0.
;;             
;;             The first line provides the incipiency for an arbitrary
;;             tally of lines to follow, designated in their entirety,
;;             including the first row, by a number H >= 0.
;;             
;;             A more imperious terminology, paravaunt in its kinship to
;;             the biological ambitus, parleys of the lines, or
;;             sequences of consecutive bit patterns in general, as
;;             "generations", the incipiency of which the zero-th
;;             specimen. In this disquisition at hand, however, we
;;             prefer the visual intimiations of the geometrical
;;             diction.
;;             
;;             The line at the zero-based index h, with 0 <= h < H, is
;;             denoted via
;;               lines[h]
;;             
;;             A specific bit of the h-th line, again zero-indexed, is
;;             referenced by a subscript i, with 0 <= i <= N, and
;;             denoted as
;;               bits[h][i]
;;       
;;       (0.2) Select a cellular automaton rule "rule", where "rule"
;;             constitutes an integer number in the range [0, 255].
;;   
;;   (1) SET CURSOR TO FIRST LINE:
;;       Set the cursor, or index, h for the following stages to 0,
;;       designating the incipient row:
;;         h <- 0
;;   
;;   (2) ITERATE THROUGH ALL SOURCE BITS, FORMING A BIT KEY FOR EACH:
;;       In the current line, lines[h], at the processed line index h,
;;       iterate with a variable i from 0 to (N-1); and for each bit
;;       bits[h][i], form a bit key composed of three bits, the bitKey:
;;         bits[h][i]   provides the center,
;;         bits[h][i-1] its sinistral neighbor cell, and
;;         bits[h][i+1] the dextral bit,
;;       resulting in the triad sequence:
;;         bitKey <- (bits[h][i-1], bits[h][i], bits[h][i+1])
;;       As an apostil:
;;         bits[h][i-1] accommodates the most significant bit
;;         bits[h][i]   accommodates the next lower bit
;;         bits[h][i+1] accommodates the least significant bit
;;       of the treble.
;;       
;;       The following diagram shall administer a cursory nortelry anent
;;       the bit key's design:
;;       
;;         the left neighbor, located at the most significant bit (MSB)
;;                 |
;;                 |       the processed cell
;;                 |               |               
;;                 |               |     the right neighbor, located at
;;                 |               |     the least significant bit (LSB)
;;                 |               |               |
;;                 V               V               V
;;          +--------------+ +------------+ +--------------+
;;          | bits[h][i-1] | | bits[h][i] | | bits[h][i+1] |
;;          +--------------+ +------------+ +--------------+
;;       
;;       If any of the two neighbors, bits[h][i-1] or bits[h][i+1],
;;       specify an invalid position, one outside of the line's bournes,
;;       any of three possible actions may be adhibited:
;;       
;;         (a) DEFAULT VALUE:
;;             The outlying cell is substituted by a constant bit,
;;             commonly 0.
;;         (b) WRAPPING AROUND:
;;             The outlying cell "wraps around" and assumes the bit on
;;             the opposite end of the line:
;;               bits[h][i-1], the left neighbor, thus acquires the bit
;;                             located at the end of the line:
;;                               bits[h][N-1].
;;               bits[h][i+1], the right neighbor, appropriates the bit
;;                             at the start of the line:
;;                               bits[h][0].
;;         (c) OMISSION:
;;             Such an ambiguous case is simply obviated by commencing
;;             and concluding the iteration process one cell before the
;;             bournes, that is, the first and the last line cell are
;;             not processed, retained verbatim in the next line.
;;             In lieu of iterating with i from 0 to (N-1), for a line
;;             of N cells, one progresses from 1 to (N-2),
;;   
;;   (3) APPLY RULE TO THE CURRENT LINE, BIT FOR BIT:
;;       Query the rule "rule" for the bit which it associates with the
;;       3-bit combination bitKey --- the same is tantamount to the
;;       decimal integer index among the rule's binary pattern for said
;;       key:
;;         outputBit <- applyRule(rule, bitKey),
;;        where
;;         rule   --- the rule, identified by its decimal number in the
;;                    integral [0, 255]
;;         bitKey --- the three-bit bitKey, actually referencing in its
;;                    decimal equivalency the bit position into the
;;                    "rule"'s binary pattern.
;;   
;;   (4) WRITE RULE OUTPUT TO LINE BELOW:
;;       Write the outputBit, computed for the i-th cell of the current
;;       line, to the i-th cell of the next line, which is located at
;;       the row position (h+1), that is, the cell immediately below the
;;       currently iterated line bitKey's center bit:
;;         bits[h+1][i] <- outputBut
;;       In a visually more expressive manner:
;;       
;;                      +--------------+--------------+--------------+
;;         lines[h]:    | bits[h][i-1] |  bits[h][i]  | bits[h][i+1] |
;;                      +--------------+--------------+--------------+
;;                                            |
;;                                            | write in cell below, to
;;                                            | bits[h+1][i]
;;                                            | 
;;                                            V
;;                                     +--------------+
;;                                     |  outputBit   |
;;         lines[h+1]:                 |..............|
;;                                     | bits[h+1][i] |
;;                                     +--------------+
;;   
;;   (5) REPEAT FOR NEXT LINE:
;;       Repeat this process, starting at the stage (2), for the next
;;       line by setting the current line index h to (h+1):
;;         h <- h + 1
;;       Perpetuate these steps (2) through (5) until h reaches the
;;       desinent line index (H-1), that is, until:
;;         h >= H - 1
;; 
;; == THE CELLS MAY BE ORNAMENTED IN AN ARBITRARY FASHION ==
;; The stringency of its mathematical substrate, the supputations
;; implemented in the realm of bits as members desumed from the integral
;; set {0, 1}, does not impose a requisitum for the aesthetical aspects.
;; 
;; Concretely, one is not obliged to display the rectangular grid of
;; generations in the verbatim 0/1 form, and may substitute a zero (0)
;; bit by any symbol or graphical element produced by the personal
;; deliberations, acting similiter for the one (1) bit.
;; 
;; Given, as a forbisen, the rule 30 output for 24 generations, the
;; actual numeric presentation would encompass:
;; 
;;   0000000000000000000000001000000000000000000000000
;;   0000000000000000000000011100000000000000000000000
;;   0000000000000000000000110010000000000000000000000
;;   0000000000000000000001101111000000000000000000000
;;   0000000000000000000011001000100000000000000000000
;;   0000000000000000000110111101110000000000000000000
;;   0000000000000000001100100001001000000000000000000
;;   0000000000000000011011110011111100000000000000000
;;   0000000000000000110010001110000010000000000000000
;;   0000000000000001101111011001000111000000000000000
;;   0000000000000011001000010111101100100000000000000
;;   0000000000000110111100110100001011110000000000000
;;   0000000000001100100011100110011010001000000000000
;;   0000000000011011110110011101110011011100000000000
;;   0000000000110010000101110001001110010010000000000
;;   0000000001101111001101001011111001111111000000000
;;   0000000011001000111001111010000111000000100000000
;;   0000000110111101100111000011001100100001110000000
;;   0000001100100001011100100110111011110011001000000
;;   0000011011110011010011111100100010001110111100000
;;   0000110010001110011110000011110111011000100010000
;;   0001101111011001110001000110000100010101110111000
;;   0011001000010111001011101101001110110101000100100
;;   0110111100110100111010001001111000100101101111110
;; 
;; Maugre its lealty to the arithmetical background, the mete of the
;; expressed pulchritude is encumbered with peccability in regards to
;; any notion. If, on the other hand, we commit to the following
;; substitutions:
;; 
;;   -------------------------
;;   Bit value | Output symbol
;;   ----------+--------------
;;   0         | (space)
;;   .........................
;;   1         | #
;;   -------------------------
;; 
;; this more farand visual response is elicited:
;; 
;;                           #                        
;;                          ###                       
;;                         ##  #                      
;;                        ## ####                     
;;                       ##  #   #                    
;;                      ## #### ###                   
;;                     ##  #    #  #                  
;;                    ## ####  ######                 
;;                   ##  #   ###     #                
;;                  ## #### ##  #   ###               
;;                 ##  #    # #### ##  #              
;;                ## ####  ## #    # ####             
;;               ##  #   ###  ##  ## #   #            
;;              ## #### ##  ### ###  ## ###           
;;             ##  #    # ###   #  ###  #  #          
;;            ## ####  ## #  # #####  #######         
;;           ##  #   ###  #### #    ###      #        
;;          ## #### ##  ###    ##  ##  #    ###       
;;         ##  #    # ###  #  ## ### ####  ##  #      
;;        ## ####  ## #  ######  #   #   ### ####     
;;       ##  #   ###  ####     #### ### ##   #   #    
;;      ## #### ##  ###   #   ##    #   # # ### ###   
;;     ##  #    # ###  # ### ## #  ### ## # #   #  #  
;;    ## ####  ## #  ### #   #  ####   #  # ## ###### 
;; 
;; == DEFINITIONS ==
;; In a rather withershins procession, the diorisms and contexts of
;; terms and concepts appertaining to the elementary cellular automaton
;; shall now be introduced as the practical segment's coda.
;; 
;; == CELLULAR AUTOMATON: A GRID OF CELLS ==
;; A *cellular automaton*, abbreviated as CA, furnishes a computational
;; model, discrete and abstract in its nature, occupying a grid, regular
;; in its ordonnance, and inhabited by cells capacitated to assume a
;; finite tally of states.
;; 
;; == A CELL IS DEFINED BY ITS STATE ==
;; At any instant woning in exactly one state, a cell's competence
;; amplects a transition into another from the recognized set through
;; a champarty of its vincula with its neighbors and the automaton rule.
;; 
;; In a very simple case, a dichotomy in conformance with binary values
;; exhausts the complete state roster.
;; 
;; == CELLS REACT TO THEIR NEIGHBORHOOD ==
;; Besides its state, a second proprium applying to a cell is realized
;; in its neighborhood, a certain perimeter comprehending the cells in
;; its vicinage, the selection of which is governed by some regulation,
;; and whose affluence serves as a contribution to a cell state's
;; development.
;; 
;; == A RULE MODIFIES A CELL BASED UPON ITS STATE AND NEIGHBORHOOD ==
;; A compernage to the neighborhood, the indicium of most peisant
;; influence constitutes the cellular automaton rule, or simply rule,
;; to whom the competence is imparted to supputate for a cell, founded
;; upon its current state and its neighboring cells, a new state,
;; intended to be assigned to the perquired cell in a subsequent stage
;; and override its contemporaneous content.
;; 
;; == GENERATIONS: ALL CELLS MODIFIED BY THE RULE INTO NEW STATES ==
;; Proceeding from an initial state allotted to each cell, any of these
;; units develops, in respondency to its neighborhood and the substrate
;; rule, into a new generation, having acquired a modulated state.
;; 
;; == ELEMENTARY CELLULAR AUTOMATON: 1 DIMENSION, 2 BITS, 3 NEIGHBORS ==
;; A polymechany, both for its allicient and its theoretical vallidom,
;; by Stephen Wolfram, the elementary cellular automaton employs a
;; one-dimensional arrangement --- a line --- of cells.
;; 
;; Each such unit's state desumes its diorism from the binary type, that
;; is, an integral value from the range {0, 1}, usually initialized to
;; zero (0) at the process' inchoation.
;; 
;; A cell's neighborhood embraces a very scanty perimeter, considering
;; merely its accolent sinistral peer, and the comperange immediately at
;; its dextral laterality, thus forming a triple of bits.
;; 
;; The most kenspeckle proprium of this automaton species, the rules are
;; encoded in octets, and enumerated by mediation of their decimal
;; integer equivalents, spanning the range [0, 255], and, as a
;; corollary, entailing a cardinality of 256 members. The nomenclature
;; employed in the assignment of the decimal formulation to the 8-bit
;; binary pattern is known as the "Wolfram code". Following this scheme,
;; as an example adduced, the bit pattern "00011110", tantamount to the
;; integral number 30, acquires the name "rule 30".
;; 
;; An elementary cellular automaton rule, nourished with a cell and its
;; neighborhood, produces an aefauld bit, representing the considered
;; cell's new state.
;; 
;; The cell addressed for modification and its neighboring jumelle,
;; demarcated in their states by a bit each, in conjunction form a 3-bit
;; binary sequence, which we may neven the "bit key". Such an intrined
;; base-2 compound naturally corresponds to an integer object in the
;; interval [0, 7].
;; 
;; Accoutred with these parcels of cognescence --- a rule qua an 8-bit
;; pattern, and a cell represented by a triad of itself and the accolent
;; near-dwellers ---, the nexus governing their interplay is rendered
;; conspicable: A rule consists of eight bits, indexed with integers in
;; the range [0, 7]; the 3-bit bit key, begotten by a processed cell's
;; vicinage, siclike reproduces this integral interval, and actually
;; defines an index into the rule, selecting and returning the bit at
;; the respective location. This bit designates the cell's next state.
;; 
;; An purlicue shall serve in the administration of the requisite
;; nortelry concerning the elementary cellular automaton's indicia:
;; 
;;   ------------------------------------------------------------------
;;   Criterion    | Elementary cellular automaton
;;   -------------+----------------------------------------------------
;;   Layout       | One-dimensional line.
;;   ..................................................................
;;   Cell states  | Binary (bits): {0, 1}.
;;   ..................................................................
;;   Neighborhood | Three cells: the cell immediately to the left and
;;                | that bordering to the right of the perquired cell.
;;   ------------------------------------------------------------------
;; 
;; == RULE TABLE ==
;; A tabular apercu concerning the 256 different elementary cellular
;; automata rules, in conjunction with their respondency to the eight
;; possible bit keys, shall be the following presentation's cynosure:
;; 
;;   ----------------------------------------------------------------
;;        |          |              Output for bit key              
;;   Rule | Pattern  | 111 | 110 | 101 | 100 | 011 | 010 | 001 | 000
;;   -----+----------+-----+-----+-----+-----+-----+-----+-----+-----
;;   0    | 00000000 |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   1    | 00000001 |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   2    | 00000010 |  0  |  0  |  0  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   3    | 00000011 |  0  |  0  |  0  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   4    | 00000100 |  0  |  0  |  0  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   5    | 00000101 |  0  |  0  |  0  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   6    | 00000110 |  0  |  0  |  0  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   7    | 00000111 |  0  |  0  |  0  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   8    | 00001000 |  0  |  0  |  0  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   9    | 00001001 |  0  |  0  |  0  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   10   | 00001010 |  0  |  0  |  0  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   11   | 00001011 |  0  |  0  |  0  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   12   | 00001100 |  0  |  0  |  0  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   13   | 00001101 |  0  |  0  |  0  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   14   | 00001110 |  0  |  0  |  0  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   15   | 00001111 |  0  |  0  |  0  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   16   | 00010000 |  0  |  0  |  0  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   17   | 00010001 |  0  |  0  |  0  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   18   | 00010010 |  0  |  0  |  0  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   19   | 00010011 |  0  |  0  |  0  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   20   | 00010100 |  0  |  0  |  0  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   21   | 00010101 |  0  |  0  |  0  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   22   | 00010110 |  0  |  0  |  0  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   23   | 00010111 |  0  |  0  |  0  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   24   | 00011000 |  0  |  0  |  0  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   25   | 00011001 |  0  |  0  |  0  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   26   | 00011010 |  0  |  0  |  0  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   27   | 00011011 |  0  |  0  |  0  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   28   | 00011100 |  0  |  0  |  0  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   29   | 00011101 |  0  |  0  |  0  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   30   | 00011110 |  0  |  0  |  0  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   31   | 00011111 |  0  |  0  |  0  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   32   | 00100000 |  0  |  0  |  1  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   33   | 00100001 |  0  |  0  |  1  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   34   | 00100010 |  0  |  0  |  1  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   35   | 00100011 |  0  |  0  |  1  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   36   | 00100100 |  0  |  0  |  1  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   37   | 00100101 |  0  |  0  |  1  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   38   | 00100110 |  0  |  0  |  1  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   39   | 00100111 |  0  |  0  |  1  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   40   | 00101000 |  0  |  0  |  1  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   41   | 00101001 |  0  |  0  |  1  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   42   | 00101010 |  0  |  0  |  1  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   43   | 00101011 |  0  |  0  |  1  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   44   | 00101100 |  0  |  0  |  1  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   45   | 00101101 |  0  |  0  |  1  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   46   | 00101110 |  0  |  0  |  1  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   47   | 00101111 |  0  |  0  |  1  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   48   | 00110000 |  0  |  0  |  1  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   49   | 00110001 |  0  |  0  |  1  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   50   | 00110010 |  0  |  0  |  1  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   51   | 00110011 |  0  |  0  |  1  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   52   | 00110100 |  0  |  0  |  1  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   53   | 00110101 |  0  |  0  |  1  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   54   | 00110110 |  0  |  0  |  1  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   55   | 00110111 |  0  |  0  |  1  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   56   | 00111000 |  0  |  0  |  1  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   57   | 00111001 |  0  |  0  |  1  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   58   | 00111010 |  0  |  0  |  1  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   59   | 00111011 |  0  |  0  |  1  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   60   | 00111100 |  0  |  0  |  1  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   61   | 00111101 |  0  |  0  |  1  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   62   | 00111110 |  0  |  0  |  1  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   63   | 00111111 |  0  |  0  |  1  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   64   | 01000000 |  0  |  1  |  0  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   65   | 01000001 |  0  |  1  |  0  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   66   | 01000010 |  0  |  1  |  0  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   67   | 01000011 |  0  |  1  |  0  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   68   | 01000100 |  0  |  1  |  0  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   69   | 01000101 |  0  |  1  |  0  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   70   | 01000110 |  0  |  1  |  0  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   71   | 01000111 |  0  |  1  |  0  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   72   | 01001000 |  0  |  1  |  0  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   73   | 01001001 |  0  |  1  |  0  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   74   | 01001010 |  0  |  1  |  0  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   75   | 01001011 |  0  |  1  |  0  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   76   | 01001100 |  0  |  1  |  0  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   77   | 01001101 |  0  |  1  |  0  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   78   | 01001110 |  0  |  1  |  0  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   79   | 01001111 |  0  |  1  |  0  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   80   | 01010000 |  0  |  1  |  0  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   81   | 01010001 |  0  |  1  |  0  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   82   | 01010010 |  0  |  1  |  0  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   83   | 01010011 |  0  |  1  |  0  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   84   | 01010100 |  0  |  1  |  0  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   85   | 01010101 |  0  |  1  |  0  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   86   | 01010110 |  0  |  1  |  0  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   87   | 01010111 |  0  |  1  |  0  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   88   | 01011000 |  0  |  1  |  0  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   89   | 01011001 |  0  |  1  |  0  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   90   | 01011010 |  0  |  1  |  0  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   91   | 01011011 |  0  |  1  |  0  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   92   | 01011100 |  0  |  1  |  0  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   93   | 01011101 |  0  |  1  |  0  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   94   | 01011110 |  0  |  1  |  0  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   95   | 01011111 |  0  |  1  |  0  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   96   | 01100000 |  0  |  1  |  1  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   97   | 01100001 |  0  |  1  |  1  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   98   | 01100010 |  0  |  1  |  1  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   99   | 01100011 |  0  |  1  |  1  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   100  | 01100100 |  0  |  1  |  1  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   101  | 01100101 |  0  |  1  |  1  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   102  | 01100110 |  0  |  1  |  1  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   103  | 01100111 |  0  |  1  |  1  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   104  | 01101000 |  0  |  1  |  1  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   105  | 01101001 |  0  |  1  |  1  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   106  | 01101010 |  0  |  1  |  1  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   107  | 01101011 |  0  |  1  |  1  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   108  | 01101100 |  0  |  1  |  1  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   109  | 01101101 |  0  |  1  |  1  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   110  | 01101110 |  0  |  1  |  1  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   111  | 01101111 |  0  |  1  |  1  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   112  | 01110000 |  0  |  1  |  1  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   113  | 01110001 |  0  |  1  |  1  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   114  | 01110010 |  0  |  1  |  1  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   115  | 01110011 |  0  |  1  |  1  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   116  | 01110100 |  0  |  1  |  1  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   117  | 01110101 |  0  |  1  |  1  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   118  | 01110110 |  0  |  1  |  1  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   119  | 01110111 |  0  |  1  |  1  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   120  | 01111000 |  0  |  1  |  1  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   121  | 01111001 |  0  |  1  |  1  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   122  | 01111010 |  0  |  1  |  1  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   123  | 01111011 |  0  |  1  |  1  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   124  | 01111100 |  0  |  1  |  1  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   125  | 01111101 |  0  |  1  |  1  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   126  | 01111110 |  0  |  1  |  1  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   127  | 01111111 |  0  |  1  |  1  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   128  | 10000000 |  1  |  0  |  0  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   129  | 10000001 |  1  |  0  |  0  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   130  | 10000010 |  1  |  0  |  0  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   131  | 10000011 |  1  |  0  |  0  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   132  | 10000100 |  1  |  0  |  0  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   133  | 10000101 |  1  |  0  |  0  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   134  | 10000110 |  1  |  0  |  0  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   135  | 10000111 |  1  |  0  |  0  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   136  | 10001000 |  1  |  0  |  0  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   137  | 10001001 |  1  |  0  |  0  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   138  | 10001010 |  1  |  0  |  0  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   139  | 10001011 |  1  |  0  |  0  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   140  | 10001100 |  1  |  0  |  0  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   141  | 10001101 |  1  |  0  |  0  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   142  | 10001110 |  1  |  0  |  0  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   143  | 10001111 |  1  |  0  |  0  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   144  | 10010000 |  1  |  0  |  0  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   145  | 10010001 |  1  |  0  |  0  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   146  | 10010010 |  1  |  0  |  0  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   147  | 10010011 |  1  |  0  |  0  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   148  | 10010100 |  1  |  0  |  0  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   149  | 10010101 |  1  |  0  |  0  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   150  | 10010110 |  1  |  0  |  0  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   151  | 10010111 |  1  |  0  |  0  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   152  | 10011000 |  1  |  0  |  0  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   153  | 10011001 |  1  |  0  |  0  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   154  | 10011010 |  1  |  0  |  0  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   155  | 10011011 |  1  |  0  |  0  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   156  | 10011100 |  1  |  0  |  0  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   157  | 10011101 |  1  |  0  |  0  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   158  | 10011110 |  1  |  0  |  0  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   159  | 10011111 |  1  |  0  |  0  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   160  | 10100000 |  1  |  0  |  1  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   161  | 10100001 |  1  |  0  |  1  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   162  | 10100010 |  1  |  0  |  1  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   163  | 10100011 |  1  |  0  |  1  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   164  | 10100100 |  1  |  0  |  1  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   165  | 10100101 |  1  |  0  |  1  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   166  | 10100110 |  1  |  0  |  1  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   167  | 10100111 |  1  |  0  |  1  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   168  | 10101000 |  1  |  0  |  1  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   169  | 10101001 |  1  |  0  |  1  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   170  | 10101010 |  1  |  0  |  1  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   171  | 10101011 |  1  |  0  |  1  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   172  | 10101100 |  1  |  0  |  1  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   173  | 10101101 |  1  |  0  |  1  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   174  | 10101110 |  1  |  0  |  1  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   175  | 10101111 |  1  |  0  |  1  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   176  | 10110000 |  1  |  0  |  1  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   177  | 10110001 |  1  |  0  |  1  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   178  | 10110010 |  1  |  0  |  1  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   179  | 10110011 |  1  |  0  |  1  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   180  | 10110100 |  1  |  0  |  1  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   181  | 10110101 |  1  |  0  |  1  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   182  | 10110110 |  1  |  0  |  1  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   183  | 10110111 |  1  |  0  |  1  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   184  | 10111000 |  1  |  0  |  1  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   185  | 10111001 |  1  |  0  |  1  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   186  | 10111010 |  1  |  0  |  1  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   187  | 10111011 |  1  |  0  |  1  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   188  | 10111100 |  1  |  0  |  1  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   189  | 10111101 |  1  |  0  |  1  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   190  | 10111110 |  1  |  0  |  1  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   191  | 10111111 |  1  |  0  |  1  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   192  | 11000000 |  1  |  1  |  0  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   193  | 11000001 |  1  |  1  |  0  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   194  | 11000010 |  1  |  1  |  0  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   195  | 11000011 |  1  |  1  |  0  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   196  | 11000100 |  1  |  1  |  0  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   197  | 11000101 |  1  |  1  |  0  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   198  | 11000110 |  1  |  1  |  0  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   199  | 11000111 |  1  |  1  |  0  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   200  | 11001000 |  1  |  1  |  0  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   201  | 11001001 |  1  |  1  |  0  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   202  | 11001010 |  1  |  1  |  0  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   203  | 11001011 |  1  |  1  |  0  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   204  | 11001100 |  1  |  1  |  0  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   205  | 11001101 |  1  |  1  |  0  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   206  | 11001110 |  1  |  1  |  0  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   207  | 11001111 |  1  |  1  |  0  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   208  | 11010000 |  1  |  1  |  0  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   209  | 11010001 |  1  |  1  |  0  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   210  | 11010010 |  1  |  1  |  0  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   211  | 11010011 |  1  |  1  |  0  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   212  | 11010100 |  1  |  1  |  0  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   213  | 11010101 |  1  |  1  |  0  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   214  | 11010110 |  1  |  1  |  0  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   215  | 11010111 |  1  |  1  |  0  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   216  | 11011000 |  1  |  1  |  0  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   217  | 11011001 |  1  |  1  |  0  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   218  | 11011010 |  1  |  1  |  0  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   219  | 11011011 |  1  |  1  |  0  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   220  | 11011100 |  1  |  1  |  0  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   221  | 11011101 |  1  |  1  |  0  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   222  | 11011110 |  1  |  1  |  0  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   223  | 11011111 |  1  |  1  |  0  |  1  |  1  |  1  |  1  |  1
;;   ................................................................
;;   224  | 11100000 |  1  |  1  |  1  |  0  |  0  |  0  |  0  |  0
;;   ................................................................
;;   225  | 11100001 |  1  |  1  |  1  |  0  |  0  |  0  |  0  |  1
;;   ................................................................
;;   226  | 11100010 |  1  |  1  |  1  |  0  |  0  |  0  |  1  |  0
;;   ................................................................
;;   227  | 11100011 |  1  |  1  |  1  |  0  |  0  |  0  |  1  |  1
;;   ................................................................
;;   228  | 11100100 |  1  |  1  |  1  |  0  |  0  |  1  |  0  |  0
;;   ................................................................
;;   229  | 11100101 |  1  |  1  |  1  |  0  |  0  |  1  |  0  |  1
;;   ................................................................
;;   230  | 11100110 |  1  |  1  |  1  |  0  |  0  |  1  |  1  |  0
;;   ................................................................
;;   231  | 11100111 |  1  |  1  |  1  |  0  |  0  |  1  |  1  |  1
;;   ................................................................
;;   232  | 11101000 |  1  |  1  |  1  |  0  |  1  |  0  |  0  |  0
;;   ................................................................
;;   233  | 11101001 |  1  |  1  |  1  |  0  |  1  |  0  |  0  |  1
;;   ................................................................
;;   234  | 11101010 |  1  |  1  |  1  |  0  |  1  |  0  |  1  |  0
;;   ................................................................
;;   235  | 11101011 |  1  |  1  |  1  |  0  |  1  |  0  |  1  |  1
;;   ................................................................
;;   236  | 11101100 |  1  |  1  |  1  |  0  |  1  |  1  |  0  |  0
;;   ................................................................
;;   237  | 11101101 |  1  |  1  |  1  |  0  |  1  |  1  |  0  |  1
;;   ................................................................
;;   238  | 11101110 |  1  |  1  |  1  |  0  |  1  |  1  |  1  |  0
;;   ................................................................
;;   239  | 11101111 |  1  |  1  |  1  |  0  |  1  |  1  |  1  |  1
;;   ................................................................
;;   240  | 11110000 |  1  |  1  |  1  |  1  |  0  |  0  |  0  |  0
;;   ................................................................
;;   241  | 11110001 |  1  |  1  |  1  |  1  |  0  |  0  |  0  |  1
;;   ................................................................
;;   242  | 11110010 |  1  |  1  |  1  |  1  |  0  |  0  |  1  |  0
;;   ................................................................
;;   243  | 11110011 |  1  |  1  |  1  |  1  |  0  |  0  |  1  |  1
;;   ................................................................
;;   244  | 11110100 |  1  |  1  |  1  |  1  |  0  |  1  |  0  |  0
;;   ................................................................
;;   245  | 11110101 |  1  |  1  |  1  |  1  |  0  |  1  |  0  |  1
;;   ................................................................
;;   246  | 11110110 |  1  |  1  |  1  |  1  |  0  |  1  |  1  |  0
;;   ................................................................
;;   247  | 11110111 |  1  |  1  |  1  |  1  |  0  |  1  |  1  |  1
;;   ................................................................
;;   248  | 11111000 |  1  |  1  |  1  |  1  |  1  |  0  |  0  |  0
;;   ................................................................
;;   249  | 11111001 |  1  |  1  |  1  |  1  |  1  |  0  |  0  |  1
;;   ................................................................
;;   250  | 11111010 |  1  |  1  |  1  |  1  |  1  |  0  |  1  |  0
;;   ................................................................
;;   251  | 11111011 |  1  |  1  |  1  |  1  |  1  |  0  |  1  |  1
;;   ................................................................
;;   252  | 11111100 |  1  |  1  |  1  |  1  |  1  |  1  |  0  |  0
;;   ................................................................
;;   253  | 11111101 |  1  |  1  |  1  |  1  |  1  |  1  |  0  |  1
;;   ................................................................
;;   254  | 11111110 |  1  |  1  |  1  |  1  |  1  |  1  |  1  |  0
;;   ................................................................
;;   255  | 11111111 |  1  |  1  |  1  |  1  |  1  |  1  |  1  |  1
;;   ----------------------------------------------------------------
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
;; enjoying — a more serious enterprise certainly would be assayed in an
;; athwart airt.
;; 
;;   ------------------------------------------------------------------
;;   No. | File                 | Purpose
;;   ------------------------------------------------------------------
;;    1  | types.lisp           | Defines the custom types deployed in
;;       |                      | the program and utilized by the
;;       |                      | subsequent project files, including,
;;       |                      | among others, ``list-of'', ``octet'',
;;       |                      | etc.
;;   ..................................................................
;;    2  | command.lisp         | Encompasses implementations of the
;;       |                      | Celum commands as classes.
;;   ..................................................................
;;    3  | commandIterator.lisp | Implements an interpreter over a list
;;       |                      | of Celum commands.
;;   ..................................................................
;;    4  | line.lisp            | Implements the program lines as
;;       |                      | objects akin to a node in a linked
;;       |                      | list.
;;   ..................................................................
;;    5  | program.lisp         | Realizes an executable Celum
;;       |                      | program as a linked list of lines.
;;   ..................................................................
;;    6  | lineSearch.lisp      | Furnishes operations for the
;;       |                      | detection of lines in a Celum program
;;       |                      | by certain criteria, such as the
;;       |                      | prefix bit or a label identifier.
;;   ..................................................................
;;    7  | lexer.lisp           | Provides a lexical analyzer, or
;;       |                      | lexer, the duty of which comprehends
;;       |                      | the detection and extraction of
;;       |                      | significant objects from a piece of
;;       |                      | Celum source code.
;;   ..................................................................
;;    8  | parser.lisp          | Implements the parser, assigned the
;;       |                      | wike of producing from a piece of
;;       |                      | Celum source code, aided therein by
;;       |                      | the lexer, an executable program.
;;   ..................................................................
;;    9  | ecAutomaton.lisp     | Assigned the onus of elementary
;;       |                      | cellular automaton's foundational
;;       |                      | principles' dation.
;;   ..................................................................
;;   10  | tape.lisp            | Provides the infinite tape of
;;       |                      | bit-valued cells.
;;   ..................................................................
;;   11  | input.lisp           | Implements the input buffer, an
;;       |                      | adminicle for the bit-based, buffered
;;       |                      | consumption of user input.
;;   ..................................................................
;;   12  | output.lisp          | Implements the output buffer, an
;;       |                      | adminicle for the bit-based, buffered
;;       |                      | printing to the standard output.
;;   ..................................................................
;;   13  | conditions.lisp      | Specifies the conditions appertaining
;;       |                      | to a Celum program's evaluation, in
;;       |                      | particular the error type
;;       |                      | ``Missing-Prefix-Line-Error''.
;;   ..................................................................
;;   14  | interpreter.lisp     | Accommodates the Celum interpreter's
;;       |                      | manifestation.
;;   ..................................................................
;;   15  | tests.lisp           | Accoutres the interpreter test cases.
;;   ..................................................................
;;   16  | main.lisp            | Establishes an entry point into the
;;       |                      | project by loading the requisite
;;       |                      | Common Lisp source files elucidated
;;       |                      | above in their correct order.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-11
;; 
;; Sources:
;;   [asu2018onedeca]
;;   ASU School of Earth and Space, Arizona State University,
;;     "1D Elementary Cellular Automata (1D ECA)",
;;     2018
;;   URL: "https://elife-asu.github.io/wss-modules/modules/
;;         1-1d-cellular-automata/"
;;   
;;   [esolang2006Celum]
;;   The Esolang contributors, "Celum", October 22nd, 2006
;;   URL: "https://esolangs.org/wiki/Celum"
;;   
;;   [oeis2021indexeca]
;;   The The On-Line Encyclopedia of Integer Sequences (OEIS) Wiki
;;   contributors,
;;     "Index to Elementary Cellular Automata",
;;     December 17th, 2021
;;   URL: "https://oeis.org/wiki/Index_to_Elementary_Cellular_Automata"
;;   
;;   [rosettaCode2023elemcellauto]
;;   The Rosetta Code contributors, "Elementary cellular automaton",
;;     September 8th, 2023
;;   URL: "https://www.rosettacode.org/wiki/
;;         Elementary_cellular_automaton"
;;   
;;   [shiffman2012natureofcode]
;;   Daniel Shiffman, "The Nature of Code", 2012
;;   URL: "https://natureofcode.com/book/chapter-7-cellular-automata/"
;;   
;;   [standfordencyclopedia2017cellularauto]
;;   The Stanford Encyclopedia of Philosophy,
;;     "Cellular Automata (Stanford Encyclopedia of Philosophy)",
;;     August 22nd, 2017
;;   URL: "https://plato.stanford.edu/entries/cellular-automata/"
;;   
;;   [standfordencyclopedia2017supplcellauto]
;;   The Stanford Encyclopedia of Philosophy,
;;     "Cellular Automata > The 256 Rules (Stanford Encyclopedia of Philosophy)",
;;     2017
;;   URL: "https://plato.stanford.edu/entries/cellular-automata/
;;         supplement.html"
;;   
;;   [weisstein2023elemcellauto]
;;   Eric W. Weisstein, "Elementary Cellular Automaton",
;;     From "MathWorld"--A Wolfram Web Resource,
;;     2023
;;   URL: "https://mathworld.wolfram.com/
;;         ElementaryCellularAutomaton.html"
;;   
;;   [wikipedia2023cellularautomaton]
;;   The Wikipedia contributors, "Cellular Automaton", October 4th, 2023
;;   URL: "https://en.wikipedia.org/wiki/Cellular_automaton"
;;   
;;   [wolfram2002newkindofscience]
;;   Stephen Wolfram, "A New Kind of Science", 2002,
;;     pages 23--60, 112, 865--866
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype file-source ()
  "The ``file-source'' type defines a valid source for an external
   file's obtention, which comprehends a ``pathname'' object, a stream
   connected to a file, or a string representation of a file path."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "Specifies the directory on the executing machine the same contains
   this Celum interpreter project's Common Lisp source files, and whence
   the separate file designations are derived.
   ---
   Please substitute the +PROJECT-DIRECTORY+ path by your personal
   directory which comprehends the requisite Common Lisp source files
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
                      |                 \"Celum\"
                      |                 \"Celum_001\"))
     ............................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/Celum/Celum_001/\")
     ------------------------------------------------------------")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file access operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-project-file (file-name)
  "Loads the Common Lisp file located under the FILE-NAME inwith the
   +PROJECT-DIRECTORY+, interprets its content, and returns no value."
  (declare (type file-source file-name))
  (load (merge-pathnames +PROJECT-DIRECTORY+ file-name))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import project files.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-project-file "types.lisp")
(load-project-file "command.lisp")
(load-project-file "commandIterator.lisp")
(load-project-file "line.lisp")
(load-project-file "program.lisp")
(load-project-file "lineSearch.lisp")
(load-project-file "lexer.lisp")
(load-project-file "parser.lisp")
(load-project-file "ecAutomaton.lisp")
(load-project-file "tape.lisp")
(load-project-file "input.lisp")
(load-project-file "output.lisp")
(load-project-file "conditions.lisp")
(load-project-file "interpreter.lisp")
(load-project-file "tests.lisp")
