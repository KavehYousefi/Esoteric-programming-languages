;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "InstructionPointerBF", presented by the Esolang user
;; "Pikal" in the year 2022, and intended as a compressed binary
;; encoding of "PocketBF", designed by the same author as a variant of
;; the extant programming language "tinyBF" by the Esolang user
;; "Bataais" (real name: Michael Gianfreda), all three of which
;; ultimately resolve to a more compact alternative to Urban Mueller's
;; "brainfuck" by dispatching the operative variety to a switch, known
;; as the "direction", which associates with each command token two
;; distinct effects.
;; 
;; 
;; Concept
;; =======
;; An InstructionPointerBF program consists of binary digits only,
;; decoding into a PocketBF program by switching at each binary zero (0)
;; to the next PocketBF instruction, and confirming the current
;; selection via an encounter with a binary one (1).
;; 
;; == SELECT AN INSTRUCTION WITH 0, CONFIRM WITH 1 ==
;; The concept is established upon the following scaffolding: Given the
;; very scarce variability of bits, entailing a set of the integers
;; {0, 1}, all of PocketBF's five instructions shall be representable.
;; This is accomplished by permitting to switch among the instruction
;; set members using the zero-bit (0), and fixating the choice by aide
;; of the one-bit (1).
;; 
;; Any InstructionPointerBF program starts with its PocketBF command
;; selector empighted on the PocketBF operation "="; the complete order
;; encompasses the following:
;; 
;;   ------------------------------------------------------------------
;;   Number | PocketBF token | PocketBF command effect
;;   -------+----------------+-----------------------------------------
;;   1      | =              | switch direction
;;   ..................................................................
;;   2      | +              | increment or decrement current cell
;;   ..................................................................
;;   3      | >              | move cell pointer
;;   ..................................................................
;;   4      | |              | jump forward or back
;;   ..................................................................
;;   5      | ;              | input or output
;;   ------------------------------------------------------------------
;; 
;; Having arrived at the desinent entry ";", the command marker returns
;; to the first operation, "=", repeating the traversal. Consequently,
;; the following relations betwixt the current PocketBF command and its
;; successor hold:
;; 
;;   ------------------------------------------------------------------
;;   Current PocketBF command | Next PocketBF command
;;   -------------------------+----------------------------------------
;;   =                        | +
;;   ..................................................................
;;   +                        | >
;;   ..................................................................
;;   >                        | |
;;   ..................................................................
;;   |                        | ;
;;   ..................................................................
;;   ;                        | =
;;   ------------------------------------------------------------------
;; 
;; == AN EXEMPLARY DECODING ==
;; An exemplary case shall administer itself to the binary decoding
;; principle's elucidation.
;; 
;; The following InstructionPointerBF code has been supplied:
;; 
;;   0100101010000100001
;; 
;; Arranged into bytes, from the most significant (sinistral) to the
;; least significant position (dextral), a more aesthetical design may
;; be delivered to one's obtention:
;; 
;;   01001010
;;   10000100
;;   001
;; 
;; In graduated steps, the bits, from the most significant bit (MSB) on
;; the left to the least significant bit (LSB) at the otherwart
;; laterality are processed in conformance with this method:
;; 
;;   ------------------------------------------------------------------
;;   Step | Event         | Effect
;;   -----+---------------+--------------------------------------------
;;    0   | Start         | At the start of each InstructionPointerBF
;;        |               | program, the command selector always resides
;;        |               | at the PocketBF instruction "=".
;;   ==================================================================
;;   Processing of the first byte 01001010:
;;   ..................................................................
;;    1   | 0 encountered | The command selector moves from "=" to the
;;        |               | next PocketBF instruction "+".
;;   ..................................................................
;;    2   | 1 encountered | The current PocketBF command, "+", is
;;        |               | collected. The hitherto generated PocketBF
;;        |               | code amounts to "+".
;;   ..................................................................
;;    3   | 0 encountered | The command selector moves from "+" to the
;;        |               | next PocketBF instruction ">".
;;   ..................................................................
;;    4   | 0 encountered | The command selector moves from ">" to the
;;        |               | next PocketBF instruction "|".
;;   ..................................................................
;;    5   | 1 encountered | The current PocketBF command, "|", is
;;        |               | collected. The hitherto generated PocketBF
;;        |               | code amounts to "+|".
;;   ..................................................................
;;    6   | 0 encountered | The command selector moves from "|" to the
;;        |               | next PocketBF instruction ";".
;;   ..................................................................
;;    7   | 1 encountered | The current PocketBF command, ";", is
;;        |               | collected. The hitherto generated PocketBF
;;        |               | code amounts to "+|;".
;;   ..................................................................
;;    8   | 0 encountered | The command selector moves from ";" to the
;;        |               | next PocketBF instruction "=", that is, the
;;        |               | selector, having reached the last item in
;;        |               | the PocketBF instruction order, relocates
;;        |               | to the first entry.
;;   ==================================================================
;;   Processing of the second byte 10000100:
;;   ..................................................................
;;    9   | 1 encountered | The current PocketBF command, "=", is
;;        |               | collected. The hitherto generated PocketBF
;;        |               | code comprehends "+|;=".
;;   ..................................................................
;;   10   | 0 encountered | The command selector moves from "=" to the
;;        |               | next PocketBF instruction "+".
;;   ..................................................................
;;   11   | 0 encountered | The command selector moves from "+" to the
;;        |               | next PocketBF instruction ">".
;;   ..................................................................
;;   12   | 0 encountered | The command selector moves from ">" to the
;;        |               | next PocketBF instruction "|".
;;   ..................................................................
;;   13   | 0 encountered | The command selector moves from "|" to the
;;        |               | next PocketBF instruction ";".
;;   ..................................................................
;;   14   | 1 encountered | The current PocketBF command, ";", is
;;        |               | collected. The hitherto generated PocketBF
;;        |               | code comprehends "+|;=;".
;;   ..................................................................
;;   15   | 0 encountered | The command selector moves from ";" to the
;;        |               | next PocketBF instruction "=". Please note
;;        |               | how the PocketBF instruction sequence again
;;        |               | restarts at the beginning.
;;   ..................................................................
;;   16   | 0 encountered | The command selector moves from "=" to the
;;        |               | next PocketBF instruction "+".
;;   ==================================================================
;;   Processing of the third byte 001:
;;   ..................................................................
;;   17   | 0 encountered | The command selector moves from "+" to the
;;        |               | next PocketBF instruction ">".
;;   ..................................................................
;;   18   | 0 encountered | The command selector moves from ">" to the
;;        |               | next PocketBF instruction "|".
;;   ..................................................................
;;   19   | 1 encountered | The current PocketBF command, "|", is
;;        |               | collected. The hitherto generated PocketBF
;;        |               | code comprehends "+|;=;|".
;;   ------------------------------------------------------------------
;; 
;; In corollary, the InstructionPointerBF code
;; 
;;   0100101010000100001
;; 
;; composed of nineteen bits, thus encodes the six-characters PocketBF
;; program
;; 
;;   +|;=;|
;; 
;; == ZERO-BITS PAD INSTRUCTIONPOINTERBF CODE TO A BYTE-BOUNDARY ==
;; If required to terminate on a byte boundary, which describes a bit
;; sequence with a length being a multiple of eight (8), the necessary
;; tally of zero-bits (0) may be appended to the InstructionPointerBF
;; code's tail, that is, its least significant positions, located at the
;; dextral margin. These additional PocketBF command selector
;; modifications, lacking the consequential one-bit (1), will not
;; contribute to the output PocketBF program.
;; 
;; In our illustrative case, the nineteen bits InstructionPointerBF
;; program, supplemented by five zero-bits, would expand into this
;; twenty-four bits sequence:
;; 
;;   010010101000010000100000
;;                      *****
;; 
;; Or, again assuming a reticular fashion:
;; 
;;   01001010
;;   10000100
;;   00100000
;;      *****
;; 
;; == POCKETBF: COMMANDS ARE TWIFACED ==
;; PocketBF originates from tinyBF's afflation --- the curtailment of
;; brainfuck's octuple instruction set without its capabilities'
;; forfeiture. To this end, each command token operates at any time in
;; one of two possible modes, or "directions".
;; 
;; Inherent to any PocketBF program wones a direction switch, capable
;; at an instant's assumption of one of two possible states: "positive"
;; and "negative", with the former providing the initial configuration.
;; Upon an encounter with the "=" command, the direction alters, from
;; "positive" to "negative" or from "negative" to "positive".
;; 
;; This constitutes one of the quintuple examples appertaining to
;; the language's double nature that is incorporated in any of its
;; operations. The following table shall provide a cursory description
;; regarding the commands and their effects in response to the direction
;; governing at the moment of their application:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   =       | If the direction is positive, sets it to negative;
;;           | otherwise, renders it positive.
;;   ..................................................................
;;   +       | If the direction is positive, increments the current
;;           | cell by one; otherwise, decrements it by one.
;;   ..................................................................
;;   >       | If the direction is positive, moves the cell pointer one
;;           | step to the right; otherwise, one step to the left.
;;   ..................................................................
;;   |       | If the direction is positive, and if the current cell
;;           | contains zero (0), jumps forward past the matching
;;           | closing "|". If the direction is negative, and if the
;;           | current cell does not contain zero (0), jumps back past
;;           | the matching opening "|".
;;   ..................................................................
;;   ;       | Independent of the direction, if the current cell
;;           | contains zero (0), queries the user for a character and
;;           | stores its ASCII code in the current cell. If the
;;           | current cell does not contain zero (0), outputs the
;;           | character for the ASCII code equal to the cell value.
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; PocketBF's cleronomy, itself a scion of the more liberal brainfuck
;; notion, restricts its architectural department to a linear memory's
;; deployment, unrestrained in the cells' tally and the magnitude of
;; their entailed signed integer scalar. Please note that in the coming
;; treatise the caracts of InstructionPointerBF and PocketBF conflate
;; into fungability.
;; 
;; == THE MEMORY: A TAPE OF CELLS ==
;; InstructionPointerBF programs rely on a linear arrangement of cells,
;; extending on both lateralities into a theoretically infinite range,
;; and each such compartment a salvatory to a single signed integer
;; whose gamut itself does not wist of any constraints.
;; 
;; Whereas, with the array or vector type being embossomed solutions,
;; the memory might be conceived as indexed, the only criterion for its
;; adit responds to the designation of the incipient cell, and the
;; contingency to traverse to its immediate neighbors in the sinistral
;; and dextral airt.
;; 
;; == A POINTER MOVES ALONG THE MEMORY ==
;; Operative in conjunction with the cell arrangement, a cursor, yclept
;; the "cell pointer", or simply "pointer", applies itself to the
;; memory.
;; 
;; At any instant, the pointer marks exactly one of the cells as the
;; currently selected on, homologating its exclusive indagation and
;; modification. At the program's inchoation empight at the first cell,
;; its respondency to move operations permits its stepwise traversal to
;; the current cell's sinistral or dextral neighbor, thus, ultimately,
;; capacitating a sojourn of all available cells.
;; 
;; 
;; Data Types
;; ==========
;; PocketBF's derivation from brainfuck perpetuates its influence in the
;; data type department, employing for the program memory castaldy
;; signed integers, whereas the currency of the human-machine
;; interaction resorts to the characters' bailiwick.
;; 
;; == INTEGERS: CURRENCY OF THE MEMORY ==
;; The paravaunt objects operated upon in a program are defined as the
;; data occurring in the memory: signed integers of any magnitude, which
;; fact relates to the range [-infinity, +infinity].
;; 
;; Incrementation and reduction, as well as translation from and to
;; ASCII characters, apply to this numeric ilk.
;; 
;; == CHARACTERS: CURRENCY OF INTERACTIONS ==
;; Hid in the memory, the numeric objects form participants in a rather
;; indirect capacity; in contrast, interactions with the human via input
;; and output are realized in character tokens.
;; 
;; The reception of input from the user conforms to an ASCII character,
;; the integer-valued character code of which is conceived and persisted
;; in the current memory cell.
;; 
;; In an athwart procession, the output facility elicits from the active
;; cell the character corresponding to the cell value when interpreted
;; as an ASCII code, resulting in a character printed to the user's
;; standard output.
;; 
;; 
;; Syntax
;; ======
;; While both a paragon of simplicity, the InstructionPointerBF syntaxis
;; vanquishes PocketBF in regard of frugality in reservations, extending
;; merely to the bit numbers 0 (zero) and 1 (one) any potentials,
;; whereas characters of all other kind are apportioned intolerance.
;; 
;; == INSTRUCTIONS ==
;; InstructionPointerBF program consist of zero or more bits, that is,
;; zeroes (0) and ones (1). Except for binary strings, which may include
;; whitespaces for the sake of lucidity, no demarcating objects are
;; required nor permitted to intrude in the commands' interstices.
;; 
;; The reservation of operations in PocketBF amounts to five characters,
;; each of which corresponds to one or more affiliated causata. Any
;; other character is admitted to the administration of commentary or
;; formatting purposes.
;; 
;; == WHITESPACES ==
;; With strictness in scrutiny, InstructionPointerBF programs are
;; delineated by 0-1 sequences, thus deprived of leniency towards other
;; content. However, if assuming a string representation, whitespaces,
;; a category embracing spaces, tabs, and newlines, shall be homologated
;; as sepiments, bare of constructive influences.
;; 
;; PocketBF's adherence to brainfuck permits whitespaces in the same
;; liberal mete as the remaining non-command tokens.
;; 
;; == COMMENTS ==
;; The stringency that wones in InstructionPointerBF's niggardliness
;; excludes any provisions for comments.
;; 
;; PocketBF, on the other hand, as an adherent of the brainfuck
;; philosophy, construes non-command tokens are comments.
;; 
;; == GRAMMAR ==
;; The InstructionPointerBF donat shall be the following Extended
;; Backus-Naur Form (EBNF) formulation's subject:
;; 
;;   instructionPointerBFprogram := { bit } ;
;;   bit                         := "0" | "1" ;
;; 
;; The PocketBF syntax, naturally, differs in syntactical matters from
;; InstructionPointerBF and shall thus be treated accordingly:
;; 
;;   pocketBFProgram := { pocketBFCommand | comment } ;
;;   comment         := character - pocketBFCommand ;
;;   pocketBFCommand := "=" | "+" | ">" | "|" | ";" ;
;; 
;; 
;; Instructions
;; ============
;; Ensuing from its state as an encoding of PocketBF's instruction set,
;; all InstructionPointerBF operations serve in the pursuit of
;; generating a valid piece of PocketBF source code. The following
;; sections will apply themselve to the elucidation of the encoding
;; language, succeeded by the target product.
;; 
;; == INSTRUCTIONPOINTERBF: A BINARY ENCODER ==
;; InstructionPointerBF wists only of two command types: the binary zero
;; (0) and the binary one (1). By utilizing these operations, a PocketBF
;; program is constructed for immediate or subsequent execution.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Switches to the next PocketBF instruction and mark it as
;;           | the current one.
;;           | The list conforms to the following order, cycling around
;;           | upon moving forward from the desinent fifth member:
;;           |   =
;;           |   +
;;           |   >
;;           |   |
;;           |   ;
;;   ..................................................................
;;   1       | Executes the current PocketBF instruction.
;;   ------------------------------------------------------------------
;; 
;; == POCKETBF: BRAINFUCK'S OCTUPLE INSTRUCTIONS CURTAILED TO FIVE ==
;; A conceptual scion of tinyBF, itself an instance of frugality applied
;; to the original brainfuck, reducing the eight ancestral instructions
;; to four, PocketBF increases the tally to five, superseding therein
;; the inspiring language's complex input/output rules based upon
;; particular sequences of three or four characters.
;; 
;; The five PocketBF commands shall become the following apercu's
;; cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   =       | Switches the direction from positive to negative.
;;           | Each PocketBF program starts in the positive direction.
;;           |---------------------------------------------------------
;;           | If the direction is positive: switches it to negative.
;;           |---------------------------------------------------------
;;           | If the direction is negative: switches it to positive.
;;   ..................................................................
;;   +       | Increments or decrements the current cell.
;;           |---------------------------------------------------------
;;           | If the direction is positive: increments the current
;;           | cell by one.
;;           | This corresponds to brainfuck's "+".
;;           |---------------------------------------------------------
;;           | If the direction is negative: decrements the current
;;           | cell by one.
;;           | This corresponds to brainfuck's "-".
;;   ..................................................................
;;   >       | Moves the cell pointer right or left.
;;           |---------------------------------------------------------
;;           | If the direction is positive: moves the cell pointer one
;;           | step to the right.
;;           | This corresponds to brainfuck's ">".
;;           |---------------------------------------------------------
;;           | If the direction is negative: moves the cell pointer one
;;           | step to the left.
;;           | This corresponds to brainfuck's "<".
;;   ..................................................................
;;   |       | Jumps forward or backward to the matching "|"
;;           | counterpart.
;;           |---------------------------------------------------------
;;           | If the direction is positive: If the current cell value
;;           | equals zero, moves the instruction pointer forward to
;;           | the position immediately following the matching jump end
;;           | marker "|"; otherwise, proceeds as usual.
;;           | This corresponds to brainfuck's "[".
;;           |---------------------------------------------------------
;;           | If the direction is negative: If the current cell value
;;           | does not equal zero, moves the instruction pointer back
;;           | to the position immediately following the matching jump
;;           | start marker "|"; otherwise, proceeds as usual.
;;           | This corresponds to brainfuck's "]".
;;   ..................................................................
;;   ;       | Performs an input or output operaton.
;;           |---------------------------------------------------------
;;           | Regardless of the direction: If the current cell value
;;           | equals zero, queries the user for a character and stores
;;           | its ASCII code in the current cell.
;;           | If the current cell value does not equal zero, prints to
;;           | the standard output the character associated with the
;;           | current cell value as its ASCII code.
;;           | This constitutes a deviant conflation of brainfuck's ","
;;           | and "." commands.
;;   ------------------------------------------------------------------
;; 
;; == FROM INSTRUCTIONPOINTERBF TO POCKETBF ==
;; The essential and inventive role of the InstructionPointerBF behavior
;; being affiliated with the "0" command, the switching betwixt PocketBF
;; instructions for ultimate appropriation, shall be two tabular
;; illustrations' material:
;; 
;;   ------------------------------------------------------------------
;;   Number | PocketBF token | PocketBF command effect
;;   -------+----------------+-----------------------------------------
;;   1      | =              | switch direction
;;   ..................................................................
;;   2      | +              | increment or decrement current cell
;;   ..................................................................
;;   3      | >              | move cell pointer
;;   ..................................................................
;;   4      | |              | jump forward or back
;;   ..................................................................
;;   5      | ;              | input or output
;;   ------------------------------------------------------------------
;; 
;; Please note that the sequence of PocketBF commands, having arrived at
;; the desinent member, ";", iterum commences with the first entity "=".
;; This cyclic characteristic permits the selection of any command
;; starting at any other by proceeding through zero-bits (0) until the
;; optated destination member has been encountered.
;; 
;;   ------------------------------------------------------------------
;;   Current PocketBF command | Next PocketBF command
;;   -------------------------+----------------------------------------
;;   =                        | +
;;   ..................................................................
;;   +                        | >
;;   ..................................................................
;;   >                        | |
;;   ..................................................................
;;   |                        | ;
;;   ..................................................................
;;   ;                        | =
;;   ------------------------------------------------------------------
;; 
;; The following pseudocode shall be a warkloom to the switching
;; process' further elucidation, effectively affiliating with the
;; InstructionPointerBF command "0":
;; 
;;   let pocketBFCommand <- {"=", "+", ">", "|", ";"}
;;   
;;   function getNextCommand (currentCommand)
;;     input:
;;       currentCommand --- a member of the pocketBFCommand set
;;     begin
;;       if currentCommand = "=" then
;;         return "+"
;;       else if currentCommand = "+" then
;;         return ">"
;;       else if currentCommand = ">" then
;;         return "|"
;;       else if currentCommand = "|" then
;;         return ";"
;;       else if currentCommand = ";" then
;;         return "="
;;       else
;;         error: Invalid PocketBF command {currentCommand}.
;;       end if
;;     end
;;   end function
;;   
;;   function decodeInstructionPointerBFCode (b)
;;     input:
;;       b --- a sequence of N bits, b[1] to b[N],
;;             with b[i] in {0, 1}, for 1 <= i <= N
;;     begin
;;       let pocketBFCode   <- empty string
;;       let currentCommand <- "=", with currentCommand in
;;                                  pocketBFCommand
;;       
;;       for i from 1 to N
;;         if b[i] = 0 then
;;           currentCommand <- getNextCommand(currentCommand)
;;         else
;;           append currentCommand to pocketBFCode
;;         end if
;;       end for
;;       
;;       return pocketBFCode
;;     end
;;   end function
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-26
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/InstructionPointerBF"
;;   -> "https://esolangs.org/wiki/PocketBF"
;;   -> "https://esolangs.org/wiki/TinyBF"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elments, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype pocketBF-instruction ()
  "The ``pocketBF-instruction'' type enumerates the recognized PocketBF
   commands."
  '(member
    :switch-direction     ;; PocketBF token "=".
    :increment/decrement  ;; PocketBF token "+".
    :move-pointer         ;; PocketBF token ">".
    :jump                 ;; PocketBF token "|".
    :input/output))       ;; PocketBF token ";".

;;; -------------------------------------------------------

(deftype pocketBF-program ()
  "The ``pocketBF-program'' type defines a PocketBF program as a vector
   of PocketBF instructions."
  '(vector pocketBF-instruction *))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the valid states for the direction
   switch state."
  '(member :positive :negative))

;;; -------------------------------------------------------

(deftype brainfuck-instruction ()
  "The ``brainfuck-instruction'' type enumerates the recognized
   brainfuck commands."
  '(member
    :increment
    :decrement
    :move-right
    :move-left
    :jump-forward
    :jump-back
    :input
    :output))

;;; -------------------------------------------------------

(deftype brainfuck-program ()
  "The ``brainfuck-program'' type defines a brainfuck program as a
   vector of brainfuck instructions."
  '(vector brainfuck-instruction *))

;;; -------------------------------------------------------

(deftype instructionPointerBF-source ()
  "The ``instructionPointerBF-source'' enumerates the types of objects
   in whose forms a piece of InstructionPointerBF source code may be
   induced into this program."
  '(or integer bit-vector string))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump to back
   jump positions in a brainfuck program, and vice versa, manifesting as
   a hash table which associates fixnum keys with fixnum values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as an association of
   unsigned integer cell indices to cell values of the same type,
   realized as a hash table mapping from the former to the latter."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of InstructionBF parser.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-next-instruction (current-instruction)
  "Returns the instruction succeeding the CURRENT-INSTRUCTION in the
   order of commands, potentially looping around."
  (declare (type pocketBF-instruction current-instruction))
  (the pocketBF-instruction
    (case current-instruction
      (:switch-direction    :increment/decrement)
      (:increment/decrement :move-pointer)
      (:move-pointer        :jump)
      (:jump                :input/output)
      (:input/output        :switch-direction)
      (otherwise
        (error "Invalid instruction: ~s." current-instruction)))))

;;; -------------------------------------------------------

(defgeneric parse-InstructionPointerBF (source)
  (:documentation
    "Extracts and returns from the InstructionPointerBF SOURCE code a
     one-dimensional simple-array of zero or more PocketBF
     instructions."))

;;; -------------------------------------------------------

(defmethod parse-InstructionPointerBF ((bits integer))
  "Extracts and returns from the BITS, an integer-encoded binary
   sequence whose length is padded with zero-bits to a multiple of eight
   (8) upon necessity, starting with the most significant position and
   proceeding to the least significant, a one-dimensional simple array
   of PocketBF instructions."
  (declare (type integer bits))
  (let ((current-instruction :switch-direction)
        (completed-bit-count (* (ceiling (integer-length bits) 8) 8)))
    (declare (type pocketBF-instruction current-instruction))
    (declare (type (integer 0 *)        completed-bit-count))
    (the (simple-array pocketBF-instruction (*))
      (coerce
        (loop
          for bit-position
            of-type integer
            from    (1- completed-bit-count)
            downto  0
          and position
            of-type fixnum
            from    0
          ;; Current bit equals one?
          ;; => Execute the current instruction.
          if (logbitp bit-position bits)
            collect current-instruction
          ;; Current bit equals zero?
          ;; => Select the next instruction following the order
          ;;    ("=", "+", ">", "|", ";").
          else
            do
              (setf current-instruction
                (get-next-instruction current-instruction)))
        '(simple-array pocketBF-instruction (*))))))

;;; -------------------------------------------------------

(defmethod parse-InstructionPointerBF ((bits bit-vector))
  "Extracts and returns from the BITS bit vector a one-dimensional
   simple array of PocketBF instructions."
  (declare (type bit-vector bits))
  (let ((current-instruction :switch-direction))
    (declare (type pocketBF-instruction current-instruction))
    (the (simple-array pocketBF-instruction (*))
      (coerce
        (loop
          for bit of-type bit across bits
          ;; Current bit equals one?
          ;; => Execute the current instruction.
          if (= bit 1)
            collect current-instruction
          ;; Current bit equals zero?
          ;; => Select the next instruction following the order
          ;;    ("=", "+", ">", "|", ";").
          else
            do
              (setf current-instruction
                (get-next-instruction current-instruction)))
        '(simple-array pocketBF-instruction (*))))))

;;; -------------------------------------------------------

(defmethod parse-InstructionPointerBF ((binary-string string))
  "Extracts and returns from the BINARY-STRING a one-dimensional simple
   array of PocketBF instructions."
  (declare (type string binary-string))
  (let ((instructions        NIL)
        (current-instruction :switch-direction))
    (declare (type (list-of pocketBF-instruction) instructions))
    (declare (type pocketBF-instruction           current-instruction))
    (loop
      for token    of-type character across binary-string
      and position of-type fixnum    from   0
      do
        (case token
          ;; Select the next instruction following the order
          ;; ("=", "+", ">", "|", ";").
          (#\0
            (setf current-instruction
              (get-next-instruction current-instruction)))
          ;; Execute the current instruction.
          (#\1
            (push current-instruction instructions))
          ;; Skip whitespaces.
          ((#\Space #\Tab #\Newline)
            NIL)
          ;; Any other content is prohibited.
          (otherwise
            (error "Invalid character \"~c\" at position ~d."
              token position))))
    (the (simple-array pocketBF-instruction (*))
      (coerce
        (nreverse instructions)
        '(simple-array pocketBF-instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of InstructionPointerBF code generator.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-InstructionPointerBF-code
    (instructions
     &key (normalize-p NIL))
  "Generates and returns a bit vector representing the
   InstructionPointerBF source code capable of implementing the PocketBF
   INSTRUCTIONS, additionally, if NORMALIZE-P resolves to a Boolean
   true, padded with trailing zero-bits in order to ascertain a length
   being a multiple of eight, thus reaching a byte boundary."
  (declare (type pocketBF-program instructions))
  (declare (type boolean          normalize-p))
  
  (let ((bits
          (make-array
            (length instructions)
            :element-type    'bit
            :initial-element 0
            :adjustable      T
            :fill-pointer    0))
        (current-instruction :switch-direction))
    (declare (type bit-vector           bits))
    (declare (type pocketBF-instruction current-instruction))
    
    (flet
        ((change-to-instruction (new-instruction)
          "Appends the requisite tally of zero-bits to the BITS vector
           for translating from the CURRENT-INSTRUCTION to the
           NEW-INSTRUCTION, and returns no value."
          (declare (type pocketBF-instruction new-instruction))
          (loop until (eq current-instruction new-instruction) do
            (vector-push-extend 0 bits)
            (setf current-instruction
              (get-next-instruction current-instruction)))
          (values))
         
         (normalize-if-necessary ()
          "If NORMALIZE-P resolves to a Boolean true value, appends the
           requisite number of zero-bits to the BITS vector to pad the
           same to a multiple of eight bits' size, and returns no
           value."
          (when normalize-p
            (let ((number-of-missing-bits
                    (abs (nth-value 1 (ceiling (length bits) 8)))))
              (declare (type (integer 0 7) number-of-missing-bits))
              (loop repeat number-of-missing-bits do
                (vector-push-extend 0 bits))))
          (values)))
      
      (loop
        for instruction
          of-type pocketBF-instruction
          across  instructions
        and position
          of-type fixnum
          from    0
        do
          (change-to-instruction instruction)
          (vector-push-extend 1 bits)
        finally
          (normalize-if-necessary)))
    
    (the bit-vector bits)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of PocketBF parser.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-PocketBF-instructions (code)
  "Parses the piece of PocketBF source CODE and returns a
   one-dimensional simple array of PocketBF instructions."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of pocketBF-instruction) instructions))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0
      do
        (case token
          (#\= (push :switch-direction    instructions))
          (#\+ (push :increment/decrement instructions))
          (#\> (push :move-pointer        instructions))
          (#\| (push :jump                instructions))
          (#\; (push :input/output        instructions))
          ((#\Space #\Tab #\Newline) NIL)
          (otherwise
            (error "Invalid token ~s in PocketBF code at position ~d."
              token position))))
    (the (simple-array pocketBF-instruction (*))
      (coerce
        (nreverse instructions)
        '(simple-array pocketBF-instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of PocketBF code generator.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-PocketBF-token (instruction)
  "Returns the PocketBF character corresponding to the INSTRUCTION."
  (declare (type pocketBF-instruction instruction))
  (the character
    (case instruction
      (:switch-direction    #\=)
      (:increment/decrement #\+)
      (:move-pointer        #\>)
      (:jump                #\|)
      (:input/output        #\;)
      (otherwise
        (error "Invalid PocketBF instruction: ~s." instruction)))))

;;; -------------------------------------------------------

(defun generate-PocketBF-code (instructions
                               &key (destination NIL))
  "Writes the PocketBF source code tantamount to the INSTRUCTIONS to the
   DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise responding with a fresh string containing the
   output."
  (declare (type pocketBF-program instructions))
  (declare (type destination      destination))
  (the (or null string)
    (if destination
      (loop
        for instruction
          of-type pocketBF-instruction
          across  instructions
        do
          (write-char
            (get-PocketBF-token instruction)
            destination))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-PocketBF-code instructions :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of PocketBF-to-brainfuck converter.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-next-direction (current-direction)
  "Returns the switch direction opposing the CURRENT-DIRECTION."
  (declare (type direction current-direction))
  (the direction
    (case current-direction
      (:positive :negative)
      (:negative :positive)
      (otherwise
        (error "Invalid switch direction: ~s." current-direction)))))

;;; -------------------------------------------------------

(defun get-brainfuck-instruction (direction pocketBF-instruction)
  "Returns the brainfuck instruction corresponding to the combination
   PocketBF switch direction and POCKETBF-INSTRUCTION."
  (declare (type direction            direction))
  (declare (type pocketBF-instruction pocketBF-instruction))
  (flet ((matches-combination-p (expected-direction
                                 expected-pocketBF-instruction
                                 brainfuck-instruction)
          "Checks whether the input DIRECTION equals the
           EXPECTED-DIRECTION and the input POCKETBF-INSTRUCTION equals
           the EXPECTED-POCKETBF-INSTRUCTION, on confirmation of both
           returning the BRAINFUCK-INSTRUCTION, otherwise ``NIL''."
          (declare (type direction             expected-direction))
          (declare (type pocketBF-instruction  pocketBF-instruction))
          (declare (type brainfuck-instruction brainfuck-instruction))
          (the (or null brainfuck-instruction)
            (when (and (eq expected-direction direction)
                       (eq expected-pocketBF-instruction
                           pocketBF-instruction))
              brainfuck-instruction))))
    
    (the brainfuck-instruction
      (or
        (matches-combination-p :positive :increment/decrement
                               :increment)
        (matches-combination-p :negative :increment/decrement
                               :decrement)
        (matches-combination-p :positive :move-pointer :move-right)
        (matches-combination-p :negative :move-pointer :move-left)
        (matches-combination-p :positive :jump         :jump-forward)
        (matches-combination-p :negative :jump         :jump-back)
        (matches-combination-p :positive :input/output :input)
        (matches-combination-p :negative :input/output :output)
        (error "Invalid combination of direction ~s and PocketBF ~
                instruction ~s."
          direction pocketBF-instruction)))))

;;; -------------------------------------------------------

(defun convert-PocketBF-instructions-to-brainfuck
    (pocketBF-instructions)
  "Creates and returns a one-dimensional simple array of brainfuck
   instructions corresponding to the POCKETBF-INSTRUCTIONS."
  (declare (type pocketBF-program pocketBF-Instructions))
  (let ((brainfuck-instructions NIL)
        (direction              :positive))
    (declare (type (list-of brainfuck-instruction)
                   brainfuck-instructions))
    (declare (type direction direction))
    (loop
      for pocketBF-instruction
        of-type pocketBF-instruction
        across  pocketBF-instructions
      do
        (if (eq pocketBF-instruction :switch-direction)
          (setf direction (get-next-direction direction))
          (push
            (get-brainfuck-instruction direction pocketBF-instruction)
            brainfuck-instructions)))
    (the (simple-array brainfuck-instruction (*))
      (coerce
        (nreverse brainfuck-instructions)
        '(simple-array brainfuck-instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation PocketBF-compatible brainfuck interpreter.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (brainfuck-instructions)
  "Creates and returns a jump table for the BRAINFUCK-INSTRUCTIONS,
   associating with each forward jump instruction index in the same the
   matching back jump location, and vice versa.
   ---
   The detection of any mismatches betwixt such jump boundaries will
   cause an error of an unspecified type to be signaled."
  (declare (type brainfuck-program brainfuck-instructions))
  
  (let ((jump-table  (make-hash-table :test #'eql))
        (jump-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-starts))
    (loop
      for instruction
        of-type brainfuck-instruction
        across  brainfuck-instructions
      and position
        of-type fixnum
        from    0
      do
        (case instruction
          (:jump-forward
            (push position jump-starts))
          (:jump-back
            (if jump-starts
              (let ((start-position (pop jump-starts)))
                (declare (type fixnum start-position))
                (setf (gethash start-position jump-table) position)
                (setf (gethash position jump-table) start-position))
              (error "Unmatched jump end at position ~d." position)))
          (otherwise
            NIL)))
    
    (when jump-starts
      (error "Unmatched jump starts at positions ~{~d~^, ~}."
        jump-starts))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-brainfuck-instructions (instructions)
  "Processes the brainfuck INSTRUCTIONS according to the rules of
   PocketBF and returns no value."
  (declare (type brainfuck-program instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (jump-table  (build-jump-table instructions))
          (memory      (make-hash-table :test #'eql))
          (pointer     0))
      (declare (type fixnum                          ip))
      (declare (type (or null brainfuck-instruction) instruction))
      (declare (type jump-table                      jump-table))
      (declare (type memory                          memory))
      (declare (type integer                         pointer))
      
      (flet
          ((advance ()
            "Relocates the instruction pointer IP to the next position
             in the INSTRUCTIONS vector, if possible, updates the
             current INSTRUCTION, and returns no value."
            (setf instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to-opposite-boundary ()
            "Expected to be located on a jump forward or back
             instruction, relocates the instruction pointer IP to the
             opposite boundary, updates the current INSTRUCTION, and
             returns no value."
            (setf ip (gethash ip jump-table))
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (current-cell ()
            "Returns the value of the current cell."
            (the integer
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the current cell and returns no
             value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:increment
              (incf (current-cell)))
            
            (:decrement
              (decf (current-cell)))
            
            (:move-right
              (incf pointer))
            
            (:move-left
              (decf pointer))
            
            (:jump-forward
              (when (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (:jump-back
              (unless (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            ((:input :output)
              (cond
                ((zerop (current-cell))
                  (format T "~&Please input a character: ")
                  (setf (current-cell)
                        (char-code (read-char)))
                  (clear-input))
                (T
                  (write-char (code-char (current-cell))))))
            
            (otherwise
              (error "Invalid brainfuck instruction ~s at position ~d."
                instruction ip)))
          
          (advance)))))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of PocketBF interpreter.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-PocketBF (code)
  "Interprets the piece of PocketBF source CODE and returns no value."
  (declare (type string code))
  (process-brainfuck-instructions
    (convert-PocketBF-instructions-to-brainfuck
      (parse-PocketBF-instructions code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of InstructionPointerBF interpreter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-InstructionPointerBF (code)
  "Interprets the piece of InstructionPointerBF source CODE and returns
   no value."
  (declare (type instructionPointerBF-source code))
  (process-brainfuck-instructions
    (convert-PocketBF-instructions-to-brainfuck
      (parse-InstructionPointerBF code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; InstructionPointerBF:
;; Infinitely repeating cat program.
(interpret-InstructionPointerBF
  "01001001010000100001010000101000100101001001010000100010")

;;; -------------------------------------------------------

;; PocketBF:
;; Infinitely repeating cat program.
(interpret-PocketBF "+|=+=;=;=|=+|=+=|")

;;; -------------------------------------------------------

;; Generate the InstructionPointerBF code, in the form of a bit vector,
;; equivalent to the PocketBF instructions for an infinitely repeating
;; cat program.
(generate-InstructionPointerBF-code
  (coerce
    '(:increment/decrement
      
      ;; Start cat program loop.
      :jump
      ;; Decrement current cell to zero in order to activate input.
      :switch-direction
      :increment/decrement
      
      ;; Query user input.
      :switch-direction
      :input/output
      
      ;; Print user input.
      :switch-direction
      :input/output
      
      ;; Iterate until the current cell equals zero in order to prepare
      ;; next input precondition.
      :switch-direction
      :jump
      :switch-direction
      :increment/decrement
      :jump
      
      ;; Increment the current cell by one such that the cat program
      ;; loop does not terminate.
      ;; The value will be decremented by one at the beginning of the
      ;; next iteration, which please see above.
      :switch-direction
      :increment/decrement
      :switch-direction
      
      :jump)
    'pocketBF-program)
  :normalize-p T)

;;; -------------------------------------------------------

;; Generate the PocketBF source code equivalent to the instructions of
;; an infinitely repeating cat program, and print the result to the
;; standard output.
(generate-PocketBF-code
  (coerce
    '(:increment/decrement
      
      ;; Start cat program loop.
      :jump
      ;; Decrement current cell to zero in order to activate input.
      :switch-direction
      :increment/decrement
      
      ;; Query user input.
      :switch-direction
      :input/output
      
      ;; Print user input.
      :switch-direction
      :input/output
      
      ;; Iterate until the current cell equals zero in order to prepare
      ;; next input precondition.
      :switch-direction
      :jump
      :switch-direction
      :increment/decrement
      :jump
      
      ;; Increment the current cell by one such that the cat program
      ;; loop does not terminate.
      ;; The value will be decremented by one at the beginning of the
      ;; next iteration, which please see above.
      :switch-direction
      :increment/decrement
      :switch-direction
      
      :jump)
    'pocketBF-program)
  :destination T)

;;; -------------------------------------------------------

;; Generate the brainfuck instructions equivalent to the infinitely
;; repeating cat program in PocketBF.
(convert-PocketBF-instructions-to-brainfuck
  (coerce
    '(:increment/decrement
      
      ;; Start cat program loop.
      :jump
      ;; Decrement current cell to zero in order to activate input.
      :switch-direction
      :increment/decrement
      
      ;; Query user input.
      :switch-direction
      :input/output
      
      ;; Print user input.
      :switch-direction
      :input/output
      
      ;; Iterate until the current cell equals zero in order to prepare
      ;; next input precondition.
      :switch-direction
      :jump
      :switch-direction
      :increment/decrement
      :jump
      
      ;; Increment the current cell by one such that the cat program
      ;; loop does not terminate.
      ;; The value will be decremented by one at the beginning of the
      ;; next iteration, which please see above.
      :switch-direction
      :increment/decrement
      :switch-direction
      
      :jump)
    'pocketBF-program))
