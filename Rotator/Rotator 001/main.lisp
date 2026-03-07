;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Rotator", invented by the Esolang user "ChuckEsoteric08"
;; and presented on September 19th, 2025, the woning of its proprium a
;; variation on Urban Mueller's "brainfuck", kenspeckle in the
;; curtailment of the provenance's octuple instruction set by the
;; sinistral tape cell pointer's membership, as well as the original
;; infinite byte tape's supersession by a catena of five cells, designed
;; in an annulate fashion, the trisulc of dioristic innovations'
;; patration molded in a supererogative dextral cell pointer traversal
;; as a perclose to any instruction's actuation.
;; 
;; 
;; Concept
;; =======
;; The Rotator programming language's conception ensues from brainfuck's
;; firmament, upon whose inspiration a specimen deviating in the
;; operative and architectural tenets has been begotten, producing a
;; curtal in the tape cell pointer's sinistral dislocation instruction,
;; "<", and, consanguinous in the haecceity, superseding the
;; stock-father's bilaterally infinite cells of unsigned byte-valued
;; capacity by a quintuple annulation of non-negative integer units,
;; each such wisting of no upper mear in the mickleness; finally
;; imposing as an epiphenomon to each successfully evaluated command
;; a dextral cell pointer movement.
;; 
;; == THE MEMORY: A QUINTUPLE ANNULATION OF NON-NEGATIVE INTEGERS ==
;; The memory architecture ostends a deviation from brainfuck's infinite
;; tape towards a mere quintuple cells catena, apposted in an annular
;; design, with a unit's capacity now liberated from the entheus'
;; unsigned byte constitution in favor of a non-negative integer number
;; disencumbered from any mickleness along the positive axis.
;; 
;; Each cell at its inchoacy endowed with the minimum state of zero (0),
;; the cell pointer occupies the first position along the tape, its
;; motation one impounded to dextral peragrations; with a wrapping to
;; the first cell upon the desinence's transgression.
;; 
;; == ROTATOR DISCARDS THE "<" COMMAND ==
;; The annulary constitution applied to the denumerable cell accompt of
;; five units reverberates in the cell pointer's incession, which
;; admits merely a dextral species of peragration, wrapping, upon the
;; right march's violation, around to the first position.
;; 
;; This ilk of tolerable carency produces a septuple membership in the
;; instruction set's circumference, discounted by one element, the "<"
;; member, when equiparated to the brainfuck standard.
;; 
;; == DEXTRAL CELL POINTER MOTION AS AN ADSCITITIOUS EPIPHENOMENON ==
;; A parhedral action, serving as a prosthesis to an instruction's
;; patration, manifests in a subsequent dextral cell pointer movement
;; by a single step.
;; 
;; 
;; Instructions
;; ============
;; The Rotator programming language, conceived as a derivation of
;; brainfuck, entertains a curtailment of both the operative
;; circumference and the memory's dispansion; the latter engages in an
;; act of cambistry that supersedes the bilaterally infinite tape of
;; unsigned bytes for a ring of quintuple constitution lending a
;; commorancy to unsigned integer numbers of any mickleness; the aspect
;; concerning the instructions disposes of the now supernumerary
;; sinistral cell pointer translation behest, the owelty of whose
;; causatum may be obtained by the wrapping memory nature.
;; 
;; Any successfully peracted operation concludes with a dextral
;; movement of the cell pointer.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be patefied in a sufficient mete
;; of gnarity's communication anent the language's operative
;; competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right. Upon
;;           | a transition from the desinent, fifth cell, the pointer
;;           | wraps around to the first memory position.
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ..................................................................
;;   +       | Increments the current cell's value by one (1).
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ..................................................................
;;   -       | If the current cell's value is greater than zero (0),
;;           | decrements the same by one (1); otherwise accompasses no
;;           | causatum.
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code concurs with the
;;           | current cell value to the standard output conduit.
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ..................................................................
;;   [       | If the current cell contains the value zero (0), moves
;;           | the instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ..................................................................
;;   ]       | If the current cell does not contain the value zero (0),
;;           | moves the instruction pointer (IP) backward to the
;;           | position immediately succeeding the matching "["
;;           | instruction; otherwise accompasses no causatum.
;;           |---------------------------------------------------------
;;           | As an epiphenomenal ultimity, the cell pointer
;;           | translates one step to the right.
;;   ------------------------------------------------------------------
;; 
;; == ROTATOR AND BRAINFUCK ==
;; The vinculum of consanguinity contexing the Rotator programming
;; language and its stock-father brainfuck homologates an equiparation
;; of the latter's competences with the former's vouchsafements:
;; 
;;   --------------------
;;   brainfuck | Rotator
;;   ----------+---------
;;   >         | >>>
;;   ....................
;;   <         | >>
;;   ....................
;;   +         | +>>
;;   ....................
;;   -         | ->>
;;   ....................
;;   ,         | ,>>
;;   ....................
;;   .         | .>>
;;   ....................
;;   [         | [>>
;;   ....................
;;   ]         | ]>>
;;   --------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization constitutes an effort in the
;; programming language Common Lisp, the gestion's entirety a twifold
;; process, inchoating in the extraction of the recognized instruction
;; identifiers, their transformation into dedicated ``Command'' objects,
;; and collation in an ordered sequence; and concluding in these
;; commands evaluation by adminiculum of their amplected identifier
;; symbols.
;; 
;; == INSTRUCTIONS ARE REPRESENTED BY TRIPLETS ==
;; Each Rotator instruction ensconced in the original source code string
;; translates into a ``Command'' object, siccan complex limns the
;; following triple's design:
;; 
;;   (symbol, position, destination)
;; 
;; This documentation shall not be arraigned with the wite of an
;; epexegesis' lapsus concerning the trisulc componency:
;; 
;;   (1) SYMBOL:
;;       The character identifying the thus modeled command, the
;;       parcery of which relates to the instruction's ultimate
;;       recognition and execution.
;;   
;;   (2) POSITION:
;;       The zero-based index of the command in the original source code
;;       string, the agency of whose participation does not ascend aboon
;;       contingent error messages' enrichment.
;;   
;;   (3) DESTINATION:
;;       If the command represents a forward or back jump behest, the
;;       zero-based index in the parsed program vector --- not the
;;       original source code string --- of the opposite destination
;;       command. For a non-navigation instruction, the disqualifying
;;       sentinel -1 is assigned.
;; 
;; == THE INSTRUCTIONS ARE CONVERTED INTO A COMMAND LIST ==
;; The process involved in the conversion from instruction identifiers
;; in the source code string to their evaluation as ``Command''
;; compounds shall be following listing's cynosure:
;; 
;;   (1) COMMAND SEQUENCE GENERATIONS:
;;       Each character in the Rotator source code string is iterated.
;;       If the character identifies one of the seven recognized
;;       instruction names, a new ``Command'' instance is produced,
;;       storing the instruction symbol and in conjunction with the
;;       zero-based position of its extractions, and appending the
;;       command object to the ordered sequence of hitherto encountered
;;       instructions; otherwise, the character is ignored.
;;   
;;   (2) JUMP COMMANDS CONTEXTURE:
;;       The ``Command'' sequence is iterated, alligating the jump
;;       instructions, represented by "[" and "]", in a bilateral
;;       fashion by storing in each instance the zero-based index of the
;;       obverse jump point in the command sequence.
;;   
;;   (3) COMMAND SEQUENCE EXECUTION:
;;       The instructions partaking in the command sequence are
;;       submitted to the interpretation process, the operations'
;;       identification ensuing from the symbol incorporated in the
;;       contemporaneously evaluated ``Command'' instance.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-03-05
;; 
;; Sources:
;;   [esolang:2025:Rotator]
;;   The Esolang contributors, "Rotator", September 19th, 2025
;;   URL: "https://esolangs.org/wiki/Rotator"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type-related operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fixnum-list-p (object)
  "Determines whether the OBJECT represents a list whose conformation
   does not wist of any other species' participation than ``fixnum''
   species, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null
      (and
        (listp object)
        (every
          #'(lambda (current-element)
              (declare (type T current-element))
              (typep current-element 'fixnum))
          (the list object)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype program ()
  "The ``program'' type defines an executable Rotator program as a
   one-dimensional simple array composed of ``Command'' objects."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype fixnum-list ()
  "The ``fixnum-list'' type defines a list whose conformation is
   restricted to zero or more ``fixnum'' elements."
  '(satisfies fixnum-list-p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Command".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-a-command (symbol position)))
  "The ``Command'' class applies itself to the encapsulation of a
   Rotator instruction's pertinent constituents, this compass exhausted
   by the operative symbol, desumed from the septuple admissible
   contingency, the zero-based position into the original source code
   whence its provenance is obtained, and an optional destination index,
   the same, of significance only in the representation of forward
   (\"]\") or back jump (\"[\") operations, references the zero-based
   position of the obverse end point in the parsed command sequence,
   rather than the original source string.
   ---
   An epexegesis whose pernancy enjoys an enhaused nimiety of gnarity
   shall present the incorporated trisulc in detail:
     ------------------------------------------------------------------
     Slot        | Purpose
     ------------+-----------------------------------------------------
     symbol      | The symbol, always one of the septuple admissible
                 | instruction identifiers, represented by this
                 | command.
     ..................................................................
     position    | The zero-based index designating the location in the
                 | original Rotator source code string whence this
                 | command has been extracted.
     ..................................................................
     destination | Sensible merely for forward (\"[\") and back (\"]\")
                 | jump instructions, maintains the zero-based index
                 | of the opposite jump command object in the vector
                 | comprehending the parsed commands, rather than the
                 | original source code string.
     ------------------------------------------------------------------"
  (symbol      (error "No command symbol has been communicated.")
               :type      standard-char
               :read-only T)
  (position    (error "No command position has been communicated.")
               :type      fixnum
               :read-only T)
  (destination -1
               :type      fixnum
               :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 7) +COMMAND-NAMES+))

;;; -------------------------------------------------------

(defconstant +COMMAND-NAMES+ ">+-,.[]"
  "Defines the septuple Rotator command identifiers.")

;;; -------------------------------------------------------

(defun command-name-p (candidate)
  "Determines whether the CANDIDATE represents a member of the septuple
   Rotator command identifiers, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate +COMMAND-NAMES+ :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-the-commands (code)
  "Extracts from the piece of Rotator source code the amplected commands
   and returns a one-dimensional simple array of ``Command''
   representatives."
  (declare (type string code))
  (the program
    (coerce
      (loop
        for current-token    of-type character across code
        and current-position of-type fixnum    from   0 by 1
        when (command-name-p current-token) collect
          (make-a-command current-token current-position))
      '(simple-array Command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump point contexture operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-the-jump-points (program)
  "Alligates those commands in the PROGRAM representing jump points by
   adminiculum of their zero-based position inwith the same and returns
   no value."
  (declare (type program program))
  (loop
    with forward-jump-points of-type fixnum-list =      NIL
    for  current-command     of-type Command     across program
    and  current-position    of-type fixnum      from   0 by 1
    do
      (case (command-symbol current-command)
        (#\[
          (push current-position forward-jump-points))
        (#\]
          (if forward-jump-points
            (let ((forward-jump-point (pop forward-jump-points))
                  (back-jump-point    current-position))
              (declare (type fixnum forward-jump-point))
              (declare (type fixnum back-jump-point))
              (psetf
                (command-destination current-command)
                  forward-jump-point
                (command-destination (aref program forward-jump-point))
                  back-jump-point))
            (error "The back jump command at the position ~d does ~
                    not correspond to any forward jump token."
              (command-position current-command))))
        (otherwise
          NIL))
    finally
      (when forward-jump-points
        (let ((unmatched-positions
                (map 'list #'command-position forward-jump-points)))
          (declare (type fixnum-list unmatched-positions))
          (error "The forward jump command~p at the position~:p ~
                  ~{~d~^, ~} do not correspond to any forward jump ~
                  ~2:*token~p."
            (length unmatched-positions)
            unmatched-positions))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor prepare-a-pristine-tape ()))
  "The ``Tape'' class establishes the program memory as a quintuple
   annulation of non-negative integer cells, their mickleness tholing
   no imposition along the positive axis."
  (cells    (make-array 5
              :element-type    '(integer 0 *)
              :initial-element 0
              :adjustable      NIL
              :fill-pointer    NIL)
            :type      (simple-array (integer 0 *) (5))
            :read-only T)
   (pointer 0
            :type      (integer 0 4)
            :read-only NIL))

;;; -------------------------------------------------------

(defun advance-the-cell-pointer (tape)
  "Avaunts the TAPE's cell pointer to the next cell, contingently
   wrapping around to the first one upon its dextral bourne's
   transgression, and returns no value."
  (declare (type Tape tape))
  (setf (tape-pointer tape)
    (mod
      (1+ (tape-pointer tape))
      (length (tape-cells tape))))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the current TAPE cell's value."
  (declare (type Tape tape))
  (the (integer 0 *)
    (aref
      (tape-cells   tape)
      (tape-pointer tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the current TAPE cell, contingently
   restricting its magnitude to the minimum of zero (0), and returns no
   value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (aref
      (tape-cells   tape)
      (tape-pointer tape))
    (max new-value 0))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-rotator-code (code)
  "Interprets the piece of Rotator source CODE and returns no value."
  (declare (type string code))
  (let ((commands (extract-the-commands code))
        (ip       0)
        (tape     (prepare-a-pristine-tape)))
    (declare (type program commands))
    (declare (type fixnum  ip))
    (declare (type Tape    tape))
    (connect-the-jump-points commands)
    (symbol-macrolet
        ((current-command
          (the Command
            (aref commands ip))))
      (declare (type Command current-command))
      (loop while (< ip (length commands)) do
        (case (command-symbol current-command)
          (#\>
            (advance-the-cell-pointer tape))
          (#\+
            (incf (current-cell-value tape)))
          (#\-
            (decf (current-cell-value tape)))
          (#\.
            (format T "~c"
              (code-char
                (current-cell-value tape)))
            (finish-output))
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell-value tape)
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          (#\[
            (when (zerop (current-cell-value tape))
              (setf ip
                (command-destination current-command))))
          (#\]
            (unless (zerop (current-cell-value tape))
              (setf ip
                (command-destination current-command))))
          (otherwise
            (error "The symbol \"~c\" at the position ~d was not ~
                    expected to occur."
              (command-symbol   current-command)
              (command-position current-command))))
        (advance-the-cell-pointer tape)
        (incf ip))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-the-rotator-code ",>>[>>.>>,>>]>>")

;;; -------------------------------------------------------

;; Print the digit sequence "1234" by first assigning each such digit
;; to one of the four first tape cells, with the fifth unit remaining
;; zero-valued for the subsequent loop, ere iterating through these
;; first cells and displaying their states, until the last, zero-valued
;; cell terminates the process.
(interpret-the-rotator-code
  "+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>
   >>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>
   >>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>+>>
   >>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>+>>
   +>>+>>+>>+>>+>>+>>+>>
   >>>>>>
   [>>.>>>>>]>>")
