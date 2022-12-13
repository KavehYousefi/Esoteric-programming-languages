;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Edge", presented by the Esolang user Michael Gianfreda in
;; the year 2013, founded upon Urban Mueller's "brainfuck" as a
;; derivative destitute of input and output facilities, yet capable of
;; the remaining capabilities' emulation utilizing four symbols in
;; champarty with two manipulable switches.
;; 
;; 
;; Concept
;; =======
;; The Edge esoteric programming language constitutes a derivative of
;; brainfuck, its cleronomy encompassing the application of single
;; character instructions unto a one-dimesional memory composed of
;; integer-valued cells. Edge, however, is vested with a dioristic
;; approach to its stock-father's octuple instruction set's curtailment
;; to a moeity in their tally, doing so by discarding the input and
;; output conduits, and introducing two switches as confederates in the
;; substitution of the four remaining cell pointer and cell value
;; manipulators.
;; 
;; == BRAINFUCK: MANY CELLS, FEW COMMANDS, YET TURING-COMPLETE ==
;; The original brainfuck specification lay its amplex around the
;; following characteristics:
;; 
;;   (a) It operates on a tape-like memory composed of 30,000 cells,
;;       amenable to non-negative indices in the range [0; 29,000].
;;       Subsequent iterations have administered a conspicable mete of
;;       liberality in permitting a theoretically infinite tally,
;;       contingently homologating even negative indices.
;;   (b) Each cell is ordained to the wike of a salvatory to a single
;;       unsigned byte datum, thus an occupant of the range [0, 255].
;;       This caract, too, has been expanded in order to accommodate
;;       any sign and magnitude of integers.
;;   (c) A cursor, known as the "cell pointer", serves to designate at
;;       any instant during the program the currently selected cell ---
;;       the only inspectable and configurable entity during a behest.
;;       This pointer responds to certain operations for its stepwise
;;       translation to the left and right neighbor.
;;   (d) As stated above, in (c), the current cell constitutes the
;;       exclusive item of manipulation. The instructions to the
;;       program's avail are very restricted, encompassing the
;;       incrementing by one and decrementing by the same amount per
;;       invocation.
;;   (e) Each of the eight commands accommodated are specified by a
;;       single character. These are enumerated as follows:
;;       
;;         ------------------------------------------------------------
;;         Command | Effect
;;         --------+---------------------------------------------------
;;         >       | Moves the cell pointer one step to the right.
;;         ............................................................
;;         <       | Moves the cell pointer one step to the left.
;;         ............................................................
;;         +       | Increments the current cell value by one.
;;         ............................................................
;;         -       | Decrements the current cell value by one.
;;         ............................................................
;;         .       | Outputs the characters associated with the current
;;                 | cell value interpreted as an ASCII code.
;;         ............................................................
;;         ,       | Inputs a character and stores its ASCII code in
;;                 | the current cell.
;;         ............................................................
;;         [       | If the current cell value equals zero (0), jumps
;;                 | forward to the position immediately following
;;                 | the matching "]". Otherwise, advances as usual.
;;         ............................................................
;;         ]       | If the current cell value does not equal zero (0),
;;                 | jumps back to the position immediately following
;;                 | the matching "[". Otherwise, advances as usual.
;;         ------------------------------------------------------------
;;   
;;   (f) Any other character will be ignored, a tolerance which admits
;;       comments as embedded contents.
;; 
;; A thing of amazement that wones in this ostentatiously impuissant
;; language appertains to the fact that, if, permissive to an unbounded
;; memory size (see point -> (a)), or one such with a minimum of three
;; cells that, however, are not imposed in their content's sign and
;; magnitude (see point -> (b)), brainfuck is rendered Turing-complete,
;; equipotent with any Universal Turing machine.
;; 
;; == EDGE: TWO REMOVALS AND TWO RETENTIONS ==
;; Edge's bisected cardinality when referencing its inherited
;; instruction set ought to be preceded by mentioning the input (",")
;; and output operation's omission, already curtailing the octuple to a
;; sextuple size --- a cambistry that, on the other hand, significantly
;; abates its potentials.
;; 
;; The interactive instruments' pretermission does not experience a
;; duplication in the bequeathed remnants; in particular, the jump
;; facilities "[" and "]" can claim verbatim appropriation.
;; 
;; == A TWAIN OF SWITCHES FOR A QUADRUPLE OF OPERATIONS ==
;; The vestigial account of commands operating on the pointer or the
;; therewithal referenced current cell, which comprehends the four items
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   +       | Increments the current cell value by one.
;;   ..................................................................
;;   -       | Decrements the current cell value by one.
;;   ------------------------------------------------------------------
;; 
;; is targeted by the diorism commorant in Edge: its two coefficient
;; switches
;; 
;;   - destination
;;   - direction
;; 
;; whose states in combination exhaust the superseded commands.
;; 
;; == THE DESTINATION SWITCH: POINTER VERSUS MEMORY ==
;; The "destination" switch appertains to the discrepancy betwixt the
;; cell pointer's translation via ">" and "<", and the currently active
;; memory cell's increment or reduction through "+" or "-",
;; respectively.
;; 
;; The following states are enumerated for this switch:
;; 
;;   ------------------------------------------------------------------
;;   Destination state | Effect
;;   ------------------+-----------------------------------------------
;;   pointer           | Moves the cell pointer right or left.
;;   ..................................................................
;;   memory            | Increments or decrements the current memory
;;                     | cell.
;;   ------------------------------------------------------------------
;; 
;; == THE DIRECTION SWITCH: POSITIVE VERSUS NEGATIVE ACTION ==
;; Whereas the destination selects the object to manipulate --- either
;; the cell pointer or the current cell itself ---, the "direction"
;; switch serves as a discriminator betwixt the application of a
;; "positive" or a "negative" operation.
;; 
;;   ------------------------------------------------------------------
;;   Direction state | Effect
;;   ----------------+-------------------------------------------------
;;   positive        | For the cell pointer: moves it right.
;;                   | For the current cell: increments it.
;;   ..................................................................
;;   negative        | For the cell pointer: moves it left.
;;                   | For the current cell: decrements it.
;;   ------------------------------------------------------------------
;; 
;; Succeeding the incipient intelligence anenst the switches, their
;; possible states, and their ensuing causata, with an emphasis on their
;; interrelations, shall be conveyed in the following table:
;; 
;;   ------------------------------------------------------------------
;;   Switch      | States   | Effect
;;   ------------+----------+------------------------------------------
;;   destination | pointer  | The cell pointer shall be moved.
;;               |-----------------------------------------------------
;;               | memory   | The current cell value shall be altered.
;;   ..................................................................
;;   direction   | positive | If operating on the cell pointer, it
;;               |          | shall be translated dextrally.
;;               |          |------------------------------------------
;;               |          | If operating on the current cell, it
;;               |          | shall be incremented.
;;               |-----------------------------------------------------
;;               | negative | If operating on the cell pointer, it
;;               |          | shall be translated sinistrally.
;;               |          |------------------------------------------
;;               |          | If operating on the current cell, it
;;               |          | shall be decrement.
;;   ------------------------------------------------------------------
;; 
;; A foreshortened representation juxtaposing the four switch
;; combinations shall be extended in the table below:
;; 
;;   ------------------------------------------------------------------
;;   Destination | Direction | Effect
;;   ------------+-----------+-----------------------------------------
;;   pointer     | positive  | Moves the cell pointer one step to the
;;               |           | right.
;;   ..................................................................
;;   pointer     | negative  | Moves the cell pointer one step to the
;;               |           | left.
;;   ..................................................................
;;   memory      | positive  | Increments the current cell value by
;;               |           | one.
;;   ..................................................................
;;   memory      | negative  | Decrements the current cell value by
;;               |           | one.
;;   ------------------------------------------------------------------
;; 
;; If pursuing the order of the switches' transitions, the following
;; rearrangement applies to the aboon treatment, without its contents'
;; modifications:
;; 
;;   ------------------------------------------------------------------
;;   Destination | Direction | Effect
;;   ------------+-----------+-----------------------------------------
;;   pointer     | positive  | Moves the cell pointer one step to the
;;               |           | right.
;;   ..................................................................
;;   memory      | positive  | Increments the current cell value by
;;               |           | one.
;;   ..................................................................
;;   pointer     | negative  | Moves the cell pointer one step to the
;;               |           | left.
;;   ..................................................................
;;   memory      | negative  | Decrements the current cell value by
;;               |           | one.
;;   ------------------------------------------------------------------
;; 
;; == DESTINATION SWITCHES DIRECTLY, DIRECTION INDIRECTLY ==
;; The Edge command responsible for the switching process manifests in
;; the "%" token. However, only the destination may be altered
;; immediately, whereas the direction depends upon the transition being
;; from the former's memory to pointer state.
;; 
;; Starting at a program's inchoation in the cell pointer state, each
;; encounter with the "%" command switches the destination from the
;; pointer to the memory mode, or athwart.
;; 
;; On the other hand, the direction switch, initialized in the positive
;; state, alters its mode to the opposite only if the "%" invocation is
;; transitioning the destination from the memory to the pointer mode.
;; 
;; An exemplary sequence of invocations, restricted to significant
;; boundary events and encounters with the switch command "%", shall
;; convey a deeper gnarity involving the destination and direction modes
;; and their vincula.
;; 
;;   ------------------------------------------------------------------
;;   Step | Event           | Destination | Direction
;;   -----+-----------------+-------------+----------------------------
;;   0    | Program start   | pointer     | positive
;;   ..................................................................
;;   1    | "%" encountered | memory      | positive
;;   ..................................................................
;;   2    | "%" encountered | pointer     | negative
;;   ..................................................................
;;   3    | "%" encountered | memory      | negative
;;   ..................................................................
;;   4    | "%" encountered | pointer     | positive
;;   ..................................................................
;;   [...]
;;   ------------------------------------------------------------------
;; 
;; Please note how the direction changes each time the destination mode
;; transitions from "memory" to "pointer" (steps 1--2 and 3--4), but
;; abstains from modifications during a motion from "pointer" to
;; "memory" (steps 0--1 and 2--3).
;; 
;; As a summarizing exposition, the following transitions hold, with the
;; table's incipient row constituting the program's initial state, and
;; each subsequent line defining the configuration following a "%"
;; command's encounter; concluding with the desinent line whose
;; collision with a "%" will return to the table's start, that is, the
;; arrangement as determined by the first row.
;; 
;;   ------------------------------------------------------------------
;;   Destination | Direction
;;   ------------+-----------------------------------------------------
;;   pointer     | positive
;;   ..................................................................
;;   memory      | positive
;;   ..................................................................
;;   pointer     | negative
;;   ..................................................................
;;   memory      | negative
;;   ------------------------------------------------------------------
;; 
;; An equivalent expression, yet of enhancement in lucidity, shall be
;; accoutred by the next table, where the sinistral column twain
;; designates the current switch states' synartesis, accompanied in its
;; dextral laterality by the new modes if encountering a "%" command:
;; 
;;   ------------------------------------------------------------------
;;   Current     | Current   || Next        | Next
;;   destination | direction || destination | direction
;;   ------------+-----------||-------------+--------------------------
;;   pointer     | positive  || memory      | positive
;;   ........................||........................................
;;   memory      | positive  || pointer     | negative
;;   ........................||........................................
;;   pointer     | negative  || memory      | negative
;;   ........................||........................................
;;   memory      | negative  || pointer     | positive
;;   ------------------------------------------------------------------
;; 
;; Please note here how the dextral column jumelle always correlates to
;; the sinistral pair in the subsequent row; and also how the ultimate
;; line's dextral twain partakes of congruency with the two left upper
;; cells in the table, as resorting to the initial configuration.
;; 
;; 
;; Instructions
;; ============
;; The Edge programming language's instruction set constitutes a
;; composition of a quadruple cardinality, eschewing brainfuck's input
;; and output conduits, yet retaining its jump commands verbatim, while
;; substituting the remaining four cell pointer and value manipulators
;; by a twain comprised of a mode switch operating in conjunction with a
;; single applicator.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer at least a cursory mete of
;; gnarity regarding Edge's instructions:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   %       | Switches the destination mode. If it had hitherto been
;;           | residing in the memory state, also switches the direction
;;           | mode.
;;           | At the start of the program, the destination resides at
;;           | the pointer mode, and the direction assumes the positive
;;           | state.
;;           |---------------------------------------------------------
;;           | If the destination is in the pointer mode: transitions
;;           | into the memory mode, but does not alter the direction
;;           | state.
;;           |---------------------------------------------------------
;;           | If the destination is in the memory mode: transitions
;;           | into the pointer mode, and concomitantly switches the
;;           | direction state.
;;   ..................................................................
;;   *       | Depending on the destination and direction modes, either
;;           | translates the cell pointer or adjusts the current cell.
;;           |---------------------------------------------------------
;;           | If the destination is in the pointer mode and the
;;           | direction is positive: moves the cell pointer one step
;;           | to the right.
;;           | This behavior is paregal to brainfuck's ">" command.
;;           |---------------------------------------------------------
;;           | If the destination is in the pointer mode and the
;;           | direction is negative: moves the cell pointer one step
;;           | to the left.
;;           | This behavior is paregal to brainfuck's "<" command.
;;           |---------------------------------------------------------
;;           | If the destination is in the memory mode and the
;;           | direction is positive: increments the current cell value
;;           | by one.
;;           | This behavior is paregal to brainfuck's "+" command.
;;           |---------------------------------------------------------
;;           | If the destination is in the memory mode and the
;;           | direction is negative: decrements the current cell value
;;           | by one.
;;           | This behavior is paregal to brainfuck's "-" command.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer forward to the first position
;;           | immediately following the matching "]".
;;           | Otherwise, proceeds as usual.
;;           | This command has been appropriated verbatim from
;;           | brainfuck.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer back to the first position
;;           | immediately following the matching "[".
;;           | Otherwise, proceeds as usual.
;;           | This command has been appropriated verbatim from
;;           | brainfuck.
;;   ------------------------------------------------------------------
;; 
;; The table already consigned to our acquaintance shall be reiterated
;; at this position, defining the coefficiency of the "%" and "*"
;; commands in respondency to the two switches' various configurations:
;; 
;;   ------------------------------------------------------------------
;;   Destination | Direction | Effect
;;   ------------+-----------+-----------------------------------------
;;   pointer     | positive  | Moves the cell pointer one step to the
;;               |           | right.
;;   ..................................................................
;;   pointer     | negative  | Moves the cell pointer one step to the
;;               |           | left.
;;   ..................................................................
;;   memory      | positive  | Increments the current cell value by
;;               |           | one.
;;   ..................................................................
;;   memory      | negative  | Decrements the current cell value by
;;               |           | one.
;;   ------------------------------------------------------------------
;; 
;; In regard to the potentials inhabiting the Edge language to emulate
;; its ancestor brainfuck's cell pointer and memory manipulation
;; capabilities, the following combinations of destination and direction
;; switch hold:
;; 
;;   ------------------------------------------------------------------
;;             Edge          |
;;   ------------------------| brainfuck
;;   Destination | Direction |
;;   ------------+-----------+-----------------------------------------
;;   pointer     | positive  | >
;;   ..................................................................
;;   pointer     | negative  | <
;;   ..................................................................
;;   memory      | positive  | +
;;   ..................................................................
;;   memory      | negative  | -
;;   ------------------------------------------------------------------
;; 
;; The following table shall illustrate the equivalency, where possible,
;; of brainfuck commands in Edge:
;; 
;;   ------------------------------------------------------------------
;;             |                         Edge
;;   brainfuck |-------------------------------------------------------
;;             | Destination               | Direction
;;   ----------+---------------------------+---------------------------
;;   >         | pointer                   | positive
;;   ..................................................................
;;   <         | pointer                   | negative
;;   ..................................................................
;;   +         | memory                    | positive
;;   ..................................................................
;;   -         | memory                    | negative
;;   ..................................................................
;;   .         | None implemented.
;;   ..................................................................
;;   ,         | None implemented.
;;   ..................................................................
;;   [         | Same as brainfuck, independent of the switches.
;;   ..................................................................
;;   ]         | Same as brainfuck, independent of the switches.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-09
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Edge"
;;   -> "https://esolangs.org/wiki/Brainfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping from jump start positions
   to their respective end locations inside of a piece of Edge source
   code, and vice versa, implemented in the form of a hash table
   associating fixnum keys with equivalently typed values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a mapping of signed integer cell
   indices to cell values occupying the same range, manifesting in the
   form of a hash table associating integer keys with integer values."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type enumerates the recognized subjects for Edge
   operation targets."
  '(member :pointer :memory))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized operation classes to
   apply unto Edge targets."
  '(member :positive :negative))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates those Edge commands which depend
   upon the destination and direction switch."
  '(member :move-right :move-left :increment :decrement))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents an theoretically infinite linear
   arrangement of integer-valued cells, amenable to manipulations of its
   cell pointer and, ensuing from the same's wike, the currently active
   cell entity."
  ;; Associates each cell index with the value stored at the same.
  (cells   (make-hash-table :test #'eql) :type cell-table)
  ;; The currently select cell's index (key) in the CELLS.
  (pointer 0                             :type integer)
  ;; The inclusive minimum index (key) in the CELLS.
  (start   0                             :type integer)
  ;; The inclusive maximum index (key) in the CELLS.
  (end     0                             :type integer))

;;; -------------------------------------------------------

(defun memory-update-bounds (memory)
  "Updates the MEMORY's internally managed minimum and maximum cell
   indices in regard to its cell pointer and returns the modified
   MEMORY."
  (declare (type Memory memory))
  (setf (memory-start memory)
        (min (memory-start   memory)
             (memory-pointer memory)))
  (setf (memory-end memory)
        (max (memory-end     memory)
             (memory-pointer memory)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Relocates the MEMORY's cell pointer one step to the left and returns
   the modified MEMORY."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (memory-update-bounds memory)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Relocates the MEMORY's cell pointer one step to the right and returns
   the modified MEMORY."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (memory-update-bounds memory)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the integer
    (gethash (memory-pointer memory) (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns the
   modified MEMORY."
  (declare (type integer new-value)) 
  (declare (type Memory  memory))
  (setf (gethash (memory-pointer memory) (memory-cells memory) 0)
        new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell, contingently wrapping its value
   around into the byte range [0, 255], and returns the modified
   MEMORY."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell, contingently wrapping its value
   around into the byte range [0, 255], and returns the modified
   MEMORY."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-print (memory)
  "Prints to the standard output the MEMORY's size and cell values and
   returns the MEMORY."
  (declare (type Memory memory))
  (format T "~&The memory consists of ~d cell~:*~p:"
    (hash-table-count (memory-cells memory)))
  (loop
    for cell-index
      of-type fixnum
      from    (memory-start memory)
      to      (memory-end   memory)
    do
      (format T "~&~2tcells[~d] = ~d" cell-index
        (gethash cell-index
          (memory-cells memory) 0)))
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Creates and returns a jump table for the piece of Edge CODE,
   associating with each forward jump position in the source the
   respective back jump location, and vice versa."
  (declare (type string code))
  (let ((jump-table    (make-hash-table :test #'eql))
        (forward-jumps NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jumps))
    
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0
      do
        (case token
          (#\[
            (push position forward-jumps))
          (#\]
            (if forward-jumps
              (let ((start (pop forward-jumps))
                    (end   position))
                (declare (type fixnum start))
                (declare (type fixnum end))
                (setf (gethash start jump-table) end)
                (setf (gethash end   jump-table) start))
              (error "Unmatched \"]\" at position ~d." position)))
          (otherwise
            NIL)))
    
    (when forward-jumps
      (error "Unmatched \"[\"s at positions ~{~d~^, ~}." forward-jumps))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun switch-destination (current-destination)
  "Returns the destination forming the CURRENT-DESTINATION's
   counterpart."
  (declare (type destination current-destination))
  (the destination
    (case current-destination
      (:pointer  :memory)
      (:memory   :pointer)
      (otherwise (error "Invalid destination: ~s."
                   current-destination)))))

;;; -------------------------------------------------------

(defun switch-direction (current-direction)
  "Returns the direction forming the CURRENT-DIRECTION's counterpart."
  (declare (type direction current-direction))
  (the direction
    (case current-direction
      (:positive :negative)
      (:negative :positive)
      (otherwise (error "Invalid direction: ~s." current-direction)))))

;;; -------------------------------------------------------

(defun get-instruction (destination direction)
  "Returns the instruction associated with this combination of the
   DESTINATION and DIRECTION, or signals an error if no such
   correspondence could be detected."
  (declare (type destination destination))
  (declare (type direction   direction))
  (flet
      ((matches-combination-p (expected-destination
                               expected-direction
                               associated-instruction)
        "Checks whether the DESTINATION matches the EXPECTED-DESTINATION
         and the DIRECTION matches the EXPECTED-DIRECTION, on
         confirmation returning the ASSOCIATED-INSTRUCTION, otherwise
         ``NIL''."
        (declare (type destination expected-destination))
        (declare (type direction   expected-direction))
        (declare (type instruction associated-instruction))
        (the (or null instruction)
          (when (and (eq destination expected-destination)
                     (eq direction   expected-direction))
            associated-instruction))))
    (the instruction
      (or
        (matches-combination-p :pointer :positive :move-right)
        (matches-combination-p :pointer :negative :move-left)
        (matches-combination-p :memory  :positive :increment)
        (matches-combination-p :memory  :negative :decrement)
        (error "Invalid combination of destination ~s and direction ~s."
          destination direction)))))

;;; -------------------------------------------------------

(defun interpret-Edge (code
                       &key (initial-destination :pointer)
                            (initial-direction   :positive))
  "Interprets the piece of Edge CODE, with the program initially
   assuming the INITIAL-DESTINATION and the INITIAL-DIRECTION, finally
   prints the memory content, and returns no value."
  (declare (type string      code))
  (declare (type destination initial-destination))
  (declare (type direction   initial-direction))
  
  (when (plusp (length code))
    (let ((ip          0)
          (jump-table  (build-jump-table code))
          (destination initial-destination)
          (direction   initial-direction)
          (memory      (make-memory)))
      (declare (type fixnum      ip))
      (declare (type jump-table  jump-table))
      (declare (type destination destination))
      (declare (type direction   direction))
      (declare (type Memory      memory))
      
      (symbol-macrolet
          ((has-next-token-p
            (the boolean
              (not (null
                (array-in-bounds-p code ip)))))
           (current-token
            (the character
              (char code ip))))
        (declare (type boolean   has-next-token-p))
        (declare (type character current-token))
        
        (loop while has-next-token-p do
          (case current-token
            ;; Switch the destination and direction.
            (#\%
              ;; When switching from the memory to the pointer
              ;; DESTINATION, concomitantly switch the DIRECTION.
              (when (eq destination :memory)
                (setf direction (switch-direction direction)))
              
              (setf destination (switch-destination destination))
              (incf ip))
            
            ;; Either move the cell pointer or increment/decrement the
            ;; current memory cell, depending upon the DESTINATION and
            ;; DIRECTION switches.
            (#\*
              (case (get-instruction destination direction)
                (:move-right (memory-move-right memory))
                (:move-left  (memory-move-left  memory))
                (:increment  (memory-increment  memory))
                (:decrement  (memory-decrement  memory))
                (otherwise
                  (error "Invalid combination of destination ~s and ~
                          direction ~s."
                    destination direction)))
              (incf ip))
            
            ;; If the current cell contains zero (0), relocate the
            ;; instruction pointer (IP) to the matching "]"; otherwise,
            ;; simply advance to the next position.
            (#\[
              (if (zerop (memory-current-cell memory))
                (setf ip (gethash ip jump-table))
                (incf ip)))
            
            ;; If the current cell does not contain zero (0), relocate
            ;; the instruction pointer (IP) to the matching "[";
            ;; otherwise simply advance to the next position.
            (#\]
              (if (not (zerop (memory-current-cell memory)))
                (setf ip (gethash ip jump-table))
                (incf ip)))
            
            (otherwise
              (incf ip)))))
      
      ;; Terminate by printing the complete program MEMORY.
      (memory-print memory)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the value of the first cell to 1.
(interpret-Edge "%*")

;;; -------------------------------------------------------

;; Set first cell to the value ten (10), then move its content to the
;; dextral neighbor, while decrementing the first cell to zero (0).
(interpret-Edge
  "
  Set mode to (memory, positive) and increment by ten
  %**********
  %%
  [
    Set mode to (pointer, positive) and move pointer right
    %*
    Set mode to (memory, positive) and increment by one
    %*
    Set mode to (pointer, negative) and move pointer left
    %*
    Set mode to (memory, negative) and decrement by one
    %*
  ]
  ")

;;; -------------------------------------------------------

;; Replicate in the first thirteen memory cells the ASCII codes of the
;; text "Hello, World!", that is:
;;   72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33.
(interpret-Edge
  "
  Change from positive pointer to positive memory mode
  %************************************************************************

  Change from positive memory to negative pointer
  %
  Change from negative pointer to negative memory
  %
  Change from negative memory to positive pointer
  %
  Advance pointer to next cell
  *
  Change from positive pointer to positive memory mode
  %*****************************************************************************************************

  %%%*%************************************************************************************************************
  %%%*%************************************************************************************************************
  %%%*%***************************************************************************************************************
  %%%*%********************************************
  %%%*%********************************
  %%%*%***************************************************************************************
  %%%*%***************************************************************************************************************
  %%%*%******************************************************************************************************************
  %%%*%************************************************************************************************************
  %%%*%****************************************************************************************************
  %%%*%*********************************
  ")

;;; -------------------------------------------------------

;; The preceding "Hello, World!" program deprived of its comments.
(interpret-Edge
  "
  %************************************************************************
  %%%*%*****************************************************************************************************
  %%%*%************************************************************************************************************
  %%%*%************************************************************************************************************
  %%%*%***************************************************************************************************************
  %%%*%********************************************
  %%%*%********************************
  %%%*%***************************************************************************************
  %%%*%***************************************************************************************************************
  %%%*%******************************************************************************************************************
  %%%*%************************************************************************************************************
  %%%*%****************************************************************************************************
  %%%*%*********************************
  ")
