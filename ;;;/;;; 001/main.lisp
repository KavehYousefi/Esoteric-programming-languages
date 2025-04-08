;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language ";;;", invented by the Esolang user "Yayimhere" and
;; presented on August 4th, 2024, its diorism's cynosure the operation
;; of a quadruple instruction set on an infinite tape of unsigned bytes,
;; its competences' circumference amplecting basic arithmetics and
;; line-based control flow helming.
;; 
;; 
;; Concept
;; =======
;; The ;;; programming language's foundry is edified upon the
;; perquisitions and modulations adhibited to an infinite tape of
;; unsigned byte-valued cells, the currently active unit among these
;; selected by a cell pointer, the instructions' homologation entailing
;; the memory's manipulation, as wells as the navigation across the
;; program's lines based on the line enumeration.
;; 
;; == THE MEMORY: A BILATERALLY INFINITE TAPE OF UNSIGNED BYTES ==
;; The program memory's conformation ensues from a catena of cells,
;; in their tally without any marches along both lateralities, and
;; everichon in this structure a salvatory to a scalar unsigned byte,
;; the occupied gamut naturally confined to the closed interval
;; [0, 255].
;; 
;; == A POINTER SELECTS THE CURRENTLY ACTIVE MEMORY CELL ==
;; At any instant in the program's execution, a dedicated cursor, the
;; "cell pointer", applies itself to the currently selected tape cell's
;; designation, defining in this agency the sole unit entalented with
;; an amenability to perquisitions and modulations.
;; 
;; The motile nature with which the pointer is invested expresses its
;; capacity in the homologation of stillatim translation along both of
;; the tape's axes.
;; 
;; == THE PROGRAM: A SEQUENCE OF INSTRUCTION-BEARING LINES ==
;; A ;;; program's designment ensues from the notion of its lines'
;; bailiwicks, everichon of these a woning to zero or more instructions.
;; These horizontal dispansion's zero-based indices capacitate the
;; navigation by the responsible operation.
;; 
;; 
;; Syntax
;; ======
;; The syntaxis as a component of the ;;; language's diorism establishes
;; a program's conformation as an ordered sequence of zero or more
;; lines, each such an arbitrary account of instructions' commorancy.
;; 
;; == A ;;; PROGRAM: A SEQUENCE OF LINES ==
;; A ;;; program's constitution distributed along zero or more lines
;; each effective member comprehending zero or more instructions. Blank
;; lines enjoy a tolerance which segues into their ultimate expungement
;; in terms of the language's consideration.
;; 
;; == INSTRUCTIONS: SINGLE SYMBOLS ==
;; A quadruple contingency applying to the operative warklumes, an
;; instruction identifier does not dispand ayond an aefauld symbol's
;; enumeration anenst its agnomination, desisting from the reliance upon
;; operands of any form.
;; 
;; == WHITESPACES ==
;; The participation of whitespaces enjoys a twifaced attendance, with
;; spaces and horizontal tabs instigating neither an erroneous
;; circumstance, nor rising beyond an aesthetical warklume's causatum.
;; 
;; Linebreak entities, on the other hand, constitute a requisite for
;; the demarcation of lines, in particular of a peisant attribute for
;; the diorism whence ensues the goto facility's target line enumeration
;; principle.
;; 
;; == COMMENTS ==
;; No provision for comments participates in this rendition of the
;; language.
;; 
;; 
;; Instructions
;; ============
;; The ;;; programming language wists of a quadruple membership in its
;; operative warklumes, enumerating specimens to whom the modification
;; of the cell pointer's location, the current cell's content, as well
;; as the control flow's airt is entrusted.
;; 
;; == OVERVIEW ==
;; A cursory mete of nortelry's adhibition shall comprise the following
;; apercu's vocation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ;       | If the current cell contains zero (0), translates the
;;           | cell pointer one step to the left; otherwise decrements
;;           | the current cell value by one (1).
;;           |---------------------------------------------------------
;;           | The following pseudocode diction applies:
;;           |   if memory[cellPointer] = 0 then
;;           |     cellPointer <- cellPointer - 1
;;           |   else
;;           |     memory[cellPointer] <- memory[cellPointer] - 1
;;           |   end if
;;   ..................................................................
;;   :       | If the current cell value is less than 255, increments
;;           | the current cell value by one (1), and subsequently
;;           | translates the cell pointer one step to the right;
;;           | otherwise translates the cell pointer one step to the
;;           | left and relocates the instruction pointer (IP) to the
;;           | start of the line amenable to the zero-based line number
;;           | which equals the just selected current cell's value.
;;           |---------------------------------------------------------
;;           | If the target line number transcends the program's
;;           | bournes, the execution immediately halts.
;;           |---------------------------------------------------------
;;           | The following pseudocode diction applies:
;;           |   if memory[cellPointer] < 255 then
;;           |     memory[cellPointer] <- memory[cellPointer] + 1
;;           |     cellPointer         <- cellPointer         + 1
;;           |   else
;;           |     cellPointer    <- cellPointer - 1
;;           |     ip.lineNumber  <- memory[cellPointer]
;;           |     ip.columNumber <- 0
;;           |   end if
;;   ..................................................................
;;   /       | If the current cell contains zero (0), relocates the
;;           | instruction pointer (IP) to the start of the program;
;;           | otherwise decrements the current cell value by one (1).
;;           |---------------------------------------------------------
;;           | The following pseudocode diction applies:
;;           |   if memory[cellPointer] = 0 then
;;           |     ip.lineNumber  <- 0
;;           |     ip.columNumber <- 0
;;           |   else
;;           |     memory[cellPointer] <- memory[cellPointer] - 1
;;           |   end if
;;   ..................................................................
;;   \       | If the current cell value does not represent a prime
;;           | number, translates the cell pointer one step to the
;;           | left; otherwise accompasses no causatum.
;;           |---------------------------------------------------------
;;           | The following pseudocode diction applies:
;;           |   if memory[cellPointer] is not a prime number then
;;           |     cellPointer <- cellPointer - 1
;;           |   end if
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation's manifestation is incarnated in the programming
;; language Common Lisp, the evaluation process a twifold exertion;
;; imprimis, goaded by the source string's reformulation as a vector of
;; non-empty lines, themselves character sequences, succeeded by the
;; actual interpretation stage.
;; 
;; A second dioristic proprium's acquisition refers to the program
;; memory's design, the infinite tape being attended to by a doubly
;; linked list of unsigned byte-valued nodes, each such representing a
;; cell, or, in a twissel of special cases, the amplecting sentinels.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-04
;; 
;; Sources:
;;   [esolang2024;;;]
;;   The Esolang contributors, ";;;", September 25th, 2024
;;   URL: "https://esolangs.org/wiki/;;;"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, and as such a commorant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism applied to whom enumerates, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member among these complies with the ELEMENT-TYPE,
   the default amounting to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (every
                #'(lambda (current-element)
                    (declare (type T current-element))
                    (typep current-element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype printing-policy ()
  "The ``printing-policy'' type enumerates the admissive modes of
   issuing the memory's display on the standard output conduit."
  '(member
    :print-perpetually
    :print-at-end))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for the
   ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   the diorism apportioned to which amplects the linefeed, newline,
   space, and horizontal tab specimens, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun instruction-character-p (candidate)
  "Determines whether the CANDIDATE represents an instruction character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate ";:/\\" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-simple-string (source)
  "Creates and returns a fresh ``simple-string'' representation of the
   SOURCE."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents an empty string, the diorism
   of which imposes a length of zero (0) characters, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (get-boolean-value-of
      (zerop
        (length source)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of prime number supputation operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-aliquot-p (dividend divisor)
  "Determines whether the DIVISOR represents an aliquot of the DIVIDEND,
   that is, the former divides the latter without a rest, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type octet dividend))
  (declare (type octet divisor))
  (the boolean
    (get-boolean-value-of
      (zerop (mod dividend divisor)))))

;;; -------------------------------------------------------

(defun assign-bit-value-to-primality (candidate)
  "Indagates the CANDIDATE regarding its membership among the prime
   numbers, returning on confirmation a bit value of one (1), otherwise
   zero (0)."
  (declare (type octet candidate))
  (the bit
    (or
      (and
        (< candidate 2)
        0)
      (loop
        for divisor
          of-type (integer 0 256)
          from    1
          to      candidate
        when (is-aliquot-p candidate divisor)
          count   1
          into    number-of-divisors
          of-type fixnum
        when (> number-of-divisors 2) do
          (return 0)
        finally
          (return 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of prime number table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-bit-vector 256) +PRIME-NUMBER-TABLE+))

;;; -------------------------------------------------------

(defun build-prime-number-table-for-bytes ()
  "Creates and returns a fresh ``simple-bit-vector'' assigning to each
   number in the unsigned byte interval [0, 255] a bit as a
   discriminator anent its membership among the prime numbers, with a
   bit value of one (1) serving to vouch for this compliance, while
   zero (0) refutes thilk."
  (the (simple-bit-vector 256)
    (make-array 256
      :element-type     'bit
      :initial-contents
        (loop
          for     prime-candidate of-type (integer 0 256) from 0 to 255
          collect (assign-bit-value-to-primality prime-candidate))
      :adjustable       NIL
      :fill-pointer     NIL)))

;;; -------------------------------------------------------

(defparameter +PRIME-NUMBER-TABLE+
  (build-prime-number-table-for-bytes)
  "A simple bit vector which affiliates with the octet values in the
   closed interval [0, 255] a bit as a designator of its primaility,
   the candidate concomitantly serving as the index into the sequence,
   while a bit value of one (1) ascertains its status as a prime number,
   counterdistinguished from a zero (0) bit as a signification of its
   failure to attend to this set's membership.")

;;; -------------------------------------------------------

(defun prime-number-p (candidate)
  "Determines whether the CANDIDATE subsumes into the species of prime
   numbers, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type octet candidate))
  (the boolean
    (get-boolean-value-of
      (plusp
        (sbit +PRIME-NUMBER-TABLE+ candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory cell.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-cell (value
                           &optional (previous NIL) (next NIL))))
  "The ``Cell'' class serves in the encapsulation of a memoyr cell,
   represented in compliance with the doubly linked node's notion,
   comprehending besides its amplected value a pointer to its
   contingent predecessor and a second to its possible successor cell."
  (value    (error "Missing cell value.")
            :type      octet
            :read-only NIL)
  (previous NIL
            :type      (or null Cell)
            :read-only NIL)
  (next     NIL
            :type      (or null Cell)
            :read-only NIL))

;;; -------------------------------------------------------

(defmethod print-object ((node Cell) (stream T))
  (declare (type Cell        node))
  (declare (type destination stream))
  (format stream "~:[x-~;<-~][~d]~:[-x~;->~]"
    (cell-previous node)
    (cell-value  node)
    (cell-next     node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Memory Cell Cell) *) insert-cell-between))

;;; -------------------------------------------------------

(defclass Memory ()
  ((header
    :initform      (make-cell 0)
    :reader        memory-header
    :type          Cell
    :documentation "The header sentinel cell, preceding any other cell
                    in the MEMORY, but not partaking of its effective
                    content.")
   (trailer
    :initform      (make-cell 0)
    :reader        memory-trailer
    :type          Cell
    :documentation "The trailer sentinel cell, succeeding any other cell
                    in the MEMORY, but not partaking of its effective
                    content.")
   (pointer
    :accessor      memory-pointer
    :type          Cell
    :documentation "The current selected cell.
                    ---
                    Only cells containing actual data are deemed
                    covenable as the POINTER's target; this excludes the
                    HEADER and trailer sentinel cells."))
  (:documentation
    "The ``Memory'' class furnishes an implementation of the ;;; program
     memory as a bilaterally infinite dispansion of unsigned byte-valued
     cells, realized by adminiculum of a doubly linked list."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((memory Memory) &key)
  "Ligates the MEMORY header and trailer cells, inserts a fresh
   zero-valued cell in the intermede, and returns no value."
  (declare (type Memory memory))
  (psetf
    (cell-next     (memory-header  memory)) (memory-trailer memory)
    (cell-previous (memory-trailer memory)) (memory-header  memory))
  (setf (memory-pointer memory)
    (insert-cell-between memory
      (memory-header  memory)
      (memory-trailer memory)))
  (values))

;;; -------------------------------------------------------

(defun prepare-pristine-memory ()
  "Creates and returns a fresh ``Memory'' object in its default state."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun insert-cell-between (memory predecessor successor)
  "Inserts a fresh zero-valued cell betwixt the PREDECESSOR and
   SUCCESSOR cells in the MEMORY, establishes the requisite vincula,
   and returns the thus created cell."
  (declare (type Memory memory))
  (declare (ignore      memory))
  (declare (type Cell   predecessor))
  (declare (type Cell   successor))
  (let ((new-cell (make-cell 0 predecessor successor)))
    (declare (type Cell new-cell))
    (psetf
      (cell-next     predecessor) new-cell
      (cell-previous successor)   new-cell)
    (the Cell new-cell)))

;;; -------------------------------------------------------

(defun insert-cell-at-left-end (memory)
  "Inserts a new zero-valued cell at the MEMORY's front and returns no
   value."
  (declare (type Memory memory))
  (insert-cell-between memory
    (memory-header memory)
    (cell-next
      (memory-header memory)))
  (values))

;;; -------------------------------------------------------

(defun insert-cell-at-right-end (memory)
  "Inserts a new zero-valued cell at the MEMORY's rear and returns no
   value."
  (declare (type Memory memory))
  (insert-cell-between memory
    (cell-previous
      (memory-trailer memory))
    (memory-trailer memory))
  (values))

;;; -------------------------------------------------------

(defun left-neighbor-cell-exists-p (memory)
  "Determines whether MEMORY's cell pointer contains a sinistral
   neighbor, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (eq
      (cell-previous
        (memory-pointer memory))
      (memory-header memory)))))

;;; -------------------------------------------------------

(defun right-neighbor-cell-exists-p (memory)
  "Determines whether MEMORY's cell pointer contains a dextral neighbor,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (eq
      (cell-next
        (memory-pointer memory))
      (memory-trailer memory)))))

;;; -------------------------------------------------------

(defun is-pointer-cell-p (memory candidate)
  "Determines whether the CANDIDATE cell represents the MEMORY's
   pointer, returning on confirmation a ``boolean'' of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (declare (type Cell   candidate))
  (the boolean
    (get-boolean-value-of
      (eq candidate
          (memory-pointer memory)))))

;;; -------------------------------------------------------

(defun is-trailer-cell-p (memory candidate)
  "Determines whether the CANDIDATE cell represents the MEMORY's trailer
   sentinel, returning on confirmation a ``boolean'' of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (declare (type Cell   candidate))
  (the boolean
    (get-boolean-value-of
      (eq candidate
          (memory-trailer memory)))))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (unless (right-neighbor-cell-exists-p memory)
    (insert-cell-at-right-end memory))
  (setf (memory-pointer memory)
    (cell-next
      (memory-pointer memory)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (unless (left-neighbor-cell-exists-p memory)
    (insert-cell-at-left-end memory))
  (setf (memory-pointer memory)
    (cell-previous
      (memory-pointer memory)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the unsigned byte value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (the octet
    (cell-value
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Store the NEW-VALUE in the MEMORY's current cell and returns no
   value."
  (declare (type octet  new-value))
  (declare (type Memory memory))
  (setf (cell-value (memory-pointer memory)) new-value)
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (memory)
  "Increments the MEMORY's current cell, if its state is strictly less
   than the upper byte bourne of 255, and returns no value."
  (declare (type Memory memory))
  (when (< (current-cell-value memory) 255)
    (incf (current-cell-value memory)))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (memory)
  "Decrements the MEMORY's current cell, if its state is strictly
   greater than lower upper byte bourne of zero (0), and returns no
   value."
  (declare (type Memory memory))
  (when (plusp (current-cell-value memory))
    (decf (current-cell-value memory)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (memory)
  "Determines whether the MEMORY's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-cell-value memory)))))

;;; -------------------------------------------------------

(defun current-cell-contains-prime-number-p (memory)
  "Determines whether the MEMORY's current cell contains a prime number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (prime-number-p
      (current-cell-value memory))))

;;; -------------------------------------------------------

(defun can-increment-current-cell-p (memory)
  "Determines whether the MEMORY's current cell value is below the
   admissible maximum of 255, which implies its amenability to at least
   one further incrementation, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (get-boolean-value-of
      (< (current-cell-value memory)
         255))))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory      memory))
  (declare (type destination stream))
  (loop
    initially
      (format stream "[Memory:")
    for current-cell
      of-type Cell
      =       (cell-next (memory-header memory))
      then    (cell-next current-cell)
    until (is-trailer-cell-p memory current-cell) do
      (format stream " ~:[~;*~]~d"
        (is-pointer-cell-p memory current-cell)
        (cell-value current-cell))
    finally
      (format stream "]")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((lines
    :initarg       :lines
    :initform      (error "Missing program lines.")
    :type          (simple-array simple-string (*))
    :reader        program-lines
    :documentation "The effective program lines as a vector of
                    strings.")
   (line-number
    :initform      0
    :accessor      program-line-number
    :type          fixnum
    :documentation "The zero-based index of the currently selected line
                    among the LINES.")
   (column-number
    :initform      0
    :accessor      program-column-number
    :type          fixnum
    :documentation "The zero-based index of the currently selected
                    character in the current line."))
  (:documentation
    "The ``Program'' class applies itself to the castaldy of a ;;;
     program, furnished as a vector strings, upon which an instruction
     pointer (IP), defined in terms of a zero-based line number and
     a column index from the same realm operates."))

;;; -------------------------------------------------------

(defun make-program (lines)
  "Creates and returns a fresh ``Program'' from the list of LINES."
  (declare (type (list-of string) lines))
  (the Program
    (make-instance 'Program :lines
      (coerce lines
        '(simple-array simple-string (*))))))

;;; -------------------------------------------------------

(defun get-current-program-line (program)
  "Returns the PROGRAM's currently selected line."
  (declare (type Program program))
  (the simple-string
    (aref
      (program-lines       program)
      (program-line-number program))))

;;; -------------------------------------------------------

(defun get-current-program-line-length (program)
  "Returns the length of the PROGRAM's currently selected line."
  (declare (type Program program))
  (the fixnum
    (length
      (get-current-program-line program))))

;;; -------------------------------------------------------

(defun get-current-token (program)
  "Returns the character in the PROGRAM's currently selected line."
  (declare (type Program program))
  (the character
    (schar
      (get-current-program-line program)
      (program-column-number    program))))

;;; -------------------------------------------------------

(defun current-program-line-is-exhausted-p (program)
  "Determines whether the PROGRAM's current line has been processed in
   its entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Program program))
  (the boolean
    (get-boolean-value-of
      (>= (program-column-number           program)
          (get-current-program-line-length program)))))

;;; -------------------------------------------------------

(defun move-instruction-pointer-to-line (program new-line-number)
  "Relocates the PROGRAM's instruction pointer to the beginning of the
   line amenable to the NEW-LINE-number and returns no value."
  (declare (type Program program))
  (declare (type fixnum  new-line-number))
  (psetf
    (program-line-number   program) new-line-number
    (program-column-number program) 0)
  (values))

;;; -------------------------------------------------------

(defun move-instruction-pointer-to-next-line (program)
  "Relocates the PROGRAM's instruction pointer to the beginning of the
   next line and returns no value."
  (declare (type Program program))
  (move-instruction-pointer-to-line program
    (1+ (program-line-number program)))
  (values))

;;; -------------------------------------------------------

(defun move-instruction-pointer-forward (program)
  "Advances the PROGRAM's instruction pointer to the subsequent token,
   contingently moving to the next line's inchoation if necessary, and
   returns no value."
  (declare (type Program program))
  (incf (program-column-number program))
  (when (current-program-line-is-exhausted-p program)
    (move-instruction-pointer-to-next-line program))
  (values))

;;; -------------------------------------------------------

(defun get-number-of-program-lines (program)
  "Returns the tally of lines comprising the PROGRAM."
  (declare (type Program program))
  (the fixnum
    (length
      (program-lines program))))

;;; -------------------------------------------------------

(defun program-is-exhausted-p (program)
  "Determines whether the PROGRAM has been traversed in its entirety,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Program program))
  (the boolean
    (get-boolean-value-of
      (>= (program-line-number         program)
          (get-number-of-program-lines program)))))

;;; -------------------------------------------------------

(defun get-program-line-at (program line-number)
  "Returns the PROGRAM line amenable to the zero-based LINE-NUMBER."
  (declare (type Program program))
  (declare (type fixnum  line-number))
  (the simple-string
    (aref (program-lines program) line-number)))

;;; -------------------------------------------------------

(defun get-program-line-length-at (program line-number)
  "Returns the number of character entailed in the PROGRAM line amenable
   to the zero-based LINE-NUMBER."
  (declare (type Program program))
  (declare (type fixnum  line-number))
  (the fixnum
    (length
      (get-program-line-at program line-number))))

;;; -------------------------------------------------------

(defun get-program-token-at (program line-number column-number)
  "Returns the character at the zero-based COLUMN-NUMBER of the PROGRAM
   lines amenable to the zero-based LINE-NUMBER."
  (declare (type Program program))
  (declare (type fixnum  line-number))
  (declare (type fixnum  column-number))
  (the character
    (schar
      (get-program-line-at program line-number)
      column-number)))

;;; -------------------------------------------------------

(defun instruction-pointer-is-located-at-p (program
                                            line-number
                                            column-number)
  "Determines whether the zero-based LINE-NUMBER and the zero-based
   COLUMN-NUMBER in conjunction designate the position of the PROGRAM's
   instruction pointer (IP), returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  line-number))
  (declare (type fixnum  column-number))
  (the boolean
    (get-boolean-value-of
      (and (= line-number   (program-line-number   program))
           (= column-number (program-column-number program))))))

;;; -------------------------------------------------------

(defmethod print-object ((program Program) (stream T))
  (declare (type Program     program))
  (declare (type destination stream))
  (format stream "~&Program:")
  (dotimes (line-number (get-number-of-program-lines program))
    (declare (type fixnum line-number))
    (format stream "~&~2tLine # ~d: " line-number)
    (dotimes (column-number
               (get-program-line-length-at program line-number))
      (declare (type fixnum column-number))
      (format stream "~:[~c~;[~c]~]"
        (instruction-pointer-is-located-at-p
          program line-number column-number)
        (get-program-token-at program line-number column-number)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-source-line (source line-number)
  "Determines whether the SOURCE, enuemrated with the one-based
   LINE-NUMBER, satisfies the stipulations levied against a valid line
   in a ;;; program, returning on confirmation the SOURCE purged from
   any ineffectuous content; otherwise signals an error of an
   unspecified type."
  (declare (type string source))
  (declare (type fixnum line-number))
  (the simple-string
    (convert-to-simple-string
      (with-output-to-string (validated-source)
        (declare (type string-stream validated-source))
        (loop
          for current-token    of-type character across source
          and current-position of-type fixnum    from   0 by 1
          do
            (cond
              ((whitespace-character-p current-token)
                NIL)
              ((instruction-character-p current-token)
                (write-char current-token validated-source))
              (T
                (error "Invalid character \"~c\" at position ~d ~
                        of line ~d."
                  current-token current-position line-number))))))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of ;;; SOURCE code and returns a ``Program''
   representation thereof."
  (declare (type string source))
  (the Program
    (make-program
      (with-input-from-string (input-stream source)
        (declare (type string-stream input-stream))
        (loop
          for current-line
            of-type (or null string)
            =       (read-line input-stream NIL NIL)
          and line-number
            of-type fixnum
            from    1
            by      1
          while current-line append
            (let ((validated-line
                    (validate-source-line current-line line-number)))
              (declare (type simple-string validated-line))
              (and
                (not (string-is-empty-p validated-line))
                (list validated-line))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        interpreter-program
    :type          Program
    :documentation "The ;;; program as a vector of line strings.")
   (memory
    :initform      (prepare-pristine-memory)
    :reader        interpreter-memory
    :type          Memory
    :documentation "The program memory molded into catena of unsigned
                    bytes.")
   (printing-policy
    :initarg       :printing-policy
    :initform      :print-at-end
    :reader        interpreter-printing-policy
    :type          printing-policy
    :documentation "Determines the instant at which the program memory
                    shall be issued to the standard output during the
                    PROGRAM's execution."))
  (:documentation
    "The ``Interpreter'' class' dever admits its incarnation in the
     capacity to accompass actual efficacy to a piece of ;;; source
     code whose provision complies with a vector of lines in string
     form."))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the subsequent
   position and returns no value."
  (declare (type Interpreter interpreter))
  (move-instruction-pointer-forward
    (interpreter-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun jump-to-line (interpreter new-line-number)
  "Relocates the INTERPRETER's instruction pointer (IP) to the start of
   the line amenable to the zero-based NEW-LINE-NUMBER and returns no
   value."
  (declare (type Interpreter interpreter))
  (move-instruction-pointer-to-line
    (interpreter-program interpreter)
    new-line-number)
  (values))

;;; -------------------------------------------------------

(defun jump-to-program-start (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the start of
   its program and returns no value."
  (declare (type Interpreter interpreter))
  (move-instruction-pointer-to-line
    (interpreter-program interpreter)
    0)
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter) (instruction (eql #\;)))
    (declare (type Interpreter interpreter))
    (declare (type character   instruction))
    (declare (ignore           instruction))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (if (current-cell-contains-zero-p memory)
        (move-cell-pointer-left memory)
        (decrement-current-cell memory)))
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql #\:)))
    (declare (type Interpreter interpreter))
    (declare (type character   instruction))
    (declare (ignore           instruction))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (cond
        ((can-increment-current-cell-p memory)
          (increment-current-cell      memory)
          (move-cell-pointer-right     memory)
          (advance-to-next-instruction interpreter))
        (T
          (move-cell-pointer-left memory)
          (print (list :jump (current-cell-value memory)))
          (jump-to-line interpreter
            (current-cell-value memory)))))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql #\/)))
    (declare (type Interpreter interpreter))
    (declare (type character   instruction))
    (declare (ignore           instruction))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (cond
        ((current-cell-contains-zero-p memory)
          (jump-to-program-start interpreter))
        (T
          (decrement-current-cell      memory)
          (advance-to-next-instruction interpreter))))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql #\\)))
    (declare (type Interpreter interpreter))
    (declare (type character   instruction))
    (declare (ignore           instruction))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (unless (current-cell-contains-prime-number-p memory)
        (move-cell-pointer-left memory)))
    (advance-to-next-instruction interpreter)
    (values)))

;;; -------------------------------------------------------

(defun print-memory-in-accord-with-policy (interpreter expected-policy)
  "If the INTERPRETER's printing policy concurs with the
   EXPECTED-POLICY, output's its memory to the standard output,
   otherwise accompasses no causatum, in any case returning no value."
  (declare (type Interpreter     interpreter))
  (declare (type printing-policy expected-policy))
  (when (eq (interpreter-printing-policy interpreter) expected-policy)
    (format T "~%~a"
      (interpreter-memory interpreter))
    (finish-output))
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Interprets the ;;; program consigned to the INTERPRETER's castaldy,
   prints the memory's state in compliance with the configured display
   policy, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program) interpreter
    (declare (type Program program))
    (loop until (program-is-exhausted-p program) do
      (process-instruction interpreter
        (get-current-token program))
      (print-memory-in-accord-with-policy
        interpreter
        :print-perpetually)))
  (print-memory-in-accord-with-policy interpreter :print-at-end)
  (values))

;;; -------------------------------------------------------

(defun interpret-|;;;| (code &key (printing-policy :print-at-end))
  "Interprets the piece of ;;; source CODE, obeying in matters of the
   memory's display the PRINTING-POLICY, and returns no value."
  (declare (type string          code))
  (declare (type printing-policy printing-policy))
  (execute-program
    (make-instance 'Interpreter
      :program         (parse-program code)
      :printing-policy printing-policy))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increment the first cell to the value two (2).
(interpret-|;;;| ":;:")

;;; -------------------------------------------------------

;; Generate the two first random numbers two (2) and three (3) in the
;; first two cells, relocate the cell pointer, prevent a recess to the
;; first cell based upon the prime number antecedent, and increment
;; the second cell value from three (3) to four (4).
(interpret-|;;;|
  ":;:
   :;:;:
   ;\\:")

;;; -------------------------------------------------------

;; Gradually increment all cells to the state of one (1) employing the
;; program restart operation "/".
(interpret-|;;;|
  ":/"
  :printing-policy :print-perpetually)

;;; -------------------------------------------------------

;; Utilize the conditional jump facility to infinite preprend two
;; columns, the sinistral of which furnishes a counter from inclusive
;; zero (0) to inclusive 255, while the dextral always bears the value
;; zero (0).
;; 
;; The ensuing memory layout follows the forbisen
;; 
;;   counter1 0
;;   counter2 0 counter1 0
;;   counter3 0 counter2 0 counter 1 0
;; 
;; where "counter[i]" increments from inclusive zero (0) to inclusive
;; 255.
(interpret-|;;;|
  "\\ :;:;:;:;:;:;:;:;:;:;:;:;:;:;:;: /"
  :printing-policy :print-perpetually)
