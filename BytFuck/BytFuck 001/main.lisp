;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BytFuck", created by the Esolang user "DmilkaSTD" and
;; presented on May 12th, 2020, its haecceity's commorancy the
;; extension of Urban Mueller's "brainfuck" to a bit-level via a bit
;; pointer which at any instant selects the currently active cell
;; byte's amenable bit.
;; 
;; 
;; Concept
;; =======
;; The BytFuck programming language establishes an extension of the
;; brainfuck substratum, its supererogation airted at the operation on
;; a bit tier, in addition to its entheus' byte-level focus, deploying
;; a second pointer, the "bit pointer", parhedral to the cell cursor
;; for the designation of a currently active bit in the
;; contemporaneously selected cell's octet content.
;; 
;; == THE TAPE: A UNILATERAL INFINITE DISPANSION OF BYTES ==
;; A verbatim appropriation from the brainfuck clerenomy, BytFuck naits
;; a catena of cells, originating from a left margin, but without any
;; bournes along the dextral axis, each such a scalar unsigned byte
;; values salvatory, admitting objects in the integral range [0, 255].
;; 
;; Upon any of its marches' transgression, the cell state wraps around
;; to the opposite extremum, resorting from the maximum of 255 to zero
;; (0), and proceeding in the counterdistinguished case in the
;; widdershins direction.
;; 
;; == TWO POINTERS OPERATE ON THE TAPE ==
;; A deviation in the rather plain selection mechanism innate to
;; brainfuck, the scion ordains a twain of pointers: one pledging lealty
;; to the stock-father's byte-level validity; the second, a finer tier's
;; tenant, commorant in the lower realm of the bits.
;; 
;; == THE CELL POINTER: SELECTING THE CURRENTLY ACTIVE BYTE ==
;; The traditional cell pointer's services retain their acquainted
;; mode, inchoating in the leftmost cell, and entalented with the
;; liberty to perambulate the tape along both axes, except for the
;; sinistral margin's impediment. By adminiculum of this effort, the
;; currently selected cell is memorized, the aefauld unit endowed with
;; the amenability to modifications.
;; 
;; == THE BIT POINTER: SELECTING THE ACTIVE BIT OF THE CURRENT BYTE ==
;; A second, paregal pointer constitutes a supererogation autochthonous
;; to BytFuck, yet forinsecal to the entheus, selecting the
;; contemporaneously active bit in the current cell's octet. 
;; 
;; This pointer's origin locates it in the lowest (rightmost) bit of
;; the first selected cell's byte, which, as mentioned aboon, relates
;; to the tape's leftmost bit. Stillatim modulations of the position
;; homologate sinistral as well as dextral translations, which may
;; cross the cells' boundaries; by doing so, potentially alterating the
;; cell pointer's location.
;; 
;; == NOTATIONAL PRINCIPLES APPERTAINING TO BITS AND BYTES ==
;; The notions applicable to an octet's enumeration on the bit tier
;; conflate with the following diagram, which also comprehends the
;; least significant bit (LSB) and most significant bit (MSB) markers:
;; 
;;   +---------------------------------------------------------------+
;;   | Bit position ||  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
;;   |--------------||-----+-----+-----+-----+-----+-----+-----+-----|
;;   | Role         || MSB |     |     |     |     |     |     | LSB |
;;   +---------------------------------------------------------------+
;; 
;; A separate cell's conformation shall be the following illustration's
;; cynosure:
;; 
;;   |- single cell -|
;;   
;;   [7 6 5 4 3 2 1 0]
;;    ^             ^
;;    |             |
;;    last bit      first bit
;; 
;; == BIT POINTER TRANSLATIONS TRANSCEND CELL BOUNDARIES ==
;; A tantamount of the cell pointer's translation principles,
;; transferred onto the finer bit stratum, the bit pointer may be
;; translated sinistrally and dextrally in gradual steps. Upon the
;; current cell bournes' transgression, both the bit pointer and the
;; coefficient cell pointer change to the respective neighbor cell.
;; 
;; In the forbisen alow, the bit pointer's woning is accommodated in
;; the leftmost (highest) position of the current cell:
;; 
;;   Left neighbor      Current cell
;;   [7 6 5 4 3 2 1 0]  [7 6 5 4 3 2 1 0]
;;                       *
;; 
;; A sinistral bit motion, actuated via the "≤" instruction,
;; subsequently transcends the current cell's left bourne, carrying the
;; cell pointer to the sinistral neighbor as the new selected cell,
;; while concomitantly this new cynosure's rightmost (lowest) bit is
;; rendered the bit pointer's updated commorancy:
;; 
;;   Left neighbor      Current cell
;;   [7 6 5 4 3 2 1 0]  [7 6 5 4 3 2 1 0]
;;                  *<---|
;; 
;; The conjunction principles governing the translation relations
;; betwixt bits and cells, of course, applies to the athwart airt, too.
;; Given a cell whose rightmost (lowest) bit provides the bit pointer's
;; reference
;; 
;;   Current cell       Right neighbor
;;   [7 6 5 4 3 2 1 0]  [7 6 5 4 3 2 1 0]
;;                  *
;;   
;; a dextral step, by adminiculum of the "≥" command, conditions the
;; right neighbor cell's selection under the cell pointer, as well as
;; the leftmost (highest) position's assignment to the bit pointer:  
;;   
;;   Current cell       Right neighbor
;;   [7 6 5 4 3 2 1 0]  [7 6 5 4 3 2 1 0]
;;                  |--->*
;; 
;; Please heed that the tape, being bounded to the left, but bourneless
;; with respect to the dextral expansion, does not permit a translation
;; from the rightmost (highest) bit position of the leftmost cell, as
;; no sinistral near-dweller exists for such.
;; 
;; 
;; Syntax
;; ======
;; BytFuck's appropration of its stock-father's tenets incorporates the
;; allotment of single symbols to commands, in the obbligatio of any
;; non-operative token's tolerance in a grade paregal to its negligence,
;; whence is begotten the potential for commentary purposes in these
;; supererogative contents.
;; 
;; 
;; Instruction
;; ===========
;; BytFuck's instruction set enumerates thirteen members, an octuple
;; preponderance a dation of its brainfuck ancestor, while a quintuple
;; participation apperains to the novel introduction of bit-related
;; operations in reference to the currently selected bit position.
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity's impartation with respective to the
;; operative avails shall ensue from the below apercu:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one (1). Upon a
;;           | transgression of its upper bourne of 255, wraps its
;;           | state around to the lower extremum of zero (0).
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1). Upon a
;;           | transgression of its lower bourne of zero (0), wraps its
;;           | state around to the upper extremum of 255.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left, if
;;           | not already empight on the leftmost cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]"; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "["; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   ≥       | Translates the bit pointer one step to the right.
;;           |---------------------------------------------------------
;;           | If, immediately prevenient to this operation's
;;           | invocation, the bit pointer resides on the rightmost
;;           | (lowest) bit of the current cell, the cell pointer
;;           | advances to the right neighboring cell, while the bit
;;           | pointer assumes the leftmost (highest) bit of this new
;;           | cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;   ..................................................................
;;   ≤       | Translates the bit pointer one step to the left.
;;           |---------------------------------------------------------
;;           | If, immediately prevenient to this operation's
;;           | invocation, the bit pointer resides on the leftmost
;;           | (highest) bit of the current cell, and this cell does
;;           | not constitute the leftmost unit in the memory, the
;;           | cell pointer recedes to the left neighboring cell, while
;;           | the bit pointer assumes the rightmost (lowest) bit of
;;           | this new cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;           |---------------------------------------------------------
;;           | If, immediately prevenient to this operation's
;;           | invocation, the bit pointer resides on the leftmost
;;           | (highest) bit of the current cell, and this cell
;;           | constitutes the leftmost unit in the memory, no motion
;;           | is accompassed at all.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;   ..................................................................
;;   *       | Flips the state of the bit referenced by the bit
;;           | pointer.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;   ..................................................................
;;   !       | Relocates the bit pointer to the rightmost (lowest)
;;           | position of its ensconcing cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;   ..................................................................
;;   (       | If the bit referenced by the bit pointer equals zero
;;           | (0), moves the instruction pointer (IP) to the position
;;           | immediately succeeding the matching ")" instruction;
;;           | otherwise executes the code commorant betwixt this "("
;;           | and the matching ")" token.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;   ..................................................................
;;   )       | Demarcates the conditional code block instigated by the
;;           | preceding matching "(" instruction.
;;           |---------------------------------------------------------
;;           | This operation constitutes a polymechany introduced by
;;           | BytFuck, forinsecal to brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, accompassing its causata by
;; immediate evaluation of the provided source code string.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-03-07
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2020BytFuck]
;;   The Esolang contributors, "BytFuck", August 10th, 2020
;;   URL: "https://esolangs.org/wiki/BytFuck"
;;   
;;   [esolang2020Talk:BytFuck]
;;   The Esolang contributors, "Talk:BytFuck", August 10th, 2020
;;   URL: "https://esolangs.org/wiki/Talk:BytFuck"
;;   Notes:
;;     - Discusses certain specifications of the BytFuck programming
;;       language not expressly partaking of the main treatise.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype byte-locator ()
  "The ``byte-locator'' type defines a byte designator in an
   implementation-dependent manner, conable for as a result value for
   the native ``byte'' function.."
  'T)

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type defines a zero-based index into a BytFuck
   program memory's cell sequence, its diorism reified in an integral
   number greater than or equal to zero (0), but without any natural
   march along the upper extremum, that is, a commorant of the interval
   [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits as a number in the closed integral interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of which lays its amplectation, among others, around the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose circumference
   enumerates zero or more entries, each key among these complies with
   the KEY-TYPE and answers with a value of the VALUE-TYPE, for both is
   stipulated the generic sentinel ``*'' as a default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and
                  (or (eq    key-type      '*)
                      (typep current-key   key-type))
                  (or (eq    value-type    '*)
                      (typep current-value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list of a conformation which admits
   zero or more elements of the ELEMENT-TYPE, the default chosen as the
   generic sentinel ``*''."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   jump points in a piece of BytFuck program, mediated by adminiculum
   of their zero-based indices into the source string, and reified via
   a hash table which maps ``fixnum'' keys to values of the same realm."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" value
   and returns a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of cell pointer.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell-Pointer
  (:constructor make-cell-pointer ()))
  "The ``Cell-Pointer'' class encapsulates the notion of the twifaced
   BytFuck cell pointer in both of its aspects: imprimis, as a reference
   to the currently active cell, or byte; secondly, as a cursor into the
   contemporaneously selected bit of this byte."
  (byte-position 0 :type (integer 0 *) :read-only NIL)
  (bit-position  0 :type (integer 0 7) :read-only NIL))

;;; -------------------------------------------------------

(defun leftmost-bit-is-selected-p (pointer)
  "Determines whether the cell POINTER is located at the leftmost, the
   last, bit of the currently selected cell, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Cell-Pointer pointer))
  (the boolean
    (get-boolean-value-of
      (>= (cell-pointer-bit-position pointer)
          7))))

;;; -------------------------------------------------------

(defun rightmost-bit-is-selected-p (pointer)
  "Determines whether the cell POINTER is located at the righmost, the
   first, bit of the currently selected cell, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Cell-Pointer pointer))
  (the boolean
    (get-boolean-value-of
      (zerop
        (cell-pointer-bit-position pointer)))))

;;; -------------------------------------------------------

(defun leftmost-byte-is-selected-p (pointer)
  "Determines whether the cell POINTER's currently selected cell
   represents the leftmost, the first, of the respective tape, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Cell-Pointer pointer))
  (the boolean
    (get-boolean-value-of
      (zerop
        (cell-pointer-byte-position pointer)))))

;;; -------------------------------------------------------

(defun can-move-one-bit-to-the-left-p (pointer)
  "Determines whether the cell POINTER is homologated to move to the
   bit position along its sinistral laterality, which is the case if and
   only if it does not reside at the leftmost (last) bit of the leftmost
   (first) cell, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Cell-Pointer pointer))
  (the boolean
    (not
      (and
        (leftmost-bit-is-selected-p  pointer)
        (leftmost-byte-is-selected-p pointer)))))

;;; -------------------------------------------------------

(defun move-to-first-bit-of-byte-at (pointer target-byte-position)
  "Relocates the cell POINTER to the first (rightmost) bit of the cell
   at the TARGET-BYTE-POSITION and returns no value."
  (declare (type Cell-Pointer  pointer))
  (declare (type (integer 0 *) target-byte-position))
  (psetf
    (cell-pointer-byte-position pointer) target-byte-position
    (cell-pointer-bit-position  pointer) 0)
  (values))

;;; -------------------------------------------------------

(defun move-to-last-bit-of-byte-at (pointer target-byte-position)
  "Relocates the cell POINTER to the last (leftmost) bit of the cell
   at the TARGET-BYTE-POSITION and returns no value."
  (declare (type Cell-Pointer  pointer))
  (declare (type (integer 0 *) target-byte-position))
  (psetf
    (cell-pointer-byte-position pointer) target-byte-position
    (cell-pointer-bit-position  pointer) 7)
  (values))

;;; -------------------------------------------------------

(defun move-to-first-bit-of-current-byte (pointer)
  "Relocates the cell POINTER's bit cursor to the first (rightmost)
   position in its currently selected cell and returns no value."
  (declare (type Cell-Pointer pointer))
  (setf (cell-pointer-bit-position pointer) 0)
  (values))

;;; -------------------------------------------------------

(defun move-to-left-bit (pointer)
  "Translates the cell POINTER's bit cursor one position to the left,
   is possible, contingently transgressing into the first (leftmost) bit
   of the sinistral neighbor cell, and returns no value."
  (declare (type Cell-Pointer pointer))
  (cond
    ;; Can move into cell to the left?
    ((and (can-move-one-bit-to-the-left-p pointer)
          (leftmost-bit-is-selected-p pointer))
      (move-to-first-bit-of-byte-at pointer
        (1- (cell-pointer-byte-position pointer))))
    ;; Can move inside current cell?
    ((can-move-one-bit-to-the-left-p pointer)
      (incf (cell-pointer-bit-position pointer)))
    ;; Already in the leftmost bit of the leftmost cell?
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defun move-to-right-bit (pointer)
  "Translates the cell POINTER's bit cursor one position to the right,
   is possible, contingently transgressing into the last (rightmost) bit
   of the dextral neighbor cell, and returns no value."
  (declare (type Cell-Pointer pointer))
  (if (rightmost-bit-is-selected-p pointer)
    (move-to-last-bit-of-byte-at pointer
      (1+ (cell-pointer-byte-position pointer)))
    (decf (cell-pointer-bit-position pointer)))
  (values))

;;; -------------------------------------------------------

(defun move-to-left-byte (pointer)
  "Translates the cell POINTER to the sinistral byte cell, if possible,
   and returns no value."
  (declare (type Cell-Pointer pointer))
  (unless (leftmost-byte-is-selected-p pointer)
    (decf (cell-pointer-byte-position  pointer))
    (move-to-first-bit-of-current-byte pointer))
  (values))

;;; -------------------------------------------------------

(defun move-to-right-byte (pointer)
  "Translates the cell POINTER to the dextral byte cell and returns no
   value."
  (declare (type Cell-Pointer pointer))
  (incf (cell-pointer-byte-position  pointer))
  (move-to-first-bit-of-current-byte pointer)
  (values))

;;; -------------------------------------------------------

(defun cell-pointer-byte-offset (pointer)
  "Returns the absolute offset of the first bit in the byte (cell)
   referenced by the cell POINTER."
  (declare (type Cell-Pointer pointer))
  (the (integer 0 *)
    (* (cell-pointer-byte-position pointer)
       8)))

;;; -------------------------------------------------------

(defun cell-pointer-byte-locator (pointer)
  "Returns an implementation-dependent byte designator which references
   the octuple bits constituting the cell POINTER's occupied cell."
  (declare (type Cell-Pointer pointer))
  (the byte-locator
    (byte 8
      (cell-pointer-byte-offset pointer))))

;;; -------------------------------------------------------

(defun cell-pointer-bit-offset (pointer)
  "Returns the absolute offset of the bit referenced by the POINTER."
  (declare (type Cell-Pointer pointer))
  (the (integer 0 *)
    (+ (cell-pointer-byte-offset  pointer)
       (cell-pointer-bit-position pointer))))

;;; -------------------------------------------------------

(defun cell-pointer-bit-locator (pointer)
  "Returns an implementation-dependent byte designator which references
   the singular bit marked by the cell POINTER in its current byte."
  (declare (type Cell-Pointer pointer))
  (the byte-locator
    (byte 1
      (cell-pointer-bit-offset pointer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((bits
    :initform      #b00000000
    :accessor      memory-bits
    :type          (unsigned-byte *)
    :documentation "The cell vector, bounded along the sinistral
                    semi-axis, but bourneless on the right, realized as
                    a theoretically infinite dispansion of
                    integer-encoded bits.")
   (pointer
    :initform      (make-cell-pointer)
    :reader        memory-pointer
    :type          Cell-Pointer
    :documentation "The cell pointer, which concomitantly operates on a
                    byte (cell) and on a bit level.")
   (maximum-cell-index
    :initform      0
    :accessor      memory-maximum-cell-index
    :type          cell-index
    :documentation "The largest zero-based cell (byte) index sojourned
                    by the cell POINTER, naited merely for printing
                    purposes, not for supputations."))
  (:documentation
    "The ``Memory'' class is apportioned the dever of accommodating the
     BytFuck program memory, commorant in its diorism the concept of
     a catena of unsigned bytes, dispanded infinitely along the dextral
     axis, and operated upon by a twissel of pointers: one dedicated to
     the currently active cell's designation, the second to the bit's
     selection from this cell's.
     ---
     This implementation relies on an unsigned integer number as the
     vehicle of the bytes' and bits' castaldy. The theoretically
     bourneless nature of this data type in Common Lisp, in conjunction
     with the rather mickle compass of available logical operations,
     redes such representation for both the byte-valued cells and their
     serelepes bits as a parhedral stratum."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ``Memory'' constructor operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns a fresh ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory maximum index operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-maximum-cell-index (memory accessed-index)
  "Updates the MEMORY's sojourned maximum cell index to ensure the
   ACCESSED-INDEX' contribution and returns no value."
  (declare (type Memory     memory))
  (declare (type cell-index accessed-index))
  (setf (memory-maximum-cell-index memory)
    (max accessed-index
      (memory-maximum-cell-index memory)))
  (values))

;;; -------------------------------------------------------

(defun update-maximum-cell-index-with-respect-to-pointer (memory)
  "Updates the MEMORY's sojourned maximum cell index to ensure the
   inclusion of its cell pointer's location and returns no value."
  (declare (type Memory memory))
  (the cell-index
    (update-maximum-cell-index memory
      (cell-pointer-byte-position
        (memory-pointer memory)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of random-access cell operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-byte-offset-for-cell-at (index)
  "Returns the bit offset of the byte amenable to the cell INDEX."
  (declare (type cell-index index))
  (the (integer 0 *)
    (* index 8)))

;;; -------------------------------------------------------

(defun get-byte-locator-for-cell-at (index)
  "Returns an implementation-dependent byte specifier capacitated to
   select the eight accolent bits from the zero-based bit offset
   corrensponding to the cell INDEX."
  (declare (type cell-index index))
  (the T
    (byte 8
      (get-byte-offset-for-cell-at index))))

;;; -------------------------------------------------------

(defun byte-value-at (memory index)
  "Returns the unsigned byte value stored at the zero-based INDEX into
   the MEMORY."
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (the octet
    (ldb
      (get-byte-locator-for-cell-at index)
      (memory-bits                memory))))

;;; -------------------------------------------------------

(defun (setf byte-value-at) (new-value memory index)
  "Stores the NEW-VALUE in the byte amenable to the zero-based cell
   INDEX into the MEMORY, and returns no value."
  (declare (type octet      new-value))
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (setf
    (ldb
      (get-byte-locator-for-cell-at index)
      (memory-bits                memory))
    new-value)
  (update-maximum-cell-index memory index)
  (values))

;;; -------------------------------------------------------

(defun cell-value-at (memory index)
  "Returns the unsigned byte value stored at the zero-based INDEX into
   the MEMORY."
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (the octet
    (byte-value-at memory index)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory index)
  "Stores the NEW-VALUE in the cell amenable to the zero-based INDEX
   into the MEMORY, contingently preceded by a wrapping of its state
   into the valid unsigned byte range of [0, 255], and returns no
   value."
  (declare (type integer    new-value))
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (setf
    (byte-value-at memory index)
    (mod           new-value 256))
  (update-maximum-cell-index memory index)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of current cell access operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-cell-value (memory)
  "Returns the unsigned byte value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (the octet
    (cell-value-at memory
      (cell-pointer-byte-position
        (memory-pointer memory)))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceded by a wrapping of its state into the unsigned byte range of
   [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (cell-value-at memory
      (cell-pointer-byte-position
        (memory-pointer memory)))
    new-value)
  (update-maximum-cell-index-with-respect-to-pointer memory)
  (values))

;;; -------------------------------------------------------

(defun current-cell-equals-zero-p (memory)
  "Determines whether the MEMORY's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-cell-value memory)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of current bit access operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-current-bit-locator (memory)
  "Returns an implementation-dependent byte locator capacitated to
   select from the MEMORY the current cell's contemporaneously selected
   bit."
  (declare (type Memory memory))
  (the byte-locator
    (cell-pointer-bit-locator
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun current-bit-value (memory)
  "Returns the value of the MEMORY's currently selected bit."
  (declare (type Memory memory))
  (the bit
    (ldb
      (get-current-bit-locator memory)
      (memory-bits             memory))))

;;; -------------------------------------------------------

(defun (setf current-bit-value) (new-value memory)
  "Sets the value of the MEMORY's currently selected bit to the
   NEW-VALUE and returns no value."
  (declare (type bit    new-value))
  (declare (type Memory memory))
  (setf
    (ldb
      (get-current-bit-locator memory)
      (memory-bits             memory))
    new-value)
  (update-maximum-cell-index-with-respect-to-pointer memory)
  (values))

;;; -------------------------------------------------------

(defun current-bit-equals-zero-p (memory)
  "Determines whether the MEMORY's currently selected bit equals zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-bit-value memory)))))

;;; -------------------------------------------------------

(defun flip-current-bit (memory)
  "Flips the state of the MEMORY's currently selected bit and returns
   no value."
  (declare (type Memory memory))
  (setf (current-bit-value memory)
    (- 1 (current-bit-value memory)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of cell pointer manipulation operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left, if
   possible, and returns no value."
  (declare (type Memory memory))
  (move-to-left-byte
    (memory-pointer memory))
  (update-maximum-cell-index-with-respect-to-pointer memory)
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (move-to-right-byte
    (memory-pointer memory))
  (update-maximum-cell-index-with-respect-to-pointer memory)
  (values))

;;; -------------------------------------------------------

(defun move-bit-pointer-left (memory)
  "Translates the MEMORY's bit pointer one position to the left and
   returns no value."
  (declare (type Memory memory))
  (move-to-left-bit
    (memory-pointer memory))
  (update-maximum-cell-index-with-respect-to-pointer memory)
  (values))

;;; -------------------------------------------------------

(defun move-bit-pointer-right (memory)
  "Translates the MEMORY's bit pointer one position to the right and
   returns no value."
  (declare (type Memory memory))
  (move-to-right-bit
    (memory-pointer memory))
  (update-maximum-cell-index-with-respect-to-pointer memory)
  (values))

;;; -------------------------------------------------------

(defun move-bit-pointer-to-first-bit (memory)
  "Relocates the MEMORY's bit pointer to the first (lowest or leftmost)
   position of the currently selected cell and returns no value."
  (declare (type Memory memory))
  (move-to-first-bit-of-current-byte
    (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory printing operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory      memory))
  (declare (type destination stream))
  (loop
    initially
      (format stream "(Memory")
    for current-cell-index
      of-type cell-index
      from    0
      to      (memory-maximum-cell-index memory)
    do
      (format stream " ~8,'0b"
        (cell-value-at memory current-cell-index))
    finally
      (format stream ")")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-vacant-jump-table ()
  "Creates and returns an initially empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun supputate-zero-byte-jump-points (code jump-table)
  "Detects the matching \"[\" and \"]\" instructions in the piece of
   BytFuck source CODE, connects these in the JUMP-TABLE, and returns
   the modified JUMP-TABLE."
  (declare (type string     code))
  (declare (type jump-table jump-table))
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-token
        of-type character
        across  code
      and current-position
        of-type fixnum
        from    0
        by      1
      
      if (char= current-token #\[) do
        (push current-position forward-jump-points)
      else if (char= current-token #\]) do
        (if forward-jump-points
          (let ((forward-jump-point (pop forward-jump-points))
                (back-jump-point    current-position))
            (declare (type fixnum forward-jump-point))
            (declare (type fixnum back-jump-point))
            (psetf
              (gethash forward-jump-point jump-table)
                back-jump-point
              (gethash back-jump-point    jump-table)
                forward-jump-point))
          (error "Unmatched \"]\" instruction at position ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched \"[\" instruction~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            (nreverse forward-jump-points))))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun supputate-zero-bit-jump-points (code jump-table)
  "Detects the matching \"(\" and \")\" instructions in the piece of
   BytFuck source CODE, connects these in the JUMP-TABLE, and returns
   the modified JUMP-TABLE."
  (declare (type string     code))
  (declare (type jump-table jump-table))
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-token
        of-type character
        across  code
      and current-position
        of-type fixnum
        from    0
        by      1
      
      if (char= current-token #\() do
        (push current-position forward-jump-points)
      else if (char= current-token #\)) do
        (if forward-jump-points
          (let ((forward-jump-point (pop forward-jump-points))
                (back-jump-point    current-position))
            (declare (type fixnum forward-jump-point))
            (declare (type fixnum back-jump-point))
            (psetf
              (gethash forward-jump-point jump-table)
                back-jump-point
              (gethash back-jump-point    jump-table)
                forward-jump-point))
          (error "Unmatched \")\" instruction at position ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched \"(\" instruction~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            (nreverse forward-jump-points))))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun supputate-jump-table-for (code)
  "Creates a new ``jump-table'' which comprehends the matching \"[\" and
   \"]\" as well as \"(\" and \")\" instruction jumelles woning in the
   piece of BytFuck source CODE and returns thus produced mapping."
  (declare (type string code))
  (the jump-table
    (supputate-zero-byte-jump-points code
      (supputate-zero-bit-jump-points code
        (prepare-vacant-jump-table)))))

;;; -------------------------------------------------------

(defun locate-jump-target (jump-table point-of-departure)
  "Returns the jump target allied with the POINT-OF-DEPARTURE in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No jump target defined for the position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BytFuck (code)
  "Interprets the piece of BytFuck source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (supputate-jump-table-for code))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (symbol-macrolet
        ((program-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length code)))))
         (current-token
          (the character
            (char code ip))))
      (declare (type boolean   program-completed-p))
      (declare (type character current-token))
      (loop until program-completed-p do
        (case current-token
          (#\+
            (incf (current-cell-value memory)))
          
          (#\-
            (decf (current-cell-value memory)))
          
          (#\>
            (move-cell-pointer-right memory))
          
          (#\<
            (move-cell-pointer-left memory))
          
          (#\.
            (write-char
              (code-char
                (current-cell-value memory))))
          
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell-value memory)
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          
          (#\[
            (when (current-cell-equals-zero-p memory)
              (setf ip
                (locate-jump-target jump-table ip))))
          
          (#\]
            (unless (current-cell-equals-zero-p memory)
              (setf ip
                (locate-jump-target jump-table ip))))
          
          (#\≥
            (move-bit-pointer-right memory))
          
          (#\≤
            (move-bit-pointer-left memory))
          
          (#\*
            (flip-current-bit memory))
          
          (#\!
            (move-bit-pointer-to-first-bit memory))
          
          (#\(
            (when (current-bit-equals-zero-p memory)
              (setf ip
                (locate-jump-target jump-table ip))))
          
          (#\) NIL)
          
          (otherwise NIL))
        
        (incf ip))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replicate the ASCII code of the letter "H" (= 72 (decimal)
;; = 01001000 (binary)) and print its character form.
(interpret-BytFuck "≤≤≤*≤≤≤*.")

;;; -------------------------------------------------------

;; Truth-machine which exploits the fact that the representation of the
;; character "0" in its ASCII's binary form (00110000) carries a zero
;; (0) bit on the first position, while the equivalent of the "1" symbol
;; in this radix (00110001) ostends a one-valued (1) bit.
;; 
;; Concept expression in pseudocode:
;; 
;;   {"0" (= 00110000 in binary) or "1" (= 00110001 in binary)}
;;   {              ^                             ^           }
;;   memory[0] <- user input
;;   
;;   if first bit of memory[0] = 1 then
;;     repeat while memory[0] != 0 do
;;       print memory[0]
;;     end repeat
;;   else
;;     print memory[0]
;;   end if
(interpret-BytFuck
  ",
   ([.])
   .")

;;; -------------------------------------------------------

;; Invert the bits of the binary sequence
;;   10111110
;; thus yielding the catena of bits
;;   01000001
;; which is tantamount to the ASCII code 65 of the letter "A", and
;; print thilk.
(interpret-BytFuck
  "≤*≤*≤*≤*≤*≤≤*
   !
   *≤*≤*≤*≤*≤*≤*≤*
   .")

;;; -------------------------------------------------------

;; Bit-negating repeating cat program which terminates on a
;; "null character" input.
;; 
;; This program queries the standard input for a character, inverts all
;; of its bits, and prints the character corresponding to the modified
;; binary representation to the standard output, ere commencing anew.
(interpret-BytFuck ",[!*≤*≤*≤*≤*≤*≤*≤*.[-],]")
