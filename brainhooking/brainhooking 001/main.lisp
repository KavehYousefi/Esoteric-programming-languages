;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainhooking", invented by the Esolang user "Yayimhere" and
;; presented on September 8th, 2024, its haecceity's firmament accoutred
;; by a minimization of the same author's "Brainhook", itself dedicated
;; to a curtailment's adhibition on Urban Mueller's seminal language
;; "brainfuck", the indicium of the novel among the conjoined
;; provenance's jumelle a memory tape navigation instruction's
;; expungement, with a concomitant arithmetic operation's modulation,
;; and the memory's restriction to 500 hexad-valued cells in lieu of
;; the predecessor's dextrally infinite dispansion.
;; 
;; 
;; Concept
;; =======
;; The brainhooking programming language constitutes a specimen to whom
;; the curtailment of brainfuck's instruction set from its eight
;; members to a trisulc thereof.
;; 
;; == BRAINHOOKING: A DIMIDIATION OF BRAINFUCK'S INSTRUCTION SET ==
;; Brainhooking's chief purpose wones in the reduction of brainfuck's
;; octuple instruction set, already dimidiated by the concepts of the
;; predecessor Brainhook, resulting in a triad in operative
;; constituents.
;; 
;; == THE MEMORY: A VECTOR OF 500 HEXADS ==
;; A proprium divergent in its nature from brainfuck, brainhooking's
;; memory is edified upon a memory whose conformation admits 500
;; elements, unsigned integer numbers enumerating six (6) attiguous
;; bits, the interval of whose apprehension resolves to [0, 63].
;; 
;; == CELL STATES WRAP AROUND THE EXTREMA ==
;; Upon the upper bourne's transgression, the cell state wraps around
;; from 63 along the mimimum of zero (0).
;; 
;; == A POINTER DESIGNATES THE CURRENTLY ACTIVE CELL ==
;; At any instant a particular cursor, norned the "cell pointer" and
;; at the program's inchoacy empight on the first (leftmost) cell,
;; serves in the selection of the contemporaneously active unit, the
;; sole entity entalented with the indagation and modulation of the
;; same's state.
;; 
;; Its gressible nature homologates the navigation in a sinistrodextral
;; airt. Upon the desinent cell's valediction, the cursor wraps around
;; to the first, leftmost, unit.
;; 
;; 
;; Instructions
;; ============
;; A trisulc contingency's participation already manages to exhaust
;; brainhooking's warklumes, an epiphenomenon enstated in each member the
;; a dextral cell pointer movement as the action's perclose.
;; 
;; == OVERVIEW ==
;; A mete of nortelry tharfed for the language's operative avail's
;; comprehension shall constitute the following apercu's dation.
;; 
;; Please heed the demarcation of succedaneous tmemata by mediation of
;; an underlining catena of asterisks ("*"), their occurrencies intended
;; to be substituted by actual brainhooking program in the program's
;; state of ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command      | Effect
;;   -------------+----------------------------------------------------
;;   +mountance   | Increments the current cell value by the
;;    *********   | {mountance}, expressed as signed binary number.
;;                |----------------------------------------------------
;;                | {mountance} must be a catena of zero or more binary
;;                | digits, "0" or "1", whose two's complement supplies
;;                | the amount to add to or deduct from the current
;;                | cell state. If no content is supplied, a standard
;;                | amount of zero (0) is imputed.
;;                |----------------------------------------------------
;;                | If the new cell state transgresses the upper march
;;                | of 63, the value wraps around, commencing from the
;;                | minimum of zero (0).
;;   ..................................................................
;;   (statements) | Repeats the {statements} while the current cell
;;    **********  | value does not equal zero (0), and advances the
;;                | cell pointer one step to the right immediately
;;                | succeeding the iterance's termination.
;;                |----------------------------------------------------
;;                | {statements} must be a sequence compact of zero or
;;                | more instructions or non-operative symbols.
;;   ..................................................................
;;   X            | Accompasses no particular causatum, but advances
;;                | the cell pointer one step to the right.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been peracted in the
;; programming language Common Lisp, the entelechy a per saltum
;; application of the effort on the brainhooking source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-06-04
;; 
;; Sources:
;;   [esolang:2024:brainhooking]
;;   The Esolang contributors, "brainhooking", September 8th, 2024
;;   URL: "https://esolangs.org/wiki/Brainhooking"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hexad ()
  "The ``hexad'' type defines an unsigned integer species whose
   conformation's firmament is edified upon a catena enumerating six (6)
   attiguous bits, the thus established gamut conflating with the closd
   interval [0, 63]."
  '(unsigned-byte 6))

;;; -------------------------------------------------------

(deftype hexad-vector ()
  "The ``hexad-vector'' type defines a catena of 500 attiguous hexads
   as a simple array."
  '(simple-array hexad (500)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a linked list whose componency is
   founded upon a cardinality of zero or more members, each such
   alligated to a lealty to the ELEMENT-TYPE, for thilk the generic
   sentinel ``*'' accommodates the default configuration."
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

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table based upon the
   castaldy of zero or more entries, each such a jumelle of a key
   complying with the KEY-TYPE and an affiliated value ostending a
   compability with the VALUE-TYPE, for both species is furnished the
   generic sentinel ``*'' as the default configuration."
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
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional mapping atwixen the
   jump points in a brainhooking program, mediated by adminiculum of
   their zero-based indices into the same, and manifested in a hash
   table whose keys and values both assume ``fixnum'' objects."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun binary-digit-p (candidate)
  "Determines whether the CANDIDATE represents a binary digit in a
   character's plasmature, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\0)
          (char= candidate #\1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary number operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-the-twos-complement (bits number-of-bits)
  "Construes the BITS as an integer number encoded in two's complement,
   naiting the NUMBER-OF-BITS for its representation, and returns the
   decoded signed integer decimal value."
  (declare (type unsigned-byte bits))
  (declare (type (integer 0 *) number-of-bits))
  (the integer
    (let ((discriminator-position (1- number-of-bits)))
      (declare (type (integer 0 *) discriminator-position))
      (if (logbitp discriminator-position bits)
        (+ (- (expt 2 discriminator-position))
           (ldb (byte discriminator-position 0) bits))
        bits))))

;;; -------------------------------------------------------

(defun read-a-signed-binary-number (source start-position)
  "Proceeding from the inclusive START-POSITION into the SOURCE,
   consumes a catena compact of zero or more attiguous binary digits
   and returns two values:
     (1) If one or more binary digits have been consumed, the signed
         decimal value of the thus extracted binary number, conceived
         as decoded in two's complement; otherwise, the default value
         of zero (0).
     (2) If one or more binary digits have been consumed, the position
         into the SOURCE immediately succeeding the consumed tmema;
         otherwise, the START-POSITION itself."
  (declare (type string source))
  (declare (type fixnum start-position))
  (let ((end-position
          (or (position-if-not #'binary-digit-p source
                :start start-position)
              (length source))))
    (declare (type fixnum end-position))
    (the (values integer fixnum)
      (if (> end-position start-position)
        (values
          (decode-the-twos-complement
            (parse-integer source
              :start start-position
              :end   end-position
              :radix 2)
            (- end-position start-position))
          end-position)
        (values 0 start-position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-array 500
                     :element-type    'hexad
                     :initial-element 0
                     :adjustable      NIL
                     :fill-pointer    NIL)
    :accessor      tape-cells
    :type          hexad-vector
    :documentation "A vector comprehending 500 hexad-valued cells.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          fixnum
    :documentation "The cell pointer, designating the currently active
                    member among the CELLS, the adminiculum to its
                    signification's realization the selected unit's
                    zero-based index into the same vector."))
  (:documentation
    "The ``Tape'' class applies itself to a catena of unsigned 6-bit
     cells, upon whose entirety a cell pointer operates in order to
     designate the currently active unit, the sole member to whom the
     homologation of its state's conspection and modulation is
     vouchsafed."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a ``Tape'' whose cells' states at their
   inchoacy's instant accompts for the default zero (0) value."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Relocates the TAPE's cell pointer one step to the right, contingently
   wrapping around upon the desinent position's transgression, and
   returns in any case no value."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type hexad-vector cells))
    (declare (type fixnum       pointer))
    (incf pointer)
    (when (>= pointer (length cells))
      (setf pointer 0)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the 6-bit value of the TAPE's currently selected cell."
  (declare (type Tape tape))
  (the hexad
    (aref
      (tape-cells   tape)
      (tape-pointer tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current selected cell,
   contingently preceded by a wrapping of its state into the admissible
   unsigned 6-bit interval of [0, 63], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (aref
      (tape-cells   tape)
      (tape-pointer tape))
    (mod new-value 64))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (tape)
  "Determines whether the TAPE's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (not (null
      (zerop
        (current-cell-value tape))))))

;;; -------------------------------------------------------

(defmethod print-object ((tape Tape) (destination T))
  (declare (type Tape   tape))
  (declare (type stream destination))
  (loop
    initially
      (format destination "[")
    
    for current-cell-value of-type hexad   across (tape-cells tape)
    and requires-comma-p   of-type boolean = NIL then T
    do  (format destination "~@[, ~*~]~d"
          requires-comma-p
          current-cell-value)
    
    finally
      (format destination "]"))
  (the Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table builder.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-the-jump-table-for (code)
  "Supputates and returns the jump table which ligates the forward and
   back jump points partaking in the piece of brainhooking source CODE
   by adminiculum of their zero-based indices into the same."
  (declare (type string code))
  (let ((connections  (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table       connections))
    (declare (type (list-of fixnum) start-points))
    (loop
      for current-token    of-type character across code
      and current-position of-type fixnum    from   0 by 1
      
      if (char= current-token #\() do
        (push current-position start-points)
      else if (char= current-token #\)) do
        (if start-points
          (let ((start-point (pop start-points))
                (end-point   current-position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf
              (gethash start-point connections) end-point
              (gethash end-point   connections) start-point))
          (error "No matching \"(\" token could detected for the ~
                  \")\" instruction at the position ~d."
            current-position))
      end
      
      finally
        (when start-points
          (error "The \")\" token~p at the position~:p ~{~d~^, ~} ~
                  could not be matched."
            (length   start-points)
            (nreverse start-points))))
    (the jump-table connections)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-brainhooking-code (code)
  "Interprets the piece of brainhooking source CODE and returns no
   value."
  (declare (type string code))
  (let ((jump-table (build-the-jump-table-for code))
        (ip         0)
        (tape       (prepare-a-pristine-tape)))
    (declare (type jump-table jump-table))
    (declare (type fixnum     ip))
    (declare (type Tape       tape))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\+
          (incf ip)
          (multiple-value-bind (incrementation new-ip)
              (read-a-signed-binary-number code ip)
            (declare (type integer incrementation))
            (declare (type fixnum  new-ip))
            (incf (current-cell-value tape) incrementation)
            (setf ip                        new-ip))
          (move-the-cell-pointer-right tape))
        (#\(
          (when (current-cell-contains-zero-p tape)
            (setf ip
              (gethash ip jump-table))
            (move-the-cell-pointer-right tape))
          (incf ip))
        (#\)
          (setf ip
            (gethash ip jump-table)))
        (#\X
          (move-the-cell-pointer-right tape)
          (incf ip))
        (otherwise
          (incf ip))))
    (print tape))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the first tape cells to 63, 1, and 63.
(interpret-the-brainhooking-code "+1+01+0111111")
