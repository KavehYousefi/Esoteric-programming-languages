;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brainhook", invented by the Esolang user "Yayimhere" and
;; presented on September 4th, 2024, the particular specimen a
;; minimization on Urban Mueller's seminal "brainfuck", the entheus'
;; octuple instruction set being developed into the pernor of a
;; dimidiating curtailment to an operative tesseratomy, its concomitant
;; the memory tape's impounding to a dextrally infinite catena of 63-bit
;; unsigned integer cells.
;; 
;; 
;; Concept
;; =======
;; The Brainhook programming language's diorism ensues from the telos of
;; the brainfuck instruction set's minimization, exercising a
;; curtailment upon the octuple original to a quadruple novelty, while
;; peracting a parhedral forelightening on the memory cells from their
;; unsigned byte to a hexad capacity.
;; 
;; == BRAINHOOK: A DIMIDIATION OF BRAINFUCK'S INSTRUCTION SET ==
;; Brainhook's paravaunt contribution appertains to the aminishing of
;; brainfuck eight operations to a quartet.
;; 
;; == THE MEMORY: AN INFINITE CATENA OF HEXADS ==
;; A proprium divergent in its nature from brainfuck, Brainhook's memory
;; is edified upon a dextrally infinite catena of hexads, unsigned
;; integer numbers enumerating six (6) attiguous bits, the interval of
;; whose apprehension resolves to [0, 63].
;; 
;; == CELL STATES WRAP AROUND THE EXTREMA ==
;; Upon the lower bourne's transgression, the cell state wraps around
;; from zero (0) to the maximum of 63.
;; 
;; == A POINTER DESIGNATES THE CURRENTLY ACTIVE CELL ==
;; At any instant a particular cursor, norned the "cell pointer" and
;; at the program's inchoacy empight on the first (leftmost) cell,
;; serves in the selection of the contemporaneously active unit, the
;; sole entity entalented with the indagation and modulation of the
;; same's state. Its gressible nature homologates the navigation in a
;; sinistrodextral airt.
;; 
;; 
;; Instructions
;; ============
;; A quadruple contingency's participation already manages to exhaust
;; Brainhook's warklumes, an epiphenomenon enstated in each member the
;; a dextral cell pointer movement as the action's perclose.
;; 
;; == OVERVIEW ==
;; A mete of nortelry tharfed for the language's operative avail's
;; comprehension shall constitute the following apercu's dation.
;; 
;; Please heed the demarcation of succedaneous tmemata by mediation of
;; an underlining catena of asterisks ("*"), their occurrencies intended
;; to be substituted by actual Brainhook program in the program's state
;; of ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command      | Effect
;;   -------------+----------------------------------------------------
;;   #            | Relocates the cell pointer to the first (leftmost)
;;                | cell and subsequently advances the cell pointer one 
;;                | step to the right.
;;   ..................................................................
;;   -            | Decrements the current cell value by one (1) and
;;                | subsequently advances the cell pointer one step to
;;                | the right.
;;                |----------------------------------------------------
;;                | If the new cell state violates the lower bourne of
;;                | zero (0), the value wraps around to the maximum of
;;                | 63.
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
;; application of the effort on the Brainhook source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-06-01
;; 
;; Sources:
;;   [esolang:2025:Brainhook]
;;   The Esolang contributors, "Brainhook", November 20th, 2025
;;   URL: "https://esolangs.org/wiki/Brainhook"
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
  "The ``hexad-vector'' type defines a dextrally infinite dispansion of
   cells, everichon of these metes a capacity of an unsigned integral
   datum composed of six (6) bits, the ultimate patefaction that of a
   dynamic vector amplecting ``hexad'' objects."
  '(vector hexad *))

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
   jump points in a Brainhook program, mediated by adminiculum of their
   zero-based indices into the same, and manifested in a hash table
   whose keys and values both assume ``fixnum'' objects."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-array 1
                     :element-type    'hexad
                     :initial-element 0
                     :adjustable      T
                     :fill-pointer    T)
    :accessor      tape-cells
    :type          hexad-vector
    :documentation "The dextrally infinite cell catena as a dynamic
                    vector of ``hexad'' unsigned integer numbers.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          fixnum
    :documentation "The cell pointer, designating the currently active
                    member among the CELLS, the adminiculum to its
                    signification's realization the selected unit's
                    zero-based index into the same vector."))
  (:documentation
    "The ``Tape'' class applies itself to a dextrally infinite
     dispansion of unsigned 6-bit cells, upon whose entirety a cell
     pointer operates in order to designate the currently active unit,
     the sole member to whom the homologation of its state's conspection
     and modulation is vouchsafed."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a ``Tape'' whose cells' states at their
   inchoacy's instant accompts for the default zero (0) value."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Relocates the TAPE's cell pointer one step to the right and returns
   in any case no value."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type hexad-vector cells))
    (declare (type fixnum       pointer))
    (incf pointer)
    (when (>= pointer (fill-pointer cells))
      (vector-push-extend 0 cells)))
  (values))

;;; -------------------------------------------------------

(defun reset-the-cell-pointer (tape)
  "Relocates the TAPE's cell pointer to the first cell and returns no
   value."
  (declare (type Tape tape))
  (setf (tape-pointer tape) 0)
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
   back jump points partaking in the piece of Brainhook source CODE by
   adminiculum of their zero-based indices into the same."
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

(defun interpret-the-brainhook-code (code)
  "Interprets the piece of Brainhook source CODE and returns no value."
  (declare (type string code))
  (let ((jump-table (build-the-jump-table-for code))
        (ip         0)
        (tape       (prepare-a-pristine-tape)))
    (declare (type jump-table jump-table))
    (declare (type fixnum     ip))
    (declare (type Tape       tape))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\#
          (reset-the-cell-pointer      tape)
          (move-the-cell-pointer-right tape))
        (#\-
          (decf (current-cell-value tape))
          (move-the-cell-pointer-right tape))
        (#\(
          (when (current-cell-contains-zero-p tape)
            (setf ip
              (gethash ip jump-table))
            (move-the-cell-pointer-right tape)))
        (#\)
          (setf ip
            (1-
              (gethash ip jump-table))))
        (#\X
          (move-the-cell-pointer-right tape))
        (otherwise
          NIL))
      (incf ip))
    (print tape))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the first three tape cells to the value 63.
(interpret-the-brainhook-code "---")

;;; -------------------------------------------------------

;; Naiting the third memory cell "tape[2]" as a loop driver, decrement
;; the second cell "tape[1]" repeatedly until it reaches the value
;; one (1).
(interpret-the-brainhook-code "XX-#X(#--#X)")
