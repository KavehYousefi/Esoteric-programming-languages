;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "2DChanger", presented by the Esolang user "ChuckEsoteric08"
;; in the year 2022, and devised as a two-dimensional variant of
;; Jeffry Johnston's "BitChanger", itself a constricted variation of
;; Urban Mueller's "brainfuck", operating, however, on bits.
;; 
;; 
;; Concept
;; =======
;; The 2DChanger programming language's foundment manifests in its
;; derivation from the brainfuck-inspired BitChanger, utilizing a very
;; compendious donat for a restricted instruction set, while deploying
;; an infinite bits series for its programs' memory. The kenspeckle
;; element of its haecceity yet relates to the two-dimensional code
;; layout.
;; 
;; == A PROGRAM IS A MATRIX OF INSTRUCTIONS ==
;; 2DChanger establishes its programs in a two-dimensional layout,
;; comprehending zero or more columns and rows, with each grid cell
;; referring to a single instruction.
;; 
;; This Cartesian design associates with every cell a coordinate tuple
;; (x, y), where x referes to the zero-based column index, and y
;; designates the zero-based row subscript; as a consectary, a position
;; p applies to the diorism
;; 
;;   p := (x, y), where x is an element of the integers >= 0,
;;                and   y is an element of the integers >= 0.
;; 
;; == THE INSTRUCTION POINTER: A TRAVELER ALONG THE GRID ==
;; During its inchoation empight at the left upper cell (0, 0) and
;; airted at the right, the instruction pointer (IP), or program counter
;; (PC), is assigned the onus of traveling across the grid in order to
;; determine the next abiding operation. Counterdistinguished from
;; one-dimensional code layouts, this cursor must be augmented to
;; accommodate three properties:
;; 
;;   (1) The current column index x, with x >= 0.
;;   (2) The current row    index y, with y >= 0.
;;   (3) The current traveling direction d, with d being a member of the
;;       set {right, down, left, up}.
;; 
;; For a more formal diorism, the instruction pointer ip can be
;; described as a triple:
;; 
;;   ip := (x, y, d), where x is an element of the integers >= 0,
;;                          y is an element of the integers >= 0,
;;                    and   d is an element of the set
;;                            {right, down, left, up}.
;; 
;; == INFINITELY MANY BITS PARTICIPATE IN THE MEMORY ==
;; The memory constitutes a series of a theoretically infinite tally of
;; cells, linearly arranged, with each unit lending a salvatory to a
;; single bit, initially set to zero (0).
;; 
;; At any instant, a cursor, known as the cell pointer, determines the
;; currently active cell, exclusively amenable to indagations and
;; manipulations. The pointer itself responds to instructions for its
;; gradual relocation in sinistral as well as dextral airt.
;; 
;; == THE 2DCHANGER EXECUTION MODEL: NAVIGATING THE GRID ==
;; Executing a piece of 2DChanger code is perfectly tantamount a
;; navigation through the grid with a concomitant response to any
;; non-NOP instruction.
;; 
;; Its instruction pointer's placement in the left upper grid cell,
;; combined with a dextral orientation, incites a 2DChanger program's
;; commencement.
;; 
;; A causatum of the encounter with an effective operation, the same is
;; executed, contingently modifying the instruction pointer's direction;
;; in any case, however, advancing the same along its airt as a
;; consequence of the performed activity.
;; 
;; If the instruction pointer attempts to trespass one of the four
;; bournes embraced by the grid's circumference, the program terminates
;; with immediate eventuation.
;; 
;; 
;; Instructions
;; ============
;; A testimony to 2DChanger's compendious capabilities, its instruction
;; set tallies a mere of three members, destitute of input and output
;; facilities in the same mete as its abstinence from dedicated
;; commodities on a higher tier serves to impose a severe programmatic
;; encumbrance.
;; 
;; The thrice applicances steadable to the developer comprehend these:
;; 
;;   (a) Cell pointer translation
;;   (b) Bit value inversion
;;   (c) Instruction pointer rotation
;; 
;; The two-dimensional nature of 2DChanger code and its capacitation in
;; rotating the instruction pointer homologates a basic ilk of control
;; flow.
;; 
;; == OVERVIEW ==
;; A cursory nortelry's adhibition shall be apportioned to the following
;; tabular illustration's engagement:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   }       | Flips the current cell bit and moves the cell pointer
;;           | one step to the right.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   +       | If the current cell bit equals zero, rotates the
;;           | instruction pointer counter-clockwise.
;;           | If the current cell bit equals one, rotates the
;;           | instruction pointer clockwise.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-01-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/2DChanger"
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
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized 2DChanger instruction
   variants, extended by the no-operation sentinel ``:nop'' for cells
   being delegates to non-command characters."
  '(member
    :flip-bit-and-move-right
    :move-left
    :rotate-ip
    :nop))

;;; -------------------------------------------------------

(deftype point ()
  "The ``point'' type defines a two-dimensional position in a grid
   specified by a cons, the sinistral compartment of which lends
   harborage to the x-coordinate, accompanied in the dextral moiety by
   the y-coordinate, both assuming the fixnum character."
  '(cons fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-array ()
  "The ``cell-array'' type defines a sparse two-dimensional matrix of
   commands, represented by a hash table whose keys constitute ``point''
   objects, that is, x-y-coordinate conses, and affiliate each with a
   ``command'' to specify the cell's instruction."
  '(hash-table-of point command))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the valid airts along which the
   instruction pointer may navigate through a 2DChanger code grid."
  '(member :right :down :left :up))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   enumerating, without the claim of exhaustion, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype write-direction ()
  "The ``write-direction'' type enumerates the recognized directions
   applying to the text program generator's production of 2DChanger code
   grid rows."
  '(member :left-to-right :right-to-left))

;;; -------------------------------------------------------

(deftype print-row-type ()
  "The ``print-row-type'' type enumerates the recognized variants of
   2DChanger code grid rows distinguished during a text program
   generator's operations."
  '(member
    :empty
    :singleton
    :first-row
    :last-right-row
    :last-left-row
    :inner-right-row
    :inner-left-row))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementations of instruction table.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for-token (token)
  "Returns the command associated with the TOKEN, or the no-operation
   member ``:nop'' upon a disrespondency."
  (declare (type character token))
  (the command
    (case token
      (#\}       :flip-bit-and-move-right)
      (#\<       :move-left)
      (#\+       :rotate-ip)
      (otherwise :nop))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Grid
  (:constructor make-grid ()))
  "The ``Grid'' class realizes a two-dimensional arrangement of
   2DChanger instructions, implemented using a sparse structure by
   adminiculum of a hash table which associates x-y-coordinate conses
   with commands."
  (width  0                                :type fixnum)
  (height 0                                :type fixnum)
  (cells  (make-hash-table :test #'equalp) :type cell-array))

;;; -------------------------------------------------------

(defmacro with-grid ((grid
                      width-variable height-variable cells-variable)
                     &body body)
  "Evaluates the GRID, binds its width to a local symbol macro with the
   name WIDTH-VARIABLE, its height to such answering to the
   HEIGHT-VARIABLE and the cells to the CELL-VARIABLE, evaluates the
   BODY forms, and returns the last evaluated form's results."
  (let ((evaluated-grid (gensym)))
    (declare (type symbol evaluated-grid))
    `(let ((,evaluated-grid ,grid))
       (declare (type Grid ,evaluated-grid))
       (symbol-macrolet
           ((,width-variable
             (the fixnum
               (grid-width ,evaluated-grid)))
            (,height-variable
             (the fixnum
               (grid-height ,evaluated-grid)))
            (,cells-variable
             (the cell-array
               (grid-cells ,evaluated-grid))))
         (declare (type fixnum     width))
         (declare (type fixnum     height))
         (declare (type cell-array cells))
         (declare (ignorable       width))
         (declare (ignorable       height))
         (declare (ignorable       cells))
         ,@body))))

;;; -------------------------------------------------------

(defun grid-contains-point-p (grid x y)
  "Checks whether the point specified by the X-th column and the Y-th
   row resides inside of the GRID's dimensions, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid   grid))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (with-grid (grid width height cells)
    (the boolean
      (not (null
        (and (>= x 0) (< x width)
             (>= y 0) (< y height)))))))

;;; -------------------------------------------------------

(defun grid-check-point (grid x y)
  "Checks whether the point specified by the X-th column and the Y-th
   row resides inside of the GRID's dimensions, on confirmation
   returning the GRID, otherwise signaling an error of an unspecified
   type."
  (declare (type Grid   grid))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (unless (grid-contains-point-p grid x y)
    (with-grid (grid width height cells)
      (error "The point (x=~d, y=~d) violates the boundaries of the ~
              ~d x ~d grid."
        x y width height)))
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-instruction-at (grid x y)
  "Returns the command at the X-th column of the Y-th row in the GRID,
   or signals an error of an unspecified type upon the coordinates'
   violation of the GRID boundaries."
  (declare (type Grid   grid))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (grid-check-point grid x y)
  (with-grid (grid width height cells)
    (the command
      (gethash (cons x y) cells :nop))))

;;; -------------------------------------------------------

(defun (setf grid-instruction-at) (new-command grid x y)
  "Stores the NEW-COMMAND in the GRID's X-th column of the Y-th row,
   upon necessity updating the GRID's dimensions, and returns the
   modified GRID."
  (declare (type command new-command))
  (declare (type Grid    grid))
  (declare (type fixnum  x))
  (declare (type fixnum  y))
  (with-grid (grid width height cells)
    (setf width  (max width  (1+ x)))
    (setf height (max height (1+ y)))
    (setf (gethash (cons x y) cells) new-command))
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-append-empty-row (grid)
  "Extends the GRID by an empty row succeeding its currently desinent
   line and returns the modified GRID."
  (declare (type Grid grid))
  (incf (grid-height grid))
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-empty-p (grid)
  "Checks whether the GRID is empty, that is, exhibits a width and
   height which both equal zero, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Grid grid))
  (with-grid (grid width height cells)
    (the boolean
      (not (null
        (and (zerop width)
             (zerop height)))))))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid        grid))
  (declare (type destination stream))
  (with-grid (grid width height cells)
    (format stream "~&Grid of ~d x ~d cells:" width height)
    (dotimes (y height)
      (declare (type fixnum y))
      (format stream "~%")
      (dotimes (x width)
        (declare (type fixnum x))
        (format stream "~26a" (grid-instruction-at grid x y)))))
  (the Grid grid))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-code-grid (code)
  "Creates and returns an instruction grid for the piece of 2DChanger
   CODE."
  (declare (type string code))
  (let ((grid (make-grid)))
    (declare (type Grid grid))
    (loop
      with x     of-type fixnum    = 0
      with y     of-type fixnum    = 0
      for  token of-type character across code
      do
        (case token
          ((#\Newline #\Return)
            (setf x 0)
            (incf y)
            (grid-append-empty-row grid))
          (otherwise
            (setf (grid-instruction-at grid x y)
                  (get-command-for-token token))
            (incf x))))
    (the Grid grid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents a tape-like arrangement of bits,
   extending bilaterally into infinity, operating in coefficiency with a
   pointer that selects the currently active cell.
   ---
   This implementation avails itself with three properties of Common
   Lisp's ``integer'' type for the representations of an unbounded bit
   sequence:
     (1) Integers naturally serve to encode bits, with the language
         comprehending operations for indagating and manipulating these
         constituents.
     (2) Integers, encompassing the ``bignum'' type as a subset, are not
         bounded in their magnitude, in corollary theoretically
         permitting an infinite tally of bits.
     (3) Operations on integer-encoded bit sequence are reckoned to
         inflict little performance penalities only. In particular, the
         insertion at the \"head\" requires a simple bit shifting,
         whereas random access for insertion, being expected to respect
         the requirements of low-level efficiency concerns, should not
         be especially costly.
   ---
   This integer-based memory design contributes a panoply of desirable
   properties, a select of which shall be following treatise's cynosure.
   
   == INSERTION AT THE FRONT ==
   The most kenspeckle object of its manipulation, the insertion at the
   bit sequence's front, that is, its least significant position,
   applies by adminiculum of bit shifting.
   
   Such use case intrudes upon the cell pointer's sinistral translation
   towards a bit not yet registered at the memory. A shifting of the
   bits to the left by a single position accommodates the space for the
   newly discovered element.
   
   == RANDOM ACCESS ==
   Any bit, regardless of its location, can be accessed in an efficient
   manner by its zero-based subscript's mediation. This attribute
   capacitates a deployment of the integer-encoding variant as a very
   similar substitute to an actual sequence of the bit vector type.
   
   == INSERTION AT THE END ==
   The facilities proffered by Common Lisp for direct manipulations of
   single bits or bit portions, when committed to a champarty with the
   efficient random access characteristic elucidated above, define the
   insertion at the end in terms of any bit's setting. A detrimental
   discrepancy betwixt integer-encoded bits and veridical bit vectors
   constitutes the lack of a pointer castaldy on the language's side ---
   that is, the highest bit's index must be maintained by the programmer
   himself."
  (bits          0 :type (unsigned-byte *))
  (pointer       0 :type integer)
  (minimum-index 0 :type (integer * 0))
  (maximum-index 0 :type (integer 0 *)))

;;; -------------------------------------------------------

(defmacro with-memory ((memory) &body body)
  "Evaluates the MEMORY, binds its slots ``bits'', ``pointer'',
   ``minimum-index'', and ``maximum-index'' to eponymous local symbol
   macros, executes the BODY forms, and returns the last evaluated
   form's results.
   ---
   In conjunction with the symbol macros, a pentad of local functions
   for the sake of convenient computations will be established:
   
     ------------------------------------------------------------------
     Local function                | Effect
     ------------------------------+-----------------------------------
     accommodate-space-for-index   | If the specified cell index
                                   | transgresses the boundaries of the
                                   | hitherto visited cells, the
                                   | memory's minimum and maximum
                                   | indices are recomputed and
                                   | contingently a bit shift applies
                                   | to the integer-encoded bits.
     ..................................................................
     accommodate-space-for-pointer | If the cell pointer transgresses
                                   | the boundaries of the hitherto
                                   | visited cells, the memory's
                                   | minimum and maximum indices are
                                   | recomputed and contingently a bit
                                   | shift applies to the
                                   | integer-encoded bits.
     ..................................................................
     translate-index               | Translates the signed integer
                                   | index into a zero-based unsigned
                                   | integer bit designator for
                                   | accessing the integer-encoded
                                   | memory bits.
     ..................................................................
     index-in-bounds-p             | Checks whether the specified index
                                   | resides in the hitherto visited
                                   | memory bounds.
     ..................................................................
     count-visited-cells           | Returns the number of cells in the
                                   | memory that have been explicitly
                                   | queried or modified.
     ------------------------------------------------------------------"
  (let ((evaluated-memory (gensym)))
    (declare (type symbol evaluated-memory))
    `(let ((,evaluated-memory ,memory))
       (declare (type Memory ,evaluated-memory))
       (symbol-macrolet
           ((bits
             (the (unsigned-byte *)
               (memory-bits ,evaluated-memory)))
            (pointer
             (the integer
               (memory-pointer ,evaluated-memory)))
            (minimum-index
             (the (integer * 0)
               (memory-minimum-index ,evaluated-memory)))
            (maximum-index
             (the (integer 0 *)
               (memory-maximum-index ,evaluated-memory))))
         (declare (type (unsigned-byte *) bits))
         (declare (type integer           pointer))
         (declare (type (integer * 0)     minimum-index))
         (declare (type (integer 0 *)     maximum-index))
         (declare (ignorable              bits))
         (declare (ignorable              pointer))
         (declare (ignorable              minimum-index))
         (declare (ignorable              maximum-index))
         (labels
             ((accommodate-space-for-index (index)
               "Upon necessity, shifts the MEMORY's BITS in a sufficient
                manner so as to locate the INDEX unto the MINIMUM-INDEX,
                updating the latter as well as the MAXIMUM-INDEX, and
                returns no value."
               (declare (type integer index))
               (when (< index minimum-index)
                 (let ((shift-offset (abs (- index minimum-index))))
                   (declare (type (integer 0 *) shift-offset))
                   (setf bits (ash bits shift-offset))))
               (setf minimum-index (min minimum-index index))
               (setf maximum-index (max maximum-index index))
               (values))
              
              (accommodate-space-for-pointer ()
               "Upon necessity, shifts the MEMORY's BITS and updates the
                MINIMUM-INDEX and MAXIMUM-INDEX in order to ascertain
                the POINTER's commorancy inside of the MEMORY's
                recognized cell portion, and returns no value."
               (accommodate-space-for-index pointer)
               (values))
              
              (translate-index (index)
               "Returns for the signed integer INDEX a non-negative
                index suitable for the use in the BITS."
               (declare (type integer index))
               (the (integer 0 *)
                 (+ index (abs minimum-index))))
              
              (index-in-bounds-p (index)
               "Checks whether the INDEX resides inside of the range
                demarcated by the inclusive MINIMUM-INDEX and the
                inclusive MAXIMUM-INDEX, returning on confirmantion a
                ``boolean'' value of ``T'', otherwise ``NIL''."
               (declare (type integer index))
               (the boolean
                 (not (null
                   (<= minimum-index index maximum-index)))))
              
              (count-visited-cells ()
               "Returns the number of explicitly modified or queried
                MEMORY cells."
               (the (integer 0 *)
                 (1+ (abs (- minimum-index maximum-index))))))
           
           ,@body)))))

;;; -------------------------------------------------------

(defun memory-bit-at (memory index)
  "Returns the bit in the MEMORY's INDEX-th cell."
  (declare (type Memory  memory))
  (declare (type integer index))
  (with-memory (memory)
    (the bit
      (if (index-in-bounds-p index)
        (ldb (byte 1 (translate-index index)) bits)
        0))))

;;; -------------------------------------------------------

(defun (setf memory-bit-at) (new-value memory index)
  "Sets the MEMORY's INDEX-th cell bit to the NEW-VALUE and returns the
   modified MEMORY."
  (declare (type bit     new-value))
  (declare (type Memory  memory))
  (declare (type integer index))
  (with-memory (memory)
    (accommodate-space-for-index index)
    (setf (ldb (byte 1 (translate-index index)) bits) new-value))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-bit (memory)
  "Returns the MEMORY's current cell bit."
  (declare (type Memory memory))
  (the bit
    (memory-bit-at memory
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun (setf memory-current-bit) (new-value memory)
  "Stores in the MEMORY's current cell the NEW-VALUE and returns the
   modified MEMORY."
  (declare (type bit    new-value))
  (declare (type Memory memory))
  (setf (memory-bit-at memory (memory-pointer memory)) new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-bit-zero-p (memory)
  "Checks whether the MEMORY's current cell contains a zero-valued bit,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (null
      (zerop (memory-current-bit memory))))))

;;; -------------------------------------------------------

(defun memory-flip-bit (memory)
  "Flips the bit of the MEMORY's current cell and returns the modified
   MEMORY."
  (declare (type Memory memory))
  (setf (memory-current-bit memory)
        (- 1 (memory-current-bit memory)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (with-memory (memory)
    (incf pointer)
    (accommodate-space-for-pointer))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (with-memory (memory)
    (decf pointer)
    (accommodate-space-for-pointer))
  (the Memory memory))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) stream)
  (declare (type Memory      memory))
  (declare (type destination stream))
  (with-memory (memory)
    (format stream "~&Memory of ~d cells spanning the ~
                      positions ~d to ~d: "
      (count-visited-cells) minimum-index maximum-index)
    (loop
      for bit-index of-type integer from minimum-index to maximum-index
      do  (format stream "~d"
            (memory-bit-at memory bit-index))))
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction-Pointer".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction-Pointer
  (:constructor make-instruction-pointer ()))
  "The ``Instruction-Pointer'' class models a two-dimensional
   instruction pointer (IP) or program counter (PC), the diorism of
   which encapsulates the current column and row indices, as well as the
   direction of travel."
  (x         0      :type fixnum)
  (y         0      :type fixnum)
  (direction :right :type direction))

;;; -------------------------------------------------------

(defun instruction-pointer-advance (ip)
  "Moves the instruction pointer IP one step into the current direction
   and returns the modified IP."
  (declare (type Instruction-Pointer ip))
  (case (instruction-pointer-direction ip)
    (:right (incf (instruction-pointer-x ip)))
    (:down  (incf (instruction-pointer-y ip)))
    (:left  (decf (instruction-pointer-x ip)))
    (:up    (decf (instruction-pointer-y ip)))
    (otherwise
      (error "Invalid instruction pointer direction: ~s."
        (instruction-pointer-direction ip))))
  (the Instruction-Pointer ip))

;;; -------------------------------------------------------

(defun instruction-pointer-rotate-clockwise (ip)
  "Rotates the instruction pointer IP clockwise by 90 degrees and
   returns the modified IP."
  (declare (type Instruction-Pointer ip))
  (setf (instruction-pointer-direction ip)
    (case (instruction-pointer-direction ip)
      (:right :down)
      (:down  :left)
      (:left  :up)
      (:up    :right)
      (otherwise
        (error "Invalid instruction pointer direction: ~s."
          (instruction-pointer-direction ip)))))
  (the Instruction-Pointer ip))

;;; -------------------------------------------------------

(defun instruction-pointer-rotate-counter-clockwise (ip)
  "Rotates the instruction pointer IP counter-clockwise by 90 degrees
   and returns the modified IP."
  (declare (type Instruction-Pointer ip))
  (setf (instruction-pointer-direction ip)
    (case (instruction-pointer-direction ip)
      (:right :up)
      (:down  :right)
      (:left  :down)
      (:up    :left)
      (otherwise
        (error "Invalid instruction pointer direction: ~s."
          (instruction-pointer-direction ip)))))
  (the Instruction-Pointer ip))

;;; -------------------------------------------------------

(defun instruction-pointer-in-grid-p (ip grid)
  "Checks whether the instruction pointer IP's coordinates reside inside
   of the GRID's boundaries, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Instruction-Pointer ip))
  (declare (type Grid                grid))
  (the boolean
    (grid-contains-point-p grid
      (instruction-pointer-x ip)
      (instruction-pointer-y ip))))

;;; -------------------------------------------------------

(defun instruction-pointer-get-command (ip grid)
  "Returns the command in the GRID cell amenable to the instruction
   pointer IP's coordinates, or signals an error upon the latter's
   transgression of the GRID boundaries."
  (declare (type Instruction-Pointer ip))
  (declare (type Grid                grid))
  (the command
    (if (instruction-pointer-in-grid-p ip grid)
      (grid-instruction-at grid
        (instruction-pointer-x ip)
        (instruction-pointer-y ip))
      (error "The instruction pointer ~s is outside of the bounds of ~
              the grid ~s."
        ip grid))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-code-grid (grid)
  "Evaluates the 2DChanger code GRID's commands and returns no value."
  (declare (type Grid grid))
  (unless (grid-empty-p grid)
    (let ((ip     (make-instruction-pointer))
          (memory (make-memory)))
      (declare (type Instruction-Pointer ip))
      (declare (type Memory              memory))
      (symbol-macrolet
          ((current-instruction
            (the command
              (instruction-pointer-get-command ip grid))))
        (loop while (instruction-pointer-in-grid-p ip grid) do
          (case current-instruction
            (:flip-bit-and-move-right
              (memory-flip-bit   memory)
              (memory-move-right memory))
            
            (:move-left
              (memory-move-left memory))
            
            (:rotate-ip
              (if (memory-current-bit-zero-p memory)
                (instruction-pointer-rotate-counter-clockwise ip)
                (instruction-pointer-rotate-clockwise         ip)))
            
            (:nop
              NIL)
            
            (otherwise
              (error "Invalid instruction ~s at position (~d, ~d)."
                current-instruction
                (instruction-pointer-x ip)
                (instruction-pointer-y ip))))
          
          (instruction-pointer-advance ip)))
      
      (format T "~&~a" memory)))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-2DChanger (code)
  "Interprets the piece of 2DChanger CODE and returns no value."
  (declare (type string code))
  (interpret-code-grid
    (build-code-grid code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenates the list of strings LINES into a single string, with
   each two members separated by a single linebreak, and returns the
   result."
  (declare (type (list-of string) lines))
  (the string
    (with-output-to-string (code)
      (declare (type string-stream code))
      (loop
        for line         of-type string in lines
        for first-line-p of-type boolean = T then NIL
        do
          (unless first-line-p
            (format code "~%"))
          (format code "~a" line)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum +INSTRUCTION-SEGMENT-LENGTH+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-SEGMENT-LENGTH+ 24
  "The maximum number of 2DChanger instructions theoretically requisite
   for reproducing a single character's eight bits in the program
   memory.
   ---
   In the \"worst case\", all eight bits of a character's ASCII code
   amount to zero (0), each such represented by the sequence of three
   instructions \"}<}\". In corollary, as a vouch for a ubiquitous
   2DChanger code width, the instruction segment responsible for these
   bits' replication in the memory assumes the fixed width
     24 (= 8 bits * 3 instructions).")

;;; -------------------------------------------------------

(defun print-zero-bit (destination)
  "Prints to the DESTINATION the instruction sequence \"}<}\", which
   sets to current memory bit to zero (0) and subsequently moves the
   cell pointer one step to the right, and returns no value."
  (declare (type destination destination))
  (format destination "}<}")
  (the fixnum 3))

;;; -------------------------------------------------------

(defun print-one-bit (destination)
  "Prints to the DESTINATION the instruction sequence \"}\", which sets
   to current memory bit to one (1) and subsequently moves the cell
   pointer one step to the right, and returns no value."
  (declare (type destination destination))
  (format destination "}")
  (the fixnum 1))

;;; -------------------------------------------------------

(defun print-byte-bits (byte direction destination)
  "Prints to the DESTINATION the 2DChanger instruction sequence, ordered
   to be consumed in the grid row DIRECTION, requisite for appending to
   the memory the BYTE's eight bits, sinistrodextrally aligned from the
   most significant to the least significant position, and returns the
   number of characters issued to the DESTINATION."
  (declare (type (unsigned-byte 8) byte))
  (declare (type write-direction   direction))
  (declare (type destination       destination))
  (let ((number-of-printed-characters 0))
    (declare (type fixnum number-of-printed-characters))
    (case direction
      (:left-to-right
        (dotimes (bit-position 8)
          (declare (type (integer 0 8) bit-position))
          (incf number-of-printed-characters
            (if (logbitp (- 7 bit-position) byte)
              (print-one-bit  destination)
              (print-zero-bit destination)))))
      (:right-to-left
        (dotimes (bit-position 8)
          (declare (type (integer 0 8) bit-position))
          (incf number-of-printed-characters
            (if (logbitp bit-position byte)
              (print-one-bit  destination)
              (print-zero-bit destination)))))
      (otherwise
        (error "Cannot print a byte in the direction ~s." direction)))
    (the fixnum number-of-printed-characters)))

;;; -------------------------------------------------------

(defun print-character-bits (character direction destination)
  "Prints to the DESTINATION the 2DChanger instruction sequence, ordered
   to be consumed in the grid row DIRECTION, requisite for appending to
   the memory the eight bits forming the CHARACTER's ASCII code in
   binary form, sinistrodextrally aligned from the most significant to
   the least significant position, and returns the number of characters
   issued to the DESTINATION."
  (declare (type character       character))
  (declare (type write-direction direction))
  (declare (type destination     destination))
  (the fixnum
    (print-byte-bits (char-code character) direction destination)))

;;; -------------------------------------------------------

(defun empty-string-p (text)
  "Checks whether the TEXT constitutes an empty string, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string text))
  (the boolean
    (not (null
      (zerop (length text))))))

;;; -------------------------------------------------------

(defun singleton-string-p (text)
  "Checks whether the TEXT constitutes a string comprehending exactly
   a single character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string text))
  (the boolean
    (not (null
      (= (length text) 1)))))

;;; -------------------------------------------------------

(defun get-print-row-type (text index)
  "Analyzes the TEXT to print and the INDEX into the currently processed
   character and returns two values:
     (1) the ``print-row-type'' which determines the kind of the
         instruction grid row used for printing the character
     (2) the character at the INDEX into the TEXT, or ``NIL'' if the
         same trespasses the character sequence's boundaries."
  (declare (type string text))
  (declare (type fixnum index))
  (let ((character
          (when (array-in-bounds-p text index)
            (char text index)))
        (first-character-p
          (the boolean
            (not (null
              (zerop index)))))
        (last-character-p
          (not (null
            (= index (1- (length text))))))
        (left-to-right-direction-p
          (not (null
            (evenp index)))))
    (declare (type (or null character) character))
    (declare (type boolean             first-character-p))
    (declare (type boolean             last-character-p))
    (declare (type boolean             left-to-right-direction-p))
    (declare (ignorable                first-character-p))
    (declare (ignorable                last-character-p))
    (declare (ignorable                left-to-right-direction-p))
    (the (values (or null print-row-type) (or null character))
      (values
        (or
          ;; Empty string?
          (and (empty-string-p text)
               :empty)
          
          ;; Single-character string with valid index?
          (and (singleton-string-p text)
               character
               :singleton)
          
          ;; Single-character string with invalid index?
          (and (singleton-string-p text)
               (null character)
               :exhausted)
          
          ;; First character of multi-character string?
          (and character
               first-character-p
               :first-row)
          
          ;; Last character of even (left-to-right) row?
          (and character
               last-character-p
               left-to-right-direction-p
               :last-right-row)
          
          ;; Last character of odd (right-to-left) row?
          (and character
               last-character-p
               (not left-to-right-direction-p)
               :last-left-row)
          
          ;; Inner character of even (left-to-right) row?
          (and character
               (not last-character-p)
               left-to-right-direction-p
               :inner-right-row)
          
          ;; Inner character of odd (right-to-left) row?
          (and character
               (not last-character-p)
               (not left-to-right-direction-p)
               :inner-left-row)
          
          (and (null character)
               :exhausted))
        character))))

;;; -------------------------------------------------------

(defun write-empty-left-margin (destination)
  "Prints to the DESTINATION a single space that represents an empty
   left margin and returns no value."
  (declare (type destination destination))
  (format destination " ")
  (values))

;;; -------------------------------------------------------

(defun write-effective-left-margin (destination)
  "Prints to the DESTINATION the rotation instruction \"+\" and returns
   no value."
  (declare (type destination destination))
  (format destination "+")
  (values))

;;; -------------------------------------------------------

(defun write-empty-right-margin (destination)
  "Prints to the DESTINATION three spaces that represent an empty right
   margin and returns no value."
  (declare (type destination destination))
  (format destination "   ")
  (values))

;;; -------------------------------------------------------

(defun write-upper-right-margin (destination)
  "Prints to the DESTINATION the instruction sequence \"}<+\", which
   represents a deasil rotation of the instruction pointer (IP) from its
   expected dextral orientation to a downward navigation, and returns no
   value."
  (declare (type destination destination))
  (format destination "}<+")
  (values))

;;; -------------------------------------------------------

(defun write-lower-right-margin (destination)
  "Prints to the DESTINATION the instruction sequence \"<}+\", which
   represents a deasil rotation of the instruction pointer (IP) from its
   expected downward trajectory to the sinistral laterality for an
   instruction grid row that expands from left right, and returns no
   value."
  (declare (type destination destination))
  (format destination "<}+")
  (values))

;;; -------------------------------------------------------

(defun write-no-operations (number-of-no-operations destination)
  "Prints to the DESTINATION the NUMBER-OF-NO-OPERATIONS tally of
   spaces, representing 2DChanger no-operation tokens, and returns no
   value."
  (declare (type fixnum      number-of-no-operations))
  (declare (type destination destination))
  (loop repeat number-of-no-operations do
    (format destination " "))
  (values))

;;; -------------------------------------------------------

(defun write-instruction-segment (character direction destination)
  "Writes to the DESTINATION along the DIRECTION the 2DChanger
   instructions capable of reproducing the CHARACTER's ASCII codes in
   binary representation, proceeding from the most significant bit (MSB)
   leftwards, inserting padding spaces on the dextral laterality if
   necessary, and returns no value."
  (declare (type destination     destination))
  (declare (type write-direction direction))
  (let ((padding-size
          (- +INSTRUCTION-SEGMENT-LENGTH+
             (print-character-bits character direction destination))))
    (declare (type fixnum padding-size))
    (write-no-operations padding-size destination))
  (values))

;;; -------------------------------------------------------

(defun write-linebreak (destination)
  "Prints to the DESTINATION an unconditional newline and returns no
   value."
  (declare (type destination destination))
  (format destination "~%")
  (values))

;;; -------------------------------------------------------

(defun write-character-row (row-type character destination)
  "Prints to the DESTINATION a row of 2DChanger instructions requisite
   for reproducing the CHARACTER's ASCII code's binary form in the
   program, depending on the ROW-TYPE potentially accompanied in the
   margins by instruction pointer (IP) rotation commands, and returns no
   value."
  (declare (type print-row-type      row-type))
  (declare (type (or null character) character))
  (declare (type destination         destination))
  
  (case row-type
    ((:empty :exhausted)
      NIL)
    
    (:singleton
      (write-empty-left-margin                            destination)
      (write-instruction-segment character :left-to-right destination)
      (write-empty-right-margin                           destination)
      T)
    
    (:first-row
      (write-empty-left-margin                            destination)
      (write-instruction-segment character :left-to-right destination)
      (write-upper-right-margin                           destination)
      (write-linebreak                                    destination)
      T)
    
    (:last-right-row
      (write-effective-left-margin                        destination)
      (write-instruction-segment character :left-to-right destination)
      (write-empty-right-margin                           destination)
      T)
    
    (:last-left-row
      (write-empty-left-margin                             destination)
      (write-instruction-segment character :right-to-left  destination)
      (write-lower-right-margin                            destination)
      T)
    
    (:inner-right-row
      (write-effective-left-margin                        destination)
      (write-instruction-segment character :left-to-right destination)
      (write-upper-right-margin                           destination)
      (write-linebreak                                    destination)
      T)
    
    (:inner-left-row
      (write-effective-left-margin                         destination)
      (write-instruction-segment character :right-to-left  destination)
      (write-lower-right-margin                            destination)
      (write-linebreak                                     destination)
      T)
    
    (otherwise
      (error "Invalid print row type: ~s." row-type)))
  
  (values))

;;; -------------------------------------------------------

(defun generate-text-program (text &key (destination NIL))
  "Creates a 2DChanger program capable of reproducing the TEXT
   characters' binary representations in the memory, writes the code to
   the DESTINATION, and returns for a non-``NIL'' DESTINATION the
   ``NIL'' value, otherwise responding with a fresh string comprehending
   the result.
   ---
   The program, to a wide extent disencumbered from its TEXT content in
   this matter, adheres to a stringent layout:
   
     ------------------------------------------------------------------
     Area           || Left margin | Instruction area | Right margin
     ..................................................................
     No. characters ||      1      |        24        |        3
     ------------------------------------------------------------------
   
   == LEFT MARGIN ==
   The left margin, reserving a single character, ascertains the
   instruction pointer's rotation from its left orientation to the
   right.
     Upon traveling a grid line from right to left, the next row
   requires an athwart orientation, thus two widdershins instruction
   pointer rotations --- one from left to down, another from down to
   right --- ought to be applied, manifesting in two vertically aligned
   \"+\" instructions in the leftmost column. With each character bit
   modification operation concomitantly relocating the cell pointer to
   the dextral zero-valued bit, the counter-clockwise case transpires
   without further manipulations.
   
   == INSTRUCTION AREA ==
   This area serves to comprehend the 2DChanger instructions necessary
   to replicate the TEXT characters' bit in the program memory.
     Each bit requires either one or three such commands, with the
   zero-bit incurring the latter expense. With every code grid row's
   dedication to a single TEXT character, and every character's
   circumference exactly tallying eight bits, the \"worst case\"
   accounts for 24 characters' accommodation:
     8 bits * 3 instructions = 24 characters.
   In order to respect the strictly rectangular grid design, especially
   in reference to the right margin, which please see below, the
   positions assigned to desuetude are filled with no-operation tokens,
   that is, spaces, to a length of 24 places.
   
   == RIGHT MARGIN ==
   The right margin handles the case of an instruction pointer's
   rotation, when traveling in a sinistrodextral airt, from the
   rightward orientation to a sinistral.
     If traversing a grid row from left to right, the next line must be
   airted into the athwart direction --- from right to left ---. To this
   end, the right-bound instruction pointer experience twice a deasil
   rotation: one from right to down, and another from down to left. The
   fact that, following a character bits' settings, the current cell
   always contains a zero-bit, which actually eventuates a widdershins
   turn, must be amended by flipping the bit value to one. This
   manipulation constitutes the causatum of the instruction sequence
   \"{<\", followed by the first rotation \"+\", thus prescribing the
   three right margin characters
     }<+
   The now downwards aimed instruction pointer is rotated once more
   using a \"+\". As a requisite parasceve for the pending next
   character bit application, the current cell must return from its
   one-bit to the zero-bit state, realized using the operations \"<}\",
   of course consumed from right to left. The segment one line
   immediately alow preceding command treble thus comprehends the triple
     <}+
   The right margin thus always produces these three rightmost columns
   spread across two rows as one jumelle:
     }<+
     <}+"
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for index of-type fixnum from 0 below (length text) do
        (multiple-value-bind (row-type character)
            (get-print-row-type text index)
          (declare (type print-row-type      row-type))
          (declare (type (or null character) character))
          (write-character-row row-type character destination)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program text :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the first memory cell to one (1), rotate the instruction pointer
;; (IP) direction deasil, move down, iterum rotate deasil, move left,
;; flip the first cell to zero (0), advance to the next cell, and
;; finally flip and proceed.
;; The resulting memory state constitutes:
;;   0, 1, 0
(interpret-2DChanger
  (concatenate-lines
    "}<+"
    "}}+"))

;;; -------------------------------------------------------

;; Replicate in the first memory bits the binary representation of the
;; message "Hello, World!" --- the bits being in sinistrodextral
;; direction aligned from the most significant position to the least
;; significant one ---.
;; 
;; Please note that, in the memory output, a singular zero-bit succeeds
;; the desinent text binary representation, as a supernumerary cell
;; pointer translation to the right cannot be avoided while setting the
;; bits.
;; 
;; The generated memory bit sequence constitutes the following:
;; 
;;   01001000 01100101 01101100 01101100 01101111 00101100 00100000 01010111 01101111 01110010 01101100 01100100 00100001 0
;;   -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- X
;;     "H"      "e"      "l"      "l"      "o"      ","      " "      "W"      "o"      "r"      "l"      "d"      "!"
(interpret-2DChanger
" }<}}}<}}<}}}<}}<}}<}    }<+
+}}<}}}<}}<}}}}<}        <}+
+}<}}}}<}}}}<}}<}        }<+
+}<}}<}}}}<}}}}<}        <}+
+}<}}}}<}}}}}            }<+
+}<}}<}}}}<}}}<}}<}      <}+
+}<}}<}}}<}}<}}<}}<}}<}  }<+
+}}}}<}}}<}}}<}          <}+
+}<}}}}<}}}}}            }<+
+}<}}}<}}<}}}}}<}        <}+
+}<}}}}<}}}}<}}<}        }<+
+}<}}<}}}<}}<}}}}<}      <}+
+}<}}<}}}<}}<}}<}}<}}       ")

;;; -------------------------------------------------------

;; Create a 2DChanger program which replicates in its first memory bits
;; the binary representation of the message "Hello, World!" --- the bits
;; being in sinistrodextral direction aligned from the most significant
;; position to the least significant one ---, and print the source code
;; to the standard output.
;; 
;; The thus generated 2DChanger source code amounts to:
;; 
;;    }<}}}<}}<}}}<}}<}}<}    }<+
;;   +}}<}}}<}}<}}}}<}        <}+
;;   +}<}}}}<}}}}<}}<}        }<+
;;   +}<}}<}}}}<}}}}<}        <}+
;;   +}<}}}}<}}}}}            }<+
;;   +}<}}<}}}}<}}}<}}<}      <}+
;;   +}<}}<}}}<}}<}}<}}<}}<}  }<+
;;   +}}}}<}}}<}}}<}          <}+
;;   +}<}}}}<}}}}}            }<+
;;   +}<}}}<}}<}}}}}<}        <}+
;;   +}<}}}}<}}}}<}}<}        }<+
;;   +}<}}<}}}<}}<}}}}<}      <}+
;;   +}<}}<}}}<}}<}}<}}<}}       
;; 
;; Please note that, in the memory output, a singular zero-bit succeeds
;; the desinent text binary representation, as a supernumerary cell
;; pointer translation to the right cannot be avoided while setting the
;; bits.
;; 
;; The generated memory bit sequence constitutes the following:
;; 
;;   01001000 01100101 01101100 01101100 01101111 00101100 00100000 01010111 01101111 01110010 01101100 01100100 00100001 0
;;   -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- X
;;     "H"      "e"      "l"      "l"      "o"      ","      " "      "W"      "o"      "r"      "l"      "d"      "!"
(generate-text-program "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Create a 2DChanger program which replicates in its first memory bits
;; the binary representation of the message "Hello, World!" --- the bits
;; being in sinistrodextral direction aligned from the most significant
;; position to the least significant one ---, and execute the resulting
;; program.
;; Please note that, in the memory output, a singular zero-bit succeeds
;; the desinent text binary representation, as a supernumerary cell
;; pointer translation to the right cannot be avoided while setting the
;; bits.
;; 
;; The thus generated 2DChanger source code amounts to:
;; 
;;    }<}}}<}}<}}}<}}<}}<}    }<+
;;   +}}<}}}<}}<}}}}<}        <}+
;;   +}<}}}}<}}}}<}}<}        }<+
;;   +}<}}<}}}}<}}}}<}        <}+
;;   +}<}}}}<}}}}}            }<+
;;   +}<}}<}}}}<}}}<}}<}      <}+
;;   +}<}}<}}}<}}<}}<}}<}}<}  }<+
;;   +}}}}<}}}<}}}<}          <}+
;;   +}<}}}}<}}}}}            }<+
;;   +}<}}}<}}<}}}}}<}        <}+
;;   +}<}}}}<}}}}<}}<}        }<+
;;   +}<}}<}}}<}}<}}}}<}      <}+
;;   +}<}}<}}}<}}<}}<}}<}}       
;; 
;; Please note that, in the memory output, a singular zero-bit succeeds
;; the desinent text binary representation, as a supernumerary cell
;; pointer translation to the right cannot be avoided while setting the
;; bits.
;; 
;; The generated memory bit sequence constitutes the following:
;; 
;;   01001000 01100101 01101100 01101100 01101111 00101100 00100000 01010111 01101111 01110010 01101100 01100100 00100001 0
;;   -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- X
;;     "H"      "e"      "l"      "l"      "o"      ","      " "      "W"      "o"      "r"      "l"      "d"      "!"
(interpret-2DChanger
  (generate-text-program "Hello, World!"))
