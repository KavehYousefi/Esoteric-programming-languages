;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Number2D", invented by the Esolang user "ChuckEsoteric08"
;; and presented on February 20th, 2022, the kenspeckle indicium
;; apportioned to which appertains to its two-dimensional code grid,
;; which as its sole operative instruments admits decimal digits, its
;; perquisitions and manipulations applied to a bilaterally infinite
;; bit vector whose active element at any instant is determined by a
;; cell pointer.
;; 
;; 
;; Concept
;; =======
;; The Number2D programming language constitutes a two-dimensional
;; specimen, its Cartesian grid admitting merely decimal digits and, as
;; warklumes of arrangement, spaces, its instructions operating on a
;; bilaterally infinite dispansion of bit-valued cells.
;; 
;; == THE PROGRAM: A GRID COMPOSED OF NUMBERS AND SPACES ==
;; A Number2D program's ordonnance complies with a two-dimensional
;; Cartesian conformation, its reticulation's tolerance restricted to
;; decimal digits and spaces only, the former serving in the language's
;; operative expression, while the latter, deprieved of any causta,
;; are endowed with the faculty of contributing to the designment.

;; == THE INSTRUCTION POINTER: POSITION + DIRECTION ==
;; The instruction pointer (IP), or program counter (PC), at the
;; execution's inchoation empight in the grid's top-left corner and
;; airted in a dextral orientation, traverses the grid cells in its
;; pursuit to evaluate each encountered instructive token.
;; 
;; A twissel of operation's are entalented with the competence to
;; redirect the instruction pointer's orientation in an express manner.
;; 
;; Additionally, if the pointer's advancement coerces its mobility to
;; transgress any of the four imposed bournes, its airt is retained,
;; while its position subjects to a relocation to the start of the
;; opposite grid side, thus wrapping around the boundary, whence it
;; propagates its locomotion.
;; 
;; == THE PROGRAM MEMORY: AN INFINITE TAPE OF BITS ==
;; The data's castaldy is consigned to the efforts a tape of cells,
;; extending along its both axes into infinity, with each unit measuring
;; an aefauld bit's capacity, this at the program's inchoation installed
;; in the default state of zero (0).
;; 
;; A cursor, the "cell pointer", serves in the administration of the
;; currently active cell's designation, the same offers the sole
;; instance responsive to perquisitions and modulations for a
;; contemporaneous operation's application. The marker's amenability to
;; gradual translations along both airts homologates the selected unit's
;; alteration.
;; 
;; 
;; Instructions
;; ============
;; A septuple cardinality's governance delineates the Number2D
;; language's operative competences, everichon among this membership a
;; decimal digit desumed from the closed interval [0, 6].
;; 
;; == OVERVIEW ==
;; The available warklumes' compendious elucidation shall be the coming
;; apercu's dation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Immediately terminates the program.
;;   ..................................................................
;;   1       | Rotates the instruction pointer (IP) by 90 degrees in
;;           | concord with this nomothesia:
;;           | 
;;           |   ---------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+--------------
;;           |   left              | up
;;           |   .................................
;;           |   right             | down
;;           |   .................................
;;           |   up                | left
;;           |   .................................
;;           |   down              | right
;;           |   ---------------------------------
;;   ..................................................................
;;   2       | Rotates the instruction pointer (IP) to a horizontal
;;           | direction in concord with the following nomothesia:
;;           | 
;;           |   ---------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+--------------
;;           |   left              | right
;;           |   .................................
;;           |   right             | left
;;           |   .................................
;;           |   up                | right
;;           |   .................................
;;           |   down              | left
;;           |   ---------------------------------
;;   ..................................................................
;;   3       | Inverts (flips) the bit in the current memory cell, ere
;;           | moving the cell pointer one step to the right.
;;   ..................................................................
;;   4       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   5       | If the current cell value equals zero (0), skips the
;;           | next instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   6       | Interprets the eight bits commencing with the current
;;           | cell value as an unsigned integer number in the range
;;           | [0, 255] and prints the character whose ASCII code
;;           | matches this decimal value to the standard output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the Number2D protolog's lucid explications, a few inroads of
;; incertain nature retain their involvement; as a consequence, the
;; following section shall serve in an attendance to the most pertinent
;; subset therefrom.
;; 
;; == WHICH DETERMINATION POLICY APPLIES TO THE PROGRAM START POINT? ==
;; A dedicated program start point determination policy eludes the
;; language standard's nomothesia. Forecause all examples' adductions
;; proceed by the incipial symbol's allocation in the left upper grid
;; position, an intimation for a deviating case cannot be obtained.
;; 
;; It has been adjudged to select as the start symbol such entity which
;; is first encountered by a sinistrodextral search commencing from
;; the left upper grid corner.
;; 
;; == WHICH REDIRECTION PRINCIPLE APPLIES TO THE INSTRUCTION POINTER? ==
;; The instruction pointer's (IP) contingency for a traversal beyond the
;; code grid's imposed verges constitutes an event whose attendance in
;; the protolog's standard is deprieved of a precise diction.
;; 
;; The first salient location in the document produces the following,
;; replicated in an ipsissima verba reverberation:
;; 
;;   If IP is outside program IP, then back from opposite direction 
;; 
;; The accompanying example "Example 1", however, barters the
;; "direction" term for the "side" in its elucidation:
;; 
;;   Because IP is outside of program, it back from down side,
;;   then program stops
;; 
;; The former passage's insinuation may be construed as a reversal of
;; the instruction pointer at the boundary; whereas the latter appears
;; to incline in its intimation towards a wrapping around the grid, for
;; instance, from its top row to the bottom, upon such transgressions.
;; 
;; It has been adjudged to comply to the second interpretation, to whom
;; applies the stipulation that the instruction pointer retains its
;; airt upon a bourne's infraction, consequently wrapping around the
;; violated grid border to the obverse laterality.
;; 
;; 
;; Implementation
;; ==============
;; The reification of this interpreter has been performed in the
;; programming language Common Lisp, in adhering to a twifold stage of
;; evaluation; Imprimis, the Number2D source code's modeling in a guise
;; paravaunt in concinnity as a veridical two-dimensional arrangement
;; of character-valued cells; this being installing a prevenience to
;; the program grid's interpretation by the exectuor dedicated entity.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-09-20
;; 
;; Sources:
;;   [esolang2023Number2D]
;;   The Esolang contributors, "Number2D", October 7th, 2023
;;   URL: "https://esolangs.org/wiki/Number2D"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination's provenance is realized in
   the TYPE-NAME, its formal parameters appropriated in an ipsissima
   verba fashion from the LAMBDA-LIST, and whose predicate apportions
   to the subject of this docimasy the CANDIDATE-NAME, evaluating the
   BODY forms with access granted to the same, expected to return in the
   desinent form's first result a generalized boolean value of \"true\"
   upon the candidate's covenableness, otherwise \"false\".
   ---
   The first BODY form, if resolving to a string object, is adminstered
   a construe as a documentation string to the derived type, and is
   subsequently reappropriated for this telos."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype opcode ()
  "The ``opcode'' type defines a Number2D instruction's operation code,
   or \"opcode\", as an integral number commorant in the closed interval
   [0, 6]."
  '(integer 0 6))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the valid airts for an instruction
   pointer's (IP) movement inside of a Number2D program's grid."
  '(member :down :left :right :up))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which amplects, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for
   which holds the default of the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\"
   truth value, returning for a non-``NIL'' input a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-rotated-direction (traversal-direction)
  "Returns for the TRAVERSAL-DIRECTION the rotated airt in accordance
   with the Number2D stipulations."
  (declare (type direction traversal-direction))
  (the direction
    (case traversal-direction
      (:down  :right)
      (:left  :up)
      (:right :down)
      (:up    :left)
      (otherwise
        (error "Invalid direction: ~s." traversal-direction)))))

;;; -------------------------------------------------------

(defun get-horizontally-rotated-direction (traversal-direction)
  "Returns for the TRAVERSAL-DIRECTION the rotated airt along the
   horizontal axis."
  (declare (type direction traversal-direction))
  (the direction
    (case traversal-direction
      (:down  :left)
      (:left  :right)
      (:right :left)
      (:up    :right)
      (otherwise
        (error "Invalid direction: ~s." traversal-direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Location" class.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class serves in the encapsulation of a
   two-dimensional Cartesian coordinates commorant in the integral
   realm, where, in the context of a grid ordonnance, the x-coordinate
   equiparates with the column index, while the y-coordinate conflates
   with the row's subscript."
  (x (error "Missing x-coordinate.") :type fixnum :read-only NIL)
  (y (error "Missing y-coordinate.") :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defun move-location-to (location new-x new-y)
  "Relocates the LOCATION to the coordinates specified by the NEW-X and
   NEW-Y and returns no value."
  (declare (type Location location))
  (declare (type fixnum   new-x))
  (declare (type fixnum   new-y))
  (psetf (location-x location) new-x
         (location-y location) new-y)
  (values))

;;; -------------------------------------------------------

(defun set-location-to (recipient source)
  "Copies the coordinates from the SOURCE location to the RECIPIENT and
   returns no value."
  (declare (type Location recipient))
  (declare (type Location source))
  (psetf (location-x recipient) (location-x source)
         (location-y recipient) (location-y source))
  (values))

;;; -------------------------------------------------------

(defun translate-location-to (location direction)
  "Translates the LOCATION one step into the DIRECTION and returns the
   modified LOCATION."
  (declare (type Location  location))
  (declare (type direction direction))
  (case direction
    (:left     (decf (location-x location)))
    (:right    (incf (location-x location)))
    (:up       (decf (location-y location)))
    (:down     (incf (location-y location)))
    (otherwise (error "Invalid direction: ~s." direction)))
  (the Location location))

;;; -------------------------------------------------------

(defun get-neighbor-location (location direction)
  "Returns the immediately accolent coordinates relative to the LOCATION
   in the DIRECTION."
  (declare (type Location  location))
  (declare (type direction direction))
  (the Location
    (translate-location-to
      (copy-location location)
      direction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction-Pointer"                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction-Pointer
  (:constructor make-instruction-pointer ()))
  "The ``Instruction-Pointer'' class serves in the modeling of an
   instrunction pointer (IP) or program counter (PC), specialized for
   the traversal of a Number2D program in its veridical ``Code-Grid''
   form, this cursor's diorism ensuing from the twissel of the
   two-dimensional position and the motion direction."
  (location  (make-location 0 0) :type Location  :read-only NIL)
  (direction :right              :type direction :read-only NIL))

;;; -------------------------------------------------------

(defun advance-instruction-pointer (ip)
  "Translates the instruction pointer IP one step into its currently
   installed direction and returns no value."
  (declare (type Instruction-Pointer ip))
  (translate-location-to
    (instruction-pointer-location  ip)
    (instruction-pointer-direction ip))
  (values))

;;; -------------------------------------------------------

(defun move-instruction-pointer-to (ip new-position)
  "Relocates the instruction pointer IP to the NEW-POSITION and returns
   no value."
  (declare (type Instruction-Pointer ip))
  (declare (type Location            new-position))
  (set-location-to (instruction-pointer-location ip) new-position)
  (values))

;;; -------------------------------------------------------

(defun rotate-instruction-pointer (ip)
  "Rotates the instruction pointer IP according to the Number2D
   stipulations and returns no value."
  (declare (type Instruction-Pointer ip))
  (setf (instruction-pointer-direction ip)
    (get-rotated-direction
      (instruction-pointer-direction ip)))
  (values))

;;; -------------------------------------------------------

(defun rotate-instruction-pointer-horizontally (ip)
  "Rotates the instruction pointer IP according to the Number2D
   stipulations into a horizontal airt and returns no value."
  (declare (type Instruction-Pointer ip))
  (setf (instruction-pointer-direction ip)
    (get-horizontally-rotated-direction
      (instruction-pointer-direction ip)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Code-Grid" class.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Code-Grid ()
  ((cells
    :initform      (make-hash-table :test #'equalp)
    :type          (hash-table-of Location opcode)
    :documentation "A sparse two-dimensional array of cells, amenable
                    to x-y ``Location'' keys.")
   (width
    :initform      0
    :type          fixnum
    :documentation "The maximum number of columns in the grid.")
   (height
    :initform      0
    :type          fixnum
    :documentation "The maximum number of rows in the grid.")
   (start-point
    :initform      NIL
    :type          (or null Location)
    :documentation "The first instruction's location among the CELLS."))
  (:documentation
    "The ``Code-Grid'' class accoutres a representation of a Number2D
     program in a veridical two-dimensional format, its establishment
     proceeding from a Cartesian arrangement, actings as merist into
     rows and columns."))

;;; -------------------------------------------------------

(defun make-empty-code-grid ()
  "Creates and returns a fresh ``Code-Grid'' whose rows and columns
   both measure a cardinality of zero (0)."
  (the Code-Grid
    (make-instance 'Code-Grid)))

;;; -------------------------------------------------------

(defun get-grid-cell-at (grid location)
  "Returns the cell empight at the LOCATION in the GRID, or responds
   with ``NIL'' upon its disrespondency."
  (declare (type Code-Grid grid))
  (declare (type Location  location))
  (the (or null opcode)
    (nth-value 0
      (gethash location
        (slot-value grid 'cells)))))

;;; -------------------------------------------------------

(defun set-grid-cell-at (grid x y instruction)
  "Stores the INSTRUCTION in the GRID cell designated by the X- and
   Y-coordinates and returns a fresh ``Location'' instance composed of
   these spatial specifications."
  (declare (type Code-Grid grid))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (declare (type opcode    instruction))
  (let ((location (make-location x y)))
    (declare (type Location location))
    (with-slots (cells width height) grid
      (declare (type hash-table cells))
      (declare (type fixnum     width))
      (declare (type fixnum     height))
      (psetf width                    (max width  (1+ x))
             height                   (max height (1+ y))
             (gethash location cells) instruction))
    (the Location location)))

;;; -------------------------------------------------------

(defun get-grid-start-point (grid)
  "Returns the start point allocated in the code GRID, or responds with
   ``NIL'' upon its disrespondency."
  (declare (type Code-Grid grid))
  (the (or null Location)
    (slot-value grid 'start-point)))

;;; -------------------------------------------------------

(defun set-start-point-if-necessary (grid location)
  "Determines whether the code GRID currently lacks a start point, on
   confirmation appropriating the LOCATION for this purpose, otherwise
   adhibiting no causatum, and in any case returning no value."
  (declare (type Code-Grid grid))
  (declare (type Location  location))
  (with-slots (start-point) grid
    (declare (type (or null Location) start-point))
    (unless start-point
      (setf start-point location)))
  (values))

;;; -------------------------------------------------------

(defun grid-has-instruction-at-p (grid location)
  "Determines whether the code GRID entails a cell at the specified
   LOCATION, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Code-Grid grid))
  (declare (type Location  location))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash location
          (slot-value grid 'cells))))))

;;; -------------------------------------------------------

(defun grid-contains-point-at-p (grid point)
  "Determines whether POINT resides inside of the the code GRID's
   bournes, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Code-Grid grid))
  (declare (type Location  point))
  (the boolean
    (get-boolean-value-of
      (with-slots (width height) grid
        (declare (type fixnum width))
        (declare (type fixnum height))
        (and
          (<= 0 (location-x point) (1- width))
          (<= 0 (location-y point) (1- height)))))))

;;; -------------------------------------------------------

(defun adjust-point-to-grid (grid point)
  "Adjusts the POINT's coordinates with respect to the GRID's bournes
   and returns no value."
  (declare (type Code-Grid grid))
  (declare (type Location  point))
  (with-slots (width height) grid
    (declare (type fixnum width))
    (declare (type fixnum height))
    (cond
      ((minusp (location-x point))
        (setf (location-x point) (1- width)))
      ((>= (location-x point) width)
        (setf (location-x point) 0))
      (T
        NIL))
    (cond
      ((minusp (location-y point))
        (setf (location-y point) (1- height)))
      ((>= (location-y point) height)
        (setf (location-y point) 0))
      (T
        NIL)))
  (values))

;;; -------------------------------------------------------

(defun instruction-pointer-outside-of-grip-p (grid ip)
  "Determines whether the instruction pointer IP currently occupies a
   location outside of the GRID's bournes, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Code-Grid           grid))
  (declare (type Instruction-Pointer ip))
  (the boolean
    (not
      (grid-contains-point-at-p grid
        (instruction-pointer-location ip)))))

;;; -------------------------------------------------------

(defun build-code-grid (code)
  "Creates and returns a fresh ``Code-Grid'' based upon the piece of
   Number2D source code."
  (declare (type string code))
  (let ((grid (make-empty-code-grid))
        (x    0)
        (y    0))
    (declare (type Code-Grid grid))
    (declare (type fixnum    x))
    (declare (type fixnum    y))
    (loop for token of-type character across code do
      (case token
        (#\Newline
          (setf x 0)
          (incf y 1))
        (#\Space
          (incf x 1))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6)
          (set-start-point-if-necessary grid
            (set-grid-cell-at grid x y
              (digit-char-p token)))
          (incf x 1))
        (otherwise
          (error "Invalid character \"~c\" in row ~d, column ~d."
            token y x))))
    (the Code-Grid grid)))

;;; -------------------------------------------------------

(defmethod print-object ((grid Code-Grid) (stream T))
  (declare (type Code-Grid   grid))
  (declare (type destination stream))
  (with-slots (width height) grid
    (declare (type fixnum width))
    (declare (type fixnum height))
    (format stream "~&Grid of ~d x ~d cells, " width height)
    (with-slots (start-point) grid
      (declare (type (or null Location) start-point))
      (if start-point
        (format stream "anchored at (x=~d,y=~d):"
          (location-x start-point)
          (location-y start-point))
        (format stream "without start point:")))
    (let ((current-location (make-location 0 0)))
      (declare (type Location current-location))
      (dotimes (y height)
        (declare (type fixnum y))
        (format stream "~&")
        (dotimes (x width)
          (declare (type fixnum x))
          (move-location-to current-location x y)
          (if (grid-has-instruction-at-p grid current-location)
            (format stream "~d"
              (get-grid-cell-at grid current-location))
            (format stream " ")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Tape" class.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer bit)
    :documentation "A sparse vector of cells, represented by a hash
                    table, the keys of which accommodate the cell
                    indices, while their affiliated values equiparate
                    with the cell contents.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The mobile cell pointer, mediated by the currently
                    selected cell's index, which is a tantamount of its
                    key in the CELLS hash table."))
  (:documentation
    "The ``Tape'' class implements the Number2D program memory as a
     tape composed of a bilaterally infinite tally of bit-valued cells,
     the active member among which at any instant answers to a mobile
     cell pointer."))

;;; -------------------------------------------------------

(defun make-empty-tape ()
  "Creates and returns a fresh ``Tape'' whose cells' entirety is
   initialized to the inicipial state of zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun get-cell-value-at (tape index)
  "Returns the bit stored in the TAPE cell amenable to the INDEX."
  (declare (type Tape    tape))
  (declare (type integer index))
  (the bit
    (gethash index
      (slot-value tape 'cells)
      0)))

;;; -------------------------------------------------------

(defun get-current-cell-value (tape)
  "Returns the bit stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the bit
    (get-cell-value-at tape
      (slot-value tape 'pointer))))

;;; -------------------------------------------------------

(defun set-current-cell-value (tape new-value)
  "Stores the NEW-VALUE in the TAPE's current cell and returns no
   value."
  (declare (type Tape tape))
  (declare (type bit  new-value))
  (setf (gethash (slot-value tape 'pointer)
          (slot-value tape 'cells))
    new-value)
  (values))

;;; -------------------------------------------------------

(defun flip-current-cell-value (tape)
  "Negates the bit stored in the TAPE's current cell and returns no
   value."
  (declare (type Tape tape))
  (set-current-cell-value tape
    (- 1 (get-current-cell-value tape)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (tape)
  "Determines whether the TAPE's current cell contains a zero-valued
   bit, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (zerop
        (get-current-cell-value tape)))))

;;; -------------------------------------------------------

(defun extract-current-byte (tape)
  "Proceeding from the TAPE's current cell, extracts a catena of the
   eight subsequent bits and returns an unsigned byte value
   representation thereof.
   ---
   Please heed that the resulting octet's assemblage proceeds from the
   conflation of the current cell pointer position as the designation
   of the most significant bit (MSB) towards the least significant one
   (LSB); as a consectary, the further a tape bit is eloigned from this
   cursor, the less its magnitude in the constructed byte value."
  (declare (type Tape tape))
  (let ((byte-value #b11111111))
    (declare (type (unsigned-byte 8) byte-value))
    (loop
      for cell-index
        of-type integer
        from    (slot-value tape 'pointer)
        to      (+ (slot-value tape 'pointer) 7)
      for bit-position
        of-type (integer -1 7)
        from    7
        downto  0
      do
        (setf (ldb (byte 1 bit-position) byte-value)
          (get-cell-value-at tape cell-index)))
    (the (unsigned-byte 8) byte-value)))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for the interpreter.")
    :type          Code-Grid
    :documentation "The Number2D program to evaluate, furnished in the
                    form of a two-dimensional grid.")
   (ip
    :initform      (make-instruction-pointer)
    :type          Instruction-Pointer
    :documentation "The instruction pointer (IP) or program counter
                    (PC), designated as a cursor on the two-dimensional
                    PROGRAM grid, compact of a twain of a location and
                    a direction.")
   (skips-next-instruction-p
    :initform      NIL
    :accessor      skips-next-instruction-p
    :type          boolean
    :documentation "A Boolean flag to signify whether the next
                    instruction shall be ignored.")
   (program-halted-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the Number2D
                    PROGRAM has been terminated.")
   (tape
    :initform      (make-empty-tape)
    :type          Tape
    :documentation "The program memory, conceived as a bilaterally
                    infinite dispansion of bit-values cells."))
  (:documentation
    "The ``Interpreter'' class is entalented with that dever that
     obliges it to accompass actual efficacy to a Number2D program
     supplied in a reticulate ordonnance."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Relocates the INTERPRETER's instruction pointer (IP) to the start
   point in its grid, if possible, otherwise flags the program as
   terminated, in any case returning no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip program-halted-p) interpreter
    (declare (type Code-Grid           program))
    (declare (type Instruction-Pointer ip))
    (declare (type boolean             program-halted-p))
    (if (get-grid-start-point program)
      (move-instruction-pointer-to ip
        (get-grid-start-point program))
      (setf program-halted-p T)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Number2D
   PROGRAM's evaluation, expecting its induction as a two-dimensional
   grid."
  (declare (type Code-Grid program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-halted-p (interpreter)
  "Determines whether the program maintained by the INTERPRETER is
   halted, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (slot-value interpreter 'program-halted-p)))

;;; -------------------------------------------------------

(defun halt-program (interpreter)
  "Terminates the program maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'program-halted-p) T)
  (values))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the next
   cell and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type Code-Grid           program))
    (declare (type Instruction-Pointer ip))
    (advance-instruction-pointer ip)
    (adjust-point-to-grid program
      (instruction-pointer-location ip)))
  (values))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction stored in the INTERPRETER code grid's
   currently selected cell."
  (declare (type Interpreter interpreter))
  (the opcode
    (get-grid-cell-at
      (slot-value interpreter 'program)
      (instruction-pointer-location
        (slot-value interpreter 'ip)))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter) (instruction (eql 0)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (halt-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql 1)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (rotate-instruction-pointer
      (slot-value interpreter 'ip))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql 2)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (rotate-instruction-pointer-horizontally
      (slot-value interpreter 'ip))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql 3)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (with-slots (tape) interpreter
      (declare (type Tape tape))
      (flip-current-cell-value tape)
      (move-cell-pointer-right tape))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql 4)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (move-cell-pointer-left
      (slot-value interpreter 'tape))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql 5)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (when (current-cell-contains-zero-p
            (slot-value interpreter 'tape))
      (setf (skips-next-instruction-p interpreter) T))
    (values))
  
  (:method ((interpreter Interpreter) (instruction (eql 6)))
    (declare (type Interpreter interpreter))
    (declare (type opcode      instruction))
    (declare (ignore           instruction))
    (format T "~c"
      (code-char
        (extract-current-byte
          (slot-value interpreter 'tape))))
    (values))
  
  (:method ((interpreter Interpreter) (instruction T))
    (declare (type Interpreter interpreter))
    (declare (type T           instruction))
    (with-slots (program ip) interpreter
      (declare (type Code-Grid           program))
      (declare (type Instruction-Pointer ip))
      (when (instruction-pointer-outside-of-grip-p program ip)
        (error "Unrecognized instruction ~s at location (x=~d, y=~d)."
          instruction
          (location-x (instruction-pointer-location ip))
          (location-y (instruction-pointer-location ip)))))
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Number2D program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-halted-p interpreter) do
    (if (skips-next-instruction-p interpreter)
      (setf (skips-next-instruction-p interpreter) NIL)
      (process-instruction interpreter
        (get-current-instruction interpreter)))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Number2D (code)
  "Interprets the piece of Number2D source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (build-code-grid code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenates the LINES into a unified string, each twissel of the
   input strings separated by a single newline character, and returns
   the result."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~&~a~}" lines)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate the concept of rediction:
;;   (1) The instruction pointer (IP) rotates from the dextral to a
;;       downward airt at the "1" instruction.
;;   (2) The IP is horizontally adjusted to advance sinistrally by the
;;       "2" operation's efforts.
;;   (3) While proceeding in a leftward motion, the IP wraps around the
;;       sinistral grid margin, continuing at the dextral laterality.
;;   (4) The direction changes with the "1" instruction to an upward
;;       translation.
;;   (5) Moving up, the IP iterum encounters a border, this time the
;;       upper one, and wraps around to continue at the bottom row.
;;   (6) The "0" instruction in the right-bottom corner is activated,
;;       which terminates the program.
(interpret-Number2D
  "1
21
 0")

;;; -------------------------------------------------------

;; Demonstrate control flow.
;; 
;; This program skips the the forking "1" and hence traverses forward.
(interpret-Number2D "3510
  4
  3
  0")

;;; -------------------------------------------------------

;; Demonstrate control flow.
;; 
;; This program does not skip the forking "1" and hence traverses
;; downward.
(interpret-Number2D "34510
   4
   3
   0")

;;; -------------------------------------------------------

;; Print the letter "H".
(interpret-Number2D "334334333433433434444444460")
