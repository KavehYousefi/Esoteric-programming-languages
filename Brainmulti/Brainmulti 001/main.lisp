;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brainmulti", invented by the Esolang user "Cinnamony" and
;; presented on June 21st, 2023, the haecceity of which resides in its
;; extension of Urban Mueller's brainfuck anent its program memory to
;; a three-dimensional arrangement of unsigned byte-valued cells.
;; 
;; 
;; Concept
;; =======
;; The Brainmulti programming language constitutes a derivation of
;; brainfuck, its sole point of divergence maintaining its woning in the
;; program memory, the same extends from a catena of octet-valued cells
;; to a three-dimensional reticulation thereof.
;; 
;; == THE MEMORY: A THREE-DIMENSIONAL ARRAY OF UNSIGNED BYTES ==
;; Its augmentation of brainfuck's program memory exends the
;; one-dimensional arrangement of unsigned byte cells to a
;; three-dimesional spatial ordonnance, amenable to a diction desumed
;; from the conventions of compass navigation, scilicet, "north",
;; "south", "west", and "east", in conjunction with "up" and "down"
;; motions for the cell pointer's negotiation athwart this more potent
;; spatiality.
;; 
;; 
;; Instructions
;; ============
;; Establishing an extension of brainfuck's cleronomy, enhances the
;; stock-father's octuple tally of operations to a duodecimal
;; cardinality in order to accommodate the three-dimesional memory
;; space.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall constitute the furnishment of a
;; cursory mete of gnarity concerning the language's operative features:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the north.
;;   ..................................................................
;;   <       | Translates the cell pointer oen step to the south.
;;   ..................................................................
;;   ^       | Translates the cell pointer one step to the west.
;;   ..................................................................
;;   v       | Translates the cell pointer one step to the east.
;;   ..................................................................
;;   o       | Translates the cell pointer one step upwards.
;;   ..................................................................
;;   O       | Translates the cell pointer one step downwards.
;;   ..................................................................
;;   +       | Increments the current cell value by one. If the new
;;           | state exceeds the inclusive upper bourne of 255, the
;;           | value wraps around to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one. If the new
;;           | state transgresses the inclusive lower bourne of zero
;;           | (0), the value wraps around to the maximum of 255.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been developed in the programming language
;; Common Lisp, its efforts exercised immediately on the Brainmulti
;; source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date: 2024-09-24
;; 
;; Sources:
;;   [esolang2024Brainmulti]
;;   The Esolang contributors, "Brainmulti", May 7th, 2024
;;   URL: "https://esolangs.org/wiki/Brainmulti"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-derived-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type the agnomination of which is appropriated from
   the TYPE-NAME, its formal parameters the LAMBDA-LIST's dation, and
   whose subject of the docimasy receives in norning from the
   CANDIDATE-NAME, evaluating the BODY forms with access to the latter
   two pieces of information, and expected to return in its desinent
   form's primary value a generalized boolean value of true to express
   candidate's covenableness, otherwise, for its rejection, to produce
   a false response.
   ---
   The first BODY form, if resolving to a string object, experiences an
   adhibition of a construe as a documentation string to the derived
   type, and as such is reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
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

(define-derived-type hash-table-of (candidate
                                    &optional (key-type   T)
                                              (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each member composed of a key
   complying with the KEY-TYPE and a value of the VALUE-TYPE, both
   defaulting to the comprehensive ``T''."
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
             (typep value value-type)))))

;;; -------------------------------------------------------

(define-derived-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the
   same defaults to the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral association betwixt the
   jump points in a Brainmulti program, realized in a hash table whose
   keys and values both replicate the respective locations as zero-based
   fixnum positions inside of the code."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, thus spanning the closed integer range of [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-jump-table ()
  "Creates and returns a fresh and vacant ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the jump START-POINT and END-POINT in the JUMP-TABLE and
   returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun calculate-jump-table (code)
  "Creates and returns a fresh jump table which connects the jump points
   in the piece of the Brainmulti source CODE in a bilteral fashion,
   communicated via their positions in the program."
  (declare (type string code))
  (let ((jump-table   (make-empty-jump-table))
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      do
        (case token
          (#\[
            (push position start-points))
          (#\]
            (if start-points
              (connect-jump-points jump-table
                (pop start-points)
                position)
              (error "No start point for back jump instruction at ~
                      position ~d found."
                position)))
          (otherwise NIL)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table departure-point)
  "Returns the jump destination position associated with the
   DEPARTURE-POINT in the JUMP-TABLE, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     departure-point))
  (the fixnum
    (or (gethash departure-point jump-table)
        (error "No destination associated with the jump point ~d."
                departure-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of point class.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Point
  (:constructor make-point (x y z)))
  "The ``Point'' class establishes a model for a three-dimensional
   location specification, supputated along the x-, y-, and z-axes."
  (x 0 :type integer :read-only NIL)
  (y 0 :type integer :read-only NIL)
  (z 0 :type integer :read-only NIL))

;;; -------------------------------------------------------

(defun get-translated-point (original-point by-x by-y by-z)
  "Returns a fresh ``Point'' obtained by a translation of the
   ORIGINAL-POINT along the x-axis by BY-X, along the y-axis by BY-Y,
   and along the z-axis by BY-Z.
   ---
   The ORIGINAL-POINT will not be subjected to modifications."
  (declare (type Point  original-point))
  (declare (type fixnum by-x))
  (declare (type fixnum by-y))
  (declare (type fixnum by-z))
  (the Point
    (make-point
      (+ (point-x original-point) by-x)
      (+ (point-y original-point) by-y)
      (+ (point-z original-point) by-z))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-empty-tape ()))
  "The ``Tape'' class furnishes an implementation of the Brainmulti
   program memory in the guise of an infinite dispasion of unsigned
   byte-valued cells, its commorancy a three-dimensional Cartesian
   space, inwith which operates a cell pointer whose onus lies in the
   selection of the currently active cell, this cursor's amenability to
   poco a poco translations capacitating the choice's modulation.
   ---
   The Brainmulti programming language's parlance induces a divergence
   from the Cartesian notions of x-, y-, and z-coordinates, employing
   a compass' cardinal directions in champarty with vertical position
   designators; forecause siccan purview of conflict shall be
   alleviated, the following tabular juxtaposition serves in the two
   systems' equiparation:
     -------------------------------------------------
     Brainmulti direction | Cartesian coordinate axis
     ---------------------+---------------------------
     north                | positive y-axis
     south                | negative y-axis
     west                 | negative x-axis
     east                 | positive x-axis
     up                   | positive z-axis
     down                 | negative z-axis
     -------------------------------------------------
   As a consectary, the traditional Cartesian coordinates triplet
     (x, y, z)
   transmogrifies into the Brainmulti directional specification, where
   the positive member of the twissel precedes the negative in the
   listing:
     (east/west, north/south, up/down)"
  (cells     (make-hash-table :test #'equalp)
             :type      (hash-table-of Point octet)
             :read-only T)
  (pointer   (make-point 0 0 0)
             :type      Point
             :read-only NIL))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the TAPE's current cell value."
  (declare (type Tape tape))
  (the octet
    (gethash
      (tape-pointer tape)
      (tape-cells   tape)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   preceded by a wrapping adjustment of the input in order to
   accommodate the imposed byte range of [0, 255], and returns no
   value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf (gethash
          (tape-pointer tape)
          (tape-cells   tape)
          0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (tape)
  "Increments the TAPE's current cell value, contingently wrapping
   around along its upper bourne, and returns no value."
  (declare (type Tape tape))
  (incf (current-cell-value tape))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (tape)
  "Decrements the TAPE's current cell value, contingently wrapping
   around along its lower bourne, and returns no value."
  (declare (type Tape tape))
  (decf (current-cell-value tape))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (tape)
  "Determines whether the TAPE's current cell comprehends the value
   zero (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (not (null
      (zerop
        (current-cell-value tape))))))

;;; -------------------------------------------------------

(defun translate-cell-pointer (tape by-x by-y by-z)
  "Translates the TAPE's cell pointer along its three dimensions
   relative to its current location as specified along the west/east
   axis by BY-X, along the north/south axis by BY-Y, and along the
   up/down axis by BY-Z, and returns no value."
  (declare (type Tape    tape))
  (declare (type integer by-x))
  (declare (type integer by-y))
  (declare (type integer by-z))
  (setf (tape-pointer tape)
    (get-translated-point
      (tape-pointer tape)
      by-x by-y by-z))
  (values))

;;; -------------------------------------------------------

(defun move-tape-north (tape)
  "Translates the TAPE's cell pointer one step to the north and returns
   no value."
  (declare (type Tape tape))
  (translate-cell-pointer tape 0 +1 0)
  (values))

;;; -------------------------------------------------------

(defun move-tape-south (tape)
  "Translates the TAPE's cell pointer one step to the south and returns
   no value."
  (declare (type Tape tape))
  (translate-cell-pointer tape 0 -1 0)
  (values))

;;; -------------------------------------------------------

(defun move-tape-west (tape)
  "Translates the TAPE's cell pointer one step to the west and returns
   no value."
  (declare (type Tape tape))
  (translate-cell-pointer tape -1 0 0)
  (values))

;;; -------------------------------------------------------

(defun move-tape-east (tape)
  "Translates the TAPE's cell pointer one step to the east and returns
   no value."
  (declare (type Tape tape))
  (translate-cell-pointer tape +1 0 0)
  (values))

;;; -------------------------------------------------------

(defun move-tape-up (tape)
  "Translates the TAPE's cell pointer one step up and returns no value."
  (declare (type Tape tape))
  (translate-cell-pointer tape 0 0 +1)
  (values))

;;; -------------------------------------------------------

(defun move-tape-down (tape)
  "Translates the TAPE's cell pointer one step down and returns no
   value."
  (declare (type Tape tape))
  (translate-cell-pointer tape 0 0 -1)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Brainmulti (code)
  "Interprets the piece of Brainmulti source CODE and returns no value."
  (declare (type string code))
  (let ((ip          0)
        (jump-points (calculate-jump-table code))
        (tape        (make-empty-tape)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-points))
    (declare (type Tape       tape))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\>
          (move-tape-north tape))
        (#\<
          (move-tape-south tape))
        (#\^
          (move-tape-west  tape))
        (#\v
          (move-tape-east tape))
        (#\o
          (move-tape-up    tape))
        (#\O
          (move-tape-down  tape))
        (#\+
          (increment-current-cell tape))
        (#\-
          (decrement-current-cell tape))
        (#\.
          (format T "~c"
            (code-char
              (current-cell-value tape))))
        (#\,
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell-value tape)
            (char-code
              (read-char NIL NIL #\Null)))
          (clear-input))
        (#\[
          (when (current-cell-contains-zero-p tape)
            (setf ip
              (get-jump-destination jump-points ip))))
        (#\]
          (unless (current-cell-contains-zero-p tape)
            (setf ip
              (get-jump-destination jump-points ip))))
        (otherwise
          NIL))
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Kiwiscript: Print "kiwi".
(interpret-Brainmulti
  "++++++++++[v++++++++++^-]v+++++++.
   [^+v-]^--.
   ++++++++++++++.
   --------------.")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Brainmulti ",.[--o+[oo]O[.]OO]")
