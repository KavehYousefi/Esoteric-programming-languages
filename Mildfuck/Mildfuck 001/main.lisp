;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Mildfuck", invented by the Esolang user "Yb1" and presented
;; on September 8th, 2024, itself representing a modulation of Urban
;; Mueller's "brainfuck", it kenspeckle propria reside in a twissel of
;; innvoations: imprimis, a cell pointer whose mobility eludes a
;; immediate manpulation mechanism, instead relying on an intrinsic
;; direction attribute for its progression's determination; secondary,
;; the jump-based control flow facility enumerates a twissel of
;; epiphenoma in lieu of the traditional zero test criterion.
;; 
;; 
;; Concept
;; =======
;; The Mildfuck programming language pursues the telos of brainfuck's
;; mimicry by a concomitant reduction of the original instruction set
;; from an octuple cardinality to a membership of six, its chevisance
;; attained by the delegation of the concrete memory cell pointer
;; translation to an incorporated direction property, as well as a
;; conflation of the back jump instruction into the arithmetic deduction
;; operation.
;; 
;; == MILDFUCK: A "MILD" BRAIN[FUCK] ==
;; The sinistral moiety of the "Mildfuck" agnomination furnishes an
;; allusion to the language's first quadruple of instruction
;; identifiers, which coalesce into the term "mild"; while the latter
;; compartment intimates the brain[fuck] cleronomy.

;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTES ==
;; A consectary begotten by Mildfuck's cleronomy, the language's mimicry
;; of brainfuck's memory model delineates its conformation as a
;; bilaterally infinite dispansion of unsigned byte-valued cells,
;; amplified in its capacitation by a cell pointer, the dever of whom
;; wones in its selection of the currently active unit at any instant
;; during a progra's course.
;; 
;; == THE CELL POINTER: GOVERNOR OF ITS OWN AIRT ==
;; A deviation from its stock-father, the cell pointer's incorporation
;; of a direction invests this cursor with a inherent attribute that
;; eloigns from the Mildfuck instruction set one dedicated member's
;; participation. Counterdistinguished from brainfuck's express requests
;; for the cell pointer's sinistral or dextral translation, Mildfuck
;; conflates these into an aefauld actuation behest, the reified
;; epiphenoma ensue from the contemporaneous configuration.
;; 
;; 
;; Instructions
;; ============
;; Mildfuck's incorporation of the directional component in the cell
;; pointer, in lieu of explicit translation behests, and the conflation
;; applied to cell value deduction and back jump facilities serves in
;; the curtailment of the brainfuck octuple instruction set to a
;; sextuple membership.
;; 
;; == OVERVIEW ==
;; The following apercu shall serve in the adhibition of a cursory mete
;; of gnarity concerning the operative circumference:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   m       | Moves the cell pointer one step in its current
;;           | direction.
;;           |---------------------------------------------------------
;;           | The cell pointer direction assumes at a program's
;;           | inchoation a dextral orientation.
;;   ..................................................................
;;   i       | If the current cell value is less than 255, increments
;;           | the same by one; otherwise produces no effect.
;;   ..................................................................
;;   l       | Defines a return point for the next "d" instruction.
;;   ..................................................................
;;   d       | Reverses the cell pointer's direction. If the current
;;           | cell contains the value zero (0), relocates the
;;           | instruction pointer to nearest preceding "l"
;;           | instruction; otherwise decrements the current cell value
;;           | by one (1).
;;   ..................................................................
;;   g       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   o       | Prints the character whose ASCII code corresponds to
;;           | the current cell value to the standard output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand has been owes its parturition to the Common
;; Lisp programming language, the multifarious tiers of its entire wike
;; exercised directly on the input Mildfuck source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-09-24
;; 
;; Sources:
;;   [esolang2024Mildfuck]
;;   The Esolang contributors, "Mildfuck", September 22nd, 2024
;;   URL: "https://esolangs.org/wiki/Mildfuck"
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

(deftype skip-table ()
  "The ``skip-table'' type defines a unilateral affiliation betwixt a
   Mildfuck jump instruction (\"d\") and its destination point (\"l\"),
   mediated by adminculum of their zero-based positions into the code,
   and realized in a hash table that maps fixnum subscripts to values
   of selfsame species."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype pointer-direction ()
  "The ``pointer-direction'' type enumerates the recognized airts into
   which a Mildfuck tape's cell pointer may be conducted."
  '(member :left :right))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, thus spanning the closed integer range of [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of skip table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-skip-table (code)
  "Creates and returns a fresh skip table which connects the jump points
   in the piece of the Mildfuck source CODE to their respective
   destinations, both communicated via their positions in the program."
  (declare (type string code))
  (let ((skip-table    (make-hash-table :test #'eql))
        (return-points NIL))
    (declare (type skip-table       skip-table))
    (declare (type (list-of fixnum) return-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      do
        (case token
          (#\l
            (push position return-points))
          (#\d
            (if return-points
              (setf (gethash position skip-table)
                (first return-points))
              (error "No return point for jump instruction at ~
                      position ~d."
                position)))
          (otherwise NIL)))
    (the skip-table skip-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-opposite-direction (current-direction)
  (:documentation
    "Returns for the CURRENT-DIRECTION the obverse pointer direction.")
  (:method ((current-direction (eql :left)))
    (declare (type pointer-direction current-direction))
    (declare (ignore                 current-direction))
    (the pointer-direction :right))
  (:method ((current-direction (eql :right)))
    (declare (type pointer-direction current-direction))
    (declare (ignore                 current-direction))
    (the pointer-direction :left)))

;;; -------------------------------------------------------

(defgeneric get-next-position-in-direction (direction current-position)
  (:documentation
    "Returns the position obtained by translation of the
     CURRENT-POSITION by one step in the given DIRECTION.")
  (:method ((direction (eql :left)) (current-position integer))
    (declare (type pointer-direction direction))
    (declare (ignore                 direction))
    (declare (type integer           current-position))
    (the integer
      (1- current-position)))
  (:method ((direction (eql :right)) (current-position integer))
    (declare (type pointer-direction direction))
    (declare (ignore                 direction))
    (declare (type integer           current-position))
    (the integer
      (1+ current-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-empty-tape ()))
  "The ``Tape'' class serves in the furnishment of a linear arrangement
   of unsigned byte-valued cells, operated upon by a cell pointer,
   endowed with the attributes of mobility and direction, the latter
   governs the former's progression in its pursuit to select the
   currently active cell."
  (cells     (make-hash-table :test #'eql)
             :type      (hash-table-of integer octet)
             :read-only T)
  (pointer   0
             :type      integer
             :read-only NIL)
  (direction :right
             :type      pointer-direction
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

(defun current-cell-contains-zero-p (tape)
  "Determines whether the TAPE's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (zerop
      (current-cell-value tape))))

;;; -------------------------------------------------------

(defun increment-current-cell (tape)
  "Increments the TAPE's current cell value by one, if not already
   commorant on its upper extremum, and returns no value."
  (declare (type Tape tape))
  (when (< (current-cell-value tape) 255)
    (incf (current-cell-value tape)))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (tape)
  "Decrements the TAPE's current cell value by one, if not already
   commorant on its lower extremum, and returns no value."
  (declare (type Tape tape))
  (unless (current-cell-contains-zero-p tape)
    (decf (current-cell-value tape)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer (tape)
  "Translates the TAPE's cell pointer one step in its currently
   governing direction and returns no value."
  (declare (type Tape tape))
  (setf (tape-pointer tape)
    (get-next-position-in-direction
      (tape-direction tape)
      (tape-pointer   tape)))
  (values))

;;; -------------------------------------------------------

(defun reverse-cell-pointer-direction (tape)
  "Reverses the TAPE cell pointer's direction and returns no value."
  (declare (type Tape tape))
  (setf (tape-direction tape)
    (get-opposite-direction
      (tape-direction tape)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Mildfuck (code)
  "Interprets the piece of Mildfuck source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (skip-table (calculate-skip-table code))
        (tape       (make-empty-tape)))
    (declare (type fixnum     ip))
    (declare (type skip-table skip-table))
    (declare (type Tape       tape))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\m
          (move-cell-pointer tape))
        
        (#\i
          (increment-current-cell tape))
        
        (#\l NIL)
        
        (#\d
          (reverse-cell-pointer-direction tape)
          (if (current-cell-contains-zero-p tape)
            (setf ip (gethash ip skip-table))
            (decrement-current-cell tape)))
        
        (#\g
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell-value tape)
            (char-code
              (read-char)))
          (clear-input))
        
        (#\o
          (write-char
            (code-char
              (current-cell-value tape))))
        
        (otherwise NIL))
      
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Mildfuck
  "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiioo
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio
   miiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiio")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Mildfuck "lmidgomd")
