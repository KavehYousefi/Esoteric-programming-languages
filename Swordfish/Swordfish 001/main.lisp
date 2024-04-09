;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Swordfish", invented by the Esolang user "Numeri" and
;; presented on August 26th, 2013, appertaining to the language "><>",
;; also stevened "Fish", of the user "Harpyon", as its entheus, the
;; diorism of which wones in its two-dimensional Cartesian code layout,
;; the single-symbol instructions of which manipulate a trinity of
;; memory components, namely a stack, a scalar variable, and a scalar
;; register, whose coefficiency capacitates the operative causata.
;; 
;; 
;; Concept
;; =======
;; The Swordfish programming language operates on a two-dimensional grid
;; of characters, the navigation across the spatial dispansion
;; perpetuated in a pursuit to manipulate the treble of a stack, a
;; scalar variable, and a register entalented with a simulacrum of the
;; latter's conformation.
;; 
;; == PROGRAMS FORM A TWO-DIMENSIONAL GRID ==
;; A Swordfish program's layout follows a two-dimensional Cartesian
;; arrangement of characters, across which an instruction pointer (IP)
;; perambulates.
;; 
;; == THE MEMORY: COMPOSITION OF STACK, SCALAR VARIABLE, AND REGISTER ==
;; The language's data segment intrines the services of a stack, a
;; scalar register, and a variable of the latter's singleton nature,
;; all membership among this triad entalented with a generous mete of
;; mickleness in the admissive data item types.
;; 
;; 
;; Architecture
;; ============
;; The language's architectural department enumerates three
;; constituents ligated into collaborations for a program's efficacy:
;; the stack, the variable, and the register.
;; 
;; == THE STACK: A LAST-IN FIRST-OUT STORAGE ==
;; The paravail vehicle of potence is realized in the program stack, the
;; mickleness of whom does not wist of any bournes.
;; 
;; The stack admits any species of object, which in its diorism enlists
;; Boolean truth values, signed and unsigned integer numbers, and
;; strings of arbitrary composition.
;; 
;; The champarty of the stack and variable, in particular, serves in a
;; Swordfish program's capacitation for sophistication in solving
;; problems.
;; 
;; == THE VARIABLE: A VOLATILE SALVATORY ==
;; A conspicable transience wones in the variable, a compeer to the more
;; retentive register, partaking of a tantamount in the quantity and
;; nature of its content, yet intended for more intense intercourse with
;; the stack and, a fortiori, further crebritude in its value's
;; forfeiture.
;; 
;; The aefauld variable desumes its scalar element from the same
;; contingency as the stack's membership, which enumerates Boolean
;; values, integral numbers, and strings.
;; 
;; A dioristic proprium maintaining its commorancy in this salvatory
;; constitutes the content's clearance as certain instruction's
;; epiphenomenon, as well as the potential for the purge's direct
;; instigation.
;; 
;; == THE REGISTER: A PERMANENT SALVATORY ==
;; A parhedral conspecies from the variable's foundatinoal principle,
;; the register's participation, maugre the tendance to the siccan
;; variety and quantity of elements as its peer, accounts for a more
;; permanent storage alternative, albeit less involved in the
;; intercourse with the stack.
;; 
;; 
;; Data Types
;; ==========
;; The Swordfish programming language's data bailiwick amplects a triad
;; of species, namely, signed integer numbers, strings, and Boolean
;; truth values.
;; 
;; == INTEGER NUMBERS: WARKLUMES OF ARITHMETICS ==
;; Allocated a rank equipendent with string objects, integral numbers,
;; of any polarity and magnitude, contribute the constituents endowed
;; with a most elevated versatility.
;; 
;; A slight superiority in their agency appertains to integers being the
;; sole input tokens.
;; 
;; == STRINGS: CHARACTER SEQUENCES ==
;; Approximating an excellence capable of being equipensated with the
;; integral aspect, strings of any extent permit the production of
;; content intended for output purposes.
;; 
;; == BOOLEAN TRUTH VALUES: ADMINICLES FOR CONDITIONAL EXECUTION ==
;; A parergal conspecific among the types, the twissel of Boolean truth
;; values, "true" and "false", ensues in its existency only when
;; begotten by an equiparation betwixt the top stack element and the
;; variable's state, replicating the juxtaposition's response iterum on
;; the stack.
;; 
;; This data type's involvement conflates in an exclusive manner with
;; the conditional execution instructions, "{" and "}".
;; 
;; 
;; Instructions
;; ============
;; Swordfish's operative componency enumerates a membership of 28
;; constituents, the cardinality's amplification being a consectary of
;; the tolerance adhibited to any non-instruction token whose admission
;; instigates a Procrustean respondency in the program variable.
;; 
;; == CATEGORIES OF INSTRUCTIONS ==
;; The circumference of Swordfish's competences amplects a wide gamut of
;; categories, whence shall originate a listing whose entirety is
;; segregated into the conceptual species.
;; 
;; Please note that any symbol not partaking of which tabulation is
;; added to the variable according to a particular formula, its
;; elucidation is offered aboon.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   NAVIGATIONAL INSTRUCTIONS (DIRECTION MODULATORS)
;;   ------------------------------------------------------------------
;;   >       | Rotate to right
;;   ..................................................................
;;   <       | Rotate to left
;;   ..................................................................
;;   ^       | Rotate to up
;;   ..................................................................
;;   v       | Rotate to down
;;   ..................................................................
;;   |       | Mirror horizontally: left <-> right
;;   ..................................................................
;;   _       | Mirror vertically:   up <-> down
;;   ..................................................................
;;   /       | Mirror diagonally:   left <-> up, right <-> down
;;   ..................................................................
;;   \       | Mirror diagonally:   left <-> down, right <-> up
;;   ==================================================================
;;   CONTROL FLOW INSTRUCTIONS
;;   ------------------------------------------------------------------
;;   !       | Skip unconditionally
;;   ..................................................................
;;   {       | Skip if stack top is false
;;   ..................................................................
;;   }       | Skip if stack top is true
;;   ..................................................................
;;   ;       | Halt program
;;   ==================================================================
;;   ARITHMETICS
;;   ------------------------------------------------------------------
;;   +       | Add stack top to variable
;;   ..................................................................
;;   -       | Subtract stack top from variable
;;   ..................................................................
;;   *       | Multiply variable by stack top
;;   ..................................................................
;;   d       | Divide variable by stack top
;;   ..................................................................
;;   D       | Divide stack top by variable
;;   ==================================================================
;;   LOGICAL OPERATIONS
;;   ------------------------------------------------------------------
;;   =       | If stack top = variable, push "true", otherwise "false"
;;   ==================================================================
;;   INPUT AND OUTPUT
;;   ------------------------------------------------------------------
;;   :       | Append next character to variable
;;   ..................................................................
;;   #       | Print variable
;;   ..................................................................
;;   ?       | Query for integer input
;;   ==================================================================
;;   MEMORY MANAGEMENT
;;   ------------------------------------------------------------------
;;   [       | Pop stack top into register
;;   ..................................................................
;;   ]       | Push register onto stack top
;;   ..................................................................
;;   %       | Clear variable
;;   ..................................................................
;;   ~       | Push variable onto stack top
;;   ..................................................................
;;   @       | Pop stack top into variable
;;   ==================================================================
;;   META COMMANDS
;;   ------------------------------------------------------------------
;;   ,       | No-operation (NOP)
;;   ..................................................................
;;   $       | Sleep amount of milliseconds equal to variable
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; The following apercu shall serve in a cursory gnarity's adhibition
;; concerning the language's operative potentials.
;; 
;; Please note that any symbol not partaking of which tabulation is
;; added to the variable according to a particular formula, its
;; elucidation is offered aboon.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Sets the instruction pointer (IP) direction to a
;;           | rightward movement.
;;   ..................................................................
;;   >       | Sets the instruction pointer (IP) direction to a
;;           | leftward movement.
;;   ..................................................................
;;   ^       | Sets the instruction pointer (IP) direction to an upward
;;           | movement.
;;   ..................................................................
;;   v       | Sets the instruction pointer (IP) direction to a
;;           | downward movement.
;;   ..................................................................
;;   |       | Modifies the instruction pointer (IP) direction in a
;;           | horizontal mode:
;;           |   ----------------------------------
;;           |   Input direction | Output direction
;;           |   ----------------+-----------------
;;           |   down            | down
;;           |   ..................................
;;           |   left            | right
;;           |   ..................................
;;           |   right           | left
;;           |   ..................................
;;           |   up              | up
;;           |   ----------------------------------
;;   ..................................................................
;;   _       | Modifies the instruction pointer (IP) direction in a
;;           | vertical mode:
;;           |   ----------------------------------
;;           |   Input direction | Output direction
;;           |   ----------------+-----------------
;;           |   down            | up
;;           |   ..................................
;;           |   left            | left
;;           |   ..................................
;;           |   right           | right
;;           |   ..................................
;;           |   up              | down
;;           |   ----------------------------------
;;   ..................................................................
;;   /       | Modifies the instruction pointer (IP) direction
;;           | according to the following mirror rules:
;;           |   ----------------------------------
;;           |   Input direction | Output direction
;;           |   ----------------+-----------------
;;           |   down            | left
;;           |   ..................................
;;           |   left            | down
;;           |   ..................................
;;           |   right           | up
;;           |   ..................................
;;           |   up              | right
;;           |   ----------------------------------
;;   ..................................................................
;;   \       | Modifies the instruction pointer (IP) direction
;;           | according to the following mirror rules:
;;           |   ----------------------------------
;;           |   Input direction | Output direction
;;           |   ----------------+-----------------
;;           |   down            | right
;;           |   ..................................
;;           |   left            | up
;;           |   ..................................
;;           |   right           | down
;;           |   ..................................
;;           |   up              | left
;;           |   ----------------------------------
;;   ..................................................................
;;   !       | Skips the subsequent instruction.
;;   ..................................................................
;;   {       | Pops the top stack element; if its value constitutes a
;;           | Boolean value of "false", skips the subsequent
;;           | instruction. Otherwise proceeds as usual.
;;   ..................................................................
;;   }       | Pops the top stack element; if its value constitutes a
;;           | Boolean value of "true", skips the subsequent
;;           | instruction. Otherwise proceeds as usual.
;;   ..................................................................
;;   ,       | Constitutes a no-operation (NOP), that is, accompasses
;;           | no causatum.
;;   ..................................................................
;;   ;       | Immediately terminates the program.
;;   ..................................................................
;;   +       | Pops the stack's top element and increments the variable
;;           | by the same value:
;;           |   let topElement <- pop from stack
;;           |   variable       <- variable + topElement
;;   ..................................................................
;;   -       | Pops the stack's top element and decrements the variable
;;           | by the same value:
;;           |   let topElement <- pop from stack
;;           |   variable       <- variable - topElement
;;   ..................................................................
;;   *       | Pops the stack's top element and multiplis the variable
;;           | by the same value:
;;           |   let topElement <- pop from stack
;;           |   variable       <- variable * topElement
;;   ..................................................................
;;   d       | Pops the stack's top element, divides the variable
;;           | by the same value, and rounds the result to the nearest
;;           | integer:
;;           |   let topElement <- pop from stack
;;           |   variable       <- round(variable / topElement)
;;           |---------------------------------------------------------
;;           | This operation modifies the variable.
;;   ..................................................................
;;   D       | Pops the stack's top element, calculates the quotient
;;           | produced by dividing this value by the variable, without
;;           | modifying the variable, rounds the quotient to the
;;           | nearest integer, and pushes it unto the stack:
;;           |   let topElement <- pop from stack
;;           |   let quotient   <- round(topElement / variable)
;;           |   push quotient onto the stack
;;           |---------------------------------------------------------
;;           | This operation does not modify the variable.
;;   ..................................................................
;;   =       | Pops the stack's top element; if the same equals the
;;           | variable's value, pushes a Boolean truth value of "true"
;;           | onto the stack, otherwise pushes "false".
;;   ..................................................................
;;   :       | Appends the subsequent character to the variable.
;;           |---------------------------------------------------------
;;           | If the variable constitutes an integer number and the
;;           | character represents a decimal digit, the digit's
;;           | numeric value is appended to the variable, that is,
;;           | it holds, for the numeric value c(n) of the character c:
;;           |   variable <- (variable * 10) + c(n)
;;           | Otherwise, if the variable does not store an number or
;;           | the character does correspond to a decimal digit, the
;;           | variable is converted to a string, and the character is
;;           | appended to the same, which means, for a character c:
;;           |   variable <- ascertain that variable is a string
;;           |   variable <- append c to variable
;;   ..................................................................
;;   #       | Prints the content of the variable in a covenable form
;;           | to the standard output.
;;   ..................................................................
;;   ?       | Queries the standard input for a signed or unsigned
;;           | integer number and stores the same in the variable.
;;   ..................................................................
;;   [       | Pops the stack's top element and stores the same in the
;;           | regster.
;;   ..................................................................
;;   ]       | Pushes the register's value onto the stack without
;;           | modifying the register itself.
;;   ..................................................................
;;   %       | Clears the variable.
;;   ..................................................................
;;   ~       | Pushes the variable's value onto the stack, while
;;           | concomitantly clearing the variable.
;;   ..................................................................
;;   @       | Pops the stack's top element and stores the same in the
;;           | variable
;;   ..................................................................
;;   ,       | Accompasses no effect, that is, provides a no-operation
;;           | (NOP).
;;   ..................................................................
;;   $       | Delays the program's execution for a tally of
;;           | milliseconds equal to the variable's value.
;;   ------------------------------------------------------------------
;; 
;; Any character disembodied from an explicit agency in this operation
;; table experiences a subjection to the same rule as the symbol
;; succeeding the ":" instruction, which resolves to an appendage to the
;; program variable in concord with the following stipulations:
;; 
;;   (a) If the variable maintains an integer number and the
;;       non-instruction character c represents a decimal digit, the
;;       digit's numeric value, c(n), is appended to the variable as a
;;       terminal digit --- or, tantamount in its causatum, it holds:
;;       
;;         variable <- (variable * 10) + c(n)
;;   
;;   (b) In any other case, which appertains to the variable's castaldy
;;       of a non-numeric entity and/or the character c's disassociation
;;       from a decimal digit, the variable value is treated as a
;;       string, and the character c is appended in an ipsissima verba
;;       fashion, which designates the process as follows:
;;       
;;         variable <- ensure string value
;;         variable <- append c to variable
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-02
;; 
;; Sources:
;;   [esolang2021Swordfish]
;;   The Esolang contributors, Swordfish", May 5th, 2021
;;   URL: "https://esolangs.org/wiki/Swordfish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hash table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro every-table-entry-satisfies-p
    ((key-variable value-variable) probed-table
     &body body)
  "Determines whether each of the PROBED-TABLE's entry's keys and
   values, the former being bound to the KEY-VARIABLE, the latter to the
   VALUE-VARIABLE satisfies the predicate imposed by the BODY, the
   primary return value ought to return a generalized boolean value of
   \"true\" if the PROBED-TABLE satisfies the requirement, otherwise
   ``NIL'' for a failure to comply, finally this operation returns a
   ``boolean'' value of ``T'' for confirmed covenableness, or ``NIL''
   upon its refutation."
  `(the boolean
     (not (null
       (loop
         for ,key-variable
           of-type T
           being the hash-keys in ,probed-table
         using  (hash-value ,value-variable)
         always (progn ,@body))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type via the ``deftype'' infrastructure in
   coefficiency with the ``satisfies'' type specifier, the agnomination
   of which proceeds from the TYPE-NAME, the formal parameters being
   assumed verbatim from the LAMBDA-LIST, while the predicate is
   constructed from an anonymous lambda function, the CANDIDATE-VARIABLE
   stevenes the object to be assayed, evaluating the BODY forms, the
   result of which determines the candidate's covenableness, the primary
   return value of the desinent BODY form, if producing a non-``NIL''
   response, ascertains the eligibility, the ``NIL'' value signals its
   failure."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(or (and (stringp (first body)) (pop body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype direction ()
  "The ``direction'' type enumerates the recognized airts along which an
   instruction pointer (IP) may traverse."
  '(member
    :down
    :left
    :right
    :up))

;;; -------------------------------------------------------

(deftype mirror ()
  "The ``mirror'' type enumerates the recognized arrangements capable
   to be assumed by a conceived mirror."
  '(member
    :horizontal             ;; |
    :vertical               ;; _
    :slanted-deasil         ;; /
    :slanted-withershins))  ;; \

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among which conforms to the KEY-TYPE, allied
   with a value of the VALUE-TYPE, for both holds the comprehensive
   default of ``T''."
  (and
    (hash-table-p candidate)
    (every-table-entry-satisfies-p (key value)
      (the hash-table candidate)
      (and
        (typep key key-type)
        (typep value value-type)))))

;;; -------------------------------------------------------

(deftype cell-matrix ()
  "The ``cell-matrix'' type defines a sparse two-dimensional arrangement
   of characters, manifesting in a hash table, the keys of which
   comprehend the positions in the form of ``Location'' instances,
   mapped to the values as character objects."
  '(hash-table-of Location character))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral object greater
   than or equal to zero (0), but bourneless along the upper extremum,
   and as thus an occupant of the interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which subsumes, as a forbisen rather than a patration, the
   ``format'' and ``write-char'' functions."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, the elements of which in their entirety maintain their
   lealty to the ELEMENT-TYPE, for the same holds the comprehensive
   default of ``T''."
  (flet ((element-type-matches-p (probed-element)
          "Determines whether the PROBED-ELEMENT complies with the
           ELEMENT-TYPE, returning on confirmation a ``boolean'' value
           of ``T'', otherwise ``NIL''."
          (declare (type T probed-element))
          (the boolean
            (not (null
              (typep probed-element element-type))))))
    (and
      (listp candidate)
      (every #'element-type-matches-p
        (the list candidate)))))

;;; -------------------------------------------------------

(deftype program-stack ()
  "The ``program-stack'' type defines a list-based stack, being a
   first-in last-out data structure, the elements of which in their
   entirety subsume into the ``SFObject'' species."
  '(list-of SFObject))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variants on
   binary operators, desumed from both the bailiwick of arithmetics and
   logical expertise."
  '(member
    :add
    :append
    :divide
    :equals
    :multiply
    :subtract))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of directional operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric mirror-direction (direction mirror)
  (:documentation
    "Returns the direction obtained by applying the MIRROR effect to the
     input DIRECTION."))

;;; -------------------------------------------------------

(defmacro define-mirror-effect (input-direction mirror result-direction)
  "Defines an implementation of the generic function
   ``mirror-direction'', the first argument of which assumes the
   INPUT-DIRECTION, the second the MIRROR-DIRECTION, both stevened in an
   automatical manner, and returns the RESULT-DIRECTION as the function
   output."
  (let ((input-direction-name (gensym))
        (mirror-name          (gensym)))
    (declare (type symbol input-direction-name))
    (declare (type symbol mirror-name))
    `(defmethod mirror-direction
         ((,input-direction-name (eql ,input-direction))
          (,mirror-name          (eql ,mirror)))
        (declare (type direction ,input-direction-name))
        (declare (ignore         ,input-direction-name))
        (declare (type mirror    ,mirror-name))
        (declare (ignore         ,mirror-name))
        (the direction ,result-direction))))

;;; -------------------------------------------------------

;; Horizontal mirror: "|".
(define-mirror-effect :down  :horizontal          :down)
(define-mirror-effect :left  :horizontal          :right)
(define-mirror-effect :right :horizontal          :left)
(define-mirror-effect :up    :horizontal          :up)

;; Vertical mirror: "_".
(define-mirror-effect :down  :vertical            :up)
(define-mirror-effect :left  :vertical            :left)
(define-mirror-effect :right :vertical            :right)
(define-mirror-effect :up    :vertical            :down)

;; Deasil slanted mirror: "/".
(define-mirror-effect :down  :slanted-deasil      :left)
(define-mirror-effect :left  :slanted-deasil      :down)
(define-mirror-effect :right :slanted-deasil      :up)
(define-mirror-effect :up    :slanted-deasil      :right)

;; Withershins slanted mirror: "\".
(define-mirror-effect :down  :slanted-withershins :right)
(define-mirror-effect :left  :slanted-withershins :up)
(define-mirror-effect :right :slanted-withershins :down)
(define-mirror-effect :up    :slanted-withershins :left)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of location.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (&optional (x 0) (y 0))))
  "The ``Location'' object encapsulates a two-dimensional Cartesian
   coordinate twissel whose values are desumed from the discrete signed
   integer range."
  (x 0 :type integer :read-only NIL)
  (y 0 :type integer :read-only NIL))

;;; -------------------------------------------------------

(defun translate-location (location x-distance y-distance)
  "Moves the LOCATION horizontally by the X-DISTANCE and vertically by
   the Y-DISTANCE and returns the modified LOCATION."
  (declare (type Location location))
  (declare (type integer  x-distance))
  (declare (type integer  y-distance))
  (incf (location-x location) x-distance)
  (incf (location-y location) y-distance)
  (the Location location))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of pointer.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Pointer
  (:constructor make-pointer ()))
  "The ``Pointer'' class represents a mobile pointer, composed of its
   two-dimensional Cartesian position and the current translation
   direction."
  (location  (make-location 0 0) :type Location  :read-only NIL)
  (direction :right              :type direction :read-only NIL))

;;; -------------------------------------------------------

(defun advance-pointer (pointer)
  "Translates the POINTER one step into its current direction and
   returns no value."
  (declare (type Pointer pointer))
  (case (pointer-direction pointer)
    (:down  (incf (location-y (pointer-location pointer))))
    (:left  (decf (location-x (pointer-location pointer))))
    (:right (incf (location-x (pointer-location pointer))))
    (:up    (decf (location-y (pointer-location pointer))))
    (otherwise
      (error "Invalid pointer direction: ~s."
        (pointer-direction pointer))))
  (values))

;;; -------------------------------------------------------

(defun mirror-pointer (pointer mirror)
  "Applies the MIRROR upon the POINTER's current movement direction and
   returns no value."
  (declare (type Pointer pointer))
  (declare (type mirror  mirror))
  (setf (pointer-direction pointer)
    (mirror-direction
      (pointer-direction pointer)
      mirror))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code grid.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Grid ()
  ((width
    :initform      0
    :type          non-negative-integer
    :documentation "The number of columns comprising the grid.")
   (height
    :initform      0
    :type          non-negative-integer
    :documentation "The number of rows comprising the grid.")
   (cells
    :initform      (make-hash-table :test #'equalp)
    :type          cell-matrix
    :documentation "A sparse two-dimensional array of characters,
                    realized in a hash table whose keys maintain the
                    positions and whose values store the symbols."))
  (:documentation
    "The ``Grid'' class serves in the representation of a Swordfish
     program as a two-dimensional Cartesian grid of symbols."))

;;; -------------------------------------------------------

(defun make-empty-grid ()
  "Creates and returns an initially empty, zero-dimensioned ``Grid''."
  (the Grid
    (make-instance 'Grid)))

;;; -------------------------------------------------------

(defun set-grid-cell (grid x y character)
  "Stores the CHARACTER in the GRID's X-th column of the Y-th row and
   returns no value."
  (declare (type Grid                 grid))
  (declare (type non-negative-integer x))
  (declare (type non-negative-integer y))
  (declare (type character     character))
  (with-slots (cells width height) grid
    (declare (type cell-matrix          cells))
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (psetf (gethash (make-location x y) cells) character
           width  (max width  (1+ x))
           height (max height (1+ y))))
  (values))

;;; -------------------------------------------------------

(defun get-grid-cell (grid location)
  "Returns the symbol stored in the GRID at the LOCATION."
  (declare (type Grid      grid))
  (declare (type Location location))
  (with-slots (cells width height) grid
    (declare (type cell-matrix          cells))
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (the character
      (gethash location cells #\0))))

;;; -------------------------------------------------------

(defun build-grid (code)
  "Creates and returns a new ``Grid'' derived from the piece of
   Swordfish source CODE as its foundry."
  (declare (type string code))
  (let ((grid (make-empty-grid))
        (x    0)
        (y    0))
    (declare (type Grid                 grid))
    (declare (type non-negative-integer x))
    (declare (type non-negative-integer y))
    (loop
      for token of-type character across code
      if (char= token #\Newline) do
        (setf x 0)
        (incf y 1)
      else do
        (set-grid-cell grid x y token)
        (incf x 1))
    (the Grid grid)))

;;; -------------------------------------------------------

(defun adjust-location-to-grid (grid probed-location)
  "Modulates the PROBED-LOCATION to a position endowed with a sickerness
   of validity with respect to the GRID's boundaries, which may be one
   whose obtention ensues from a wrapping around along the GRID's
   marches, and returns the contingently modified PROBED-LOCATION."
  (declare (type Grid     grid))
  (declare (type Location probed-location))
  (with-slots (width height) grid
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (symbol-macrolet
        ((location-x (location-x probed-location))
         (location-y (location-y probed-location)))
      (declare (type integer location-x))
      (declare (type integer location-y))
      (setf location-x (mod location-x width))
      (setf location-y (mod location-y height))))
  (the Location probed-location))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) (stream T))
  (declare (type Grid        grid))
  (declare (type destination stream))
  (with-slots (width height) grid
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (let ((location (make-location 0 0)))
      (declare (type Location location))
      (dotimes (y height)
        (declare (type non-negative-integer y))
        (format stream "~&")
        (dotimes (x width)
          (declare (type non-negative-integer x))
          (setf (location-x location) x)
          (setf (location-y location) y)
          (format stream "~c"
            (get-grid-cell grid location)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of SFObjects.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (SFObject)
  "The ``SFObject'' interface furnishes a common foundry for all classes
   pursing the encapsulation of object native to the Swordfish
   programming language.")

;;; -------------------------------------------------------

(defstruct (SFBoolean
  (:include     SFObject)
  (:constructor make-sfboolean
    (generalized-boolean-value
     &aux (value (not (null generalized-boolean-value))))))
  "The ``SFBoolean'' class encapsulates a Boolean truth value
   autochthonous to the Swordfish programming language."
  (value (error "Missing Boolean truth value.")
         :type      boolean
         :read-only T))

;;; -------------------------------------------------------

(defstruct (SFCharacter
  (:include     SFObject)
  (:constructor make-sfcharacter (value)))
  "The ``SFCharacter'' class encapsulates a character autochthonous to
   the Swordfish programming language."
  (value (error "Missing character.")
         :type      character
         :read-only T))

;;; -------------------------------------------------------

(defstruct (SFInteger
  (:include     SFObject)
  (:constructor make-sfinteger (value)))
  "The ``SFInteger'' class encapsulates an signed or unsigned integer
   number autochthonous to the Swordfish programming language."
  (value (error "Missing integer value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (SFNull
  (:include     SFObject)
  (:constructor make-sfnull ()))
  "The ``SFNull'' class encapsulates the notion of a neutral or absent
   object, or \"nil\" sentinel."
  (value NIL :type null :read-only T))

;;; -------------------------------------------------------

(defstruct (SFString
  (:include     SFObject)
  (:constructor make-sfstring (value)))
  "The ``SFString'' class encapsulates a string object autochthonous to
   the Swordfish programming language."
  (value (error "Missing string value.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defgeneric get-sfobject-value (sfobject)
  (:documentation
    "Returns the value ensconced in the SFOBJECT.")
  
  (:method ((sfboolean SFBoolean))
    (declare (type SFBoolean sfboolean))
    (the boolean
      (sfboolean-value sfboolean)))
  
  (:method ((sfcharacter SFCharacter))
    (declare (type SFCharacter sfcharacter))
    (the character
      (sfcharacter-value sfcharacter)))
  
  (:method ((sfinteger SFInteger))
    (declare (type SFInteger sfinteger))
    (the integer
      (sfinteger-value sfinteger)))
  
  (:method ((sfnull SFNull))
    (declare (type SFNull sfnull))
    (the null
      (sfnull-value sfnull)))
  
  (:method ((sfstring SFString))
    (declare (type SFString sfstring))
    (the string
      (sfstring-value sfstring))))

;;; -------------------------------------------------------

(defgeneric get-sfobject-as-boolean (sfobject)
  (:documentation
    "Returns a ``boolean'' equivalent to the SFOBJECT, or signals an
     error of an unspecified type upon its want of eligiblity.")
  
  (:method ((sfboolean SFBoolean))
    (declare (type SFBoolean sfboolean))
    (the boolean
      (get-sfobject-value sfboolean)))
  
  (:method ((sfobject SFObject))
    (declare (type SFObject sfobject))
    (error "The object ~a cannot be resolved to a Boolean truth value."
      sfobject)))

;;; -------------------------------------------------------

(defgeneric print-sfobject (sfobject destination)
  (:documentation
    "Prints a covenable representation of the SFOBJECT to the
     DESTINATION and returns no value.")
  
  (:method ((sfboolean SFBoolean) (destination T))
    (declare (type SFBoolean   sfboolean))
    (declare (type destination destination))
    (format destination "~:[false~;true~]"
      (sfboolean-value sfboolean))
    (values))
  
  (:method ((sfnull SFNull) (destination T))
    (declare (type SFNull      sfnull))
    (declare (ignore           sfnull))
    (declare (type destination destination))
    (format destination "<null>")
    (values))
  
  (:method ((sfobject SFObject) (destination T))
    (declare (type SFObject    sfobject))
    (declare (type destination destination))
    (format destination "~a"
      (get-sfobject-value sfobject))
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of SFObject operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-object right-object)
  (:documentation
    "Applies the binary OPERATOR to the the LEFT-OBJECT and the
     RIGHT-OBJECT in this order and returns a result connable for this
     combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operator
    (operator (left-object-class right-object-class result-class)
     &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operator'', the first formal parameter of which is
   agnominated as ``$operator'' and dispatches via an
   ``eql''-specialization on the evaluated OPERATOR instance, the second
   parameter assumes the stevening ``$left'' with a concomitant
   specialization on the class LEFT-OBJECT-CLASS, while the third and
   desinent input, norned ``$right'', is desumed from the
   RIGHT-OBJECT-CLASS, the implementation being provided by the BODY
   forms, ensconced in a ``progn'' context inwith a ``the'' special
   operator that conveys the RESULT-CLASS as the method output,
   ultimately returning the final BODY form's results.
   ---
   The first BODY form, if establishing a string object, is construed as
   a documentation string to the method implementation and appropriated
   for this encheson."
  `(defmethod apply-binary-operator (($operator (eql ,operator))
                                     ($left     ,left-object-class)
                                     ($right    ,right-object-class))
     ,(or (and (stringp (first body)) (pop body))
          "")
     (declare (type keyword             $operator))
     (declare (ignorable                $operator))
     (declare (type ,left-object-class  $left))
     (declare (ignorable                $left))
     (declare (type ,right-object-class $right))
     (declare (ignorable                $right))
     (the ,result-class
       (progn ,@body))))

;;; -------------------------------------------------------

(define-binary-operator :append (SFBoolean SFObject SFString)
  (make-sfstring
    (format NIL "~:[true~;false~]~a"
      (get-sfobject-value $left)
      (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :append (SFInteger SFInteger SFInteger)
  (make-sfinteger
    (+ (* (get-sfobject-value $left) 10)
       (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :append (SFInteger SFCharacter SFObject)
  (if (digit-char-p (get-sfobject-value $right))
    (apply-binary-operator :append $left
      (make-sfinteger
        (digit-char-p
          (get-sfobject-value $right))))
    (make-sfstring
      (format NIL "~a~a"
        (get-sfobject-value $left)
        (get-sfobject-value $right)))))

;;; -------------------------------------------------------

(define-binary-operator :append (SFInteger SFObject SFString)
  (make-sfstring
    (format NIL "~a~a"
      (get-sfobject-value $left)
      (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :append (SFNull SFCharacter SFObject)
  (if (digit-char-p (get-sfobject-value $right))
    (make-sfinteger
      (digit-char-p
        (get-sfobject-value $right)))
    $right))

;;; -------------------------------------------------------

(define-binary-operator :append (SFNull SFObject SFObject)
  $right)

;;; -------------------------------------------------------

(define-binary-operator :append (SFObject SFBoolean SFString)
  (make-sfstring
    (format NIL "~a~:[true~;false~]"
      (get-sfobject-value $left)
      (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :append (SFObject SFNull SFObject)
  $left)

;;; -------------------------------------------------------

(define-binary-operator :append (SFObject SFObject SFString)
  (make-sfstring
    (format NIL "~a~a"
      (get-sfobject-value $left)
      (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :add (SFInteger SFInteger SFInteger)
  (make-sfinteger
    (+ (get-sfobject-value $left)
       (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :add (SFNull SFInteger SFInteger)
  $right)

;;; -------------------------------------------------------

(define-binary-operator :subtract (SFInteger SFInteger SFInteger)
  (make-sfinteger
    (- (get-sfobject-value $left)
       (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :subtract (SFNull SFInteger SFInteger)
  (make-sfinteger
    (- (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :multiply (SFInteger SFInteger SFInteger)
  (make-sfinteger
    (* (get-sfobject-value $left)
       (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :multiply (SFNull SFInteger SFInteger)
  (make-sfinteger 0))

;;; -------------------------------------------------------

(define-binary-operator :divide (SFInteger SFInteger SFInteger)
  (make-sfinteger
    (floor (get-sfobject-value $left)
           (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :divide (SFNull SFInteger SFInteger)
  (make-sfinteger 0))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFBoolean SFBoolean SFBoolean)
  (make-sfboolean
    (eq (get-sfobject-value $left)
        (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFCharacter SFCharacter SFBoolean)
  (make-sfboolean
    (char= (get-sfobject-value $left)
           (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFCharacter SFString SFBoolean)
  (make-sfboolean
    (char= (get-sfobject-value $left)
           (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFInteger SFInteger SFBoolean)
  (make-sfboolean
    (= (get-sfobject-value $left)
       (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFString SFString SFBoolean)
  (make-sfboolean
    (string= (get-sfobject-value $left)
             (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFString SFCharacter SFBoolean)
  (make-sfboolean
    (string= (get-sfobject-value $left)
             (get-sfobject-value $right))))

;;; -------------------------------------------------------

(define-binary-operator :equals (SFObject SFObject SFBoolean)
  (make-sfboolean NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Stack-Error (simple-error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the signaling of
     an anomalous situation begotten by the trial to indagate or pop
     from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((grid
    :initarg       :grid
    :initform      (error "Missing code grid for the interpreter.")
    :reader        get-grid
    :type          Grid
    :documentation "The program as a two-dimensional grid of
                    characters.")
   (instruction-pointer
    :initform      (make-pointer)
    :accessor      instruction-pointer
    :type          Pointer
    :documentation "The instruction pointer (IP).")
   (stack
    :initform      NIL
    :accessor      program-stack
    :type          program-stack
    :documentation "The program stack.")
   (variable
    :initform      (make-sfnull)
    :accessor      program-variable
    :type          SFObject
    :documentation "The program variable.")
   (register
    :initform      (make-sfnull)
    :accessor      program-register
    :type          SFObject
    :documentation "The program register.")
   (program-halted-p
    :initform      NIL
    :accessor      program-halted-p
    :type          boolean
    :documentation "Determines whether the program has been
                    terminated."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of accompassing
     actual competence to a Swordfish code grid."))

;;; -------------------------------------------------------

(defun make-interpreter (grid)
  "Creates and returns a new ``Interpreter'' nuncupated to the Swordfish
   code GRID's evaluation."
  (declare (type Grid grid))
  (the Interpreter
    (make-instance 'Interpreter :grid grid)))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the next
   cells in its GRID and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (grid instruction-pointer) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer instruction-pointer))
    (advance-pointer instruction-pointer)
    (adjust-location-to-grid grid
      (pointer-location instruction-pointer)))
  (values))

;;; -------------------------------------------------------

(defun get-current-token (interpreter)
  "Returns the grid symbol located at the INTERPRETER's instruction
   pointer (IP) position in its code grid."
  (declare (type Interpreter interpreter))
  (the character
    (get-grid-cell
      (get-grid interpreter)
      (pointer-location
        (instruction-pointer interpreter)))))

;;; -------------------------------------------------------

(defun push-unto-stack (interpreter object)
  "Pushes the OBJECT unto the INTERPRETER's stack and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type SFObject    object))
  (push object
    (program-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (interpreter)
  "Pops the top element from the INTERPRETER's stack and returns no
   value, or, upon the stack's vacancy, signals an error of the type
   ``Empty-Stack-Error''."
  (declare (type Interpreter interpreter))
  (the SFObject
    (or (pop (program-stack interpreter))
        (error 'Empty-Stack-Error
          :format-control "Cannot pop from an empty stack."))))

;;; -------------------------------------------------------

(defun add-to-variable (interpreter object)
  "Adds or appends the OBJECT to the INTERPRETER's variable and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type SFCharacter object))
  (setf (program-variable interpreter)
    (apply-binary-operator :append
      (program-variable interpreter)
      object))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor
    (instruction (interpreter-variable)
     &body body)
  "Defines an implementation of the generic function
   ``process-instruction'', the first argument of which derives its
   agnomination from the INTERPRETER-VARIABLE, the second being nevend
   automatically, and dispatches on an ``eql''-equality to the
   INSTRUCTION character, the method's body is injected with the BODY
   forms, and returns no values.
   ---
   If the first BODY form represents a string, the same is construed as
   the generated ``defmethod'''s documentation string and reappropriated
   for this purpose."
  (let ((instruction-variable (gensym)))
    (declare (type symbol instruction-variable))
    `(defmethod process-instruction
         ((,interpreter-variable Interpreter)
          (,instruction-variable (eql ,instruction)))
       ,(or (and (stringp (first body)) (pop body))
            "")
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type character   ,instruction-variable))
       (declare (ignore           ,instruction-variable))
       ,@body
       (values))))


;;; === Implementation of navigational instructions ================ ;;;

(define-instruction-processor #\> (interpreter)
  "Changes the INTERPRETER's instruction pointer (IP) direction to a
   dextral movement and returns no value."
  (setf (pointer-direction (instruction-pointer interpreter))
        :right))

;;; -------------------------------------------------------

(define-instruction-processor #\< (interpreter)
  "Changes the INTERPRETER's instruction pointer (IP) direction to a
   sinistral movement and returns no value."
  (setf (pointer-direction (instruction-pointer interpreter))
        :left))

;;; -------------------------------------------------------

(define-instruction-processor #\^ (interpreter)
  "Changes the INTERPRETER's instruction pointer (IP) direction to an
   upward movement and returns no value."
  (setf (pointer-direction (instruction-pointer interpreter))
        :up))

;;; -------------------------------------------------------

(define-instruction-processor #\v (interpreter)
  "Changes the INTERPRETER's instruction pointer (IP) direction to an
   upward movement and returns no value."
  (setf (pointer-direction (instruction-pointer interpreter))
        :down))

;;; -------------------------------------------------------

(define-instruction-processor #\| (interpreter)
  "Reflects the INTERPRETER's instruction pointer (IP) on a horizontally
   redirecting mirror and returns no value."
  (mirror-pointer
    (instruction-pointer interpreter)
    :horizontal))

;;; -------------------------------------------------------

(define-instruction-processor #\_ (interpreter)
  "Reflects the INTERPRETER's instruction pointer (IP) on a vertically
   redirecting mirror and returns no value."
  (mirror-pointer
    (instruction-pointer interpreter)
    :vertical))

;;; -------------------------------------------------------

(define-instruction-processor #\/ (interpreter)
  "Reflects the INTERPRETER's instruction pointer (IP) on a mirror
   slanted in a clockwise direction and returns no value."
  (mirror-pointer
    (instruction-pointer interpreter)
    :slanted-deasil))

;;; -------------------------------------------------------

(define-instruction-processor #\\ (interpreter)
  "Reflects the INTERPRETER's instruction pointer (IP) on a mirror
   slanted in a counterclockwise direction and returns no value."
  (mirror-pointer
    (instruction-pointer interpreter)
    :slanted-withershins))


;;; === Implementation of control flow instructions ================ ;;;

(define-instruction-processor #\! (interpreter)
  "Omits the next character and returns no value."
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-instruction-processor #\{ (interpreter)
  "Tops the top element from the INTERPRETER stack; if the same
   constitutes a Boolean \"false\" value, omits the subsequent
   instruction, otherwise proceeds as usual, in any case returning no
   value."
  (unless (get-sfobject-as-boolean (pop-from-stack interpreter))
    (advance-program interpreter)))

;;; -------------------------------------------------------

(define-instruction-processor #\} (interpreter)
  "Tops the top element from the INTERPRETER stack; if the same
   constitutes a Boolean \"false\" value, omits the subsequent
   instruction, otherwise proceeds as usual, in any case returning no
   value."
  (when (get-sfobject-as-boolean (pop-from-stack interpreter))
    (advance-program interpreter)))

;;; -------------------------------------------------------

(define-instruction-processor #\; (interpreter)
  "Halts the program stored in the INTERPRETER and returns no value."
  (setf (program-halted-p interpreter) T))


;;; === Implementation of arithmetic instructions ================== ;;;

(define-instruction-processor #\+ (interpreter)
  "Pops the top element from the INTERPRETER's stack, adds it to the
   variable, and returns no value."
  (setf (program-variable interpreter)
    (apply-binary-operator :add
      (program-variable interpreter)
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

(define-instruction-processor #\- (interpreter)
  "Pops the top element from the INTERPRETER's stack, subtracts it from
   the variable, and returns no value."
  (setf (program-variable interpreter)
    (apply-binary-operator :subtract
      (program-variable interpreter)
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

(define-instruction-processor #\* (interpreter)
  "Pops the top element from the INTERPRETER's stack, multiplies the
   variable by the same, and returns no value."
  (setf (program-variable interpreter)
    (apply-binary-operator :multiply
      (program-variable interpreter)
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

(define-instruction-processor #\D (interpreter)
  "Pops the top element from the INTERPRETER's stack, divides the same
   by the variable's value, stores the quotient in the variable, and
   returns no value."
  (setf (program-variable interpreter)
    (apply-binary-operator :subtract
      (pop-from-stack interpreter)
      (program-variable interpreter))))

;;; -------------------------------------------------------

(define-instruction-processor #\d (interpreter)
  "Pops the top element from the INTERPRETER's stack, divides the
   variable by the same, stores the quotient in the variable, and
   returns no value."
  (setf (program-variable interpreter)
    (apply-binary-operator :divide
      (program-variable interpreter)
      (pop-from-stack interpreter))))


;;; === Implementation of logical instructions ===================== ;;;

(define-instruction-processor #\= (interpreter)
  "Pops the top element from the INTERPRETER's stack, juxtaposes it with
   the variable's value, upon parity pushing a Boolean \"true\" value
   unto the stack, otherwise a \"false\", in any case returning no
   value."
  (push-unto-stack interpreter
    (apply-binary-operator :equals
      (pop-from-stack   interpreter)
      (program-variable interpreter))))


;;; === Implementation of input/output instructions ================ ;;;

(define-instruction-processor #\: (interpreter)
  "Construes the subsequent token as a literal character, adds the same
   to the INTERPRETER's variable, and returns no value."
  (advance-program interpreter)
  (add-to-variable interpreter
    (make-sfcharacter
      (get-current-token interpreter))))

;;; -------------------------------------------------------

(define-instruction-processor #\# (interpreter)
  "Prints the INTERPRETER variable's content to the standard output and
   returns no value."
  (with-open-stream (output-conduit *standard-output*)
    (declare (type destination output-conduit))
    (print-sfobject (program-variable interpreter) output-conduit)
    (format output-conduit " ")
    (finish-output output-conduit)))

;;; -------------------------------------------------------

(define-instruction-processor #\? (interpreter)
  "Queries the standard input for an integer number, stores it in the
   INTERPRETER's variable, and returns no value."
  (format T "~&>> ")
  (finish-output)
  (setf (program-variable interpreter)
    (make-sfinteger
      (parse-integer
        (read-line NIL NIL "0"))))
  (clear-input)
  (values))


;;; === Implementation of memory management ======================== ;;;

(define-instruction-processor #\[ (interpreter)
  "Pops the top element from the INTERPRETER's stack, stores the same in
   the registry, and returns no value."
  (setf (program-register interpreter)
    (pop-from-stack interpreter)))

;;; -------------------------------------------------------

(define-instruction-processor #\] (interpreter)
  "Pushes the INTERPRETER register's value unto the stack without
   modifying the register and returns no value."
  (push-unto-stack interpreter
    (program-register interpreter)))

;;; -------------------------------------------------------

(define-instruction-processor #\% (interpreter)
  "Clears the INTERPRETER's variable and returns no value."
  (setf (program-variable interpreter)
    (make-sfnull)))

;;; -------------------------------------------------------

(define-instruction-processor #\~ (interpreter)
  "Pushes the INTERPRETER variable's value unto the stack, clears the
   variable, and returns no value."
  (push-unto-stack interpreter
    (program-variable interpreter))
  (setf (program-variable interpreter)
    (make-sfnull)))

;;; -------------------------------------------------------

(define-instruction-processor #\@ (interpreter)
  "Pops the top element from the INTERPRETER's stack, stores the same in
   its variable, and returns no value."
  (setf (program-variable interpreter)
    (pop-from-stack interpreter)))


;;; === Implementation of meta instructions ======================== ;;;

(define-instruction-processor #\, (interpreter)
  "Ignores the INTERPRETER and the instruction, establishing a
   no-operation (NOP) and returns no value.")

;;; -------------------------------------------------------

(define-instruction-processor #\$ (interpreter)
  "Sleeps for a tally of milliseconds tantamount to the INTERPRETER
   variable's numeric value and returns no value."
  (sleep
    (round (get-sfobject-value (program-variable interpreter))
           1000)))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction character))
  "Adds the INTERPRETER's current character, which is identical to the
   INSTRUCTION token, to its variable and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type character   instruction))
  (declare (ignore           instruction))
  (add-to-variable interpreter
    (make-sfcharacter
      (get-current-token interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Executes the program maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop until (program-halted-p interpreter) do
    (process-instruction interpreter
      (get-current-token interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Swordfish (code)
  "Interprets the piece of Swordfish source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (build-grid code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenates a list of zero or more LINES into a single string, each
   twissel among the input being connected by a newline character as the
   sepiment.
   ---
   This operation furnishes a commodity for the eath generation of a
   string covenable for its subsequent interpretation as a Swordfish
   program grid by eliminating the complexity of leading spaces for an
   aesthetically appealing ordonnance among the symbols to align."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~a~^~%~}" lines)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Swordfish
  (concatenate-lines
    "Hello:, \\"
    "/#;,,,,,W"
    "\\!:d:lro/"))

;;; -------------------------------------------------------

;; Print "Hello, World!" following a more conventional avenue.
(interpret-Swordfish "Hello:, Worl:d:!#;")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program.
(interpret-Swordfish
  (concatenate-lines
    ">?#v"
    "^,,<"))

;;; -------------------------------------------------------

;; Generate and print the infinite series of triangular numbers, that
;; is:
;;   1, 3 (=1+2), 6 (=1+2+3), 10 (= 1+2+3+4), etc.
(interpret-Swordfish
  (concatenate-lines
    "1~[0,,,,,v"
    "]+#~500$%\\"
    "1]+~[@,,,\\"))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Swordfish
  (concatenate-lines
    "?~1={v%0#;"
    ",,,,,%,,,,"
    ",,,,,1,,,,"
    ",,,,>#,,,,"
    ",,,,^<,,,,"))
