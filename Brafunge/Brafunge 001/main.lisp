;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brafunge," invented by the Esolang user "C0ffee" and
;; presented on May 20th, 2025, its conception a coalescence of Urban
;; Mueller's "brainfuck" and Chris Pressey's "Befunge", the former
;; parentage's dation ostends itself in the memory's designment as a
;; bilaterally infinite tape of integral values, while the coefficient
;; entheus' second moeity in its patefaction limns the two-dimensional
;; Cartesian code grid conformation as its paravaunt vouchsafement.
;; 
;; 
;; Concept
;; =======
;; The Brafunge programming language constitutes a two-dimensional
;; entity, its epiphenoma's object a tape of signed integer numbers.
;; 
;; == BRAFUNGE: [BRA]INFUCK + BE[FUNGE] ==
;; The Brafunge programming language enstates a champarty in its
;; engendered entirety, ensuing from the languages brainfuck and
;; Befunge, the coalized cleronomy of these two producing in the
;; former's case the infinite dispansion of cells, signed integer-valued
;; in this specific specimen, as opposed to the unsigned byte capacity
;; that harbors its commorancy in the parentage; while Befunge's
;; paravaunt contribution is patefied in the two-dimensional grid of
;; characters forming its source code.
;; 
;; == THE PROGRAM: A TWO-DIMENSIONAL GRID ==
;; An accolyte of Befunge's syntactical conformation, Brafunge's
;; lealty pursues the propagated beau ideal: The programs are molded
;; into a two-dimensional Cartesian reticulation of characters, norned
;; the "playfield" in concord with Befunge's parlance.
;; 
;; == THE INSTRUCTION POINTER: A STRAVAIGER ACROSS THE PLAYFIELD ==
;; Operating on the twyfold dimense, the commorant symbols' traversal
;; limns a wike assigned to the instruction pointer (IP), at its
;; gendrure's instant empight on the left upper cell, and oriented in a
;; sinistrodextral direction.
;; 
;; == CONTROL FLOW WRAPS AROUND ALONG THE PLAYFIELD'S MARGENTS ==
;; A dioristic deportment's patefaction may be retrieved from the
;; playfield's response to its bournes' violations by the instruction
;; pointer (IP), in which case the motation wraps around to the obverse
;; laterality.
;; 
;; == THE MEMORY: A BILATERALLY INFINITE VECTOR OF INTEGERS ==
;; The program's data castaldy constitutes a bilaterally infinite tape's
;; dever, its cells' amenability composed in such a manner as to
;; actuate signed integer indices' impositions.
;; 
;; Each cell's capacity metes a signed integer number, the mickleness
;; and polarity vouchsafed to whom does not wist of any restrictions,
;; its inchoate state's acquisition that of the value zero (0).
;; 
;; == A CELL POINTER SELECTS THE CURRENTLY ACTIVE MEMORY CELL ==
;; At any instant during the execution, a dedicated cursor, the
;; "cell pointer", partakes in the designation of the currently active
;; cell, this entity constituting the sole member contemporaneously
;; entalented with an amenability to indagations and modulations.
;; 
;; The pointer's gressible proprium instills in thilk the contingency
;; for its traversal athwart the tape, both in a stillatim mode, and a
;; more liberal random-access alternative.
;; 
;; 
;; Instructions
;; ============
;; Brafunge's instruction set enumerates a cardinality of 21 members,
;; their bailiwicks pertinent in such matters as the memory tape's
;; castaldy, basic arithmetics, input and output commerce, navigational
;; competences, as well as control flow duction.
;; 
;; Any symbol to whom no alligation with a causatum is apportioned,
;; bears a mete of ineffectuous participation equipollent to its
;; tolerance and  neglect.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall manifest in a requisite mete of
;; gnarity's adhibition anent the language's operative competences.
;; 
;; Please heed that amplection of succedaneous tmemata in a jumelle of
;; braces, "{" and "}", their so enhalsed content intended for its
;; supersession by actual Brafunge code in the program's ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ==================================================================
;;   MEMORY MANAGEMENT
;;   ------------------------------------------------------------------
;;   >        | Translates the tape's cell pointer one step to the
;;            | right.
;;   ..................................................................
;;   <        | Translates the tape's cell pointer one step to the
;;            | left.
;;   ..................................................................
;;   @        | Relocates the tape's cell pointer to the cell whose
;;            | index concurs with the tape's current cell value.
;;   ..................................................................
;;   +        | Increments the tape's current cell value by one (1).
;;   ..................................................................
;;   -        | Decrements the tape's current cell value by one (1).
;;   ..................................................................
;;   {digits} | Sets the tape's current cell value to the unsigned
;;            | decimal integer number compact of the {digits}.
;;            |--------------------------------------------------------
;;            | {digits} must be a catena comprised of one or more
;;            | decimal digits.
;;   ..................................................................
;;   '{char}  | Stores the ASCII code of the character {char} in the
;;            | tape's current cell.
;;            |--------------------------------------------------------
;;            | {char} must be a character.
;;   ==================================================================
;;   INPUT AND OUTPUT INTERCOURSE
;;   ------------------------------------------------------------------
;;   .        | Prints the character whose ASCII code concurs with the
;;            | tape's current cell value to the standard output
;;            | conduit.
;;   ..................................................................
;;   !        | Prints the tape's current cell value in its verbatim
;;            | numeric form to the standard output conduit.
;;   ..................................................................
;;   ,        | Queries the standard input conduit for a character and
;;            | stores its ASCII code in the tape's current cell.
;;   ..................................................................
;;   &        | Queries the standard input for a signed or unsigned
;;            | integer number and stores the same in the tape's
;;            | current cell.
;;   ==================================================================
;;   NAVIGATIONAL OPERATIONS
;;   ------------------------------------------------------------------
;;   )        | Redirects the instruction pointer's (IP) orientation
;;            | rightwards.
;;   ..................................................................
;;   (        | Redirects the instruction pointer's (IP) orientation
;;            | leftwards.
;;   ..................................................................
;;   ^        | Redirects the instruction pointer's (IP) orientation
;;            | upwards.
;;   ..................................................................
;;   v        | Redirects the instruction pointer's (IP) orientation
;;            | downwards.
;;   ..................................................................
;;   ?        | Sets the instruction pointer's (IP) orientation to an
;;            | aleatorily chosen direction.
;;   ..................................................................
;;   /        | Reflects the instruction pointer's (IP) orientation on
;;            | an "ascending" mirror.
;;            |--------------------------------------------------------
;;            | The following vincula is imposed by the mirror's
;;            | nomothesia:
;;            |   -------------------------------------
;;            |   Current direction | New direction
;;            |   ------------------+------------------
;;            |   left              | down
;;            |   .....................................
;;            |   right             | up
;;            |   .....................................
;;            |   up                | right
;;            |   .....................................
;;            |   down              | left
;;            |   -------------------------------------
;;   ..................................................................
;;   \        | Reflects the instruction pointer's (IP) orientation on
;;            | a "descending" mirror.
;;            |--------------------------------------------------------
;;            | The following vincula is imposed by the mirror's
;;            | nomothesia:
;;            |   -------------------------------------
;;            |   Current direction | New direction
;;            |   ------------------+------------------
;;            |   left              | up
;;            |   .....................................
;;            |   right             | down
;;            |   .....................................
;;            |   up                | left
;;            |   .....................................
;;            |   down              | right
;;            |   -------------------------------------
;;   ..................................................................
;;   _        | Horizontal IF: If the tape's current cell value equals
;;            | zero (0), redirects the instruction pointer's (IP)
;;            | orientation rightwards, otherwise leftwards.
;;   ..................................................................
;;   |        | Vertical IF: If the tape's current cell value equals
;;            | zero (0), redirects the instruction pointer's (IP)
;;            | orientation downwards, otherwise upwards.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   #        | Bridge: Skips the next playfield symbol.
;;   ..................................................................
;;   *        | Immediately terminates the program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter represents an effort performed in the programming
;; language Common Lisp, the entirety's manifestation a dimidiation
;; applied through a parasceuastic stage inwith whose circumference the
;; linear Brafunge code transmogrifies its plasmature into a veridicous
;; two-dimensional designment; ere, in the subsequent moeity, the
;; program execution ensues on this reticulate commorancy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-06-22
;; 
;; Sources:
;;   [esolang:2025:Brafunge]
;;   The Esolang contributors, "Brafunge", July 10th, 2025
;;   URL: "https://esolangs.org/wiki/Brafunge"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicate-based-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type, the agnomination of whom constitutes the
   TYPE-NAME's vouchsafement, and the formal parameters are desumed in
   an ipsissima verba fashion from the LAMBDA-LIST, the object to probe
   assuming the CANDIDATE-NAME; evaluates the BODY forms, and construes
   the desinent form's primary result as the docimasy's perclose, with
   a \"generalized boolean\" value of \"true\" serving to enstate the
   compatibility's ascertainment, whereas a \"false\" response is
   adhibited the agency of a failure's patefaction.
   ---
   If the first BODY form resolve to a string, the same is expected to
   contribute the type definition's documentation string; as a
   consectary being reappropriated for this exact purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
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
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicate-based-type hash-table-of
    (candidate &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency is
   founded upon a membership of zero or more entries, each such a
   twissel comprehending a key of the KEY-TYPE and an associated value
   complying with the VALUE-TYPE, both being governed by the generic
   sentinel ``*'' as the default configuration."
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
          (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-a-predicate-based-type list-of
    (candidate &optional (element-type '*))
  "The ``list-of'' type defines a linked list compact of zero or more
   elements, each member parataking in whom subscribes to the
   ELEMENT-TYPE, for thilk holds the generic sentinel ``*'' as the
   default configuration."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype character-matrix ()
  "The ``character-matrix'' type defines a sparse two-dimensional array
   of characters, its manifestation edified upon a hash table whose keys
   assume ``Point'' objects and correspond to the respective character
   entities."
  '(hash-table-of Point character))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized airts along which
   a Brafunge program's control flow is homologated in its
   peragrations."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines the program memory as a bilaterally
   infinite dispansion of signed integer-valued cells, their polarity
   as well as mickleness wisting of no natural bournes' tholance, and
   whose plasmature is realized in a hash table, the signed integer
   keys of which specify the cell indices, while the values, desumed
   from the selfsame vale, furnish the cell states."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type character     +NULL-CHARACTER+))
(declaim (type standard-char +SPACE-CHARACTER+))

;;; -------------------------------------------------------

(defparameter +NULL-CHARACTER+
  (code-char 0)
  "Represents the \"null character\", allied with the ASCII code 0, in
   a portable manner.")

(defparameter +SPACE-CHARACTER+
  (code-char 32)
  "Represents the space character, allied with the ASCII code 32, in a
   portable manner.")

;;; -------------------------------------------------------

(defun linebreak-character-p (candidate)
  "Determines whether the CANDIDATE represents a linebreak character,
   the diorism of whose assignment is delineated by the newline, or
   line feed, the vertical tabulation, the form feed, as well as the
   carriage return entities, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (<= 10 (char-code candidate) 13))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents an empty string, its length
   measuring exactly zero character, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (zerop
      (length source))))

;;; -------------------------------------------------------

(defun convert-into-a-simple-string (source)
  "Converts the SOURCE into a simple string, either returning a fresh
   instance, upon the SOURCE's incompatibility with the desiderated
   type, or, if already subsumed into this species, delivering the
   SOURCE in its verbatim form.
   ---
   The SOURCE string will not be subjected to any modulations."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun concatenate-the-lines (&rest lines)
  "Concatenates the LINES into a single string, each attiguous twissel's
   interstice being contexed via one newline character, and returns a
   fresh simple string comprehending the result."
  (declare (type (list-of string) lines))
  (the simple-string
    (convert-into-a-simple-string
      (format NIL "~{~a~^~%~}" lines))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Point".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Point
  (:constructor make-a-point-at (&optional (x 0) (y 0))))
  "The ``Point'' class enstates a two-dimensional Cartesian coordinates
   jumelle whose diorism avaunts from a horizontal x- and a vertical
   y-offset from a conceived origin (0, 0), both moeities desuming their
   resolution from the dimense of signed integer numbers."
  (x 0 :type fixnum :read-only NIL)
  (y 0 :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defun move-the-point-to (point new-x new-y)
  "Relocates the POINT to the coordinates compact of (NEW-X, NEW-Y) and
   returns no value."
  (declare (type Point point))
  (psetf
    (point-x point) new-x
    (point-y point) new-y)
  (values))

;;; -------------------------------------------------------

(defun copy-the-point-coordinates-from (destination source)
  "Copies the SOURCE's coordinates into the DESTINATION point and
   returns the modified DESTINATION."
  (declare (type Point destination))
  (declare (type Point source))
  (psetf
    (point-x destination) (point-x source)
    (point-y destination) (point-y source))
  (the Point destination))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Playfield".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Playfield ()
  ((width
    :initform      0
    :accessor      playfield-width
    :type          fixnum
    :documentation "The playfield's horizontal dimense.")
   (height
    :initform      0
    :accessor      playfield-height
    :type          fixnum
    :documentation "The playfield's vertical dimense.")
   (cells
    :initform      (make-hash-table :test #'equalp)
    :accessor      playfield-cells
    :type          character-matrix
    :documentation "A sparse matrix of characters, mapping the
                    two-dimensional ``Point'' coordinates to character
                    entities."))
  (:documentation
    "The ``Playfield'' class is apportioned the dever of a Brafunge
     code grid's modeling, the plasmature being also referred to as a
     \"playfield\", establishing a two-dimensional Cartesian
     reticulation of characters, along whose margents the control flow
     is wont to wrap around towards the obverse bourn."))

;;; -------------------------------------------------------

(defun prepare-a-vacant-playfield ()
  "Creates and returns a fresh and initially empty and zero-dimensioned
   ``Playfield''."
  (the Playfield
    (make-instance 'Playfield)))

;;; -------------------------------------------------------

(defun query-the-playfield-symbol-at (playfield point)
  "Returns the character located at the POINT in the PLAYFIELD."
  (declare (type Playfield playfield))
  (declare (type Point     point))
  (the character
    (gethash point
      (playfield-cells playfield)
      #\Space)))

;;; -------------------------------------------------------

(defun print-the-playfield-symbol-at (playfield point destination)
  "Writes the PLAYFIELD cell's content empight at the POINT to the
   DESTINATION and returns no value."
  (declare (type Playfield playfield))
  (declare (type Point     point))
  (declare (type stream    destination))
  (write-char
    (query-the-playfield-symbol-at playfield point)
    destination)
  (values))

;;; -------------------------------------------------------

(defun load-the-program-into-the-playfield (playfield source)
  "Stores the piece of Brafunge SOURCE code in the PLAYFIELD and returns
   the modified PLAYFIELD.
   ---
   The PLAYFIELD will be destructively modified."
  (declare (type Playfield     playfield))
  (declare (type simple-string source))
  (with-slots (width height cells) playfield
    (declare (type fixnum           width))
    (declare (type fixnum           height))
    (declare (type character-matrix cells))
    (clrhash cells)
    (if (string-is-empty-p source)
      (psetf
        width  0
        height 0)
      (loop
        initially
          (psetf
            width  1
            height 1)
        
        with x of-type fixnum = 0
        and  y of-type fixnum = 0
        
        for current-token of-type character across source
        
        if (linebreak-character-p current-token) do
          (setf x      0)
          (incf y      1)
          (incf height 1)
        else do
          (setf (gethash (make-a-point-at x y) cells) current-token)
          (incf x 1)
          (setf width (max width x))
        end)))
  (the Playfield playfield))

;;; -------------------------------------------------------

(defmethod print-object ((field Playfield) (stream T))
  (with-slots (width height cells) field
    (declare (type fixnum           width))
    (declare (type fixnum           height))
    (declare (type character-matrix cells))
    (format stream
      "~&Playfield composed of ~d columns and ~d rows:"
      width height)
    (let ((current-point (make-a-point-at 0 0)))
      (declare (type Point current-point))
      (dotimes (y height)
        (declare (type fixnum y))
        (terpri stream)
        (dotimes (x width)
          (declare (type fixnum x))
          (move-the-point-to current-point x y)
          (print-the-playfield-symbol-at field current-point stream)))))
  (the Playfield field))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the interpreter state variables and constants. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array direction (4)) +DIRECTIONS+))

(declaim (type Playfield                    *playfield*))
(declaim (type Point                        *current-position*))
(declaim (type direction                    *current-direction*))
(declaim (type character                    *current-token*))
(declaim (type boolean                      *program-has-halted-p*))
(declaim (type tape                         *tape*))
(declaim (type integer                      *cell-pointer*))
(declaim (type integer                      *current-cell-value*))

;;; -------------------------------------------------------

(defconstant +DIRECTIONS+
  (make-array 4
    :element-type     'direction
    :initial-contents '(:left :right :up :down)
    :adjustable       NIL
    :fill-pointer     NIL)
  "Enlists the recognized control flow directions in a vector.")

(defparameter *playfield*
  (prepare-a-vacant-playfield)
  "The Brafunge program to execute reformulated in a veridicous
   two-dimensional plasmature.")

(defparameter *current-position*
  (make-a-point-at 0 0)
  "The instruction pointer's (IP) contemporaneous position.")

(defparameter *current-direction* :right
  "The instruction pointer's (IP) contemporaneous peragration airt.")

(define-symbol-macro *current-token*
  (the character
    (query-the-playfield-symbol-at *playfield* *current-position*)))

(defparameter *tape*
  (make-hash-table :test #'eql)
  "The program memory as a sparse vector which assigns signed integer
   subscripts to its signed integer elements.")

(defparameter *cell-pointer* 0
  "The current cell pointer position into the memory ``*TAPE*'' as the
   selected cell's integral subscript.")

(define-symbol-macro *current-cell-value*
  (the integer
    (gethash *cell-pointer* *tape* 0)))

(defparameter *program-has-halted-p* NIL
  "A Boolean flag which determines whether the current Brafunge program
   has been terminated explicitly via an \"*\" operation's invocation.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory handling operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-cell-value ()
  "Returns the ``*TAPE*'''s current cell value. "
  (the integer
    (gethash *cell-pointer* *tape* 0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value)
  "Stores the NEW-VALUE in the ``*TAPE*'''s current cell and returns no
   value."
  (declare (type integer new-value))
  (setf (gethash *cell-pointer* *tape* 0) new-value)
  (values))

;;; -------------------------------------------------------

(defun reset-the-memory ()
  "Restores the memory to its state of inchoacy and returns no value."
  (clrhash *tape*)
  (setf    *cell-pointer* 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter parasceve operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-the-new-program (new-source)
  "Replaces the ``*PLAYFIELD*'' by the program obtained from the
   Brafunge's NEW-SOURCE code and returns no value."
  (declare (type simple-string new-source))
  (setf *playfield*
    (load-the-program-into-the-playfield *playfield* new-source))
  (values))

;;; -------------------------------------------------------

(defun reset-the-instruction-pointer ()
  "Restores the instruction pointer's (IP) ``*CURRENT-POSITION*'' and
   ``*CURRENT-DIRECTION*'' components' configurations to their state of
   inchoacy and returns no value."
  (psetf
    (point-x *current-position*) 0
    (point-y *current-position*) 0
    *current-direction*          :right)
  (values))

;;; -------------------------------------------------------

(defun graith-the-interpreter (new-source)
  "Graiths the interpreter by restoring its pristine status and
   concomitantly adjusting the same to the Brafunge code's NEW-SOURCE,
   and returns no value."
  (declare (type string new-source))
  (load-the-new-program
    (convert-into-a-simple-string new-source))
  (reset-the-instruction-pointer)
  (setf *program-has-halted-p* NIL)
  (reset-the-memory)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the navigational operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun correct-the-instruction-pointer-position ()
  "Upon necessity, adjusts the instruction pointer (IP) location in such
   a manner as to vouch for its validity in the ``*PLAYFIELD*'', in any
   case returning no value."
  (psetf
    (point-x *current-position*)
      (mod
        (point-x         *current-position*)
        (playfield-width *playfield*))
    (point-y *current-position*)
      (mod
        (point-y          *current-position*)
        (playfield-height *playfield*)))
  (values))

;;; -------------------------------------------------------

(defun vaunce-the-instruction-pointer ()
  "Advances the instruction pointer (IP) to the next character in the
   ``*PLAYFIELD*'', contingently wrapping around to the opposite
   margent, and returns no value."
  (case *current-direction*
    (:left
      (decf (point-x *current-position*)))
    (:right
      (incf (point-x *current-position*)))
    (:up
      (decf (point-y *current-position*)))
    (:down
      (incf (point-y *current-position*)))
    (otherwise
      (error "The direction ~s is invalid." *current-direction*)))
  (correct-the-instruction-pointer-position)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the executive operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-the-current-token ()
  "Evaluates the ``*PLAYFIELD*'''s current symbol and returns no value."
  (case *current-token*
    ;; ================================================================
    ;; Memory management operations.
    ;; ================================================================
    (#\>
      (incf *cell-pointer*))
    (#\<
      (decf *cell-pointer*))
    (#\@
      (setf *cell-pointer* *current-cell-value*))
    (#\+
      (incf *current-cell-value*))
    (#\-
      (decf *current-cell-value*))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (setf *current-cell-value*
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop
              while
                (digit-char-p *current-token*)
              for prevenient-position
                of-type Point
                =       (copy-point *current-position*)
                then    (copy-the-point-coordinates-from
                          prevenient-position
                          *current-position*)
              do
                (write-char *current-token* digits)
                (vaunce-the-instruction-pointer)
              finally
                (setf *current-position* prevenient-position))))))
    (#\'
      (vaunce-the-instruction-pointer)
      (setf *current-cell-value*
        (char-code *current-token*)))
    
    ;; ================================================================
    ;; Input and output operations.
    ;; ================================================================
    (#\.
      (format *query-io* "~c"
        (code-char *current-cell-value*))
      (force-output *query-io*))
    (#\!
      (format       *query-io* "~&~d~%" *current-cell-value*)
      (force-output *query-io*))
    (#\,
      (format       *query-io* "~&Please input a character: ")
      (force-output *query-io*)
      (setf *current-cell-value*
        (char-code
          (read-char *query-io* NIL +NULL-CHARACTER+)))
      (clear-input *query-io*))
    (#\&
      (loop
        for raw-input
          of-type string
          =       (progn
                    (format *query-io*
                      "~&Please input an integer: ")
                    (force-output *query-io*)
                    (prog1
                      (read-line   *query-io* NIL "")
                      (clear-input *query-io*)))
        for parsed-input
          of-type (or null integer)
          =       (ignore-errors
                    (parse-integer raw-input))
        
        if parsed-input do
          (setf *current-cell-value* parsed-input)
          (loop-finish)
        else do
          (format *query-io*
            "~&The input ~s does not represent an integer number."
            raw-input)
        end))
    
    ;; ================================================================
    ;; Navigational operations.
    ;; ================================================================
    (#\)
      (setf *current-direction* :right))
    (#\(
      (setf *current-direction* :left))
    (#\^
      (setf *current-direction* :up))
    (#\v
      (setf *current-direction* :down))
    (#\?
      (setf *current-direction*
        (aref +DIRECTIONS+
          (random
            (length +DIRECTIONS+)))))
    (#\/
      (setf *current-direction*
        (case *current-direction*
          (:left  :down)
          (:right :up)
          (:up    :right)
          (:down  :left)
          (otherwise
            (error "The direction ~s cannot be mirrored."
              *current-direction*)))))
    (#\\
      (setf *current-direction*
        (case *current-direction*
          (:left  :up)
          (:right :down)
          (:up    :left)
          (:down  :right)
          (otherwise
            (error "The direction ~s cannot be mirrored."
              *current-direction*)))))
    (#\_
      (setf *current-direction*
        (if (zerop *current-cell-value*)
          :right
          :left)))
    (#\|
      (setf *current-direction*
        (if (zerop *current-cell-value*)
          :down
          :up)))
    
    ;; ================================================================
    ;; Control flow duction operations.
    ;; ================================================================
    (#\#
      (vaunce-the-instruction-pointer))
    (#\*
      (setf *program-has-halted-p* T))
    
    ;; ================================================================
    ;; Ineffectual operations.
    ;; ================================================================
    (otherwise
      NIL))
  
  (values))

;;; -------------------------------------------------------

(defun execute-the-current-program ()
  "Executes the Brafunge program contemporaneously concredited to the
   interpreter's castaldy and being represented by the ``*PLAYFIELD*'',
   and returns no value."
  (loop until *program-has-halted-p* do
    (process-the-current-token)
    (vaunce-the-instruction-pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpretation operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-brafunge-code (code)
  "Interprets the piece of Brafunge source CODE and returns no value."
  (declare (type string code))
  (graith-the-interpreter code)
  (execute-the-current-program)
  (values))

;;; -------------------------------------------------------

(defun interpret-the-brafunge-code-lines (&rest lines)
  "Concatenates the LINES into a two-dimensional Brafunge source code
   string, interprets thilk, and returns no value."
  (declare (type (list-of string) lines))
  (interpret-the-brafunge-code
    (apply #'concatenate-the-lines lines))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, world!" to the standard output conduit.
(interpret-the-brafunge-code
  "72.101.108.108.111.44.32.119.111.114.108.100.33.*")

;;; -------------------------------------------------------

;; Print the Fibonacci numbers.
(interpret-the-brafunge-code
  "v        )>+> v
)1)!<10.>|-<<+(
  ^     >(")

;;; -------------------------------------------------------

;; Perpetually repeating character-based cat program.
(interpret-the-brafunge-code-lines
  "),.v"
  "^  (")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-brafunge-code-lines
  "&v)!v"
  " )|^("
  "  !"
  "  *")
