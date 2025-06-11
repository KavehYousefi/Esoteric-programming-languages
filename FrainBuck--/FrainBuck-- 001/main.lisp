;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "FrainBuck--", invented by the Esolang user "Xi-816" and
;; presented on September 2nd, 2023, the commorancy of its diorism a
;; transformation of Urban Mueller's language "brainfuck" from the
;; single dimension to a two-dimensional construct, concomitant to its
;; capabilities' severe curtailment to basic arithmetics, output, and
;; instruction pointer navigation.
;; 
;; 
;; Concept
;; =======
;; The FrainBuck-- programming language's existency ensues from a
;; derivation of brainfuck, its variations a twifaced enterprise;
;; imprimis, applying a two-dimensional rearrangement to the flat string
;; format of the syntactical design; secondly, exercising a curtailment
;; in the instruction set from the provenance's octuple into a quartet
;; which in its paravaunt actions increments the current cell and
;; prints its character form, supplemented by a navigational twain that
;; permits a controlled perambulation in the code.
;; 
;; == THE PROGRAM: A CARTESIAN GRID OF SYMBOLS ==
;; FrainBuck--'s notion of a program ensues from a two-dimensional
;; Cartesian grid of character, admitting any content; however, merely
;; a quadruple subset of this contingency's investment with actual
;; causata is accounted for.
;; 
;; == THE INSTRUCTION POINTER MOVES ACROSS THE GRID ==
;; Commencing in the lacis' left upper corner, the instruction pointer
;; (IP) in its inchoate configuration seeks a perambulation in the
;; sinistrodextral airt, evaluating those characters endowed with
;; epiphenomenal, while neglecting orra constitutents.
;; 
;; A twissel of instructions, namely, ":" and "~", are entalented with
;; the capacity for the instruction pointer's redirection, whence a
;; more liberal anabasis athwart the code may be eventuated.
;; 
;; == A WAYWARD INSTRUCTION POINTER HALTS THE PROGRAM ==
;; The instruction pointer's transgression ayond the character grid's
;; defined marches instigates the program's immediate cessation.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; An equipoise's establishment partakes of FrainBuck--'s memory
;; architecture in juxtaposition program conformation that pursues an
;; "a reticelli" concept, the data castaldy retaining its lealty to 
;; brainfuck's desigment, appropriating in an ipsissima verba fashion a
;; bilaterally bourneless dispansion of unsigned byte-valued cells,
;; apposted in a seriatim ordonnance.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; 
;; Instructions
;; ============
;; A mere quadruple cardinality governs modest instruction set of the
;; FrainBuck-- programming language, inwith whose bournes are enhalsed
;; one arithmetic operations, instruction pointer and cell pointer
;; redirections, and an aefauld output behest.
;; 
;; == OVERVIEW ==
;; The following tabulation's wike shall be realized in a sufficient
;; mete of nortelry's adhibition concerning the language's operative
;; competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one (1). If the new
;;           | state transcends the upper bourne of 255, the cells
;;           | wraps around to the lower extremum of zero (0).
;;   ..................................................................
;;   :       | If the current cell value constitutes an even number,
;;           | changes the instruction pointer (IP) direction to a
;;           | sinistral locomotion, and translates the cell pointer
;;           | one step to the right; otherwise, for an odd cell value,
;;           | alters the IP direction to a dextral movement, while the
;;           | cell pointer is translated one cell to the left.
;;   ..................................................................
;;   ~       | Rotates the instruction pointer (IP) by 90 degrees along
;;           | a deasil, that is, rightwards trajectory.
;;           |---------------------------------------------------------
;;           | A consectary of this reorientation, the following
;;           | nomothesy applies for the transition:
;;           | 
;;           |   ----------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+---------------
;;           |   right             | down
;;           |   ..................................
;;           |   down              | left
;;           |   ..................................
;;           |   left              | up
;;           |   ..................................
;;           |   up                | right
;;           |   ----------------------------------
;;   ..................................................................
;;   $       | Prints the character whose ASCII code corresponds to
;;           | the current cell value to the standard output conduit.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been exercised in the programming
;; language Common Lisp, the vinculum betwixt the FrainBuck-- source
;; code's reception and its ultimate execution an intermediary stratum
;; whose componency enumerates a twissel effort in the location and
;; collation of the effective symbols and their actual insertion into
;; a grid-like array.
;; 
;; == FROM A ONE-DIMENSIONAL STRING TO A TWO-DIMENSIONAL GRID ==
;; The input source code's rearrangement into a veridicous
;; two-dimensional reticulation, whence the dation of an executable
;; FrainBuck-- program realizes, shall be explicated in its two stages
;; alow:
;; 
;;   (1) OPERATION SYMBOLS EXTRACTION
;;       Proceeding in a sinistrodextral airt in the FrainBuck-- source
;;       code, and considering each linebreak to introduce a new row
;;       in the targeted two-dimensional grid structure, any symbol of
;;       operative potential, that is, a member of the quadruple set
;;       { "+", ":", "~", "$" }, is collected into an unordered
;;       sequence, comprehending, in conjunction to its identifier, the
;;       zero-based future column index, or x-coordinate, and the row
;;       index, or y-coordinate, whence ensues a list of triplets
;;       
;;         (x, y, character)
;;       
;;       Characters not endowed with an epiphenomenon do not contribute
;;       to the resulting collection.
;;       
;;       As a further, peisant adminiculum to this collation stage, the
;;       maximum column and row indices occupied by any character,
;;       with a Procrustean conspectuity's administration on its actual
;;       application in the program, enter the supputation; proceeding
;;       from this gnarity, we assign to the triplet list the thus
;;       reckoned grid width and height, ultimately returning three
;;       values:
;;       
;;         (grid-width, grid-height, list-of-instruction-triplets)
;;   
;;   (2) TWO-DIMENSIONAL ARRAY GENERATIONS
;;       Enjoying the usufructure of the grid dimensions and
;;       instruction triplets list, --- which, as an apostille
;;       furnished, bears the simulacrum of one possible sparse matrix
;;       implementations ---, the capacitation of a veridical
;;       two-dimensional reification for the program is accoutred.
;;       
;;       In this pursuit, a two-dimensional array of the supputated
;;       width and height, initially populated with the default
;;       non-operative space symbol (ASCII code: 32), chosen rather
;;       in an aleatory mode as the standard no-operation designator,
;;       is generated.
;;       
;;       The ultimate grid incarnation concludes with the insertion
;;       of the operation identifiers at their appropriate locations,
;;       a gendrure enabled by a destructuring and conspection of the
;;       instruction triplets, each such a compound
;;       
;;         (x, y, character)
;;       
;;       By storing this CHARACTER in the zero-based X-th column of the
;;       Y-th row in the array, the executable FrainBuck-- program's
;;       reticulate conformation is begotten.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-06-07
;; 
;; Sources:
;;   [esolang2023FrainBuck--]
;;   The Esolang contributors, "FrainBuck--", September 3rd, 2023
;;   URL: "https://esolangs.org/wiki/FrainBuck--"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-new-type (type-name (candidate-name &rest lambda-list)
                           &body body)
  "Defines a derived type, the agnomination of whom constitutes the
   TYPE-NAME's dation, its formal parameters that of the LAMBDA-LIST,
   while the subject of the docimasy is nevened by the CANDIDATE-NAME,
   the assessment process itself ensues from the evaluation of the BODY
   forms, the desinent form's primary result producing the conclusion,
   where a \"generalized boolean\" value of \"true\" is adjudged as the
   candidate's compatibility, while a \"false\" response shall be a
   rejection's tantamount.
   ---
   The first BODY, upon its resolution to a string, partakes of the
   consideration as the derived type's documentation string, and will
   be, as a corollary, reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            (format NIL "The type ~a." type-name))
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

(define-new-type list-of (candidate
                          &optional (element-type '*)
                                    (size         '*))
  "The ``list-of'' type defines a list composed of the SIZE tally of
   elements, each member of which complies with the ELEMENT-TYPE,
   the same to the generic sentinel ``*'', while the SIZE by default
   assumes ``*'' in order to admit any componency."
  (and
    (listp candidate)
    (or (eq size '*)
        (= (length (the list candidate))
           size))
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-new-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a tuple as an ordered list whose
   elements in their tally and type comply with the ELEMENT-TYPES.
   ---
   In a concrete diction, for
     the candidate being a list of N elements      C = (c1, ..., cM)
     and the element types as a list of M elements T = (t1, ..., tN),
   it must hold:
     (N = M)
     and
     (c[i] is of the type t[i], for all c[i] in C and t[i] in T,
      with 1 <= i <= N)."
  (and
    (listp candidate)
    (= (length (the list candidate))
       (length (the list element-types)))
    (every #'typep
      (the list candidate)
      (the list element-types))))

;;; -------------------------------------------------------

(deftype grid-entry-triplet ()
  "The ``grid-entry-triplet'' type defines a FrainBuck-- operation
   symbol and its location ensconced in a single compound, founded upon
   an ordered list of three elements: (x-position, y-position, token),
   where the former twain assumes a fixnum guise, while the latter
   resolves to a ``standard-char''."
  '(tuple-of fixnum fixnum standard-char))

;;; -------------------------------------------------------

(deftype code-grid ()
  "The ``code-grid'' type defines a two-dimensional Cartesian ordonnance
   of characters forming a grid capable of traversal and evaluation.
   ---
   This code grid format usually serves as a polymechany whose
   conformation in its spatial rigor and stringency homologates a more
   accessible reformulation of a one-dimensional source code string."
  '(simple-array standard-char (* *)))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the valid airts along which the
   program flow may be conducted."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, and, as a corollary, an occupant of the closed
   integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-as-boolean-value (object)
  "Construes the OBJECT as a \"generalized boolean\" value and returns
   a veridicous Boolean tantamount thereof, returning for a non-``NIL''
   input a ``boolean'' value of ``T''; otherwise, for a ``NIL'' OBJECT,
   produces ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of fixnum 4) +LINEBREAK-CHARACTER-CODES+))

;;; -------------------------------------------------------

(defparameter +LINEBREAK-CHARACTER-CODES+
  '(10 11 12 13)
  "Enumerates the ASCII codes of the recognized characters capacitated
   to act as linebreaks, ultimately serving to segregate a piece of
   FrainBuck-- source code issued as a string into grid rows.
   ---
   The following definitions are included:
     -----------------------------------------------------
     ASCII code | Linebreak character name | Abbreviation
     -----------+--------------------------+--------------
     10         | Newline, Line Feed       | LF
     .....................................................
     11         | Vertical Tabulation      | VT
     .....................................................
     12         | Form Feed                | FF
     .....................................................
     13         | Carriage Return          | CR
     -----------------------------------------------------")

;;; -------------------------------------------------------

(defun linebreak-character-p (candidate)
  "Determines whether the CANDIDATE belongs to the species entailing the
   line termination characters, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (interpret-as-boolean-value
      (member
        (char-code candidate)
        +LINEBREAK-CHARACTER-CODES+ :test #'=))))

;;; -------------------------------------------------------

(defun operative-character-p (candidate)
  "Determines whether the CANDIDATE represents a FrainBuck-- operation
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (interpret-as-boolean-value
      (find candidate "+:~$" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operative entries extractor.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-operative-grid-entries (code)
  "Extracts from the piece of FrainBuck-- source CODE the entailed
   operative symbols, as well as the dimensions of the entire
   rectangular space represented in the CODE, and returns three values:
     (1) The number of columns comprising the spanned grid.
     (2) The number of rows    comprising the spanned grid.
     (3) A list of the located operation symbols, each such a triplet,
         the compound ensconcing the zero-based column index, the row
         index, and the instruction token in this exact order as a list
         (x-position, y-position, character).
   ---
   The resulting sequence and its element stipulations may be considered
   the firmament for a sparse matrix representation."
  (declare (type string code))
  (the (values fixnum fixnum (list-of grid-entry-triplet))
    (let ((x      0)
          (y      0)
          (width  0)
          (height 0))
      (declare (type fixnum x))
      (declare (type fixnum y))
      (declare (type fixnum width))
      (declare (type fixnum height))
      (flet
          ((break-line ()
            "Applies a linebreak in the CODE and returns no value."
            (setf x 0)
            (incf y 1)
            (values))
           
           (advance-to-next-character ()
            "Advances to the next column and returns no value."
            (incf x 1)
            (values))
           
           (update-dimensions ()
            "Supputates the WIDTH and HEIGHT with respect to the current
             position (X, Y) and returns no value."
            (psetf
              width  (max width  (1+ x))
              height (max height (1+ y)))
            (values)))
        
        (loop
          for current-token of-type character across code
          
          if (linebreak-character-p current-token) do
            (break-line)
            (update-dimensions)
          else if (operative-character-p current-token)
            collect
              (prog1
                (list x y current-token)
                (update-dimensions)
                (advance-to-next-character))
              into operative-entries
          else do
            (update-dimensions)
            (advance-to-next-character)
          end
          
          finally
            (return
              (values width height operative-entries)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code grid operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-code-grid (width height)
  "Creates and returns a code grid measuring the WIDTH tally of columns
   and the HEIGHT account of rows, its cells in its entirety being
   initialized to space characters (ASCII code: 32)."
  (declare (type fixnum width))
  (declare (type fixnum height))
  (the code-grid
    (make-array
      (list height width)
      :element-type    'standard-char
      :initial-element #\Space
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun code-grid-contains-point-p (grid x y)
  "Determines whether the position specified by the zero-based column
   index X and the zero-based row index Y designates a valid cell in
   the code GRID, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type code-grid grid))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (the boolean
    (interpret-as-boolean-value
      (array-in-bounds-p grid y x))))

;;; -------------------------------------------------------

(defun get-code-grid-symbol-at (grid x y)
  "Returns the symbol stored in the code GRID cell specified by the
   zero-based column index X and the zero-based row index Y."
  (declare (type code-grid grid))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (the standard-char
    (aref grid y x)))

;;; -------------------------------------------------------

(defun set-code-grid-symbol-at (grid x y new-symbol)
  "Stores the NEW-SYMBOL in the GRID cell amenable to the position
   specified by the zero-based column index X and the zero-based row
   index Y, and returns no value."
  (declare (type code-grid grid))
  (declare (type fixnum        x))
  (declare (type fixnum        y))
  (declare (type standard-char new-symbol))
  (setf (aref grid y x) new-symbol)
  (values))

;;; -------------------------------------------------------

(defun build-code-grid-from-entries (width height operative-entries)
  "Creates and returns a fresh code grid, measuring the WIDTH tally of
   columns and HEIGHT account of rows, bearing space symbols at any
   position except for those specified by the OPERATIVE-ENTRIES, thilk
   are appropriated by the grid."
  (declare (type fixnum                       width))
  (declare (type fixnum                       height))
  (declare (type (list-of grid-entry-triplet) operative-entries))
  (let ((code-grid (prepare-empty-code-grid width height)))
    (declare (type code-grid code-grid))
    (loop
      for (x y token)
        of-type (fixnum fixnum standard-char)
        in      operative-entries
      do
        (set-code-grid-symbol-at code-grid x y token))
    (the code-grid code-grid)))

;;; -------------------------------------------------------

(defun make-code-grid-for-program (code)
  "Creates and returns a fresh code grid whose dimensions and operative
   symbols are desumed from the piece of FrainBuck-- source CODE, while
   any non-effective position assumes the default space (ASCII code: 32)
   symbol."
  (declare (type string code))
  (the code-grid
    (multiple-value-call #'build-code-grid-from-entries
      (extract-operative-grid-entries code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-direction-turned-right (current-direction)
  "Returns the direction resulting from a rotation applied to the
   CURRENT-DIRECTION by 90 degrees along a deasil trajectory."
  (declare (type direction current-direction))
  (the direction
    (case current-direction
      (:right :down)
      (:down  :left)
      (:left  :up)
      (:up    :right)
      (otherwise
        (error "Invalid direction: ~s." current-direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction pointer (IP).                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Instruction-Pointer ()
  ((x
    :initform      0
    :accessor      ip-x
    :type          fixnum
    :documentation "The zero-based column index.")
   (y
    :initform      0
    :accessor      ip-y
    :type          fixnum
    :documentation "The zero-based row index.")
   (direction
    :initform      :right
    :accessor      ip-direction
    :type          direction
    :documentation "The traversal direction."))
  (:documentation
    "The ``Instruction-Pointer'' class serves in the reification of
     the instruction pointer (IP) diorism, its state intrining the
     current row and column indices, both zero-based and integral in
     their nature, and a traversal airt."))

;;; -------------------------------------------------------

(defun advance-instruction-pointer (pointer)
  "Advances the instruction POINTER one step into its current direction
   and returns no value."
  (declare (type Instruction-Pointer pointer))
  (case (ip-direction pointer)
    (:right (incf (ip-x pointer)))
    (:down  (incf (ip-y pointer)))
    (:left  (decf (ip-x pointer)))
    (:up    (decf (ip-y pointer)))
    (otherwise
      (error "Invalid instruction pointer direction: ~s."
        (ip-direction pointer))))
  (values))

;;; -------------------------------------------------------

(defun wherve-instruction-pointer-deasil (pointer)
  "Wherves the instruction POINTER's direction by 90 degrees from its
   current orientation along a deasil trajectory and returns no value."
  (declare (type Instruction-Pointer pointer))
  (setf (ip-direction pointer)
    (get-direction-turned-right
      (ip-direction pointer)))
  (values))

;;; -------------------------------------------------------

(defun redirect-instruction-pointer (pointer new-direction)
  "Alters the instruction POINTER's orientation to the NEW-DIRECTION and
   returns no value."
  (declare (type Instruction-pointer pointer))
  (declare (type direction           new-direction))
  (setf (ip-direction pointer) new-direction)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((bits
    :initform      #b00000000
    :accessor      tape-bits
    :type          unsigned-byte
    :documentation "A sequence of bits, encoded in an unsigned integer
                    number, whose octal groups each represent a certain
                    cell's state.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          integer
    :documentation "The cell pointer, responsible for the currently
                    active cell's designation.")
   (smallest-accessed-cell-index
    :initform      0
    :accessor      tape-smallest-accessed-cell-index
    :type          integer
    :documentation "The smallest value assumed by the cell POINTER
                    during a program's execution."))
  (:documentation
    "The ``Tape'' class implements the FrainBuck-- program memory as a
     bilaterally infinite tape composed of unsigned byte-valued cells,
     and operated upon by a mobile cell pointer which at any instant
     designates the currently active unit.
     ---
     The salvatory for the tape's cell information manifests in a
     single unsigned byte value, encoding in each eight consecutive bits
     one cell's state."))

;;; -------------------------------------------------------

(defun prepare-pristine-tape ()
  "Creates and returns a fresh ``Tape'' assuming its inchoate state of
   zero-valued cells."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun translate-cell-pointer-into-bit-offset (tape)
  "Returns the unsigned integer bit offset at which the bits comprising
   the cell under the TAPE's cell pointer commence in its binary
   sequence."
  (declare (type Tape tape))
  (the (integer 0 *)
    (* (- (tape-pointer                      tape)
          (tape-smallest-accessed-cell-index tape))
       8)))

;;; -------------------------------------------------------

(defun locate-bits-of-current-cell (tape)
  "Returns an implementation-dependent byte specifier which selects in
   the TAPE's binary sequence the eight bits comprising the cell under
   the TAPE's cell pointer."
  (declare (type Tape tape))
  (the T
    (byte 8
      (translate-cell-pointer-into-bit-offset tape))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the octet
    (ldb
      (locate-bits-of-current-cell tape)
      (tape-bits                   tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   preceding this consumption by a wrapping of the input into the
   admissible unsigned byte range [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (ldb
      (locate-bits-of-current-cell tape)
      (tape-bits                   tape))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (tape)
  "Increments the TAPE's current cell value by one (1), contingently
   wrapping its state around, upon a transgression of the upper bourne
   of 255, and returns no value."
  (declare (type Tape tape))
  (incf (current-cell-value tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (tape-pointer tape))
  ;; If necessitated, "insert" eight new bits at the tape bits
  ;; sequence's lowest positions in order to accommodate a freshly
  ;; discovered cell.
  (when (< (tape-pointer                      tape)
           (tape-smallest-accessed-cell-index tape))
    (psetf
      (tape-smallest-accessed-cell-index tape)
        (tape-pointer tape)
      (tape-bits tape)
        (ash (tape-bits tape) 8)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :reader        interpreter-program
    :type          code-grid
    :documentation "The FrainBuck-- program defined in terms of a
                    two-dimensional grid of characters.")
   (ip
    :initform      (make-instance 'Instruction-Pointer)
    :reader        interpreter-ip
    :type          Instruction-Pointer
    :documentation "The instruction pointer (IP), responsible for the
                    PROGRAM grid's traversal in order to execute the
                    encompassed instructions.")
   (tape
    :initform      (prepare-pristine-tape)
    :reader        interpreter-tape
    :type          Tape
    :documentation "The program memory as a bilaterally infinite catena
                    of unsigned byte-valued cells."))
  (:documentation
    "The ``Interpreter'' class furnishes an entity whose bailiwick
     relates to the execution of a FrainBuck-- program."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   execution of the FrainBuck-- PROGRAM, the same is supplied in the
   form of a two-dimensional character grid."
  (declare (type code-grid program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-has-halted-p (interpreter)
  "Determines whether the INTERPRETER's program has halted as a
   consequence of its instruction pointer's (IP) transcendence ayond
   the admissible bournes."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (code-grid-contains-point-p
        (interpreter-program  interpreter)
        (ip-x (interpreter-ip interpreter))
        (ip-y (interpreter-ip interpreter))))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position along its trajectory and returns no value."
  (declare (type Interpreter interpreter))
  (advance-instruction-pointer
    (interpreter-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun get-current-token (interpreter)
  "Returns the symbol located at the INTERPRETER's instruction pointer
   (IP) position with respect to the underlying program grid."
  (declare (type Interpreter interpreter))
  (the standard-char
    (get-code-grid-symbol-at
      (interpreter-program  interpreter)
      (ip-x (interpreter-ip interpreter))
      (ip-y (interpreter-ip interpreter)))))

;;; -------------------------------------------------------

(defgeneric process-token (interpreter token)
  (:documentation
    "Evaluates the TOKEN in the INTERPRETER's context and returns no
     value.")
  
  (:method ((interpreter Interpreter) (token (eql #\+)))
    (declare (type Interpreter   interpreter))
    (declare (type standard-char token)
             (ignore             token))
    (increment-current-cell
      (interpreter-tape interpreter))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\:)))
    (declare (type Interpreter   interpreter))
    (declare (type standard-char token)
             (ignore             token))
    (with-slots (ip tape) interpreter
      (declare (type Instruction-Pointer ip))
      (declare (type Tape                tape))
      (cond
        ((evenp (current-cell-value tape))
          (redirect-instruction-pointer ip :left)
          (move-cell-pointer-right      tape))
        (T
          (redirect-instruction-pointer ip :right)
          (move-cell-pointer-left       tape))))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\~)))
    (declare (type Interpreter   interpreter))
    (declare (type standard-char token)
             (ignore             token))
    (wherve-instruction-pointer-deasil
      (interpreter-ip interpreter))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\$)))
    (declare (type Interpreter   interpreter))
    (declare (type standard-char token)
             (ignore             token))
    (format *standard-output* "~c"
      (code-char
        (current-cell-value
          (interpreter-tape interpreter))))
    (finish-output *standard-output*)
    (values))
  
  (:method ((interpreter Interpreter) (token character))
    (declare (type Interpreter   interpreter)
             (ignore             interpreter))
    (declare (type standard-char token)
             (ignore             token))
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the FrainBuck-- program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-has-halted-p interpreter) do
    (process-token interpreter
      (get-current-token interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-FrainBuck-- (code)
  "Interprets the piece of FrainBuck-- source CODE and returns no
   value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (make-code-grid-for-program code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenate the LINES into a single string by the insertion of
   exactly one newline character in the interstice betwixt two accolent
   lines and returns the a simple string representation of the result."
  (declare (type (list-of string) lines))
  (the simple-string
    (coerce
      (format NIL "~{~a~^~%~}" lines)
      'simple-string)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A" to the standard output.
(interpret-FrainBuck--
  (concatenate-lines
    "+++++++++++++++++++++++++++++++++~"
    "$++++++++++++++++++++++++++++++++~"))

;;; -------------------------------------------------------

;; Repeatedly print the "null character" and the digit "1" as twains in
;; immediate succession.
(interpret-FrainBuck--
  (concatenate-lines
    "++++++++++++++++++++++++++++++++++++++++++++++++~    "
    "                                            : + :$  :"))
