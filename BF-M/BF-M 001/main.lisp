;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BF-M", invented by the Esolang user "AshuraTheHedgehog"
;; and presented on August 6th, 2016, the dioristic contributions of
;; which wone in two aspects: imprimis, its extension of Urban Mueller's
;; "brainfuck" to a further memory component as a compernage to the
;; bilaterally infinite tape, defined by a single unsigned byte-valued
;; cell, or register; further its supererogation in the printing of
;; both data salvatory's contents in their verbatim numeric form as an
;; alternative to the traditional character output.
;; 
;; 
;; Concept
;; =======
;; The BF-M programming language receives its competences from the
;; entheus brainfuck, augmented in its haecceity by several
;; polymechanies, including a parergal register cell in the data
;; castaldy's context and the contingency for printing both memory
;; components' contents in their original numeric form.
;; 
;; == THE MEMORY: A TWISSEL OF TAPE AND REGISTER ==
;; BF-M's memory design enumerates a twain of components: imprimis, the
;; bilaterally infinite tape, desumed verbatim from its brainfuck's
;; stock-father; and, as a parhedral warklume, a single register of an
;; unsigned byte-valued cell's capacity and deportment.
;; 
;; == THE TAPE: A BILATERALLY INFINITE SEQUENCE OF BYTES ==
;; The memory's first moeity, the tape, ensues in its diorism from a
;; bilaterally infinite catena of cells, everichon among these an
;; unsigned byte value's salvatory, its capacity, as a corollary, meted
;; with the integral interval [0, 255]. Upon any of its bournes'
;; transgression, the state wraps around along the athwart extremum.
;; 
;; Operating upon this dispansion, a cell pointer selects at any instant
;; during the program's execution the currently active cell, thilk
;; furnishing the only unit entalented with an amenability to
;; perquisitions and modulations. Its mobile nature begets the
;; homologation of this cursor's stillatim translations along the tape's
;; axes, by the same faculty any cell may be navigated.
;; 
;; == THE REGISTER: A SINGLE UNSIGNED BYTE CELL ==
;; An extension of brainfuck's notions, BF-M's architecture lends a
;; compernage to the bilaterally infinite tape composed of unsigned
;; bytes by an aefauld cell, eloigned from the tape's cohesion, but
;; invested with the selfsame attributes for such a component, while
;; accompassing its causata in an isolated manner.
;; 
;; This register also admits values of the range [0, 255], wrapping upon
;; transgression along the marches.
;; 
;; 
;; Instructions
;; ============
;; A derivative of brainfuck entalented with the telos of its faculties'
;; augmentation, the circumference of BF-M's instruction set accrues in
;; a twifold manner, comprehending 16 members rather than the octuple
;; cardinality whose acquaintance is contributed in the provenance.
;; 
;; Such magnanimity's cynosure bifurcates into warklumes requisite for
;; the newly introduced memory register's handling, as well as towards
;; a more eath reproduction of numeric values on the standard output
;; conduit.
;; 
;; == OVERVIEW ==
;; A dation vouchsafed by the following apercu shall be realized in a
;; sufficient mete of nortelry anent the BF-M programming language's
;; operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the tape's current cell by one (1).
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's competences.
;;   ..................................................................
;;   -       | Decrements the tape's current cell by one (1).
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's competences.
;;   ..................................................................
;;   >       | Translates the tape's cell pointer one step to the
;;           | right.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's competences.
;;   ..................................................................
;;   <       | Translates the tape's cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's competences.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the tape's current cell.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code conflates with the
;;           | tape's current cell value to the standard output
;;           | conduit.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's competences.
;;   ..................................................................
;;   '       | Prints the tape's current cell value in its verbatim
;;           | numeric form to the standard output conduit.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   [       | If the tape's current cell value contains the value
;;           | zero (0), moves the instruction pointer (IP) forward to
;;           | the position immediately succeeding the matching "]"
;;           | token; otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | If the tape's current cell value does not contain the
;;           | value zero (0), moves the instruction pointer (IP) back
;;           | to the position immediately succeeding the matching "["
;;           | token; otherwise proceeds as usual.
;;   ==================================================================
;;   |       | Increments the register value by one (1).
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   _       | Decrements the register value by one (1).
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   /       | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the register.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   \       | Prints the character whose ASCII code conflates with the
;;           | register's value to the standard output conduit.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   "       | Prints the register in its verbatim numeric form to the
;;           | standard output conduit.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   (       | If the register contains the value zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching ")" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ..................................................................
;;   )       | If the register does not contain the value zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "(" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious novelty
;;           | forinsecal to brainfuck.
;;   ------------------------------------------------------------------
;; 
;; == JUXTAPOSITION OF OPERATIONS ON TAPE AND REGISTER ==
;; An ultimity whose emergence relates to the duality of BF-M's
;; traditional tape and the memory's novel register component --- except
;; for the autochthonous accommodation to the former haecceity's in the
;; cell pointer's navigation ---, an equiparation may be yielded for
;; all remaining operative competences:
;; 
;;   -------------------------------------------
;;   Operation          | On tape | On register
;;   -------------------+---------+-------------
;;   Increment          | +       | |
;;   ...........................................
;;   Decrement          | -       | _
;;   ...........................................
;;   Input character    | ,       | /
;;   ...........................................
;;   Print as character | .       | \
;;   ...........................................
;;   Print as number    | '       | "
;;   ...........................................
;;   Jump forward       | [       | (
;;   ...........................................
;;   Jump back          | ]       | )
;;   -------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The eath nature of brainfuck as BF-M's provenance of cleronomy, in
;; champarty with its personal expositions, in their preponderance
;; nomothetic cognates to the ejusdem generis principle applicable to
;; the inspiration's operations, disencumbers the protolog from nearly
;; any inroads of ambivalency.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand's implementation constitutes a produce in
;; the programming language Common Lisp, its mode of accompassing the
;; expected efficacy adhibited immediately on the input source code
;; string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-07-24
;; 
;; Sources:
;;   [esolang2016User:AshuraTheHedgehog]
;;   The Esolang contributors, "User:AshuraTheHedgehog",
;;     September 2nd, 2016
;;   URL: "https://esolangs.org/wiki/User:AshuraTheHedgehog"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, imposing upon the keys a compliance with the KEY-TYPE,
   and for the value a siclike lealty to the VALUE-TYPE, both species
   defaulting to the generic sentinel ``*''."
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
                (and (typep current-key   key-type)
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, every member of which complies with the ELEMENT-TYPE, for
   thilk is defined the generic sentinel ``*'' as a default."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   forward and back jump points in a BF-M program, their mediation
   proceeds by adminiculum of the respective command token's zero-based
   positions into the source code, and whose manifestation's firmament
   is edified upon a hash table whose keys and values both admit
   ``fixnum'' objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value as an occupant of
   the closed integral interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype sparse-cell-vector ()
  "The ``sparse-cell-vector'' type defines a sparse vector of unsigned
   byte-valued elements, amenable to signed integer indices, and
   realized in terms of a hash table whose keys wone in the realm of
   signed integer numbers, ligated into an affiliation with values of
   the ``octet'' species."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-jump-points (jump-table
                            code
                            forward-jump-token
                            back-jump-token)
  "Matches the jump points communicated via the FORWARD-JUMP-TOKEN and
   BACK-JUMP-TOKEN positions into the piece of BF-M source CODE, stores
   these affiliations in the JUMP-TABLE, and returns the latter."
  (declare (type jump-table    jump-table))
  (declare (type simple-string code))
  (declare (type standard-char forward-jump-token))
  (declare (type standard-char back-jump-token))
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-token    of-type character across code
      and current-position of-type fixnum    from   0 by 1
      
      if (char= current-token forward-jump-token) do
        (push current-position forward-jump-points)
      else if (char= current-token back-jump-token) do
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
          (error "Unmatched \"~c\" at position ~d."
            back-jump-token
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched \"~c\" token~p at position~:p ~{~d~^, ~}."
            forward-jump-token
            (length forward-jump-points)
            forward-jump-points))))
  (the jump-table jump-table))

;;; -------------------------------------------------------

(defun compute-jump-table (code)
  "Supputates and returns a fresh ``jump-table'' which connects the
   forward and back jump points partaking of the piece of BF-M in a
   bilateral manner by adminiculum of their zero-based positions into
   the same."
  (declare (type simple-string code))
  (the jump-table
    (collect-jump-points
      (collect-jump-points
        (make-hash-table :test #'eql)
        code #\[ #\])
      code #\( #\))))

;;; -------------------------------------------------------

(defun locate-jump-destination (connections departure-point)
  "Returns the obverse jump point amenable to the DEPARTURE-POINT in
   the CONNECTIONS jump table; or, upon its disrespondency, signals an
   error of an unspecified type."
  (declare (type jump-table connections))
  (declare (type fixnum     departure-point))
  (the fixnum
    (or (gethash departure-point connections)
        (error "No destination found for the departure point ~d."
          departure-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          sparse-cell-vector
    :documentation "Maps the signed integer cell indices to their
                    integral states.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer which designates the currently
                    active cell, represented by the selected key into
                    the CELLS hash table."))
  (:documentation
    "The ``Tape'' class is apportioned the wike of a bilaterally
     infinite catena of unsigned byte-valued cells' furnishment, thilk
     wists of an aefauld instance only endowed at any instant with the
     amenability to perquisitions and modulations, ensuing in its
     respondency from a mobile cell pointer's efforts.
     ---
     This tape implementation's firmament is edified upon a hash table
     acting in the agency of a sparse vector of bytes, its keys'
     compass that of the signed integer cell indices, the values the
     respective cell states in an ``octet'' guise."))

;;; -------------------------------------------------------

(defun make-tape ()
  "Creates and returns a fresh ``Tape'' whose cells are initially all
   set to zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the value consigned to the active TAPE cell's castaldy."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type sparse-cell-vector cells))
    (declare (type integer            pointer))
    (the octet
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's active cell, contingently
   preceding the transfer by a wrapping of the value into the admissible
   unsigned byte range of [0, 255], and returns no value. and returns no
   value."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type sparse-cell-vector cells))
    (declare (type integer            pointer))
    (setf (gethash pointer cells 0)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer dextrally by one step and returns
   no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer sinistrally by one step and
   returns no value."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of register.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Register ()
  ((value
    :initform      0
    :type          octet
    :documentation "The current register state."))
  (:documentation
    "The ``Register'' class is encumbered with the dever of providing a
     single unsigned byte-valued register or cell, eloigned from the
     paravaunt salvatory realized in the tape."))

;;; -------------------------------------------------------

(defun make-register ()
  "Creates and returns a fresh ``Register'' whose inchoate state
   defaults to the value zero (0)."
  (the Register
    (make-instance 'Register)))

;;; -------------------------------------------------------

(defun register-value (register)
  "Returns the REGISTER's value."
  (declare (type Register register))
  (the octet
    (slot-value register 'value)))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value register)
  "Stores the NEW-VALUE in the REGISTER, contingently preceding the
   transfer by a wrapping of the value into the admissible unsigned
   byte range of [0, 255], and returns no value."
  (declare (type integer  new-value))
  (declare (type Register register))
  (setf (slot-value register 'value)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BF-M
    (code
     &key (displays-prompt-p T)
     &aux (optimized-code    (coerce code 'simple-string)))
  "Interprets the piece of BF-M source CODE and returns no value.
   ---
   The DISPLAYS-PROMPT-P configuration homologates the ostention or
   suppression of the prompt message preceding an input request."
  (declare (type string        code))
  (declare (type simple-string optimized-code))
  (declare (type boolean       displays-prompt-p))
  (let ((ip         0)
        (jump-table (compute-jump-table optimized-code))
        (tape       (make-tape))
        (register   (make-register)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Tape       tape))
    (declare (type Register   register))
    
    (flet ((jump-to-opposite-destination ()
            "Relocates the instruction pointer (IP) to the position into
             the OPTIMIZED-CODE of the opposite jump point affiliated
             with the IP's contemporaneous location and returns no
             value."
            (setf ip
              (locate-jump-destination jump-table ip))
            (values)))
      
      (loop while (< ip (length optimized-code)) do
        (case (schar optimized-code ip)
          (#\+
            (incf (current-cell-value tape)))
          
          (#\-
            (decf (current-cell-value tape)))
          
          (#\>
            (move-cell-pointer-right tape))
          
          (#\<
            (move-cell-pointer-left tape))
          
          (#\.
            (format *standard-output* "~c"
              (code-char
                (current-cell-value tape))))
          
          (#\'
            (format *standard-output* "~d"
              (current-cell-value tape)))
          
          (#\,
            (when displays-prompt-p
              (format *standard-output* "~&>> "))
            (finish-output *standard-output*)
            (setf (current-cell-value tape)
              (char-code
                (read-char *standard-input* NIL #\Null)))
            (clear-input *standard-input*))
          
          (#\[
            (when (zerop (current-cell-value tape))
              (jump-to-opposite-destination)))
          
          (#\]
            (unless (zerop (current-cell-value tape))
              (jump-to-opposite-destination)))
          
          (#\|
            (incf (register-value register)))
          
          (#\_
            (decf (register-value register)))
          
          (#\/
            (when displays-prompt-p
              (format *standard-output* "~&>> "))
            (finish-output *standard-output*)
            (setf (register-value register)
              (char-code
                (read-char *standard-input* NIL #\Null)))
            (clear-input *standard-input*))
          
          (#\\
            (format *standard-output* "~c"
              (code-char
                (register-value register))))
          
          (#\"
            (format *standard-output* "~d"
              (register-value register)))
          
          (#\(
            (when (zerop (register-value register))
              (jump-to-opposite-destination)))
          
          (#\)
            (unless (zerop (register-value register))
              (jump-to-opposite-destination)))
          
          (otherwise
            NIL))
        
        (incf ip))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Hello, World!" program thilk employs the traditional tape
;; infrastructure.
(interpret-BF-M
  "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.")

;;; -------------------------------------------------------

;; Truth-machine which employs the register exclusively.
(interpret-BF-M
  "/
   ________________________________________________
   (\")
   \"")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input,
;; and which relies on the register only.
(interpret-BF-M
  "/(\\/)")
