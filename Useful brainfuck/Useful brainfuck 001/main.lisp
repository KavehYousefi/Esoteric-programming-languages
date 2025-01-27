;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Useful brainfuck", invented by the Esolang user "EvyLah"
;; and presented on March 25th, 2024, its conception an emergence from
;; the substratum edified by Urban Mueller's language "brainfuck",
;; hoisted in its competences via an accumulator's collaboration, as
;; well as the supplementation of input and output communications in a
;; signed integer format.
;; 
;; 
;; Concept
;; =======
;; The Useful brainfuck programming language applies itself to the
;; furnishment of adscititious competences to the sparse, yet attestedly
;; Turing-complete octuple cardinality of brainfuck, administering in
;; this polymechany's guise an adminicle manifested in a scalar
;; accumulator, akin to a segregated cell, and the capacitation for the
;; reception of input in a numeric form, as well as the athwart
;; communication along the standard output conduit in the same currency.
;; 
;; == THE WRAPPING MODE: CHOOSING UNSIGNED BYTES OR SIGNED INTEGERS ==
;; A stark criterion in which Useful brainfuck deviates from its
;; ancestor wones in the architectural department, the prevenient
;; ultimately being specified with a certain mete of adumbration, but
;; frequently, not exclusively, chosen to countenance the admission of
;; unsigned byte objects in the memory, these accounting for occupants
;; of the closed integral interval [0, 255].
;; 
;; Useful brainfuck, on the other hand, proffers a bivious solution, in
;; that this standard model might be substituted at any time by the
;; homologation of integer numbers dispanding along both axes of
;; polarity and amplecting any mickleness in their magnitude. This
;; capacitation emanates from the so-called "wrapping mode", which, if
;; active, wraps all data into the byte range [0, 255]; upon its
;; deactivation, the unbounded integer realm applies.
;; 
;; == THE MEMORY: AN INFINITE TAPE AND A SCALAR ACCUMULATOR ==
;; The memory architecture ostends a biune conformation, allocating to
;; the bilaterally infinite brainfuck tape, which in dependence upon
;; the wrapping mode switch might dispand the unsigned octet range into
;; the bourneless liberty of signed integer constituents, an accumulator
;; begotten by the same notions in adaptable capacity, enumerating,
;; however, a scalar commorant.
;; 
;; == THE TAPE: AN ININITE CATENA OF BYTES OR INTEGERS ==
;; A scion of brainfuck, this extension's appropriation includes the
;; bilaterally infinite sequence of cells, operated upon by a cell
;; pointer which at any instant designates the currently amenable unit.
;; Amplifying this granted aptitude, Useful brainfuck permits the
;; departure from the fetters of the unsigned byte range [0, 255], with
;; its wrapping behavior along the marches, for a signed integer realm
;; eloigned from any natural constraints, the segue betwixt these
;; type systems actuated by the wrapping mode, its inchoation registered
;; at the byte tier.
;; 
;; == THE ACCUMULATOR: A SINGLE BYTE OR INTEGER ==
;; A parergon to the more potent tape, Useful brainfuck's magnanimity
;; begets an adminicle in the accumulator, a scalar salvatory, its
;; capacity a tantamount to the principle applicable to the tape:
;; Proceeding from an inchoate restriction to the unsigned byte species,
;; the wrapping mode's adhibition homologates the extension towards
;; signed integers, and athwart.
;; 
;; == TAPE AND ACCUMULATOR MAY COLLABORATE ==
;; The champarty of the accumulator and the tape, molded into the
;; superior Useful brainfuck instruction set, serves as a warklume for
;; meliorated convenience in the castaldy and handling of data.
;; 
;; 
;; Instructions
;; ============
;; The Useful brainfuck instruction set enumerates a membership of 25,
;; inwith eight elements' gendrure originates from the brainfuck
;; provenance, while the 17 remnants emerge as a peculium adventicium,
;; paraphernalia peculiar to ainsell.
;; 
;; Commorant in this octuple cleronomy, a quintuple subset participates
;; with no modicum of deviation from the provenance's specification;
;; where as a pair royal wists of adjustment as a consectary provoked
;; by the wrapping mode.
;; 
;; Siclike to its stock-father's tenets, any non-command operation
;; produces a no-operation establishment, bearing the potential for
;; descant purposes.
;; 
;; == OVERVIEW ==
;; The below apercu's dever shall be realized in a cursory mete of
;; gnarity's communication anenst the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step in a dextral airt.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   <       | Translates the cell pointer one step in a sinistral
;;           | airt.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   +       | Increments the current cell value by one (1).
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the current cell's
;;           | new state transgresses 255, its value wraps around to
;;           | zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes an adjusted variation on
;;           | the brainfuck original.
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1).
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the current cell's
;;           | new state descends alow zero (0), its value wraps around
;;           | to 255.
;;           |---------------------------------------------------------
;;           | This instruction constitutes an adjusted variation on
;;           | the brainfuck original.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code matches the
;;           | current cell value to the standard output.
;;   ..................................................................
;;   [       | If the current cell contains the value zero (0), moves
;;           | the instruction pointer (IP) forward to the position of
;;           | the matching "]" token; otherwise advances as usual.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   ]       | If the current cell does not contain the value zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | of the matching "[" token; otherwise advances as usual.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   (       | Sets the accumulator's value to the current cell's
;;           | value.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the copied value
;;           | violates the admissible byte range of [0, 255], the
;;           | accumulator wraps the received number into this
;;           | interval.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   )       | Sets the current cell's value to the accumulator's
;;           | value.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the copied value
;;           | violates the admissible byte range of [0, 255], the
;;           | current cell wraps the received number into this
;;           | interval.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   0       | Resets the accumulator to its default state of zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   ?       | Sets the accumulator's value to an aleatorily selected
;;           | integer value from the closed interval [0, 255].
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   {       | Translates the cell pointer in a sinistral airt by a
;;           | tally of steps equal to the accumulator's value.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   }       | Translates the cell pointer in a dextral airt by a tally
;;           | of steps equal to the accumulator's value.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   i       | Increments the accumulator by a quantity of one (1).
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state transgresses 255, its value wraps around to
;;           | zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   d       | Decrements the accumulator by a quantity of one (1).
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state descends alow zero (0), its value wraps around
;;           | to 255.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   I       | Increments the accumulator by a quantity of ten (10).
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state transgresses 255, its value wraps around
;;           | upwards from the lower bourne of zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   D       | Decrements the accumulator by a quantity of ten (10).
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state descends alow zero (0), its value wraps around
;;           | downwards from the upper bourne of 255.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   *       | Doubles the accumulator's value.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state transgresses 255, its value wraps around
;;           | upwards from the lower bourne of zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   /       | Halves the accumulator's value, rounding towards the
;;           | nearest integer if necessary.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state descends alow zero (0), its value wraps around
;;           | downwards from the upper bourne of 255.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the accumulator's
;;           | new state transgresses 255, its value wraps around
;;           | upwards from the lower bourne of zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   a       | Increments the accumulator by the value of the current
;;           | cell.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the copied value
;;           | violates the admissible byte range of [0, 255], the
;;           | accumulator wraps the received number into this
;;           | interval.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   A       | Increments the current cell by the value of the
;;           | accumulator.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the copied value
;;           | violates the admissible byte range of [0, 255], the
;;           | current cell wraps the received number into this
;;           | interval.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   ;       | Queries the standard input for a signed or unsigned
;;           | integer number and stores the same in the current cell.
;;           |---------------------------------------------------------
;;           | If the wrapping mode is activated and the copied value
;;           | violates the admissible byte range of [0, 255], the
;;           | current cell wraps the received number into this
;;           | interval.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   :       | Prints the current cell's value in its verbatim numeric
;;           | form to the standard output.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ..................................................................
;;   '       | Toggles the wrapping mode.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a novel introduction not
;;           | present in brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been realized in the programming language
;; Common Lisp, its causata accompassed via an immediate processing of
;; the Useful brainfuck source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-26
;; 
;; Sources:
;;   [esolang2024Usefulbrainfuck]
;;   The Esolang contributors, "Useful brainfuck", March 31st, 2024
;;   URL: "https://esolangs.org/wiki/Useful_brainfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type, the agnomination of which is desumed from the
   TYPE-NAME, its formal parameters accounting for the LAMBDA-LIST's
   dation as an ipsissima verba appropriation, whereas the probed object
   acquires the nevening of the CANDIDATE-NAME, evaluates the BODY
   forms, and expects the desinent form's primary value to produce a
   \"generalized boolean\" assessment, construing a non-``NIL''
   response as the affirmation of the candidate's compliance with the
   predicate, while a ``NIL'' output supputates the docimasy's subject
   as failed in this purpose.
   ---
   The first BODY form, is bearing a string object, is construed as the
   type definition's documentation string, and reappropriated for this
   intention."
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
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Assesses the OBJECT in its aspect as a \"generalized boolean\" and
   responds with a veridical Boolean paregal thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, produces the identical ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hash table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-hash-table-entry-satisfies-p (probed-table predicate)
  "Determines whether every single entry in the PROBED-TABLE satisfies
   the PREDICATE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''.
   ---
   The PREDICATE must be a function of two arguments, the first
   constituting the currently probed key, the second admitting the
   associated value. Its result type ought to provide a \"generalized
   boolean\" object, which for a \"true\" value ascertains the
   PROBED-TABLE's compliance with the PREDICATE, whereas a \"false\", or
   ``NIL'', response expresses its failure. The signature, as a
   corollary, assumes:
     lambda (current-key current-value) => generalized-boolean"
  (declare (type hash-table         probed-table))
  (declare (type (function (* *) *) predicate))
  (the boolean
    (get-boolean-value-of
      (loop
        for current-key
          of-type T
          being the hash-keys in probed-table
        using
          (hash-value current-value)
        always
          (funcall predicate current-key current-value)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   T)
                                                 (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key among these complies with the KEY-TYPE and
   answers to a value of the VALUE-TYPE, for both is imposed the
   comprehensive ``T'' as a default."
  (and
    (hash-table-p candidate)
    (every-hash-table-entry-satisfies-p
      (the hash-table candidate)
      #'(lambda (current-key current-value)
          (declare (type T current-key))
          (declare (type T current-value))
          (and (typep current-key   key-type)
               (typep current-value value-type))))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which complies with the ELEMENT-TYPE, for thilk is
   imposed the comprehensive ``T'' as a default."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype wrapping-mode ()
  "The ``wrapping-mode'' type enumerates the admissible states for the
   byte-integer wrapping mode flag."
  '(member :on :off))

;;; -------------------------------------------------------

(deftype sparse-integer-vector ()
  "The ``sparse-integer-vector'' type defines a sparse one-dimensional
   array of signed integer numbers, bourneless anent their magnitudes,
   as a hash table, the keys of which accommodate the signed integer
   subscripts, mapping to the values desumed from selfsame realm."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of fixnum fixnum)
    :documentation "Affiliates the jump start and end points in a
                    bidirection fashion by their zero-based positions
                    in the underlying Useful brainfuck program."))
  (:documentation
    "The ``Jump-Table'' class serves in the castaldy of the jump points'
     affiliation in a Useful brainfuck program, mediated by their
     zero-based indices into the same, and operated in a bilateral
     fashion."))

;;; -------------------------------------------------------

(defun prepare-empty-jump-table ()
  "Creates and returns an initially vacant ``Jump-Table''"
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defmacro with-jump-table ((jump-table) &body body)
  "Evaluates the JUMP-TABLE, binds its slot ``connections'' to the
   local symbol macro ``$connections'', evaluates the BODY forms, and
   returns the desinent form's results."
  (let ((evaluated-table (gensym)))
    (declare (type symbol evaluated-table))
    `(let ((,evaluated-table ,jump-table))
       (declare (type Jump-Table ,evaluated-table))
       (declare (ignorable       ,evaluated-table))
       (symbol-macrolet
           (($connections
             (the (hash-table-of fixnum fixnum)
               (slot-value ,evaluated-table 'connections))))
         (declare (type (hash-table-of fixnum fixnum) $connections))
         (declare (ignorable                          $connections))
         ,@body))))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Associates the START-POINT and END-POINT in a bidirection fashion in
   the JUMP-TABLE and returns no value.
   ---
   Any extant entry with either the START-POINT or END-POINT as the key
   will be tacitly superseded by the new alliance."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (with-jump-table (jump-table)
    (psetf
      (gethash start-point $connections) end-point
      (gethash end-point   $connections) start-point))
  (values))

;;; -------------------------------------------------------

(defun supputate-jump-points-for (jump-table code)
  "Detects the jump points in the piece of Useful brainfuck source CODE,
   registers thilk in the JUMP-TABLE, and returns no value.
   ---
   Depending on the presence of jump instructions in the CODE, the
   JUMP-TABLE will destructively modified; a natural epiphenomenon
   engendered by this motion appertains to the contingency for extant
   entries' supersession by newly extracted keys. Please heed, however,
   that the JUMP-TABLE will not be purged during this function's
   execution: Vestigial keys not present in the CODE as instruction
   locations will, as a consectary, retain their presence."
  (declare (type Jump-Table    jump-table))
  (declare (type simple-string code))
  (let ((jump-start-points NIL))
    (declare (type (list-of fixnum) jump-start-points))
    (loop
      for current-token    of-type character across code
      for current-position of-type fixnum    from 0 by 1
      if (char= current-token #\[) do
        (push current-position jump-start-points)
      else if (char= current-token #\]) do
        (if jump-start-points
          (connect-jump-points jump-table
            (pop jump-start-points)
            current-position)
          (error "Unmatched jump end point at position ~d."
            current-position))
      end
      finally
        (when jump-start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length jump-start-points)
            jump-start-points))))
  (values))

;;; -------------------------------------------------------

(defun locate-jump-destination (jump-table departure-point)
  "Returns the zero-based jump instruction position obverse to the
   DEPARTURE-POINT as registered in the JUMP-TABLE; or, upon its
   disrespondency, signals an error of an unspecified type."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     departure-point))
  (with-jump-table (jump-table)
    (the fixnum
      (or (gethash departure-point $connections)
          (error "No destination associated with the jump point ~d."
            departure-point)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of wrapping mode operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-next-wrapping-mode (current-wrapping-mode)
  (:documentation
    "Returns the opposite state to the CURRENT-WRAPPING-MODE.")
  
  (:method ((current-wrapping-mode (eql :on)))
    (declare (type wrapping-mode current-wrapping-mode))
    (declare (ignore             current-wrapping-mode))
    (the wrapping-mode :off))
  
  (:method ((current-wrapping-mode (eql :off)))
    (declare (type wrapping-mode current-wrapping-mode))
    (declare (ignore             current-wrapping-mode))
    (the wrapping-mode :on)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          sparse-integer-vector
    :documentation "A sparse vector of byte- or integer-valued cells.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer which at any instant selects the
                    currently active cell by adminiculum of its position
                    into the tape, tantamount to a key in the CELLS
                    table.")
   (wrapping-mode
    :initform      :on
    :type          wrapping-mode
    :documentation "Determines whether the tape cells shall store
                    unsigned bytes only or admit signed integers of any
                    mickleness:
                      -----------------------------------------------
                      Mode | Causatum
                      -----+-----------------------------------------
                      :on  | A cell may contain only an unsigned byte
                           | object, commorant in the closed range
                           | [0, 255], and wrapping around along its
                           | bournes upon a transgression.
                           |-----------------------------------------
                           | This establishes the default state.
                      ...............................................
                      :off | A cell may contain any integer value.
                      -----------------------------------------------"))
  (:documentation
    "The ``Tape'' class serves as the reification of the Useful
     brainfuck program tape, its diorism ensuing from a bilaterally
     infinite arrangement of, in dependence upon the wrapping mode,
     either unsigned byte-valued or signed integer-valued cells, along
     which a mobile \"cell pointer\" operates, its bailiwick the
     currently active unit's designation."))

;;; -------------------------------------------------------

(defun make-pristine-tape ()
  "Creates and returns a fresh ``Tape'' whose entirety of cells is
   initialized to the default state of zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defmacro with-tape ((tape) &body body)
  "Evaluates the TAPE, binds its slot ``cells'' to the local symbol
   macro ``$cells'', its ``pointer'' to ``$pointer'', and the
   ``wrapping-mode'' to ``$wrapping-mode'', evaluates the BODY forms,
   and returns the desinent form's results."
  (let ((evaluated-tape (gensym)))
    (declare (type symbol evaluated-tape))
    `(let ((,evaluated-tape ,tape))
       (declare (type Tape ,evaluated-tape))
       (declare (ignorable ,evaluated-tape))
       (symbol-macrolet
           (($cells
             (the sparse-integer-vector
               (slot-value ,evaluated-tape 'cells)))
            ($pointer
             (the integer
               (slot-value ,evaluated-tape 'pointer)))
            ($wrapping-mode
             (the wrapping-mode
               (slot-value ,evaluated-tape 'wrapping-mode))))
         (declare (type sparse-integer-vector $cells))
         (declare (ignorable                  $cells))
         (declare (type integer               $pointer))
         (declare (ignorable                  $pointer))
         (declare (type wrapping-mode         $wrapping-mode))
         (declare (ignorable                  $wrapping-mode))
         ,@body))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte or signed integer datum stored in the
   TAPE's currently selected cell."
  (declare (type Tape tape))
  (with-tape (tape)
    (the integer
      (gethash $pointer $cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   preceded by a wrapping of its value into the unsigned byte range of
   [0, 255], if necessitated by the TAPE's governing wrapping mode, in
   any case returning no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-tape (tape)
    (case $wrapping-mode
      (:on
        (setf (gethash $pointer $cells 0)
          (mod new-value 256)))
      (:off
        (setf (gethash $pointer $cells 0) new-value))
      (otherwise
        (error "Invalid wrapping mode in tape: ~s." $wrapping-mode))))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (tape)
  "Determines whether the TAPE's currently selected cell comprehends
   the value zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (zerop (current-cell-value tape)))))

;;; -------------------------------------------------------

(defun toggle-tape-wrapping-mode (tape)
  "Switches the TAPE's wrapping mode to its opposite state and returns
   no value."
  (declare (type Tape tape))
  (with-tape (tape)
    (setf $wrapping-mode
      (get-next-wrapping-mode $wrapping-mode)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape &optional (number-of-steps 1))
  "Translates the TAPE's cell pointer by the NUMBER-OF-STEPS to the
   sinistral airt and returns no value."
  (declare (type Tape    tape))
  (declare (type integer number-of-steps))
  (with-tape (tape)
    (decf $pointer number-of-steps))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape &optional (number-of-steps 1))
  "Translates the TAPE's cell pointer by the NUMBER-OF-STEPS in the
   dextral airt and returns no value."
  (declare (type Tape    tape))
  (declare (type integer number-of-steps))
  (with-tape (tape)
    (incf $pointer number-of-steps))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program accumulator.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Accumulator ()
  ((value
    :initform      0
    :type          integer
    :documentation "The unsigned byte or signed integer datum consigned
                    to this accumulator's castaldy.")
   (wrapping-mode
    :initform      :on
    :type          wrapping-mode
    :documentation "Determines whether this accumulator shall merely
                    admit and produce an unsigned byte value, or extend
                    its capacity to a signed integer of any mickleness
                    in its magnitude:
                      -----------------------------------------------
                      Mode | Causatum
                      -----+-----------------------------------------
                      :on  | The accumulator may contain only an
                           | unsigned byte object, commorant in the
                           | closed range [0, 255], and wrapping around
                           | along its bournes upon a transgression.
                           |-----------------------------------------
                           | This establishes the default state.
                      ...............................................
                      :off | The accumulator may contain any integer
                           | value.
                      -----------------------------------------------"))
  (:documentation
    "The ``Accumulator'' class furnishes a storage entity whose capacity
     is exhausted by a scalar unsigned byte or signed integer object,
     the concrete species vouchsafed tolerance at any instant deriving
     from the contemporaneously governing wrapping mode."))

;;; -------------------------------------------------------

(defun make-accumulator ()
  "Creates and returns a fresh ``Accumulator'' instance whose value
   resolves to the default state of zero (0)."
  (the Accumulator
    (make-instance 'Accumulator)))

;;; -------------------------------------------------------

(defmacro with-accumulator ((accumulator) &body body)
  "Evaluates the ACCUMULATOR, binds its slot ``value'' to the local
   symbol macro ``$value'' and its ``wrapping-mode'' to
   ``$wrapping-mode'', evaluates the BODY forms, and returns the
   desinent form's results."
  (let ((evaluated-accumulator (gensym)))
    (declare (type symbol evaluated-accumulator))
    `(let ((,evaluated-accumulator ,accumulator))
       (declare (type Accumulator ,evaluated-accumulator))
       (declare (ignorable        ,evaluated-accumulator))
       (symbol-macrolet
           (($value
             (the integer
               (slot-value ,evaluated-accumulator 'value)))
            ($wrapping-mode
             (the wrapping-mode
               (slot-value ,evaluated-accumulator 'wrapping-mode))))
         (declare (type integer       $value))
         (declare (ignorable          $value))
         (declare (type wrapping-mode $wrapping-mode))
         (declare (ignorable          $wrapping-mode))
         ,@body))))

;;; -------------------------------------------------------

(defun accumulator-value (accumulator)
  "Returns the unsigned byte or signed integer datum stored in the
   ACCUMULATOR."
  (declare (type Accumulator accumulator))
  (with-accumulator (accumulator)
    (the integer $value)))

;;; -------------------------------------------------------

(defun (setf accumulator-value) (new-value accumulator)
  "Stores the NEW-VALUE in the ACCUMULATOR, contingently preceded by
   a wrapping around of its state in the unsigned byte range [0, 255],
   if prescribed by the currently governing wrapping mode, in any case
   returning no value."
  (declare (type Accumulator accumulator))
  (with-accumulator (accumulator)
    (case $wrapping-mode
      (:on  (setf $value (mod new-value 256)))
      (:off (setf $value      new-value))
      (otherwise
        (error "Invalid accumulator wrapping mode: ~s."
          $wrapping-mode))))
  (values))

;;; -------------------------------------------------------

(defun toggle-accumulator-wrapping-mode (accumulator)
  "Switches the ACCUMULATOR's wrapping mode to its opposite state and
   returns no value."
  (declare (type Accumulator accumulator))
  (with-accumulator (accumulator)
    (setf $wrapping-mode
      (get-next-wrapping-mode $wrapping-mode)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of random number operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-random-number-generator ()
  "Initializes the random number generator and returns no value."
  (setf *random-state*
    (make-random-state T))
  (values))

;;; -------------------------------------------------------

(defun select-random-byte-value ()
  "Returns a random unsigned byte value desumed from the closed interval
   [0, 255]."
  (the (unsigned-byte 8)
    (random 256)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((code
    :initarg       :code
    :initform      (error "Missing source code for interpreter.")
    :type          simple-string
    :documentation "The piece of Useful brainfuck source code to
                    evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position.")
   (jump-table
    :initform      (prepare-empty-jump-table)
    :type          Jump-Table
    :documentation "A bidirection mapping betwixt the jump points.")
   (tape
    :initform      (make-pristine-tape)
    :type          Tape
    :documentation "A bilaterally infinite catena of cells either
                    comprehending unsigned bytes or signed integers.")
   (accumulator
    :initform      (make-accumulator)
    :type          Accumulator
    :documentation "A scalar salvatory for an unsigned byte or signed
                    integer datum.")
   (wrapping-mode
    :initform      :on
    :type          wrapping-mode
    :documentation "Determines whether the interpreter's tape and
                    accumulator shall merely admit and produce unsigned
                    byte values, or extend their capacity to signed
                    integers of any mickleness their magnitude:
                      -----------------------------------------------
                      Mode | Causatum
                      -----+-----------------------------------------
                      :on  | The tape and the accumulator may contain
                           | only unsigned byte objects, commorant in
                           | the closed range [0, 255], and wrapping
                           | around along its bournes upon a
                           | transgression.
                           |-----------------------------------------
                           | This establishes the default state.
                      ...............................................
                      :off | The tape and the accumulator may contain
                           | any integer value.
                      -----------------------------------------------"))
  (:documentation
    "The ``Interpreter'' class serves in the adhibition of actual
     efficacy to a Useful brainfuck program delivered as a string of
     symbols."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``code'' to the local
   symbol macro ``$code'', its ``ip'' to ``$ip'', the ``jump-table'' to
   ``$jump-table'', the ``tape'' to ``$tape'', the ``accumulator'' to
   ``$accumulator'', and the ``wrapping-mode'' to ``$wrapping-mode'',
   evaluates the BODY forms, and returns the desinent form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($code
             (the simple-string
               (slot-value interpreter 'code)))
            ($ip
             (the fixnum
               (slot-value interpreter 'ip)))
            ($jump-table
             (the Jump-Table
               (slot-value interpreter 'jump-table)))
            ($tape
             (the Tape
               (slot-value interpreter 'tape)))
            ($accumulator
             (the Accumulator
               (slot-value interpreter 'accumulator)))
            ($wrapping-mode
             (the wrapping-mode
               (slot-value interpreter 'wrapping-mode))))
         (declare (type simple-string $code)
                  (ignorable          $code))
         (declare (type fixnum        $ip)
                  (ignorable          $ip))
         (declare (type Jump-Table    $jump-table)
                  (ignorable          $jump-table))
         (declare (type Tape          $tape)
                  (ignorable          $tape))
         (declare (type Accumulator   $accumulator)
                  (ignorable          $accumulator))
         (declare (type wrapping-mode $wrapping-mode)
                  (ignorable          $wrapping-mode))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Populates the jump table consigned to the INTERPRETER's castaldy with
   its underlying Useful brainfuck source code's comprehended jump
   points and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (supputate-jump-points-for $jump-table $code))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (code)
  "Creates and returns a fresh ``Interpreter'' whose dedication is
   airted towards the piece of Useful brainfuck source CODE's
   execution."
  (declare (type string code))
  (the Interpreter
    (make-instance 'Interpreter
      :code (coerce code 'simple-string))))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the INTERPRETER's internally managed Useful
   brainfuck program has been executed to its fullest patration,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (the boolean
      (get-boolean-value-of
        (>= $ip (length $code))))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its underlying Useful brainfuck program, if possible, and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $ip
      (min (1+ $ip)
        (length $code))))
  (values))

;;; -------------------------------------------------------

(defun get-current-token (interpreter)
  "Returns the currently processed character in the INTERPRETER's
   underlying Useful brainfuck program."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (the character
      (schar $code $ip))))

;;; -------------------------------------------------------

(defgeneric process-token (interpreter token)
  (:documentation
    "Evaluates the Useful brainfuck TOKEN in the INTERPRETER's context
     and returns no value.")
  
  (:method ((interpreter Interpreter) (token character))
    (declare (type Interpreter interpreter)
             (ignore           interpreter))
    (declare (type character   token)
             (ignore           token))
    (values)))

;;; -------------------------------------------------------

(defmacro define-token-processor (token (interpreter-name) &body body)
  "Defines an implementation of the generic function ``process-token'',
   its first formal parameter nevened by the INTERPRETER-NAME, the
   second an automatically generated agnomination's recipient,
   ``eql''-generalizing on the TOKEN character, wraps the BODY forms in
   a ``with-interpreter'' invocation employing the INTERPRETER-NAME,
   evaluates the BODY forms, and returns no value."
  (let ((token-name (gensym)))
    (declare (type symbol token-name))
    `(defmethod process-token ((,interpreter-name Interpreter)
                               (,token-name       (eql ,token)))
       (declare (type Interpreter ,interpreter-name)
                (ignorable        ,interpreter-name))
       (declare (type character   ,token-name)
                (ignore           ,token-name))
       (with-interpreter (,interpreter-name)
         ,@body)
       (values))))

;;; -------------------------------------------------------

(define-token-processor #\> (interpreter)
  (move-cell-pointer-right $tape))

;;; -------------------------------------------------------

(define-token-processor #\< (interpreter)
  (move-cell-pointer-left $tape))

;;; -------------------------------------------------------

(define-token-processor #\+ (interpreter)
  (incf (current-cell-value $tape)))

;;; -------------------------------------------------------

(define-token-processor #\- (interpreter)
  (decf (current-cell-value $tape)))

;;; -------------------------------------------------------

(define-token-processor #\, (interpreter)
  (format T "~&Please input a character: ")
  (finish-output)
  (setf (current-cell-value $tape)
    (char-code
      (read-char NIL NIL #\Null)))
  (clear-input))

;;; -------------------------------------------------------

(define-token-processor #\. (interpreter)
  (format T "~c"
    (code-char
      (current-cell-value $tape))))

;;; -------------------------------------------------------

(define-token-processor #\[ (interpreter)
  (when (current-cell-contains-zero-p $tape)
    (setf $ip
      (locate-jump-destination $jump-table $ip))))

;;; -------------------------------------------------------

(define-token-processor #\] (interpreter)
  (unless (current-cell-contains-zero-p $tape)
    (setf $ip
      (locate-jump-destination $jump-table $ip))))

;;; -------------------------------------------------------

(define-token-processor #\( (interpreter)
  (setf (accumulator-value $accumulator)
    (current-cell-value $tape)))

;;; -------------------------------------------------------

(define-token-processor #\) (interpreter)
  (setf (current-cell-value $tape)
    (accumulator-value $accumulator)))

;;; -------------------------------------------------------

(define-token-processor #\0 (interpreter)
  (setf (accumulator-value $accumulator) 0))

;;; -------------------------------------------------------

(define-token-processor #\? (interpreter)
  (setf (accumulator-value $accumulator)
    (select-random-byte-value)))

;;; -------------------------------------------------------

(define-token-processor #\{ (interpreter)
  (move-cell-pointer-left $tape
    (accumulator-value $accumulator)))

;;; -------------------------------------------------------

(define-token-processor #\} (interpreter)
  (move-cell-pointer-right $tape
    (accumulator-value $accumulator)))

;;; -------------------------------------------------------

(define-token-processor #\i (interpreter)
  (incf (accumulator-value $accumulator) 1))

;;; -------------------------------------------------------

(define-token-processor #\d (interpreter)
  (decf (accumulator-value $accumulator) 1))

;;; -------------------------------------------------------

(define-token-processor #\I (interpreter)
  (incf (accumulator-value $accumulator) 10))

;;; -------------------------------------------------------

(define-token-processor #\D (interpreter)
  (decf (accumulator-value $accumulator) 10))

;;; -------------------------------------------------------

(define-token-processor #\* (interpreter)
  (setf (accumulator-value $accumulator)
    (* (accumulator-value $accumulator)
       (accumulator-value $accumulator))))

;;; -------------------------------------------------------

(define-token-processor #\/ (interpreter)
  (setf (accumulator-value $accumulator)
    (round (accumulator-value $accumulator) 2)))

;;; -------------------------------------------------------

(define-token-processor #\a (interpreter)
  (incf (accumulator-value $accumulator)
    (current-cell-value $tape)))

;;; -------------------------------------------------------

(define-token-processor #\A (interpreter)
  (incf (current-cell-value $tape)
    (accumulator-value $accumulator)))

;;; -------------------------------------------------------

(define-token-processor #\; (interpreter)
  (format T "~&Please enter a number: ")
  (finish-output)
  (setf (current-cell-value $tape)
    (parse-integer
      (read-line NIL NIL "0")))
  (clear-input))

;;; -------------------------------------------------------

(define-token-processor #\: (interpreter)
  (format T "~&~d"
    (current-cell-value $tape)))

;;; -------------------------------------------------------

(define-token-processor #\' (interpreter)
  (setf $wrapping-mode
    (get-next-wrapping-mode $wrapping-mode))
  (toggle-tape-wrapping-mode        $tape)
  (toggle-accumulator-wrapping-mode $accumulator))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Useful brainfuck program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-token interpreter
      (get-current-token interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Useful-brainfuck (code)
  "Interprets the piece of Useful brainfuck source CODE and returns no
   value."
  (declare (type string code))
  (execute-program
    (make-interpreter code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The "A + B problem" employing the accumulator.
(interpret-Useful-brainfuck ";(;A:")

;;; -------------------------------------------------------

;; The "A + B problem" employing only the tape, desisting from the
;; accumulator's involvement.
(interpret-Useful-brainfuck ";>;<[>+<-]>:")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-Useful-brainfuck
  "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.")

;;; -------------------------------------------------------

;; Repeating character-based cat program which terminates on a
;; "null character" reception.
(interpret-Useful-brainfuck ",.[,.]")

;;; -------------------------------------------------------

;; Repeating numeric cat program which terminates on a zero (0) integer
;; input.
(interpret-Useful-brainfuck "';:[;:]")

;;; -------------------------------------------------------

;; Truth-machine which harnesses the numeric input and output conduits.
(interpret-Useful-brainfuck ";[:]:")

;;; -------------------------------------------------------

;; Looping counter.
;; 
;; Concept:
;;   toggle wrap mode to "off"
;;   
;;   tape[0] <- numeric input   { Number of lines to print.        }
;;   tape[1] <- 0               { Asterisk ("*") counter.          }
;;   tape[2] <- 42              { ASCII code of "*".               }
;;   tape[3] <- 10              { ASCII code of newline character. }
;;   
;;   while tape[0] != 0 do
;;     { Increase number of asterisks ("*") per line. }
;;     accumulator <- accumulator + 1
;;     tape[1] <- accumulator
;;     
;;     { Print asterisks ("*"). }
;;     while tape[1] != 0 do
;;       print tape[2] as character
;;       tape[1] <- tape[1] - 1
;;     end while
;;     
;;     { Print newline. }
;;     print tape[3] as character
;;     tape[0] <- tape[0] - 1
;;   end while
;; 
(interpret-Useful-brainfuck
  "'
   ;
   >
   >++++++++++++++++++++++++++++++++++++++++++
   >++++++++++
   <<<
   [
     i
     >)
     [
       >.
       <-
     ]
     >>.
     <<<-
   ]
  ")

;;; -------------------------------------------------------

;; Looping counter which homologates the specification of the character
;; employed in the lines' limning by the user.
;; 
;; Concept:
;;   toggle wrap mode to "off"
;;   
;;   tape[0] <- numeric input   { Number of lines to print.        }
;;   tape[1] <- 0               { Asterisk ("*") counter.          }
;;   tape[2] <- character input { Character to print on lines.     }
;;   tape[3] <- 10              { ASCII code of newline character. }
;;   
;;   while tape[0] != 0 do
;;     { Increase number of characters to print per line. }
;;     accumulator <- accumulator + 1
;;     tape[1] <- accumulator
;;     
;;     { Print user-defined character. }
;;     while tape[1] != 0 do
;;       print tape[2] as character
;;       tape[1] <- tape[1] - 1
;;     end while
;;     
;;     { Print newline. }
;;     print tape[3] as character
;;     tape[0] <- tape[0] - 1
;;   end while
;; 
(interpret-Useful-brainfuck
  "'
   ;
   >
   >,
   >++++++++++
   <<<
   [
     i
     >)
     [
       >.
       <-
     ]
     >>.
     <<<-
   ]
  ")
