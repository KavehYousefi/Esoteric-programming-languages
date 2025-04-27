;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ArrowFuck", invented by the Esolang user
;; "Mipinggfxgbtftybfhfyhfn" and presented on May 11th, 2019, its
;; conception that of a derivation of Urban Mueller's language
;; "brainfuck", with the scion's proprium commorant in the Cartesian
;; lacis of unsigned byte-valued cells which administers a dispansion
;; to the traditional one-dimensional catena into a second spatial
;; accommodation.
;; 
;; 
;; Concept
;; =======
;; The ArrowFuck programming language constitutes a derivative of
;; brainfuck, governed by lealty to its cleronomy to a mete of patration
;; except for the memory, which assumes a two-dimensional Cartesian
;; extension of the traditional byte-valued catena.
;; 
;; == THE PROGRAM MEMORY: A GRID OF CELLS ==
;; The dioristic attribute whose vouchsafement contributes the
;; provenance of its distinguishment from its entheus relates in
;; ArrowFuck to the memory tape's rearrangement in a Cartesian
;; reticulation, extending along two axis, scilicet, a traditional
;; horizontal and a novel vertical one.
;; 
;; == EACH CELL ENTAILS A SCALAR UNSIGNED BYTE ==
;; This grid's constituents manifest in unsigned byte-valued cells,
;; each meted in its capacity to the integral closed interval of
;; [0, 255].
;; 
;; If incremented ayond its highest value of 255, the state wraps around
;; to the lower extremum of zero (0). Siclike, upon a coerced descent
;; that infringes the minimum, the value relapses to the upper bourne
;; of 255.
;; 
;; == THE CELL POINTER DESIGNATES THE CURRENTLY ACTIVE CELL ==
;; A rather restrictive species of adit appertains to the memory access,
;; mediated by adminiculum of a specialized cursor, the "cell pointer",
;; which designates at any instant the currently active cell; this
;; specimen the aefauld entity entalented with the amenability to its
;; state's perquisitions of modulations.
;; 
;; The mobile nature participating in the cell pointer's haecceity
;; inherits the homologation for progressive navigations along both
;; tape axes, pursuing the telos of alteration in the active cell's
;; selection.
;; 
;; 
;; Instructions
;; ============
;; Its substratum a tantamount of its stock-father, ArrowFuck's
;; operative competences are valorized beyond the octuple firmament by
;; the necessity to accommodate a further tape dimension's negotation,
;; whence ensues the decimal cardinality.
;; 
;; == OVERVIEW ==
;; The following apercu's vindication shall be realized in a cursory
;; nortelry's adhibition regarding the instructive avails:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   ^       | Translates the cell pointer one step up.
;;   ..................................................................
;;   v       | Translates the cell pointer one step down.
;;   ..................................................................
;;   +       | Increments the current cell value by one (1). If the new
;;           | state transgresses the admissible upper bourne of 255,
;;           | the value wraps around the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1). If the new
;;           | state transgresses the admissible lower bourne of zero
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
;; This interpreter's implementation has been exercised in the
;; programming language Common Lisp, the mode of its operation
;; immediately targeting the source code in its string form.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-19
;; 
;; Sources:
;;   [esolang2022ArrowFuck]
;;   The Esolang contributors, "ArrowFuck", January 16th, 2022
;;   URL: "https://esolangs.org/wiki/ArrowFuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' OBJECT a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' input, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination is desumed from the
   TYPE-NAME, the formal parameters appropriated from the LAMBDA-LIST,
   and the subject of the docimasy enjoying a nevening by
   CANDIDATE-NAME, evaluates the BODY form with access to the both the
   parameters and the candidate identifier, and returns the desinent
   form's results, construing the primary value to communicate the
   result of this assessment, with a \"generalized boolean\" true output
   being tantamount to the type specifier's satisfaction, while false
   serves in the rejection's signification.
   ---
   The first BODY form, if amounting to a string object, is construed
   as the derived type's documentation string, and, as a corollary, is
   reappropriated for to align with this purpose."
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

;;; -------------------------------------------------------

(defun designates-any-type-p (type-specifier)
  "Determines whether the TYPE-SPECIFIER conflates with the generic
   sentinel ``*'', which signifies any type's admissibility, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T type-specifier))
  (the boolean
    (get-boolean-value-of
      (and (symbolp type-specifier)
           (eq      type-specifier '*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table inwith whose compass
   lies the woning of zero or more entries, everichon from this
   membership edified upon a key that subsumed into the KEY-TYPE and an
   allied value of the VALUE-TYPE, for both governs the generic sentinel
   ``*'' as a default."
  (and
    (hash-table-p candidate)
    (or
      (and (designates-any-type-p key-type)
           (designates-any-type-p value-type))
      (loop
        for current-key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value current-value)
        always
          (and
            (or (designates-any-type-p key-type)
                (typep current-key key-type))
            (or (designates-any-type-p value-type)
                (typep current-value value-type)))))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list with a conformation that accepts
   zero or more elements of the ELEMENT-TYPE, the default species
   signified by the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (designates-any-type-p element-type)
      (loop
        for    current-element of-type T in (the list candidate)
        always (typep current-element element-type)))))

;;; -------------------------------------------------------

(deftype location ()
  "The ``location'' type defines a two-dimensional Cartesian position as
   a tuple of x- and y-coordinates, molded into a cons cell, the first
   compartment of which replicates the x-axis, while the second acquires
   the status of the y-axis, both defined in terms of signed integer
   numbers."
  '(cons integer integer))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value whose edification
   constitutes a product of eight accolent bits, and whose gamut, as a
   consectary, registers the closed integral interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral association betwixt
   forward and back jump points in a ArrowFuck program, mediated by
   adminiculum of their zero-based indices into the same, and assuming
   the guise of a hash table whose keys and value both represent these
   locations as fixnum objects."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table-for (code)
  "Calculates and returns for the piece of ArrowFuck source CODE a
   jump table which connects its matching jump points in a bilateral
   fashion."
  (declare (type string code))
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    
    (loop
      for current-token    of-type character across code
      and current-position of-type fixnum    from   0 by 1
      
      if (char= current-token #\[) do
        (push current-position forward-jump-points)
      else if (char= current-token #\]) do
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
          (error "Unmatched back jump point at position ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            forward-jump-points)))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-destination-jump-point (jump-table point-of-departure)
  "Returns the jump point associated with the POINT-OF-DEPARTURE in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No matching destination jump point found for the ~
                position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'equal)
    :reader        tape-cells
    :type          (hash-table-of location octet)
    :documentation "A sparse vector of unsigned byte-valued cells,
                    amenable to x- and y-coordinates communicated in
                    specialized cons cells.")
   (pointer
    :initform      (cons 0 0)
    :accessor      tape-pointer
    :type          location
    :documentation "The cell pointer as a two-dimensional position
                    designator, realized as a cons cell as (x . y)
                    compound."))
  (:documentation
    "The ``Tape'' class serves in the modeling of the ArrowFuck program
     memory as a two-dimensional reticulation of unsigned byte-valued
     cells, extending along a horizontal and a vertical axis, and
     operated upon by a mobile cursor, the \"cell pointer\", whose
     bailiwick is fulfilled in the designation of the currently
     selected unit as the aefauld amenable entity at any instant during
     the interpreter's execution stage.
     ---
     Wisting of no natural bournes, this class' implementation resorts
     to a sparse species of deliberation, whence ensues as a natural
     choice a hash table, accommodating cons cells as its keys, each
     such bearing the x- and y-coordinates of a cell, and allying thilk
     with the cell states as the entry values."))

;;; -------------------------------------------------------

(defun prepare-pristine-tape ()
  "Creates and returns a fresh ``Tape'' whose entirety of cells assumes
   the default state of zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun copy-tape-pointer (tape)
  "Returns a fresh copy of the TAPE's cell pointer."
  (declare (type Tape tape))
  (the location
    (cons
      (car (tape-pointer tape))
      (cdr (tape-pointer tape)))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value ensconced in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (gethash
      (copy-tape-pointer tape)
      (tape-cells        tape)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping of its state in order to
   accommodate the valid byte bournes of [0, 255], and returns no
   value."
  (declare (type Tape tape))
  (setf
    (gethash
      (copy-tape-pointer tape)
      (tape-cells        tape)
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (car (tape-pointer tape)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (car (tape-pointer tape)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-up (tape)
  "Translates the TAPE's cell pointer one step up and returns no value."
  (declare (type Tape tape))
  (incf (cdr (tape-pointer tape)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-down (tape)
  "Translates the TAPE's cell pointer one step down and returns no
   value."
  (declare (type Tape tape))
  (decf (cdr (tape-pointer tape)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-ArrowFuck (code
                            &key (displays-prompt-p T))
  "Interprets the piece of ArrowFuck source CODE and returns no value.
   ---
   If the DISPLAYS-PROMPT-P flag resolves to ``NIL'', no prompt message
   is issued on the standard input conduit ere a datum's request;
   otherwise, the character sequence \">> \" furnishes a parasceve to
   the interaction."
  (declare (type string  code))
  (declare (type boolean displays-prompt-p))
  (let ((ip         0)
        (jump-table (build-jump-table-for code))
        (tape       (prepare-pristine-tape)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Tape       tape))
    (symbol-macrolet
        ((program-is-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length code)))))
         (current-token
          (the character
            (aref code ip))))
      (declare (type boolean   program-is-completed-p))
      (declare (type character current-token))
      (loop until program-is-completed-p do
        (case current-token
          (#\>
            (move-cell-pointer-right tape))
          
          (#\<
            (move-cell-pointer-left tape))
          
          (#\^
            (move-cell-pointer-up tape))
          
          (#\v
            (move-cell-pointer-down tape))
          
          (#\+
            (incf (current-cell-value tape)))
          
          (#\-
            (decf (current-cell-value tape)))
          
          (#\.
            (format T "~c"
              (code-char
                (current-cell-value tape))))
          
          (#\,
            (when displays-prompt-p
              (format T "~&>> "))
            (finish-output)
            (setf (current-cell-value tape)
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          
          (#\[
            (when (zerop (current-cell-value tape))
              (setf ip
                (get-destination-jump-point jump-table ip))))
          
          (#\]
            (unless (zerop (current-cell-value tape))
              (setf ip
                (get-destination-jump-point jump-table ip))))
          
          (otherwise
            NIL))
        
        (incf ip))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cat program which queries the user for three characters and prints
;; these in the reverse order by traversing the occupied cells in the
;; athwart airt.
(interpret-ArrowFuck
  ",v,v,
   .^.^.")

;;; -------------------------------------------------------

;; Query the standard input for characters until the "null character"
;; has been committed, storing each such in a separate cell by following
;; a downward-airted catena; subsequently, proceed in the widdershins
;; direction, printing these cells, and thus replicating the input in
;; its reverse form.
(with-input-from-string (user-input "ABC")
  (declare (type string-stream user-input))
  (let ((*standard-input* user-input))
    (interpret-ArrowFuck
      ",[v,]
       ^[.^]"
      :displays-prompt-p NIL)))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Arrowfuck ",.[--v+[vv]^[.]^^]")
