;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brainfucker", invented by the Esolang user "Tommyaweosme"
;; and presented on June 10th, 2024, founded upon the substratum
;; vouchsafed by Urban Mueller's "brainfuck", the operative ogdoad's
;; enker designment experiencing an elevation to a duodecimal accompt,
;; extending the arithmetic capabilities, transforming the input and
;; output conduits to numeric forms, and extending the one-dimensional
;; unsigned byte catena to a two-dimensional reseau of signed integer
;; cells which weets of no tholance anenst their mickleness along both
;; signs.
;; 
;; 
;; Concept
;; =======
;; The Brainfucker programming language's firmament is such of
;; brainfuck's basic dation, the pernancy's dioristic transmogrification
;; effort dimidiating into a restriction of the human-machine
;; intercourse to numeric tokens of currency only, this novelty rendered
;; enhaused by further arithmetic capabilities, and a two-dimensional
;; memory model the same wists of no natural bournes in its signed
;; integer cell.
;; 
;; == INTEGER NUMBERS: THE TOKENS OF CURRENCY ==
;; The aefauld species of significant agency in the language emerges in
;; the guise of signed integer numbers, theirs a disencumberance from
;; any impositions concerning the mickleness.
;; 
;; This paravaunt rank's affedavit may be recovered in both the commerce
;; athwart the input and output conduits, as well as the two-dimensional
;; memory grid's constitution; ensuing from this consideration, a twain
;; of supererogations anent the arithmetic bailiwick's capacitation is
;; accommodated, as juxtaposed with the brainfuck stock-father's less
;; magnanimous dations.
;; 
;; == THE MEMORY: A TWO-DIMENSIONAL GRID OF SIGNED INTEGERS ==
;; One of the dioristic ostensions of the language appertains to the
;; memory's plasmature, such wists of a reseau of signed integer numbers
;; of no natural marches along both polarities.
;; 
;; Infinite in its dispansion athwart the horizontal as well as vertical
;; airts, a cell pointer is empight on an incipial entity; its
;; vouchsafement of a nature that involves an allative parlance to its
;; existency serves in the homologation of per gradus progressions
;; into all four directions.
;; 
;; 
;; Instructions
;; ============
;; A duodecimal accompt of capabilities extends its purview over the
;; Brainfucker instruction set's entirety, its bailiwicks a tesseratomy
;; into rudimentary arithmetics, input and output communications,
;; warklumes for the navigation athwart the two-dimensional memory, as
;; well as an aefauld control flow duction mechanism.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be realized in a sufficient mete
;; of gnarity's dation concerning the language's operative faculties:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   ARITHMETICS
;;   ------------------------------------------------------------------
;;   +       | Increments the current cell value by one (1).
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1).
;;   ..................................................................
;;   *       | Multiplies the current cell value by itself.
;;   ..................................................................
;;   /       | Supputates the integer square root of the current cell
;;           | value and replaces the same by the result.
;;           |---------------------------------------------------------
;;           | The actual causatum remains a thing of unspecified
;;           | deportment upon this operation's application on a cell
;;           | comprehending a negative integer number.
;;   ==================================================================
;;   INPUT/OUTPUT COMMUNICATION
;;   ------------------------------------------------------------------
;;   .       | Prints the current cell value in its verbatim numeric
;;           | form to the standard output conduit.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a signed or
;;           | unsigned integer number in its decimal representation
;;           | and stores thilk in the current cell.
;;   ==================================================================
;;   MEMORY MANAGEMENT
;;   ------------------------------------------------------------------
;;   >       | Translates the memory's cell pointer one step to the
;;           | right.
;;   ..................................................................
;;   <       | Translates the memory's cell pointer one step to the
;;           | left.
;;   ..................................................................
;;   ^       | Translates the memory's cell pointer one step upwards.
;;   ..................................................................
;;   v       | Translates the memory's cell pointer one step downwards.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" token; otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) backward to the position
;;           | immediately succeeding the matching "[" token; otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter, its entelechia an effort peracted in the
;; programming language Common Lisp, avaunts in a per saltum application
;; of the causata on the Brainfucker source code.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-20
;; 
;; Sources:
;;   [esolang2025:Brainfucker]
;;   The Esolang contributors, "Brainfucker", November 27th, 2025
;;   URL: "https://esolangs.org/wiki/Brainfucker"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon zero or
   more entries, everichon among these a dimidiations into a key,
   subsumed into the KEY-TYPE, and a value, adhering to the VALUE-TYPE,
   both defaulting to the generic sentinel ``*''."
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
                (and
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a linked list edified upon a componency
   enumerating zero or more elements of the ELEMENT-TYPE, for which is
   imposed the default configuration involving the generic sentinel
   ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional affiliation atwixen
   the jump points in a Brainfucker program, mediated per procurationem
   of their zero-based positions into the source code, its manifestation
   that of a hash table whose keys and values both assume ``fixnum''
   objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype location ()
  "The ``location'' type defines a two-dimensional Cartesian coordinates
   jumelle in the guise of a complex number composed of integral
   constituents or a scalar integer number, the former case a mold
   inwith whose diorism the real part contributes the x-coordinate,
   complemented by the imaginary part's reappropriation for the
   y-coordinate, while the scalar variant serves only in the explicit
   castaldy of the abscissa, with the moeity appertaining the ordinate
   is construed to limn an owelty to the zero (0).
   ---
   The equiparation betwixt the concretely chosen numeric data type and
   its interpretation shall be the following tabulation's cynosure:
     -------------------------------------------------------
     Numeric representation | x-coordinate | y-coordinate
     -----------------------+--------------+----------------
     complex                | real part    | imaginary part
     .......................................................
     integer                | value        | 0
     -------------------------------------------------------"
  '(or integer (complex integer)))

;;; -------------------------------------------------------

(deftype cell-matrix ()
  "The ``cell-matrix'' type defines a sparse two-dimensional
   reticulation of signed integer cells, realized as a hash table whose
   subscripts assume ``location'' specifiers, answering with ``integer''
   values as the matrix entries."
  '(hash-table-of location integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supputate-the-jump-table-for (code)
  "Generates and returns for the piece of Brainfucker source CODE the
   jump table, which serves in the alligation of its jump points in a
   bilateral fashion, mediated by adminiculum of their zero-based
   indices into the program."
  (declare (type simple-string code))
  (let ((connections  (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table       connections))
    (declare (type (list-of fixnum) start-points))
    (loop
      for current-token    of-type character across code
      and current-position of-type fixnum    from   0 by 1
      if (char= current-token #\[) do
        (push current-position start-points)
      else if (char= current-token #\]) do
        (if start-points
          (let ((start-point (pop start-points))
                (end-point   current-position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf
              (gethash start-point connections) end-point
              (gethash end-point   connections) start-point))
          (error "No matching forward jump point could be detected ~
                  for the \"]\" instruction at position ~d in the ~
                  source code."
            current-position))
      end
      finally
        (if start-points
          (let ((number-of-unmatched-points (length start-points)))
            (declare (type fixnum number-of-unmatched-points))
            (error "The forward jump point~p at the position~:p ~
                    ~{~d~^, ~} lack~:[~;s~] a matching back jump ~
                    instruction."
              number-of-unmatched-points
              (nreverse start-points)
              (<= number-of-unmatched-points 1)))
          (return connections)))))

;;; -------------------------------------------------------

(defun locate-the-destination-jump-point (connections
                                          point-of-departure)
  "Returns the jump point reached by the POINT-OF-DEPARTURE, as defined
   in the CONNECTIONS table; or, upon its disrespondency, signals an
   error of an unspecified type."
  (declare (type jump-table connections))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure connections)
        (error "No destination jump point exists for the position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program memory.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :accessor      memory-cells
    :type          cell-matrix
    :documentation "A sparse two-dimensional array of signed integer
                    numbers, amenable to biscalar subscripts.")
   (pointer
    :initform      (complex 0 0)
    :accessor      memory-pointer
    :type          location
    :documentation "The x-y-coordinate of the currently selected entry
                    among the CELLS, its diorism either ensuing from the
                    complex (x, y), or the integer scalar construed as
                    (x, 0)."))
  (:documentation
    "The ``Memory'' class applies itself to the vouchsafement of a
     two-dimensional Cartesian reticulation whose entities are edified
     upon cells, everichon partaking in this salvatory a scalar integer
     datum's woning, the numeric content wisting of bournes along both
     axes."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-memory ()
  "Creates and returns a fresh ``Memory'' empight during its inchoacy
   in the default state comprehending merely zero-valued cells."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun translate-the-cell-pointer (memory x-offset y-offset)
  "Relocates the MEMORY's cell pointer relative to its current location
   by the X-OFFSET along the horizontal and the Y-OFFSET along the
   vertical axis and returns no value."
  (declare (type Memory          memory))
  (declare (type (integer -1 +1) x-offset))
  (declare (type (integer -1 +1) y-offset))
  (setf (memory-pointer memory)
    (+ (memory-pointer memory)
       (complex x-offset y-offset)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the signed integer number maintained by the MEMORY's
   currently selected cell."
  (declare (type Memory memory))
  (the integer
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's currently selected cell and
   returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)
    new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the arithmetic operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (number)
  "Returns the product of the NUMBER multplied by itself."
  (declare (type integer number))
  (the integer
    (* number number)))

;;; -------------------------------------------------------

(define-modify-macro square-in-place ()
  square
  "Multiplies its first argument, expecting the same to represent a
   place, by itself, stores the product in the argument, and returns
   this product.")

;;; -------------------------------------------------------

(defun square-root-of (number)
  "Supputates and returns the square root of the NUMBER."
  (declare (type integer number))
  (the (integer 0 *)
    (isqrt number)))

;;; -------------------------------------------------------

(define-modify-macro square-root-in-place ()
  square-root-of
  "Supputates the square root of its first argument, expecting the same
   to represent a place, stores the value in the argument, and returns
   this result.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-brainfucker-code
    (code
     &aux (optimized-code (coerce code 'simple-string)))
  "Interprets the piece of Brainfucker source CODE and returns no
   value."
  (declare (type string        code))
  (declare (type simple-string optimized-code))
  (let ((ip         0)
        (jump-table (supputate-the-jump-table-for code))
        (memory     (prepare-a-pristine-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (loop while (< ip (length optimized-code)) do
      (case (schar optimized-code ip)
        (#\+
          (incf (current-cell-value memory)))
        (#\-
          (decf (current-cell-value memory)))
        (#\*
          (square-in-place
            (current-cell-value memory)))
        (#\/
          (square-root-in-place
            (current-cell-value memory)))
        (#\.
          (format T "~&~d"
            (current-cell-value memory)))
        (#\,
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell-value memory)
            (parse-integer
              (read-line NIL NIL "0")))
          (clear-input))
        (#\[
          (when (zerop (current-cell-value memory))
            (setf ip
              (locate-the-destination-jump-point jump-table ip))))
        (#\]
          (unless (zerop (current-cell-value memory))
            (setf ip
              (locate-the-destination-jump-point jump-table ip))))
        (#\>
          (translate-the-cell-pointer memory 1 0))
        (#\<
          (translate-the-cell-pointer memory -1 0))
        (#\v
          (translate-the-cell-pointer memory 0 +1))
        (#\^
          (translate-the-cell-pointer memory 0 -1))
        (otherwise
          NIL))
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the number 123 to the standard output conduit.
(interpret-the-brainfucker-code "+++*++*++.")

;;; -------------------------------------------------------

;; Print the first eight members of the series naiting squares:
;;   1, 2, 4 (= 2 * 2), 16 (= 4 * 4), 256 (= 16 * 16), ...
(interpret-the-brainfucker-code
  "++++++
   ^+.+.
   v[^*.v-]")

;;; -------------------------------------------------------

;; Repeating numeric cat program which terminates on a zero (0) input.
(interpret-the-brainfucker-code ",[.,]")

;;; -------------------------------------------------------

;; Query the standard input for a number and print the same, in
;; conjunction with its integer square root.
(interpret-the-brainfucker-code ",./.")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-brainfucker-code ",[.].")
