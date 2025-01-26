;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "WrongFuck", invented by the Esolang user "EvyLah" and
;; presented on May 29th, 2024, its kenspeckle contribution a derivation
;; of Urban Mueller's language "brainfuck" with the aefauld discrepancy
;; woning in the octuple instruction symbols' affiliation with the
;; actuated causata.
;; 
;; 
;; Concept
;; =======
;; The WrongFuck programming language constitutes a derivative of
;; brainfuck founded upon the confoundment of the stock-father's
;; effective symbols by a parcery of these characters to purposes
;; discrepant from the cleronomy's formulations.
;; 
;; == THE MEMORY: AN INFINITE SEQUENCE OF BYTES ==
;; Enjoying a paregal foundment as its entheus, WrongFuck emplys a
;; bilaterally infinite dispansion of unsigned byte-valued cells, each
;; such a scalar element's salvatory, which occupies the integral range
;; of [0, 255].
;; 
;; The participation of operations entalented with the capacity to
;; modulate a cell's state vindicate the wrapping behavior of its
;; diorism: Upon an incrementation aboon its upper extremum of 255, the
;; value overflows to the minimum of zero (0); in an athwart airt, the
;; lower bourne's transgression, submerging alow the minimum of zero
;; (0), eventuates a continuance at the upper march of 255.
;; 
;; Operating upon this catena of numbers, a "cell pointer" is allotted
;; the dever of the currently active cell's castaldy, thilk represents
;; the sole member at any instant ostending an amenability to
;; perquisitions and modifications. Its motile nature serves in this
;; pointer's capacitation to gradually traverse the memory along both
;; of its axes.
;; 
;; 
;; Instructions
;; ============
;; brainfuck's dation extends to its operative aspect's entirety,
;; investing WrongFuck with an equipollence in its octuple instruction
;; set --- albeit divergent in the chosen agnominations.
;; 
;; == OVERVIEW ==
;; The following apercu's onus shall be a cursory ilk of nortelry's
;; adhibition with respect to the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Increments the current cell value by one (1). If the new
;;           | state exceeds the upper march of 255, the value wraps
;;           | around to the lower extremum of zero (0).
;;   ..................................................................
;;   <       | Decrements the current cell value by one (1). If the new
;;           | state descends alow the minimum of zero (0), the value
;;           | wraps around to the upper extremum of 255.
;;   ..................................................................
;;   ]       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   [       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   +       | Prints the character whose ASCII code concurs with the
;;           | current cell value to the standard output.
;;   ..................................................................
;;   -       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   ,       | If the current cell contains the value zero (0), moves
;;           | the instruction pointer (IP) forward to the position of
;;           | the matching "." token; otherwise proceeds as usual.
;;   ..................................................................
;;   .       | If the current cell does not contain the value zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | of the matching "," token; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == WRONGFUCK AND BRAINFUCK ==
;; The aforementioned equipollent diorisms in all bailiwicks of
;; WrongFuck and its inspiration brainfuck homologate an equiparation of
;; their symbols:
;; 
;;   ----------------------
;;   WrongFuck | brainfuck
;;   ----------+-----------
;;   >         | +
;;   ......................
;;   <         | -
;;   ......................
;;   ]         | >
;;   ......................
;;   [         | <
;;   ......................
;;   ,         | [
;;   ......................
;;   .         | ]
;;   ......................
;;   +         | .
;;   ......................
;;   -         | ,
;;   ----------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort in the
;; language Common Lisp, the causata's gendrure produced immediately on
;; the provided WrongFuck source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-25
;; 
;; Sources:
;;   [esolang2024WrongFuck]
;;   The Esolang contributors, "WrongFuck", May 29th, 2024
;;   URL: "https://esolangs.org/wiki/WrongFuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which amplects, without the claim of the
   contingency's exhaustion, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, the gamut's logical edification from this
   specification accounting for the closed interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a value of the VALUE-TYPE, for both is specified the comprehensive
   ``T'' as the default."
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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list that enumerates zero or more
   members, everichon among these conforming to the ELEMENT-TYPE, for
   thilk holds the comprehensive ``T'' as a default."
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

(deftype sparse-byte-vector ()
  "The ``sparse-byte-vector'' type defines a sparse one-dimensional
   array of unsigned byte values with an infinite expansion as a hash
   table whose keys assume signed integer numbers, simulating the array
   subscripts, while the values comprehend the ``octet'' data."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype position-mapping ()
  "The ``position-mapping'' type defines an association betwixt the
   forward and back jump instructions in a WrongFuck program by
   adminiculum of their zero-based indices into the source code, its
   reification elicited from a hash table whose keys and values encode
   both the provenance and the destination as ``fixnum'' objects."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\"
   datum and whence produces a veridicous Boolean tantamount, returning
   for a non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for
   the ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of translator from brainfuck to WrongFuck.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-WrongFuck (brainfuck-code
                                         &optional (destination NIL))
  "Transliterates the BRAINFUCK-CODE to the equivalent WrongFuck
   program, writes the resulting code to the DESTINATION, and returns
   for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise produces
   and responds with a fresh string comprehending the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (flet ((write-WrongFuck-command (wrongFuck-token)
              "Writes the WRONGFUCK-TOKEN to the DESTINATION and returns
               no value."
              (declare (type character wrongFuck-token))
              (format destination "~c" wrongFuck-token)
              (values)))
        (loop
          for brainfuck-token of-type character across brainfuck-code
          do
            (case brainfuck-token
              (#\+       (write-WrongFuck-command #\>))
              (#\-       (write-WrongFuck-command #\<))
              (#\>       (write-WrongFuck-command #\]))
              (#\<       (write-WrongFuck-command #\[))
              (#\[       (write-WrongFuck-command #\,))
              (#\]       (write-WrongFuck-command #\.))
              (#\.       (write-WrongFuck-command #\+))
              (#\,       (write-WrongFuck-command #\-))
              (otherwise NIL))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-brainfuck-to-WrongFuck brainfuck-code output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of translator from WrongFuck to brainfuck.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-WrongFuck-to-brainfuck (wrongFuck-code
                                         &optional (destination NIL))
  "Transliterates the piece of WRONGFUCK-CODE to its brainfuck
   equivalent, writes the resulting program to the DESTINATION, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, responds with a fresh string comprehending
   the output."
  (declare (type string      wrongFuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (flet ((write-brainfuck-command (brainfuck-token)
              "Writes the BRAINFUCK-TOKEN to the DESTINATION and returns
               no value."
              (declare (type character brainfuck-token))
              (format destination "~c" brainfuck-token)
              (values)))
        (loop
          for wrongFuck-token of-type character across wrongFuck-code
          do
            (case wrongFuck-token
              (#\>       (write-brainfuck-command #\+))
              (#\<       (write-brainfuck-command #\-))
              (#\]       (write-brainfuck-command #\>))
              (#\[       (write-brainfuck-command #\<))
              (#\,       (write-brainfuck-command #\[))
              (#\.       (write-brainfuck-command #\]))
              (#\+       (write-brainfuck-command #\.))
              (#\-       (write-brainfuck-command #\,))
              (otherwise NIL))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-WrongFuck-to-brainfuck wrongFuck-code output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          sparse-byte-vector
    :documentation "A sparse vector of bytes, the keys provide the
                    indices, answering with unsigned byte values.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The current index, or key, into the CELLS table."))
  (:documentation
    "The ``Memory'' class serves in the provision of the WrongFuck
     program memory as a bilaterally infinite dispansion of unsigned
     byte-valued cells, operated upon by a cell pointer which at any
     instant selects the currently responsive member among these."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a fresh ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the unsigned byte value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type sparse-byte-vector cells))
    (declare (type integer            pointer))
    (the octet
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (cells pointer) memory
    (declare (type sparse-byte-vector cells))
    (declare (type integer            pointer))
    (setf (gethash pointer cells 0)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (memory)
  "Determines whether the MEMORY's current cell comprehends the value
   zero (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.."
  (declare (type Memory memory))
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-cell-value memory)))))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (with-slots (pointer) memory
    (declare (type integer pointer))
    (decf pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (with-slots (pointer) memory
    (declare (type integer pointer))
    (incf pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((entries
    :initform      (make-hash-table :test #'eql)
    :type          position-mapping
    :documentation "Maps the forward and back jump instructions by their
                    zero-based positions' adminiculum in the underlying
                    WrongFuck program."))
  (:documentation
    "The ``Jump-Table'' class serves in the realization of a bilateral
     association betwixt jump points in a WrongFuck program, mediated by
     their zero-based indices into the same."))

;;; -------------------------------------------------------

(defun make-empty-jump-table ()
  "Creates and returns a fresh and vacant ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Affiliates the START-POINT and END-POINT in the JUMP-TABLE in a
   bilateral fashion and returns no valeu."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (with-slots (entries) jump-table
    (declare (type position-mapping entries))
    (psetf (gethash start-point entries) end-point
           (gethash end-point   entries) start-point))
  (values))

;;; -------------------------------------------------------

(defun supputate-jump-table-for (wrongFuck-code)
  "Builds and returns a fresh ``Jump-Table'' for the piece of
   WRONGFUCK-CODE, ligating its forward and back jump instructions by
   mediation of their zero-based indices into the program."
  (declare (type string wrongFuck-code))
  (let ((jump-table   (make-empty-jump-table))
        (start-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for token    of-type character across wrongFuck-code
      and position of-type fixnum    from   0 by 1
      
      if (char= token #\,) do
        (push position start-points)
      else if (char= token #\.) do
        (if start-points
          (connect-jump-points jump-table
            (pop start-points)
            position)
          (error "Unmatched back jump instruction at position ~d."
            position))
      end
      
      finally
        (when start-points
          (error "Unmatched forward jump instruction~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points) start-points)))
    (the Jump-Table jump-table)))

;;; -------------------------------------------------------

(defun query-jump-destination (jump-table source-point)
  "Returns the athwart jump point to the SOURCE-POINT in the JUMP-TABLE;
   or signals an error of an unspecified type upon its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     source-point))
  (the fixnum
    (with-slots (entries) jump-table
      (declare (type position-mapping entries))
      (or (gethash source-point entries)
          (error "No jump destination associated with the position ~d."
            source-point)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-WrongFuck (code)
  "Interprets the piece of WrongFuck source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (supputate-jump-table-for code))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (declare (type Memory     memory))
    (symbol-macrolet
        ((program-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length code)))))
         (current-token
          (the character
            (char code ip))))
      (declare (type boolean   program-completed-p))
      (declare (type character current-token))
      (loop until program-completed-p do
        (case current-token
          (#\>
            (incf (current-cell-value memory)))
          (#\<
            (decf (current-cell-value memory)))
          (#\]
            (move-cell-pointer-right memory))
          (#\[
            (move-cell-pointer-left memory))
          (#\,
            (when (current-cell-contains-zero-p memory)
              (setf ip
                (query-jump-destination jump-table ip))))
          (#\.
            (unless (current-cell-contains-zero-p memory)
              (setf ip
                (query-jump-destination jump-table ip))))
          (#\+
            (write-char
              (code-char
                (current-cell-value memory))))
          (#\-
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell-value memory)
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          (otherwise
            NIL))
        (incf ip))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program.
;; 
;; This constitutes a tantamount to the brainfuck program
;;   ,.[,.]
(interpret-WrongFuck "-+,-+.")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; This program constitutes a transliteration of brainfuck's
;;   ,.[-->+[>>]<[.]<<]
(interpret-WrongFuck "-+,<<]>,]].[,+.[[.")
