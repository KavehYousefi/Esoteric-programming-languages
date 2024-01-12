;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Threi", invented by the Esolang user "Infinitehexagon" and
;; presented on December 6th, 2023, the kenspeckle diorism of its
;; specimen resides in the operation of bits on an infinite tape.
;; 
;; 
;; Instructions
;; ============
;; Threi's instruction set is composed of an octuple membership, the
;; circumference of which amplects the manipulation of the bit-valued
;; tape cells, the cell pointer's translation, an output facility, as
;; well as a jump-based control flow mechanims.
;; 
;; == OVERVIEW ==
;; The following apercu shall serve in the adhibition of a foundational
;; mete of nortelry concerning the language's operational facilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   <       | Translates the cell pointer one step to the left.
;;           | If the cell pointer currently resides in the first cell,
;;           | no causatum is issued.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   h       | Inverts the current cell's bit.
;;   ..................................................................
;;   x       | Sets the current cell value to a random bit.
;;   ..................................................................
;;   e       | Resets the current cell value to zero (0).
;;   ..................................................................
;;   x       | If both the values of the current cell and of that
;;           | immediately to its left constitute one (1), sets the
;;           | cell three position to the current cell's right to the
;;           | bit one (1); otherwise exercises no effect.
;;   ..................................................................
;;   o       | Prints the current cell value to the standard output.
;;   ..................................................................
;;   {       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "}" token; otherwise
;;           | exercises no effect.
;;   ..................................................................
;;   }       | If the current cell value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "{" token; otherwise
;;           | exercises no effect.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-12
;; 
;; Sources:
;;   [esolang2023Threi]
;;   The Esolang contributors, "Threi", December 16th, 2023
;;   URL: "https://esolangs.org/wiki/Threi"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table the conformation of
   which refers to the KEY-TYPE for its keys and the VALUE-TYPE to
   adhere to the associated values, both by default assigned generic
   sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list the conformation of which refers
   to the ELEMENT-TYPE for its zero or more elements, by default
   established as the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (loop for element of-type T in (the list candidate) always
            (typep element element-type))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type affiliates the forward jump and back jump
   instructions in a bidirectional manner, expressed via their positions
   in the respective Threi program, and committed to a hash table's
   castaldy, the keys of which, as well as its values, produce fixnum
   objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type defines a location designator compatible with
   the valid bit vector index range."
  `(integer 0 ,array-total-size-limit))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of an octuple
   tally of accolent bits, whose value, as a corollary, wones in the
   closed interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   compass of its purview enumerating, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type jump-table        *jump-table*))
(declaim (type simple-bit-vector *tape*))
(declaim (type cell-index        *cell-pointer*))

;;; -------------------------------------------------------

(defparameter *jump-table*
  (make-hash-table :test #'eql)
  "Associates the forward jump (\"{\") and back jump (\"}\") commands in
   a bilateral fashion by mediation of their positions in the processed
   Threi program.")

(defparameter *tape*
  (make-array        0
    :element-type    'bit
    :initial-element 0
    :adjustable      NIL
    :fill-pointer    NIL)
  "Represents the program memory as a unilaterally infinite vector of
   bits.")

(defparameter *cell-pointer* 0
  "Designates the index of the current cell into the *TAPE* vector.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Configuration of random number generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *random-state* (make-random-state T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (code)
  "Computes the *JUMP-TABLE* for the piece of Threi source CODE and
   returns no value."
  (declare (type string code))
  (clrhash *jump-table*)
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      if (char= token #\{) do
        (push position forward-jump-points)
      else if (char= token #\}) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point *jump-table*) end-point)
            (setf (gethash end-point   *jump-table*) start-point))
          (error "Unmatched back jump point at position ~d." position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length   forward-jump-points)
            (nreverse forward-jump-points)))))
  (values))

;;; -------------------------------------------------------

(defun get-jump-destination (current-position)
  "Returns the opposite jump end point to the CURRENT-POSITION, if
   extant, otherwise signals an error of an unspecified type."
  (declare (type fixnum current-position))
  (the fixnum
    (or (gethash current-position *jump-table*)
        (error "No jump connection associated with the position ~d."
          current-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-memory ()
  "Resets the *TAPE* and *CELL-POINTER* to their incipial states and
   returns no value."
  (setf *tape*
    (make-array        0
      :element-type    'bit
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL))
  (setf *cell-pointer* 0)
  (values))

;;; -------------------------------------------------------

(defun ensure-tape-capacity (index)
  "Ascertains the *TAPE*'s capacity for a cell at the INDEX by
   contingently extending its size in a degree as to accommodate the
   INDEX' validity, in any case returning no value."
  (declare (type cell-index index))
  (when (>= index (length *tape*))
    (setf *tape*
      (adjust-array *tape* (1+ index)
        :initial-element 0)))
  (values))

;;; -------------------------------------------------------

(defun in-first-cell-p ()
  "Determines whether the *CELL-POINTER* resides in the first cell,
   designated by the index zero (0), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (zerop *cell-pointer*)))))

;;; -------------------------------------------------------

(defun cell-at (index)
  "Returns the *TAPE* cell at the INDEX."
  (declare (type cell-index index))
  (ensure-tape-capacity index)
  (the bit (sbit *tape* index)))

;;; -------------------------------------------------------

(defun (setf cell-at) (new-value index)
  "Stores the NEW-VALUE in the *TAPE* cell at the INDEX and returns no
   value."
  (declare (type bit        new-value))
  (declare (type cell-index index))
  (ensure-tape-capacity index)
  (setf (sbit *tape* index) new-value)
  (values))

;;; -------------------------------------------------------

(defun current-cell ()
  "Returns the bit stored in the current cell."
  (the bit (cell-at *cell-pointer*)))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value)
  "Stores the NEW-VALUE in the current cell and returns no value."
  (declare (type bit new-value))
  (setf (cell-at *cell-pointer*) new-value)
  (values))

;;; -------------------------------------------------------

(defun invert-current-bit ()
  "Inverts the *TAPE*'s current bit value and returns no value."
  (setf (current-cell) (- 1 (current-cell)))
  (values))

;;; -------------------------------------------------------

(defun store-random-bit ()
  "Stores a random bit in the *TAPE*'s current cell and returns no
   value."
  (setf (current-cell) (random 2))
  (values))

;;; -------------------------------------------------------

(defun reset-current-cell ()
  "Sets the *TAPE*'s current cell value to the inicipial state of zero
   (0) and returns no value."
  (setf (current-cell) 0)
  (values))

;;; -------------------------------------------------------

(defun one-bit-twain-p ()
  "Determines whether the *TAPE*'s current cell as well as its immediate
   sinistral peers maintain the bit value one (1) returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (and
        (not (in-first-cell-p))
        (= (cell-at (1- *cell-pointer*)) 1))))))

;;; -------------------------------------------------------

(defun and-combine-into-third-cell ()
  "Determines whether the current *TAPE* cell and it immediate
   predecessor both maintain the bit value one (1), on confirmation
   storing the value one (1) in the cell located three positions
   dextrally to the current unit, otherwise exercising an abstinence of
   any further causata, in any case returning no value."
  (when (one-bit-twain-p)
    (setf (cell-at (+ *cell-pointer* 3)) 1))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left ()
  "Translates the *CELL-POINTER* one step to the left, if not already
   empighted on the sinistral bourne, otherwise accompasses no effect,
   in any case returning no value."
  (unless (in-first-cell-p)
    (decf *cell-pointer*))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right ()
  "Translates the *CELL-POINTER* one step to the right and returns no
   value."
  (incf *cell-pointer*)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Threi (code)
  "Interprets the piece of Threi source CODE and returns no value."
  (declare (type string code))
  (compute-jump-table code)
  (initialize-memory)
  (loop
    with  ip of-type fixnum = 0
    while (< ip (length code))
    for   token of-type character = (char code ip)
    do
      (case token
        (#\> (move-cell-pointer-right))
        (#\< (move-cell-pointer-left))
        (#\h (invert-current-bit))
        (#\x (store-random-bit))
        (#\& (and-combine-into-third-cell))
        (#\o (format T "~d" (current-cell)))
        (#\e (reset-current-cell))
        (#\{ (when (zerop (current-cell))
               (setf ip
                 (get-jump-destination ip))))
        (#\} (unless (zerop (current-cell))
               (setf ip
                 (get-jump-destination ip))))
        (otherwise NIL))
      (incf ip))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-byte-printing-code (bits &key (destination NIL))
  "Generates the Threi program fragment capacitated to replicate the
   as well as output octuple BITS and writes the same to the
   DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise responding with a fresh string comprehending the
   result."
  (declare (type octet       bits))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for bit-position
          of-type (integer -1 7)
          from    7
          downto  0
        for current-bit
          of-type bit
          =       (ldb (byte 1 bit-position) bits)
        if (zerop current-bit) do
          (format destination "o>")
        else do
          (format destination "ho>"))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-byte-printing-code bits :destination output)))))

;;; -------------------------------------------------------

(defun generate-text-program (message
                              &key (destination         NIL)
                                   (character-separator ""))
  "Generates a Threi program entalented with the competence to replicate
   the MESSAGE's content in binary form, writes the same to the
   DESTINATION, contingently segregating each twain of consecutive
   character encoding command sequences by the CHARACTER-SEPARATOR, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise
   responding with a fresh string comprehending the result."
  (declare (type string      message))
  (declare (type destination destination))
  (declare (type T           character-separator))
  (the (or null string)
    (if destination
      (loop
        for character         of-type character across message
        and first-character-p of-type boolean =        T then NIL
        do
          (unless first-character-p
            (format destination "~a" character-separator))
          (generate-byte-printing-code
            (char-code character)
            :destination destination))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program message
          :destination         output
          :character-separator character-separator)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the binary representation of the message "Hello, World!".
(interpret-Threi
  "o>ho>o>o>ho>o>o>o>
   o>ho>ho>o>o>ho>o>ho>
   o>ho>ho>o>ho>ho>o>o>
   o>ho>ho>o>ho>ho>o>o>
   o>ho>ho>o>ho>ho>ho>ho>
   o>o>ho>o>ho>ho>o>o>
   o>o>ho>o>o>o>o>o>
   o>ho>o>ho>o>ho>ho>ho>
   o>ho>ho>o>ho>ho>ho>ho>
   o>ho>ho>ho>o>o>ho>o>
   o>ho>ho>o>ho>ho>o>o>
   o>ho>ho>o>o>ho>o>o>
   o>o>ho>o>o>o>o>ho>")

;;; -------------------------------------------------------

;; Generate the Threi program to print the binary representation of the
;; message "Hello, World!" and interpret the same.
(interpret-Threi
  (generate-text-program "Hello, World!"
    :character-separator #\Newline))

;;; -------------------------------------------------------

;; Truth-machine, the same simulates a user input of 1.
(interpret-Threi
  "
  h
  >h{&
   >o<<
  }
  ")

;;; -------------------------------------------------------

;; Truth-machine, the same simulates a user input of 0.
(interpret-Threi
  "
  >h{&
   >o<<
  }
  ")
