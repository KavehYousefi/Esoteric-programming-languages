;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tick", introduced by the Esolang user "Dpleshkov" in the
;; year 2017, designed as a less potent variation of Urban Mueller's
;; "brainfuck" with its only expressive competence the output of ASCII
;; characters.
;; 
;; 
;; Concept
;; =======
;; Representing a curtailed derivation of the brainfuck esoteric
;; programming language, Tick's capabilities are defined merely in the
;; management of an infinite tape of unsigned-byte-valued cells,
;; responding to gradual incrementation and printing of the ASCII
;; characters associated with the numeric data.
;; 
;; 
;; Architecture
;; ============
;; Tick programs operate on a tape of cells, extending its components
;; bilaterally into infinity, with each cell amplecting a single 8-bit
;; unsigned byte datum, corresponding to the integer range [0, 255]. If
;; faced with an attempt to increment its value above the octet
;; interval's maximum of 255, the cell wraps around to the lowest state
;; of zero (0).
;; 
;; At the program's inchoation empight on an initial unit, a special
;; cursor, known as the "selector", refers at any instant to the
;; currently active cell, the sole entity amenable to indagations and
;; manipulations. Instructions exist to gradually shift the cursor along
;; the tape.
;; 
;; 
;; Data Types
;; ==========
;; Tick employs two major categories of data: octets and characters.
;; 
;; == BYTES ==
;; Specifying each cell's content, unsigned bytes of eight bit size,
;; also known as octets and spanning the integer range [0, 255], enjoy a
;; paravaunt significance.
;; 
;; == CHARACTERS ==
;; Engaging in a puisne role, ASCII characters form an expression of
;; the unsigned byte basis commorant in the cells, deployed solely for
;; output purposes.
;; 
;; 
;; Syntax
;; ======
;; The sole significant tokens in a piece of Tick source code are
;; exhausted by the four single-character command identifiers, with any
;; other content's homologation adhibiting nothing more than the
;; puissance of apostilles.
;; 
;; 
;; Instructions
;; ============
;; Appropriated from its inspiration, Tick assumes a subset of
;; brainfuck's abilities, bisecting the cleronomy from eight to four
;; effective members.
;; 
;; == OVERVIEW ==
;; An apercu of cursory penetration shall administer a slight ilk of
;; education about the language's appliances:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell selector one step to the right.
;;   ..................................................................
;;   <       | Moves the cell selector one step to the left.
;;   ..................................................................
;;   +       | Increments the current memory cell's value by one.
;;           | If surpassing the maximum bourne of 255, the cell value
;;           | relapses to the minimum of zero (0).
;;   ..................................................................
;;   *       | Prints to the standard output the character
;;           | corresponding to the current memory cell's value when
;;           | construed as an ASCII code.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-03-16
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Tick"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight bits,
   thus being a commorant of the closed integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' type defines a sparse array of unsigned-byte-valued
   cells, indexed by signed integer subscripts, designed using a hash
   table of integer keys affiliating with ``octet'' values."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   'integer)
                     (typep value 'octet))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type octet +MAXIMUM-CELL-VALUE+))

;;; -------------------------------------------------------

(defparameter +MAXIMUM-CELL-VALUE+ 255
  "The largest value admissive to a cell's datum.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory))
  "The ``Memory'' class models the infinite tape of unsigned byte cells,
   operated upon by a selector that at any instant refers to the active
   cell."
  (cells    (make-hash-table :test #'eql) :type cell-map)
  (selector 0                             :type integer))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the current MEMORY cell's byte value."
  (declare (type Memory memory))
  (the octet
    (gethash (memory-selector memory) (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns the
   modified MEMORY."
  (declare (type octet  new-value))
  (declare (type Memory memory))
  (setf (gethash (memory-selector memory) (memory-cells memory) 0)
        new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell value by one, contingently
   wrapping the content around to the minimum of zero (0) if exceeding
   the upper byte bourne of 255, and returns the modified MEMORY."
  (declare (type Memory memory))
  (if (>= (memory-current-cell memory) +MAXIMUM-CELL-VALUE+)
    (setf (memory-current-cell memory) 0)
    (incf (memory-current-cell memory) 1))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell selector one step to the right and returns
   the modified MEMORY."
  (declare (type Memory memory))
  (incf (memory-selector memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell selector one step to the left and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (decf (memory-selector memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-print-current-cell (memory &optional (destination T))
  "Prints the character associated with the MEMORY's current cell, when
   its value is construed as an ASCII code, to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise
   responding with a fresh string containing the output."
  (declare (type Memory      memory))
  (declare (type destination destination))
  (the (or null string)
    (format destination "~c"
      (code-char
        (memory-current-cell memory)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Tick (code)
  "Interprets the piece of Tick source CODE and returns no value."
  (declare (type string code))
  (let ((memory (make-memory)))
    (declare (type Memory memory))
    (loop for token of-type character across code do
      (case token
        (#\>       (memory-move-right         memory))
        (#\<       (memory-move-left          memory))
        (#\+       (memory-increment          memory))
        (#\*       (memory-print-current-cell memory T))
        (otherwise NIL))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello world!" to the standard output.
(interpret-Tick
"
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*+++++++++++++++++++++++++++++*+++++++**+++*>+++++++++++++++++++++++++++
+++++*<++++++++*>+++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++*+++*>++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++*>+++++++++++++++++++++++++++++++++*
")
