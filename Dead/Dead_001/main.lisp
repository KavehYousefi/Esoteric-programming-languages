;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Dead", invented by the Esolang user "Ilaylevy", and
;; intended as a cell-based medium for programming, each command of
;; which utilizes a single character.
;; 
;; 
;; Architecture
;; ============
;; A Dead program's data management is realized in the form of a
;; bilaterally unbounded tally of cells, linearly arranged on a
;; tape-like structure. Each cell stores a scalar integer value in the
;; range [-infinity, +infinity], initially set to zero (0).
;; 
;; A cell pointer, at the start of the program empight to the incipient
;; cell, selects at any time the current instance, while being amenable
;; to instructions for its stepwise lateral translation.
;; 
;; 
;; Data Types
;; ==========
;; The Dead programming language wists only about one data type: signed
;; integer numbers of no restriction in their magnitude, that is,
;; commorant in the space [-infinity, +infinity].
;; 
;; 
;; Instructions
;; ============
;; A sextuple of specimens serves to exhaust the instruction set:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   p       | Prints the numeric value stored in the current cell to
;;           | the standard output.
;;           | Please note that no space nor other sepiment intrudes
;;           | betwixt two outputs.
;;   ..................................................................
;;   a       | Increments the current cell value by one.
;;   ..................................................................
;;   s       | Decrements the current cell value by one.
;;   ..................................................................
;;   i       | Queries the user for an integer input and stores the
;;           | same into the current cell.
;;   ..................................................................
;;   l       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   r       | Moves the cell pointer one step to the right.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-08-30
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Dead"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines a Dead program's cell-based memory in the
   form of a hash table associating with the integer keys,
   representatives of the cell indices, the integer cell values."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Dead (code)
  "Interprets the piece of Dead CODE and returns no value."
  (declare (type string code))
  (let ((memory  (make-hash-table :test #'eql))
        (pointer 0))
    (declare (type memory  memory))
    (declare (type integer pointer))
    (symbol-macrolet
        ((current-cell
          (the integer
            (gethash pointer memory 0))))
      (loop
        for command  of-type character across code
        and position of-type fixnum    from 0
        do
          (case command
            ((#\Space #\Tab #\Newline)
              NIL)
            (#\p
              (format T "~d" current-cell))
            (#\a
              (incf current-cell))
            (#\s
              (decf current-cell))
            (#\i
              (format T "~&Please input an integer: ")
              (setf current-cell
                (or (ignore-errors (parse-integer (read-line)))
                    0))
              (clear-input))
            (#\l
              (decf pointer))
            (#\r
              (incf pointer))
            (otherwise
              (error "Invalid command \"~c\" at position ~d."
                command position))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the text "1234".
(interpret-Dead "apapapap")

;;; -------------------------------------------------------

;; A one-time numeric cat program.
(interpret-Dead "ip")

;;; -------------------------------------------------------

;; Store the character codes of the text "Hello, World!" in consecutive
;; cells of the program memory and concomitantly print each such number.
;; The final memory state will resolve to the following:
;; 
;;   72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33
(interpret-Dead "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaapraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaap")
