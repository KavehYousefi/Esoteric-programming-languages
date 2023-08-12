;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Braisnack, invented by the Esolang user "Cinnamony" and
;; presented on June 23rd, 2023, the foundation of which bifurcates into
;; the unit of information designed by the same author, the "SNACBIT",
;; and Urban Mueller's language "brainfuck".
;; 
;; 
;; Concept
;; =======
;; The Braisnack programming language establishes a jocular species
;; whose competences, desumed from the Turing-complete stock-father
;; brainfuck, but curtailed significantly by the underlying SNACBIT data
;; type, homologate a mediocre amount of productivity.
;; 
;; == SNACBIT: A DELECTABLE UNIT OF INFORMATION ==
;; The SNACBIT, invented by the same author as this language, represents
;; an information unit exhausted by eleven members which partake of an
;; ordinal ordonnance:
;; 
;;   ------------------------------
;;   Number | SNACBIT state
;;   -------+----------------------
;;   1      | Peanut butter cracker
;;   ..............................
;;   2      | Cosmic brownie
;;   ..............................
;;   3      | Cheezit
;;   ..............................
;;   4      | Cheeto
;;   ..............................
;;   5      | Dorito
;;   ..............................
;;   6      | Tortilla chip
;;   ..............................
;;   7      | Cheese cracker
;;   ..............................
;;   8      | Potato chip
;;   ..............................
;;   9      | Applesauce
;;   ..............................
;;   10     | Peanut butter cup
;;   ..............................
;;   11     | Pickle
;;   ------------------------------
;; 
;; == BRAISNACK: A TAPE OF SNACBIT STATES ==
;; Braisnack operates on an bilaterally infinite tally of cells, each
;; specimen of which comprehends an aefauld SNACBIT state, initialized
;; to the incipient member, "Peanut butter cracker".
;; 
;; A movable cell pointer selects at any instant the currently active
;; cell, whose amenability to indagations and modifications capacitates
;; basic arithmetics, input, output, and simple control flow.
;; 
;; 
;; Instructions
;; ============
;; A kenspeckle variation of its stock-father's cleronomy, Braisnack
;; tallies the equinumerant octuple instruction set, yet accommodated to
;; the private diorisms.
;; 
;; == OVERVIEW ==
;; The eight commands to the programmer's avail shall now be a cursory
;; treatise's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   <       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   >       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   +       | Increments the current cell's state number by one, thus
;;           | proceeding to the next SNACBIT state.
;;           | If the new state number trespasses the valid range
;;           | [1, 11], it is wrapped around into this interval.
;;   ..................................................................
;;   -       | Decrements the current cell's state number by one, thus
;;           | returning to the next SNACBIT state.
;;           | If the new state number trespasses the valid range
;;           | [1, 11], it is wrapped around into this interval.
;;   ..................................................................
;;   .       | Prints the current cell's SNACBIT state name to the
;;           | standard output.
;;   ..................................................................
;;   ,       | Queries the user for a SNACBIT state name and stores it
;;           | in the current cell.
;;           | The concrete deportment upon an invalid input
;;           | constitutes an implementation-dependent choice.
;;   ..................................................................
;;   [       | If the current cell's SNACBIT state equals the first
;;           | member, "Peanut butter cracker", moves the instruction
;;           | pointer (IP) forward to the character immediately
;;           | succeeding the matching "]" token. Otherwise proceeds as
;;           | usual.
;;   ..................................................................
;;   [       | If the current cell's SNACBIT state does not equal the
;;           | first member, "Peanut butter cracker", moves the
;;           | instruction pointer (IP) back to the character
;;           | immediately succeeding the matching "[" token. Otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-12
;; 
;; Sources:
;;   [esolang2023Braisnack]
;;   The Esolang contributors, "Braisnack", June 23, 2023
;;   URL: "https://esolangs.org/wiki/Braisnack"
;;   Notes:
;;     - Specification of the Braisnack programming language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype state-number ()
  "The ``state-number'' type defines the range of valid state numbers,
   positions that enumerate the SNACBIT states in their natural order,
   being tantamount to the closed intervall [1, 11]."
  '(integer 1 11))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional mapping of forward
   and back jump positions in a piece of Braisnack source code,
   implemented as a hash table whose fixnum keys associate with fixnum
   values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   bilaterally infinite extent, the integer keys of which represent the
   cell indices, mapping to ``state-number''s that reference the
   respective SNACBIT states."
  '(hash-table-of integer state-number))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of SNACBIT states.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array simple-string (11)) +SNACBIT-STATES+))

;;; -------------------------------------------------------

(defparameter +SNACBIT-STATES+
  (make-array 11
    :element-type 'simple-string
    :initial-contents
      '("Peanut butter cracker"
        "Cosmic brownie"
        "Cheezit"
        "Cheeto"
        "Dorito"
        "Tortilla chip"
        "Cheese cracker"
        "Potato chip"
        "Applesauce"
        "Peanut butter cup"
        "Pickle")
    :adjustable   NIL
    :fill-pointer NIL)
  "Maintains the SNACBIT states in a one-dimensional simple array of its
   string representations.
   ---
   The vector qua a zero-based collection requires the one-based SNACBIT
   state numbers to be reduced by one (1) in order to map to the name at
   the respective vector index. The state with the number one (1),
   norned \"Peanut butter cracker\", as a corollary, resides at the
   +SNACBIT-STATES+ array's first position zero (0).")

;;; -------------------------------------------------------

(defun get-snack (state-number)
  "Returns the name of the SNACBIT state commorant at the one-based
   STATE-NUMBER."
  (declare (type state-number state-number))
  (the simple-string
    (aref +SNACBIT-STATES+
      (1- state-number))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of state search operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array string (11)) +NORMALIZED-SNACBIT-STATES+))

;;; -------------------------------------------------------

(defun normalize-string (string)
  "Returns a modified form of the STRING apt for the utilization in a
   snack name search."
  (declare (type string string))
  (the string
    (string-upcase
      (remove #\Space string :test #'char=))))

;;; -------------------------------------------------------

(defparameter +NORMALIZED-SNACBIT-STATES+
  (map '(simple-array string (11))
    #'normalize-string +SNACBIT-STATES+)
  "Maintains a variation of the SNACBIT states with the snack names
   being normalized, that is, accommodated for search purposes by their
   name, which comprehends the removal of spaces and the minuscular
   transformation.
   ---
   The following correspondences hold betwixt the original SNACBIT state
   name and its normalized agnomination:
     -------------------------------------------
     Original state name | Normalized state name
     --------------------+----------------------
     Cosmic brownie      | cosmicbrownie
     Cheezit             | cheezit
     Cheeto              | cheeto
     Dorito              | dorito
     Tortilla chip       | tortillachip
     Cheese cracker      | cheesecracker
     Potato chip         | potatochip
     Applesauce          | applesauce
     Peanut butter cup   | peanutbuttercup
     Pickle              | pickle
     -------------------------------------------")

;;; -------------------------------------------------------

(defun get-state-number (search-term)
  "Returns the one-based state number corresponding to the SNACBIT state
   whose name more aptly matches the SEARCH-TERM, or ``NIL'' upon its
   disrespondency."
  (declare (type string search-term))
  (let ((position
          (position
            (normalize-string search-term)
            +NORMALIZED-SNACBIT-STATES+
            :test #'string=)))
    (declare (type (or null (integer 0 10)) position))
    (the (or null state-number)
      (when position
        (1+ position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Builds and returns a ``jump-table'' that associates the forward jump
   locations in the CODE with the matching back jump indices, and vice
   versa."
  (declare (type string code))
  (let ((jump-table (make-hash-table :test #'eql)))
    (declare (type jump-table jump-table))
    (let ((forward-jump-points NIL))
      (declare (type (list-of fixnum) forward-jump-points))
      (loop
        for token    of-type character across code
        for position of-type fixnum    from   0 by 1
        if (char= token #\[) do
          (push position forward-jump-points)
        else if (char= token #\]) do
          (if forward-jump-points
            (let ((start-point (pop forward-jump-points))
                  (end-point   position))
              (declare (type fixnum start-point))
              (declare (type fixnum end-point))
              (setf (gethash start-point jump-table) end-point)
              (setf (gethash end-point   jump-table) start-point))
            (error "Unmatched back jump point at position ~d."
              position))
        finally
          (when forward-jump-points
            (error "Unmatched forward jump points at ~
                    positions ~{~d~^, ~}."
              forward-jump-points))))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun query-snack ()
  "Repeatedly queries the standard input for a character sequence
   resembling a SNACBIT state until either a match is detected, or the
   source is exhausted, in the former case returning the one-based state
   number, otherwise ``NIL''."
  (let ((input                  NIL)
        (state-number-for-input NIL))
    (declare (type (or null string)       input))
    (declare (type (or null state-number) state-number-for-input))
    (loop do
      (format T "~&>> ")
      (setf input (read-line *standard-input* NIL NIL))
      (clear-input)
      (cond
        (input
          (setf state-number-for-input
            (get-state-number input))
          (when state-number-for-input
            (loop-finish)))
        (T
          (loop-finish))))
    (the (or null state-number) state-number-for-input)))

;;; -------------------------------------------------------

(defun normalize-state-number (state-number)
  "Wraps the STATE-NUMBER into the valid SNACBIT number range [1, 11]
   and returns the thus produced value."
  (declare (type integer state-number))
  (the state-number
    (1+ (mod (- state-number 1) 11))))

;;; -------------------------------------------------------

(defun interpret-Braisnack (code)
  "Interprets the piece of Braisnack source CODE and returns no value."
  (declare (type string code))
  (let ((ip           0)
        (jump-table   (build-jump-table code))
        (memory       (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type memory     memory))
    (declare (type integer    cell-pointer))
    (symbol-macrolet
        ((current-cell
          (the state-number
            (gethash cell-pointer memory 1)))
         
         (on-peanut-butter-cracker-p
          (the boolean
            (not (null
              (= current-cell 1))))))
      
      (loop while (< ip (length code)) do
        (case (char code ip)
          (#\>
            (incf cell-pointer))
          
          (#\<
            (decf cell-pointer))
          
          (#\-
            (setf current-cell
              (normalize-state-number
                (1- current-cell))))
          
          (#\+
            (setf current-cell
              (normalize-state-number
                (1+ current-cell))))
          
          (#\[
            (when on-peanut-butter-cracker-p
              (setf ip
                (gethash ip jump-table))))
          
          (#\]
            (unless on-peanut-butter-cracker-p
              (setf ip
                (gethash ip jump-table))))
          
          (#\.
            (format T "~&~a"
              (get-snack current-cell)))
          
          (#\,
            (let ((snack-number (query-snack)))
              (declare (type (or null state-number)))
              (when snack-number
                (setf current-cell snack-number))))
          
          (otherwise
            NIL))
        
        (incf ip))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Braisnack ",.")

;;; -------------------------------------------------------

;; Print the following entries:
;;   Pickle
;;   Pickle
;;   Dorito
;;   Tortilla chip
;;   Applesauce
(interpret-Braisnack "-..+++++.+.+++.")

;;; -------------------------------------------------------

;; Print all SNACBIT states in their natural order by employing the
;; jump facility.
(interpret-Braisnack ".+[.+]")
