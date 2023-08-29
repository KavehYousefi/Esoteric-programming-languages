;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "...", invented by the Esolang user "RandomIdiot" and
;; presented on May 11th, 2023, the concept of which involves an
;; infinite tape of cells that comprehend an unsigned byte as well as an
;; executable action, operated upon by a commands desumed from the set
;; of periods ("."), colons (":"), and spaces.
;; 
;; 
;; Concept
;; =======
;; The ... programming language operates on a bilaterally infinite tape
;; of cells, each such a compound of an unsigned byte value and an
;; input or output action, with the current instance amenable to a cell
;; pointer, being capable of modification via two-character instructions
;; compact of dots ("."), colons (":"), and spaces (" ").
;; 
;; == THE TAPE: AN INFINITE TAPE OF CELLS ==
;; The tape contributes an infinitely sized memory, its ordonnance a
;; bilateral expanse of cells in linear arrangement.
;; 
;; == THE CELLS: VALUE-ACTION COMPOUNDS ==
;; Every cells's componency tallies a twain of unsigned byte value and
;; action.
;; 
;; == CELL VALUES: UNSIGNED BYTES ==
;; The former moeity amounts to an integral datum commorant in the range
;; [0, 255]. Alterations that eloign the value outside of this realm
;; are subjected to a wrapping behavior, that is, when transcending the
;; upper bourne of 255, the value restarts with the minimum of zero (0),
;; while the athwart transgression, a descent alow the lower extremum
;; of zero (0), translates into the maximum of 255.
;; 
;; A cell's value acquires the minimum of zero (0) as its inchoate
;; state.
;; 
;; == CELL ACTIONS: INPUT/OUTPUT CAPABILITIES RESIDE IN CELLS ==
;; The parcel of supererogation institutionalized into the cells, the
;; action compartment, specifies a capability intended for the cell's
;; pursuit upon request.
;; 
;; At any instant, one of two alternative actions may be stored in this
;; segment, comprehending only output or input, the former assumes the
;; default state.
;; 
;;   ------------------------------------------------------------------
;;   Action | Effect
;;   -------+----------------------------------------------------------
;;   output | Prints the character whose ASCII code matches the current
;;          | cell's value to the standard output.
;;   ..................................................................
;;   input  | Queries the standard input for an ASCII character and
;;          | stores its character code in the current cell's value
;;          | compartment.
;;   ------------------------------------------------------------------
;; 
;; The transition betwixt these states does not ensue from direct
;; imperatives, but from a switching, or toggling, supplanting the
;; current action by its obverse. The following regulations proceed as a
;; consectary:
;; 
;;   ----------------------------
;;   Current action | Next action
;;   ---------------+------------
;;   output         | input
;;   ............................
;;   input          | output
;;   ----------------------------
;; 
;; == COMMANDS ARE COMPOSED OF DOTS, COLONS, AND SPACES ==
;; The language's donet restricts its adit to a treble of symbols only,
;; namely the dot ("."), colon (":"), and space (" ").
;; 
;; A command's agnomination always embraces a twain of such in
;; combination, with no other content tolerated to participate.
;; 
;; 
;; Instructions
;; ============
;; ...'s instruction set tallies a sextuple membership, whose the
;; perimeter encompasses modulations of the current memory cell's value
;; and action, the latter's execution, and the translation of the cell
;; pointer.
;; 
;; == OVERVIEW ==
;; A cursory nortelry anenst the operational competences shall be
;; conveyed by the following apercu.
;; 
;; Please heed that, based upon the requisitum to illustrate spaces,
;; which, however, would vanish in the documentation betwixt the
;; surrounding whitespaces, the command identifiers are ensconced in
;; double quotation marks, '"' and '"'; a forbisen applicable to this
;; principle, the token amplecting a period (".") and a subsequent space
;; (" ") would be limned as
;; 
;;   ". "
;; 
;; Hence, the quotation mark jumelles must be elided in the actual code.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ".:"    | Increments the current cell value by one (1).
;;           | If the new value transgresses the upper march of 255, it
;;           | is wrapped around to the minimum of zero (0).
;;   ..................................................................
;;   ":."    | Decrements the current cell value by one (1).
;;           | If the new value transgresses the lower bourne of 255,
;;           | it wrapped around to the maximum of 255.
;;   ..................................................................
;;   ".."    | Switches the current cell action to its opposite state.
;;   ..................................................................
;;   "::"    | Executes the action stored in the current cell.
;;   ..................................................................
;;   ". "    | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   " ."    | Moves the cell pointer one step to the left.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-29
;; 
;; Sources:
;;   [esolang2023...]
;;   The Esolang contributors, "...", May 15th, 2023
;;   URL: "https://esolangs.org/wiki/..."
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type predicates.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-of-p (candidate &optional (element-type T))
  "Determines whether the CANDIDATE represents a list composed of zero
   or more elements, whose members conform to the ELEMENT-TYPE, which
   defaults to the comprehensive ``T'', returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (declare (type T element-type))
  (the boolean
    (not (null
      (and
        (listp candidate)
        (every
          #'(lambda (element)
              (declare (type T element-type))
              (typep element element-type))
          (the list candidate)))))))

;;; -------------------------------------------------------

(defun hash-table-of-p (candidate &optional (key-type T) (value-type T))
  "Determines whether the CANDIDATE represents a hash table of zero or
   more entries, whose keys conform to the KEY-TYPE and whose values to
   the VALUE-TYPE, where both default to the comprehensive ``T'',
   returning on confirmation a ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (declare (type T key-type))
  (declare (type T value-type))
  (the boolean
    (not (null
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
                 (typep value value-type))))))))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variants of ...
   operations."
  '(member
    :increment
    :decrement
    :change-action
    :execute-action
    :move-right
    :move-left))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf  (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (list-of-p candidate element-type)))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose keys conform to
   the KEY-TYPE and whose values conforms to the VALUE-TYPE, both
   defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (hash-table-of-p candidate key-type value-type)))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight accolent
   bits as an integer in the range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype action ()
  "The ``action'' class enumerates the possible variants of actions
   admitted to a cell in compernage of its traditional octet value."
  '(member :output :input))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' type defines a sparse vector of memory cells as a
   hash table whose keys maintain the cell indices as ``integer''s,
   associating with the cell values in the form of ``Cell'' instances."
  '(hash-table-of integer Cell))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-cell ()))
  "The ``Cell'' class applies itself to the onus of representing a
   aefauld memory cell as a compound of an octet scalar and an action."
  (value  0       :type octet)
  (action :output :type action))

;;; -------------------------------------------------------

(defun cell-switch-action (cell)
  "Switches the action stored in the cell to the obverse state and
   returns no value."
  (declare (type Cell cell))
  (setf (cell-action cell)
    (case (cell-action cell)
      (:output   :input)
      (:input    :output)
      (otherwise
        (error "Invalid cell action: ~s."
          (cell-action cell)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-map
    :documentation "A sparse vector of (cellValue, cellAction) objects,
                    maintained as ``Cell'' instances, and amenable to
                    integer subscripts.")
   (pointer
    :initform      0
    :type          integer
    :documentation "Designates the current instance among the CELLS by
                    maintaining its cell index, that is, the hash table
                    key."))
  (:documentation
    "The ``Memory'' class defines the program memory, an infinite linear
     arrangement of cells maintaining an octet value and an action, as a
     hash table of ``Cell'' instances, operated upon by a mobile cell
     pointer."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory''."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-ensure-cell (memory)
  "Ensures that a cell exists at the MEMORY's current pointer location,
   either returning the already extant cell or the newly created and
   registered instance."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type cell-map cells))
    (declare (type integer  pointer))
    (multiple-value-bind (current-cell contains-cell-p)
        (gethash pointer cells)
      (declare (type (or null Cell) current-cell))
      (declare (type T              contains-cell-p))
      (the Cell
        (or
          (and contains-cell-p current-cell)
          (let ((new-cell (make-cell)))
            (declare (type Cell new-cell))
            (setf (gethash pointer cells) new-cell)
            (the Cell new-cell)))))))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY cell at the current pointer position."
  (declare (type Memory memory))
  (the Cell
    (memory-ensure-cell memory)))

;;; -------------------------------------------------------

(defun memory-current-value (memory)
  "Returns the value of the MEMORY cell at the current pointer
   position."
  (declare (type Memory memory))
  (the octet
    (cell-value
      (memory-ensure-cell memory))))

;;; -------------------------------------------------------

(defun (setf memory-current-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY cell at the current pointer
   position, contingently wrapping it around to accommodate the byte
   range [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (cell-value
      (memory-ensure-cell memory))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the value of the MEMORY cell at the current pointer
   position by one, contingently wrapping it around to accommodate the
   byte range [0, 255], and returns no value."
  (declare (type Memory memory))
  (incf (memory-current-value memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the value of the MEMORY cell at the current pointer
   position by one, contingently wrapping it around to accommodate the
   byte range [0, 255], and returns no value."
  (declare (type Memory memory))
  (decf (memory-current-value memory))
  (values))

;;; -------------------------------------------------------

(defun memory-current-action (memory)
  "Returns the action of the MEMORY cell at the current pointer
   position."
  (declare (type Memory memory))
  (the action
    (cell-action
      (memory-ensure-cell memory))))

;;; -------------------------------------------------------

(defun memory-switch-current-action (memory)
  "Switches the action of the MEMORY cell at the current pointer
   position to its alternative state and returns no value."
  (declare (type Memory memory))
  (cell-switch-action
    (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns no
   value."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns no
   value."
  (declare (type Memory memory))
  (decf (slot-value memory 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          string
    :documentation "The piece of ... source code to evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (memory
    :initform      (make-memory)
    :reader        interpreter-memory
    :type          Memory
    :documentation "The program memory."))
  (:documentation
    "The ``Interpreter'' applies itself to the processing of a piece of
     ... source code in order to accompass its execution."))

;;; -------------------------------------------------------

(defun make-interpreter (source)
  "Creates and returns a new ``Interpreter'' which operates on the ...
   SOURCE program."
  (declare (type string source))
  (the Interpreter
    (make-instance 'Interpreter :source source)))

;;; -------------------------------------------------------

(defun interpreter-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's source is exhausted, that is,
   its instruction pointer (IP) has transcended the desinent token,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (slot-value interpreter 'ip)
          (length (slot-value interpreter 'source)))))))

;;; -------------------------------------------------------

(defun interpreter-get-next-command (interpreter)
  "Proceeding from the INTERPRETER's current position into its source,
   extract and returns the next command.
   ---
   If an invalid token is encountered or the INTERPRETER's source is
   exhausted, an error of an unspecified type is signaled."
  (declare (type Interpreter interpreter))
  (with-slots (source ip) interpreter
    (declare (type string source))
    (declare (type fixnum ip))
    (flet ((token-equals-p (expected-identifier)
            "Determines whether the one or two characters proceeding
             from the current instruction pointer (IP) position match
             the EXPECTED-IDENTIFIER, on confirmation returning a
             ``boolean'' value of ``T'', otherwise ``NIL''."
            (declare (type (string 2) expected-identifier))
            (the boolean
              (not (null
                (string= source expected-identifier
                  :start1 ip
                  :end1   (min (+ ip 2)
                               (length source))))))))
      (the command
        (prog1
          (cond
            ((token-equals-p ".:") :increment)
            ((token-equals-p ":.") :decrement)
            ((token-equals-p "..") :change-action)
            ((token-equals-p "::") :execute-action)
            ((token-equals-p ". ") :move-right)
            ((token-equals-p " .") :move-left)
            (T
              (error "Invalid token ~s at position ~d."
                (subseq source ip
                  (min (+ ip 2)
                       (length source)))
                ip)))
          (incf ip 2))))))

;;; -------------------------------------------------------

(defun interpreter-execute (interpreter)
  "Interprets the ... program stored in the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop
    until (interpreter-exhausted-p interpreter)
    for command
      of-type command
      =       (interpreter-get-next-command interpreter)
    do
      (case command
        (:increment
          (memory-increment
            (interpreter-memory interpreter)))
        
        (:decrement
          (memory-decrement
            (interpreter-memory interpreter)))
        
        (:change-action
          (memory-switch-current-action
            (interpreter-memory interpreter)))
        
        (:execute-action
          (let ((cell-action
                  (memory-current-action
                    (interpreter-memory interpreter))))
            (declare (type action cell-action))
            (case cell-action
              (:output
                (write-char
                  (code-char
                    (memory-current-value
                      (interpreter-memory interpreter)))))
              (:input
                (format T "~&>> ")
                (force-output)
                (setf (memory-current-value
                        (interpreter-memory interpreter))
                      (char-code
                        (read-char)))
                (clear-input))
              (otherwise
                (error "Invalid cell action: ~s." cell-action)))))
        
        (:move-right
          (memory-move-right
            (interpreter-memory interpreter)))
        
        (:move-left
          (memory-move-left
            (interpreter-memory interpreter)))
        
        (otherwise
          (error "Invalid command ~s at position ~d." command
            (slot-value interpreter 'ip)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-|...| (code)
  "Interprets the piece of ... source CODE and returns no value."
  (declare (type string code))
  (interpreter-execute
    (make-interpreter code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-|...| "..::..::")
