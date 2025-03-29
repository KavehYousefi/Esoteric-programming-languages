;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the ``` program memory, a bilaterally infinite
;; dispansion of signed integer-valued cells, amenable to subscripts
;; desumed from the same realm.
;; 
;; As a parergon to its efforts in the foundational castaldy, and a
;; natural consequence produced via its consanguinity with the task,
;; this spatial parcery furnishes the commodities of special addresses'
;; indagation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Special cell address indagation operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun address-of-ip-cell-p (candidate)
  "Determines whether the CANDIDATE represents the address dedicated to
   the instruction pointer's (IP) castaldy, its diorism conflating with
   the index zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type integer candidate))
  (the boolean
    (get-boolean-value-of
      (zerop candidate))))

;;; -------------------------------------------------------

(defun address-of-execution-cell-p (candidate)
  "Determines whether the CANDIDATE represents the address dedicated to
   the conditional execution's helming, its diorism conflating with the
   index one (1), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type integer candidate))
  (the boolean
    (get-boolean-value-of
      (= candidate 1))))

;;; -------------------------------------------------------

(defun address-of-io-switch-cell-p (candidate)
  "Determines whether the CANDIDATE represents the address dedicated to
   the input or output facility's actuation, its diorism conflating with
   the index two (2), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type integer candidate))
  (the boolean
    (get-boolean-value-of
      (= candidate 2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Memory integer) integer)
         cell-value))

(declaim (ftype (function (integer Memory integer) (values))
         (setf cell-value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :accessor      memory-cells
    :type          cell-vector
    :documentation "A sparse vector of infinite dispansion accommodating
                    signed integer cells."))
  (:documentation
    "The ``Memory'' class serves in the realization of the ``` program
     memory as a bilaterally infinite expansion of cells, each such a
     signed integer number's salvatory, and its amenability described by
     a signed integer address."))

;;; -------------------------------------------------------

(defun make-default-memory ()
  "Creates and returns a fresh ``Memory'' whose incipial elements
   are all set to zero (0)."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun make-memory-initialized-with (&rest initial-cell-values)
  "Creates and returns a fresh ``memory'' whose incipial elements,
   commencing from the lowest address zero (0), are appropriated from
   the INITIAL-CELL-VALUES."
  (declare (type (list-of integer) initial-cell-values))
  (let ((memory (make-default-memory)))
    (declare (type Memory memory))
    (loop
      for cell-address of-type (integer 0 *) from 0 by 1
      for cell-value   of-type (integer * *) in   initial-cell-values
      do  (setf (cell-value memory cell-address) cell-value))
    (the memory memory)))

;;; -------------------------------------------------------

(defun cell-value (memory address)
  "Returns the integer number stored in the MEMORY cell amenable to the
   ADDRESS."
  (declare (type Memory  memory))
  (declare (type integer address))
  (the integer
    (gethash address (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf cell-value) (new-value memory address)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the ADDRESS and
   returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (declare (type integer address))
  (setf (gethash address (memory-cells memory) 0) new-value)
  (values))

;;; -------------------------------------------------------

(defun cell-bit-value (memory address)
  "Interprets the content of the MEMORY cell amenable to the ADDRESS as
   a bit value, returning for a zero-valued cell the number zero (0);
   otherwise, for any non-zero state, responds with one (1)."
  (declare (type Memory  memory))
  (declare (type integer address))
  (the bit
    (if (zerop (cell-value memory address))
      0
      1)))

;;; -------------------------------------------------------

(defun get-ip-position (memory)
  "Returns the value stored in the MEMORY's first cell, located at the
   address zero (0), which designates the instruction pointer (IP)
   position."
  (declare (type Memory memory))
  (the integer
    (cell-value memory 0)))

;;; -------------------------------------------------------

(defun advance-ip (memory)
  "Increments the instruction pointer (IP), its commorancy established
   in the first MEMORY cell, at the index zero (0), by a value of one
   (1) and returns no value."
  (declare (type Memory  memory))
  (incf (cell-value memory 0))
  (values))

;;; -------------------------------------------------------

(defun execution-enabled-p (memory)
  "Determines whether the respective MEMORY flag empight at the address
   one (1) currently homologates an instruction's execution, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (get-boolean-value-of
      (zerop
        (cell-value memory 1)))))

;;; -------------------------------------------------------

(defun disable-io (memory)
  "Disables the input and output capabilities by setting the MEMORY cell
   at the address two (2) to zero (0) and returns no value."
  (declare (type Memory memory))
  (setf (cell-value memory 2) 0)
  (values))

;;; -------------------------------------------------------

(defun get-io-mode (memory)
  "Returns the currently established input/output mode, deriving its
   status from the MEMORY cell at the address three (3)."
  (declare (type Memory memory))
  (the io-mode
    (case (cell-value memory 3)
      (0         :output)
      (1         :input)
      (otherwise :none))))

;;; -------------------------------------------------------

(defun determine-bounding-addresses (memory)
  "Returns the range of the explicitly specified MEMORY cell addresses
   as two values:
     (1) The inclusive minimum MEMORY cell address.
     (2) The inclusive maximum MEMORY cell address."
  (declare (type Memory memory))
  (the (values integer integer)
    (loop
      for current-address
        of-type integer
        being the hash-keys in (memory-cells memory)
      minimize current-address into minimum-address
      maximize current-address into maximum-address
      finally
        (return
          (values minimum-address maximum-address)))))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory      memory))
  (declare (type destination stream))
  (multiple-value-bind (minimum-address maximum-address)
      (determine-bounding-addresses memory)
    (declare (type integer minimum-address))
    (declare (type integer maximum-address))
    (loop
      for current-address
        of-type integer
        from    minimum-address
        to      maximum-address
      do
        (format stream "~&Memory[~d] = ~d" current-address
          (cell-value memory current-address)))))
