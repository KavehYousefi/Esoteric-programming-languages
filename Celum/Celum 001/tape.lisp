;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the implementation of the bilaterally infinite
;; tape of bit cells, which contributes a Celum program's chief memory
;; component.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer bit)
    :documentation "Maps each cell index to a single bit.")
   (start-index
    :initform      0
    :type          (integer * 0)
    :documentation "The most negative cell index to consider.")
   (end-index
    :initform      0
    :type          (integer 0 *)
    :documentation "The most positive cell index to consider."))
  (:documentation
    "The ``Tape'' class models the Celum program memory as a bilaterally
     infinite sparse vector of bit-valued cells, realized in the form of
     a hash table whose keys furnish the cell indices, associating with
     bits as the cell values."))

;;; -------------------------------------------------------

(defun make-tape (&optional (initial-size 9))
  "Creates and returns a new ``Tape'' with the optional INITIAL-SIZE
   utilized as a key in the determination of the most negative and most
   positive index into the tape."
  (declare (type (integer 0 *) initial-size))
  (let ((tape (make-instance 'Tape)))
    (declare (type Tape tape))
    (with-slots (cells start-index end-index) tape
      (declare (type (hash-table-of integer bit) cells))
      (declare (type (integer * 0)               start-index))
      (declare (type (integer 0 *)               end-index))
      (setf cells (make-hash-table :test #'eql :size initial-size))
      ;; Determine how many cells are located left (on the negative
      ;; axis) and right (on the positive axis) of the middle bit.
      (let ((number-of-cells-per-axis (floor initial-size 2)))
        (declare (type (integer 0 *) number-of-cells-per-axis))
        (setf start-index
          (if (evenp initial-size)
            (1+ (- number-of-cells-per-axis))
            (- number-of-cells-per-axis)))
        (setf end-index number-of-cells-per-axis)))
    (the Tape tape)))

;;; -------------------------------------------------------

(defun tape-length (tape)
  "Returns the tally of actually defined cells in this conceptually
   infinite TAPE."
  (declare (type Tape tape))
  (the (integer 0 *)
    (1+ (abs (- (slot-value tape 'start-index)
                (slot-value tape 'end-index))))))

;;; -------------------------------------------------------

(defun tape-update-indices (tape cell-index)
  "Updates the TAPE's internally managed start and end indices in order
   to ascertain the CELL-INDEX' inclusion and returns no value."
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (with-slots (start-index end-index) tape
    (declare (type (integer * 0) start-index))
    (declare (type (integer 0 *) end-index))
    (setf start-index (min start-index cell-index))
    (setf end-index   (max end-index   cell-index)))
  (values))

;;; -------------------------------------------------------

(defun tape-cell-at (tape cell-index)
  "Returns the bit stored in the TAPE cell located at the CELL-INDEX."
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (tape-update-indices tape cell-index)
  (the bit
    (gethash cell-index (slot-value tape 'cells) 0)))

;;; -------------------------------------------------------

(defun (setf tape-cell-at) (new-value tape cell-index)
  "Sets the value of the TAPE cell located at the CELL-INDEX to the
   NEW-VALUE and returns no value."
  (declare (type bit     new-value))
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (setf (gethash cell-index (slot-value tape 'cells) 0) new-value)
  (tape-update-indices tape cell-index)
  (values))

;;; -------------------------------------------------------

(defun tape-middle-cell (tape)
  "Returns the bit in the TAPE's middle cell."
  (declare (type Tape tape))
  (the bit (tape-cell-at tape 0)))

;;; -------------------------------------------------------

(defun (setf tape-middle-cell) (new-value tape)
  "Sets the TAPE's middle cell to the NEW-VALUE and returns no value."
  (declare (type bit  new-value))
  (declare (type Tape tape))
  (setf (tape-cell-at tape 0) new-value)
  (values))

;;; -------------------------------------------------------

(defun tape-invert-middle-cell (tape)
  "Inverts the bit stored in the TAPE's middle cell and returns the new
   value."
  (declare (type Tape tape))
  (setf (tape-middle-cell tape)
        (- 1 (tape-middle-cell tape)))
  (the bit
    (tape-middle-cell tape)))

;;; -------------------------------------------------------

(defun tape-copy-cells-from (target-tape source-tape)
  "Transfers the SOURCE-TAPE's cell states into the TARGET-TAPE,
   destructively modifying the latter, while the former remains
   unaltered, and finally returns no value."
  (declare (type Tape target-tape))
  (declare (type Tape source-tape))
  (loop
    for cell-index
      of-type integer
      from    (slot-value target-tape 'start-index)
      to      (slot-value target-tape 'end-index)
    do
      (setf (tape-cell-at target-tape cell-index)
        (tape-cell-at source-tape cell-index)))
  (values))

;;; -------------------------------------------------------

(defun build-neighborhood (tape cell-index)
  "Builds and returns for the TAPE cell located at the CELL-INDEX the
   three-bit neighborhood, composed of its immediate sinistral and
   dextral neighbors, the former of which provides the most significant
   bit, the latter its least significant, while the processed cell
   itself acts in the interstition."
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (let ((neighborhood #b000))
    (declare (type cell-neighborhood neighborhood))
    ;; Left neighbor cell bit.
    (setf (ldb (byte 1 2) neighborhood)
          (tape-cell-at tape (1- cell-index)))
    ;; Cell bit at the CELL-INDEX.
    (setf (ldb (byte 1 1) neighborhood)
          (tape-cell-at tape cell-index))
    ;; Right neighbor cell bit.
    (setf (ldb (byte 1 0) neighborhood)
          (tape-cell-at tape (1+ cell-index)))
    (the cell-neighborhood neighborhood)))

;;; -------------------------------------------------------

(defun apply-elementary-cellular-automaton (rule current-tape)
  "Applies the elementary cellular automaton RULE on the CURRENT-TAPE by
   calculating the next generation based upon the contemporary states,
   ere updating the CURRENT-TAPE, and returns no value."
  (declare (type automaton-rule rule))
  (declare (type Tape           current-tape))
  (let ((new-tape (make-tape (tape-length current-tape))))
    (declare (type Tape new-tape))
    (with-slots (cells start-index end-index) new-tape
      (declare (type (hash-table-of integer bit) cells))
      (declare (type (integer * 0)               start-index))
      (declare (type (integer 0 *)               end-index))
      (loop
        for cell-index
          of-type integer
          from    start-index
          to      end-index
        do
          (setf (tape-cell-at new-tape cell-index)
                (get-rule-output rule
                  (build-neighborhood current-tape cell-index))))
      (tape-copy-cells-from current-tape new-tape)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((tape Tape) stream)
  (declare (type Tape        tape))
  (declare (type destination stream))
  (with-slots (cells start-index end-index) tape
    (declare (type (hash-table-of integer bit) cells))
    (declare (type (integer * 0)               start-index))
    (declare (type (integer 0 *)               end-index))
    (format stream "Tape(length=~d, start=~d, end=~d, cells="
      (tape-length tape) start-index end-index)
    (loop
      for cell-index
        of-type integer
        from    start-index
        to      end-index
      do
        ;; The center cell (CELL-INDEX = 0) shall be emphasized.
        (if (zerop cell-index)
          (format stream "[~d]" (gethash cell-index cells 0))
          (format stream "~d"   (gethash cell-index cells 0))))
    (format stream ")")))
