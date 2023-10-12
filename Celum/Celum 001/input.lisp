;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the realization of the input buffer, an
;; adminicular component assigned the onus of processing a standard
;; input response, provided as a character's ASCII code, in a bitwise
;; fashion, proceeding from the least significant (LSB) to the most
;; significant bit (MSB).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Input".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input ()
  ((bits
    :initarg       :bits
    :initform      #b00000000
    :type          octet
    :documentation "The byte value of the most recent user input
                    character's ASCII code.
                    ---
                    The CURSOR marks the next bit to return upon a
                    query. If all bits have been requested, the
                    EXHAUSTED-P flag assumes the value ``T'' (true).")
   (cursor
    :initform      0
    :type          (integer 0 8)
    :documentation "References the next bit from the BITS to return,
                    akin to an iterator.
                    ---
                    If the most significant bit (MSB), that is, the
                    entity at the seventh position into the BITS, has
                    been queried, the ``Input'' is regarded as
                    exhausted, for which please see the EXHAUSTED-P
                    slot.")
   (exhausted-p
    :initform      T
    :type          boolean
    :documentation "Determines whether all bits of the most recent input
                    byte BITS have been queried."))
  (:documentation
    "The ``Input'' class furnishes a bit-oriented input buffer; its
     content being specified by an unsigned byte, it applies itself to
     the castaldy of the single bits by adminiculum of a cursor that
     at any instant maintains the next bit to return, proceeding from
     the least significant (LSB) to the most significant bit (MSB)."))

;;; -------------------------------------------------------

(defun make-input ()
  "Creates and returns a new ``Input'' buffer, initially containing an
   unprocessed byte value of zero."
  (the Input
    (make-instance 'Input)))

;;; -------------------------------------------------------

(defun input-set-to (input new-bits)
  "Stores the NEW-BITS in the INPUT buffer, resets its state, and
   returns no value."
  (declare (type Input input))
  (declare (type octet new-bits))
  (with-slots (bits cursor exhausted-p) input
    (declare (type octet         bits))
    (declare (type (integer 0 8) cursor))
    (declare (type boolean       exhausted-p))
    (setf bits        new-bits)
    (setf cursor      0)
    (setf exhausted-p NIL))
  (values))

;;; -------------------------------------------------------

(defun input-exhausted-p (input)
  "Determines whether the INPUT buffer is exhausted, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Input input))
  (the boolean
    (slot-value input 'exhausted-p)))

;;; -------------------------------------------------------

(defun input-get-next-bit (input)
  "Returns the next bit from the INPUT buffer, while advancing its
   position cursor, or, upon the INPUT's exhaustion, responds with the
   default value of zero (0)."
  (declare (type Input input))
  (with-slots (bits cursor exhausted-p) input
    (declare (type octet         bits))
    (declare (type (integer 0 8) cursor))
    (declare (type boolean       exhausted-p))
    (the bit
      (if exhausted-p
        0
        (prog1
          (ldb (byte 1 cursor) bits)
          (if (< cursor 7)
            (incf cursor)
            (setf exhausted-p T)))))))
