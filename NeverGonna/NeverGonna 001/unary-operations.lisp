;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the unary operations, both arithmetical and
;; logical in their nature, applicable unto the "NGObject" operands in
;; all efforts to evaluate expressions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-unary-operator (operator operand)
  (:documentation
    "Applies the unary OPERATOR to the OPERAND and returns a result
     appropriate for this combination."))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :plus))
                                 (operand  NGInteger))
  "Returns a new ``NGInteger'' whose ensconced value constitutes an
   unaltered modification of the OPERAND's integral datum."
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type NGInteger      operand))
  (the NGInteger
    (make-nginteger
      (+ (ngobject-value operand)))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :minus))
                                 (operand  NGInteger))
  "Returns a new ``NGInteger'' which represents the integeral OPERAND's
   value with a negated signum."
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type NGInteger      operand))
  (the NGInteger
    (make-nginteger
      (- (ngobject-value operand)))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :plus))
                                 (operand  NGBoolean))
  "Returns a new ``NGBoolean'' whose Boolean truth value amounts to the
   negated variant of its OPERAND."
  (declare (type unary-operator operator))
  (declare (ignore              operator))
  (declare (type NGBoolean      operand))
  (the NGBoolean
    (make-ngboolean
      (not (ngobject-value operand)))))
