;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the binary operations, both arithmetical and
;; logical in their nature, applicable unto the "NGObject" operands in
;; all efforts to evaluate expressions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and the
     RIGHT-OPERAND and returns a result appropriate for this
     combination."))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGInteger
    (make-nginteger
      (+ (ngobject-value left-operand)
         (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  NGInteger)
                                  (right-operand NGString))
  "Returns a new ``NGString'' produced by prepending the LEFT-OPERAND in
   in its textual form to the verbatim RIGHT-OPERAND."
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGString        right-operand))
  (the NGString
    (make-ngstring
      (format NIL "~a~a"
        (ngobject-value left-operand)
        (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  NGString)
                                  (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGString
    (make-ngstring
      (format NIL "~a~a"
        (ngobject-value left-operand)
        (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :plus))
                                  (left-operand  NGString)
                                  (right-operand NGInteger))
  "Returns a new ``NGString'' produced by prepending the verbatim
   LEFT-OPERAND to the RIGHT-OPERAND in its textual form."
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGInteger       right-operand))
  (the NGString
    (make-ngstring
      (format NIL "~a~A"
        (ngobject-value right-operand)
        (ngobject-value left-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :minus))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGInteger
    (make-nginteger
      (- (ngobject-value left-operand)
         (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :times))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGInteger
    (make-nginteger
      (* (ngobject-value left-operand)
         (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :times))
                                  (left-operand  NGInteger)
                                  (right-operand NGString))
  "Returns a new ``NGString'' generated through a repetition of the
   RIGHT-OPERAND for a LEFT-OPERAND number of times."
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGString        right-operand))
  (the NGString
    (make-ngstring
      (format NIL "~v@{~a~:*~}"
        (ngobject-value left-operand)
        (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :times))
                                  (left-operand  NGString)
                                  (right-operand NGInteger))
  "Returns a new ``NGString'' generated through a repetition of the
   LEFT-OPERAND for a RIGHT-OPERAND number of times."
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGInteger       right-operand))
  (the NGString
    (make-ngstring
      (format NIL "~v@{~a~:*~}"
        (ngobject-value right-operand)
        (ngobject-value left-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :divided))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGInteger
    (make-nginteger
      (round
        (ngobject-value left-operand)
        (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :remainder))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGInteger
    (make-nginteger
      (mod
        (ngobject-value left-operand)
        (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :power))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGInteger
    (make-nginteger
      (expt
        (ngobject-value left-operand)
        (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal))
                                  (left-operand  NGBoolean)
                                  (right-operand NGBoolean))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGBoolean       left-operand))
  (declare (type NGBoolean       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (eq (ngobject-value left-operand)
            (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (= (ngobject-value left-operand)
           (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal))
                                  (left-operand  NGObject)
                                  (right-operand NGObject))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGObject        left-operand))
  (declare (ignore               left-operand))
  (declare (type NGObject        right-operand))
  (declare (ignore               right-operand))
  (the NGBoolean
    (make-ngboolean NIL)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :equal))
                                  (left-operand  NGString)
                                  (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (string=
          (ngobject-value left-operand)
          (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :not-equal))
                                  (left-operand  NGBoolean)
                                  (right-operand NGBoolean))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGBoolean       left-operand))
  (declare (type NGBoolean       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not
        (eq (ngobject-value left-operand)
            (ngobject-value right-operand))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :not-equal))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (/= (ngobject-value left-operand)
            (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :not-equal))
                                  (left-operand  NGObject)
                                  (right-operand NGObject))
  (declare (type binary-operator operator))
  (declare (type NGObject        left-operand))
  (declare (ignore               left-operand))
  (declare (type NGObject        right-operand))
  (declare (ignore               right-operand))
  (the NGBoolean
    (make-ngboolean T)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :not-equal))
                                  (left-operand  NGString)
                                  (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (string/=
          (ngobject-value left-operand)
          (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :greater-than))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (> (ngobject-value left-operand)
           (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :greater-than))
                                  (left-operand  NGObject)
                                  (right-operand NGObject))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGObject        left-operand))
  (declare (ignore               left-operand))
  (declare (type NGObject        right-operand))
  (declare (ignore               right-operand))
  (the NGBoolean
    (make-ngboolean NIL)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :greater-than))
                                  (left-operand  NGString)
                                  (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (string>
          (ngobject-value left-operand)
          (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator
    ((operator      (eql :greater-or-equal))
     (left-operand  NGInteger)
     (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (>= (ngobject-value left-operand)
            (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator
    ((operator      (eql :greater-or-equal))
     (left-operand  NGObject)
     (right-operand NGObject))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGObject        left-operand))
  (declare (ignore               left-operand))
  (declare (type NGObject        right-operand))
  (declare (ignore               right-operand))
  (the NGBoolean
    (make-ngboolean NIL)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator
    ((operator      (eql :greater-or-equal))
     (left-operand  NGString)
     (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (string>=
          (ngobject-value left-operand)
          (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-than))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (< (ngobject-value left-operand)
           (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-than))
                                  (left-operand  NGObject)
                                  (right-operand NGObject))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGObject        left-operand))
  (declare (ignore               left-operand))
  (declare (type NGObject        right-operand))
  (declare (ignore               right-operand))
  (the NGBoolean
    (make-ngboolean NIL)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-than))
                                  (left-operand  NGString)
                                  (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (string<
          (ngobject-value left-operand)
          (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-or-equal))
                                  (left-operand  NGInteger)
                                  (right-operand NGInteger))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGInteger       left-operand))
  (declare (type NGInteger       right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (<= (ngobject-value left-operand)
            (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-or-equal))
                                  (left-operand  NGObject)
                                  (right-operand NGObject))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGObject        left-operand))
  (declare (ignore               left-operand))
  (declare (type NGObject        right-operand))
  (declare (ignore               right-operand))
  (the NGBoolean
    (make-ngboolean NIL)))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :less-or-equal))
                                  (left-operand  NGString)
                                  (right-operand NGString))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGString        left-operand))
  (declare (type NGString        right-operand))
  (the NGBoolean
    (make-ngboolean
      (not (null
        (string<=
          (ngobject-value left-operand)
          (ngobject-value right-operand)))))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :logical-and))
                                  (left-operand  NGBoolean)
                                  (right-operand NGBoolean))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGBoolean       left-operand))
  (declare (type NGBoolean       right-operand))
  (the NGBoolean
    (make-ngboolean
      (and (ngobject-value left-operand)
           (ngobject-value right-operand)))))

;;; -------------------------------------------------------

(defmethod apply-binary-operator ((operator      (eql :logical-or))
                                  (left-operand  NGBoolean)
                                  (right-operand NGBoolean))
  (declare (type binary-operator operator))
  (declare (ignore               operator))
  (declare (type NGBoolean       left-operand))
  (declare (type NGBoolean       right-operand))
  (the NGBoolean
    (make-ngboolean
      (or (ngobject-value left-operand)
          (ngobject-value right-operand)))))
