;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the unary and binary operations incarnated in
;; the V3i programming language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary operators.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-unary-operator (operator operand)
  (:documentation
    "Applies the unary OPERATOR to the OPERAND and returns a value
     connable for this combination.")
  
  (:method ((operator (eql :plus)) (operand integer))
    (declare (type unary-operator operator))
    (declare (ignore              operator))
    (declare (type integer        operand))
    (the integer operand))
  
  (:method ((operator (eql :minus)) (operand integer))
    (declare (type unary-operator operator))
    (declare (ignore              operator))
    (declare (type integer        operand))
    (the integer
      (- operand))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operators.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and the
     RIGHT-OPERAND in this exact order and returns a value connable for
     this combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operation
    (operator (left-operand-type right-operand-type)
     &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operator'', its first formal parameter's agnomination
   established as ``$operator'' and dispatching on ``eql''-equality with
   the OPERATOR, its second input being norned ``$left'', specializing
   on the LEFT-OPERAND-TYPE, the third being ``$right'' with a
   specialization on the RIGHT-OPERAND-TYPE, the method statements
   desumed from the BODY, returning the desinent BODY forms results."
  `(defmethod apply-binary-operator
       (($operator (eql ,operator))
        ($left     ,left-operand-type)
        ($right    ,right-operand-type))
     (declare (type binary-operator     $operator))
     (declare (ignorable                $operator))
     (declare (type ,left-operand-type  $left))
     (declare (ignorable                $left))
     (declare (type ,right-operand-type $right))
     (declare (ignorable                $right))
     ,@body))

;;; -------------------------------------------------------

(define-binary-operation :plus (integer integer)
  (the integer
    (+ $left $right)))

;;; -------------------------------------------------------

(define-binary-operation :minus (integer integer)
  (the integer
    (- $left $right)))

;;; -------------------------------------------------------

(define-binary-operation :times (integer integer)
  (the integer
    (* $left $right)))

;;; -------------------------------------------------------

(define-binary-operation :divided (integer integer)
  (the integer
    (nth-value 0
      (floor $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :remainder (integer integer)
  (the integer
    (nth-value 0
      (rem $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (integer integer)
  (the bit
    (get-binary-truth-value-of
      (= $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (integer string)
  (the bit
    (the bit
      (apply-binary-operator $operator $left
        (parse-integer $right)))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (string string)
  (the bit
    (get-binary-truth-value-of
      (string= $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (string integer)
  (the bit
    (get-binary-truth-value-of
      (ignore-errors
        (apply-binary-operator $operator
          (parse-integer $left)
          $right)))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (T T)
  (the bit 0))

;;; -------------------------------------------------------

(define-binary-operation :less-than (integer integer)
  (the bit
    (get-binary-truth-value-of
      (< $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (integer string)
  (the bit
    (get-binary-truth-value-of
      (ignore-errors
        (apply-binary-operator $operator $left
          (parse-integer $right))))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (string string)
  (the bit
    (get-binary-truth-value-of
      (string< $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (string integer)
  (the bit
    (get-binary-truth-value-of
      (ignore-errors
        (apply-binary-operator $operator
          (parse-integer $left)
          $right)))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (T T)
  (the bit 0))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (integer integer)
  (the bit
    (get-binary-truth-value-of
      (> $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (integer string)
  (the bit
    (get-binary-truth-value-of
      (ignore-errors
        (apply-binary-operator $operator $left
          (parse-integer $right))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (string string)
  (the bit
    (get-binary-truth-value-of
      (string> $left $right))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (string integer)
  (the bit
    (get-binary-truth-value-of
      (ignore-errors
        (apply-binary-operator $operator
          (parse-integer $left)
          $right)))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (T T)
  (the bit 0))
