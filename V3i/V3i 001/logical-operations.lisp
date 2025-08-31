;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the foundational logical operations offering
;; their utility to several departments of the project.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Produces a veridicous Boolean equivalency of the OBJECT founded upon
   its construe as a \"generalized boolean\", returning for a
   non-``NIL'' input a ``boolean'' value of ``T'', otherwise, for a
   ``NIL'' OBJECT, the ``NIL'' constant."
  (declare (type T object))
  (the boolean
    (not (null object))))

;;; -------------------------------------------------------

(defun get-binary-truth-value-of (object)
  "Produces a binary truth value equivalency of the OBJECT founded upon
   its construe as a \"generalized boolean\", returning for a
   non-``NIL'' input a bit value of one (1), otherwise, for a ``NIL''
   OBJECT, zero (0)."
  (declare (type T object))
  (the bit
    (or (and object 1)
        0)))

;;; -------------------------------------------------------

(defun binary-value-is-true-p (numeric-truth-value)
  "Determines whether the NUMERIC-TRUTH-VALUE concurs in its
   interpretation with the Boolean \"true\" sentinel, which shall be the
   case for any non-zero numeric input, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type real numeric-truth-value))
  (the boolean
    (not (zerop numeric-truth-value))))
