;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the logical operations whose competences
;; establish an entreparted requisite to the entire project.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its aspects as a \"generalized boolean\"
   designator and produces a veridicous Boolean paregal thereof,
   returning for any non-``NIL'' OBJECT a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' input, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))
