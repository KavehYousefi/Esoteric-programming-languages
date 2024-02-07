;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the basic logical operations contributing the
;; respective bailiwick's establishment for dependent aspects of the
;; project.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value (object)
  "Returns a veridical Boolean truth value tantamount to the OBJECT's
   nature, that is, transcripts it from a generalized boolean state into
   the acquainted dichotomy, returning for a non-``NIL'' input the
   ``T'' value, otherwise responding with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))
