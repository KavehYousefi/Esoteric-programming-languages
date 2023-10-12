;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the provision of the elementary cellular
;; automaton, an implement for the infinite bit tape's manipulation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of elementary cellular automaton.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-rule-output (rule neighborhood)
  "Returns the bit associated with the three-bit NEIGHBORHOOD in the
   elementary cellular automaton RULE."
  (declare (type automaton-rule    rule))
  (declare (type cell-neighborhood neighborhood))
  (the bit
    (ldb (byte 1 neighborhood) rule)))
