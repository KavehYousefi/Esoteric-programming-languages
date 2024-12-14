;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the reference notion, an encapsulation of a
;; variable identifier pursuing a discrimination betwixt these
;; succedanea and the syntactically tantamount string objects.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable reference.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Reference
  (:constructor make-reference (name)))
  "The ``Reference'' class serves in the encapsulation of a variable
   name in a dedicated object, capacitating its distiguishment from a
   string object."
  (name (error "Missing reference name.")
        :type      variable-name
        :read-only T))
