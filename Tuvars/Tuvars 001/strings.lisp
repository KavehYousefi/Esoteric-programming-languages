;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the commonly deployed string operations and
;; macros.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string case switch facility.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-clause-p (clause)
  "Determines whether the CLAUSE represents a default clause, the
   diorism of which entails the symbols ``otherwise'' and ``T'',
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type list clause))
  (the boolean
    (get-boolean-value
      (and (listp clause)
           (symbolp (first clause))
           (member  (first clause) '(otherwise T) :test #'eq)))))

;;; -------------------------------------------------------

(defmacro string-case (key-form &rest clauses)
  "Realizes a conditional execution based upon a KEY-FORM's conformance
   with the key of the CLAUSES, probing the latter in their specified
   order, with an optional contingency for a default case by adminiculum
   of the ``otherwise'' or ``T'' key, evaluating the matching clause's
   forms and returning the desinent form's result, or, if none could be
   equiparated, responding with ``NIL''."
  (let ((evaluated-key-form (gensym)))
    (declare (type symbol evaluated-key-form))
    `(let ((,evaluated-key-form ,key-form))
       (declare (type string ,evaluated-key-form))
       (cond
         ,@(loop
             for clause of-type list in clauses
             if (default-clause-p clause)
               collect `(T ,@(cdr clause))
             else
               collect
                 `((string= ,evaluated-key-form ,(car clause))
                    ,@(cdr clause)))))))
