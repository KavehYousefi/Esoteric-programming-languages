;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file lends a commorancy to the diorism of the tokens as
;; significant objects extracted during the lexical analyzation stage
;; from a piece of V3i source code.
;; 
;; A token's perimeter is exhausted by its type information, the same
;; provides a categorizing aspect, and the detailing value, admitting
;; any object species.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from an evaluated piece of V3i source code."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))
