;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the token species as the encapsulation of a
;; significant object extracted from a piece of ``` source code
;; communicated in the form of a string.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from a piece of `` source code."
  (type  (error "Missing token type.")
         :type      keyword
         :read-only T)
  (value (error "Missing token value.")
         :type      T
         :read-only T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))
