;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the "Parse-Result" class, an entity endowed
;; with the capacity of encapsulating a parser or parser combinator's
;; response to a parse state provision.
;; 
;; The parse result's amplectation enumerates a success or failure flag
;; in order to determine the parser's or combinator's eligiblity for
;; its input state's requirements, a new or original parse state as an
;; advancement in the case of its success, and an output object, which
;; commonly delineates the parser's or combinator's contribution to the
;; ultimate produce, usually an abstract syntax tree (AST) node for the
;; entire AST.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p state output)))
  "The ``Parse-Result'' class encapsulates a ``Parser'' instance's
   response to its invocation with a ``Parse-State'', amplecting in this
   compound a success/failure flag, the subsequent parse state, in the
   case of an advancement, and the generating parser's output, which
   usually defines its contribution to the overall parsing stage's
   product."
  (succeeded-p NIL :type boolean               :read-only T)
  (state       NIL :type (or null Parse-State) :read-only T)
  (output      NIL :type T                     :read-only T))

;;; -------------------------------------------------------

(defmethod print-object ((result Parse-Result) stream)
  (declare (type Parse-Result result))
  (declare (type destination  stream))
  (format stream "(Parse-Result succeeded-p=~a output=~s)"
    (parse-result-succeeded-p result)
    (parse-result-output      result)))
