;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the "Parse-State" class, an encapsulation of
;; the entire parsing activity's progress, each such serving to furnish
;; a parser or parser combinator with a reference to the current
;; state's intelligence, its telos the acquisition of a response, the
;; "Parse-Result", in order to further advance the process.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-state-current-element (parse-state)
  (:documentation
    "Returns the PARSE-STATE's element."))

;;; -------------------------------------------------------

(defgeneric parse-state-advance (parse-state)
  (:documentation
    "Returns a new ``Parse-State'' which represents an advanced version
     of the PARSE-STATE."))

;;; -------------------------------------------------------

(defstruct Parse-State
  "The ``Parse-State'' class encapsulates the parsing activity's
   progress, its amplectation enumerating the gradually populated token
   queue, the currently processed token's node reference in the former,
   and the lexer whose dever assigns it to the token purveyance.
   ---
   The parse state's paravaunt piece of information, its evaluated
   token, is admitted to perquisitions by adminiculum of the token queue
   node cursor, a reference to the token object."
  (lexer  (error "Missing lexer.")
          :type      Lexer
          :read-only T)
  (tokens (error "Missing token queue.")
          :type      Token-Queue
          :read-only T)
  (cursor NIL
          :type      (or null SLNode)
          :read-only T))

;;; -------------------------------------------------------

(defun make-initial-parse-state (lexer)
  "Creates and returns a ``Parse-State'' conable as the parsing process'
   inchoation, receiving its token from the LEXER, the same it may
   propagate to its successor states."
  (declare (type Lexer lexer))
  (let ((tokens (make-token-queue)))
    (declare (type Token-Queue tokens))
    (the Parse-State
      (make-parse-state
        :lexer  lexer
        :tokens tokens
        :cursor (token-queue-add-last tokens
                  (lexer-get-next-token lexer))))))

;;; -------------------------------------------------------

(defmethod parse-state-current-element ((parse-state Parse-State))
  "Returns the token maintained by this PARSE-STATE."
  (declare (type Parse-State parse-state))
  (the Token
    (slnode-element
      (parse-state-cursor parse-state))))

;;; -------------------------------------------------------

(defmethod parse-state-advance ((parse-state Parse-State))
  "Returns a new ``Parse-State'' which ensconces the token immediately
   succeeding this PARSE-STATE's object in the shared token queue."
  (declare (type Parse-State parse-state))
  (let ((successor-node
          (token-queue-get-successor
            (parse-state-tokens parse-state)
            (parse-state-cursor parse-state))))
    (declare (type (or null SLNode) successor-node))
    ;; If the input PARSE-STATE's cursor constitutes the desinence of
    ;; the token queue TOKENS, query the next token from the LEXER and
    ;; append it to the token queue's rear, preparing it for the
    ;; NEW-PARSE-STATE's cursor.
    (unless successor-node
      (setf successor-node
        (token-queue-add-last
          (parse-state-tokens parse-state)
          (lexer-get-next-token
            (parse-state-lexer parse-state)))))
    ;; Utilize the successor node of the input PARSE-STATE's cursor in
    ;; the shared token queue TOKENS as the NEW-PARSE-STATE's lexer,
    ;; thus storing the next token in the queue into it.
    (the Parse-State
      (make-parse-state
        :lexer  (parse-state-lexer  parse-state)
        :tokens (parse-state-tokens parse-state)
        :cursor successor-node))))
