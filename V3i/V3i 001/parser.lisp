;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the parser in its paravaunt appropriation of
;; competences, in this agency constituting a recipient of the Pratt
;; parselet entities' latreutical contribution in order to crown its
;; efforts with a satisfactory accomplishment.
;; 
;; The superior potence manifested in this context is expressed,
;; a fortiori, by the collation of the personal parsing routines in a
;; concomitant spatial consanguinity with the parselet definitions;
;; their diorisms please extract from the "parselet.lisp" file, if
;; necessitated.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Stream) Block-Node)
                parse-statement-list))
(declaim (ftype (function (Token-Stream) (values))
                consume-optional-block-end))
(declaim (ftype (function (Token-Stream) AST-Node)
                parse-else-branch))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of adminicular operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-if-statement (tokens)
  "Parses a V3i \"if\" statement block utilizing the stream of TOKENS
   and returns an ``If-Node'' representation of the cnosumed content."
  (declare (type Token-Stream tokens))
  (the If-Node
    (make-if-node
      :antecedent
        (prog1
          (parse-expression 0 tokens)
          (expect-token     tokens :semicolon))
      :consequent
        (prog1
          (parse-statement-list       tokens)
          (consume-optional-block-end tokens))
      :alternative
        (when (next-token-is-of-type-p tokens :else)
          (consume-token     tokens)
          (parse-else-branch tokens)))))

;;; -------------------------------------------------------

(defun parse-else-branch (tokens)
  "Parses an V3i \"if\" statement's \"else\" branch and returns a
   covenable ``AST-Node'' representation thereof."
  (declare (type Token-Stream tokens))
  (the AST-Node
    (case (token-type (peek-token tokens))
      ;; "else if ...".
      (:if
        (consume-token      tokens)
        (parse-if-statement tokens))
      ;; "else;".
      (:semicolon
        (consume-token tokens)
        (prog1
          (parse-statement-list       tokens)
          (consume-optional-block-end tokens)))
      ;; Invalid token sequence.
      (otherwise
        (error "Unexpected token succeeding an \"else\" ~
                instruction: ~s."
          (peek-token tokens))))))

;;; -------------------------------------------------------

(defun parse-statement (tokens)
  "Parses a statement utilizing the stream of TOKENS and returns a
   covenable ``AST-Node'' representation thereof; otherwise, if not
   possible, responds with ``NIL''."
  (declare (type Token-Stream tokens))
  (let ((next-token (peek-token tokens)))
    (declare (type Token next-token))
    (the (or null AST-Node)
      (cond
        ((std-token-p next-token)
          (consume-token   tokens)
          (parse-std-token next-token tokens))
        ((nud-token-p next-token)
          (consume-token   tokens)
          (parse-nud-token next-token tokens))
        (T
          NIL)))))

;;; -------------------------------------------------------

(defun parse-statement-list (tokens)
  "Parses a sequence of zero or more statements utilizing the stream of
   TOKENS and returns a ``Block-Node'' encapsulation thereof."
  (declare (type Token-Stream tokens))
  (let ((first-statement (parse-statement tokens))
        (statements      NIL))
    (declare (type (or null AST-Node) first-statement))
    (declare (type node-list          statements))
    (when first-statement
      (loop
        for next-statement
          of-type (or null AST-Node)
          =       (parse-statement tokens)
        while next-statement
          collect next-statement
          into    subsequent-statements
        finally
          (setf statements
            (cons first-statement subsequent-statements))))
    (the Block-Node
      (make-block-node :statements statements))))

;;; -------------------------------------------------------

(defun consume-optional-semicolon (tokens)
  "Determines whether the next token obtained from the TOKENS stream
   represents a semicolon (\";\") token, on confirmation consuming the
   same, otherwise accompassing no causatum, in any case return no
   value."
  (declare (type Token-Stream tokens))
  (when (next-token-is-of-type-p tokens :semicolon)
    (consume-token tokens))
  (values))

;;; -------------------------------------------------------

(defun consume-optional-block-end (tokens)
  "Determines whether the next token obtained from the TOKENS stream
   represents the \"end;\" block terminator sequence, on confirmation
   consuming the same, otherwise accompassing no causatum, in any case
   return no value."
  (declare (type Token-Stream tokens))
  ;; Consume optional "end;" instruction.
  (when (next-token-is-of-type-p tokens :end)
    (consume-token tokens)
    (expect-token  tokens :semicolon))
  (values))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Assembles a V3i program from the sequence of TOKENS and returns a
   ``Program-Node'' representation of its statements."
  (declare (type Token-Stream tokens))
  (the Program-Node
    (make-program-node :statements
      (parse-statement-list tokens))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-nud-parselet :number
  (make-nud-parselet
    (define-nud-transformer (current-token tokens)
      (make-number-node :value
        (token-value current-token)))))

;;; -------------------------------------------------------

(register-nud-parselet :string
  (make-nud-parselet
    (define-nud-transformer (current-token tokens)
      (make-string-node :value
        (token-value current-token)))))

;;; -------------------------------------------------------

(register-unary-parselet :plus  50 :right)
(register-unary-parselet :minus 50 :right)

;;; -------------------------------------------------------

(register-nud-parselet :input
  (make-nud-parselet
    (define-nud-transformer (current-token tokens)
      (make-input-node))))

;;; -------------------------------------------------------

(register-nud-parselet :sqrt
  (make-nud-parselet
    (define-nud-transformer (current-token tokens)
      (make-sqrt-node :argument
        (parse-expression 0 tokens)))))

;;; -------------------------------------------------------

(register-nud-parselet :var
  (make-nud-parselet
    (define-nud-transformer (current-token tokens)
      (make-variable-call-node :name
        (token-value
          (expect-token tokens :variable))))))

;;; -------------------------------------------------------

(register-nud-parselet :variable
  (make-nud-parselet
    (define-nud-transformer (current-token tokens)
      (make-variable-node :name
        (token-value current-token)))))

;;; -------------------------------------------------------

(register-binary-parselet :equal-to     10 :left)
(register-binary-parselet :less-than    20 :left)
(register-binary-parselet :greater-than 20 :left)

(register-binary-parselet :plus         30 :left)
(register-binary-parselet :minus        30 :left)
(register-binary-parselet :times        40 :left)
(register-binary-parselet :divided      40 :left)
(register-binary-parselet :remainder    40 :left)

;;; -------------------------------------------------------

(register-statement :def
  (define-std-transformer (current-token tokens)
    (the Variable-Definition-Node
      (prog1
        (make-variable-definition-node
          :name  (token-value (expect-token tokens :variable))
          :value (parse-expression 0 tokens))
        (expect-token tokens :semicolon)))))

;;; -------------------------------------------------------

(register-statement :if
  (define-std-transformer (current-token tokens)
    (parse-if-statement tokens)))

;;; -------------------------------------------------------

(register-statement :iloop
  (define-std-transformer (current-token tokens)
    (consume-optional-semicolon tokens)
    (the Infinite-Loop-Node
      (make-infinite-loop-node :statements
        (prog1
          (parse-statement-list       tokens)
          (consume-optional-block-end tokens))))))

;;; -------------------------------------------------------

(register-statement :loop
  (define-std-transformer (current-token tokens)
    (let ((repetitions (parse-expression 0 tokens)))
      (declare (type Expression-Node repetitions))
      (expect-token tokens :semicolon)
      (let ((statements (parse-statement-list tokens)))
        (declare (type Block-Node statements))
        (consume-optional-block-end tokens)
        (the Finite-Loop-Node
          (make-finite-loop-node
            :repetitions repetitions
            :statements  statements))))))

;;; -------------------------------------------------------

(register-statement :print
  (define-std-transformer (current-token tokens)
    (the Print-Node
      (prog1
        (make-print-node :arguments
          (parse-expression-list 0 tokens))
        (expect-token tokens :semicolon)))))

;;; -------------------------------------------------------

(register-statement :var
  (define-std-transformer (current-token tokens)
    (the Variable-Call-Node
      (prog1
        (make-variable-call-node
          :name (token-value (expect-token tokens :variable)))
        (expect-token tokens :semicolon)))))

;;; -------------------------------------------------------

(register-statement :wait
  (define-std-transformer (current-token tokens)
    (the Wait-Node
      (prog1
        (make-wait-node :delay
          (parse-expression 0 tokens))
        (expect-token tokens :semicolon)))))
