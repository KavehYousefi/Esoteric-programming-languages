;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the expression parser with its speciality
;; kithing in the Pratt parser component, a warklume dedicated to the
;; eath procession of infix operations, while adhibiting due respect to
;; their precedences.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of precedence.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence (binding-power associativity)))
  "The ``Precedence'' class encapsulates the information requisite for
   the representation of a nud or led token's binding power and
   associativity property."
  (binding-power 0     :type integer)
  (associativity :none :type associativity))

;;; -------------------------------------------------------

(defun get-effective-binding-power (precedence)
  "Returns the PRECEDENCE's effective binding power, which takes into
   account both the nominal puissance and the associativity."
  (declare (type Precedence precedence))
  (the integer
    (case (precedence-associativity precedence)
      ((:none :left)
        (precedence-binding-power precedence))
      (:right
        (1- (precedence-binding-power precedence)))
      (otherwise
        (error "Invalid associativity: ~s."
          (precedence-associativity precedence))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselets.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Parselet
  "The ``Parselet'' interface establishes a common foundry for the nud
   and led parselet specializations, the former of which, at least
   contemporaneously, does not veridically apply itself to the project's
   circumference.")

;;; -------------------------------------------------------

(defstruct (Led-Parselet
  (:include     Parselet)
  (:constructor make-led-parselet (processor precedence)))
  "The ``Led-Parselet'' class represents an entity capacitated to parse
   a led token in conjunction with an already extant left expression in
   order to produce a new expression form thereof.
   ---
   Most commonly, led parselets serve in the realization of infix and
   postfix operations, the former exemplified by addition and
   exponentiation, the latter amplecting, among others, post-increment,
   post-decrement, as well as array indices access."
  (processor  (error "Missing processor.")  :type led-processor)
  (precedence (error "Missing precedence.") :type precedence))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet registries.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyowrd Led-Parselet) +LED-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +LED-PARSELETS+
  (make-hash-table :test #'eql)
  "Associates the recognized led token types with ``Led-Parselet''
   objects responsible for their parsing.")

;;; -------------------------------------------------------

(defun register-led-parselet (token-type led-parselet)
  "Associates the TOKEN-TYPE with the LED-PARSELET in the
   +LED-PARSELETS+ hash table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Led-Parselet led-parselet))
  (setf (gethash token-type +LED-PARSELETS+) led-parselet)
  (values))

;;; -------------------------------------------------------

(defun led-token-p (token)
  "Determines whether the TOKEN represents a led token, returning on
   confirmation on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +LED-PARSELETS+))))))

;;; -------------------------------------------------------

(defun get-led-binding-power (token)
  "Returns the led TOKEN's binding power."
  (declare (type Token token))
  (the integer
    (get-effective-binding-power
      (led-parselet-precedence
        (or (gethash (token-type token) +LED-PARSELETS+)
            (error "No led token: ~s." token))))))

;;; -------------------------------------------------------

(defun get-led-processor (token)
  "Returns the processor function responsible for parsing the led TOKEN."
  (declare (type Token token))
  (the function
    (led-parselet-processor
      (or (gethash (token-type token) +LED-PARSELETS+)
          (error "No led token: ~s." token)))))

;;; -------------------------------------------------------

(defun get-led-parser (token left-expression)
  "Returns the ``Parser'' responsible for parsing the led TOKEN, using
   the LEFT-EXPRESSION as the sinistral operand in order to produce a
   result."
  (declare (type Token token))
  (declare (type T     left-expression))
  (the Parser
    (funcall
      (get-led-processor token)
      token
      left-expression)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Pratt expression parser.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-nud ()
  "Returns a new ``Parser'' which succeeds if its probed token
   represents a nud token, on confirmation returning a ``Parser''
   endowed with the competence to parse its ``Parse-State'' object's nud
   token in order to produce the expression form associated with the nud
   token."
  (the Parser
    (match-any-of
      (match-input-command)
      
      ;; Signed (plus or minus) expression.
      (let-parser* ((sign
                      (match-any-of
                        (match-token-of-type :plus)
                        (match-token-of-type :minus)))
                    (operand
                      (parse-pratt 
                        (get-effective-binding-power
                          (make-precedence 170 :right)))))
        (declare (type Token    sign))
        (declare (type AST-Node operand))
        (return-output
          (make-unary-operation-node
            :operator (token-type sign)
            :operand  operand)))
      
      ;; Logical not.
      (match-chain
        (match-token-of-type :logical-not)
        (let-parser (operand
                      (parse-pratt 
                        (get-effective-binding-power
                          (make-precedence 170 :right))))
          (return-output
            (make-unary-operation-node
              :operator :logical-not
              :operand  operand))))
      
      ;; Parenthesized (grouped) expression.
      (match-chain
        (match-token-of-type :left-parenthesis)
        (let-parser (expression (match-expression))
          (match-chain
            (match-token-of-type :right-parenthesis)
            (return-output
              (make-group-node :body expression)))))
      
      (match-boolean-literal)
      (match-number-literal)
      (match-string-literal)
      (match-variable))))

;;; -------------------------------------------------------

(defun probe-led-token (competing-binding-power)
  "Returns a new ``Parser'' which succeeds if the token provided by its
   ``Parse-State'' represents a led token which is associated with a
   binding power strictly greater than the COMPETING-BINDING-POWER, on
   confirmation returning in its ``Parse-Result'' the probed token."
  (declare (type integer competing-binding-power))
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type Token token))
          (and (led-token-p token)
               (> (get-led-binding-power token)
                  competing-binding-power))))))

;;; -------------------------------------------------------

(defun parse-led (competing-binding-power left-expression)
  "Returns a new ``Parser'' which succeeds if its probed token
   represents a led token with a binding power strictly greater than the
   COMPETING-BINDING-POWER, on confirmation returning the ``Parser''
   responsible for the parsing this led token in order to produce an
   expression form associated with the led token."
  (declare (type integer  competing-binding-power))
  (declare (type AST-Node left-expression))
  (the Parser
    (let-parser (operator-token
                  (probe-led-token competing-binding-power))
      (declare (type Token operator-token))
      (get-led-parser operator-token left-expression))))

;;; -------------------------------------------------------

(defun parse-pratt (current-binding-power)
  "Parses an expression using Pratt's principles, with the invoking
   context represented by the CURRENT-BINDING-POWER, and returns an
   ``AST-Node'' representation thereof."
  (declare (type integer current-binding-power))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          
          (let ((nud-result
                  (apply-parser
                    (match-padded
                      (match-any-of
                        (parse-nud)
                        (fail-parsing "No nud token.")))
                    state)))
            (declare (type Parse-Result nud-result))
            
            (if (parse-result-succeeded-p nud-result)
              (let ((left-expression
                      (parse-result-output nud-result)))
                (declare (type T left-expression))
                
                (loop
                  for new-state
                    of-type Parse-State
                    =       (parse-result-state nud-result)
                    then    (parse-result-state led-candidate-result)
                  
                  ;; Attempt to match the next parser, which will
                  ;; succeed if the following criteria are satisfied:
                  ;;   (1) The next token represets a led token.
                  ;;   (2) The binding power associated with this
                  ;;       led token is strictly greater than the
                  ;;       CURRENT-BINDING-POWER.
                  for led-candidate-parser
                    of-type Parser
                    =       (match-padded
                              (parse-led current-binding-power
                                    left-expression))
                  
                  ;; Obtain the parse result from the probed
                  ;; LED-CANDIDATE-PARSER, which might or might not have
                  ;; matched given the two criteria stated above.
                  for led-candidate-result
                    of-type Parse-Result
                    =       (apply-parser led-candidate-parser
                                          new-state)
                  
                  ;; If the LED-CANDIDATE-PARSER has matched, update the
                  ;; LEFT-EXPRESSION to its parse result output.
                  if (parse-result-succeeded-p led-candidate-result) do
                    (setf left-expression
                      (parse-result-output led-candidate-result))
                  ;; If the LED-CANDIDATE-PARSER has not matched, either
                  ;; because the probed token was not recognized as a
                  ;; led token, or because its binding power was not
                  ;; greater than the CURRENT-BINDING-POWER, terminate
                  ;; the current Pratt expression parsing process.
                  else do
                    (loop-finish)
                  end
                  
                  finally
                    (return
                      (make-parse-result T new-state left-expression))))
              
              ;; No led (consequent) token follows?
              ;; => Return parsed nud (initial) token's node itself.
              nud-result))))))

;;; -------------------------------------------------------

(register-led-parselet :logical-or
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 40 :left)))

;;; -------------------------------------------------------

(register-led-parselet :logical-and
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 50 :left)))

;;; -------------------------------------------------------

(register-led-parselet :plus
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 130 :left)))

;;; -------------------------------------------------------

(register-led-parselet :minus
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 130 :left)))

;;; -------------------------------------------------------

(register-led-parselet :times
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 140 :left)))

;;; -------------------------------------------------------

(register-led-parselet :divided
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 140 :left)))

;;; -------------------------------------------------------

(register-led-parselet :remainder
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 140 :left)))

;;; -------------------------------------------------------

(register-led-parselet :power
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 150 :right)))

;;; -------------------------------------------------------

(register-led-parselet :equal
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 90 :left)))

;;; -------------------------------------------------------

(register-led-parselet :not-equal
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 90 :left)))

;;; -------------------------------------------------------

(register-led-parselet :greater-than
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 100 :left)))

;;; -------------------------------------------------------

(register-led-parselet :greater-or-equal
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 100 :left)))

;;; -------------------------------------------------------

(register-led-parselet :less-than
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 100 :left)))

;;; -------------------------------------------------------

(register-led-parselet :less-or-equal
  (make-led-parselet
    #'(lambda (led-token left-expression)
        (declare (type Token    led-token))
        (declare (type AST-Node left-expression))
        (let-parser (right-expression
                      (parse-pratt
                        (get-led-binding-power led-token)))
          (declare (type AST-Node right-expression))
          (return-output
            (make-binary-operation-node
              :operator      (token-type led-token)
              :left-operand  left-expression
              :right-operand right-expression))))
    (make-precedence 100 :left)))
