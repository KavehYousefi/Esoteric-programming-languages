;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the foundational parsers and parser combinator
;; operations, the same are imparted with a very generic nature, whence
;; the more specific, bespoke statement and expression parsers
;; originate.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () Parser) match-statement-block))

;;; -------------------------------------------------------

(declaim (ftype (function (integer) Parser) parse-pratt))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable name verification.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of string) +RESERVED-NAMES+))

;;; -------------------------------------------------------

(defparameter +RESERVED-NAMES+
  '("True" "False")
  "Lists those identifiers whose employment as variable names imposes a
   thing of interdiction, forecause ambiguities betwixt their keyword
   nature and the placeholder agency may arise.")

;;; -------------------------------------------------------

(defun reserved-name-p (name)
  "Determines whether the NAME constitutes a reserved identifier, whose
   appropriation for a variable context would incur a faulty state of
   the program, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string name))
  (the boolean
    (not (null
      (member name +RESERVED-NAMES+ :test #'eq)))))

;;; -------------------------------------------------------

(defun verify-variable-name (name)
  "Applies a docimasy on the NAME in its contingent agency as a variable
   identifier, returning the input itself upon its eligibility,
   otherwise signals an error of the type ``Invalid-Variable-Error''."
  (declare (type string name))
  (the string
    (if (reserved-name-p name)
      (error 'Invalid-Variable-Error :offending-name name)
      name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of statement parsers.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-comment ()
  "Returns a ``Parser'' which succeeds if a comment section, introduced
   by two immediate instances of the hyphen or minus token (\"-\"),
   occurs, returning on confirmation in its parse result's output the
   ``NIL'' value."
  (the Parser
    (match-chain
      (match-token-of-type :minus)
      (match-token-of-type :minus)
      (skip-zero-or-more-times
        (match-any-token-except :newline :eof))
      (return-output NIL))))

;;; -------------------------------------------------------

(defun match-optional-comment ()
  "Returns a ``Parser'' which always succeeds, contingently skipping an
   encountered comment up until, but not including, the end-of-line or
   end-of-file token, returning in its parse result's output the ``NIL''
   value."
  (the Parser
    (match-optionally
      (match-comment))))

;;; -------------------------------------------------------

(defun match-newline ()
  "Returns a ``Parser'' which succeeds if a newline token follows,
   on confirmation returning in its result's output the probed token."
  (the Parser
    (match-token-of-type :newline)))

;;; -------------------------------------------------------

(defun match-newlines ()
  "Returns a ``Parser'' which succeeds if one or more newline tokens
   follow in immediate succession, on confirmation returning in its
   result's output a list comprehending the matched tokens."
  (the Parser
    (match-one-or-more-times
      (match-newline))))

;;; -------------------------------------------------------

(defun skip-optional-newlines ()
  "Returns a ``Parser'' which always succeeds, omitting a sequence of
   zero or more accolent newline tokens, and returning in its parse
   result's output the ``NIL'' value."
  (the Parser
    (skip-zero-or-more-times
      (match-any-of
        (match-comment)
        (match-newline)))))

;;; -------------------------------------------------------

(defun match-end-of-line ()
  "Returns a ``Parser'' which succeeds if either a newline or an
   end-of-file token, both contingently preceded by a comment segment,
   occur, on confirmation returning in its result's output the ``NIL''
   value."
  (the Parser
    (match-chain
      (match-optional-comment)
      (match-any-of
        (match-newlines)
        (match-token-of-type :eof))
      (return-output NIL))))

;;; -------------------------------------------------------

(defun match-space ()
  "Returns a ``Parser'' which succeeds if its probed token constitutes
   a space, on confirmation returning in its parse result output the
   matched token."
  (the Parser
    (match-token-of-type :space)))

;;; -------------------------------------------------------

(defun skip-optional-spaces ()
  "Returns a ``Parser'' which always succeeds, skipping zero or more
   accolent spaces, and returns in its parse result's output the ``NIL''
   value."
  (the Parser
    (skip-zero-or-more-times
      (match-space))))

;;; -------------------------------------------------------

(defun match-padded (subject)
  "Returns a ``Parser'' which succeeds if the SUBJECT, contingently
   embraced on either or both sides by zero or more accolent spaces,
   matches, on confirmation returning the SUBJECT's parse result."
  (declare (type Parser subject))
  (the Parser
    (match-between
      (skip-optional-spaces)
      (skip-optional-spaces)
      subject)))

;;; -------------------------------------------------------

(defun match-word (expected-word)
  "Returns a ``Parser'' which succeeds if its probed token conforms to
   the ``:word'' type and ostends a value equal to the EXPECTED-WORD, on
   confirmation returning in it result's output the probed token."
  (declare (type string expected-word))
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type Token token))
          (and
            (token-type-p token :word)
            (string= (token-value token) expected-word))))))

;;; -------------------------------------------------------

(defun match-words (&rest expected-words)
  "Returns a ``Parser'' which succeeds if the sequence of following
   tokens permits the replication of the EXPECTED-WORDS, on confirmation
   returning in its result's output the matching tokens."
  (declare (type (list-of string) expected-words))
  (the Parser
    (apply #'match-all
      (mapcar #'match-word expected-words))))

;;; -------------------------------------------------------

(defun match-variable ()
  "Returns a ``Parser'' which succeeds if its probed token represents a
   valid variable identifier, on confirmation returning in its result's
   output a ``Variable-Node'' representation of the gleaned variable
   name."
  (the Parser
    (let-parser (variable-token (match-token-of-type :word))
      (return-output
        (make-variable-node :name
          (verify-variable-name
            (token-value variable-token)))))))

;;; -------------------------------------------------------

(defun match-boolean-literal ()
  "Returns a ``Parser'' which succeeds if its probed token represents
   one of the twain of Boolean sentinels, \"False\" or \"True\", on
   confirmation returning in its result's output a ``Boolean-Node''
   encapsulation of the detected value."
  (the Parser
    (match-any-of
      (match-chain
        (match-word "False")
        (return-output
          (make-boolean-node :value NIL)))
      (match-chain
        (match-word "True")
        (return-output
          (make-boolean-node :value T))))))

;;; -------------------------------------------------------

(defun match-number-literal ()
  "Returns a ``Parser'' which succeeds if its token represents a numeric
   literal, on confirmation returning in its result's output a
   ``Number-Node'' encapsulation of the detected value."
  (the Parser
    (let-parser (number-token (match-token-of-type :number))
      (declare (type Token number-token))
      (return-output
        (make-number-node :value (token-value number-token))))))

;;; -------------------------------------------------------

(defun match-string-literal ()
  "Returns a ``Parser'' which succeeds if its probed token represents a
   string literal, on confirmation returning in its result's output a
   ``String-Node'' encapsulation of the consumed token value."
  (the Parser
    (let-parser (string-token (match-token-of-type :string))
      (declare (type Token string-token))
      (return-output
        (make-string-node :value (token-value string-token))))))

;;; -------------------------------------------------------

(defun match-expression ()
  "Returns a ``Parser'' which succeeds if an expression has been
   matched, on confirmation returning in its result's output an
   ``AST-Node'' representation thereof."
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (apply-parser
              (parse-pratt 0)
              state))))))

;;; -------------------------------------------------------

(defun match-variable-fragment (&rest words)
  "Returns a ``Parser'' which succeeds if the specified WORDS occur in
   immediate succession, concluded with a variable name, the latter of
   which is, on confirmation, transmitted in the parse result's output
   as a ``Variable-Node'' object."
  (declare (type (list-of string) words))
  (the Parser
    (match-chain
      (apply #'match-words words)
      (match-variable))))

;;; -------------------------------------------------------

(defun match-expression-fragment (&rest words)
  "Returns a ``Parser'' which succeeds if the specified WORDS occur in
   immediate succession, concluded with an expression, the latter of
   which is, on confirmation, transmitted in the parse result's output
   as an ``AST-Node'' object."
  (declare (type (list-of string) words))
  (the Parser
    (match-chain
      (apply #'match-words words)
      (match-expression))))

;;; -------------------------------------------------------

(defun match-variable-declaration ()
  "Returns a ``Parser'' which succeeds if a variable declaration
   ensues, on confirmation returning in its parse result's output a
   ``Variable-Declaration-Node'' encapsulation of the encountered
   variable name."
  (the Parser
    (let-parser
        (variable
          (match-variable-fragment "we're" "no" "strangers" "to"))
      (declare (type Variable-Node variable))
      (return-output
        (make-variable-declaration-node :variable variable)))))

;;; -------------------------------------------------------

(defun match-variable-assignment ()
  "Returns a ``Parser'' which succeeds if a variable assignment
   ensues, on confirmation returning in its parse result's output a
   ``Variable-Assignment-Node'' encapsulation of the encountered
   variable name."
  (the Parser
    (let-parser (variable (match-variable-fragment "gotta" "make"))
      (declare (type Variable-Node variable))
      (let-parser (value (match-expression))
        (declare (type AST-Node value))
        (return-output
          (make-variable-assignment-node
            :variable variable
            :value    value))))))

;;; -------------------------------------------------------

(defun match-input-command ()
  "Returns a ``Parser'' which succeeds if an input command invocation,
   replicated by the phrase \"your heart's been aching but you're too
   shy to say\", follows, on confirmation returning in its parse
   result's output an ``Input-Node'' representation thereof."
  (the Parser
    (let-parser
        (prompt
          (match-expression-fragment "your" "heart's" "been" "aching" "but"
                                "you're" "too" "shy" "to" "say"))
      (declare (type AST-Node prompt))
      (return-output
        (make-input-node :prompt prompt)))))

;;; -------------------------------------------------------

(defun match-print-command ()
  "Returns a ``Parser'' which succeeds if a print command follows, on
   confirmation returning in its parse result's output a ``Print-Node''
   representation thereof."
  (the Parser
    (let-parser
        (message
          (match-expression-fragment "i" "just" "wanna" "tell" "you"))
      (declare (type AST-Node message))
      (return-output
        (make-print-node :message message)))))

;;; -------------------------------------------------------

(defun match-block-terminator ()
  "Returns a ``Parser'' which succeeds if a block terminator marker
   follows, on confirmation returning in its result's output the ``NIL''
   value."
  (the Parser
    (match-chain
      (match-words "never" "gonna" "give" "you" "up")
      (return-output NIL))))

;;; -------------------------------------------------------

(defun match-for-loop ()
  "Returns a ``Parser'' which succeeds if a counting, or \"for\", loop
   ensues, on confirmation returning in its result's output a
   ``For-Loop-Node'' encapsulation of the encountered variable name and
   the iteration body statements."
  (the Parser
    (let-parser (variable (match-variable-fragment "we've" "known"))
      (declare (type Variable-Node variable))
      (let-parser (repetitions (match-expression-fragment "for"))
        (declare (type AST-Node repetitions))
        (let-parser (statements
                      (match-chain
                        (match-end-of-line)
                        (match-statement-block)))
          (declare (type Block-Node statements))
          (match-chain
            (match-block-terminator)
            (return-output
              (make-for-loop-node
                :variable    variable
                :repetitions repetitions
                :body        statements))))))))

;;; -------------------------------------------------------

(defun match-while-loop ()
  "Returns a ``Parser'' which succeeds if a \"while\" loop ensues, on
   confirmation returning in its result's output a ``While-Loop-Node''
   encapsulation of the antecedent and the iteration body statements."
  (the Parser
    (let-parser
        (guard
          (match-expression-fragment
            "a" "full" "commitment's" "what" "I'm" "thinking" "of"))
      (declare (type AST-Node guard))
      (let-parser (statements
              (match-chain
                (match-end-of-line)
                (match-statement-block)))
          (declare (type Block-Node statements))
          (match-chain
            (match-block-terminator)
            (return-output
              (make-while-loop-node
                :condition guard
                :body      statements)))))))

;;; -------------------------------------------------------

(defun match-if-then-block ()
  "Returns a ``Parser'' which succeeds if a NeverGonna \"if ... then\"
   construct follows, not yet terminated by the block terminator
   sequence \"never gonna give you up\", on confirmation returning in
   its result's output an ``If-Case-Node'' encapsulation of the involved
   antecedent and consequent statement block."
  (the Parser
    (let-parser*
        ((condition
          (match-expression-fragment "inside" "we" "both" "know"))
         (then-statements
          (match-chain
            (match-word "then")
            (match-end-of-line)
            (match-statement-block))))
      (declare (type AST-Node   condition))
      (declare (type Block-Node then-statements))
      (match-chain
        (return-output
          (make-if-case-node
            :condition condition
            :body      then-statements))))))

;;; -------------------------------------------------------

(defun match-else-if-then-block ()
  "Returns a ``Parser'' which succeeds if an \"else if\" block follows,
   on confirmation returning in its result an ``If-Case-Node''
   representation thereof."
  (the Parser
    (let-parser*
        ((condition
          (match-chain
            (skip-optional-newlines)
            (match-expression-fragment
              "never" "gonna" "turn" "around")))
         (statements
          (match-chain
            (match-word "then")
            (match-end-of-line)
            (match-statement-block))))
      (declare (type AST-Node   condition))
      (declare (type Block-Node statements))
      (return-output
        (make-if-case-node :condition condition :body statements)))))

;;; -------------------------------------------------------

(defun match-else-block ()
  "Returns a ``Parser'' which succeeds if an \"else\" block follows, on
   confirmation returning in its result an ``Else-Case-Node''
   representation thereof."
  (the Parser
    (let-parser
        (statements
          (match-chain
            (skip-optional-newlines)
            (match-words "never" "gonna" "let" "you" "down")
            (match-end-of-line)
            (match-statement-block)))
      (declare (type Block-node statements))
      (return-output
        (make-else-case-node :body statements)))))

;;; -------------------------------------------------------

(defun match-if-conditional ()
  "Returns a ``Parser'' which succeeds if an \"if\" conditional follows,
   on confirmation returning in its result's output an ``If-Node''
   encapsulation of the language construct."
  (the Parser
    (let-parser*
         ((if-then-block
            (match-if-then-block))
          (else-if-statements
            (match-chain
              (match-zero-or-more-times (match-newline))
              (match-zero-or-more-times
                (match-else-if-then-block))))
          (else-statement
            (match-chain
              (match-zero-or-more-times (match-newline))
              (match-optionally
                (match-else-block)))))
        (declare (type If-Case-Node             if-then-block))
        (declare (type (list-of If-Case-Node)   else-if-statements))
        (declare (type (or null Else-Case-Node) else-statement))
        (match-chain
          (match-block-terminator)
          (return-output
            (make-if-node
              :conditional-cases (cons if-then-block else-if-statements)
              :else-case         else-statement))))))

;;; -------------------------------------------------------

(defun match-statement ()
  "Returns a ``Parser'' which succeeds if a statement follows, on
   confirmation returning in its result's output an ``AST-Node''
   representation thereof."
  (the Parser
    (match-any-of
      (match-variable-declaration)
      (match-variable-assignment)
      (match-print-command)
      (match-input-command)
      (match-for-loop)
      (match-while-loop)
      (match-if-conditional))))

;;; -------------------------------------------------------

(defun match-statement-line ()
  "Returns a ``Parser'' which succeeds if a statement, followed by a
   newline or end-of-file (EOF) sentinel, in encountered, on
   confirmation returning in its result's output the statement's
   covenable ``AST-Node'' representation."
  (the Parser
    (let-parser (statement (match-statement))
      (declare (type AST-Node statement))
      (match-chain
        (match-end-of-line)
        (return-output statement)))))

;;; -------------------------------------------------------

(defun match-list-of-statements ()
  "Returns a ``Parser'' which always succeeds, matching a sequence of
   zero or more accolent statement lines, and returns a list of the
   consumed instructions."
  (the Parser
    (match-zero-or-more-times
      (match-between
        (skip-optional-newlines)
        (skip-optional-newlines)
        (match-statement-line)))))

;;; -------------------------------------------------------

(defun match-statement-block ()
  "Returns a ``Parser'' which always succeeds, matching a sequence of
   zero or more accolent statement lines, and returns a ``Block-Node''
   encapsulation of the consumed instructions."
  (the Parser
    (let-parser (statements (match-list-of-statements))
      (declare (type node-list statements))
      (return-output
        (make-block-node :statements statements)))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Returns a ``Parser'' which succeeds if a valid NeverGonna program is
   encountered, on confirmation returning in its result's output a
   ``Program-Node'' representation of the detected program."
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (apply-parser
            (let-parser (statements (match-statement-block))
              (match-chain
                (skip-optional-newlines)
                (match-end-of-line)
                (match-token-of-type :eof)
                (return-output
                  (make-program-node :statements statements))))
            state)))))
