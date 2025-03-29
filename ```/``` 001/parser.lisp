;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the parsers and combinators whose effort in a
;; champarty capacitates a piece of ``` source code's transformation
;; into an executable program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token vector operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-token-vector ()
  "Returns a fresh expandable vector of ``Token'' objects, at its
   inchoation empight in an empty state."
  (the (vector Token *)
    (make-array 0
      :element-type    'Token
      :initial-element (make-eof-token)
      :adjustable      T
      :fill-pointer    0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-initial-parse-state (lexer))
  (:constructor advance-parse-state
    (current-state
     &aux (lexer  (parse-state-lexer  current-state))
          (tokens (parse-state-tokens current-state))
          (cursor (1+ (parse-state-cursor current-state))))))
  "The ``Parse-State'' class serves in the encapsulation of the parsing
   process' advancement via its castaldy of the already consumed tokens
   from the underlying lexer, both shared among all instances, and a
   ``Parse-State'' object's personal index into this collection."
  (lexer  (error "Missing lexer for parse state.")
          :type      Lexer
          :read-only T)
  (tokens (make-dynamic-token-vector)
          :type      (vector Token *)
          :read-only T)
  (cursor 0
          :type      fixnum
          :read-only NIL))

;;; -------------------------------------------------------

(defun load-token-if-necessary (parse-state)
  "Determines whether the PARSE-STATE's cursor into the shared token
   buffer constitutes a valid designator inwith the currently demarcated
   bournes, upon its refutation querying the next token from the
   underlying lexer and appending the same to the buffer, in any case
   returning no value."
  (declare (type Parse-State parse-state))
  (when (>= (parse-state-cursor parse-state)
            (fill-pointer (parse-state-tokens parse-state)))
    (vector-push-extend
      (get-next-token
        (parse-state-lexer parse-state))
      (parse-state-tokens parse-state)))
  (values))

;;; -------------------------------------------------------

(defun get-current-token (parse-state)
  "Returns the token affiliated with the PARSE-STATE."
  (declare (type Parse-State parse-state))
  (load-token-if-necessary parse-state)
  (the Token
    (aref
      (parse-state-tokens parse-state)
      (parse-state-cursor parse-state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p state output)))
  "The ``Parse-Result'' class encapsulates a ``Parser'''s response to a
   parse request, committed via a ``Parse-State''."
  (succeeded-p (error "Missing success/failure flag.")
               :type      boolean
               :read-only T)
  (state       (error "Missing result state.")
               :type      Parse-State
               :read-only T)
  (output      (error "Missing result output.")
               :type      T
               :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class serves in the implementation of a parser or
   combinator, satisfying its dever by a callback function's adminicle."
  (processor (error "Missing parser processor.")
             :type      (function (Parse-State) Parse-Result)
             :read-only T))

;;; -------------------------------------------------------

(defun apply-parser (parser state)
  "Applies the PARSER to the parse STATE and returns a ``Parse-Result''
   encapsulating its response."
  (the Parse-Result
    (funcall
      (parser-processor parser)
      state)))

;;; -------------------------------------------------------

(defmacro build-parser ((state-variable) &body body)
  "Accommodates a convenience service for the creation of a fresh
   ``Parser'' instance, the processor callback function of which is
   defined in an implicit fashion, admitting as its aefauld input a
   ``Parse-State'' nevened via the STATE-VARIABLE, and employing for its
   implementations the BODY forms, the desinent one of which is expected
   to return a ``Parse-Result'' that encapsulates the thus produced
   parser's response to an invocation request."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           ,@body))))

;;; -------------------------------------------------------

(defmacro define-parser (&rest parsers)
  "Returns a fresh ``Parser'' as a ``chain-of'' composition of input
   PARSERS, hence returning the desinent one's parse result."
  (let ((state-variable (gensym)))
    (declare (type symbol state-variable))
    `(the Parser
       (build-parser (,state-variable)
         (the Parse-Result
           (apply-parser
             (chain-of ,@parsers)
             ,state-variable))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of general parsers and combinators.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun satisfies-token-predicate (predicate)
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   satisfies the PREDICATE, returning on confirmation in its parse
   result the probed token."
  (declare (type (function (Token) *) predicate))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (let ((probed-token (get-current-token state)))
          (declare (type Token probed-token))
          (if (funcall predicate probed-token)
            (make-parse-result T
              (advance-parse-state state)
              probed-token)
            (make-parse-result NIL state probed-token)))))))

;;; -------------------------------------------------------

(defmacro token-which-satisfies ((token-variable) &body body)
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   satisfies an imposed requirement, accomplishing this by binding the
   probed token to the TOKEN-VARIABLE name and evaluating the BODY
   forms, the desinent form determining whether the predicate is
   satisfied, a non-``NIL'' value communicating it affirmation, ``NIL''
   the refutation, the parser on confirmation returning in its parse
   result the probed token."
  `(the Parser
     (satisfies-token-predicate
       #'(lambda (,token-variable)
           (declare (type Token ,token-variable))
           (declare (ignorable  ,token-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun token-of-type (expected-token-type)
  "Returns a fresh ``Parser'' which succeeds if its input parse state's
   token type matches the EXPECTED-TOKEN-TYPE, returning upon
   confirmation in its parse result the probed token."
  (declare (type keyword expected-token-type))
  (the Parser
    (token-which-satisfies (probed-token)
      (token-type-p probed-token expected-token-type))))

;;; -------------------------------------------------------

(defun chain-of (&rest parsers)
  "Returns a fresh ``Parser'' which succeeds if all of its input
   PARSERS, in the exact order of their specification, match, returning
   on confirmation the desinent parser's result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       state
            then    (parse-result-state current-result)
          
          for current-parser
            of-type Parser
            in      parsers
          
          for current-result
            of-type Parse-Result
            =       (apply-parser current-parser current-state)
          
          unless (parse-result-succeeded-p current-result) do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return current-result))))))

;;; -------------------------------------------------------

(defun any-of (&rest parsers)
  "Returns a fresh ``Parser'' which succeeds if any of its input
   PARSERS, probed in the exact order of their specification, matches,
   returning on confirmation the first eligible parser's result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (input-state)
      (the Parse-Result
        (loop
          for current-parser
            of-type Parser
            in      parsers
          
          for current-result
            of-type Parse-Result
            =       (apply-parser current-parser input-state)
          
          when (parse-result-succeeded-p current-result) do
            (return current-result)
          
          finally
            (return
              (make-parse-result NIL input-state NIL)))))))

;;; -------------------------------------------------------

(defun separated-by (subject sepiment)
  "Returns a fresh ``Parser'' which always succeeds, matching zero or
   more instances of the SUBJECT, each twain segregated by the SEPIMENT,
   returning in its parse result a list of the gathered SUBJECT outputs
   in their encountered order."
  (declare (type Parser subject))
  (declare (type Parser sepiment))
  (the Parser
    (build-parser (input-state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       input-state
            then    (parse-result-state current-result)
          
          for current-result
            of-type Parse-Result
            =       (apply-parser subject current-state)
            then    (apply-parser
                      (chain-of sepiment subject)
                      current-state)
          
          while (parse-result-succeeded-p current-result)
            collect (parse-result-output current-result)
            into    outputs
          
          finally
            (return
              (make-parse-result T current-state outputs)))))))

;;; -------------------------------------------------------

(defun zero-or-more-times (parser)
  "Returns a fresh ``Parser'' which always succeeds, matching the input
   PARSER a tally of zero or more times, and returning in its parse
   result a list of the PARSER's outputs in their encountered order."
  (declare (type Parser parser))
  (the Parser
    (build-parser (input-state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       input-state
            then    (parse-result-state current-result)
          
          for current-result
            of-type Parse-Result
            =       (apply-parser parser current-state)
          
          while (parse-result-succeeded-p current-result)
            collect (parse-result-output current-result)
            into    outputs
          
          finally
            (return
              (make-parse-result T current-state outputs)))))))

;;; -------------------------------------------------------

(defun output-of (output)
  "Returns a fresh ``Parser'' which always succeeds, returning in its
   parse result the OUTPUT."
  (declare (type T output))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (make-parse-result T state output)))))

;;; -------------------------------------------------------

(defun monadic-bind (antecedent consequent-generator)
  "Returns a fresh ``Parser'' which answers to the diorism of a monadic
   binding, expecting the ANTECEDENT to match and, upon this
   prerequisite's confirmation, invokes the CONSEQUENT-GENERATOR with
   the ANTECEDENT parse result's output, its produce another parser's
   obtention which is applied to the ANTECEDENT result's state, finally
   returning this consequent parser's parse result."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) consequent-generator))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (let ((antecedent-result (apply-parser antecedent state)))
          (declare (type Parse-Result antecedent-result))
          (if (parse-result-succeeded-p antecedent-result)
            (let ((consequent-result
                    (apply-parser
                      (funcall consequent-generator
                        (parse-result-output antecedent-result))
                      (parse-result-state antecedent-result))))
              (declare (type Parse-Result consequent-result))
              (if (parse-result-succeeded-p consequent-result)
                consequent-result
                (make-parse-result NIL state NIL)))
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defmacro bind-parser ((antecedent-output-variable antecedent)
                       &body body)
  "Returns a fresh ``Parser'' which succeeds if, in an inchoate stage,
   the ANTECEDENT matches, on confirmation binding its result's output
   to the ANTECEDENT-OUTPUT-VARIABLE, evaluates the BODY forms,
   expecting the desinent form's primary value respond with a parser,
   the result of which is ultimately communicated."
  `(the Parser
     (monadic-bind ,antecedent
       #'(lambda (,antecedent-output-variable)
           (declare (type T    ,antecedent-output-variable))
           (declare (ignorable ,antecedent-output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun between (open-guard close-guard subject)
  "Returns a fresh ``Parser'' which succeeds if its OPEN-GUARD, SUBJECT,
   and CLOSE-GUARD match in this exact order, returning on confirmation
   in it parse result the SUBJECT's output."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser subject))
  (the Parser
    (define-parser
      open-guard
      (bind-parser (subject-output subject)
        (declare (type T subject-output))
        (chain-of close-guard
          (output-of subject-output))))))

;;; -------------------------------------------------------

(defun followed-by (subject suffix)
  "Returns a fresh ``Parser'' which succeeds if its SUBJECT matches,
   followed by a succeeding SUFFIX, returning on confirmation in it
   parse result the SUBJECT's output."
  (declare (type Parser subject))
  (declare (type Parser suffix))
  (the Parser
    (define-parser
      (bind-parser (subject-output subject)
        (declare (type T subject-output))
        (chain-of suffix
          (output-of subject-output))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of specialized parsers and combinators.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instruction-separator ()
  "Returns a fresh ``Parser'' which succeeds if the `` command
   separator, a whitespace, follows a tally of one or more times,
   returning on confirmation in its parse result the ``T'' value."
  (the Parser
    (define-parser
      (token-of-type :whitespace)
      (zero-or-more-times (token-of-type :whitespace))
      (output-of T))))

;;; -------------------------------------------------------

(defun padding ()
  "Returns a fresh ``Parser'' which always succeeds, consuming a
   sequence of zero or more accolent whitespaces, returning in its parse
   result the ``T'' value."
  (the Parser
    (define-parser
      (zero-or-more-times (token-of-type :whitespace))
      (output-of T))))

;;; -------------------------------------------------------

(defun integer-number ()
  "Returns a fresh ``Parser'' which succeeds if its input state token
   represents a signed or unsigned integer value, returning on
   confirmation in its parse result the probed token's numeric value."
  (the Parser
    (define-parser
      (bind-parser (numeric-token (token-of-type :number))
        (declare (type Token numeric-token))
        (output-of
          (token-value numeric-token))))))

;;; -------------------------------------------------------

(defun single-backtick ()
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   constitutes a single backtick, returning on confirmation in its parse
   result the probed token."
  (the Parser
    (define-parser
      (token-of-type :single-backtick))))

;;; -------------------------------------------------------

(defun double-backtick ()
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   constitutes two backticks in immediate succession, returning on
   confirmation in its parse result the probed token."
  (the Parser
    (define-parser
      (token-of-type :double-backtick))))

;;; -------------------------------------------------------

(defun number-sign ()
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   constitutes a number sign, or \"hash\" symbol, returning on
   confirmation in its parse result the probed token."
  (the Parser
    (define-parser
      (token-of-type :number-sign))))

;;; -------------------------------------------------------

(defun parse-|`a`#b|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"`a`#b\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (single-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (single-backtick)
          (number-sign)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (output-of
              (make-instruction
                :source-mode       :immediate
                :source-start      (make-immediate-operand b)
                :destination-start (make-immediate-operand a)))))))))

;;; -------------------------------------------------------

(defun parse-|`a`b|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"`a`b\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (single-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (single-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (output-of
              (make-instruction
                :source-mode       :referential
                :source-start      (make-immediate-operand b)
                :destination-start (make-immediate-operand a)))))))))

;;; -------------------------------------------------------

(defun parse-|`a``b|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"`a``b\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (single-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (double-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (output-of
              (make-instruction
                :source-mode       :referential
                :source-start      (make-referential-operand b)
                :destination-start (make-immediate-operand   a)))))))))

;;; -------------------------------------------------------

(defun parse-|`a``b#c|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"`a``b#c\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (single-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (double-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (chain-of
              (number-sign)
              (bind-parser (c (integer-number))
                (declare (type integer c))
                (output-of
                  (make-instruction
                    :source-mode :referential
                    :source-start
                      (make-referential-operand b)
                    :source-offset
                      (make-immediate-operand   c)
                    :destination-start
                      (make-immediate-operand   a)))))))))))

;;; -------------------------------------------------------

(defun parse-|`a``b`c|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"`a`b`c\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (single-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (double-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (chain-of
              (single-backtick)
              (bind-parser (c (integer-number))
                (declare (type integer c))
                (output-of
                  (make-instruction
                    :source-mode :referential
                    :source-start
                      (make-referential-operand b)
                    :source-offset
                      (make-referential-operand c)
                    :destination-start
                      (make-immediate-operand   a)))))))))))

;;; -------------------------------------------------------

(defun parse-|``a`#b|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"``a`#b\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (double-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (single-backtick)
          (number-sign)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (output-of
              (make-instruction
                :source-mode       :immediate
                :source-start      (make-immediate-operand   b)
                :destination-start (make-referential-operand a)))))))))

;;; -------------------------------------------------------

(defun parse-|``a#b`#c|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"``a#b`c\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (double-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (number-sign)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (chain-of
              (single-backtick)
              (number-sign)
              (bind-parser (c (integer-number))
                (declare (type integer c))
                (output-of
                  (make-instruction
                    :source-mode :immediate
                    :source-start
                      (make-immediate-operand   c)
                    :destination-start
                      (make-referential-operand a)
                    :destination-offset
                      (make-immediate-operand   b)))))))))))

;;; -------------------------------------------------------

(defun parse-|``a`b`#c|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"``a`#b`#c\" has been detected, returning
   on confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (double-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (single-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (chain-of
              (single-backtick)
              (number-sign)
              (bind-parser (c (integer-number))
                (declare (type integer c))
                (output-of
                  (make-instruction
                    :source-mode :immediate
                    :source-start
                      (make-immediate-operand   c)
                    :destination-start
                      (make-referential-operand a)
                    :destination-offset
                      (make-referential-operand b)))))))))))

;;; -------------------------------------------------------

(defun parse-|``a`b|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"``a`b\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (double-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (single-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (output-of
              (make-instruction
                :source-mode       :immediate
                :source-start      (make-referential-operand b)
                :destination-start (make-referential-operand a)))))))))

;;; -------------------------------------------------------

(defun parse-|``a#b`c|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"``a#b`c\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (double-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (number-sign)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (chain-of
              (single-backtick)
              (bind-parser (c (integer-number))
                (declare (type integer c))
                (output-of
                  (make-instruction
                    :source-mode :immediate
                    :source-start
                      (make-referential-operand c)
                    :destination-start
                      (make-referential-operand a)
                    :destination-offset
                      (make-immediate-operand   b)))))))))))

;;; -------------------------------------------------------

(defun parse-|``a`b`c|-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a ``` instruction in
   concord with the pattern \"``a`b`c\" has been detected, returning on
   confirmation in its parse result a connable ``instruction''
   representation."
  (the Parser
    (define-parser
      (double-backtick)
      (bind-parser (a (integer-number))
        (declare (type integer a))
        (chain-of
          (single-backtick)
          (bind-parser (b (integer-number))
            (declare (type integer b))
            (chain-of
              (single-backtick)
              (bind-parser (c (integer-number))
                (declare (type integer c))
                (output-of
                  (make-instruction
                    :source-mode :immediate
                    :source-start
                      (make-referential-operand c)
                    :destination-start
                      (make-referential-operand a)
                    :destination-offset
                      (make-referential-operand b)))))))))))

;;; -------------------------------------------------------

(defun parse-instruction ()
  "Returns a fresh ``Parser'' which succeeds if a `` instruction can be
   detected, returning on confirmation in its parse result an
   ``Instruction'' representation thereof."
  (the Parser
    (define-parser
      (any-of
        (parse-|`a`#b|-instruction)
        (parse-|`a`b|-instruction)
        (parse-|`a``b|-instruction)
        (parse-|`a``b#c|-instruction)
        (parse-|`a``b`c|-instruction)
        (parse-|``a`#b|-instruction)
        (parse-|``a#b`#c|-instruction)
        (parse-|``a`b`#c|-instruction)
        (parse-|``a`b|-instruction)
        (parse-|``a#b`c|-instruction)
        (parse-|``a`b`c|-instruction)))))

;;; -------------------------------------------------------

(defun parse-instructions ()
  "Returns a fresh ``Parser'' which succeeds if zero or more ```
   instructions, each twissel segregated by one or more whitespaces,
   follow, returning on confirmation in its parse result an ordered list
   of the ``` operations' ``Instruction'' object representations."
  (the Parser
    (define-parser
      (between
        (padding)
        (padding)
        (separated-by
          (parse-instruction)
          (instruction-separator))))))

;;; -------------------------------------------------------

(defun parse-program (lexer)
  "Parses the `` program whose constituents are communicated by
   mediation of the LEXER's tokens, assembles and returns it."
  (declare (type Lexer lexer))
  (let ((parse-result
          (apply-parser
            (followed-by
              (parse-instructions)
              (token-of-type :eof))
            (make-initial-parse-state lexer))))
    (declare (type Parse-Result parse-result))
    (the program
      (if (parse-result-succeeded-p parse-result)
        (make-program
          (parse-result-output parse-result))
        (error "Error during the parsing of the program.")))))
