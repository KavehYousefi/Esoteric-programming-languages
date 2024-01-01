;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the foundational parsers and parser combinator
;; operations, the same are imparted with a very generic nature, whence
;; the more specific, bespoke statement and expression parsers
;; originate.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-parser (parser state)
  (:documentation
    "Applies the PARSER to the parse STATE and returns a
     ``Parse-Result'' responsible for the specification of the process'
     success or failure, the consequent ``Parse-State'', in the former
     case, and an optional output, the same defines the PARSER's
     contribution to the overall parsing stage's outcome."))

;;; -------------------------------------------------------

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class encapsulates the concept of a parser or a
   combinator as a unit responsible for the interpretation and
   assemblage of one or more tokens into a sensible product."
  (processor (error "Missing processor.") :type parser-processor))

;;; -------------------------------------------------------

(defmethod apply-parser ((parser Parser) (state Parse-State))
  "Processes the parse STATE utilizing the PARSER and returns a
   ``Parse-Result'' representation of the latter's reaction to the
   request."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall (parser-processor parser) state)))

;;; -------------------------------------------------------

(defun parse (parser initial-state)
  "Applies the PARSER to the INITIAL-STATE and returns the resulting
   parse result."
  (declare (type Parser      parser))
  (declare (type Parse-State initial-state))
  (the Parse-Result
    (apply-parser parser initial-state)))

;;; -------------------------------------------------------

(defun probe-token (predicate)
  "Returns a new ``Parser'' which succeeds if its PREDICATE, applied to
   the processed parse state's token, is satisfied, on confirmation
   returning in its result's output the probed token.
   ---
   The PREDICATE constitutes a function which assays the parse state
   token regarding its eligibility, returning a generalized Boolean
   \"true\" value upon its satisfaction, otherwise ``NIL''. The
   signature, as a corollary, conforms to:
     lambda (parse-state-token : Token) => generalized-boolean"
  (declare (type (function (*) *) predicate))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((probed-token (parse-state-current-element state)))
            (declare (type Token probed-token))
            (the Parse-Result
              (if (funcall predicate probed-token)
                (make-parse-result T
                  (parse-state-advance state)
                  probed-token)
                (make-parse-result NIL state NIL))))))))

;;; -------------------------------------------------------

(defun match-token-of-type (expected-type)
  "Returns a ``Parser'' which succeeds if its probed token conforms to
   the EXPECTED-TYPE, on confirmation returning in its result's output
   the matched token."
  (declare (type keyword expected-type))
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type Token token))
          (token-type-p token expected-type)))))

;;; -------------------------------------------------------

(defun match-any-token-except (&rest interdicted-token-types)
  "Returns a ``Parser'' which succeeds if its probed token does not
   conform in its species to any of the INTERDICTED-TOKEN-TYPES, on
   confirmation returning in its result's toutput the matched token."
  (declare (type (list-of keyword) interdicted-token-types))
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type Token token))
          (not (member (token-type token) interdicted-token-types
                 :test #'eq))))))

;;; -------------------------------------------------------

(defun match-chain (&rest parsers)
  "Returns a ``PARSER'' which succeeds if all of its PARSERS, in this
   exact order, match, on confirmation returning in its result's output
   the desinent parser's response."
  (declare (type parser-list parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (loop
            for new-state
              of-type Parse-State
              =       state
              then    (parse-result-state result)
            for parser
              of-type Parser
              in      parsers
            for result
              of-type Parse-Result
              =       (apply-parser parser new-state)
            unless (parse-result-succeeded-p result) do
              (return
                (make-parse-result NIL state NIL))
            finally
              (return result))))))

;;; -------------------------------------------------------

(defun match-any-of (&rest parsers)
  "Returns a new ``Parser'' which succeeds if any of its input PARSERS,
   probed in the exact order of their specification, matches, on
   confirmation returning the first eligible parser's result."
  (declare (type parser-list parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for parser
                of-type Parser
                in      parsers
              for result
                of-type Parse-Result
                =       (apply-parser parser state)
              when (parse-result-succeeded-p result) do
                (return result)
              finally
                (return
                  (make-parse-result NIL state NIL))))))))

;;; -------------------------------------------------------

(defun bind-parser (antecedent parser-generator)
  "Realizes a monadic binding by presupposing the ANTECEDENT parser's
   conformation, succeeded by the consequent parser, yielded via the
   ANTECEDENT result's output when induced into the PARSER-GENERATOR,
   which again must match, on confirmation returning the consequent
   parser's result."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) parser-generator))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((antecedent-result (apply-parser antecedent state)))
            (declare (type Parse-Result antecedent-result))
            (the Parse-Result
              (if (parse-result-succeeded-p antecedent-result)
                (apply-parser
                  (funcall parser-generator
                    (parse-result-output antecedent-result))
                  (parse-result-state antecedent-result))
                antecedent-result)))))))

;;; -------------------------------------------------------

(defmacro let-parser ((output-variable antecedent) &body body)
  "Returns a ``Parser'' which realizes a monadic bind, succeeding if the
   ANTECEDENT matches, and, following the evaluation of the BODY forms,
   the consequent parser, obtained by the desinent BODY form's
   execution, matches, on confirmation returning the consequent parser's
   result."
  `(the Parser
     (bind-parser ,antecedent
       #'(lambda (,output-variable)
           (declare (type T    ,output-variable))
           (declare (ignorable ,output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defmacro let-parser* ((binding &rest bindings) &body body)
  "Returns a ``Parser'' which realizes a monadic bind, succeeding if the
   antecedent, produced from the first BINDING and all subsequent
   BINDINGS, matches, and, following the evaluation of the BODY forms,
   the consequent parser, obtained by the desinent BODY form's
   execution, matches, on confirmation returning the consequent parser's
   result."
  (if (null bindings)
    `(let-parser ,binding
       ,@body)
    `(let-parser ,binding
       (let-parser* ,bindings
         ,@body))))

;;; -------------------------------------------------------

(defun return-output (output)
  "Returns a new ``Parser'' which always succeeds, returning in its
   result's output the OUTPUT datum."
  (declare (type T output))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (make-parse-result T state output))))))

;;; -------------------------------------------------------

(defun match-optionally (parser &optional (default NIL))
  "Returns a ``Parser'' which always succeeds, if matching the input
   PARSER, returns its result verbatim, otherwise responds with a new
   parse result endowed with the input parse state and the DEFAULT value
   as its output."
  (declare (type Parser parser))
  (declare (type T      default))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((parser-result (apply-parser parser state)))
            (declare (type Parse-Result parser-result))
            (the Parse-Result
              (if (parse-result-succeeded-p parser-result)
                parser-result
                (make-parse-result T state default))))))))

;;; -------------------------------------------------------

(defun match-zero-or-more-times (parser)
  "Returns a ``Parser'' which always succeeds, matching zero or more
   accolent instances of the PARSER, returning in its result's output a,
   possibly empty, list of the gleaned PARSER outputs in their correct
   ordonnance."
  (declare (type Parser))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state result)
              for result
                of-type Parse-Result
                =       (apply-parser parser new-state)
              
              if (parse-result-succeeded-p result)
                collect (parse-result-output result)
                into    outputs
              else do
                (return
                  (make-parse-result T new-state outputs))))))))

;;; -------------------------------------------------------

(defun match-one-or-more-times (subject)
  "Returns a ``Parser'' which succeeds if the SUBJECT matches one or
   more times in immediate succession, on confirmation returning in its
   result's output a list of the ascertained SUBJECT outputs in their
   correct order."
  (declare (type Parser subject))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state result)
              for result
                of-type Parse-Result
                =       (apply-parser subject new-state)
              
              while (parse-result-succeeded-p result)
                collect (parse-result-output result)
                into    outputs
              
              finally
                (return
                  (if (plusp (length outputs))
                    (make-parse-result T   new-state outputs)
                    (make-parse-result NIL state     NIL)))))))))

;;; -------------------------------------------------------

(defun match-all (&rest parsers)
  "Returns a ``Parser'' which succeeds if all of the input PARSERS, in
   this exact order, match, on confirmation returning in its result's
   output a list of the compiled parser outputs in compliance with their
   encounters' arrangement."
  (declare (type parser-list parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state result)
              for parser
                of-type Parser
                in      parsers
              for result
                of-type Parse-Result
                =       (apply-parser parser new-state)
              
              if (parse-result-succeeded-p result)
                collect (parse-result-output result)
                into    outputs
              else do
                (return
                  (make-parse-result NIL state NIL))
              
              finally
                (return
                  (make-parse-result T
                    (parse-result-state result)
                    outputs))))))))

;;; -------------------------------------------------------

(defun skip-zero-or-more-times (parser)
  "Returns a ``Parser'' which always succeeds by matching zero or more
   accolent occurrencies of the input PARSER, returning in its parse
   result's output the ``NIL'' value."
  (declare (type Parser))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state result)
              for result
                of-type Parse-Result
                =       (apply-parser parser new-state)
              while (parse-result-succeeded-p result)
              finally
                (return
                  (make-parse-result T new-state NIL))))))))

;;; -------------------------------------------------------

(defun match-between (open-guard close-guard body)
  "Returns a ``Parser'' which succeeds if the BODY parser occurs in the
   immediate interstition betwixt the OPEN-GUARD and the CLOSE-GUARD, on
   confirmation returning in its result's output the BODY's output."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser body))
  (the Parser
    (match-chain open-guard
      (let-parser (body-output body)
        (match-chain close-guard
          (return-output body-output))))))

;;; -------------------------------------------------------

(defun fail-parsing (datum &rest arguments)
  "Returns a ``Parser'' which always fails, concomitantly signaling an
   error connable with the Common Lisp ``error'' facility, founded upon
   the DATUM's nature and the provision of the ARGUMENTS."
  (declare (type T           datum))
  (declare (type (list-of *) arguments))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (declare (ignore           state))
          (funcall #'error datum arguments)))))
