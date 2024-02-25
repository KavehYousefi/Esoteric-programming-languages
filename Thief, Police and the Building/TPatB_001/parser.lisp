;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the project's parser, its foundry derived from
;; the parser combinator species.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun room-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a character covenable
   for representation of a room's content, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (and (graphic-char-p candidate)
           (not (char= candidate #\Space)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-character-at (source position)
  "Returns the character at the POSITION in the SOURCE, or ``NIL'' upon
   its bournes' transcendence."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of numeric operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-positive-integer (number)
  "Determines whether the NUMBER constitutes a positive integer object,
   returning on confirmation the input verbatim, otherwise signaling an
   error of an unspecified type."
  (declare (type integer number))
  (the positive-integer
    (if (plusp number)
      number
      (error "Expected a positive integer number, but encountered ~d."
        number))))

;;; -------------------------------------------------------

(defun validate-non-zero-integer (number)
  "Determines whether the NUMBER constitutes a non-zero signed integer
   object, returning on confirmation the input verbatim, otherwise
   signaling an error of an unspecified type."
  (declare (type integer number))
  (the integer
    (if (zerop number)
      (error "Expected a non-zero integer number, but encountered ~d."
        number)
      number)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse state.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-initial-parse-state (source &aux (position 0)))
  (:constructor make-parse-state         (source position)))
  "The ``Parse-State'' encapsulates the entirety of the parsing process
   as a token of currency in twifaced agency, once as the input to a
   parser or combinator, and, secondly, as a component of its response,
   enveloped in a parse result."
  (source   (error "Missing source.")   :type string :read-only T)
  (position (error "Missing position.") :type fixnum :read-only T))

;;; -------------------------------------------------------

(defun parse-state-element (state)
  "Returns the element stored in the parse STATE."
  (declare (type Parse-State state))
  (the (or null character)
    (get-character-at
      (parse-state-source   state)
      (parse-state-position state))))

;;; -------------------------------------------------------

(defun advance-parse-state (state)
  "Creates and returns a new ``Parse-State'' which represents an advance
   in the progression from the input STATE."
  (declare (type Parse-State state))
  (the Parse-State
    (make-parse-state
      (parse-state-source state)
      (1+ (parse-state-position state)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse result.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p state output)))
  "The ``Parse-Result'' class represents a parser's response to a parse
   request, embracing in its perimeter a success/failure flag, the
   resulting parse state, and an output which usually amounts to the
   parser's contribution to the concerted parsing effort."
  (succeeded-p (error "Missing success flag.")
               :type      boolean
               :read-only T)
  (state       (error "Missing parse state.")
               :type      Parse-State
               :read-only T)
  (output      (error "Missing output.")
               :type      T
               :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class constitutes the representation of a parser or a
   combinator, its dever fulfiled by a dedicated processor callback
   function."
  (processor (error "Missing processor.")
             :type      (function (Parse-State) Parse-Result)
             :read-only T))

;;; -------------------------------------------------------

(defun apply-parser (parser state)
  "Applies the PARSER to the parse STATE and returns a ``Parse-Result''
   encapsulation of its response."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall
      (parser-processor parser)
      state)))

;;; -------------------------------------------------------

(defmacro define-parser ((state-variable) &body body)
  "Creates and returns a fresh ``Parser'' instance whose callback
   function accepts as its sole input a ``Parse-State'' stevened by the
   STATE-VARIABLE, and whose BODY provides the handle's implementation,
   returning the desinent BODY form's returns values."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           ,@body))))

;;; -------------------------------------------------------

(defmacro build-parser (&rest parsers)
  "Returns a new ``Parser'' ensconcing a ``chain-of'' combination of the
   input PARSERS."
  (let ((state-variable (gensym)))
    (declare (type symbol state-variable))
    `(the Parser
       (define-parser (,state-variable)
         (the Parse-Result
           (apply-parser
             (chain-of ,@parsers)
             ,state-variable))))))

;;; -------------------------------------------------------

(defun probe-element (predicate)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   probed character satisfies the PREDICATE, on confirmation returning
   in its parse result the probed element."
  (declare (type (function ((or null character)) *) predicate))
  (the Parser
    (define-parser (state)
      (let ((probed-element (parse-state-element state)))
        (declare (type (or null character) probed-element))
        (the Parse-Result
          (if (funcall predicate probed-element)
            (make-parse-result T
              (advance-parse-state state)
              probed-element)
            (make-parse-result NIL state probed-element)))))))

;;; -------------------------------------------------------

(defun character-matching (predicate)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   element is not ``NIL'' and satisfies the PREDICATE, on confirmation
   returning in its parse result the probed element."
  (declare (type (function (character) *) predicate))
  (the Parser
    (probe-element
      #'(lambda (probed-element)
          (declare (type (or null character) probed-element))
          (and probed-element
            (funcall predicate probed-element))))))

;;; -------------------------------------------------------

(defun character-of (expected-character)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   element equals the EXPECTED-CHARACTER, on confirmation returning in
   its parse result the probed element."
  (declare (type character expected-character))
  (the Parser
    (character-matching
      #'(lambda (probed-element)
          (declare (type character probed-element))
          (char= expected-character probed-element)))))

;;; -------------------------------------------------------

(defun space-character ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   elements constitutes a space character, on confirmation returning in
   its parse result the probed element."
  (the Parser
    (character-of #\Space)))

;;; -------------------------------------------------------

(defun newline-character ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   element constitutes a newline character, on confirmation returning
   in its parse result the probed element."
  (the Parser
    (character-of #\Newline)))

;;; -------------------------------------------------------

(defun eof-character ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   element represents the ``NIL'' value, a tantamount of the end-of-file
   (EOF) sentinel, on confirmation returning in its result the probed
   element."
  (the Parser
    (probe-element
      #'(lambda (element)
          (declare (type (or null character) element))
          (null element)))))

;;; -------------------------------------------------------

(defun monadic-bind (antecedent consequent-generator)
  "Returns a ``Parser'' which succeeds if both its ANTECEDENT and the
   consequent parser, obtained by invoking the CONSEQUENT-GENERATOR with
   the ANTECEDENT's successful parse result output, match, on
   confirmation returning in its parse result the consequent parser's
   output."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) consequent-generator))
  (the Parser
    (define-parser (state)
      (let ((antecedent-result (apply-parser antecedent state)))
        (declare (type Parse-Result antecedent-result))
        (the Parse-Result
          (if (parse-result-succeeded-p antecedent-result)
            (let ((consequent-result
                    (apply-parser
                      ;; Obtain the consequent parser ...
                      (funcall consequent-generator
                        (parse-result-output antecedent-result))
                      ;; ... and apply it to the antecedent's
                      ;; result state.
                      (parse-result-state antecedent-result))))
              (declare (type Parse-Result consequent-result))
              (if (parse-result-succeeded-p consequent-result)
                consequent-result
                (make-parse-result NIL state NIL)))
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defmacro let-parser ((antecedent-output-variable antecedent)
                      &body body)
  "Returns a new ``Parser'' which succeeds if its ANTECEDENT matches,
   if the latter matches, binds its result output to the
   ANTECEDENT-OUTPUT-VARIABLE, executes the BODY forms, and expects the
   desinent form to supply a parser itself, whose success conditions the
   new parser's ultimate application."
  `(the Parser
     (monadic-bind ,antecedent
       #'(lambda (,antecedent-output-variable)
           (declare (type T    ,antecedent-output-variable))
           (declare (ignorable ,antecedent-output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defmacro let-parsers ((first-binding &rest further-bindings)
                       &body body)
  "Returns a new ``Parser'' which succeeds if all of its bindings,
  composed of the FIRST-BINDING and the FURTHER-BINDINGS in the
  specified order, binding all result outputs to the supplied names,
  executes the BODY forms, and expects the desinent form to supply a
  parser itself, whose success conditions the new parser's ultimate
  application."
  (the Parser
    (if (null further-bindings)
      `(let-parser ,first-binding
         ,@body)
      `(let-parser ,first-binding
         (let-parsers ,further-bindings
           ,@body)))))

;;; -------------------------------------------------------

(defun return-output (output)
  "Returns a ``Parser'' which always succeeds, returning in its parse
   result the OUTPUT."
  (declare (type T output))
  (the Parser
    (define-parser (state)
      (the Parse-Result
        (make-parse-result T state output)))))

;;; -------------------------------------------------------

(defun chain-of (&rest parsers)
  "Returns a ``Parser'' which succeeds if all of its input PARSERS, in
   their specified order, match, on confirmation returning in its parse
   result output the desinent parser's output."
  (declare (type parser-list parsers))
  (the Parser
    (define-parser (state)
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

(defun sequence-of (&rest parsers)
  "Returns a ``Parser'' which succeeds if all of its input PARSERS, in
   their specified order, match, on confirmation returning in its parse
   result output the PARSERS' collected outputs, complying to the
   encountered arrangement."
  (declare (type parser-list parsers))
  (the Parser
    (define-parser (state)
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
          
          if (parse-result-succeeded-p current-result)
            collect (parse-result-output current-result)
            into    outputs
          else do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return
              (make-parse-result T
                (parse-result-state current-result)
                outputs)))))))

;;; -------------------------------------------------------

(defun any-of (&rest choices)
  "Returns a ``Parser'' which succeeds if any of its CHOICES, probed in
   the order of their specification, matches, on confirmation returning
   the first covenable parser's result."
  (declare (type parser-list choices))
  (the Parser
    (define-parser (state)
      (the Parse-Result
        (loop
          for choice
            of-type Parser
            in      choices
          for choice-result
            of-type Parse-Result
            =       (apply-parser choice state)
          
          when (parse-result-succeeded-p choice-result) do
            (return choice-result)
          
          finally
            (return
              (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defun several-times (subject minimum maximum)
  "Returns a new ``Parser'' which succeeds if its input SUBJECT matches
   one or more times in immediate succession, on confirmation returning
   in its parse result output the compiled SUBJECT result outputs in
   their encountered order."
  (declare (type Parser                  subject))
  (declare (type (integer 0 *)           minimum))
  (declare (type (or null (integer 0 *)) maximum))
  (the Parser
    (define-parser (state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       state
            then    (parse-result-state current-result)
          for current-result
            of-type Parse-Result
            =       (apply-parser subject current-state)
          while (and (or (null maximum)
                         (< number-of-repetitions maximum))
                     (parse-result-succeeded-p current-result))
            collect (parse-result-output current-result) into outputs
            count   1 into number-of-repetitions
          finally
            (return
              (if (and (>= number-of-repetitions minimum)
                       (or (null maximum)
                           (<= number-of-repetitions maximum)))
                (make-parse-result T   current-state outputs)
                (make-parse-result NIL state         NIL))))))))

;;; -------------------------------------------------------

(defun zero-or-more-times (subject)
  "Returns a new ``Parser'' which always succeeds, matching the SUBJECT
   zero or more times in immediate succession, returning in its parse
   result output the compiled SUBJECT result outputs in their
   encountered order."
  (declare (type Parser subject))
  (the Parser
    (several-times subject 0 NIL)))

;;; -------------------------------------------------------

(defun one-or-more-times (subject)
  "Returns a new ``Parser'' which succeeds if its input SUBJECT matches
   one or more times in immediate succession, on confirmation returning
   in its parse result output the compiled SUBJECT result outputs in
   their encountered order."
  (declare (type Parser subject))
  (the Parser
    (several-times subject 1 NIL)))

;;; -------------------------------------------------------

(defun followed-by (prefix &rest suffices)
  "Returns a new ``Parser'' which succeeds if the PREFIX and the
   SUFFICES match in this exact order, on confirmation returning in its
   parse result output the PREFIX' output."
  (declare (type Parser prefix))
  (let-parser (prefix-output prefix)
    (declare (type T prefix-output))
    (chain-of
      (apply #'chain-of suffices)
      (return-output prefix-output))))

;;; -------------------------------------------------------

(defun between (open-guard close-guard subject)
  "Returns a new ``Parser'' which succeeds if the OPEN-GUARD, the
   SUBJECT, and the CLOSE-GUARD in this exact order match, on
   confirmation returning the SUBJECT's result."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser subject))
  (the Parser
    (chain-of open-guard
      (let-parser (subject-output subject)
        (declare (type T subject-output))
        (chain-of close-guard
          (return-output subject-output))))))

;;; -------------------------------------------------------

(defun all-separated-by (separator &rest subjects)
  "Returns a new ``Parser'' which succeeds if all SUBJECTS, in their
   specified order, are encountered, with each twissel's sepiment
   realized in the SEPARATOR, on confirmation returning in its parse
   result output a list of the compiled SUBJECTS' outputs in an order
   that replicates their encounter."
  (declare (type Parser           separator))
  (declare (type (list-of Parser) subjects))
  (symbol-macrolet
      ((tail
        (the Parser
          (chain-of separator
            (pop subjects)))))
    (declare (type Parser tail))
    (define-parser (state)
      (the Parse-Result
        (loop
          while subjects
          
          for current-state
            of-type Parse-State
            =       state
            then    (parse-result-state current-result)
          for current-parser
            of-type Parser
            =       (pop subjects)
            then    tail
          for current-result
            of-type Parse-Result
            =       (apply-parser current-parser current-state)
          
          if (parse-result-succeeded-p current-result)
            collect (parse-result-output current-result)
            into    outputs
          else do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return
              (if (plusp (length outputs))
                (make-parse-result T
                  (parse-result-state current-result)
                  outputs)
                (make-parse-result NIL state NIL))))))))

;;; -------------------------------------------------------

(defun one-or-more-separated-by (subject separator)
  "Returns a new ``Parser'' which succeeds if the SUBJECT occurs one or
   more times in succession, with each twissel's intermede occupied by
   the SEPARATOR, on confirmation returning a list of the SUBJECT
   outputs in their encountered order."
  (declare (type Parser subject))
  (declare (type Parser separator))
  (symbol-macrolet
      ((tail
        (the Parser
          (chain-of separator subject))))
    (declare (type Parser tail))
    (define-parser (state)
      (loop
        for current-state
          of-type Parse-State
          =       state
          then    (parse-result-state current-result)
        for current-parser
          of-type Parser
          =       subject
          then    tail
        for current-result
          of-type Parse-Result
          =       (apply-parser current-parser current-state)
        
        while (parse-result-succeeded-p current-result)
          collect (parse-result-output current-result)
          into    outputs
        
        finally
          (return
            (if (plusp (length outputs))
              (make-parse-result T   current-state outputs)
              (make-parse-result NIL state         NIL)))))))

;;; -------------------------------------------------------

(defun zero-or-more-separated-by (subject separator)
  "Returns a new ``Parser'' which always succeeds, probing for zero or
   more occurrences of the SUBJECT occurs, with each twissel's intermede
   occupied by the SEPARATOR, and returning a list of the SUBJECT
   outputs in their encountered order."
  (declare (type Parser subject))
  (declare (type Parser separator))
  (symbol-macrolet
      ((tail
        (the Parser
          (chain-of separator subject))))
    (declare (type Parser tail))
    (define-parser (state)
      (loop
        for current-state
          of-type Parse-State
          =       state
          then    (parse-result-state current-result)
        for current-parser
          of-type Parser
          =       subject
          then    tail
        for current-result
          of-type Parse-Result
          =       (apply-parser current-parser current-state)
        
        while (parse-result-succeeded-p current-result)
          collect (parse-result-output current-result)
          into    outputs
        
        finally
          (return
            (make-parse-result T current-state outputs))))))

;;; -------------------------------------------------------

(defun peek (subject)
  "Returns a new ``Parser'' which peeks or probes its SUBJECT, that is,
   this new parser succeeds if its SUBJECT matches, on confirmation
   returning in its result the input parse state, in conjunction with
   the SUBJECT's output, thus not advancing or consuming the parse
   state."
  (the Parser
    (define-parser (state)
      (the Parse-Result
        (let ((peeked-result (apply-parser subject state)))
          (declare (type Parse-Result peeked-result))
          (if (parse-result-succeeded-p peeked-result)
            (make-parse-result T state
              (parse-result-output peeked-result))
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defun fail (datum &rest arguments)
  "Returns a new ``Parser'' which always fails, as an epiphenomenon
   signaling an error whose type and configuration derive from the
   combination of the DATUM and ARGUMENTS, ensuing from the
   specification of the Common Lisp standard function ``error''."
  (declare (type (or condition string symbol) datum))
  (declare (type (list-of *)                  arguments))
  (the Parser
    (define-parser (state)
      (apply #'error datum arguments))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of specialized parsers and combinators.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word (expected-word)
  "Returns a ``Parser'' which succeeds if the subsequent characters
   replicate the EXPECTED-WORD, on confirmation returning in its parse
   result a string representation of the matched characters."
  (declare (type string expected-word))
  (the Parser
    (let-parser (characters
                  (apply #'sequence-of
                    (map 'list #'character-of expected-word)))
      (declare (type (list-of character) characters))
      (return-output
        (coerce characters 'string)))))

;;; -------------------------------------------------------

(defun sign ()
  "Returns a new ``Parser'' which always succeeds, consuming an optional
   signum character (\"+\" or \"-\"), in the case of the plus sign
   returning in its parse result the number 1, for a minus sign the
   number -1, and, upon such a token's absence, the default of 1."
  (the Parser
    (any-of
      (chain-of (character-of #\+) (return-output +1))
      (chain-of (character-of #\-) (return-output -1))
      (return-output +1))))

;;; -------------------------------------------------------

(defun decimal-digit ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   element constitutes a character representation of a decimal digit,
   on confirmation returning in its parse result the probed element."
  (the Parser
    (character-matching #'digit-char-p)))

;;; -------------------------------------------------------

(defun signed-decimal-number ()
  "Returns a new ``Parser'' which succeeds if the subsequent characters
   specify a signed or unsigned integer number, on confirmation
   returning in its parse result output the extracted numeric object."
  (the Parser
    (let-parser (sign (sign))
      (declare (type (integer -1 +1) sign))
      (let-parser (digits (one-or-more-times (decimal-digit)))
        (declare (type (list-of character) digits))
        (return-output
          (* sign
            (parse-integer
              (coerce digits 'string))))))))

;;; -------------------------------------------------------

(defun unsigned-decimal-number ()
  "Returns a new ``Parser'' which succeeds if the subsequent characters
   specify an unsigned integer number, on confirmation returning in its
   parse result output the extracted numeric object."
  (the Parser
    (let-parser (digits (one-or-more-times (decimal-digit)))
      (declare (type (list-of character) digits))
      (return-output
        (parse-integer
          (coerce digits 'string))))))

;;; -------------------------------------------------------

(defun positive-decimal-number ()
  "Returns a new ``Parser'' which succeeds if the subsequent characters
   specify an unsigned positive integer number, on confirmation
   returning in its parse result output the extracted numeric object."
  (the Parser
    (let-parser (digits (one-or-more-times (decimal-digit)))
      (declare (type (list-of character) digits))
      (return-output
        (validate-positive-integer
          (parse-integer
            (coerce digits 'string)))))))

;;; -------------------------------------------------------

(defun non-zero-decimal-number ()
  "Returns a new ``Parser'' which succeeds if the subsequent characters
   specify a non-zero signed integer number, on confirmation returning
   in its parse result output the extracted numeric object."
  (the Parser
    (let-parser (number (signed-decimal-number))
      (declare (type integer number))
      (return-output
        (validate-non-zero-integer number)))))

;;; -------------------------------------------------------

(defun ground-floor ()
  "Returns a new ``Parser'' which succeeds if the ground floor
   identifier \"G\" has been encountered, on confirmation returning in
   its parse result the representative integral value zero (0)."
  (the Parser
    (chain-of
      (character-of #\G)
      (return-output 0))))

;;; -------------------------------------------------------

;; floorDesignator := "G" | nonZeroInteger ;
(defun floor-designator ()
  "Returns a new ``Parser'' which matches a floor designator, that is,
   either the ground floor identifier character \"G\" or a signed
   integer datum, on confirmation returning in its result output the
   encountered designator."
  (the Parser
    (any-of
      (non-zero-decimal-number)
      (ground-floor))))

;;; -------------------------------------------------------

;; G/F
;; {number}/F
(defun floor/F ()
  "Returns a new ``Parser'' which succeeds if a floor designator,
   concluded by the \"/F\" suffix, is encountered, on confirmation
   returning in its parse result the floor designator's integral value."
  (the Parser
    (followed-by
      (floor-designator)
      (word "/F"))))

;;; -------------------------------------------------------

;; {number}F/s
(defun speed ()
  "Returns a new ``Parser'' which matches a speed per second
   declaration, on confirmation returning in its result the designator's
   numeric component."
  (the Parser
    (followed-by
      (unsigned-decimal-number)
      (word "F/s"))))

;;; -------------------------------------------------------

;; {number}s
(defun duration ()
  "Returns a new ``Parser'' which matches a duration in seconds
   declaration, on confirmation returning in its result the designator's
   numeric component."
  (the Parser
    (followed-by
      (unsigned-decimal-number)
      (word "s"))))

;;; -------------------------------------------------------

(defun spacing ()
  "Returns a new ``Parser'' which succeeds if one or more spaces in
   immediate succession can be ascertained, on confirmation returning in
   its result the consumed spaces complying to the encountering order."
  (the Parser
    (one-or-more-times
      (space-character))))

;;; -------------------------------------------------------

(defun padding ()
  "Returns a new ``Parser'' which matches zero or more spaces in
   immediate succession, returning in its parse a list of the consumed
   spaces in replication of their exact encounter order."
  (the Parser
    (zero-or-more-times
      (space-character))))

;;; -------------------------------------------------------

(defun padded (subject)
  "Returns a new ``Parser'' which succeeds if its SUBJECT parser,
   contingently surrounded on either side by zero or more spaces,
   matches, on confirmation returning the SUBJECT's result."
  (declare (type Parser subject))
  (the Parser
    (between
      (padding)
      (padding)
      subject)))

;;; -------------------------------------------------------

(defun linebreaks ()
  "Returns a new ``Parser'' which succeeds if one or more newlines in
   immediate succession can be ascertained, on confirmation returning in
   its result the consumed newlines complying to the encountering
   order."
  (the Parser
    (one-or-more-times
      (padded
        (newline-character)))))

;;; -------------------------------------------------------

(defun blank-lines ()
  "Returns a new ``Parser'' which always succeeds, matching zero or more
   linebreaks, contingently embraced by spaces, transpire in immediate
   succession, returning in its parse result the consumed content."
  (the Parser
    (build-parser
      (zero-or-more-times
        (padded
          (newline-character))))))

;;; -------------------------------------------------------

(defun phrase (&rest tokens)
  "Returns a new ``Parser'' which succeeds if the TOKENS, in this exact
   order, are encountered, each twissel's sepiment realized in one or
   more spaces, on confirmation returning a list of the matched tokens
   in their encounter order."
  (declare (type (list-of string) tokens))
  (the Parser
    (build-parser
      (apply #'all-separated-by
        (spacing)
        (mapcar #'word tokens)))))

;;; -------------------------------------------------------

(defun a-thief-on ()
  "Returns a new ``Parser'' which matches the statement
   \"A thief of {floor}\", on confirmation returning in its result the
   {floor}
   parcel."
  (the Parser
    (padded
      (chain-of
        (phrase "A" "thief" "on")
        (spacing)
        (let-parser (start-position (floor/F))
          (declare (type integer start-position))
          (return-output start-position))))))

;;; -------------------------------------------------------

(defun set-soe ()
  "Returns a new ``Parser'' which matches the statement
   \"Set SoE -> {speed}\", on confirmation returning in its result the
   elevator {speed} parcel."
  (the Parser
    (padded
      (chain-of
        (phrase "Set" "SoE" "->")
        (spacing)
        (let-parser (elevator-speed (speed))
          (declare (type integer elevator-speed))
          (return-output elevator-speed))))))

;;; -------------------------------------------------------

(defun set-sos ()
  "Returns a new ``Parser'' which matches the statement
   \"Set SoS -> {speed}\", on confirmation returning in its result the
   stair climbing {speed} parcel."
  (the Parser
    (padded
      (chain-of
        (phrase "Set" "SoS" "->")
        (spacing)
        (let-parser (staircase-speed (speed))
          (declare (type integer staircase-speed))
          (return-output staircase-speed))))))

;;; -------------------------------------------------------

(defun floor-selector ()
  "Returns a new ``Parser'' which matches an ordinal floor number
   designator, that is, a non-negative integer number of a ground floor
   descriptor, succeeded by the \"-th floor\" suffix, on confirmation
   returning in its parse result the floor number."
  (the Parser
    (build-parser
      (followed-by
        (floor-designator)
        (word "-th")
        (spacing)
        (word "floor")))))

;;; -------------------------------------------------------

(defun top-floor ()
  "Returns a new ``Parser'' which matches the command
   \"top: {roomNumber}-th floor\", on confirmation returning in its
   result a numeric representation of the detected floor number."
  (the Parser
    (padded
      (chain-of
        (word "top:")
        (spacing)
        (floor-selector)))))

;;; -------------------------------------------------------

(defun bottom-floor ()
  "Returns a new ``Parser'' which matches the command
   \"btm: {roomNumber}-th floor\", on confirmation returning in its
   result a numeric representation of the detected floor number."
  (the Parser
    (padded
      (chain-of
        (word "btm:")
        (spacing)
        (floor-selector)))))

;;; -------------------------------------------------------

(defun room-character ()
  "Returns a new ``Parser'' which succeeds if its probed parse state
   delivers a room character, on confirmation returning in its result
   the perquired character."
  (the Parser
    (character-matching #'room-character-p)))

;;; -------------------------------------------------------

(defun floor-plan ()
  "Returns a new ``Parser'' which matches a floor plan, that is, a line
   desumed from that section of the program which represents a specific
   floor's rooms by their character content, on confirmation returning
   in its result a ``Floor-Plan'' representation of the floor's
   structure."
  (the Parser
    (let-parsers
        ((ground-floor-p
          (any-of
            (chain-of
              (word "G/F")
              (followed-by
                (return-output T)
                (spacing)))
            (return-output NIL)))
         (rooms
          (padded
            (one-or-more-separated-by
              (followed-by
                (room-character)
                (peek
                  (any-of
                    (space-character)
                    (newline-character)
                    (eof-character))))
              (spacing)))))
        (declare (type boolean             ground-floor-p))
        (declare (type (list-of character) rooms))
        (return-output
          (make-floor-plan ground-floor-p rooms)))))

;;; -------------------------------------------------------

(defun house-plan ()
  "Returns a new ``Parser'' which matches a house plan, that is, that
   section in a program composed of one or more lines, everichon among
   the such comprehends a separate floor's room representation, on
   confirmation returning in its result a ``House-Plan'' representation
   of the percepted conformation."
  (the Parser
    (let-parsers
        ((top-floor
          (top-floor))
         (bottom-floor
          (chain-of
            (linebreaks)
            (bottom-floor)))
         (floor-plans
          (chain-of
            (linebreaks)
            (one-or-more-separated-by
              (floor-plan)
              (linebreaks)))))
      (declare (type non-negative-integer top-floor))
      (declare (type non-positive-integer bottom-floor))
      (declare (type (list-of Floor-Plan) floor-plans))
      (return-output
        (build-house-plan top-floor bottom-floor floor-plans)))))

;;; -------------------------------------------------------

(defun move-direction ()
  "Returns a new ``Parser'' which matches any of the two elevator and
   stair room movement directions \"up\" and \"down\", on confirmation
   returning in its result a connable ``direction'' representation."
  (the Parser
    (any-of
      (chain-of
        (word "up")
        (return-output :up))
      (chain-of
        (word "down")
        (return-output :down)))))

;;; -------------------------------------------------------

(defun he-climbs-into ()
  "Returns a new ``Parser'' which matches the command
   \"He climbs into {roomNumber}-th room and steals\", on confirmation
   returning in its result a ``Rob-Room-Command'' representation
   comprehending the detected room number."
  (the Parser
    (padded
      (chain-of
        (phrase "He" "climbs" "into")
        (spacing)
        (let-parser (room-number (positive-decimal-number))
          (declare (type positive-integer room-number))
          (chain-of
            (word "-th")
            (spacing)
            (phrase "room" "and" "steals")
            (return-output
              (make-instance 'Rob-Room-Command
                :room-number room-number))))))))

;;; -------------------------------------------------------

(defun he-gets-into-the-elevator ()
  "Returns a new ``Parser'' which matches the statement
   \"He gets into the elevator and gets {direction}\", on confirmation
   returning in its result an ``Enter-Elevator-Command'' representation
   thereof."
  (the Parser
    (build-parser
      (padded
        (chain-of
          (phrase "He" "gets" "into" "the" "elevator" "and" "gets")
          (spacing)
          (let-parser (direction (move-direction))
            (declare (type direction direction))
            (return-output
              (make-instance 'Enter-Elevator-Command
                :direction direction))))))))

;;; -------------------------------------------------------

(defun he-gets-into-the-stair-room ()
  "Returns a new ``Parser'' which matches the statement
   \"He gets into the stair room and gets {direction}\", on confirmation
   returning in its result an ``Enter-Stair-Room-Command''
   representation thereof."
  (the Parser
    (build-parser
      (padded
        (chain-of
          (phrase "He" "gets" "into" "the" "stair" "room" "and" "gets")
          (spacing)
          (let-parser (direction (move-direction))
            (declare (type direction direction))
            (return-output
              (make-instance 'Enter-Stair-Room-Command
                :direction direction))))))))

;;; -------------------------------------------------------

(defun he-stays-in-the-elevator ()
  "Returns a new ``Parser'' which succeeds if the statement
   \"He stays in the elevator for\", followed by a integral duration
   specifier, occurs, on confirmation returning its result an
   ``Operate-Elevator-Command'' representation thereof."
  (the Parser
    (build-parser
      (padded
        (chain-of
          (phrase "He" "stays" "in" "the" "elevator" "for")
          (spacing)
          (let-parser (duration (duration))
            (declare (type (integer 0 *) duration))
            (return-output
              (make-instance 'Operate-Elevator-Command
                :duration duration))))))))

;;; -------------------------------------------------------

(defun he-stays-in-the-stair-room ()
  "Returns a new ``Parser'' which succeeds if the statement
   \"He stays in the stair room for\", followed by a integral duration
   specifier, occurs, on confirmation returning its result a
   ``Climb-Stair-Room-Command'' representation thereof."
  (the Parser
    (build-parser
      (padded
        (chain-of
          (phrase "He" "stays" "in" "the" "stair" "room" "for")
          (spacing)
          (let-parser (duration (duration))
            (declare (type (integer 0 *) duration))
            (return-output
              (make-instance 'Climb-Stair-Room-Command
                :duration duration))))))))

;;; -------------------------------------------------------

(defun he-gets-out ()
  "Returns a new ``Parser'' which matches the command \"He gets out\",
   on confirmation returning in its result a ``Leave-Command''
   representation thereof."
  (the Parser
    (build-parser
      (padded
        (chain-of
          (phrase "He" "gets" "out")
          (return-output
            (make-instance 'Leave-Command)))))))

;;; -------------------------------------------------------

(defun the-police-have-come ()
  "Returns a new ``Parser'' which matches the command
   \"The police have come\", on confirmation returning in its result an
   ``Encounter-Police-Command'' representation thereof."
  (the Parser
    (build-parser
      (padded
        (chain-of
          (phrase "The" "police" "have" "come")
          (return-output
            (make-instance 'Encounter-Police-Command)))))))

;;; -------------------------------------------------------

(defun commands ()
  "Returns a new ``Parser'' which matches zero or more commands in
   succession, each twissel segregated by one or more newlines, on
   confirmation returning in its parse result a list of the detected
   commands in their correct order."
  (the Parser
    (build-parser
      (let-parser
        (commands
          (zero-or-more-separated-by
            (any-of
              (he-gets-into-the-stair-room)
              (he-gets-into-the-elevator)
              (he-stays-in-the-stair-room)
              (he-stays-in-the-elevator)
              (he-gets-out)
              (he-climbs-into)
              (the-police-have-come))
            (linebreaks)))
      (declare (type (list-of Command) commands))
      (return-output commands)))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Parses a piece of \"Thief, Police and the Building\" source code and
   returns a ``Program'' representation thereof, or, upon its failure
   to provide such latreutical value, signals an error of an unspecified
   type."
  (the Parser
    (build-parser
      (any-of
        (chain-of
          (blank-lines)
          (let-parsers
              ((initial-floor    (a-thief-on))
               (elevator-speed   (chain-of (linebreaks) (set-soe)))
               (stair-room-speed (chain-of (linebreaks) (set-sos)))
               (house-plan       (chain-of (linebreaks) (house-plan)))
               (commands         (any-of
                                   (chain-of (linebreaks) (commands))
                                   (return-output NIL))))
          (declare (type integer      initial-floor))
          (declare (type integer      elevator-speed))
          (declare (type integer      stair-room-speed))
          (declare (type House-Plan   house-plan))
          (declare (type command-list commands))
          (followed-by
            (return-output
              (make-instance 'Program
                :initial-floor    initial-floor
                :elevator-speed   elevator-speed
                :stair-room-speed stair-room-speed
                :house-plan       house-plan
                :commands         commands))
            (blank-lines)
            (eof-character))))
        (fail "Error during the parsing process.")))))
