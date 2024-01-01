;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the interpreter, the component capacitated to
;; process a NeverGonna program supplied in the form of an abstract
;; syntax tree (AST).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (tree)))
  "The ``Interpreter'' class applies itself to the processing of a
   NeverGonna program supplied in the form of an abstract syntax tree
   (AST) in order to accompass the same with actual effect."
  (tree      (error "Missing AST.")
             :type      Program-Node
             :read-only T)
  (variables (make-hash-table :test #'equal)
             :type      variable-map
             :read-only T))

;;; -------------------------------------------------------

(defun get-variable-by-name (interpreter name)
  "Determines whether a variable with the NAME is registered at the
   INTERPRETER, returning on confirmation the respective ``NGVariable''
   instance, otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the (or null NGVariable)
    (nth-value 0
      (gethash name
        (interpreter-variables interpreter)))))

;;; -------------------------------------------------------

(defun probe-variable-existence (interpreter name)
  "Determines whether a variable with the NAME is registered at the
   INTERPRETER, returning on confirmation the variable instance,
   otherwise an error the type ``Unknown-Variable-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the NGVariable
    (or (get-variable-by-name interpreter name)
        (error 'Unknown-Variable-Error :offending-name name))))

;;; -------------------------------------------------------

(defun probe-variable-amenability (interpreter name)
  "Determines whether a variable with the NAME is registered at the
   INTERPRETER and concomitantly declared, returning on confirmation the
   variable instance, otherwise either signals an error of the type
   ``Incomplete-Variable-Error'' if such a variable is declared but not
   yet initialized, or an ``Unknown-Variable-Error'' upon the NAME's
   absence from the registry."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (let ((variable-for-name (probe-variable-existence interpreter name)))
    (declare (type (or null NGVariable) variable-for-name))
    (the NGVariable
      (or (and variable-for-name
               (ngvariable-initialized-p variable-for-name)
               variable-for-name)
          (error 'Incomplete-Variable-Error :offending-name name)))))

;;; -------------------------------------------------------

(defun declare-variable (interpreter name)
  "Declares a variable with the NAME and registers the same at the
   INTERPRETER.
   ---
   An error of the type ``Duplicate-Variable-Error'' is signaled if a
   variable naiting this NAME already exists, that is, has either been
   declared but not assigned, or both declared and assigned."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (if (get-variable-by-name interpreter name)
    (error 'Duplicate-Variable-Error :offending-name name)
    (setf (gethash name (interpreter-variables interpreter))
          (make-ngvariable name)))
  (values))

;;; -------------------------------------------------------

(defun get-variable-value (interpreter name)
  "Returns the value of the variable registered with the NAME at the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the NGObject
    (ngvariable-value
      (probe-variable-amenability interpreter name))))

;;; -------------------------------------------------------

(defun set-variable-value (interpreter name new-value)
  "Stores the NEW-VALUE into the variable registered with the NAME at
   the INTERPRETER and returns no value, or signals an error of the type
   ``Unknown-Variable-Error'' if no such variable exists."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (declare (type NGObject    new-value))
  (let ((variable-for-name (probe-variable-existence interpreter name)))
    (declare (type NGVariable variable-for-name))
    (setf (ngvariable-value         variable-for-name) new-value)
    (setf (ngvariable-initialized-p variable-for-name) T))
  (values))

;;; -------------------------------------------------------

(defun resolve-value (interpreter object)
  "Returns the ``NGObject'' associated with the OBJECT in the
   INTERPRETER's context."
  (declare (type Interpreter interpreter))
  (declare (type T           object))
  (the NGObject
    (typecase object
      ;; Direct NGObject? => Return itself.
      (NGObject
        object)
      ;; Reference to variable name? => Return variable value.
      (Reference
        (get-variable-value interpreter
          (reference-name object)))
      ;; No other objects may participate in the program.
      (otherwise
        (error "Cannot retrieve the value of ~s." object)))))

;;; -------------------------------------------------------

(defun parse-input (input)
  "Parses and returns the user INPUT as an ``NGObject'' value.
   ---
   This operation incipiently attempts the INPUT's interpretation as a
   signed integer number, whence will issue an ``NGInteger'' response;
   if this endeavor fails, the verbatim string is returned in an
   ``NGString'' guise."
  (declare (type string input))
  (the NGObject
    (handler-case
      (let ((integer-value (parse-integer input)))
        (declare (type integer integer-value))
        (make-nginteger integer-value))
      (error ()
        (make-ngstring input)))))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Evaluates the NODE in the INTERPRETER's context and returns a value
     appropriate for this combination."))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Program-Node))
  "Evaluates the program NODE's child nodes in the INTERPRETER's context
   and returns no value."
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (visit-node interpreter
    (program-node-statements node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Binary-Operation-Node))
  "Applies the binary operation NODE's operator to its operand twain,
   resolving the twissel in the INTERPRETER's context if necessity redes
   so, and returns an ``NGObject'' representation of the calculation
   result."
  (declare (type Interpreter           interpreter))
  (declare (type Binary-Operation-Node node))
  (the NGObject
    (apply-binary-operator
      (binary-operation-node-operator node)
      (resolve-value interpreter
        (visit-node interpreter
          (binary-operation-node-left-operand node)))
      (resolve-value interpreter
        (visit-node interpreter
          (binary-operation-node-right-operand node))))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Block-Node))
  "Evaluates the block NODE's statements in the INTERPRETER's context
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Block-Node  node))
  (dolist (statement (block-node-statements node))
    (declare (type AST-Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Boolean-Node))
  "Ignores the INTERPRETER and returns the NODE's Boolean truth value
   ensconced in an ``NGBoolean'' instance."
  (declare (type Interpreter  interpreter))
  (declare (ignore            interpreter))
  (declare (type Boolean-Node node))
  (the NGBoolean
    (make-ngboolean
      (boolean-node-value node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        For-Loop-Node))
  "Executes the \"for\" loop, or counting iteration, NODE in the
   INTERPRETER's context and returns no value."
  (declare (type Interpreter   interpreter))
  (declare (type For-Loop-Node node))
  (let ((reference
          (visit-node interpreter
            (for-loop-node-variable node)))
        (repetitions
          (for-loop-node-repetitions node))
        (body
          (for-loop-node-body node)))
    (declare (type Reference  reference))
    (declare (type AST-Node   repetitions))
    (declare (type Block-Node body))
    (let ((repetition-count
            (ngobject-value
              (resolve-value interpreter
                (visit-node interpreter repetitions))))
          (counter-object
            (resolve-value interpreter reference)))
      (declare (type integer   repetition-count))
      (declare (type NGInteger counter-object))
      (flet ((update-index-variable ()
              "Increments the COUNTER-OBJECT, representative of the
               \"for\" loop counter variable, by one (1), and returns no
               value."
              (incf (nginteger-value counter-object))
              (values)))
        (loop repeat repetition-count do
          (visit-node interpreter body)
          (update-index-variable)))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Group-Node))
  "Evaluates the group NODE's amplected expression in the INTERPRETER's
   context and returns its result."
  (declare (type Interpreter interpreter))
  (declare (type Group-Node  node))
  (the ngobject
    (visit-node interpreter
      (group-node-body node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        If-Node))
  "Process the conditional \"if\" NODE in the INTERPRETER's context,
   attempting imprimis to satisfy its \"if-then\" clause, proceeding,
   upon its failure, by probing the \"else-if\" options, and resorting,
   in the aftermath of their exhaustion, with the optional \"else\"
   component, in any case returning no value."
  (declare (type Interpreter interpreter))
  (declare (type If-Node     node))
  (flet ((case-matches-p (conditional-case)
          "Determines whether the CONDITIONAL-CASE's antecedent is
           satisfied, returning on confirmation a ``boolean'' value of
           ``T'', otherwise ``NIL''."
          (declare (type If-Case-Node conditional-case))
          (the boolean
            (ngobject-get-boolean-truth-value
              (resolve-value interpreter
                (visit-node interpreter
                  (if-case-node-condition conditional-case)))))))
    (loop
      ;; Probe the next "if-then" or "if-else" case.
      for conditional-case
        of-type If-Case-Node
        in      (if-node-conditional-cases node)
      ;; Does the current CONDITIONAL-CASE match?
      ;; => Execute its body forms.
      ;; => Terminate the loop posthaste, omitting the "finally" clause.
      when (case-matches-p conditional-case) do
        (visit-node interpreter
          (if-case-node-body conditional-case))
        (return)
      ;; No "if-then" or "if-else" case has matched?
      ;; => If an "else" case exists, execute its body forms; otherwise
      ;;    apply no effect.
      finally
        (when (if-node-else-case node)
          (visit-node interpreter
            (else-case-node-body
              (if-node-else-case node))))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        If-Case-Node))
  "Unconditionally executes the \"if-then\" or \"else if\" NODE's
   statements in the INTERPRETER's context and returns no value."
  (declare (type Interpreter  interpreter))
  (declare (type If-Case-Node node))
  (visit-node interpreter
    (if-case-node-body node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Else-Case-Node))
  "Unconditionally executes the \"else\" NODE's statements in the
   INTERPRETER's context and returns no value."
  (declare (type Interpreter    interpreter))
  (declare (type Else-Case-Node node))
  (visit-node interpreter
    (else-case-node-body node))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Input-Node))
  "Evaluates the input NODE in the INTERPRETER's context to first output
   the accompanying prompt message, ere querying, parsing, and returning
   the standard input response."
  (declare (type Interpreter interpreter))
  (declare (type Input-Node  node))
  (format T "~&~a "
    (ngobject-value
      (resolve-value interpreter
        (visit-node interpreter
          (input-node-prompt node)))))
  (finish-output)
  (the NGObject
    (prog1
      (parse-input
        (read-line))
      (clear-input))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Number-Node))
  "Ignores the INTERPRETER and returns the NODE's numeric value
   ensconced in an ``NGInteger'' instance."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Number-Node node))
  (the NGInteger
    (make-nginteger
      (number-node-value node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Print-Node))
  "Interprets the print NODE's argument in the INTERPRETER's context,
   prints the result to the standard output, succeeded by a linebreak,
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Print-Node  node))
  (format T "~a~%"
    (ngobject-value
      (resolve-value interpreter
        (visit-node interpreter
          (print-node-message node)))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        String-Node))
  "Ignores the INTERPRETER and returns the NODE's string value ensconced
   in an ``NGString'' instance."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type String-Node node))
  (the NGString
    (make-ngstring
      (string-node-value node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Unary-Operation-Node))
  "Applies the unary operation NODE's operator to its aefauld operand,
   resolving the object in the INTERPRETER's context if necessity redes
   so, and returns an ``NGObject'' representation of the calculation
   result."
  (declare (type Interpreter          interpreter))
  (declare (type Unary-Operation-Node node))
  (the NGObject
    (apply-unary-operator
      (unary-operation-node-operator node)
      (resolve-value interpreter
        (visit-node interpreter
          (unary-operation-node-operand node))))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Variable-Node))
  "Ignores the INTERPRETER and returns a new ``Reference'' object
   representation of the variable NODE's variable name."
  (declare (type Interpreter   interpreter))
  (declare (ignore             interpreter))
  (declare (type Variable-Node node))
  (the Reference
    (make-reference
      (variable-node-name node))))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Variable-Declaration-Node))
  "Disseminates a new variable through the variable declaration NODE,
   which registers the same at the INTERPRETER, and returns no value."
  (declare (type Interpreter               interpreter))
  (declare (type Variable-Declaration-Node node))
  (declare-variable interpreter
    (reference-name
      (visit-node interpreter
        (variable-declaration-node-variable node))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Variable-Assignment-Node))
  "Accompasses an assignment via the variable assignment NODE, utilizing
   the INTERPRETER's context, and returns no value."
  (declare (type Interpreter              interpreter))
  (declare (type Variable-Assignment-Node node))
  (let ((reference
          (visit-node interpreter
            (variable-assignment-node-variable node))))
    (declare (type Reference reference))
    ;; Ascertain that the variable has been declared before.
    (probe-variable-existence interpreter
      (reference-name reference))
    ;; Associate the variable name with the value.
    (set-variable-value interpreter
      (reference-name reference)
      (resolve-value interpreter
        (visit-node interpreter
          (variable-assignment-node-value node)))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        While-Loop-Node))
  (declare (type Interpreter     interpreter))
  (declare (type While-Loop-Node node))
  (flet ((condition-satisfied-p ()
          "Determines whether the while loop NODE's condition is
           satisfied, returning on confirmation a ``boolean'' value of
           ``T'', otherwise ``NIL''."
          (the boolean
            (ngobject-get-boolean-truth-value
              (resolve-value interpreter
                (visit-node interpreter
                  (while-loop-node-condition node)))))))
    (loop while (condition-satisfied-p) do
      (visit-node interpreter
        (while-loop-node-body node))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the NeverGonna program provided to the INTERPRETER in the
   form of an abstract syntax tree and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-NeverGonna (code)
  "Interprets the piece of NeverGonna source CODE and returns no value."
  (declare (type string code))
  (let ((parse-result
          (parse
            (parse-program)
            (make-initial-parse-state
              (make-lexer code)))))
    (declare (type Parse-Result parse-result))
    (if (parse-result-succeeded-p parse-result)
      (interpret-program
        (make-interpreter
          (parse-result-output parse-result)))
      (error "An error occurred during parsing.")))
  (values))
