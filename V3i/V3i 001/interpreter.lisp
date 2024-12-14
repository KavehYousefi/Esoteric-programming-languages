;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the actual V3i interpreter, itself entalented
;; with the dever of accompassing efficacy to a V3i program provided in
;; an abstract syntax tree's (AST) guise.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing AST.")
    :type          Program-Node
    :documentation "The V3i program in the form of an abstract syntax
                    tree (AST).")
   (variables
    :initform      (make-variable-set)
    :type          Variable-Set
    :documentation "A registry comprehending the variable triple."))
  (:documentation
    "The ``Interpreter'' applies itself to the administration of actual
     efficacy to a V3i program supplied in the form of an abstract
     syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-interpreter (tree)
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   execution of the V3i program specified via its abstractsyntax TREE."
  (declare (type Program-Node tree))
  (the Interpreter
    (make-instance 'Interpreter :tree tree)))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``tree'' to the local
   symbol macro ``$tree'' and its ``variables'' to ``$variables'',
   evaluates the BODY forms, and returns the desinent form's results.
   ---
   Enjoying the recipience of syntomy in its treatise, the following
   local symbol macro diorisms are vouchsafed:
     ------------------------------------------------------------------
     Symbol name | Agency
     ------------+-----------------------------------------------------
     $tree       | The V3i program as an abstract syntax tree (AST).
     ..................................................................
     $variables  | The table of variables.
     ------------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($tree
              (the Program-Node
                (slot-value ,evaluated-interpreter 'tree)))
            ($variables
              (the Variable-Set
                (slot-value ,evaluated-interpreter 'variables))))
         (declare (type Program-Node $tree))
         (declare (ignorable         $tree))
         (declare (type Variable-Set $variables))
         (declare (ignorable         $variables))
         ,@body))))

;;; -------------------------------------------------------

(defgeneric resolve-value (interpreter object)
  (:documentation
    "Returns the value represented by the OBJECT in the INTERPRETER's
     context.")
  
  (:method ((interpreter Interpreter)
            (reference   Reference))
    "Returns the value stored in the variable, as defined in the
     INTERPRETER's variable set, and encapsulated in the REFERENCE."
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (with-interpreter (interpreter)
      (the v3i-object
        (variable-value $variables
          (reference-name reference)))))
  
  (:method ((interpreter Interpreter)
            (integer     integer))
    "Returns the INTEGER value, ignoring the INTERPRETER."
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type integer     integer))
    (the integer integer))
  
  (:method ((interpreter Interpreter)
            (string      string))
    "Returns the STRING value, ignoring the INTERPRETER."
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type string      string))
    (the string string)))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Processes the abstract syntax tree (AST) NODE in the INTERPRETER's
     context and returns a value conable for this combination."))

;;; -------------------------------------------------------

(defmacro define-node-visitor (node-class &body body)
  "Defines an implementation of the generic function ``visit-node'',
   the first formal argument being norned in a fixed manner as
   ``$interpreter'', specializing on the ``Interpreter'' class, the
   second argument being nevened ``$node'', its specialization imposed
   by the NODE-CLASS, evaluates the BODY forms in the thus specified
   method, which returns the desinent BODY form's results.
   ---
   Please heed that the BODY forms are ensconced in an implicitly
   defined ``with-interpreter'' macro invocation, this appropriating a
   latreutical agency for an enhanced commodity in the interpreter's
   handling."
  `(defmethod visit-node (($interpreter Interpreter)
                          ($node        ,node-class))
     (declare (type Interpreter $interpreter))
     (declare (ignorable        $interpreter))
     (declare (type ,node-class $node))
     (declare (ignorable        $node))
     (with-interpreter ($interpreter)
       ,@body)))

;;; -------------------------------------------------------

(define-node-visitor Binary-Operation-Node
  (the v3i-object
    (apply-binary-operator
      (binary-operation-node-operator $node)
      (resolve-value $interpreter
        (visit-node $interpreter
          (binary-operation-node-left-operand $node)))
      (resolve-value $interpreter
        (visit-node $interpreter
          (binary-operation-node-right-operand $node))))))

;;; -------------------------------------------------------

(define-node-visitor Block-Node
  (dolist (statement (block-node-statements $node))
    (declare (type AST-Node statement))
    (visit-node $interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-visitor Finite-Loop-Node
  (loop
    repeat
      (resolve-value $interpreter
        (visit-node $interpreter
          (finite-loop-node-repetitions $node)))
    do
      (visit-node $interpreter
        (finite-loop-node-statements $node)))
  (values))

;;; -------------------------------------------------------

(define-node-visitor If-Node
  (cond
    ;; Antecedent satisfied?
    ;; => Execute "then" branch.
    ((binary-value-is-true-p
        (resolve-value $interpreter
          (visit-node $interpreter
            (if-node-antecedent $node))))
      (visit-node $interpreter
        (if-node-consequent $node)))
    ;; "else" branch exists?
    ;; => Execute "else" branch.
    ((if-node-alternative $node)
      (visit-node $interpreter
        (if-node-alternative $node)))
    ;; Neither "then" nor "else" branch feasible?
    ;; => Abstain from further actions.
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(define-node-visitor Infinite-Loop-Node
  (loop do
    (visit-node $interpreter
      (infinite-loop-node-statements $node)))
  (values))

;;; -------------------------------------------------------

(define-node-visitor Input-Node
  (the v3i-object
    (parse-user-input
      (query-for-input))))

;;; -------------------------------------------------------

(define-node-visitor Number-Node
  (the unsigned-integer
    (number-node-value $node)))

;;; -------------------------------------------------------

(define-node-visitor Print-Node
  (dolist (current-argument (print-node-arguments $node))
    (declare (type Expression-Node current-argument))
    (format T "~a"
      (resolve-value $interpreter
        (visit-node $interpreter current-argument))))
  (format T "~%")
  (values))

;;; -------------------------------------------------------

(define-node-visitor Program-Node
  (visit-node $interpreter
    (program-node-statements $node))
  (values))

;;; -------------------------------------------------------

(define-node-visitor Sqrt-Node
  (the unsigned-integer
    (isqrt
      (resolve-value $interpreter
        (visit-node $interpreter
          (sqrt-node-argument $node))))))

;;; -------------------------------------------------------

(define-node-visitor String-Node
  (the string
    (string-node-value $node)))

;;; -------------------------------------------------------

(define-node-visitor Unary-Operation-Node
  (the v3i-object
    (apply-unary-operator
      (unary-operation-node-operator $node)
      (resolve-value $interpreter
        (visit-node $interpreter
          (unary-operation-node-operand $node))))))

;;; -------------------------------------------------------

(define-node-visitor Variable-Call-Node
  (the v3i-object
    (variable-value $variables
      (variable-call-node-name $node))))

;;; -------------------------------------------------------

(define-node-visitor Variable-Definition-Node
  (define-variable $variables
    (variable-definition-node-name $node)
    (resolve-value $interpreter
      (visit-node $interpreter
        (variable-definition-node-value $node))))
  (values))

;;; -------------------------------------------------------

(define-node-visitor Variable-Node
  (the Reference
    (make-reference
      (variable-node-name $node))))

;;; -------------------------------------------------------

(define-node-visitor Wait-Node
  (wait
    (resolve-value $interpreter
      (visit-node $interpreter
        (wait-node-delay $node))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Processes the V3i program stored in the INTERPRETER in the form of
   an abstract syntax tree (AST) and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-V3i (code)
  "Interprets the piece of V3i source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-token-stream
          (make-lexer code)))))
  (values))
