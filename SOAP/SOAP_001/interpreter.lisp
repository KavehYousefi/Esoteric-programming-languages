;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file comprehends the definition of the "Interpreter" class and
;; its appertaining operations, serving in the evaluation of an abstract
;; syntax tree (AST) representation of a SOAP program, produced by a
;; parser.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (tree)))
  "The ``Interpreter'' class provides an entity responsible for the
   application of effect to an abstract syntax tree (AST)."
  (tree     (error "Missing AST root node.") :type Node)
  (main-set (make-inclusive-soapset)         :type SOAPSet)
  (variable 1                                :type natural-number)
  (input    #\Null                           :type character))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots ``tree'', ``main-set'',
   ``variable'' and ``input'' to eponymous local symbol macros for
   general access, evaluates the BODY forms, and returns the last
   processed form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           ((tree
             (the Node
               (interpreter-tree ,evaluated-interpreter)))
            (main-set
             (the SOAPSet
               (interpreter-main-set ,evaluated-interpreter)))
            (variable
             (the natural-number
               (interpreter-variable ,evaluated-interpreter)))
            (input
             (the character
               (interpreter-input ,evaluated-interpreter))))
         (declare (type Node           tree))
         (declare (ignorable           tree))
         (declare (type SOAPSet        main-set))
         (declare (ignorable           main-set))
         (declare (type natural-number variable))
         (declare (ignorable           variable))
         (declare (type character      input))
         (declare (ignorable           input))
         ,@body))))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Visits the NODE using the INTERPRETER, dispatching on the
     NODE-TYPE as the NODE's category identifier, and returns a value
     appropriate for the same."))

;;; -------------------------------------------------------

(defun interpreter-visit-node (interpreter node)
  "Visits the NODE using the INTERPRETER by dispatching on the NODE-TYPE
   in order to invoke the most fitten ``interpreter-dispatch-node''
   mehod and returns a value appropriate for the NODE."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T
    (interpreter-dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defmacro define-node-dispatch
    (node-type (interpreter-variable node-variable) &body body)
  "Defines an implementation of the generic function
   ``interpreter-dispatch-node'' with its first argument being
   designated by the INTERPRETER-VARIABLE, its second automatically
   named, dispatching on the NODE-TYPE, and its third denoted with the
   NODE-VARIABLE designator, evaluates the BODY forms, and returns the
   last processed form's results."
  (let ((node-type-variable    (gensym)))
    (declare (type symbol node-type-variable))
    `(defmethod interpreter-dispatch-node
         ((,interpreter-variable Interpreter)
          (,node-type-variable   (eql ,node-type))
          (,node-variable        Node))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type keyword     ,node-type-variable))
       (declare (ignore           ,node-type-variable))
       (declare (type Node        ,node-variable))
       (declare (ignorable        ,node-variable))
       ,@body)))

;;; -------------------------------------------------------

(define-node-dispatch :integer-literal (interpreter node)
  (the natural-number
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :variable (interpreter node)
  (the natural-number
    (interpreter-variable interpreter)))

;;; -------------------------------------------------------

(define-node-dispatch :set-literal (interpreter node)
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (mapcar
        #'(lambda (element-node)
            (declare (type Node element-node))
            (interpreter-visit-node interpreter element-node))
        (node-attribute node :elements)))))

;;; -------------------------------------------------------

(define-node-dispatch :program (interpreter node)
  (dolist (statement (node-attribute node :statements))
    (declare (type Node statement))
    (interpreter-visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :flip-membership (interpreter node)
  (with-interpreter (interpreter)
    (soapset-flip-membership main-set
      (interpreter-visit-node interpreter
        (node-attribute node :value))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :print-character (interpreter node)
  (write-char
    (node-attribute node :value))
  (values))

;;; -------------------------------------------------------

(defun interpreter-apply-operator (interpreter operator right-operand)
  "Applies the binary set operation defined by OPERATOR, utilizing the
   INTERPRETER's main set as the first operand, accompanied by the
   RIGHT-OPERAND as the second participant, updates the main set in the
   INTERPRETER to contain the result, and returns no value."
  (declare (type Interpreter  interpreter))
  (declare (type set-operator operator))
  (declare (type SOAPSet      right-operand))
  (with-interpreter (interpreter)
    (setf main-set
      (case operator
        (:union
          (soapset-union main-set right-operand))
        (:intersection
          (soapset-intersection main-set right-operand))
        (:left-difference
          (soapset-left-difference main-set right-operand))
        (:right-difference
          (soapset-right-difference main-set right-operand))
        (otherwise
          (error "Invalid binary set operator ~s applied to ~
                  right operand ~s."
            operator right-operand)))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :set-operation (interpreter node)
  (let ((operator      (node-attribute node :operator))
        (right-operand (node-attribute node :right-operand)))
    (declare (type set-operator operator))
    (declare (type Node         right-operand))
    (interpreter-apply-operator interpreter operator
      (interpreter-visit-node interpreter right-operand)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-loop-predicate-satisfied-p (interpreter
                                               predicate
                                               test-set)
  "Checks whether the INTERPRETER's main set satisfies the PREDICATE
   when supplied as its first operand in conjunction with the TEST-SET
   as second, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter      interpreter))
  (declare (type set-relationship predicate))
  (declare (type SOAPSet          test-set))
  (with-interpreter (interpreter)
    (the boolean
      (case predicate
        (:subset
          (soapset-subset-p main-set test-set))
        (:proper-subset
          (soapset-proper-subset-p main-set test-set))
        (:not-subset
          (soapset-not-subset-p main-set test-set))
        (:superset
          (soapset-superset-p main-set test-set))
        (:proper-superset
          (soapset-proper-superset-p main-set test-set))
        (:not-superset
          (soapset-not-superset-p main-set test-set))
        (:equal
          (soapset-equal-p main-set test-set))
        (otherwise
          (error "Invalid loop predicate: ~s." predicate))))))

;;; -------------------------------------------------------

(define-node-dispatch :loop (interpreter node)
  (let ((predicate (node-attribute node :predicate))
        (guard-set (node-attribute node :guard-set))
        (body      (node-attribute node :body)))
    (declare (type set-relationship predicate))
    (declare (type Node             guard-set))
    (declare (type node-list        body))
    (declare (ignorable             body))
    (flet
        ((predicate-satisfied-p ()
          "Checks whether the loop NODE's predicate is satisfied,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (the boolean
            (interpreter-loop-predicate-satisfied-p
              interpreter
              predicate
              (interpreter-visit-node interpreter guard-set)))))
      (loop while (predicate-satisfied-p) do
        (dolist (body-statement body)
          (declare (type Node body-statement))
          (interpreter-visit-node interpreter body-statement)))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :increment-variable (interpreter node)
  (with-interpreter (interpreter)
    (incf variable))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :decrement-variable (interpreter node)
  (with-interpreter (interpreter)
    (when (> variable 1)
      (decf variable)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :input-character (interpreter node)
  (with-interpreter (interpreter)
    (setf input (read-char)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :if-character (interpreter node)
  (let ((guard-character (node-attribute node :guard-character))
        (body-statements (node-attribute node :body)))
    (declare (type character guard-character))
    (declare (type node-list body-statements))
    (declare (ignorable      body-statements))
    (with-interpreter (interpreter)
      (when (char= guard-character input)
        (dolist (body-statement body-statements)
          (declare (type Node body-statement))
          (interpreter-visit-node interpreter body-statement)))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree (AST) maintained by the
   INTERPRETER and returns its last evaluated statement's result."
  (declare (type Interpreter interpreter))
  (the T
    (interpreter-visit-node interpreter
      (interpreter-tree interpreter))))

;;; -------------------------------------------------------

(defun interpret-SOAP (code)
  "Interprets the piece of SOAP source CODE and returns the last
   evaluated statement's result."
  (declare (type string code))
  (the T
    (interpreter-interpret
      (make-interpreter
        (parser-parse
          (make-parser
            (make-lexer code)))))))
