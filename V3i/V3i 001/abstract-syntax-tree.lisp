;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file accommodates a woning to the abstract syntax tree (AST)
;; model, being an establishment of a parsed V3i program's hierarchical
;; representation in the form of nodes.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface establishes a common foundry for all
   classes nuncupated to the pursuit of representing a V3i language
   facility in an abstract syntax tree (AST) node's guise.")

;;; -------------------------------------------------------

(defstruct (Expression-Node
  (:include AST-Node))
  "The ``Expression-Node'' interface establishes a common foundry for
   all classes accommodated for the representation of expressions in
   the V3i programming language, formulated as abstract syntax tree
   (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include AST-Node))
  "The ``Block-Node'' class serves in the encapsulation of an ordered
   list of zero or more V3i statements, themselves represented as
   abstract syntax tree (AST) nodes."
  (statements (error "Missing block node statements.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include Expression-Node))
  "The ``Binary-Operation-Node'' class serves in the encapsulation of a
   binary operation as an abstract syntax tree (AST) node."
  (operator      (error "Missing binary operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      Expression-Node
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      Expression-Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Finite-Loop-Node
  (:include AST-Node))
  "The ``Finite-Loop-Node'' class serves in the encapsulation of a
   finite iterance construct, such metes its recurrences by a given
   repetition tally, molded into an abstract syntax tree (AST) node."
  (repetitions (error "Missing finite loop repetitions.")
               :type      Expression-Node
               :read-only T)
  (statements  (error "Missing finite loop statements.")
               :type      Block-Node
               :read-only T))

;;; -------------------------------------------------------

(defstruct (If-Node
  (:include AST-Node))
  "The ``If-Node'' class serves in the encapsulation of a conditional
   statement as an abstract syntax tree (AST) node."
  (antecedent  (error "Missing antecedent.")
               :type      Expression-Node
               :read-only T)
  (consequent  (error "Missing consequent.")
               :type      Block-Node
               :read-only T)
  (alternative NIL
               :type      (or null AST-Node)
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Infinite-Loop-Node
  (:include AST-Node))
  "The ``Infinite-Loop-Node'' class serves in the encapsulation of an
   infinite iterance construct, bound into perpetuality in its
   recurrences, molded into an abstract syntax tree (AST) node."
  (statements  (error "Missing infinite loop statements.")
               :type      Block-Node
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include Expression-Node))
  "The ``Input-Node'' class serves in the encapsulation of a user input
   behest as an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Number-Node
  (:include Expression-Node))
  "The ``Number-Node'' class serves in the encapsulation of a unsigned
   literal integer as an abstract syntax tree (AST) node."
  (value (error "Missing number node value.")
         :type      unsigned-integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Print-Node
  (:include AST-Node))
  "The ``Print-Node'' class serves in the encapsulation of a print
   behest as an abstract syntax tree (AST) node."
  (arguments (error "Missing print arguments.")
             :type      node-list
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include AST-Node))
  "The ``Program-Node'' class serves in the encapsulation of a V3i
   program as an abstract syntax tree (AST) node."
  (statements (error "Missing program statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Sqrt-Node
  (:include Expression-Node))
  "The ``Sqrt-Node'' class serves in the encapsulation of a square root
   supputation behest as an abstract syntax tree (AST) node."
  (argument (error "Missing sqrt arguments.")
            :type      Expression-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (String-Node
  (:include Expression-Node))
  "The ``String-Node'' class serves in the encapsulation of a literal
   string of any conformation as an abstract syntax tree (AST) node."
  (value (error "Missing string node value.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Operation-Node
  (:include Expression-Node))
  "The ``Unary-Operation-Node'' class serves in the encapsulation of a
   unary operation as an abstract syntax tree (AST) node."
  (operator (error "Missing unary operator.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing unary operand.")
            :type      Expression-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Node
  (:include Expression-Node))
  "The ``Variable-Node'' class serves in the encapsulation of a variable
   identifier in the form of an abstract syntax tree (AST) node."
  (name (error "Missing variable node name.")
        :type      string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Call-Node
  (:include Expression-Node))
  "The ``Variable-Call-Node'' class serves in the encapsulation of a
   variable invocation instruction in the form of an abstract syntax
   tree (AST) node."
  (name (error "Missing variable call node name.")
        :type      string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Definition-Node
  (:include AST-Node))
  "The ``Variable-Definition-Node'' class serves in the encapsulation of
   a variable definition or assignment instruction in the form of an
   abstract syntax tree (AST) node."
  (name  (error "Missing definition node name.")
         :type      string
         :read-only T)
  (value (error "Missing definition node value.")
         :type      AST-Node
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Wait-Node
  (:include AST-Node))
  "The ``Wait-Node'' class serves in the encapsulation of a wait behest
   as an abstract syntax tree (AST) node."
  (delay (error "Missing wait delay.")
         :type      Expression-Node
         :read-only T))
