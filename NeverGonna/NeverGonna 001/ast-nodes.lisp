;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the abstract syntax tree (AST) node variants.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface provides a foundry for all classes intent
   on the definition of abstract syntax tree (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include AST-Node))
  "The ``Block-Node'' class establishes a compound of zero or more
   statements in the guise of an abstract syntax tree (AST) node."
  (statements (error "Missing statements.")
              :type      node-list
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include AST-Node))
  "The ``Program-Node'' class' deter encompasses the provision of the
   abstract syntax tree (AST) root node."
  (statements (error "Missing statements.")
              :type      Block-Node
              :read-only T))

;;; -------------------------------------------------------

(defstruct (Boolean-Node
  (:include AST-Node))
  "The ``Boolean-Node'' class encapsulates a Boolean truth value in the
   guise of an abstract syntax tree (AST) node."
  (value (error "Missing value.") :type boolean :read-only T))

;;; -------------------------------------------------------

(defstruct (Number-Node
  (:include AST-Node))
  "The ``Number-Node'' class encapsulates a literal integer for
   employment in an abstract syntax tree (AST)."
  (value (error "Missing value.") :type integer :read-only T))

;;; -------------------------------------------------------

(defstruct (String-Node
  (:include AST-Node))
  "The ``String-Node'' class encapsulates a literal string for
   employment in an abstract syntax tree (AST)."
  (value (error "Missing value.") :type string :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Node
  (:include AST-Node))
  "The ``Variable-Node'' class encapsulates a variable reference,
   established through the placeholder's identifier."
  (name (error "Missing name.") :type string :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Declaration-Node
  (:include AST-Node))
  "The ``Variable-Declaration-Node'' class encapsulates the properties
   necessary for replicating the declaration of a variable.
   ---
   This class represents the NeverGonna language facility
     we're no strangers to {variable}"
  (variable (error "Missing variable.")
            :type      Variable-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Assignment-Node
  (:include AST-Node))
  "The ``Variable-Assignment-Node'' class encapsulates the properties
   necessary for replicating the assignment of a value to a variable.
   ---
   This class represents the NeverGonna language facility
     gotta make {variable} {value}"
  (variable (error "Missing variable.")
            :type      Variable-Node
            :read-only T)
  (value    (error "Missing value.")
            :type      AST-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Print-Node
  (:include AST-Node))
  "The ``Print-Node'' class encapsulates an output command, encompassing
   the message intended for delivery.
   ---
   This class represents the NeverGonna language facility
     i just wanna tell you {message}"
  (message (error "Missing message.") :type AST-Node :read-only T))

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include AST-Node))
  "The ``Input-Node'' class encapsulates a request for user input,
   operating in conjunction with a prompt message.
   ---
   This class represents the NeverGonna language facility
     your heart's been aching but you're too shy to say {prompt}"
  (prompt (error "Missing prompt.") :type AST-Node :read-only T))

;;; -------------------------------------------------------

(defstruct (If-Case-Node
  (:include AST-Node))
  "The ``If-Case-Node'' class serves as an encapsulation of an
   \"if-then\" or \"else if\" component in an \"if\" conditional,
   contributing an abstract syntax tree (AST) node for the superposed
   ``If-Node'' tree.
   ---
   This class represents either of the NeverGonna facilities
     inside we both know <thenCondition> then
       <thenBody>
   and
     never gonna turn around <elseIfCondition> then
       <elseIfBody>"
  (condition (error "Missing condition.")
             :type      AST-Node
             :read-only T)
  (body      (error "Missing body.")
             :type      Block-Node
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Else-Case-Node
  (:include AST-Node))
  "The ``Else-Case-Node'' class encapsulates the optional default case
   in an \"if\" conditional, as the desinent member of an ``If-Node'',
   modeled by an abstract syntax tree (AST) node.
   ---
   This class represents the NeverGonna language facility
     never gonna let you down
       <elseBody>"
  (body (error "Missing body.")
        :type      Block-Node
        :read-only T))

;;; -------------------------------------------------------

(defstruct (If-Node
  (:include AST-Node))
  "The ``If-Node'' class encapsulates an \"if\" conditional, compact of
   zero or more ``If-Case-Node'' instances, the first representative of
   the \"if then\" parcel, the remnants of the \"else if\" components,
   and an ``Else-Case-Noe'' for the default case, manifesting in an
   abstract syntax tree (AST) node form.
   ---
   This class represents the NeverGonna language facility
     inside we both know <ifCondition> then
       <thenBody>
     
     never gonna turn around <elseIfCondition_1> then
       <elseIfBody_1>
     
     ...
     
     never gonna turn around <elseIfCondition_N> then
       <elseIfBody_N>
     
     never gonna let you down
       <elseBody>"
  (conditional-cases (error "Missing cases.")
                     :type      (list-of If-Case-Node)
                     :read-only T)
  (else-case         (error "Missing else.")
                     :type      (or null Else-Case-Node)
                     :read-only T))

;;; -------------------------------------------------------

(defstruct (For-Loop-Node
  (:include AST-Node))
  "The ``For-Loop-Node'' class encapsulates the properties necessary for
   replicating a counting iteration facility.
   ---
   This class represents the NeverGonna language facility
     we've known <variable> for {repetitions}
       {body}
     never gonna give you up"
  (variable     (error "Missing variable.")
                :type      Variable-Node
                :read-only T)
  (repetitions  (error "Missing repetitions.")
                :type      AST-Node
                :read-only T)
  (body         (error "Missing body.")
                :type      Block-Node
                :read-only T))

;;; -------------------------------------------------------

(defstruct  (While-Loop-Node
  (:include AST-Node))
  "The ``While-Loop'' class serves in the encapsulation of a NeverGonna
   \"while\" iteration construct, modeled as an abstract syntax tree
   (AST) node, the conformation of which embraces the perpetuation
   condition and a body of statements.
   ---
   This class represents the NeverGonna language facility
     a full commitment's what I'm thinking of <perpetuationCondition>
       <body>
     never gonna give you up"
  (condition (error "Missing condition.")
             :type      AST-Node
             :read-only T)
  (body      (error "Missing body.")
             :type      Block-Node
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include AST-Node))
  "The ``Binary-Operation-Node'' class serves as an encapsulation of a
   binary operation, appertaining to both arithmetic and logical in this
   agency, as an abstract syntax tree (AST) node."
  (operator      (error "Missing operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      AST-Node
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      AST-Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Operation-Node
  (:include AST-Node))
  "The ``Unary-Operation-Node'' encapsulates a unary operation, both
   arithmetic and logical in its potential, as an abstract syntax tree
   (AST) node."
  (operator (error "Missing operator.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing operand.")
            :type      AST-Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Group-Node
  (:include AST-Node))
  "The ``Group-Node'' class acquires the agency of a grouping entity in
   an expression's context, such is represetend by a parenthetical
   jumelle (\"(\" and \")\"), furnishing its latreutical conation as an
   abstract syntax tree (AST) node."
  (body (error "Missing body.") :type AST-Node :read-only T))
