;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the expression parser component, adhering to
;; the principles of the Pratt parser.
;; 
;; Please heed that this file's content lays its amplection solely
;; across the parselets' class and operation provisions; the actual
;; definition of the various "null denotation" (nud), "left denotation"
;; (led), and "statement denotation" (std) parselets is relocated to
;; "parser.lisp" file's bailiwick, which please consult.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token) integer) get-nud-binding-power))
(declaim (ftype (function (TOken) integer) get-led-binding-power))
(declaim (ftype (function (integer Token-Stream) Expression-Node)
                parse-expression))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operator precedence.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supputate-effective-binding-power (basic-binding-power
                                          associativity)
  "Returns the effective binding power as the coefficient gendrure of
   the BASIC-BINDING-POWER, appertaining to the general case of two
   competing operators, and the ASSOCIATIVITY, furnishing a mechanism
   for the conflict's resolution betwixt two equipollent participants."
  (declare (type integer       basic-binding-power))
  (declare (type associativity associativity))
  (the integer
    (case associativity
      ((:none :left)
        basic-binding-power)
      (:right
        (1- basic-binding-power))
      (otherwise
        (error "Invalid associativity: ~a." associativity)))))

;;; -------------------------------------------------------

(defstruct (Precedence
  (:constructor make-precedence
    (basic-binding-power associativity
     &aux (effective-binding-power
            (supputate-effective-binding-power
              basic-binding-power
              associativity))))
  (:constructor make-neutral-precedence
    (&aux (basic-binding-power     0)
          (associativity           :none)
          (effective-binding-power 0))))
  "The ``Precedence'' class applies itself to the ensconcement of all
   information imposing a requisitum to a precedence configuration's
   modeling, amplecting in this diorism both the basic binding power
   and the associativity."
  (basic-binding-power     0     :type integer       :read-only T)
  (associativity           :none :type associativity :read-only T)
  (effective-binding-power 0     :type integer       :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet interface.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ()
  (:documentation
    "The ``Parselet'' interface serves in the accommodation of a common
     foundry for all reifications of the parsing adminicula complying
     with the Pratt evaluation designment."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of nud parselet.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nud-Parselet (Parselet)
  ((transformer
    :initarg       :transformer
    :initform      (error "Missing nud transformer.")
    :reader        nud-parselet-transformer
    :type          nud-transformer
    :documentation "The callback function responsible for the actual
                    transformation of the nud token into an abstract
                    syntax tree (AST) node.")
   (precedence
    :initarg       :precedence
    :initform      (make-neutral-precedence)
    :reader        nud-parselet-precedence
    :type          Precedence
    :documentation "The nud token's optional precedence, the same
                    defaults to a neutral specification upon
                    omission."))
  (:documentation
    "The ``Nud-Parselet'' class is apportioned that dever to parse a
     nud token, its reliance registered at a dextral expression at most,
     with no sinistral contribution's necessitation."))

;;; -------------------------------------------------------

(defun make-nud-parselet (transformer
                          &optional (precedence
                                      (make-neutral-precedence)))
  "Creates and returns a fresh ``Nud-Parselet'' whose parsing effort it
   ensconced in the TRANSFORMER function, endowed with an optional
   PRECEDENCE for resolving the contingent agons with competing
   operators."
  (declare (type nud-transformer transformer))
  (declare (type Precedence      precedence))
  (the Nud-Parselet
    (make-instance 'Nud-Parselet
      :transformer transformer
      :precedence  precedence)))

;;; -------------------------------------------------------

(defun apply-nud-parselet (parselet current-token tokens)
  "Applies the nud PARSELET to the CURRENT-TOKEN, employing the stream
   of TOKENS for a contingent dextral operand's obtention, and returns
   an abstract syntax tree (AST) node representation of the parse
   result."
  (declare (type Nud-Parselet parselet))
  (declare (type Token        current-token))
  (declare (type Token-Stream tokens))
  (the AST-Node
    (funcall
      (nud-parselet-transformer parselet)
      current-token tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of led parselet.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Led-Parselet (Parselet)
  ((transformer
    :initarg       :transformer
    :initform      (error "Missing led transformer.")
    :reader        led-parselet-transformer
    :type          led-transformer
    :documentation "The callback function responsible for the actual
                    transformation of the led token into an abstract
                    syntax tree (AST) node.")
   (precedence
    :initarg       :precedence
    :initform      (error "Missing led parselet precedence.")
    :reader        led-parselet-precedence
    :type          Precedence
    :documentation "The led token's obligatory precedence."))
  (:documentation
    "The ``Led-Parselet'' class is apportioned that dever to parse a
     led token, its reliance registered at a mandatory sinistral and
     an optional dextral expression."))

;;; -------------------------------------------------------

(defun make-led-parselet (transformer precedence)
  "Creates and returns a fresh ``Led-Parselet'' whose parsing effort it
   ensconced in the TRANSFORMER function, endowed with the PRECEDENCE
   for resolving agons with competing operators."
  (declare (type led-transformer transformer))
  (declare (type Precedence      precedence))
  (the Led-Parselet
    (make-instance 'Led-Parselet
      :transformer transformer
      :precedence  precedence)))

;;; -------------------------------------------------------

(defun apply-led-parselet (parselet current-token tokens left-node)
  "Applies the led parselet to the CURRENT-TOKEN, contingently employing
   the stream of TOKENS for a dextral operand's obtention, while
   amplecting the LEFT-NODE as the sinistral operand, and returns an
   abstract syntax tree (AST) node representation of the parse result."
  (declare (type Led-Parselet parselet))
  (declare (type Token        current-token))
  (declare (type Token-Stream tokens))
  (declare (type AST-Node     left-node))
  (the AST-Node
    (funcall
      (led-parselet-transformer parselet)
      current-token tokens left-node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of std parselet.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Std-Parselet (Parselet)
  ((transformer
    :initarg       :transformer
    :initform      (error "Missing std transformer.")
    :reader        std-parselet-transformer
    :type          std-transformer
    :documentation "The callback function responsible for the actual
                    transformation of the std token into an abstract
                    syntax tree (AST) node."))
  (:documentation
    "The ``Std-Parselet'' class is apportioned that dever to parse an
     std token, its reliance registered at a dextral expression at most,
     with no sinistral contribution's necessitation."))

;;; -------------------------------------------------------

(defun make-std-parselet (transformer)
  "Creates and returns a fresh ``Std-Parselet'' whose parsing effort it
   ensconced in the TRANSFORMER function."
  (declare (type std-transformer transformer))
  (the Std-Parselet
    (make-instance 'Std-Parselet :transformer transformer)))

;;; -------------------------------------------------------

(defun apply-std-parselet (parselet current-token tokens)
  "Applies the std PARSELET to the CURRENT-TOKEN, employing the stream
   of TOKENS for a contingent dextral operand's obtention, and returns
   an abstract syntax tree (AST) node representation of the parse
   result."
  (declare (type Std-Parselet parselet))
  (declare (type Token        current-token))
  (declare (type Token-Stream tokens))
  (the AST-Node
    (funcall
      (std-parselet-transformer parselet)
      current-token tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet creation operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-nud-transformer ((token-name token-stream-name)
                                  &body body)
  "Defines a nud transformer function, its first formal parameter's
   agnomination being appropriated from the TOKEN-NAME, and the second
   from the TOKEN-STREAM-NAME, evaluates the BODY forms, and returns the
   desinent form's results."
  `(the function
     #'(lambda (,token-name ,token-stream-name)
         (declare (type Token        ,token-name))
         (declare (ignorable         ,token-name))
         (declare (type Token-Stream ,token-stream-name))
         (declare (ignorable         ,token-stream-name))
         ,@body)))

;;; -------------------------------------------------------

(defmacro define-led-transformer
    ((token-name token-stream-name left-node-name)
     &body body)
  "Defines a led processor function, its first formal parameter's
   agnomination being appropriated from the TOKEN-NAME, the second from
   the TOKEN-STREAM-NAME, and the third from the LEFT-NODE-NAME,
   evaluates the BODY forms, and returns the desinent form's results."
  `(the function
     #'(lambda (,token-name ,token-stream-name ,left-node-name)
         (declare (type Token        ,token-name))
         (declare (ignorable         ,token-name))
         (declare (type Token-Stream ,token-stream-name))
         (declare (ignorable         ,token-stream-name))
         (declare (type T            ,left-node-name))
         (declare (ignorable         ,left-node-name))
         ,@body)))

;;; -------------------------------------------------------

(defmacro define-std-transformer ((token-name token-stream-name)
                                  &body body)
  "Defines a std transformer function, its first formal parameter's
   agnomination being appropriated from the TOKEN-NAME, and the second
   from the TOKEN-STREAM-NAME, evaluates the BODY forms, and returns the
   desinent form's results."
  `(the function
     #'(lambda (,token-name ,token-stream-name)
         (declare (type Token        ,token-name))
         (declare (ignorable         ,token-name))
         (declare (type Token-Stream ,token-stream-name))
         (declare (ignorable         ,token-stream-name))
         ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet registries.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Nud-Parselet) *nud-parselets*))
(declaim (type (hash-table-of keyword Led-Parselet) *led-parselets*))
(declaim (type (hash-table-of keyword Std-Parselet) *std-parselets*))

;;; -------------------------------------------------------

(defparameter *nud-parselets*
  (make-hash-table :test #'eq)
  "Associates the recognized nud token types with parselets dedicated to
   their transformation into an expression.")

(defparameter *led-parselets*
  (make-hash-table :test #'eq)
  "Associates the recognized led token types with parselets dedicated to
   their transformation into an expression.")

(defparameter *std-parselets*
  (make-hash-table :test #'eq)
  "Associates the recognized std token types with parselets dedicated to
   their transformation into a statement.")

;;; -------------------------------------------------------

(defun register-nud-parselet (token-type parselet)
  "Associates the nud TOKEN-TYPE with the PARSELET in the
   *nud-parselets* table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Nud-Parselet parselet))
  (setf (gethash token-type *nud-parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-unary-parselet (token-type binding-power associativity)
  "Creates a fresh ``Nud-Parselet'' which produces from its input a
   ``Unary-Operation-Node'', associates the same with the TOKEN-TYPE in
   the *nud-parselets* table, and returns no value."
  (declare (type keyword       token-type))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-nud-parselet token-type
    (make-nud-parselet
      (define-nud-transformer (current-token tokens)
        (make-unary-operation-node
          :operator token-type
          :operand  (parse-expression
                      (get-nud-binding-power current-token)
                      tokens)))
      (make-precedence binding-power associativity)))
  (values))

;;; -------------------------------------------------------

(defun nud-token-p (token)
  "Determines whether the TOKEN subsumes into the nud species, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token) *nud-parselets*)))))

;;; -------------------------------------------------------

(defun get-nud-parselet (token)
  "Returns for the TOKEN the affiliated nud parselet, if such exists,
   otherwise signals an error of an unspecified type."
  (declare (type Token token))
  (the Nud-Parselet
    (or (gethash (token-type token) *nud-parselets*)
        (error "The token ~a does not represent a nud token." token))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (token)
  "Returns the nud TOKEN's effective binding power."
  (declare (type Token token))
  (the integer
    (precedence-effective-binding-power
      (nud-parselet-precedence
        (get-nud-parselet token)))))

;;; -------------------------------------------------------

(defun parse-nud-token (token tokens)
  "Parses the nud TOKEN, utilizing the stream of TOKENS for the
   contingency of further tokens' obtention, and returns an abstract
   syntax tree (AST) representation of its efforts."
  (declare (type Token token))
  (the AST-Node
    (apply-nud-parselet
      (get-nud-parselet token)
      token tokens)))

;;; -------------------------------------------------------

(defun register-led-parselet (token-type parselet)
  "Associates the led TOKEN-TYPE with the PARSELET in the
   *led-parselets* table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Led-Parselet parselet))
  (setf (gethash token-type *led-parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-binary-parselet (token-type binding-power associativity)
  "Creates a fresh ``Led-Parselet'' which produces from its input a
   ``Binary-Operation-Node'', associates the same with the TOKEN-TYPE in
   the *led-parselets* table, and returns no value."
  (declare (type keyword       token-type))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-led-parselet token-type
    (make-led-parselet
      (define-led-transformer (current-token tokens left-node)
        (make-binary-operation-node
          :operator      token-type
          :left-operand  left-node
          :right-operand (parse-expression
                           (get-led-binding-power current-token)
                           tokens)))
      (make-precedence binding-power associativity)))
  (values))

;;; -------------------------------------------------------

(defun led-token-p (token)
  "Determines whether the TOKEN subsumes into the led species, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token) *led-parselets*)))))

;;; -------------------------------------------------------

(defun get-led-parselet (token)
  "Returns for the TOKEN the affiliated led parselet, if such exists,
   otherwise signals an error of an unspecified type."
  (declare (type Token token))
  (the Led-Parselet
    (or (gethash (token-type token) *led-parselets*)
        (error "The token ~a does not represent a led token." token))))

;;; -------------------------------------------------------

(defun get-led-binding-power (token)
  "Returns the led TOKEN's effective binding power."
  (declare (type Token token))
  (the integer
    (precedence-effective-binding-power
      (led-parselet-precedence
        (get-led-parselet token)))))

;;; -------------------------------------------------------

(defun token-binds-expression-p (candidate-token current-binding-power)
  "Determines whether the CANDIDATE-TOKEN tends to bind the subsequent
   token or expression when engaged in an agon for the same with an
   operator endowed with the CURRENT-BINDING-POWER, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   The CANDIDATE-TOKEN's superiority occurs if and only if the following
   twissel of antecedents is satisfied:
     (1) The CANDIDATE-TOKEN represents a led token.
     (2) The CANDIDATE-TOKEN's effective binding power is strictly
         greater than the CURRENT-BINDING-POWER."
  (declare (type Token   candidate-token))
  (declare (type integer current-binding-power))
  (the boolean
    (get-boolean-value-of
      (and
        (led-token-p candidate-token)
        (> (get-led-binding-power candidate-token)
           current-binding-power)))))

;;; -------------------------------------------------------

(defun parse-led-token (token tokens left-node)
  "Parses the led TOKEN, utilizing the stream of TOKENS for the
   contingency of further tokens' obtention, while employing the
   LEFT-NODE as the sinistral operand, and returns an abstract syntax
   tree (AST) representation of its efforts."
  (declare (type Token token))
  (the AST-Node
    (apply-led-parselet
      (get-led-parselet token)
      token tokens left-node)))

;;; -------------------------------------------------------

(defun register-std-parselet (token-type parselet)
  "Associates the std TOKEN-TYPE with the PARSELET in the
   *std-parselets* table and returns no value."
  (declare (type keyword      token-type))
  (declare (type Std-Parselet parselet))
  (setf (gethash token-type *std-parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-statement (token-type transformer)
  "Creates a fresh ``Std-Parselet'', invested with the TRANSFORMER as
   the callback function responsible for an abstract syntax tree (AST)
   replication of a V3i statement, associates this parselet with the
   TOKEN-TYPE in the *std-parselets* table, and returns no value."
  (declare (type keyword         token-type))
  (declare (type std-transformer transformer))
  (register-std-parselet token-type
    (make-std-parselet transformer))
  (values))

;;; -------------------------------------------------------

(defun std-token-p (token)
  "Determines whether the TOKEN subsumes into the std species, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash (token-type token) *std-parselets*)))))

;;; -------------------------------------------------------

(defun get-std-parselet (token)
  "Returns for the TOKEN the affiliated std parselet, if such exists,
   otherwise signals an error of an unspecified type."
  (declare (type Token token))
  (the Std-Parselet
    (or (gethash (token-type token) *std-parselets*)
        (error "The token ~a does not represent an std token." token))))

;;; -------------------------------------------------------

(defun parse-std-token (token tokens)
  "Parses the std TOKEN, utilizing the stream of TOKENS for the
   contingency of further tokens' obtention, and returns an abstract
   syntax tree (AST) representation of its efforts."
  (declare (type Token token))
  (the AST-Node
    (apply-std-parselet
      (get-std-parselet token)
      token tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression parser operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (current-binding-power tokens)
  "Parses an expression pursuing the Pratt concept, the invoking
   operator being represented by the CURRENT-BINDING-POWER, while the
   stream of TOKENS serves to furnish access to the requisite
   information, and returns an abstract syntax tree (AST) representation
   of the parsed expression."
  (declare (type integer      current-binding-power))
  (declare (type Token-Stream tokens))
  (let ((left-node
          (parse-nud-token
            (consume-token tokens)
            tokens)))
    (declare (type AST-Node left-node))
    (loop
      for   next-token of-type Token = (peek-token tokens)
      while (token-binds-expression-p next-token current-binding-power)
      do
        (consume-token tokens)
        (setf left-node
          (parse-led-token next-token tokens left-node)))
    (the AST-Node left-node)))

;;; -------------------------------------------------------

(defun parse-expression-list (current-binding-power tokens)
  "Parses an ordered list of zero or more expressions and returns the
   resulting node list."
  (declare (type integer      current-binding-power))
  (declare (type Token-Stream tokens))
  (the node-list
    (loop while (nud-token-p (peek-token tokens)) collect
      (parse-expression current-binding-power tokens))))
