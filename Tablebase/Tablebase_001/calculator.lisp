;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file comprehends the definition of the Tablebase calculator.
;; 
;; The calculator program is designed in concord with the Pratt parser's
;; notions, following an object-oriented approach.
;; 
;; The precedences' apportionment issues from a loose adherence to the
;; definitions stated in [grand1997javalangref]:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Description           | Type    | Precedence | Assoc.
;;   ---------+-----------------------+---------+------------+---------
;;   +        | positive sign         | prefix  |     70     | right
;;   ..................................................................
;;   -        | negative sign         | prefix  |     70     | left
;;   ..................................................................
;;   !        | factorial             | postfix |     60     | left
;;   ..................................................................
;;   ^        | power, exponentiation | infix   |     40     | right
;;   ..................................................................
;;   *        | multiplication        | infix   |     30     | left
;;   ..................................................................
;;   /        | division              | infix   |     30     | left
;;   ..................................................................
;;   +        | addition              | infix   |     20     | left
;;   ..................................................................
;;   -        | substraction          | infix   |     20     | left
;;   ..................................................................
;;   f()      | function invocation   | prefix  |      0     | none
;;   ..................................................................
;;   (        | group                 | nilfix  |      0     | none
;;   ------------------------------------------------------------------
;; 
;; Please note the correspondency betwixt the operator types and the
;; parselet categories incumbent in their representation:
;; 
;;   ------------------------------------------------------------------
;;   Operator type |               Parselet type
;;                 |...................................................
;;                 | Desmos nomenclature | Pratt nomenclature
;;   --------------+---------------------+-----------------------------
;;   nilfix        | initial             | nud
;;   ..................................................................
;;   prefix        | initial             | nud
;;   ..................................................................
;;   infix         | consequent          | led
;;   ..................................................................
;;   postfix       | consequent          | led
;;   ------------------------------------------------------------------
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of package.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :tablebase.calculator
  (:use
    :cl)
  (:export
    #:make-calculator
    #:calculator-start))

;;; -------------------------------------------------------

(in-package :tablebase.calculator)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a mapping of
   variable names to numbers, represented in terms of a hash table whose
   arbitrarily-typed keys associated with ``number'' objects."
  '(hash-table-of T number))

;;; -------------------------------------------------------

(deftype parameter-map ()
  "The ``parameter-map'' type defines a mapping from parameter names to
   values for the deployment in expressions, realized as a property
   list, or plist, of keyword symbols affiliated with arbitrary values."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for left-element
                of-type T
                in      (the list candidate)
                by      #'cddr
              always
                (keywordp left-element)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype formal-parameter-list ()
  "The ``formal-parameter-list'' defines a list of type specifiers
   acting in the agency of formal parameters, that is, such that are
   imposed as the interface to a function."
  '(list-of (or symbol list)))

;;; -------------------------------------------------------

(deftype actual-parameter-list ()
  "The ``actual-parameter-list'' defines a list of arbitrary objects
   acting in the agency of actual parameters, that is, such that are
   employed during the invocation of a function."
  '(list-of T))

;;; -------------------------------------------------------

(deftype procedure-table ()
  "The ``procedure-table'' type defines a mapping of procedure name
   identifiers to actual procedures, manifesting as a hash table from
   keyword symbols to ``Procedure'' objects."
  '(hash-table-of keyword Procedure))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, such
   as ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized variants of
   associativity appertaining to operators and their agon with peers of
   equal binding power."
  '(member
    :non-associative
    :left-associative
    :right-associative))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Context".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Context ()
  ((memory
    :initarg       :memory
    :initform      (make-hash-table :test #'equal)
    :type          memory
    :documentation "Maps variable names to numeric values.")
   (last-result
    :initarg       :last-result
    :initform      0
    :type          number
    :documentation "The value yielded by the most recent calculation."))
  (:documentation
    "The ``Context'' class encapsulates the data requisite for the
     application of a ``Procedure'', encompassing especially the program
     memory."))

;;; -------------------------------------------------------

(defun context-store-in-memory (context name value)
  "Associates the NAME with the VALUE in the CONTEXT memory and returns
   the thus stored VALUE."
  (declare (type Context context))
  (declare (type T       name))
  (declare (type number  value))
  (setf (gethash name (slot-value context 'memory) 0) value)
  (the number value))

;;; -------------------------------------------------------

(defun context-recall-from-memory (context name)
  "Recalls from the CONTEXT memory the value associated with the NAME,
   or the default value zero (0) upon its absence."
  (declare (type Context context))
  (declare (type T       name))
  (the number (gethash name (slot-value context 'memory) 0)))

;;; -------------------------------------------------------

(defun context-delete-from-memory (context name)
  "Deletes from the CONTEXT memory the entry associated with the NAME
   and returns its value, or produces the default value zero (0) upon
   the NAME's absence."
  (declare (type Context context))
  (declare (type T       name))
  (the number
    (prog1
      (context-recall-from-memory context name)
      (remhash name (slot-value context 'memory)))))

;;; -------------------------------------------------------

(defun context-clear-memory (context)
  "Purges all entries from the CONTEXT memory and returns the default
   value zero (0)."
  (declare (type Context context))
  (the number
    (prog1 0
      (clrhash (slot-value context 'memory)))))

;;; -------------------------------------------------------

(defun context-get-memory (context)
  "Returns a direct reference to the CONTEXT memory."
  (declare (type Context context))
  (the memory (slot-value context 'memory)))

;;; -------------------------------------------------------

(defun context-store-result (context result)
  "Stores the last expression evaluation RESULT into the CONTEXT and
   returns the RESULT."
  (declare (type Context context))
  (declare (type number  result))
  (setf (slot-value context 'last-result) result)
  (the number result))

;;; -------------------------------------------------------

(defun context-repeat-last-result (context)
  "Restores the last result issued to the CONTEXT, or zero (0) upon its
   absence."
  (declare (type Context context))
  (the number
    (or (slot-value context 'last-result)
        0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of mathematical operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-degrees (angle-in-radians)
  "Converts the ANGLE-IN-RADIANS into degrees and returns the result."
  (declare (type real angle-in-radians))
  (the real
    (* angle-in-radians (/ 180 PI))))

;;; -------------------------------------------------------

(defun to-radians (angle-in-degrees)
  "Converts the ANGLE-IN-DEGREES into radians and returns the result."
  (declare (type real angle-in-degrees))
  (the real
    (* angle-in-degrees (/ PI 180))))

;;; -------------------------------------------------------

(defun make-logarithm-function-for-base (base)
  "Creates and returns a new unary function which upon its invocation
   returns the logarithm of its aefauld argument to the specified BASE.
   ---
   The thus produced function's signature conforms to:
     function (number) number
   ---
   Example:
     (let ((log2 (make-logarithm-function-for-base 2)))
       (declare (type (function (number) number) log2))
       ;; Returns 3.0.
       (funcall log2 8))"
  (declare (type number base))
  (the function
    #'(lambda (number)
        (declare (type number number))
        (the number
          (log number base)))))

;;; -------------------------------------------------------

(defun non-negative-factorial (n)
  "Calculates and returns the factorial n! of the non-negative integer
   N."
  (declare (type (integer 0 *) n))
  (the (integer 1 *)
    (case n
      ((0 1)
        1)
      (otherwise
        (loop
          for factor
            of-type (integer 1 *) from 1 to n
          for factorial
            of-type (integer 1 *)
            =       1
            then    (* factorial factor)
          finally
            (return factorial))))))

;;; -------------------------------------------------------

(defun roman-factorial (n)
  "Calculates and returns the Roman factorial for the integer N.
   ---
   The Roman factorial [n]! is defined by
   
     [n]! = { n!                           for n >= 0
            { ((-1)^{-n-1}) / ((-n-1)!)    for n <  0
     
     where
       n! --- the factorial for a non-negative integer."
  (declare (type integer n))
  (the (real -1.0 *)
    (cond
      ((>= n 0)
        (non-negative-factorial n))
      (T
        (let ((-n-1 (- (- n) 1)))
          (declare (type (integer 0 *) -n-1))
          (/ (expt (- 1) -n-1)
             (non-negative-factorial -n-1)))))))

;;; -------------------------------------------------------

(defun factorial (n)
  "Calculates and returns the factorial of the integer N."
  (declare (type integer n))
  (the (real -1.0 *)
    (roman-factorial n)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Procedure".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Procedure ()
  ((name
    :initarg       :name
    :initform      (error "Missing procedure name.")
    :reader        procedure-name
    :type          string
    :documentation "The procedure's identifying name.")
   (signature
    :initarg       :signature
    :initform      NIL
    :reader        procedure-signature
    :type          formal-parameter-list
    :documentation "The formal parameter list, specifying the expected
                    procedure input types.")
   (action
    :initarg       :action
    :initform      (error "Missing procedure action.")
    :type          function
    :documentation "A callback function which entails the procedures'
                    effect when applying the same to an actual parameter
                    list.")
   (description
    :initarg       :description
    :initform      NIL
    :accessor      procedure-description
    :type          (or null string)
    :documentation ""))
  (:documentation
    "The ``Procedure'' class encapsulates the information requisite for
     the representation of an operation, encompassing the identifying
     name, the formal parameter list, and a callback function which,
     upon the procedure's invocation, applies effect to its actual
     parameters."))

;;; -------------------------------------------------------

(defun procedure-signature-matches-p (procedure parameters)
  "Determines whether the actual PARAMETERS match the PROCEDURE's formal
   parameter signature, on confirmation returning a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Procedure             procedure))
  (declare (type actual-parameter-list parameters))
  (the boolean
    (not (null
      (every
        #'(lambda (parameter expected-type)
            (declare (type T parameter))
            (declare (type T expected-type))
            (typep parameter expected-type))
        parameters
        (slot-value procedure 'signature))))))

;;; -------------------------------------------------------

(defun procedure-check-parameters (procedure parameters)
  "Determines whether the actual PARAMETERS match the PROCEDURE's formal
   parameter signature, on confirmation returning the unmodified
   PARAMETERS, otherwise signaling an error of an unspecified type."
  (declare (type Procedure             procedure))
  (declare (type actual-parameter-list parameters))
  (unless (procedure-signature-matches-p procedure parameters)
    (error "Mismatching signature for procedure ~s: Expected ~s, ~
            but received ~s."
      (slot-value procedure 'name)
      (slot-value procedure 'signature)
      parameters))
  (the actual-parameter-list parameters))

;;; -------------------------------------------------------

(defun procedure-invoke (procedure context parameters)
  "Applies the PROCEDURE's callback function on the PARAMETERS and
   returns the result."
  (declare (type Procedure             procedure))
  (declare (type actual-parameter-list parameters))
  (the number
    (apply (slot-value procedure 'action) context
      (procedure-check-parameters procedure parameters))))

;;; -------------------------------------------------------

(defun procedure-print-description (procedure &key (destination T))
  "Prints to the DESTINATION a description of the PROCEDURE, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding
   with a fresh string comprehending the output."
  (declare (type Procedure   procedure))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let* ((formal-parameters (procedure-signature procedure))
             (arity             (length formal-parameters)))
        (declare (type formal-parameter-list formal-parameters))
        (declare (type (integer 0 *)         arity))
        ;; Print the procedure name and open the formal parameter list.
        (format destination "~a(" (procedure-name procedure))
        ;; Print the formal parameters.
        (case arity
          ;; No formal parameters to print?
          (0
            NIL)
          ;; Exactly one formal parameter to print?
          (arity 1
            (format destination "x : ~(~a~)"
              (first formal-parameters)))
          ;; Two or more formal parameters to print?
          (otherwise
            (loop
              for parameter-type    of-type T in formal-parameters
              and parameter-index   of-type (integer 0 *) from 1
              and first-parameter-p of-type boolean = T then NIL
              do
                (format destination "~:[, ~;~]x~d : ~(~a~)"
                  first-parameter-p parameter-index parameter-type))))
        ;; Close the formal parameter list and potentially append the
        ;; description text.
        (format destination ")~@[ --- ~a~]"
          (procedure-description procedure)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (procedure-print-description procedure
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of procedure table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type procedure-table +PROCEDURES+))

;;; -------------------------------------------------------

(defparameter +PROCEDURES+ (make-hash-table :test #'eq)
  "Registers procedures by their name for subsequent retrievals.")

;;; -------------------------------------------------------

(defun get-context-independent-function (callback)
  "Returns a new function which accepts but ignores a ``Context'' as its
   first parameter, followed by a ``&rest'' parameter list, invoking the
   CALLBACK with the variadic parameters, and returns the respective
   CALLBACK result."
  (declare (type function callback))
  (the function
    #'(lambda (context &rest parameters)
        (declare (type Context context))
        (declare (ignore       context))
        (declare (type list    parameters))
        (the T (apply callback parameters)))))

;;; -------------------------------------------------------

(flet ((register-context-independent-procedure
          (identifier name signature action
           &optional (description NIL))
        "Creates a new ``Procedure'' designated by the NAME, formally
         specified through its signature, and implemented using the
         ACTION, a callback which is wrapped into a function that
         ignores its first ``Context'' parameter and accepts a variadic
         tally of further inputs to be delegated to the ACTION,
         optionally associates the DESCRIPTION text with the procedure,
         registers the new procedure with the IDENTIFIER at the
         +PROCEDURES+ table, and returns no value.
         ---
         Any extant entry answering to the IDENTIFIER will be silently
         superseded."
        (declare (type keyword               identifier))
        (declare (type string                name))
        (declare (type formal-parameter-list signature))
        (declare (type function              action))
        (setf (gethash identifier +PROCEDURES+)
              (make-instance 'Procedure
                :name        name
                :signature   signature
                :action      (get-context-independent-function action)
                :description description))
        (values))
       
       (register-full-procedure
          (identifier name signature action
           &optional (description NIL))
        "Creates a new ``Procedure'' designated by the NAME, formally
         specified through its signature, and implemented using the
         ACTION, optionally associates the DESCRIPTION text with the
         procedures, registers the new procedure with the IDENTIFIER at
         the +PROCUEDURES+ table, and returns no value.
         ---
         Any extant entry answering to the IDENTIFIER will be silently
         superseded."
        (declare (type keyword               identifier))
        (declare (type string                name))
        (declare (type formal-parameter-list signature))
        (declare (type function              action))
        (setf (gethash identifier +PROCEDURES+)
              (make-instance 'Procedure
                :name        name
                :signature   signature
                :action      action
                :description description))
        (values)))
  
  (register-context-independent-procedure :abs
    "abs" '(number) #'abs
    "Returns the absolute of a number.")
  
  (register-context-independent-procedure :ceiling
    "ceiling" '(real) #'ceiling
    "Rounds a number up to the nearest integer greater than
     or equal to it.")
  
  (register-context-independent-procedure :complex
    "complex" '(real real) #'complex
    "Constructs a complex number from the real and imaginary parts.")
  
  (register-context-independent-procedure :cos
    "cos" '(number) #'cos
    "Returns the cosine of an angle in radians.")
  
  (register-context-independent-procedure :floor
    "floor" '(real) #'floor
    "Rounds a number down to the nearest integer less than
     or equal to it.")
  
  (register-context-independent-procedure :gcd
    "gcd" '(integer integer) #'gcd
    "Returns the greatest common divisor (GCD) of two integers.")
  
  (register-context-independent-procedure :imaginary-part
    "Im" '(number) #'imagpart
    "Returns the imaginary part of a number, or 0 if not complex.")
  
  (register-context-independent-procedure :lcm
    "lcm" '(integer integer) #'lcm
    "Returns the least common multiple (LCM) of two integers.")
  
  (register-context-independent-procedure :log
    "log" '(number number) #'log
    "Returns the logarithm of the first number to the second as its
     base.")
  
  (register-context-independent-procedure :log10
    "log10" '(number) (make-logarithm-function-for-base 10)
    "Returns the logarithm of the number to the base 10. This
     constitutes the decimal logarithm.")
  
  (register-context-independent-procedure :log2
    "log2" '(number) (make-logarithm-function-for-base 2)
    "Returns the logarithm of the number to the base 2. This constitutes
     the binary logarithm.")
  
  (register-context-independent-procedure :ln
    "ln" '(number) (make-logarithm-function-for-base (exp 1))
    "Returns the logarithm of the number to the base e (Euler's number).
     This constitutes the natural logarithm.")
  
  (register-context-independent-procedure :real-part
    "Re" '(number) #'realpart
    "Returns the real part of a number.")
  
  (register-context-independent-procedure :round
    "round" '(real) #'round
    "Rounds a number up or down to the nearest integer.")
  
  (register-context-independent-procedure :sin
    "sin" '(number) #'sin
    "Returns the sine of an angle in radians.")
  
  (register-context-independent-procedure :sqrt
    "sqrt" '(number) #'sqrt
    "Returns the square root of a number.")
  
  (register-context-independent-procedure :tan
    "tan" '(number) #'tan
    "Returns the tangent of an angle in radians.")
  
  (register-context-independent-procedure :to-degrees
    "toDegrees" '(real) #'to-degrees
    "Converts an angle in radians to degrees.")
  
  (register-context-independent-procedure :to-radians
    "toRadians" '(real) #'to-radians
    "Converts an angle in degrees to radians.")
  
  (register-full-procedure :store "store" '(T number)
    #'(lambda (context name value)
        (declare (type Context context))
        (declare (type T       name))
        (declare (type number  value))
        (the number (context-store-in-memory context name value)))
    "Evaluates the first input as a name and stores the second input as
     its computation result in the memory.")
  
  (register-full-procedure :recall "recall" '(T)
    #'(lambda (context name)
        (declare (type Context context))
        (declare (type T       name))
        (the number (context-recall-from-memory context name)))
    "Evaluates the first input as a name and returns from the memory the
     value associated with it, or 0 if absent.")
  
  (register-full-procedure :delete "delete" '(T)
    #'(lambda (context name)
        (declare (type Context context))
        (declare (type T       name))
        (the number (context-delete-from-memory context name)))
    "Evaluates the first input as a name, deletes from the memory the
     value previously associated with it, and returns the removed value,
     or 0 if absent.")
  
  (register-full-procedure :clear "clear" '()
    #'(lambda (context)
        (declare (type Context context))
        (the number (context-clear-memory context)))
    "Clears the memory and returns 0.")
  
  (register-full-procedure :repeat "repeat" '()
    #'(lambda (context)
        (declare (type Context context))
        (the number (context-repeat-last-result context)))
    "Returns the result of the most recent expression if numeric, or 0
     if none exists.")
  
  (values))

;;; -------------------------------------------------------

(defun get-procedure (name)
  "Returns the ``Procedure'' amenable to the NAME, or signals an error
   upon its disrespondency."
  (declare (type keyword name))
  (the Procedure
    (or (gethash name +PROCEDURES+)
        (error "Unrecognized procedure name: ~s." name))))

;;; -------------------------------------------------------

(defun invoke-function (name context parameters)
  "Retrieves the procedure amenable to the NAME, applies it by utilizing
   the PARAMETERS, and returns the procedure action's result."
  (declare (type keyword               name))
  (declare (type actual-parameter-list parameters))
  (the number
    (procedure-invoke (get-procedure name) context parameters)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token ()
  ((type
    :initarg       :type
    :initform      (error "Missing token type.")
    :reader        token-type
    :type          keyword
    :documentation "The categorizing token type.")
   (value
    :initarg       :value
    :initform      (error "Missing token value.")
    :reader        token-value
    :type          T
    :documentation "The value conveyed by the token."))
  (:documentation
    "The ``Token'' class encapsulates information about a significant
     object to perception during the lexical analyzation process."))

;;; -------------------------------------------------------

(defun make-token (type value)
  "Creates and returns a new ``Token'' belonging to the TYPE, and
   associated with the VALUE."
  (declare (type keyword type))
  (declare (type T       value))
  (the Token
    (make-instance 'Token :type type :value value)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (slot-value token 'type) expected-type)))))

;;; -------------------------------------------------------

(defmethod print-object ((token Token) stream)
  (declare (type Token       token))
  (declare (type destination stream))
  (format stream "(Token ~s ~s)"
    (slot-value token 'type)
    (slot-value token 'value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal)
  "Associates the recognized identifier names with representative
   tokens.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token-type)
        "Associates the NAME in the +IDENTIFIERS+ table with a newly
         created token of the TOKEN-TYPE, bearing the NAME itself as its
         value, and returns no value.
         ---
         Any extant entry with the NAME as its key will be tacitly
         superseded by the new information."
        (declare (type string  name))
        (declare (type keyword token-type))
        (setf (gethash name +IDENTIFIERS+)
              (make-token token-type name))
        (values)))
  
  (register-identifier "exit"   :exit)
  (register-identifier "help"   :help)
  (register-identifier "memory" :memory)
  
  ;; Register each procedure from the +PROCEDURES+ table using its
  ;; identifier key and the procedure name.
  (maphash
    #'(lambda (procedure-identifier procedure)
        (declare (type keyword   procedure-identifier))
        (declare (type Procedure procedure))
        (register-identifier
          (procedure-name procedure)
          procedure-identifier)
        (values))
    +PROCEDURES+)
  
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token associated in the +IDENTIFIERS+ table with the
   IDENTIFIER name, or creates and returns a fresh ``:identifier'' token
   upon its absence."
  (declare (type string identifier))
  (the Token
    (gethash identifier +IDENTIFIERS+
      (make-token :identifier identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a character admissive to
   an identifier name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (char= candidate #\_))))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical signum,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      ""
    :type          string
    :documentation "The input --- either an arithmetic expression or a
                    metacommand --- to analyze into tokens.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position in the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE, or ``NIL'' if the latter is exhausted."))
  (:documentation
    "In the ``Lexer'' class a unit responsible for the detection,
     extraction, and provision of significant objects from an input
     string in the form of tokens is supplied."))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'' and
   ``character'' to eponymous local symbol macros, processes the BODY
   forms, and returns the last evaluated form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer)
                (ignorable  ,evaluated-lexer))
       (with-slots (source position character) ,evaluated-lexer
         (declare (type string              source)
                  (ignorable                source))
         (declare (type fixnum              position)
                  (ignorable                position))
         (declare (type (or null character) character)
                  (ignorable                character))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (&optional (source ""))
  "Creates and returns a new ``Lexer'', optionally initialized with the
   SOURCE."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-set-source (lexer new-source)
  "Sets the LEXER to the NEW-SOURCE, resets its state, and returns the
   modified LEXER."
  (declare (type Lexer  lexer))
  (declare (type string new-source))
  (with-lexer (lexer)
    (setf source    new-source)
    (setf position  0)
    (setf character
      (when (array-in-bounds-p new-source position)
        (char new-source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next location in its source,
   if possible, updates the LEXER's, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the (or null character)
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source (incf position))))))))

;;; -------------------------------------------------------

(defun lexer-extract-number (lexer)
  "Commencing from the current position into the LEXER's source, reads
   an integer or floating-point number and returns two values:
     (1) the string comprehending the detected number
     (2) a keyword symbol which amounts to ``:integer'' for a detected
         integer value, or to ``:float'' for a floating-pointer object."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (let ((number-type :integer))
      (declare (type keyword number-type))
      (with-open-stream (digits (make-string-output-stream))
        (declare (type string-stream digits))
        (labels
            ((read-optional-sign ()
              "If the current LEXER character constitutes a mathematical
               sign, writes the same to the DIGITS stream and advances
               the LEXER, otherwise exerts no effect, in any case
               returning no value."
              (when (and character (sign-character-p character))
                (write-char (lexer-advance lexer) digits))
              (values)) 
             
             (expect-digit ()
              "Determines whether the current LEXER character represents
               a decimal digit, on confirmation exerting no further
               effect, otherwise signaling an error of an unspecified
               type, in any case returning no value."
              (unless (and character (digit-char-p character))
                (error "Expected a decimal digit at the position ~d, ~
                        but encountered ~s."
                  position character))
              (values))
             
             (read-digits ()
              "Expects a sequence of one or more decimal digits, which
               are written to the DIGITS stream, returning no value.
               ---
               Upon failure to locate a decimal at this function call's
               inchoation, an error of an unspecified type is signaled."
              (expect-digit)
              (loop while (and character (digit-char-p character)) do
                (write-char (lexer-advance lexer) digits))
              (values))
             
             (read-optional-decimal-separator ()
              "Determines whether the current LEXER character
               constitutes a decimal separator, that is, the dot
               (\".\"), on confirmation writing the same to the DIGITS
               stream and setting the NUMBER-TYPE to ``:float'',
               otherwise exerting to effect, and in any case returning
               no value."
              (when (and character (char= character #\.))
                (write-char (lexer-advance lexer) digits)
                (setf number-type :float))
              (values))
             
             (read-optional-exponent-part ()
              "Determines whether the current LEXER character
               constitutes the exponent denotation \"e\" or \"E\", on
               confirmation writing the same, as well as a potentially
               signed sequence of one or more decimal digits, to the
               DIGITS stream while setting the NUMBER-TYPE to
               ``:float'', otherwise exerting no effect, and in any case
               returning no value."
              (when (and character (char-equal character #\e))
                (write-char (lexer-advance lexer) digits)
                (read-optional-sign)
                (read-digits)
                (setf number-type :float))
              (values)))
          
          (read-optional-sign)
          (read-digits)
          (read-optional-decimal-separator)
          (when (eq number-type :float)
            (read-digits))
          (read-optional-exponent-part)
          
          (the (values string keyword)
            (values
              (get-output-stream-string digits)
              number-type)))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Commencing at the current position into the LEXER's source reads an
   integer or floating-point number and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (multiple-value-bind (digits number-type)
      (lexer-extract-number lexer)
    (declare (type string  digits))
    (declare (type keyword number-type))
    (the Token
      (case number-type
        (:integer
          (make-token :number
            (parse-integer digits)))
        (:float
          (make-token :number
            (read-from-string digits)))
        (otherwise
          (error "Invalid number type: ~s." number-type))))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Commencing at the current position into the LEXER's source, skips a
   sequence of zero or more adjacent whitespaces and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Returns a new token with the LEXER's current character as its value,
   associated with the TOKEN-TYPE as its type, while concomitantly
   advancing the LEXER to the next position in its source."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (with-lexer (lexer)
    (the Token
      (prog1
        (make-token token-type character)
        (lexer-advance lexer)))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Commencing at the current position into the LEXER's source, reads an
   identifier name and returns the associated token."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier-token
      (with-lexer (lexer)
        (with-output-to-string (identifier)
          (declare (type string-stream))
          (loop
            while (and character (identifier-character-p character))
            do    (write-char (lexer-advance lexer) identifier)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, every request towards the LEXER will be
   acquitted with a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (cond
      ((null character)
        (make-token :eof NIL))
      
      ((whitespace-character-p character)
        (lexer-skip-whitespaces lexer)
        (lexer-get-next-token   lexer))
      
      ((digit-char-p character)
        (lexer-read-number lexer))
      
      ((char= character #\+)
        (lexer-read-symbol lexer :plus))
      
      ((char= character #\-)
        (lexer-read-symbol lexer :minus))
      
      ((char= character #\*)
        (lexer-read-symbol lexer :times))
      
      ((char= character #\/)
        (lexer-read-symbol lexer :divide))
      
      ((char= character #\%)
        (lexer-read-symbol lexer :remainder))
      
      ((char= character #\^)
        (lexer-read-symbol lexer :power))
      
      ((char= character #\!)
        (lexer-read-symbol lexer :ecphoneme))
      
      ((char= character #\()
        (lexer-read-symbol lexer :left-parenthesis))
      
      ((char= character #\))
        (lexer-read-symbol lexer :right-parenthesis))
      
      ((char= character #\,)
        (lexer-read-symbol lexer :comma))
      
      ((alpha-char-p character)
        (lexer-read-identifier lexer))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          character position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Stream ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing token stream lexer.")
    :type          Lexer
    :documentation "The lexer responsible for the token generation.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently obtained token from the LEXER."))
  (:documentation
    "The ``Token-Stream'' class enables the purveyance of tokens,
     including their consumption and peeking, in an abstract fashion,
     based upon a lexer's provisions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((token-stream Token-Stream) &key)
  (declare (type Token-Stream token-stream))
  (setf (slot-value token-stream 'current-token)
    (lexer-get-next-token
      (slot-value token-stream 'lexer)))
  (the Token-Stream token-stream))

;;; -------------------------------------------------------

(defun make-token-stream (lexer)
  "Creates and returns a new ``Token-Stream'' which obtains its tokens
   from the LEXER."
  (declare (type Lexer lexer))
  (the Token-Stream
    (make-instance 'Token-Stream :lexer lexer)))

;;; -------------------------------------------------------

(defun token-stream-peek (token-stream)
  "Peeks without consuming the next token from the TOKEN-STREAM."
  (declare (type Token-Stream token-stream))
  (the Token
    (slot-value token-stream 'current-token)))

;;; -------------------------------------------------------

(defun token-stream-consume (token-stream)
  "Removes and returns the next token from the TOKEN-STREAM."
  (declare (type Token-Stream token-stream))
  (the Token
    (prog1
      (slot-value token-stream 'current-token)
      (setf (slot-value token-stream 'current-token)
        (lexer-get-next-token
          (slot-value token-stream 'lexer))))))

;;; -------------------------------------------------------

(defun token-stream-expect (token-stream expected-token-type)
  "Determines whether the next token in the TOKEN-STREAM conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation removing and returning this
   token, otherwise signaling an error of an unspecified type."
  (declare (type Token-Stream token-stream))
  (declare (type keyword      expected-token-type))
  (let ((current-token (token-stream-consume token-stream)))
    (declare (type Token current-token))
    (the Token
      (if (token-type-p current-token expected-token-type)
        current-token
        (error "Expected a token of the type ~s, but encountered the ~
                token ~s."
          expected-token-type current-token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Expression".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Expression ()
  ((type
    :initarg       :type
    :initform      (error "Missing expression type.")
    :reader        expression-type
    :type          keyword
    :documentation "The type of the arithmetic expression or the
                    metacommand.")
   (parameters
    :initarg       :parameters
    :initform      NIL
    :reader        expression-parameters
    :type          parameter-map
    :documentation "A property list of zero or more attributes acting as
                    a vehicle to further delineate the expression."))
  (:documentation
    "The ``Expression'' class encapsulates the information appertaining
     to an evaluable object detected during the parsing of an input.
     ---
     An expression's delineation proceeds from two component: (1) a
     categorizing type and (2) a list of zero or more parameters
     requisite to its contingent evaluation."))

;;; -------------------------------------------------------

(defun make-expression (type &rest parameters)
  "Creates and returns a new ``Expression'' of the specified TYPE,
   optionally initialized with the PARAMETERS, conveyed as a property
   list (plist) of keywords indicators and arbitrary associated values."
  (declare (type keyword       type))
  (declare (type parameter-map parameters))
  (the Expression
    (make-instance 'Expression :type type :parameters parameters)))

;;; -------------------------------------------------------

(defun expression-parameter (expression parameter-name)
  "Returns the value of the parameter associated with the PARAMETER-NAME
   in the EXPRESSION, or ``NIL'' upon a disrespondency."
  (declare (type Expression expression))
  (declare (type keyword    parameter-name))
  (the T (getf (expression-parameters expression) parameter-name)))

;;; -------------------------------------------------------

(defmethod print-object ((expression Expression) stream)
  (declare (type Expression  expression))
  (declare (type destination stream))
  (format stream "(Expression ~s ~s)"
    (slot-value expression 'type)
    (slot-value expression 'parameters)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Stream integer) Expression)
                parse-expression))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular parselet operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-effective-binding-power (binding-power associativity)
  "Returns the effective binding power, that is, the BINDING-POWER with
   respective to the ASSOCIATIVITY."
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (the integer
    (case associativity
      (:non-associative   binding-power)
      (:left-associative  binding-power)
      (:right-associative (1- binding-power))
      (otherwise          (error "Invalid associativity: ~s."
                            associativity)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Parselet".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ()
  (:documentation
    "The ``Parselet'' interface represents the abstract concept of
     parselets, eloigned from the specific manifestations in the initial
     (nud) and consequent (led) variations."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Initial-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Initial-Parselet (Parselet)
  ()
  (:documentation
    "The ``Initial-Parselet'' interface describes an initial or nud
     parselet, an operational unit capacitated to parse an initial or
     nud token in order to produce a suitable abstract syntax tree (AST)
     node representation."))

;;; -------------------------------------------------------

(defgeneric initial-parselet-parse (parselet tokens initial-token)
  (:documentation
    "Applies the PARSELET to the INITIAL-TOKEN's conversion into a
     ``Node'' to return, permitting access to the subsequent tokens by
     mediation of the token stream TOKENS."))

;;; -------------------------------------------------------

(defgeneric initial-parselet-binding-power (parselet)
  (:documentation
    "Returns the PARSELET's binding power.
     ---
     If none is defined, an error of an unspecified type is issued."))

;;; -------------------------------------------------------

(defgeneric initial-parselet-associativity (parselet)
  (:documentation
    "Returns the initial PARSELET's associativity."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Consequent-Parselet".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Consequent-Parselet (Parselet)
  ()
  (:documentation
    "The ``Consequent-Parselet'' interface extends the more abstract
     ``Parselet'' interface to describe a consequent or led parselet,
     an operational unit capacitated to parse a consequent or led token
     in order to produce a suitable abstract syntax tree (AST) node
     representation."))

;;; -------------------------------------------------------

(defgeneric consequent-parselet-parse (parselet tokens left-node
                                       consequent-token)
  (:documentation
    "Utilizing the PARSELET, parses the CONSEQUENT-TOKEN, with the
     LEFT-NODE provided as the left-hand side expression, and the token
     stream TOKENS for contingent access to the subsequent tokens, and
     returns a ``Node'' representation of the thus generated
     expression."))

;;; -------------------------------------------------------

(defgeneric consequent-parselet-binding-power (parselet)
  (:documentation
    "Returns the binding power assigned to the consequent PARSELET."))

;;; -------------------------------------------------------

(defgeneric consequent-parselet-associativity (parselet)
  (:documentation
    "Returns the consequent PARSELET's associativity."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Literal-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Literal-Parselet (Initial-Parselet)
  ((expression-type
    :initarg       :expression-type
    :initform      (error "Missing expression type.")
    :type          keyword
    :documentation "The type of ``Expressin'' which shall be produced by
                    parsing an initial (nud) token utilizing this
                    parselet."))
  (:documentation
    "The ``Literal-Parselet'' class represents an initial parselet
     dedicated to the parsing of a literal number."))

;;; -------------------------------------------------------

(defmethod initial-parselet-parse ((parselet      Literal-Parselet)
                                   (tokens        Token-Stream)
                                   (initial-token Token))
  (declare (type Literal-Parselet parselet))
  (declare (type Token-Stream     tokens))
  (declare (ignore                tokens))
  (declare (type Token            initial-token))
  (the Expression
    (make-expression
      (slot-value parselet 'expression-type)
      :value (token-value initial-token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Unary-Parselet".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Unary-Parselet (Initial-Parselet)
  ((expression-type
    :initarg       :expression-type
    :initform      (error "Missing expression type.")
    :type          keyword
    :documentation "The type of ``Expressin'' which shall be produced by
                    parsing an initial (nud) token utilizing this
                    parselet.")
   (operator
    :initarg       :operator
    :initform      (error "Missiong unary operator.")
    :type          keyword
    :documentation "The prefix operator's identifier.")
   (binding-power
    :initarg       :binding-power
    :initform      0
    :reader        initial-parselet-binding-power
    :type          integer
    :documentation "The unary operator's binding power.")
   (associativity
    :initarg       :associativity
    :initform      :non-associative
    :reader        initial-parselet-associativity
    :type          associativity
    :documentation "Determines whether the unary operator's
                    associativity."))
  (:documentation
    "The ``Unary-Parselet'' represents an initial parselet which parses
     a unary prefix operation, compact of an operator succeeded by its
     sole operand."))

;;; -------------------------------------------------------

(defmethod initial-parselet-parse ((parselet      Unary-Parselet)
                                   (tokens        Token-Stream)
                                   (initial-token Token))
  (declare (type Unary-Parselet parselet))
  (declare (type Token-Stream   tokens))
  (declare (type Token          initial-token))
  (the Expression
    (make-expression (slot-value parselet 'expression-type)
      :operator (slot-value parselet 'operator)
      :operand  (parse-expression tokens
                  (get-effective-binding-power
                    (initial-parselet-binding-power parselet)
                    (initial-parselet-associativity parselet))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Function-Call-Parselet".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Function-Call-Parselet (Initial-Parselet)
  ((function-name
    :initarg       :function-name
    :initform      (error "Missing function name.")
    :type          keyword
    :documentation "The identifier keyword uniquely denoting the
                    represented function.")
   (binding-power
    :initarg       :binding-power
    :initform      0
    :type          integer
    :documentation "The initial binding power."))
  (:documentation
    "The ``Function-Call-Parselet'' represents an initial parselet
     dedicated to the capture of a function invocation, specified by an
     identifying name and a list of zero or more arguments, ensconced in
     a jumelle of parentheses."))

;;; -------------------------------------------------------

(defun parse-parameter-list (tokens)
  "By consuming from the token stream TOKENS, parses and returns a list
   of zero or more arguments ensconced in parentheses."
  (declare (type Token-Stream tokens))
  
  (token-stream-expect tokens :left-parenthesis)
  
  (let ((parameters NIL))
    (declare (type (list-of Expression) parameters))
    
    (unless (token-type-p (token-stream-peek tokens) :right-parenthesis)
      (loop do
        (push (parse-expression tokens 0) parameters)
        
        (case (token-type (token-stream-peek tokens))
          (:comma
            (token-stream-consume tokens))
          (:right-parenthesis
            (loop-finish))
          (otherwise
            (error "Invalid parameter list with token ~s."
              (token-stream-peek tokens))))))
    
    (token-stream-expect tokens :right-parenthesis)
    
    (the (list-of Expression)
      (nreverse parameters))))

;;; -------------------------------------------------------

(defmethod initial-parselet-parse
    ((parselet            Function-Call-Parselet)
     (tokens              Token-Stream)
     (function-name-token Token))
  (declare (type Function-Call-Parselet parselet))
  (declare (type Token-Stream           tokens))
  (declare (type Token                  function-name-token))
  (declare (ignore                      function-name-token))
  (the Expression
    (make-expression :function
      :name       (slot-value parselet 'function-name)
      :parameters (parse-parameter-list tokens))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Group-Parselet".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Group-Parselet (Initial-Parselet)
  ()
  (:documentation
    "The ``Group-Parselet'' class represents an initial parselet
     dedicated to the capture of a parenthesized expression group."))

;;; -------------------------------------------------------

(defmethod initial-parselet-parse
    ((parselet               Group-Parselet)
     (tokens                 Token-Stream)
     (left-parenthesis-token Token))
  (declare (type Group-Parselet parselet))
  (declare (ignore              parselet))
  (declare (type Token-Stream   tokens))
  (declare (type Token          left-parenthesis-token))
  (declare (ignore              left-parenthesis-token))
  (the Expression
    (prog1
      (make-expression :group :term (parse-expression tokens 0))
      (token-stream-expect tokens :right-parenthesis))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Binary-Parselet".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-Parselet (Consequent-Parselet)
  ((expression-type
    :initarg       :expression-type
    :initform      (error "Missing expression type.")
    :type          keyword
    :documentation "The type of ``Expressin'' which shall be produced by
                    parsing a consequent (led) token utilizing this
                    parselet.")
   (operator
    :initarg       :operator
    :initform      (error "Missing operator.")
    :type          keyword
    :documentation "The binary operator's identifier.")
   (binding-power
    :initarg       :binding-power
    :initform      0
    :reader        consequent-parselet-binding-power
    :type          integer
    :documentation "The binary operator's binding power.")
   (associativity
    :initarg       :associativity
    :initform      :associativity
    :reader        consequent-parselet-associativity
    :type          associativity
    :documentation "Determines whether the binary operator's
                    associativity."))
  (:documentation
    "The ``Binary-Parselet'' represents a consequent parselet which
     produces a binary operation expression, compact of an operator and
     its left and right operand twain."))

;;; -------------------------------------------------------

(defmethod consequent-parselet-parse
    ((parselet         Consequent-Parselet)
     (tokens           Token-Stream)
     (left-expression  Expression)
     (consequent-token Token))
  (declare (type Consequent-Parselet parselet))
  (declare (type Token-Stream        tokens))
  (declare (type Expression          left-expression))
  (declare (type Token               consequent-token))
  (the Expression
    (make-expression (slot-value parselet 'expression-type)
      :operator      (slot-value parselet 'operator)
      :left-operand  left-expression
      :right-operand
        (parse-expression tokens
          (get-effective-binding-power
            (consequent-parselet-binding-power parselet)
            (consequent-parselet-associativity parselet))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Postfix-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Postfix-Parselet (Consequent-Parselet)
  ((expression-type
    :initarg       :expression-type
    :initform      (error "Missing expression type.")
    :type          keyword
    :documentation "The type of ``Expressin'' which shall be produced by
                    parsing a consequent (led) token utilizing this
                    parselet.")
   (operator
    :initarg       :operator
    :initform      (error "Missing operator.")
    :type          keyword
    :documentation "The postfix operator identifier.")
   (binding-power
    :initarg       :binding-power
    :initform      0
    :reader        consequent-parselet-binding-power
    :type          integer
    :documentation "The postfix operator's binding power."))
  (:documentation
    "The ``Postfix-Parselet'' represents a consequent parselet which
     produces a unary operation expression, compact of an operator and
     a left operand, but destitute of a dextral input."))

;;; -------------------------------------------------------

(defmethod consequent-parselet-parse ((parselet        Postfix-Parselet)
                                      (tokens          Token-Stream)
                                      (left-expression Expression)
                                      (postfix-token   Token))
  (declare (type Postfix-Parselet parselet))
  (declare (type Token-Stream     tokens))
  (declare (type Expression       left-expression))
  (declare (type Token            postfix-token))
  (the Expression
    (make-expression (slot-value parselet 'expression-type)
      :operator (slot-value parselet 'operator)
      :operand  left-expression)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet registry.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Initial-Parselet)
               +INITIAL-PARSELETS+))

(declaim (type (hash-table-of keyword Consequent-Parselet)
               +CONSEQUENT-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +INITIAL-PARSELETS+
  (make-hash-table :test #'eql)
  "Associates the initial token types with the respective parselets.")

(defparameter +CONSEQUENT-PARSELETS+
  (make-hash-table :test #'eql)
  "Associates the consequent token types with the respective
   parselets.")

;;; -------------------------------------------------------

(defun register-initial-parselet (token-type parselet)
  "Associates the TOKEN-TYPE with the initial PARSELET in the
   +INITIAL-PARSELETS+ table and returns no value."
  (declare (type keyword          token-type))
  (declare (type Initial-Parselet parselet))
  (setf (gethash token-type +INITIAL-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defun register-consequent-parselet (token-type parselet)
  "Associates the TOKEN-TYPE with the consequent PARSELET in the
   +CONSEQUENT-PARSELETS+ table and returns no value."
  (declare (type keyword             token-type))
  (declare (type Consequent-Parselet parselet))
  (setf (gethash token-type +CONSEQUENT-PARSELETS+) parselet)
  (values))

;;; -------------------------------------------------------

(defun initial-token-p (token)
  "Determines whether the TOKEN represents an initial token, which would
   render it eligible for retrieving its affiliated initial parselet,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +INITIAL-PARSELETS+))))))

;;; -------------------------------------------------------

(defun get-initial-parselet (token)
  "Returns the initial parselet associated with the TOKEN, or signals an
   error of an unspecified type if no such correlation exists."
  (declare (type Token token))
  (the Initial-Parselet
    (or (nth-value 0
          (gethash (token-type token) +INITIAL-PARSELETS+))
        (error "No initial token: ~s." token))))

;;; -------------------------------------------------------

(defun consequent-token-p (token)
  "Determines whether the TOKEN represents a consequent token, which
   would render it eligible for retrieving its affiliated consequent
   parselet, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +CONSEQUENT-PARSELETS+))))))

;;; -------------------------------------------------------

(defun get-consequent-parselet (token)
  "Returns the consequent parselet associated with the TOKEN, or signals
   an error of an unspecified type if no such correlation exists."
  (declare (type Token token))
  (the Consequent-Parselet
    (or (nth-value 0
          (gethash (token-type token) +CONSEQUENT-PARSELETS+))
        (error "No consequent token: ~s." token))))

;;; -------------------------------------------------------

(defun get-consequent-binding-power (token)
  "Returns the binding power associated with the TOKEN in the role of a
   consequent token, or signals an error of an unspecified type upon a
   disrespondency."
  (declare (type Token token))
  (multiple-value-bind (parselet consequent-token-p)
      (gethash (token-type token) +CONSEQUENT-PARSELETS+)
    (declare (type (or null Consequent-Parselet) parselet))
    (declare (type T                             consequent-token-p))
    (the integer
      (if consequent-token-p
        (consequent-parselet-binding-power parselet)
        (error "No consequent parselet: ~s." token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Number literal.
(register-initial-parselet :number
  (make-instance 'Literal-Parselet
    :expression-type :literal-number))

;;; -------------------------------------------------------

;; Memory variable.
(register-initial-parselet :identifier
  (make-instance 'Literal-Parselet
    :expression-type :identifier))

;;; -------------------------------------------------------

;; Positive signum.
(register-initial-parselet :plus
  (make-instance 'Unary-Parselet
    :expression-type :unary
    :operator        :plus
    :binding-power   70
    :associativity   :left-associative))

;;; -------------------------------------------------------

;; Negative signum.
(register-initial-parselet :minus
  (make-instance 'Unary-Parselet
    :expression-type :unary
    :operator        :minus
    :binding-power   70
    :associativity   :left-associative))

;;; -------------------------------------------------------

;; Register the procedures as initial parselets of the class
;; "Function-Call-Parselet".
(loop
  for procedure-identifier
    of-type keyword
    being   the hash-keys in +PROCEDURES+
  do
    (register-initial-parselet procedure-identifier
      (make-instance 'Function-Call-Parselet
        :function-name procedure-identifier
        :binding-power 0)))

;;; -------------------------------------------------------

;; Parenthesized group.
(register-initial-parselet :left-parenthesis
  (make-instance 'Group-Parselet))

;;; -------------------------------------------------------

(register-consequent-parselet :plus
  (make-instance 'Binary-Parselet
    :expression-type :binary
    :operator        :plus
    :binding-power   20
    :associativity   :left-associative))

;;; -------------------------------------------------------

(register-consequent-parselet :minus
  (make-instance 'Binary-Parselet
    :expression-type :binary
    :operator        :minus
    :binding-power   20
    :associativity   :left-associative))

;;; -------------------------------------------------------

(register-consequent-parselet :times
  (make-instance 'Binary-Parselet
    :expression-type :binary
    :operator        :times
    :binding-power   30
    :associativity   :left-associative))

;;; -------------------------------------------------------

(register-consequent-parselet :divide
  (make-instance 'Binary-Parselet
    :expression-type :binary
    :operator        :divide
    :binding-power   30
    :associativity   :left-associative))

;;; -------------------------------------------------------

(register-consequent-parselet :power
  (make-instance 'Binary-Parselet
    :expression-type :binary
    :operator        :power
    :binding-power   40
    :associativity   :right-associative))

;;; -------------------------------------------------------

;; Factorial (n!).
(register-consequent-parselet :ecphoneme
  (make-instance 'Postfix-Parselet
    :expression-type :unary
    :operator        :factorial
    :binding-power   60))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "parse-expression" operation.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (tokens current-binding-power)
  "Parses an expression, assembled from the token stream TOKENS, the
   calling entity responsible for the request being endowed with the
   CURRENT-BINDING-POWER, and returns an ``Expression'' representation
   of the result."
  (declare (type Token-Stream tokens))
  (declare (type integer      current-binding-power))
  
  (let ((initial-token   NIL)
        (left-expression NIL))
    (declare (type (or null Token)      initial-token))
    (declare (type (or null Expression) left-expression))
    
    (setf initial-token (token-stream-consume tokens))
    
    (unless (initial-token-p initial-token)
      (error "Not an initial token: ~s." initial-token))
    
    (setf left-expression
      (initial-parselet-parse
        (get-initial-parselet initial-token)
        tokens initial-token))
    
    (loop do
      (let ((next-token (token-stream-peek tokens)))
        (declare (type Token next-token))
        
        (cond
          ;; No further tokens?
          ((token-type-p next-token :eof)
            (loop-finish))
          
          ;; No consequent token (no infix or postfix operator)?
          ((not (consequent-token-p next-token))
            (loop-finish))
          
          ;; Consequent token, but too weak?
          ((<= (get-consequent-binding-power next-token)
               current-binding-power)
            (loop-finish))
          
          ;; Consequent token, and sufficiently strong?
          (T
            (token-stream-consume tokens)
            (setf left-expression
              (consequent-parselet-parse
                (get-consequent-parselet next-token)
                tokens left-expression next-token))))))
    
    (the Expression left-expression)))

;;; -------------------------------------------------------

(defun parse-input (tokens)
  "Parses the token stream of TOKENS, interpreting these either as a
   metacommand or an arithmetic expression, and returns a suitable
   ``Expression'' representation."
  (declare (type Token-Stream tokens))
  (let ((current-token (token-stream-peek tokens)))
    (declare (type Token current-token))
    (the Expression
      (case (token-type current-token)
        ;; Exhausted?
        (:eof
          (make-expression :no-operation))
        ;; Metacommand?
        ((:exit :help :memory)
          (token-stream-consume tokens)
          (make-expression :metacommand
            :command (token-type current-token)))
        ;; Arithmetic expression?
        (otherwise
          (make-expression :calculation
            :input (parse-expression tokens 0)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of printer operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-header (text width)
  "Prints to the standard output the text, centered in a body area of
   WIDTH columns length, and returns no value."
  (declare (type string        text))
  (declare (type (integer 0 *) width))
  (format T "~&** ~v,,,' :@<~a~> **" width text)
  (values))

;;; -------------------------------------------------------

(defun print-line (text width)
  "Prints to the standard output the text, left-aligned in a body area
   of WIDTH columns length, and returns no value."
  (declare (type string        text))
  (declare (type (integer 0 *) width))
  (format T "~&** ~v,,,' @<~a~> **" width text)
  (values))

;;; -------------------------------------------------------

(defun print-empty-line (width)
  "Prints to the standard output an empty line, occupying a body area of
   WIDTH columns length, and returns no value."
  (declare (type (integer 0 *) width))
  (format T "~&** ~v,,,' <~> **" width)
  (values))

;;; -------------------------------------------------------

(defun print-separator (separator width)
  "Prints a sequence composed of replications of the SEPARATOR,
   occupying a body area of WIDTH columns length, and returns no value."
  (declare (type character     separator))
  (declare (type (integer 0 *) width))
  (format T "~&** ~v,,,v<~> **" width separator)
  (values))

;;; -------------------------------------------------------

(defun print-border (width)
  "Prints to the standard output a border composed of asterisks, filling
   a body area of WIDTH columns length, and returns no value."
  (declare (type (integer 0 *) width))
  (format T "~&***~v,,,'*<~>***" width)
  (values))

;;; -------------------------------------------------------

(defun print-margin ()
  "Prints a single linebreak in order to represent a margin and returns
   no value."
  (terpri)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Calculator".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Calculator ()
  ((version
    :initarg       :version
    :initform      "1.0"
    :allocation    :class
    :type          string
    :documentation "This calculator implementation's version.")
   (running-p
    :initarg       :running-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the calculator is still being
                    executed.")
   (context
    :initarg       :context
    :initform      (make-instance 'Context)
    :type          Context
    :documentation "Contains the program memory, in particular useful
                    for dependent procedure invocations."))
  (:documentation
    "The ``Calculator'' class is responsible for the administration of
     effect unto an abstract syntax tree (AST) of expressions."))

;;; -------------------------------------------------------

(defun make-calculator ()
  "Creates and returns a new ``Calculator''."
  (the Calculator
    (make-instance 'Calculator)))

;;; -------------------------------------------------------

(defun calculator-print-intro (calculator)
  "Prints to the standard output an introduction to the CALCULATOR and
   returns no value."
  (declare (type Calculator calculator))
  (print-border     69)
  (print-header     (format NIL "TABLEBASE CALCULATOR v~a --- WELCOME!"
                      (slot-value calculator 'version))
                    69)
  (print-separator  #\- 69)
  (print-empty-line 69)
  (print-line       "Please enter 'help' for further information." 69)
  (print-line       "Using 'memory' you can inspect the memory."   69)
  (print-line       "Issue 'exit' to terminate the program."       69)
  (print-line       (format NIL "To calculate an expression, type it ~
                                 and confirm with 'Enter'.")
                    69)
  (print-empty-line 69)
  (print-line       "Do not forget to enjoy yourself." 69)
  (print-empty-line 69)
  (print-border     69)
  (print-margin)
  (print-margin)
  (values))

;;; -------------------------------------------------------

(defun print-procedures ()
  "Prints to the standard output the available procedure name and
   returns no value."
  (format T "~&The following built-in functions are available: ")
  (loop
    for procedure
      of-type Procedure
      being the hash-values in +PROCEDURES+
    do
      (format T "~&~2t")
      (procedure-print-description procedure))
  (values))

;;; -------------------------------------------------------

(defun calculator-print-help-message (calculator)
  (declare (type Calculator calculator))
  (format T "~&This is the TableBase Calculator version v~a."
    (slot-value calculator 'version))
  (format T "~2%")
  (format T "~&The calculator accepts several metacommands ~
               and arithmetic expressions: ")
  (format T "~&~2t'help':   Prints this help message.")
  (format T "~&~2t'memory': Prints the memory content.")
  (format T "~&~2t'exit':   Terminates the calculator.")
  (format T "~&~2tMathematical expressions are evaluated as expected.")
  (format T "~2%")
  (print-procedures)
  (values))

;;; -------------------------------------------------------

(defun calculator-print-memory (calculator)
  "Prints the CALCULATOR's memory content to the standard output and
   returns no value."
  (declare (type Calculator calculator))
  (let ((memory
          (context-get-memory
            (slot-value calculator 'context))))
    (declare (type memory memory))
    (cond
      ((zerop (hash-table-count memory))
        (format T "The memory is empty."))
      (T
        (format T "~&Memory:")
        (maphash
          #'(lambda (name value)
              (declare (type T      name))
              (declare (type number value))
              (format T " [~a = ~a]" name value)
              (values))
          memory))))
  (values))

;;; -------------------------------------------------------

(defun calculator-store-last-result (calculator result)
  "Determines whether the RESULT is eligible for memorization by the
   CALCULATOR and, in confirmed, stores the same in the CALCULATOR's
   memory as the most recent result, otherwise exercises no effect, in
   any case returning the RESULT."
  (declare (type Calculator calculator))
  (declare (type T          result))
  (when (and result (numberp result))
    (context-store-result
      (slot-value calculator 'context) result))
  (the T result))

;;; -------------------------------------------------------

(defgeneric calculator-dispatch (calculator
                                 expression-type
                                 expression)
  (:documentation
    "Processes the EXPRESSION using the CALCULATOR, dispatching on the
     EXPRESSION-TYPE derived from the EXPRESSION itself, and returns a
     value appropriate for the EXPRESSION."))

;;; -------------------------------------------------------

(defun calculator-evaluate (calculator expression)
  "Evaluates the EXPRESSION using the CALCULATOR, returning a value
   appropriate for the EXPRESSION type.
   ---
   This operation functions as a facade for dispatching to the eligible
   ``calculator-dispatch'' generic function implementation."
  (declare (type Calculator calculator))
  (declare (type Expression  expression))
  (the T
    (calculator-dispatch calculator
      (expression-type expression)
      expression)))

;;; -------------------------------------------------------

(defmacro define-expression-dispatch
    (expression-type (calculator-variable expression-variable)
     &body body)
  "Defines an implementation of the generic function
   ``calculator-dispatch'' with the first argument named by the
   CALCULATOR-VARIABLE, the second designated by an automatically
   generated symbol and serving as the chief dispatching criterion by
   its juxtaposition with the EXPRESSION-TYPE, the third argument being
   nevened by the EXPRESSION-VARIABLE, and containing the BODY forms."
  (let ((expression-type-variable (gensym)))
    (declare (type symbol expression-type-variable))
    `(defmethod calculator-dispatch
         ((,calculator-variable      Calculator)
          (,expression-type-variable (eql ,expression-type))
          (,expression-variable      Expression))
       (declare (type Calculator ,calculator-variable))
       (declare (ignorable       ,calculator-variable))
       (declare (type keyword    ,expression-type-variable))
       (declare (ignore          ,expression-type-variable))
       (declare (type Expression ,expression-variable))
       (declare (ignorable       ,expression-variable))
       ,@body)))

;;; -------------------------------------------------------

(define-expression-dispatch :no-operation (calculator expression)
  (values))

;;; -------------------------------------------------------

(define-expression-dispatch :metacommand (calculator expression)
  (let ((command (expression-parameter expression :command)))
    (declare (type keyword command))
    (case command
      (:exit
        (setf (slot-value calculator 'running-p) NIL))
      (:help
        (calculator-print-help-message calculator))
      (:memory
        (calculator-print-memory calculator))
      (otherwise
        (format T "~&The command \"~a\" is not valid." command))))
  (values))

;;; -------------------------------------------------------

(define-expression-dispatch :calculation (calculator expression)
  (let ((result (calculator-evaluate calculator
                  (expression-parameter expression :input))))
    (declare (type T result))
    (calculator-store-last-result calculator result)
    (typecase result
      ((complex rational)
        (format T "~&~d~@di" (realpart result) (imagpart result)))
      ((complex float)
        (format T "~&~f+~@fi" (realpart result) (imagpart result)))
      (otherwise
        (format T "~&~a" result))))
  (values))

;;; -------------------------------------------------------

(define-expression-dispatch :literal-number (calculator expression)
  (the real
    (expression-parameter expression :value)))

;;; -------------------------------------------------------

(define-expression-dispatch :identifier (calculator expression)
  (the string
    (expression-parameter expression :value)))

;;; -------------------------------------------------------

(define-expression-dispatch :binary (calculator expression)
  (the number
    (funcall
      (case (expression-parameter expression :operator)
        (:plus      #'+)
        (:minus     #'-)
        (:times     #'*)
        (:divide    #'/)
        (:remainder #'rem)
        (:power     #'expt)
        (otherwise
          (error "Unrecognized binary operator in ~s." expression)))
      (calculator-evaluate calculator
        (expression-parameter expression :left-operand))
      (calculator-evaluate calculator
        (expression-parameter expression :right-operand)))))

;;; -------------------------------------------------------

(define-expression-dispatch :unary (calculator expression)
  (the number
    (funcall
      (case (expression-parameter expression :operator)
        (:plus      #'+)
        (:minus     #'-)
        (:factorial #'factorial)
        (otherwise
          (error "Unrecognized unary operator in ~s." expression)))
      (calculator-evaluate calculator
        (expression-parameter expression :operand)))))

;;; -------------------------------------------------------

(define-expression-dispatch :function (calculator expression)
  (the number
    (invoke-function
      (expression-parameter expression :name)
      (slot-value calculator 'context)
      ;; Build the actual parameter list.
      (mapcar
        #'(lambda (parameter)
            (declare (type Expression parameter))
            (the T (calculator-evaluate calculator parameter)))
        (expression-parameter expression :parameters)))))

;;; -------------------------------------------------------

(define-expression-dispatch :group (calculator expression)
  (the number
    (calculator-evaluate calculator
      (expression-parameter expression :term))))

;;; -------------------------------------------------------

(defun show-prompt-message ()
  "Shows the prompt message employed to signify the expectancy of user
   input, and returns no value."
  (format T "~&>> ")
  (values))

;;; -------------------------------------------------------

(defun read-input ()
  "Reads from the standard input a line of user input and returns the
   same as a string."
  (the string
    (prog1
      (read-line)
      (clear-input))))

;;; -------------------------------------------------------

(defun calculator-start (calculator)
  "Starts the CALCULATOR, repeatedly quering the user for an input and
   evaluating the same, following iteration's termination returning no
   value."
  (declare (type Calculator calculator))
  
  (setf (slot-value calculator 'running-p) T)
  (calculator-print-intro calculator)
  
  (loop while (slot-value calculator 'running-p) do
    (show-prompt-message)
    
    (calculator-evaluate calculator
      (parse-input
        (make-token-stream
          (make-lexer
            (read-input))))))
  
  (values))
