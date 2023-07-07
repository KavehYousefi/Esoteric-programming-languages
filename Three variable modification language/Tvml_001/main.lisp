;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Three variable modification language", invented by the
;; Esolang user "Lemonz" and presented in the year 2022, whose concept
;; resolves around the manipulation of three available variables via
;; unary and binary arithmetics, output, and control flow helming.
;; 
;; 
;; Concept
;; =======
;; The Three variable modification language employs a set of three
;; variables, the agnomination of which produces "a", "b", and "c", for
;; integer scalars' storage.
;; 
;; == THE MEMORY: THREE INTEGER-VALUED VARIABLES ==
;; A peculiar exhibition of niggardliness indulges in the language's
;; haecceity, as its memory tallies a mere treble of units, a diorism
;; that appertains to three variables, nevened "a", "b", and "c", each
;; entrusted to hold an unbounded signed integer, and prepared with a
;; dedicated default value:
;; 
;;   ------------------------
;;   Variable | Default value
;;   ---------+--------------
;;   a        |  3
;;   ........................
;;   b        | -2
;;   ........................
;;   c        | -1
;;   ------------------------
;; 
;; 
;; Instructions
;; ============
;; A decimal account of instructions partakes of the language, any of
;; the members being dependent upon one or two parameters that are
;; appended directly and without particular ceremony.
;; 
;; == OVERVIEW ==
;; An apercu shall adhibit a cursory nortelry concerning the
;; instructions' competences.
;; 
;; Please note the following aspects:
;; 
;;   (a) The spaces inserted into the command syntax do not partake of
;;       veridical significance in the actual language, forecause their
;;       utility's bournes do not protrude beyond formatting purposes.
;;   (b) The placeholder segments in the description are marked by aide
;;       of twifold underlining ("="), intended for substitution by
;;       actual data.
;; 
;; Based on these specifications, the following commands are available:
;; 
;;   ------------------------------------------------------------------
;;   Command               | Effect
;;   ----------------------+-------------------------------------------
;;   ! variable            | Negates the {variables}'s value by
;;     ========            | multiplying it by -1.
;;                         |-------------------------------------------
;;                         | The {variable} must be a variable name.
;;   ..................................................................
;;   R variable            | Resets the {variable} to its default
;;     ========            | default.
;;                         |-------------------------------------------
;;                         | The {variable} must be a variable name.
;;   ..................................................................
;;   + left right          | Adds the {right} variable's value to the
;;     ==== =====          | {left} variable's and stores the sum in
;;                         | the {right} variable itself.
;;                         |-------------------------------------------
;;                         | {left} must be a variable name.
;;                         |-------------------------------------------
;;                         | {right} must be a variable name.
;;   ..................................................................
;;   - left right          | Subtracts from the {left} variable's value
;;     ==== =====          | the {right} variable's and stores the
;;                         | difference in the {right} variable itself.
;;                         |-------------------------------------------
;;                         | {left} must be a variable name.
;;                         |-------------------------------------------
;;                         | {right} must be a variable name.
;;   ..................................................................
;;   * left right          | Multiplies the {left} variable's value by
;;     ==== =====          | the {right} variable's and stores the
;;                         | product in the {right} variable itself.
;;                         |-------------------------------------------
;;                         | {left} must be a variable name.
;;                         |-------------------------------------------
;;                         | {right} must be a variable name.
;;   ..................................................................
;;   / left right          | Divides the {left} variable's value by the
;;     ==== =====          | {right} variable's and stores the quotient
;;                         | in the {right} variable itself.
;;                         |-------------------------------------------
;;                         | {left} must be a variable name.
;;                         |-------------------------------------------
;;                         | {right} must be a variable name.
;;   ..................................................................
;;   > variable            | Prints to the standard output the
;;     ========            | {variable}'s value in its verbatim numeric
;;                         | form.
;;                         |-------------------------------------------
;;                         | The {variable} must be a variable name.
;;   ..................................................................
;;   < variable            | Prints to the standard output the ASCII
;;     ========            | character whose character code equals the
;;                         | {variable}'s value.
;;                         |-------------------------------------------
;;                         | The {variable} must be a variable name.
;;                         |-------------------------------------------
;;                         | An error of an unspecified type is
;;                         | signaled if the {variable}'s value fails
;;                         | to answer to a valid ASCII code.
;;   ..................................................................
;;   w[ guard statements ] | Repeats the {statements} while the {guard}
;;      ===== ==========   | variable's value is greater than zero.
;;                         |-------------------------------------------
;;                         | The {guard} must be a variable name.
;;                         |-------------------------------------------
;;                         | The {statements} must be a sequence of
;;                         | zero or more commands.
;;   ..................................................................
;;   i[ guard statements ] | Executes the {statements} once if the
;;      ===== ==========   | {guard} variable's value is less than or
;;                         | equal to zero; aliter accompasses no
;;                         | consequences.
;;                         |-------------------------------------------
;;                         | The {guard} must be a variable name.
;;                         |-------------------------------------------
;;                         | The {statements} must be a sequence of
;;                         | zero or more commands.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-06
;; 
;; Sources:
;;   [esolang2022tvml]
;;   The Esolang contributors, "Three variable modification language",
;;     2022
;;   URL: "https://esolangs.org/wiki/
;;         Three_variable_modification_language"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of package.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :tvml
  (:use
    :cl)
  (:shadow :variable))

;;; -------------------------------------------------------

(in-package :tvml)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional
                   (element-type T)
                   (size         *))
  "The ``list-of'' type defines a list either composed of exactly the
   SIZE number of members, or, if not specified, an arbitrary tally,
   each item of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (eq size '*)
                (= (length (the list candidate))
                   size))
            (loop
              for element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional
                               (indicator-type T)
                               (value-type     T))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of zero or more entries, for each non-``NIL'' member
   of which the indicator, or key, conforms to the INDICATOR-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for element of-type T in (the list candidate)
              always
                (or (null element)
                    (typep element
                      `(cons ,indicator-type ,value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype token-type ()
  "The ``token-type'' type enumerats the recognized variants of token
   categories."
  '(member
    :eof
    :variable
    :plus
    :minus
    :times
    :slash
    :output-number
    :output-character
    :negate
    :reset
    :while
    :if
    :left-bracket
    :right-bracket))

;;; -------------------------------------------------------

(deftype variable ()
  "The ``variable'' type defines a variable identifier as a character
   from the finite set {'a', 'b', 'c'}."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (characterp candidate)
            (find candidate "abc" :test #'char=))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  '(list-of Node *))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized variants of
   unary operators."
  '(member :negate :reset))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerats the recognized variants of
   binary operators."
  '(member :plus :minus :times :slash))

;;; -------------------------------------------------------

(deftype print-mode ()
  "The ``print-mode'' type enumerates the valid formats by which
   variable values can be displayed."
  '(member :numeric :character))

;;; -------------------------------------------------------

(deftype variable-table ()
  "The ``variable-table'' type defines a mapping of variable names to
   their values, realized as an association list that affiliates
   ``variable'' indicators with ``integer'' values."
  '(association-list-of variable integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations,
   entailing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token ()
  ((type
    :initarg       :type
    :initform      (error "Missing token type.")
    :reader        get-token-type
    :type          token-type
    :documentation "The category the token subsumes into.")
   (value
    :initarg       :value
    :initform      (error "Missing token value.")
    :reader        get-token-value
    :documentation "The value carried by this token."))
  (:documentation
    "The ``Token'' class encapsulates the information requisite for
     replicating a significant piece of data begotten during the lexical
     analyzation stage in the \"Three variable modification language\"
     processing."))

;;; -------------------------------------------------------

(defun token-is-of-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token      token))
  (declare (type token-type expected-type))
  (the boolean
    (not (null
      (eq (get-token-type token) expected-type)))))

;;; -------------------------------------------------------

(defmethod print-object ((token Token) (stream T))
  (declare (type Token       token))
  (declare (type destination stream))
  (format stream "(Token type=~s value=~s)"
    (get-token-type  token)
    (get-token-value token)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of character 3) +WHITESPACE-CHARACTERS+))

;;; -------------------------------------------------------

(defparameter +WHITESPACE-CHARACTERS+ '(#\Newline #\Space #\Tab)
  "Lists the recognized whitespace characters.")

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate +WHITESPACE-CHARACTERS+ :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of identifier table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'eql)
  "Correlates the recognized identifier names with a representative
   token.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token-type)
        "Associates the NAME with a new token bearing the TOKEN-TYPE
         in conjunction with the NAME as its value, and returns no
         value."
        (declare (type character  name))
        (declare (type token-type token-type))
        (setf (gethash name +IDENTIFIERS+)
              (make-instance 'Token :type token-type :value name))
        (values)))
  ;; Variables.
  (register-identifier #\a :variable)
  (register-identifier #\b :variable)
  (register-identifier #\c :variable)
  ;; Binary operations.
  (register-identifier #\+ :plus)
  (register-identifier #\- :minus)
  (register-identifier #\* :times)
  (register-identifier #\/ :slash)
  ;; Output.
  (register-identifier #\> :output-number)
  (register-identifier #\< :output-character)
  ;; Unary operations.
  (register-identifier #\! :negate)
  (register-identifier #\R :reset)
  ;; Conditionals.
  (register-identifier #\w :while)
  (register-identifier #\i :if)
  (register-identifier #\[ :left-bracket)
  (register-identifier #\] :right-bracket))
(values)

;;; -------------------------------------------------------

(defun get-identifier-token (name)
  "Returns the token affiliated with the NAME, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type character name))
  (the Token
    (or (gethash name +IDENTIFIERS+)
        (error "Unrecognized identifier name: ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :reader        get-lexer-source
    :type          string
    :documentation "The piece of \"Three variable modification
                    language\" source code to analyze.")
   (position
    :initform      0
    :accessor      current-position
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :accessor      current-character
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class' wike encompasses the lexical analyzation of a
     piece of \"Three variable modification language\" source code in
     order to glean its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Locates the LEXER's position cursor to the first character in its
   SOURCE, if the same is not empty, and returns no value."
  (declare (type Lexer lexer))
  (setf (current-character lexer)
    (when (array-in-bounds-p (get-lexer-source lexer)
            (current-position lexer))
      (char (get-lexer-source lexer)
        (current-position lexer))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-character (lexer)
  "Returns the LEXER's current character, while concomitantly moving its
   position cursor to the next character in its source, if possible."
  (declare (type Lexer lexer))
  (the (or null character)
    (prog1
      (current-character lexer)
      (setf (current-character lexer)
        (when (array-in-bounds-p (get-lexer-source lexer)
                (1+ (current-position lexer)))
          (char (get-lexer-source lexer)
            (incf (current-position lexer))))))))

;;; -------------------------------------------------------

(defun read-symbol-from-lexer (lexer token-type)
  "Creates and returns a new ``Token'' with the LEXER's current
   character as the value, affiliated with the TOKEN-TYPE, while
   concomitantly advancing the LEXER's positon cursor to the next
   character in its source."
  (declare (type Lexer      lexer))
  (declare (type token-type token-type))
  (the Token
    (make-instance 'Token
      :type  token-type
      :value (advance-to-next-character lexer))))

;;; -------------------------------------------------------

(defun skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent spaces, and returns no value."
  (declare (type Lexer lexer))
  (loop
    while
      (and (current-character lexer)
           (whitespace-character-p (current-character lexer)))
    do
      (advance-to-next-character lexer))
  (values))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source exhaustion, the LEXER responds to any request with a
   fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (case (current-character lexer)
      ((NIL)
        (make-instance 'Token :type :eof :value NIL))
      (#.+WHITESPACE-CHARACTERS+
        (skip-spaces    lexer)
        (get-next-token lexer))
      (otherwise
        (get-identifier-token
          (advance-to-next-character lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ()
  (:documentation
    "The ``Node'' interface instantiates the common foundry for all
     variants of abstract syntax tree (AST) nodes."))

;;; -------------------------------------------------------

(defclass Unary-Operation-Node (Node)
  ((operator
    :initarg       :operator
    :initform      (error "Missing unary operator.")
    :reader        get-operator
    :type          unary-operator
    :documentation "The unary operator to apply to the OPERAND.")
   (operand
    :initarg       :operand
    :initform      (error "Missing unary operand.")
    :reader        get-operand
    :type          variable
    :documentation "The variable committed to the unary OPERATOR."))
  (:documentation
    "The ``Unary-Operation-Node'' class' onus delineates the
     representation of an operation relating to a single variable's
     manipulation."))

;;; -------------------------------------------------------

(defmethod print-object ((node Unary-Operation-node) (stream T))
  (declare (type Unary-Operation-Node node))
  (declare (type destination          stream))
  (format stream "(Unary-Operation-Node ~
                    operator=~s ~
                    operand=~s)"
    (get-operator node)
    (get-operand  node)))

;;; -------------------------------------------------------

(defclass Binary-Operation-Node (Node)
  ((operator
    :initarg       :operator
    :initform      (error "Missing binary operator.")
    :reader        get-operator
    :type          binary-operator
    :documentation "The binary operator to apply to the LEFT-OPERAND and
                    the RIGHT-OPERAND.")
   (left-operand
    :initarg       :left-operand
    :initform      (error "Missing left binary operand.")
    :reader        get-left-operand
    :type          variable
    :documentation "The first variable committed to the binary
                    OPERATOR.")
   (right-operand
    :initarg       :right-operand
    :initform      (error "Missing right binary operand.")
    :reader        get-right-operand
    :type          variable
    :documentation "The second variable committed to the binary
                    OPERATOR."))
  (:documentation
    "The ``Binary-Operation-Node'' class' onus delineates the
     representation of arithmetic operations in the context of two
     variables' engagement."))

;;; -------------------------------------------------------

(defmethod print-object ((node Binary-Operation-node) (stream T))
  (declare (type Binary-Operation-Node node))
  (declare (type destination           stream))
  (format stream "(Binary-Operation-Node ~
                    operator=~s ~
                    left-operand=~s ~
                    right-operand=~s)"
    (get-operator      node)
    (get-left-operand  node)
    (get-right-operand node)))

;;; -------------------------------------------------------

(defclass Print-Node (Node)
  ((argument
    :initarg       :argument
    :initform      (error "Missing print argument.")
    :reader        get-print-argument
    :type          variable
    :documentation "The name of the variable to print.")
   (print-mode
    :initarg       :print-mode
    :initform      (error "Missing print mode.")
    :reader        get-print-mode
    :type          print-mode
    :documentation "Determines whether the VARIABLE shall be printed
                    in its verbatim numeric form or as the corresponding
                    ASCII character."))
  (:documentation
    "The ``Print-Node'' applies itself to the display of a variable's
     value, either, as a dependency upon the print mode, in its numeric
     form, or as an ASCII character issuing from the integer datum's
     correspondence with the respective ASCII code."))

;;; -------------------------------------------------------

(defmethod print-object ((node Print-Node) (stream T))
  (declare (type Print-Node  node))
  (declare (type destination stream))
  (format stream "(Print-Node argument=~s mode=~s)"
    (get-print-argument node)
    (get-print-mode     node)))

;;; -------------------------------------------------------

(defclass Block-Node (Node)
  ((probed-variable
    :initarg       :probed-variable
    :initform      (error "Missing probed variable.")
    :reader        get-probed-variable
    :type          variable
    :documentation "The variable whose satisfaction of a predicate
                    imposes a requistum to the execution or
                    continuation.")
   (statements
    :initarg       :statements
    :initform      (error "Missing body statements.")
    :reader        get-statements
    :type          node-list
    :documentation "The statements to execute upon the PROBED-VARIABLE's
                    eligibility."))
  (:documentation
    "The ``Block-Node'' class encapsulates a conditional statement
     block, composed of a probed variable in the agency of its guard,
     and zero or more statement nodes."))

;;; -------------------------------------------------------

(defmethod print-object ((node Block-Node) (stream T))
  (declare (type Block-Node  node))
  (declare (type destination stream))
  (format stream "(Block-Node probed-variable=~s statements=~s)"
    (get-probed-variable node)
    (get-statements      node)))

;;; -------------------------------------------------------

(defclass While-Node (Node)
  ((condition
    :initarg       :condition
    :initform      (error "Missing while loop condition.")
    :reader        get-condition
    :type          Block-Node
    :documentation "The condition, composed of a variable and a list of
                    body statements, whose satisfaction enables a
                    repeated execution."))
  (:documentation
    "The ``When-Node'' class represents an iterative block whose
     repeated execution is ascertained as long as its probed variable is
     greater than zero."))

;;; -------------------------------------------------------

(defmethod print-object ((node While-Node) (stream T))
  (declare (type While-Node  node))
  (declare (type destination stream))
  (format stream "(While-Node condition=~s)"
    (get-condition node)))

;;; -------------------------------------------------------

(defclass If-Node (Node)
  ((condition
    :initarg       :condition
    :initform      (error "Missing if conditional condition.")
    :reader        get-condition
    :type          Block-Node
    :documentation "The condition, composed of a variable and a list of
                    body statements, whose satisfaction enables a single
                    execution."))
  (:documentation
    "The ``If-Node'' class represents a conditional block whose
     execution materializes only if its probed variable comprehends a
     value less than or equal to zero."))

;;; -------------------------------------------------------

(defclass Program-Node (Node)
  ((statements
    :initarg       :statements
    :initform      (error "Missing program statements.")
    :reader        get-statements
    :type          node-list
    :documentation "The top-level statements comprising the program."))
  (:documentation
    "The ``Program-Node'' class implements the root node of a
     \"Three variable modification language\" representation in abstract
     syntax tree (AST) form."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :reader        get-lexer
    :type          Lexer
    :documentation "The source responsible for the purveyance of
                    tokens in the parser's pursuit of assembling the
                    same into an abstract syntax tree (AST).")
   (current-token
    :initform      (make-instance 'Token :type :eof :value NIL)
    :accessor      current-token
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class provides an entity whose capacitation permits
     the assemblage of an abstract syntax tree (AST) from a series of
     tokens."))

;;; -------------------------------------------------------

(declaim (ftype (function (Parser) Node) parse-statement))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Queries the first token from the PARSER's internally managed lexer,
   stores it, and returns no value."
  (declare (type Parser parser))
  (setf (current-token parser)
    (get-next-token
      (get-lexer parser)))
  (values))

;;; -------------------------------------------------------

(defun expect-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly querying and storing the next token from the
   PARSER's internally managed lexer; aliter an error of an unspecified
   type is signaled."
  (declare (type Parser     parser))
  (declare (type token-type expected-token-type))
  (the Token
    (if (token-is-of-type-p (current-token parser) expected-token-type)
      (prog1 (current-token parser)
        (setf (current-token parser)
          (get-next-token
            (get-lexer parser))))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type (current-token parser)))))

;;; -------------------------------------------------------

(defun consume-token (parser)
  "Returns the PARSER's current token, while concomitantly querying and
   storing the next one from the internally managed lexer."
  (declare (type Parser parser))
  (the Token
    (prog1
      (current-token parser)
      (setf (current-token parser)
        (get-next-token
          (get-lexer parser))))))

;;; -------------------------------------------------------

(defun parse-variable (parser)
  "Parses a variable name using the PARSER and returns the same."
  (declare (type Parser parser))
  (the variable
    (get-token-value
      (expect-token parser :variable))))

;;; -------------------------------------------------------

(defun parse-binary-operator (parser)
  "Parses a binary operator using the PARSER and returns the same."
  (declare (type Parser parser))
  (the binary-operator
    (get-token-type
      (consume-token parser))))

;;; -------------------------------------------------------

(defun parse-block (parser)
  "Parses a conditional block, compact of a probed variable and a list
   of zero or more ensconced statements, using the PARSER and returns a
   ``Block-Node'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :left-bracket)
  (let ((probed-variable
          (get-token-value
            (expect-token parser :variable))))
    (declare (type character probed-variable))
    (the Block-Node
      (loop
        for token of-type Token = (current-token parser)
        if (token-is-of-type-p token :eof) do
          (error "Unterminated statement block.")
        else if (token-is-of-type-p token :right-bracket) do
          (consume-token parser)
          (loop-finish)
        else
          collect (parse-statement parser)
          into    statements
        finally
          (return
            (make-instance 'Block-Node
              :probed-variable probed-variable
              :statements      statements))))))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses a single statement using the PARSER and returns a ``Node''
   representation thereof."
  (declare (type Parser parser))
  (the Node
    (case (get-token-type (current-token parser))
      ((:negate :reset)
        (make-instance 'Unary-Operation-Node
          :operator
            (get-token-type
              (consume-token parser))
          :operand
            (parse-variable parser)))
      
      ((:plus :minus :times :slash)
        (make-instance 'Binary-Operation-Node
          :operator
            (get-token-type
              (consume-token parser))
          :left-operand
            (parse-variable parser)
          :right-operand
            (parse-variable parser)))
      
      (:output-number
        (consume-token parser)
        (make-instance 'Print-Node
          :argument   (parse-variable parser)
          :print-mode :numeric))
      
      (:output-character
        (consume-token parser)
        (make-instance 'Print-Node
          :argument   (parse-variable parser)
          :print-mode :character))
      
      (:while
        (consume-token parser)
        (make-instance 'While-Node :condition
          (parse-block parser)))
      
      (:if
        (consume-token parser)
        (make-instance 'If-Node :condition
          (parse-block parser)))
      
      (otherwise
        (error "Unrecognized statement token: ~s."
          (current-token parser))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Assembles and returns from the PARSER's tokens an abstract syntax
   tree (AST) representation of the induced \"Three variable
   modification language\" source code."
  (declare (type Parser parser))
  (the Program-Node
    (make-instance 'Program-Node :statements
      (loop
        until   (token-is-of-type-p (current-token parser) :eof)
        collect (parse-statement parser)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of default variable table.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type variable-table +DEFAULT-VARIABLES+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-VARIABLES+
  '((#\a .  3)
    (#\b . -2)
    (#\c . -1))
  "Maintains the treble of available variables in conjunction with their
   default values.")

;;; -------------------------------------------------------

(defun get-default-variable-value (variable)
  "Returns the VARIABLE's default value."
  (declare (type variable variable))
  (the integer
    (cdr
      (or (assoc variable +DEFAULT-VARIABLES+ :test #'char=)
          (error "Invalid variable name: ~s." variable)))))

;;; -------------------------------------------------------

(defun build-variable-table ()
  "Creates and returns a fresh variable table, containing the default
   values."
  (the variable-table
    (copy-alist +DEFAULT-VARIABLES+)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable map.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Map ()
  ((variables
    :initform      (build-variable-table)
    :reader        get-variables
    :type          variable-table
    :documentation "Associates the variable names with the variable
                    values."))
  (:documentation
    "The ``Variable-Map'' subscribes to the onus of maintaining the
     three available variables, \"a\", \"b\" and \"c\", in conjunction
     with their current values, while concomitantly permitting adit to
     their indagation and modification by their identifiers'
     adminiculum."))

;;; -------------------------------------------------------

(defun get-variable-entry (variable-map variable)
  "Returns a reference to the entry for the VARIABLE in the
   VARIABLE-MAP.
   ---
   From the fact of the return value being a reference ensues that
   modifications to the same immediately propagate into the entry."
  (declare (type Variable-Map variable-map))
  (declare (type variable     variable))
  (the (cons variable integer)
    (or (assoc variable (get-variables variable-map) :test #'char=)
        (error "Invalid variable name: ~s." variable))))

;;; -------------------------------------------------------

(defun variable-value (variable-map variable)
  "Returns the value of the VARIABLE registered at the VARIABLE-MAP."
  (declare (type Variable-Map variable-map))
  (declare (type variable     variable))
  (the integer
    (cdr (get-variable-entry variable-map variable))))

;;; -------------------------------------------------------

(defun (setf variable-value) (new-value variable-map variable)
  "Sets the value of the VARIABLE registered at the VARIABLE-MAP to the
   NEW-VALUE and returns no value."
  (declare (type integer      new-value))
  (declare (type Variable-Map variable-map))
  (declare (type variable     variable))
  (setf (cdr (get-variable-entry variable-map variable)) new-value)
  (values))

;;; -------------------------------------------------------

(defun negate-variable (variable-map variable)
  "Negates the value of the variable, registered at the VARIABLE-MAP,
   stores the new datum into the same, and returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type variable     variable))
  (setf (cdr (get-variable-entry variable-map variable))
        (- (variable-value variable-map variable)))
  (values))

;;; -------------------------------------------------------

(defun reset-variable (variable-map variable)
  "Resets the variable, registered at the VARIABLE-MAP, to its default
   value and returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type variable     variable))
  (setf (cdr (get-variable-entry variable-map variable))
        (get-default-variable-value variable))
  (values))

;;; -------------------------------------------------------

(defun add-variables (variable-map augend addend)
  "Adds the ADDEND to the AUGEND, both variables queried from the
   VARIABLE-MAP, stores the sum back into the ADDEND variable, and
   returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type variable     augend))
  (declare (type variable     addend))
  (setf (variable-value variable-map addend)
        (+ (variable-value variable-map augend)
           (variable-value variable-map addend)))
  (values))

;;; -------------------------------------------------------

(defun subtract-variables (variable-map minuend subtrahend)
  "Subtracts the SUBTRAHEND from the MINUEND, both variables queried
   from the VARIABLE-MAP, stores the difference back into the SUBTRAHEND
   variable, and returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type variable     minuend))
  (declare (type variable     subtrahend))
  (setf (variable-value variable-map subtrahend)
        (- (variable-value variable-map minuend)
           (variable-value variable-map subtrahend)))
  (values))

;;; -------------------------------------------------------

(defun multiply-variables (variable-map multiplicand multiplier)
  "Multiplies the MULTIPLICAND by the MULTIPLIER, both variables queried
   from the VARIABLE-MAP, stores the product back into the MULTIPLIER
   variable, and returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type variable     multiplicand))
  (declare (type variable     multiplier))
  (setf (variable-value variable-map multiplier)
        (* (variable-value variable-map multiplicand)
           (variable-value variable-map multiplier)))
  (values))

;;; -------------------------------------------------------

(defun divide-variables (variable-map dividend divisor)
  "Divides the DIVIDEND by the DIVISOR, both variables queried from the
   VARIABLE-MAP, stores the quotient back into the DIVISOR variable, and
   returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type variable     dividend))
  (declare (type variable     divisor))
  (setf (variable-value variable-map divisor)
        (round (variable-value variable-map dividend)
               (variable-value variable-map divisor)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree.")
    :reader        get-program-tree
    :type          Node
    :documentation "The root node of the abstract syntax tree (AST)
                    representing the parsed program.")
   (variables
    :initarg       :variables
    :initform      (make-instance 'Variable-Map)
    :reader        get-variables
    :type          Variable-Map
    :documentation "Associates the three variables with their current
                    values."))
  (:documentation
    "The ``Interpreter'' class, proceeding from the parser's dation of
     an abstract syntax tree (AST), enjoys the responsibility to
     accompass consequences to the program's thus conceived model."))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Visits the abstract syntax tree (AST) NODE in the INTERPRETER's
     context and returns no value."))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Program-Node))
  "Evaluates the program NODE's statements in the INTERPRETER's context
   and returns no value."
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (dolist (statement (get-statements node))
    (declare (type Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Unary-Operation-Node))
  "Applies the binary operation NODE's operator to its operand in the
   INTERPRETER's context, stores the result in the operand, and returns
   no value."
  (declare (type Interpreter          interpreter))
  (declare (type Unary-Operation-Node node))
  (let ((operator (get-operator node)))
    (declare (type unary-operator operator))
    (funcall
      (case operator
        (:negate   #'negate-variable)
        (:reset    #'reset-variable)
        (otherwise (error "Invalid unary operator: ~s." operator)))
      (get-variables interpreter)
      (get-operand   node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Binary-Operation-Node))
  "Applies the binary operation NODE's operator to its operand twain in
   the INTERPRETER's context, stores the result in the second operand,
   and returns no value."
  (declare (type Interpreter           interpreter))
  (declare (type Binary-Operation-Node node))
  (let ((operator (get-operator node)))
    (declare (type binary-operator operator))
    (funcall
      (case operator
        (:plus     #'add-variables)
        (:minus    #'subtract-variables)
        (:times    #'multiply-variables)
        (:slash    #'divide-variables)
        (otherwise (error "Invalid binary operator: ~s." operator)))
      (get-variables     interpreter)
      (get-left-operand  node)
      (get-right-operand node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Print-Node))
  "Prints the print NODE's argument variable in the INTERPRETER's
   context to the standard output, employing the stipulated form, and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Print-Node  node))
  (case (get-print-mode node)
    (:numeric
      (format T "~d "
        (variable-value
          (get-variables      interpreter)
          (get-print-argument node))))
    (:character
      (format T "~c"
        (code-char
          (variable-value
            (get-variables      interpreter)
            (get-print-argument node)))))
    (otherwise
      (error "Invalid print mode: ~s."
        (get-print-mode node))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Block-Node))
  "Unconditionally executes the block NODE's statements in the
   INTERPRETER's context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Block-Node  node))
  (dolist (statement (get-statements node))
    (declare (type node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        While-Node))
  "Repeatedly executes the while NODE's statements in the INTERPRETER's
   context until its probed variable acquires a value less than or equal
   to zero (0), and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type While-Node  node))
  (let ((condition (get-condition node)))
    (declare (type Block-Node condition))
    (let ((probed-variable (get-probed-variable condition)))
      (declare (type variable probed-variable))
      (loop
        while
          (plusp
            (variable-value
              (get-variables interpreter)
              probed-variable))
        do
          (visit-node interpreter condition))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        If-Node))
  "Executes the if NODE's statements in the INTERPRETER's context if and
   only if its probed variable contains a value less than or equal to
   zero (0), and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type If-Node     node))
  (let ((condition (get-condition node)))
    (declare (type Block-Node condition))
    (unless (plusp
              (variable-value
                (get-variables       interpreter)
                (get-probed-variable condition)))
      (visit-node interpreter condition)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the \"Three variable modification language\" stored as an
   abstract syntax tree (AST) in the INTERPRETER and returns no value."
  (visit-node interpreter
    (get-program-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Three-variable-modification-language (code)
  "Interprets the piece of \"Three variable modification language\"
   source code and returns no value."
  (declare (type string code))
  (interpret-program
    (make-instance 'Interpreter :tree
      (parse-program
        (make-instance 'Parser :lexer
          (make-instance 'Lexer :source code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Three-variable-modification-language
  "*aa*ac*cc-ca<aRa-ba*ba*ba-ca<aRc+bc+bb+bc-ac<c<cRa+ac<c/caRbRc+bc+bb+bc-ac<cRc+bc+ac<cRa*ac*aa-ca<aR
a*ab-bc-ac<c+ca<aRa-ca<aRb+bb+bb+ab<bRbRc*bb*bb+bb-bc<c")

;;; -------------------------------------------------------

;; Counter which tallies from one (1) to infinity.
(interpret-Three-variable-modification-language
  "/bb*cc>cw[c+bc>c]")

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-Three-variable-modification-language
  "*aa-ab*baw[a>a
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb<b<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+bb<b<b
Rc!c-bb+cb+bb+bb+cb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb<b

>a
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb<b<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb<b

Rc!c-bb+cb+bb+bb+cb+bb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb+cb<b<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb<b

Rc+ca>a
  !c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb<b<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+cb+bb+bb+cb<b
Rc!c-bb+cb+bb+bb+bb+bb+bb<b
Rc!c-bb+cb+bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+bb+bb+bb+cb<b
Rc!c-bb+cb+bb+cb+bb+bb+cb+bb+cb+bb+bb<b<b
Rc!c-bb+cb+bb+bb+cb+bb+cb+bb+cb+bb<b
Rc!c-bb+cb+bb+bb+cb+bb<b<b
]")
