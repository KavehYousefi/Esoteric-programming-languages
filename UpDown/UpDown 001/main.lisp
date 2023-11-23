;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "UpDown", invented by the Esolang user "IdfbAn" and
;; presented on December 22nd, 2022, the kenspeckle proprium of which
;; maintains its commorancy in its reliance on variables as the sole
;; instruments for basic arithmetics and output.
;; 
;; 
;; Concept
;; =======
;; The UpDown programming language operates on a registry of at least 26
;; variables of fixed monikers --- a homologation incorporated to extend
;; the allowance into a bournless quantity with arbitrary agnominations
;; if implementing the option "UD++" extension ---, the members of which
;; respond to augmentation (the "up" command), deduction (a "down"
;; operation invocation), and printing in their ASCII character form.
;; 
;; == VARIABLES GOVERN THE DATA ASPECT ==
;; The language's data castaldy is realized exclusively by the
;; adminicula of its variables, restricted in their tally for the basic
;; UpDown mode to the 26 Latin majuscules, "A" through "Z", and, if
;; accoutred with the UD++ extension, propined the faculty to declare an
;; infinite amount of additional placeholders, each with a unique
;; identification of one's personal choice.
;; 
;; Every variable may store a scalar integer, signed and unrestrained in
;; its magnitude, defaulting to zero (0) at the salvatory's inchoation.
;; 
;; == UD++: AN EXTENSION FOR CUSTOM VARIABLES ==
;; The foundational 26-cardinality being a dioristic component of the
;; environment may be lifted by mediation of the extension "UD++", an
;; optional part of the UpDown language, whose wike solely amplects the
;; contingency for variable declarations, proceeding in an unbridled
;; mode along its amount and agnominations.
;; 
;; 
;; Syntax
;; ======
;; An UpDown program constitutes a composition of zero or more
;; statements, each such an instruction's carriage, bearing the
;; arguments, and concluding with a semicolon (";").
;; 
;; == EACH STATEMENT: A COMMAND, ITS ARGUMENTS, AND A SEMICOLON ==
;; Each statement in the UpDown programming language is compact of a
;; single command, succeeded by its one or more arguments, and
;; terminated via a semicolon (";").
;; 
;; == WHITESPACES ==
;; Whitespaces, except for their imperative agency as the tokens'
;; sepiments, may be distributed liberally.
;; 
;; == COMMENTS ==
;; No provision for comments exists in the current language rendition.
;; 
;; 
;; Instructions
;; ============
;; UpDown's bifidity, incurred upon by the supererogative competence
;; that the UD++ imports, ostends itself in the perimeter of its
;; instructions, where a pair royal governs the fundamental variation,
;; satisfying the requisites of basic arithmetics and output, whereas
;; the advenient component homologates the definition of custom
;; variables in any account and agnomination.
;; 
;; With respect to concrete specifications, the following commands
;; contribute to the mandatory standard facilities:
;;   down
;;   print
;;   up
;; 
;; Additionally, an advenient element is introduced by the optional
;; UD++ extension:
;;   var
;; 
;; == OVERVIEW ==
;; The prial of imperative operations and the aefauld constituent
;; imported by UD++ shall be a cursory ilk of nortelry adhibition's
;; material.
;; 
;; Please heed that placeholder segments are signified by a line of
;; asterisks ("*"), tharfing their substitution by valid UpDown code.
;; 
;;   ------------------------------------------------------------------
;;   Command             | Effect
;;   --------------------+---------------------------------------------
;;   down varName amount | Decrements the value of the variable
;;        ******* ****** | amenable to the name {varName} by the
;;                       | {amount}.
;;                       |---------------------------------------------
;;                       | {varName} must be the identifier of a extant
;;                       | variable.
;;                       |---------------------------------------------
;;                       | {amount} must be a signed or unsigned
;;                       | integer number.
;;                       |---------------------------------------------
;;                       | An error of the type "NoSuchVariableError"
;;                       | is signaled if {varName} does not designate
;;                       | the name of an extant variable.
;;   ..................................................................
;;   print varName       | Prints to the standard output the state of
;;         *******       | the variable amenable to the name {varName},
;;                       | concretely:
;;                       |   (a) If the variable value occupies the
;;                       |       extended ASCII code range [0, 255],
;;                       |       prints the character whose ASCII code
;;                       |       equals the variable value to the
;;                       |       standard output, not preceded by any
;;                       |       separating content.
;;                       |   (b) If the variable value is less than
;;                       |       zero (0), prints the following message
;;                       |       to the standard output on a line of
;;                       |       its own:
;;                       |         Error! Value too low
;;                       |   (c) If the variable value is greater than
;;                       |       255, prints the following message to
;;                       |       the standard output on a line of its
;;                       |       own:
;;                       |         Error! Value too high
;;                       |---------------------------------------------
;;                       | An error of the type "NoSuchVariableError"
;;                       | is signaled if {varName} does not designate
;;                       | the name of an extant variable.
;;   ..................................................................
;;   up varName amount   | Increments the value of the variable
;;      ******* ******   | amenable to the name {varName} by the
;;                       | {amount}.
;;                       |---------------------------------------------
;;                       | {varName} must be the identifier of a extant
;;                       | variable.
;;                       |---------------------------------------------
;;                       | {amount} must be a signed or unsigned
;;                       | integer number.
;;                       |---------------------------------------------
;;                       | An error of the type "NoSuchVariableError"
;;                       | is issued if {varName} does not designate
;;                       | the name of an extant variable.
;;   ..................................................................
;;   var varName         | Declares a new variable identified by the
;;       *******         | name {varName}, with its value initialized
;;                       | to zero (0).
;;                       |---------------------------------------------
;;                       | An error of the type
;;                       | "DuplicateVariableError" is issued if
;;                       | {varName} designates the name of an already
;;                       | extant variable.
;;                       |---------------------------------------------
;;                       | This operation constitutes an optional
;;                       | introduction defined by the UD++ extension.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-11-02
;; 
;; Sources:
;;   [esolang2023UpDown]
;;   The Esolang contributors, "UpDown", May 23rd, 2023
;;   URL: "https://esolangs.org/wiki/UpDown"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   amplecting in this diorism, without the claim of exhaustion, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype token-type ()
  "The ``token-type'' type enumerates the recognized variations of token
   classes."
  '(member
    :down
    :eof
    :identifier
    :number
    :print
    :semicolon
    :up
    :var))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
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

(deftype upDown-program ()
  "The ``upDown-program'' type defines an executable UpDown program as a
   list of zero or more ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (loop
            for key
              of-type T
              being the hash-keys in (the hash-table candidate)
            using
              (hash-value value)
            always
              (and (typep key   key-type)
                   (typep value value-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype variable-table ()
  "The ``variable-table'' type defines a variable registry which maps
   the available variable identifiers to their signed integer contents,
   realized as a hash table whose keys assume the string names, and
   whose integer values furnish the variable states."
  '(hash-table-of string integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token ()
  ((type
    :initarg       :type
    :initform      (error "Missing token type.")
    :reader        get-token-type
    :type          token-type
    :documentation "Defines the token's membership to a specific
                    species, such as an identifier or a number.")
   (raw-content
    :initarg       :raw-content
    :initform      (error "Missing token content.")
    :reader        get-raw-token-content
    :type          string
    :documentation "Stores the token VALUE's verbatim form, as extracted
                    during the lexical analyzation process, and
                    preceding the adjustment to the ultimate VALUE
                    form.")
   (value
    :initarg       :value
    :initform      (error "Missing token value.")
    :reader        get-token-value
    :type          T
    :documentation "Associates a datum appropriate for the token
                    type."))
  (:documentation
    "The ``Token'' class serves in the encapsulation of all information
     posing the requisites of an object extracted during the lexical
     analyzation of a piece of UpDown source code."))

;;; -------------------------------------------------------

(defun make-token (type raw-content value)
  "Creates and returns a new ``Token'' belonging to the TYPE, and whose
   RAW-CONTENT, as extracted from the source, is parsed into the VALUE
   form."
  (the Token
    (make-instance 'Token
      :type        type
      :raw-content raw-content
      :value       value)))

;;; -------------------------------------------------------

(defun make-eof-token ()
  "Creates and returns an end-of-file (EOF) token."
  (the Token
    (make-token :eof "" NIL)))

;;; -------------------------------------------------------

(defun token-of-type-p (probed-token expected-type)
  "Determines whether the PROBED-TOKEN conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token      probed-token))
  (declare (type token-type expected-type))
  (the boolean
    (not (null
      (eq (get-token-type probed-token) expected-type)))))

;;; -------------------------------------------------------

(defmethod print-object ((token Token) (stream T))
  (declare (type Token       token))
  (declare (type destination stream))
  (format stream "(Token :type ~s :raw-content ~s :value ~s)"
    (slot-value token 'type)
    (slot-value token 'raw-content)
    (slot-value token 'value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a UpDown identifier
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (and (char/= candidate #\;)
               (not (whitespace-character-p candidate))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :reader        get-lexer-source
    :type          string
    :documentation "The piece of UpDown source code to analyze.")
   (position
    :initform      0
    :accessor      lexer-position
    :type          fixnum
    :documentation "The location of the currently processed CHARACTER in
                    the SOURCE.")
   (character
    :initform      NIL
    :accessor      lexer-character
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class furnishes a lexical analyzer, also known as a
     \"scanner\", for the purpose of extracting significant objects, in
     the mold of tokens, from a piece of UpDown source code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initializes the LEXER's current character to the first position in
   its source and returns no value."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (get-lexer-source lexer)
            (lexer-position lexer))
      (char (get-lexer-source lexer)
        (lexer-position lexer))))
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' ordained to the wike of the
   UpDown SOURCE code's lexical analyzation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (get-lexer-source lexer)
            (1+ (lexer-position lexer)))
      (char (get-lexer-source lexer)
        (incf (lexer-position lexer)))))
  (values))

;;; -------------------------------------------------------

(defun on-whitespace-p (lexer)
  "Determines whether the LEXER's position cursor currently resides at a
   whitespace, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (not (null
      (and (lexer-character lexer)
           (whitespace-character-p
             (lexer-character lexer)))))))

;;; -------------------------------------------------------

(defun on-identifier-p (lexer)
  "Determines whether the LEXER's position cursor currently resides
   inside of an identifier, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (and (lexer-character lexer)
         (identifier-character-p
           (lexer-character lexer)))))

;;; -------------------------------------------------------

(defun read-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   word, demarcated by a whitespace or the source's exhaustion, and
   returns its verbatim string representation."
  (declare (type Lexer lexer))
  (the string
    (with-output-to-string (word)
      (declare (type string-stream word))
      (loop
        while (on-identifier-p lexer)
        do
          (write-char (lexer-character lexer) word)
          (advance-lexer lexer)
        finally
          (return (lexer-position lexer))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (loop while (on-whitespace-p lexer) do
    (advance-lexer lexer))
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (word)
  "Determines whether the WORD represents an UpDown language keyword,
   returning on confirmation an accommodated token for such, otherwise
   responding with a generic ``:identifier'' variant."
  (declare (type string word))
  (flet ((word-equals-p (expected-content)
          "Determines whether the probed WORD equals the
           EXPECTED-CONTENT, returning on confirmation a ``boolean''
           value of ``T'', otherwise ``NIL''."
          (declare (type string expected-content))
          (the boolean
            (not (null
              (string= word expected-content))))))
    (the Token
      (cond
        ((word-equals-p "down")  (make-token :down       word word))
        ((word-equals-p "print") (make-token :print      word word))
        ((word-equals-p "up")    (make-token :up         word word))
        ((word-equals-p "var")   (make-token :var        word word))
        (T                       (make-token :identifier word word))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        (make-eof-token))
      ((on-whitespace-p lexer)
        (skip-whitespaces lexer)
        (get-next-token   lexer))
      ((char= (lexer-character lexer) #\;)
        (prog1
          (make-token :semicolon ";" ";")
          (advance-lexer lexer)))
      (T
        (let ((word (read-word lexer)))
          (declare (type string word))
          (handler-case
            (let ((numeric-value (parse-integer word)))
              (declare (type integer numeric-value))
              (make-token :number word numeric-value))
            (error ()
              (get-identifier-token word))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command ()
  ()
  (:documentation
    "The ``Command'' interface establishes a common foundry for all
     concrete classes appropriated for the representation of UpDown
     commands."))

;;; -------------------------------------------------------

(defclass Abstract-Command (Command)
  ((variable-name
    :initarg       :variable-name
    :initform      (error "Missing variable.")
    :reader        get-variable-name
    :type          string
    :documentation "The identifier of the variable to query or alter."))
  (:documentation
    "The ``Abstract-Command'' abstract class establishes a common
     foundry for all concrete classes in representation of UpDown
     commands that depend on a variable, which, at the instant of this
     project's realization, lays its amplectation around the instruction
     roster's entirety.
     ---
     This abstract class maintains the identifier of the variable to
     perquire or modify."))

;;; -------------------------------------------------------

(defclass Down-Command (Abstract-Command)
  ((amount
    :initarg       :amount
    :initform      (error "Missing amount.")
    :reader        get-amount
    :type          integer
    :documentation "The amount by which to reduce the variable amenable
                    to the VARIABLE-NAME."))
  (:documentation
    "The ``Down-Command'' class models the variable decrementation
     operation, entailing in its compass the targeted variable's
     agnomination, as well as the integer amount to deduct by.
     ---
     This class affiliates with the UpDown command \"down\"."))

;;; -------------------------------------------------------

(defclass Print-Command (Abstract-Command)
  ()
  (:documentation
    "The ``Print-Command'' class models the variable content output
     operation, entailing in its compass merely the variable to access.
     ---
     This class affiliates with the UpDown command \"print\"."))

;;; -------------------------------------------------------

(defclass Up-Command (Abstract-Command)
  ((amount
    :initarg       :amount
    :initform      (error "Missing amount.")
    :reader        get-amount
    :type          integer
    :documentation "The amount by which to augment the variable amenable
                    to the VARIABLE-NAME."))
  (:documentation
    "The ``Up-Command'' class models the variable incrementation
     operation, entailing in its compass the targeted variable's
     agnomination, as well as the integer amount to raise by.
     ---
     This class affiliates with the UpDown command \"up\"."))

;;; -------------------------------------------------------

(defclass Var-Command (Abstract-Command)
  ()
  (:documentation
    "The ``Var-Command'' class models the variable declaration
     operation, entailing in its compass merely the variable to access.
     ---
     This class affiliates with the UpDown command \"var\"."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :reader        get-lexer
    :type          Lexer
    :documentation "The lexer responsible for the token purveyance.")
   (current-token
    :initform      (make-eof-token)
    :reader        get-current-token
    :writer        set-current-token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class is assigned the onus of an executable
     program's assemblage from a sequence of tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Queries the incipient token from PARSER's lexer, stores the same in
   the PARSER, and returns no value."
  (declare (type Parser parser))
  (set-current-token
    (get-next-token
      (get-lexer parser))
    parser)
  (values))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' whose tokens are supplied by the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation the probed token,
   while concomitantly querying the subsequent one from the lexer and
   storing it in lieu of the just delivered instance; otherwise an error
   of an unspecified type is signaled."
  (declare (type Parser     parser))
  (declare (type token-type expected-token-type))
  (the Token
    (prog1
      (get-current-token parser)
      (if (token-of-type-p
            (get-current-token parser)
            expected-token-type)
        (set-current-token
          (get-next-token
            (get-lexer parser))
          parser)
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type
          (get-current-token parser))))))

;;; -------------------------------------------------------

(defun eat-current-token (parser)
  "Returns the PARSER's current token, while concomitantly querying the
   subsequent one from the lexer and storing it in lieu of the just
   delivered instance."
  (declare (type Parser parser))
  (the Token
    (prog1
      (get-current-token parser)
      (set-current-token
        (get-next-token
          (get-lexer parser))
        parser))))

;;; -------------------------------------------------------

(defun parse-variable (parser)
  "Parses a variable identifier employing the PARSER and returns its
   name."
  (declare (type Parser parser))
  (the string
    (case (get-token-type (get-current-token parser))
      (:semicolon
        (error "Expected a variable name, ~
                but encounterd the end of the current statement."))
      (otherwise
        (get-raw-token-content
          (eat-current-token parser))))))

;;; -------------------------------------------------------

(defun parse-number (parser)
  "Parses a signed or unsigned integer number employing the PARSER and
   returns its value."
  (declare (type Parser parser))
  (the integer
    (case (get-token-type (get-current-token parser))
      (:eof
        (error "Expected an integer number, ~
                but encountered the end of the program."))
      (:number
        (get-token-value
          (eat-current-token parser)))
      (otherwise
        (error "Expected an integer number, but encountered ~s."
          (get-current-token parser))))))

;;; -------------------------------------------------------

(defun expect-end-of-statement (parser)
  "Determines whether the PARSER's current token represents the
   statement terminator, a semicolon (\";\"), on confirmation consuming
   the same and storing the next token from its lexer, while returning
   no value; otherwise an error of an unspecified type is signaled."
  (declare (type Parser parser))
  (if (token-of-type-p (get-current-token parser) :semicolon)
    (eat-token parser :semicolon)
    (error "Expected the end of the statement, but encountered ~s."
      (get-current-token parser)))
  (values))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses a single statement employing the PARSER's services and returns
   a ``Command'' representation thereof."
  (declare (type Parser parser))
  (the Command
    (case (get-token-type (get-current-token parser))
      (:down
        (eat-token parser :down)
        (let ((variable (parse-variable parser))
              (amount   (parse-number   parser)))
          (declare (type string  variable))
          (declare (type integer amount))
          (prog1
            (make-instance 'Down-Command
              :variable-name variable
              :amount        amount)
            (expect-end-of-statement parser))))
      
      (:print
        (eat-token parser :print)
        (prog1
          (make-instance 'Print-Command
            :variable-name (parse-variable parser))
          (expect-end-of-statement parser)))
      
      (:up
        (eat-token parser :up)
        (let ((variable (parse-variable parser))
              (amount   (parse-number   parser)))
          (declare (type string  variable))
          (declare (type integer amount))
          (prog1
            (make-instance 'Up-Command
              :variable-name variable
              :amount        amount)
            (expect-end-of-statement parser))))
      
      (:var
        (eat-token parser :var)
        (prog1
          (make-instance 'Var-Command
            :variable-name (parse-variable parser))
          (expect-end-of-statement parser)))
      
      (otherwise
        (error "Expected a command identifier, but encountered ~s."
          (get-current-token parser))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Employs the PARSER for the purpose of an UpDown program's
   assemblage and returns the thus generated result."
  (declare (type Parser parser))
  (the upDown-program
    (loop
      until   (token-of-type-p (get-current-token parser) :eof)
      collect (parse-statement parser))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition UpDown-Error (error)
  ()
  (:documentation
    "The ``UpDown-Error'' condition type defines a common foundry for
     all conditions pursuing the delegation of anomalous circumstances
     to an UpDown program's processing entity."))

;;; -------------------------------------------------------

(define-condition Duplicate-Variable-Error (UpDown-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        duplicate-variable-error-offending-name
    :type          string
    :documentation "The variable identifier which, during the
                    declaration's instant, had already been extant."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Variable-Error condition))
      (declare (type destination              stream))
      (format stream "Cannot declare a variable with the name ~s, ~
                      as such is already extant."
        (duplicate-variable-error-offending-name condition))))
  (:documentation
    "The ``Duplicate-Variable-Error'' condition type serves to signal an
     anomalous situation instigated by the attempt to declare a variable
     whose name is already been assigned to an extant entry."))

;;; -------------------------------------------------------

(define-condition No-Such-Variable-Error (UpDown-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        no-such-variable-error-offending-name
    :type          string
    :documentation "The variable identifier which, during the
                    declaration's instant, had not been declared."))
  (:report
    (lambda (condition stream)
      (declare (type No-Such-Variable-Error condition))
      (declare (type destination            stream))
      (format stream "Cannot access a variable with name ~s, ~
                      as such does not exist."
        (no-such-variable-error-offending-name condition))))
  (:documentation
    "The ``No-Such-Variable-Error'' condition type serves to signal an
     anomalous situation begotten by the attempt to query for a variable
     by a name not associated with any such entity."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 26) +DEFAULT-VARIABLE-NAMES+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-VARIABLE-NAMES+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Defines the 26 default variable names, each corresponding to a Latin
   majuscule.")

;;; -------------------------------------------------------

(defun build-variable-table ()
  "Creates and returns the default variable table, comprehending in its
   inchoate state only the 26 Latin majuscular letters."
  (let ((variables (make-hash-table :test #'equal)))
    (declare (type variable-table variables))
    (loop
      for default-variable-name
        of-type character
        across  +DEFAULT-VARIABLE-NAMES+
      do
        (setf (gethash (string default-variable-name) variables) 0))
    (the variable-table variables)))

;;; -------------------------------------------------------

(defun variable-exists-p (variables name)
  "Determines whether the variable table VARIABLES comprehends an entry
   identified by the NAME, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type variable-table variables))
  (declare (type string         name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash name variables))))))

;;; -------------------------------------------------------

(defun declare-variable (variables name)
  "Registers a new variable designated by the NAME at the variable table
   VARIABLES and returns no value.
   ---
   An error of the type ``Duplicate-Variable-Error'' is signaled if the
   NAME is already in use in the VARIABLES."
  (declare (type variable-table variables))
  (declare (type string         name))
  (if (variable-exists-p variables name)
    (error 'Duplicate-Variable-Error :offending-name name)
    (setf (gethash name variables) 0))
  (values))

;;; -------------------------------------------------------

(defun get-variable-value (variables name)
  "Returns the integer value of the variable designated by the NAME in
   the variable table VARIABLES.
   ---
   An error of the type ``No-Such-Variable-Error'' is signaled if the
   NAME is absent from the VARIABLES."
  (declare (type variable-table variables))
  (declare (type string         name))
  (the integer
    (if (variable-exists-p variables name)
      (gethash name variables)
      (error 'No-Such-Variable-Error :offending-name name))))

;;; -------------------------------------------------------

(defun set-variable-value (variables name new-value)
  "Stores the NEW-VALUE in the variable designated by the NAME in the
   variable table and returns no value.
   ---
   An error of the type ``No-Such-Variable-Error'' is signaled if the
   NAME is absent from the VARIABLES."
  (declare (type variable-table variables))
  (declare (type string         name))
  (declare (type integer        new-value))
  (if (variable-exists-p variables name)
    (setf (gethash name variables) new-value)
    (error 'No-Such-Variable-Error :offending-name name))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          upDown-program
    :documentation "A list of executable UpDown commands.")
   (variables
    :initform      (build-variable-table)
    :reader        get-variables
    :type          variable-table
    :documentation "The variables as a mapping of identifiers to signed
                    integer values."))
  (:documentation
    "The ``Interpreter'' class' responsibility wones in the accompassing
     of effect to a list of UpDown commands."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' responsible for the UpDown
   PROGRAM's execution."
  (declare (type upDown-program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Processes the UpDown COMMAND in the INTERPRETER's context and
     returns no value."))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Down-Command))
  (declare (type Interpreter  interpreter))
  (declare (type Down-Command command))
  (let ((variables (get-variables     interpreter))
        (name      (get-variable-name command))
        (amount    (get-amount        command)))
    (declare (type variable-table variables))
    (declare (type string         name))
    (declare (type integer        amount))
    (set-variable-value variables name
      (- (get-variable-value variables name)
         amount)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Print-Command))
  (declare (type Interpreter   interpreter))
  (declare (type Print-Command command))
  (let ((variables (get-variables     interpreter))
        (name      (get-variable-name command)))
    (declare (type variable-table variables))
    (declare (type string         name))
    (let ((value (get-variable-value variables name)))
      (declare (type integer value))
      (cond
        ((<= 0 value 255)
          (format T "~c"
            (code-char value)))
        ((< value 0)
          (format T "~&Error! Value too low~%"))
        ((> value 255)
          (format T "~&Error! Value too high~%"))
        (T
          (format T "~&Undefined state~%")))))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Up-Command))
  (declare (type Interpreter interpreter))
  (declare (type Up-Command  command))
  (let ((variables (get-variables     interpreter))
        (name      (get-variable-name command))
        (amount    (get-amount        command)))
    (declare (type variable-table variables))
    (declare (type string         name))
    (declare (type integer        amount))
    (set-variable-value variables name
      (+ (get-variable-value variables name)
         amount)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Var-Command))
  (declare (type Interpreter interpreter))
  (declare (type Var-Command command))
  (let ((variables (get-variables     interpreter))
        (name      (get-variable-name command)))
    (declare (type variable-table variables))
    (declare (type string         name))
    (declare-variable variables name))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Executes the UpDown program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (dolist (command (get-program interpreter))
    (declare (type Command command))
    (process-command interpreter command))
  (values))

;;; -------------------------------------------------------

(defun interpret-UpDown (code)
  "Interprets the piece of UpDown source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-UpDown
  "up A 72; print A;
   up B 101; print B;
   up C 108; print C; print C;
   up E 111; print E;
   up F 44; print F;
   up G 32; print G;
   up H 87; print H;
   print E;
   up I 114; print I;
   print C;
   up J 100; print J;
   up K 33; print K;")

;;; -------------------------------------------------------

;; Print "fr" naiting custom variable declarations.
(interpret-UpDown
  "var first_letter;
   var second_letter;
   
   up first_letter 102;
   up second_letter 114;
   
   print first_letter;
   print second_letter;")

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-UpDown
  "
  var 0; var 1; var 2; var 3; var 4; var 5; var 6; var 7; var 8; var 9;
  var a; var b; var c; var d; var e; var f; var g; var h; var i;
  var j; var k; var l; var m; var n; var o; var p; var q; var r;
  var s; var t; var u; var v; var w; var x; var y; var z;
  var newline;
  var space;
  var comma;
  var period;

  up 0 48; up 1 49; up 2 50; up 3 51; up 4 52;
  up 5 53; up 6 54; up 7 55; up 8 56; up 9 57;

  up A 65; up B 66; up C 67; up D 68; up E 69; up F 70; up G 71;
  up H 72; up I 73; up J 74; up K 75; up L 76; up M 77; up N 78;
  up O 79; up P 80; up Q 81; up R 82; up S 83; up T 84; up U 85;
  up V 86; up W 87; up X 88; up Y 89; up Z 90;

  up a  97; up b  98; up c  99; up d 100; up e 101; up f 102; up g 103;
  up h 104; up i 105; up j 106; up k 107; up l 108; up m 109; up n 110;
  up o 111; up p 112; up q 113; up r 114; up s 115; up t 116; up u 117;
  up v 118; up w 119; up x 120; up y 121; up z 122;

  up newline 10;
  up space   32;
  up comma   44;
  up period  46;
  
  print 9; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 1; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 9; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 8; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 7; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 6; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 5; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 4; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 3; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 2; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 1; print space;
  print b; print o; print t; print t; print l; print e; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;

  print 1; print space;
  print b; print o; print t; print t; print l; print e; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print comma; print newline;

  print 1; print space;
  print b; print o; print t; print t; print l; print e; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print period; print newline;

  print T; print a; print k; print e; print space;
  print o; print n; print e; print space;
  print d; print o; print w; print n; print comma; print space;
  print p; print a; print s; print s; print space;
  print i; print t; print space;
  print a; print r; print o; print u; print n; print d; print comma; print newline;

  print 0; print space;
  print b; print o; print t; print t; print l; print e; print s; print space;
  print o; print f; print space;
  print b; print e; print e; print r; print space;
  print o; print n; print space;
  print t; print h; print e; print space;
  print w; print a; print l; print l; print period; print newline;

  print newline;
  ")
