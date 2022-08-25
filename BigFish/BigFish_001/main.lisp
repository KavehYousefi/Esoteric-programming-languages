;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BigFish", designed by the Esolang user "Yes", and bearing
;; a semblance to the esoteric companion "Deadfish", however, extended
;; by additional registers and a loop facility.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-08-23
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BigFish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0)."
  '(integer 0 *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant datum obtained by the
   analyzation of a piece of BigFish code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token associated with the IDENTIFIER, or signals an error
   of an unspecified type if no affiliation exists."
  (declare (type string identifier))
  (the Token
    (cond
      ((string= identifier "i")
        (make-token :increment identifier))
      ((string= identifier "d")
        (make-token :decrement identifier))
      ((string= identifier "s")
        (make-token :square identifier))
      ((string= identifier "o")
        (make-token :output identifier))
      ((string= identifier "new")
        (make-token :new identifier))
      ((string= identifier "loop")
        (make-token :loop identifier))
      (T
        (error "Unrecognized identifier: ~s." identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun newline-character-p (character)
  "Checks whether the CHARACTER represents a linebreak, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Newline #\Linefeed #\Return)
        :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of BigFish code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    source."))
  (:documentation
    "The ``Lexer'' constitutes the responsible unit for the separation
     ofa piece of BigFish code into its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which operates on the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'', and
   ``character'' to eponymous symbol macros for general access,
   evaluates the BODY forms, and returns the last evaluated forms'
   results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (with-slots (source position character) ,evaluated-lexer
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         ,@body))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves to the next character in the LEXER's source, if possible, and
   returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position into the LEXER's source, reads an
   unsigned integer number and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current position into the LEXER's source, reads an
   identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (get-identifier-token
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop while (and character (alpha-char-p character)) do
            (write-char character identifier)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request by
   returning a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (lexer-advance        lexer)
          (lexer-get-next-token lexer))
        
        ((newline-character-p character)
          (lexer-advance        lexer)
          (lexer-get-next-token lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((char= character #\{)
          (prog1
            (make-token :left-brace character)
            (lexer-advance lexer)))
        
        ((char= character #\})
          (prog1
            (make-token :right-brace character)
            (lexer-advance lexer)))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Statement".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Statement
  (:constructor make-statement (type &rest arguments)))
  "The ``Statement'' class represents a statement, compact of its
   instruction type and zero to two arguments."
  (type
    (error "Missing statement type.")
    :type keyword)
  (arguments
    NIL
    :type (list-of non-negative-integer)))

;;; -------------------------------------------------------

(defun statement-argument (statement index)
  "Returns the STATEMENT's argument at the INDEX.
   ---
   An error of an unspecified type transpires upon an invalid INDEX into
   the STATEMENT's argument list."
  (declare (type Statement            statement))
  (declare (type non-negative-integer index))
  (the non-negative-integer
    (elt (statement-arguments statement) index)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for parser.")
    :type          Lexer
    :documentation "The token provider.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently obtained token from the LEXER."))
  (:documentation
    "The ``Parser'' class is responsible for the assemblage of a token
     stream, supplied by a lexer, into a sequence of statements, which
     subsequently may be interpreted."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token
      (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which obtains its requisite
   tokens from the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and ``current-token''
   to eponymous macro symbols for general access, executes the BODY
   forms, and returns the last evaluated form's values."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (with-slots (lexer current-token) ,evaluated-parser
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         (declare (ignorable  lexer))
         (declare (ignorable  current-token))
         ,@body))))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's currently stored token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the tested token,
   while querying and storing the next one from the PARSER's lexer;
   otherwise signals an error of an unexpected type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-parser (parser)
    (the Token
      (if (token-type-p current-token expected-token-type)
        (prog1
          current-token
          (setf current-token (lexer-get-next-token lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-statement (parser)
  "Using the PARSER parses a statement and returns a ``Statement''
   representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Statement
      (case (token-type current-token)
        ((:increment :decrement :square :output)
          (parser-parse-register-based-statement parser))
        (:new
          (parser-eat parser :new)
          (make-statement :new))
        (:loop
          (parser-parse-loop parser))
        (otherwise
          (error "Invalid statement token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-register-based-statement (parser)
  "Using the PARSER parses a statement which consists of a command and
   a single register as its argument, and returns a ``Statement''
   representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Statement
      (make-statement
        (token-type  (parser-eat parser (token-type current-token)))
        (token-value (parser-eat parser :number))))))

;;; -------------------------------------------------------

(defun parser-parse-loop (parser)
  "Using the PARSER parses a \"loop\" statement and returns a
  ``Statement'' representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (parser-eat parser :loop)
    (let ((repetitions (token-value (parser-eat parser :number))))
      (declare (type non-negative-integer repetitions))
      (parser-eat parser :left-brace)
      (the Statement
        (loop
          until   (token-type-p current-token :right-brace)
          collect (parser-parse-statement parser)
          into    statements
          finally
            (parser-eat parser :right-brace)
            (return (make-statement :loop repetitions statements)))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the program represented by the PARSER's internally managed
   lexer tokens, and returns a list of statements."
  (declare (type Parser parser))
  (with-parser (parser)
    (the (list-of Statement)
      (loop
        until   (token-type-p current-token :eof)
        collect (parser-parse-statement parser)
        into    statements
        finally (return statements)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((statements
    :initarg       :statements
    :initform      (error "Missing interpreter statements.")
    :type          (list-of Statement)
    :documentation "The BigFish statements to process.")
   (registers
    :initarg       :registers
    :initform      (make-array 0
                     :element-type    'integer
                     :initial-element 0
                     :adjustable      T
                     :fill-pointer    0)
    :type          (vector integer *)
    :documentation "The integer-valued register.
                    ---
                    Albeit a vector is zero-based, the interpreter's
                    functions simulate a one-based register indexing."))
  (:documentation
    "The ``Interpreter'' class applies itself to the induction of effect
     into a statement list."))

;;; -------------------------------------------------------

(defun make-interpreter (statements)
  "Creates and returns a new ``Interpreter'' which evaluates the
   STATEMENTS."
  (declare (type (list-of Statement) statements))
  (the Interpreter
    (make-instance 'Interpreter :statements statements)))

;;; -------------------------------------------------------

(defun interpreter-register-at (interpreter index)
  "Returns the value of the register maintained by the INTERPRETER at
   the one-based INDEX."
  (declare (type Interpreter          interpreter))
  (declare (type non-negative-integer index))
  (with-slots (registers) interpreter
    (declare (type (vector integer *) registers))
    (the integer (aref registers (1- index)))))

;;; -------------------------------------------------------

(defun (setf interpreter-register-at) (new-value interpreter index)
  "Sets the value of the register maintained by the INTERPRETER at the
   one-based INDEX to the NEW-VALUE and returns the modified
   INTERPRETER."
  (declare (type integer              new-value))
  (declare (type Interpreter          interpreter))
  (declare (type non-negative-integer index))
  (with-slots (registers) interpreter
    (declare (type (vector integer *) registers))
    (setf (aref registers (1- index)) new-value))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-process-statement (interpreter statement)
  "Employs the INTERPRETER in evaluating the STATEMENT and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Statement   statement))
  
  (with-slots (registers) interpreter
    (declare (type (vector integer *) registers))
    
    (case (statement-type statement)
      (:increment
        (let ((register (statement-argument statement 0)))
          (declare (type non-negative-integer register))
          (incf (interpreter-register-at interpreter register))))
      
      (:decrement
        (let ((register (statement-argument statement 0)))
          (declare (type non-negative-integer register))
          (decf (interpreter-register-at interpreter register))))
      
      (:square
        (let ((register (statement-argument statement 0)))
          (declare (type non-negative-integer register))
          (setf (interpreter-register-at interpreter register)
                (* (interpreter-register-at interpreter register)
                   (interpreter-register-at interpreter register)))))
      
      (:output
        (let ((register (statement-argument statement 0)))
          (format T "~d "
            (interpreter-register-at interpreter register))))
      
      (:new
        (vector-push-extend 0 registers))
      
      (:loop
        (destructuring-bind (repetitions body)
            (statement-arguments statement)
          (declare (type non-negative-integer repetitions))
          (declare (type (list-of Statement)  body))
          
          (loop repeat repetitions do
            (dolist (body-statement body)
              (declare (type Statement body-statement))
              (interpreter-process-statement interpreter
                body-statement)))))
      
      (otherwise
        (error "Invalid statement: ~s." statement))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Executes the INTERPRETER's statements and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (statements) interpreter
    (declare (type (list-of Statement) statements))
    (dolist (statement statements)
      (interpreter-process-statement interpreter statement)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-BigFish (code)
  "Interprets the piece of BigFish CODE and returns no value."
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create ten registers and print these.
(interpret-BigFish
  "loop 10
   {
   new
   }
   
   o 1
   o 2
   o 3
   o 4
   o 5
   o 6
   o 7
   o 8
   o 9
   o 10")

;;; -------------------------------------------------------

;; Make three register, set each to the value ten (10), and print them.
(interpret-BigFish
  "new
   new
   new
   loop 10
   {
   i 1
   }
   loop 10
   {
   i 2
   }
   loop 10
   {
   i 3
   }
   
   o 1
   o 2
   o 3")

;;; -------------------------------------------------------

;; Print the numbers from one (1) up to ten (10).
(interpret-BigFish
  "new
   loop 10
   {
   i 1
   o 1
   }
  ")

;;; -------------------------------------------------------

;; Print the numbers one (1) to 100 using a nested loop.
(interpret-BigFish
  "new
   i 1
   loop 10
   {
     loop 10
     {
       o 1
       i 1
     }
   }
  ")
