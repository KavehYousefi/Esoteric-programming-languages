;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file contributes a lexical analyzer, or lexer, the dever of
;; which appertains to the recognition and extraction of significant
;; objects from a piece of V3i source code and their furnishment to
;; interested parties in the guise of tokens.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized V3i keywords in their string form with
   representative tokens.")

;;; -------------------------------------------------------

(defun register-identifier (name token)
  "Registers the V3i identifier by its NAME, affiliated with the TOKEN
   representation, and returns no value."
  (declare (type string name))
  (declare (type Token  token))
  (setf (gethash name +IDENTIFIERS+) token)
  (values))

;;; -------------------------------------------------------

(defun register-identifiers ()
  "Registers the recognized V3i identifiers at the +IDENTIFIERS+ table
   and returns no value."
  (clrhash +IDENTIFIERS+)
  (register-identifier "def"   (make-token :def   "def"))
  (register-identifier "var"   (make-token :var   "var"))
  (register-identifier "sqrt"  (make-token :sqrt  "sqrt"))
  (register-identifier "print" (make-token :print "print"))
  (register-identifier "if"    (make-token :if    "if"))
  (register-identifier "else"  (make-token :else  "else"))
  (register-identifier "iloop" (make-token :iloop "iloop"))
  (register-identifier "loop"  (make-token :loop  "loop"))
  (register-identifier "input" (make-token :input "input"))
  (register-identifier "wait"  (make-token :wait  "wait"))
  (register-identifier "end"   (make-token :end   "end"))
  
  (register-identifier "x"     (make-token :variable "x"))
  (register-identifier "y"     (make-token :variable "y"))
  (register-identifier "z"     (make-token :variable "z"))
  (values))

;;; -------------------------------------------------------

(defun get-identifier (name)
  "Returns for the NAME a covenable token representation, this either
   resolving to a recognized language keyword, a variable agnomination,
   or a Procrustean identifier specimen."
  (declare (type string name))
  (the Token
    (gethash name +IDENTIFIERS+
      (make-token :identifier name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source for lexer.")
    :type          string
    :documentation "The piece of V3i source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class furnishes an entity invested with such
     capacitation as to analyze a piece of V3i source code and extract
     its significant objects in the form of tokens."))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slot ``source'' to the local symbol
   macro ``$source'', its ``position'' to ``$position'', and its current
   ``character'' to ``$character'', executes the BODY forms, and returns
   the desinent form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           (($source
             (the string
               (slot-value ,evaluated-lexer 'source)))
            ($position
             (the fixnum
               (slot-value ,evaluated-lexer 'position)))
            ($character
             (the (or null character)
               (slot-value ,evaluated-lexer 'character))))
         (declare (type string              $source))
         (declare (ignorable                $source))
         (declare (type fixnum              $position))
         (declare (ignorable                $position))
         (declare (type (or null character) $character))
         (declare (ignorable                $character))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initializes the LEXER's current character and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf $character
      (when (array-in-bounds-p $source $position)
        (char $source $position))))
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a fresh ``Lexer'' dedicated to the piece of V3i
   SOURCE code's evaluation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf $character
      (when (array-in-bounds-p $source (1+ $position))
        (char $source
          (incf $position)))))
  (values))

;;; -------------------------------------------------------

(defun write-current-character (lexer destination)
  "Writes the LEXER's current character to the DESTINATION, advances its
   position cursor to the next location in its source, and returns no
   value."
  (declare (type Lexer       lexer))
  (declare (type destination destination))
  (with-lexer (lexer)
    (write-char $character destination))
  (advance-lexer lexer)
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and $character (whitespace-character-p $character)) do
      (advance-lexer lexer)))
  (values))

;;; -------------------------------------------------------

(defun skip-comment (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a comment section, delineated by a jumelle of hash signs (\"#\"), and
   returns no value."
  (declare (type Lexer lexer))
  (advance-lexer lexer)
  (with-lexer (lexer)
    (loop do
      (case $character
        ((NIL)
          (error "Unterminated comment at position ~d." $position))
        (#\#
          (advance-lexer lexer)
          (loop-finish))
        (otherwise
          (advance-lexer lexer)))))
  (values))

;;; -------------------------------------------------------

(defun read-word (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an identifier and returns a string representation thereof."
  (declare (type Lexer lexer))
  (the string
    (with-output-to-string (identifier)
      (declare (type string-stream identifier))
      (with-lexer (lexer)
        (loop while (and $character (alphanumericp $character)) do
          (write-current-character lexer identifier))))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an identifier and returns a covenable token representation
   thereof."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier
      (read-word lexer))))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Returns a new token which combines the TOKEN-TYPE with the LEXER's
   current character, concomitantly consuming the latter."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (make-token token-type
      (with-lexer (lexer)
        (prog1 $character
          (advance-lexer lexer))))))

;;; -------------------------------------------------------

(defun read-string-literal (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a string literal demarcated by a twissel of quotation marks ('\"'),
   and returns a ``:string'' representation thereof."
  (declare (type Lexer lexer))
  (advance-lexer lexer)
  (the Token
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (with-lexer (lexer)
          (loop do
            (case $character
              ((NIL)
                (error "Unterminated string literal."))
              (#\"
                (advance-lexer lexer)
                (loop-finish))
              (otherwise
                (write-current-character lexer content)))))))))

;;; -------------------------------------------------------

(defun read-numeric-literal (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned integer number literal and returns a ``:number'' token
   representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (with-lexer (lexer)
            (loop while (and $character (digit-char-p $character)) do
              (write-current-character lexer digits))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (cond
        ;; End of file (EOF).
        ((null $character)
          (make-eof-token))
        
        ;; Spaces and comments.
        ((whitespace-character-p $character)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        ((char= $character #\#)
          (skip-comment   lexer)
          (get-next-token lexer))
        
        ;; Sepiments.
        ((char= $character #\;)
          (read-symbol lexer :semicolon))
        
        ;; Literals.
        ((char= $character #\")
          (read-string-literal lexer))
        ((digit-char-p $character)
          (read-numeric-literal lexer))
        
        ;; Keywords and variables.
        ((alpha-char-p $character)
          (read-identifier lexer))
        
        ;; Arithmetic operators.
        ((char= $character #\+)
          (read-symbol lexer :plus))
        ((char= $character #\-)
          (read-symbol lexer :minus))
        ((char= $character #\*)
          (read-symbol lexer :times))
        ((char= $character #\/)
          (read-symbol lexer :divide))
        ((char= $character #\%)
          (read-symbol lexer :remainder))
        
        ;; Relational operators.
        ((char= $character #\=)
          (read-symbol lexer :equal-to))
        ((char= $character #\>)
          (read-symbol lexer :greater-than))
        ((char= $character #\<)
          (read-symbol lexer :less-than))
        
        (T
          (error "Unexpected character \"~c\" at position ~d."
            $character $position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Preparation of identifiers.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-identifiers)
