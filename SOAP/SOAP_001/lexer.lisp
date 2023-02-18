;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file comprehends the "Lexer" class, its operations, and the
;; logically independent character handling adminicula, serving in the
;; extraction of tokens from a piece of SOAP source code.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer (source
                            &aux (character
                                   (when (array-in-bounds-p source 0)
                                     (char source 0))))))
  "The ``Lexer'' class establishes a unit endowed with the competence to
   recognize and extract from a piece of SOAP source code the pertinent
   objects and returning these as ``Token'' instances."
  (source    (error "Missing lexer source.") :type string)
  (position  0                               :type fixnum)
  (character NIL                             :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'' and
   ``character'' to eponymous local symbol macros for general access,
   evaluates the BODY forms, and returns the last processed form's
   results.
   ---
   As an additional service, two local functions are being defined for
   enhanced convenience:
   
     ------------------------------------------------------------------
     Local function         | Effect
     -----------------------+------------------------------------------
     advance ()             | Moves the LEXER's position cursor to the
                            | next character in its source.
     ..................................................................
     character-equals-p (c) | Checks whether the character C equals the
                            | LEXER's current character and returns a
                            | ``boolean'' result.
     ------------------------------------------------------------------"
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           ((source
             (the string
               (lexer-source ,evaluated-lexer)))
            (position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            (character
             (the (or null character)
               (lexer-character ,evaluated-lexer))))
         (declare (type string              source))
         (declare (ignorable                source))
         (declare (type fixnum              position))
         (declare (ignorable                position))
         (declare (type (or null character) character))
         (declare (ignorable                character))
         (flet
             ((advance ()
               "Moves the POSITION cursor to the next character in the
                SOURCE, if possible, updates the current CHARACTER, and
                returns no value."
               (setf character
                 (when (array-in-bounds-p source (1+ position))
                   (char source (incf position))))
               (values))
              (character-equals-p (expected-character)
               "Checks whether the current CHARACTER equals the
                EXPECTED-CHARACTER in a case-sensitive mode, returning
                on confirmation a ``boolean'' value of ``T'', otherwise
                ``NIL''."
               (the boolean
                 (not (null
                   (and character
                        (char= character expected-character)))))))
           ,@body)))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current position into the LEXER's source, skips a
   sequence of zero or more adjacent whitespaces and returns the
   contingently modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and character (whitespace-character-p character)) do
      (advance)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-singleton-token (lexer token-type)
  "Envelops the LEXER's current character as the value into a fresh
   token of the TOKEN-TYPE and returns this encapsulation, while
   concomitantly advancing the LEXER's position cursor."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (with-lexer (lexer)
    (the Token
      (prog1
        (make-token token-type character)
        (advance)))))

;;; -------------------------------------------------------

(defun lexer-read-quoted-character (lexer)
  "Expecting the LEXER to currently reside on a double quote ('\"'),
   skips the same, envelops the next character in a fresh token of the
   type ``:quoted-character'' and this encapsulation, while
   concomitantly advancing the LEXER's position cursor."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (advance)
    (the Token
      (if character
        (lexer-read-singleton-token lexer :quoted-character)
        (error "Expected a character succeeding the comma, ~
                but encountered end of file (EOF) at position ~d."
          position)))))

;;; -------------------------------------------------------

(defun lexer-read-apostrophized-character (lexer)
  "Expecting the LEXER to currently reside on an apostrophe (\"'\"),
   skips the same, envelops the next character in a fresh token of the
   type ``:apostrophized-character'' and this encapsulation, while
   concomitantly advancing the LEXER's position cursor."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (advance)
    (the Token
      (if character
        (lexer-read-singleton-token lexer :apostrophized-character)
        (error "Expected a character succeeding the apostrophe, ~
                but encountered end of file (EOF) at position ~d."
          position)))))

;;; -------------------------------------------------------

(defun ascertain-natural-number (integer)
  "Checks whether the INTEGER represents a natural number, that is, a
   positive integer value in the range [1, +infinity], on confirmation
   returning the INTEGER itself, otherwise signaling an error of an
   unspecified type."
  (declare (type integer integer))
  (the natural-number
    (if (typep integer 'natural-number)
      integer
      (error "The value ~d is not a natural number." integer))))

;;; -------------------------------------------------------

(defun lexer-read-integer (lexer)
  "Starting at the current position into the LEXER's source, reads a
   sequence of zero or more ternary (base-3) digits and returns an
   ``:integer'' token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :integer
        (ascertain-natural-number
          (parse-integer
            (with-output-to-string (digits)
              (declare (type string-stream digits))
              (loop while (and character (digit-char-p character 3)) do
                (write-char character digits)
                (advance)))
            :radix 3))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   In the case of its source's exhaustion, the LEXER responds to each
   query with a fresh instance of an end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        
        ((digit-char-p character 3)
          (lexer-read-integer lexer))
        
        ((character-equals-p #\%)
          (lexer-read-singleton-token lexer :percent))
        
        ((character-equals-p #\Ø)
          (lexer-read-singleton-token lexer :empty-set))
        
        ((character-equals-p #\{)
          (lexer-read-singleton-token lexer :left-brace))
        
        ((character-equals-p #\})
          (lexer-read-singleton-token lexer :right-brace))
        
        ((character-equals-p #\,)
          (lexer-read-singleton-token lexer :comma))
        
        ((character-equals-p #\*)
          (lexer-read-singleton-token lexer :asterisk))
        
        ((character-equals-p #\")
          (lexer-read-quoted-character lexer))
        
        ((character-equals-p #\∪)
          (lexer-read-singleton-token lexer :union))
        
        ((character-equals-p #\∩)
          (lexer-read-singleton-token lexer :intersection))
        
        ((character-equals-p #\⊆)
          (lexer-read-singleton-token lexer :subset))
        
        ((character-equals-p #\⊂)
          (lexer-read-singleton-token lexer :proper-subset))
        
        ((character-equals-p #\⊄)
          (lexer-read-singleton-token lexer :not-subset))
        
        ((character-equals-p #\⊇)
          (lexer-read-singleton-token lexer :superset))
        
        ((character-equals-p #\⊃)
          (lexer-read-singleton-token lexer :proper-superset))
        
        ((character-equals-p #\⊅)
          (lexer-read-singleton-token lexer :not-superset))
        
        ((character-equals-p #\[)
          (lexer-read-singleton-token lexer :left-bracket))
        
        ((character-equals-p #\])
          (lexer-read-singleton-token lexer :right-bracket))
        
        ((character-equals-p #\=)
          (lexer-read-singleton-token lexer :equal))
        
        ((character-equals-p #\c)
          (lexer-read-singleton-token lexer :c))
        
        ((character-equals-p #\-)
          (lexer-read-singleton-token lexer :left-difference))
        
        ((character-equals-p #\_)
          (lexer-read-singleton-token lexer :right-difference))
        
        ((character-equals-p #\:)
          (lexer-read-singleton-token lexer :colon))
        
        ((character-equals-p #\;)
          (lexer-read-singleton-token lexer :semicolon))
        
        ((character-equals-p #\~)
          (lexer-read-singleton-token lexer :tilde))
        
        ((character-equals-p #\')
          (lexer-read-apostrophized-character lexer))
        
        ((character-equals-p #\/)
          (lexer-read-singleton-token lexer :slash))
        
        ((character-equals-p #\\)
          (lexer-read-singleton-token lexer :backslash))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))
