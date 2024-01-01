;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the lexical analyzer, or lexer, the bailiwick
;; of which includes in its perimeter the generation of tokens from a
;; piece of NeverGonna source code in its provided string form.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents an identifier name's
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (char=         candidate #\')
          (char=         candidate #\_))))))

;;; -------------------------------------------------------

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or tab character,
   returning on confirmatino a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or
        (char= candidate #\Space)
        (char= candidate #\Tab))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Token +EOF-TOKEN+))

;;; -------------------------------------------------------

(defparameter +EOF-TOKEN+
  (make-token :eof NIL)
  "The end-of-file (EOF) token.")

;;; -------------------------------------------------------

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class applies itself to the onus of the significant
   objects' extraction from a piece of NeverGonna source code."
  (source    (error "Missing source.")
             :type      string
             :read-only T)
  (position  0
             :type      fixnum
             :read-only NIL)
  (character NIL
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Returns the LEXER's current character, or ``NIL'' if its source is
   exhausted, while concomitantly moving the LEXER's position cursor to
   the next character in its SOURCE, if possible, ere updating the
   current character.
   ---
   Please note that this operation returns the LEXER's character
   *before* the advance of its position cursor, not the new state's."
  (declare (type Lexer lexer))
  (the (or null character)
    (prog1
      (lexer-character lexer)
      (setf (lexer-character lexer)
        (when (array-in-bounds-p
                (lexer-source lexer)
                (1+ (lexer-position lexer)))
          (char
            (lexer-source lexer)
            (incf (lexer-position lexer))))))))

;;; -------------------------------------------------------

(defun lexer-move-to (lexer new-position)
  "Relocates the LEXER's position cursor to the NEW-POSITION, updates
   its current character, and returns no value."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (setf (lexer-position lexer) new-position)
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (lexer-source lexer) new-position)
      (char (lexer-source lexer) new-position)))
  (values))

;;; -------------------------------------------------------

(defun lexer-character-equals-p (lexer expected-character)
  "Determines whether the LEXER's current character is both non-``NIL''
   and equal to the EXPECTED-CHARACTER, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (the boolean
    (not (null
      (and
        (lexer-character lexer)
        (char= (lexer-character lexer) expected-character))))))

;;; -------------------------------------------------------

(defun lexer-character-satisfies-p (lexer predicate)
  "Determines whether the LEXER's current character is both non-``NIL''
   and satisfies the PREDICATE, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''.
   ---
   The PREDICATE must constitute a function that accepts a single
   character, the LEXER's current instance, and returns a generalized
   Boolean which if not ``NIL'' communicates its satisfaction, otherwise
   its disrespondency. The signature, as a corollary, conforms to:
     lambda (lexer-character) => generalized-boolean"
  (declare (type Lexer                    lexer))
  (declare (type (function (character) *) predicate))
  (the boolean
    (not (null
      (and
        (lexer-character lexer)
        (funcall predicate
          (lexer-character lexer)))))))

;;; -------------------------------------------------------

(defun lexer-characters-follow-p (lexer expected-characters)
  "Determines whether, proceeding from the current position into the
   LEXER's source, the consecutive characters replicate the
   EXPECTED-CHARACTERS, on confirmation moving the position cursor to
   the first location beyond the matching portion, while returning a
   ``boolean'' value of ``T'', otherwise retaining the original position
   assumed ere the invocation of this operation, while responding with
   ``NIL''."
  (declare (type Lexer  lexer))
  (declare (type string expected-characters))
  (let ((start-position (lexer-position lexer)))
    (declare (type fixnum start-position))
    (the boolean
      (loop
        for expected-character
          of-type character
          across  expected-characters
        if (lexer-character-equals-p lexer expected-character) do
          (lexer-advance lexer)
        else do
          (lexer-move-to lexer start-position)
          (return NIL)
        finally
          (return T)))))

;;; -------------------------------------------------------

(defun lexer-get-character-at (lexer position)
  "Returns the character located in the LEXER's source at the
   desiderated POSITION, or ``NIL'' if this location transcends the
   valid bournes."
  (declare (type Lexer  lexer))
  (declare (type fixnum position))
  (let ((source (lexer-source lexer)))
    (declare (type string source))
    (the (or null character)
      (when (array-in-bounds-p source position)
        (char source position)))))

;;; -------------------------------------------------------

(defun lexer-space-located-betwixt-hyphens-p (lexer)
  "Determines whether the character at the current position into the
   LEXER's source constitutes a space located betwixt two hyphens
   (\"-\") --- a case the same renders the spacing significant for the
   parsing stage --- and returns a ``boolean'' value of ``T'' upon
   confirmation, otherwise ``NIL''."
  (declare (type Lexer lexer))
  (flet ((character-at-offset-matches-p (offset expected-character)
          "Determines whether the character located by the OFFSET
           distance from the current position into the LEXER's source
           matches the EXPECTED-CHARACTER, returning on confirmation a
           ``boolean'' value of ``T'', otherwise ``NIL''."
          (declare (type fixnum    offset))
          (declare (type character expected-character))
          (let ((character-at-offset
                  (lexer-get-character-at lexer
                    (+ (lexer-position lexer) offset))))
            (declare (type (or null character) character-at-offset))
            (the boolean
              (not (null
                (and
                  character-at-offset
                  (char= character-at-offset expected-character))))))))
    (the boolean
      (and
        (lexer-character lexer)
        (character-at-offset-matches-p -1 #\-)
        (space-character-p (lexer-character lexer))
        (character-at-offset-matches-p +1 #\-)))))

;;; -------------------------------------------------------

(defun lexer-read-single-quote-string (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   string literal ensconced in single quote (\"'\"), and returns a
   ``:string'' token representation thereof."
  (declare (type Lexer lexer))
  (lexer-advance lexer)
  (the Token
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop do
          (case (lexer-character lexer)
            ((NIL)
              (error "Unterminated single-quote string at position ~d."
                (lexer-position lexer)))
            (#\'
              (lexer-advance lexer)
              (loop-finish))
            (otherwise
              (write-char (lexer-advance lexer) content))))))))

;;; -------------------------------------------------------

(defun lexer-read-double-quote-string (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   string literal ensconced in double quote ('\"'), and returns a
   ``:string'' token representation thereof."
  (declare (type Lexer lexer))
  (lexer-advance lexer)
  (the Token
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop do
          (case (lexer-character lexer)
            ((NIL)
              (error "Unterminated double-quote string at position ~d."
                (lexer-position lexer)))
            (#\"
              (lexer-advance lexer)
              (loop-finish))
            (otherwise
              (write-char (lexer-advance lexer) content))))))))

;;; -------------------------------------------------------

(defun lexer-read-word (lexer)
  "Commencing at the current position into the LEXER's source, reads an
   identifier and returns a ``:word'' token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :word
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop
          while (lexer-character-satisfies-p lexer
                  #'identifier-character-p)
          do (write-char (lexer-advance lexer) content))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Commencing at the current position into the LEXER's source, reads an
   unsigned integer number and returns a ``:number'' token
   representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (loop
            while (lexer-character-satisfies-p lexer #'digit-char-p)
            do    (write-char (lexer-advance lexer) digits)))))))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Consumes the character at the current position into the LEXER and
   returns a new token with the specified TOKEN-TYPE in conjunction with
   the consumed character as its value."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (make-token token-type
      (lexer-advance lexer))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent spaces and/or tabs, and returns
   no value."
  (declare (type Lexer lexer))
  (loop while (lexer-character-satisfies-p lexer #'space-character-p) do
    (lexer-advance lexer))
  (values))

;;; -------------------------------------------------------

(defun lexer-read-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, read
   a sequence of zero or more accolent spaces and/or tabs, and returns a
   ``:space'' token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :space
      (with-output-to-string (spaces)
        (declare (type string-stream spaces))
        (loop
          while (lexer-character-satisfies-p lexer #'space-character-p)
          do
            (write-char (lexer-character lexer) spaces)
            (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion the LEXER responds to every query with
   the same end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        +EOF-TOKEN+)
      
      ;; Returns spaces betwixt two hyphens or minus symbols ("-"), as
      ;; these ostend a twifaced haecceity: either acting as two
      ;; minus operands in succession, or as a comment ("--") inciter.
      ((lexer-space-located-betwixt-hyphens-p lexer)
        (lexer-read-spaces lexer))
      
      ;; Spaces not betwixt hyphens or minus signs ("-") may be
      ;; liberally disregarded.
      ((lexer-character-satisfies-p lexer #'space-character-p)
        (lexer-skip-spaces    lexer)
        (lexer-get-next-token lexer))
      
      ((lexer-character-equals-p lexer #\Newline)
        (lexer-read-symbol lexer :newline))
      
      ((lexer-character-satisfies-p lexer #'alpha-char-p)
        (lexer-read-word lexer))
      
      ((lexer-character-satisfies-p lexer #'digit-char-p)
        (lexer-read-number lexer))
      
      ((lexer-character-equals-p lexer #\')
        (lexer-read-single-quote-string lexer))
      
      ((lexer-character-equals-p lexer #\")
        (lexer-read-double-quote-string lexer))
      
      ((lexer-character-equals-p lexer #\+)
        (lexer-read-symbol lexer :plus))
      
      ((lexer-character-equals-p lexer #\-)
        (lexer-read-symbol lexer :minus))
      
      ((lexer-character-equals-p lexer #\*)
        (lexer-read-symbol lexer :times))
      
      ((lexer-character-equals-p lexer #\/)
        (lexer-read-symbol lexer :divided))
      
      ((lexer-character-equals-p lexer #\%)
        (lexer-read-symbol lexer :remainder))
      
      ((lexer-character-equals-p lexer #\^)
        (lexer-read-symbol lexer :power))
      
      ((lexer-characters-follow-p lexer "==")
        (make-token :equal "=="))
      
      ((lexer-characters-follow-p lexer "!=")
        (make-token :not-equal "!="))
      
      ((lexer-characters-follow-p lexer "<=")
        (make-token :less-or-equal "<="))
      
      ((lexer-characters-follow-p lexer ">=")
        (make-token :greater-or-equal ">="))
      
      ((lexer-character-equals-p lexer #\<)
        (lexer-read-symbol lexer :less-than))
      
      ((lexer-character-equals-p lexer #\>)
        (lexer-read-symbol lexer :greater-than))
      
      ((lexer-characters-follow-p lexer "&&")
        (lexer-read-symbol lexer :logical-and))
      
      ((lexer-characters-follow-p lexer "||")
        (lexer-read-symbol lexer :logical-or))
      
      ((lexer-character-equals-p lexer #\!)
        (lexer-read-symbol lexer :logical-not))
      
      ((lexer-character-equals-p lexer #\()
        (lexer-read-symbol lexer :left-parenthesis))
      
      ((lexer-character-equals-p lexer #\))
        (lexer-read-symbol lexer :right-parenthesis))
      
      ;; Any symbol may occur in a comment, thus homologate its
      ;; admission.
      (T
        (lexer-read-symbol lexer :character)))))
