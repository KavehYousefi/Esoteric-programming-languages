;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the lexical analyzer, or lexer, the parcery of
;; its purpose realized in the extraction of significant objects from
;; a piece of ``` source code in its original state as a string, whence
;; are produced and delivered ``Token'' encapsulations thereof.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Lexer) (values)) update-lexer-character))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source for the lexer.")
    :type          string
    :documentation "The piece of `` source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class serves in the extraction of significant
     objects from a piece of `` source code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Sets the LEXER's internally managed character to the first one in
   its source and returns no value."
  (declare (type Lexer lexer))
  (update-lexer-character lexer)
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' nuncupated to the `` SOURCE
   code's analyzation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun current-character-equals-p (lexer expected-character)
  "Determines whether the LEXER's current character equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''.
   ---
   An exhausted lexer, such maintains a ``NIL''-valued current
   character, always responds with ``NIL'' without the
   EXPECTED-CHARACTER's prior interrogation."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the boolean
      (get-boolean-value-of
        (and character
             (char= character expected-character))))))

;;; -------------------------------------------------------

(defun update-lexer-character (lexer)
  "Updates the LEXER's internally managed current character and
   returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-character (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position) lexer
    (declare (type string source))
    (declare (type fixnum position))
    (when (array-in-bounds-p source position)
      (incf position)))
  (update-lexer-character lexer)
  (values))

;;; -------------------------------------------------------

(defun read-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes a sequence of zero or more accolent whitespaces, and returns
   a ``:whitespace'' token representation of its aggregate."
  (declare (type Lexer lexer))
  (the Token
    (make-token :whitespace
      (with-output-to-string (whitespaces)
        (declare (type string-stream whitespaces))
        (with-slots (character) lexer
          (declare (type (or null character) character))
        (loop
          while
            (and character
                 (whitespace-character-p character))
          do
            (write-char character whitespaces)
            (advance-to-next-character lexer)))))))

;;; -------------------------------------------------------

(defun count-backticks (lexer)
  "Proceeding from the current position into the LEXER's source,
   tallies the number of accolent backtick (\"`\") and returns this
   account."
  (declare (type Lexer lexer))
  (the (integer 0 *)
    (loop
      while (current-character-equals-p lexer #\`)
      do    (advance-to-next-character  lexer)
      count 1)))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned integer number and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (with-slots (character) lexer
            (declare (type (or null character) character))
            (when (and character (sign-character-p character))
              (write-char character digits)
              (advance-to-next-character lexer))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (advance-to-next-character lexer))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (the Token
      (case character
        ((NIL)
          (make-eof-token))
        ((#\Newline #\Space #\Tab)
          (read-whitespaces lexer))
        (#\`
          (let ((number-of-backticks (count-backticks lexer)))
            (declare (type (integer 0 *) number-of-backticks))
            (case number-of-backticks
              (1 (make-token :single-backtick 1))
              (2 (make-token :double-backtick 2))
              (otherwise
                (error "Invalid number of backticks: ~d."
                  number-of-backticks)))))
        (#\#
          (prog1
            (make-token :number-sign character)
            (advance-to-next-character lexer)))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+)
          (read-number lexer))
        (otherwise
          (error "Invalid character \"~c\" at position ~d."
            character position))))))
