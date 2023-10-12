;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the realization of the lexical analyzer, also
;; nevened a lexer or scanner, whose bailiwick embraces the detection
;; and extract of significant objects from a piece of Celum source code
;; provided as a sequence of string lines.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun label-name-character-p (character)
  "Checks whether the CHARACTER represents a label name constituent,
   returning on confirmation a ``boolean'' value of ``T'', otehrwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (and
        (not (char= character #\:))
        (<= 33 (char-code character) 126))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((line
    :initarg       :line
    :initform      "The currently processed Celum code line."
    :type          string
    :documentation "The line to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The index into the processed LINE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the POSITION into the LINE."))
  (:documentation
    "The ``Lexer'' class applies itself to the wike of a scanning at a
     time a single line of Celum code and returning a ``Line'' object
     representation thereof."))

;;; -------------------------------------------------------

(defun make-lexer ()
  "Creates and returns a new ``Lexer'' which does not operate on a line
   yet."
  (the Lexer (make-instance 'Lexer)))

;;; -------------------------------------------------------

(defun lexer-set-line (lexer new-line)
  "Sets the LEXER's source to the NEW-LINE, resets all appertaining
   state, and returns the modified LEXER."
  (declare (type Lexer  lexer))
  (declare (type string new-line))
  (with-slots (line position character) lexer
    (declare (type string              line))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf line     new-line)
    (setf position 0)
    (setf character
      (when (array-in-bounds-p line position)
        (char line position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next character in its
   source, if possible, updates the LEXER's state, and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (line position character) lexer
    (declare (type string              line))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p line (1+ position))
        (char line (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-eat-colon (lexer)
  "Checks whether the current LEXER character represents the colon
   (\":\"), on confirmation moving the position cursor beyond the same
   and returning the modified LEXER; otherwise, an error of an
   unspecified type is signaled."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (if (and character (char= character #\:))
      (lexer-advance lexer)
      (error "Expected a colon but encountered ~s at position ~d."
        character position)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-expect-hexadecimal-digit (lexer)
  "Checks whether the LEXER's current character represents a hexadecimal
   digit, on confirmation returning the LEXER without advancing its
   position cursor; otherwise, an error of an unspecified type is
   signaled."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (unless (and character (digit-char-p character 16))
      (error "Expected a hexadecimal digit but encountered ~s ~
              at position ~d"
        character position)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-expect-end-of-line (lexer)
  "Checks whether the LEXER's line is exhausted, on confirmation
   returning the LEXER without advancing its position cursor; otherwise,
   an error of an unspecified type is signaled."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (when character
      (error "Expected an end of line but encountered ~s at ~
              position ~d"
        character position)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Starting at the current position in the LEXER's source, skips a
   sequence of zero or more adjacent spaces and returns the modified
   LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-prefix (lexer)
  "Reads the line prefix, that is, the first non-space character, using
   the LEXER, either returning a bit (0 or 1) for an operation line, or
   the ``NIL'' value to designate a comment line (\"C\" or \"c\");
   potentially signaling an error of an unspecified type if no valid
   prefix could be ascertained."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (the (or null bit)
      (case character
        ((#\0 #\1)
          (prog1
            (digit-char-p character 2)
            (lexer-advance     lexer)
            (lexer-skip-spaces lexer)
            (lexer-eat-colon   lexer)))
        ((#\c #\C)
          (prog1
            NIL
            (lexer-advance     lexer)
            (lexer-skip-spaces lexer)
            (lexer-eat-colon   lexer)))
        (otherwise
          (error "Expected a prefix but encountered \"~c\" at ~
                  position ~d."
            character position))))))

;;; -------------------------------------------------------

(defun lexer-read-label-name (lexer)
  "Reads a label identifier from the LEXER and returns a string
   representation thereof.
   ---
   This operation follows the Celum donet rule
     labelName := labelCharacter , { labelCharacter } ;"
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the string
      (with-output-to-string (label-name)
        (declare (type string-stream label-name))
        (loop
          while (and character (label-name-character-p character))
          do
            (write-char character label-name)
            (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-read-hexadecimal-pair (lexer)
  "Starting at the LEXER's current position into its source, reads two
   adjacent hexadecimal digits and returns their decimal value."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the (integer 0 255)
      (parse-integer
        (with-output-to-string (hexadecimal-digits)
          (declare (type string-stream hexadecimal-digits))
          (lexer-expect-hexadecimal-digit lexer)
          (write-char character hexadecimal-digits)
          (lexer-advance lexer)
          (lexer-expect-hexadecimal-digit lexer)
          (write-char character hexadecimal-digits)
          (lexer-advance lexer))
        :radix 16))))

;;; -------------------------------------------------------

(defun lexer-read-commands (lexer)
  "Reads a sequence of commands from the LEXER and returns these in a
   list."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (let ((commands NIL))
      (declare (type command-list commands))
      (flet ((collect-command (constructor &rest arguments)
              "Invokes the ``Command'' subclass CONSTRUCTOR with the
               ARGUMENTS procured as rest parameters, inserts the thus
               instantiated ``Command'' object at the COMMANDS list's
               front, and returns no value."
              (declare (type function     constructor))
              (declare (type (list-of T) arguments))
              (push (apply constructor arguments) commands)
              (values)))
        (loop with jump-command-encountered-p of-type boolean = NIL do
          (lexer-skip-spaces lexer)
          
          (cond
            ((null character)
              (loop-finish))
            
            ((space-character-p character)
              (lexer-skip-spaces character))
            
            (jump-command-encountered-p
              (error "An \"!\" command has been processed before and ~
                      should be the last constituent of the line, ~
                      but the character ~s has been encountered."
                character))
            
            ((digit-char-p character 16)
              (collect-command
                #'make-cellular-automaton-command
                :rule (lexer-read-hexadecimal-pair lexer)))
            
            ((char= character #\{)
              (collect-command #'make-search-prefix-above-command)
              (lexer-advance lexer))
            
            ((char= character #\})
              (collect-command #'make-search-prefix-below-command)
              (lexer-advance lexer))
            
            ((char= character #\i)
              (collect-command #'make-input-command)
              (lexer-advance lexer))
            
            ((char= character #\o)
              (collect-command #'make-output-command)
              (lexer-advance lexer))
            
            ((char= character #\?)
              (collect-command #'make-skip-command)
              (lexer-advance lexer))
            
            ((char= character #\!)
              (lexer-advance     lexer)
              (lexer-skip-spaces lexer)
              (collect-command #'make-search-label-command
                :name (lexer-read-label-name lexer))
              (setf jump-command-encountered-p T))
            
            (T
              (error "Invalid command token: \"~c\" at position ~d."
                character position))))
      
      (the command-list
        (nreverse commands))))))

;;; -------------------------------------------------------

(defun lexer-finished-p (lexer)
  "Checks whether the LEXER has completely processed its internally
   managed line, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (null (slot-value lexer 'character))))
