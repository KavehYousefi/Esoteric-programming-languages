;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Instructions
;; ============
;; 
;; == INSTRUCTIONS DIFFERENTIATE BY THEIR OPERANDS' TALLY ==
;; The following definitions hold:
;;   
;;   Four operands: a b c d
;;     
;;     if a = "IN" then
;;       cells[b] <- getASCIICodeForCharacter(input)
;;     else if a = "OUT" then
;;       print asciiCharacterForCode(cells[b])
;;     else if a > 0 then
;;       cells[b] <- cells[b] + a
;;     else
;;       cells[b] <- cells[b] - a
;;     end if
;;     
;;     if cells[b] = d then
;;       goto line c
;;     end if
;;   
;;   ---------------------------------------
;;   
;;   Three operands: a b c
;;     
;;     if a = "IN" then
;;       cells[b] <- getASCIICodeForCharacter(input)
;;     else if a = "OUT" then
;;       print asciiCharacterForCode(cells[b])
;;     else if a > 0 then
;;       cells[b] <- cells[b] + a
;;     else
;;       cells[b] <- cells[b] - a
;;     end if
;;     
;;     if cells[b] != 0 then
;;       goto line c
;;     end if
;;   
;;   ---------------------------------------
;;   
;;   Two operands: a b
;;     
;;     if a = "IN" then
;;       cells[b] <- getASCIICodeForCharacter(input)
;;     else if a = "OUT" then
;;       print asciiCharacterForCode(cells[b])
;;     else if a > 0 then
;;       cells[b] <- cells[b] + a
;;     else
;;       cells[b] <- cells[b] - a
;;     end if
;; 
;; == LINE NUMBERING ==
;; Line numbering is surmised to start with the index one (1), excluding
;; from the account empty lines.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-23
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Idego"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' defines a hash table of zero or more entries,
   each key of which conforms to the KEY-TYPE and associates with a
   value of the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines a tape-like arrangement of cells, each
   amenable to a signed integer index, while storing a scalar value of
   the same type."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant constituent of an
   Idego program, obtained during the lexical analyzation process."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN is of the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun sign-character-p (character)
  "Checks whether the CHARACTER represents a numerical sign, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (find character "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Idego source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class discerns and returns the tokens entailed in a
     piece of Idego source code."))

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
  "Creates and returns a new ``Lexer'' for analyzing the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next position in its source, if possible,
   updates its current character, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-newlines (lexer)
  "Starting at the LEXER's current position, reads a sequence of one or
   more adjacent newline characters and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :newlines
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop while (and character (char= character #\Newline)) do
            (write-char character content)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-spaces (lexer)
  "Starting at the LEXER's current position, reads a sequence of one or
   more adjacent space characters and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :spaces
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop while (and character (space-character-p character)) do
            (write-char character content)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the LEXER's current position, reads a signed integer
   number and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (when (and character (sign-character-p character))
              (write-char character digits)
              (lexer-advance lexer))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position, reads an identifier and
   returns a token representation thereof.
   ---
   If the consumed identifier does not represent a recognized keyword,
   an error of unspecified type is signaled."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((identifier ""))
      (declare (type string identifier))
      (setf identifier
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop while (and character (alpha-char-p character)) do
            (write-char character content)
            (lexer-advance lexer))))
      (the Token
        (cond
          ((string= identifier "IN")
            (make-token :in identifier))
          ((string= identifier "OUT")
            (make-token :out identifier))
          (T
            (error "Invalid identifier: ~s." identifier)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to each query with a
   fresh token of the type ``:eof'', signifying the end of its code."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((char= character #\Newline)
          (lexer-read-newlines lexer))
        
        ((space-character-p character)
          (lexer-read-spaces lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((sign-character-p character)
          (lexer-read-number lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        (T
          (error "Invalid character '~c' at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type value)))
  "The ``Operand'' class represents an argument to an instruction."
  (type  (error "Missing operand type.") :type keyword)
  (value NIL                             :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor create-instruction (type operands)))
  "The ``Instruction'' class encapsulates the information necessary to
   represent an instruction in the Idego programming language."
  (type     (error "Missing instruction type.")
            :type keyword)
  (operands NIL
            :type (list-of Operand)))

;;; -------------------------------------------------------

(defun make-instruction (operands)
  "Creates and returns a new ``Instruction'' suitable to represent the
   OPERANDS.
   ---
   An error is signaled if the tally of OPERANDS does not render them
   eligible for an instruction's representation."
  (declare (type (list-of Operand) operands))
  (the Instruction
    (case (length operands)
      (4 (create-instruction :jump-if-equals   operands))
      (3 (create-instruction :jump-if-not-zero operands))
      (2 (create-instruction :do-not-jump      operands))
      (otherwise
        (error "Invalid operand count: ~d. ~
                Cannot build an instruction for such."
          (length operands))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The lexer responsible for supplying the tokens.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recent token obtained from the LEXER."))
  (:documentation
    "The ``Parser'' class constitutes an entity responsible for building
     by assemblage from tokens a vector of instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which receives its tokens from
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token matches the
   EXPECTED-TOKEN-TYPE, on confirmation loading into the PARSER the next
   token from its internally managed lexer and returning the modified
   PARSER, otherwise signaling an error of unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (if (token-type-p current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but encountered the ~
              token ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-spaces (parser)
  "Skips zero or more adjacent space tokens using the PARSER and returns
   the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop while (token-type-p current-token :spaces) do
      (parser-eat parser :spaces)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-whitespaces (parser)
  "Skips zero or more adjacent space or newline tokens using the PARSER
   and returns the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop do
      (case (token-type current-token)
        (:spaces
          (parser-eat parser :spaces))
        (:newlines
          (parser-eat parser :newlines))
        (otherwise
          (loop-finish)))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-operand (parser)
  "Parses an operand using the PARSER and returns an ``Operand'' object
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (the Operand
      (case (token-type current-token)
        
        (:number
          (prog1
            (make-operand :number (token-value current-token))
            (parser-eat parser :number)))
        
        (:in
          (prog1
            (make-operand :input (token-value current-token))
            (parser-eat parser :in)))
        
        (:out
          (prog1
            (make-operand :output (token-value current-token))
            (parser-eat parser :out)))
        
        (otherwise
          (error "Invalid operand token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses a line representing an instruction using the PARSER and
   returns the thus generated ``Instruction'' object."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((operands NIL))
      (declare (type (list-of Operand) operands))
      (loop do
        (case (token-type current-token)
          (:spaces
            (parser-skip-spaces parser))
          ((:newlines :eof)
            (loop-finish))
          (otherwise
            (push (parser-parse-operand parser) operands))))
      (the Instruction
        (make-instruction (nreverse operands))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses an Idego program using the PARSER and returns a vector of
   instructions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((instructions NIL))
      (declare (type (list-of Instruction) instructions))
      (loop do
        (parser-skip-whitespaces parser)
        (case (token-type current-token)
          (:eof
            (loop-finish))
          (otherwise
            (push (parser-parse-instruction parser) instructions))))
      (the (simple-array Instruction (*))
        (coerce (nreverse instructions)
          '(simple-array Instruction (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          (vector Instruction *)
    :documentation "The instructions to process.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer designating the
                    CURRENT-INSTRUCTION in the INSTRUCTIONS vector.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null Instruction)
    :documentation "The instruction referenced by the instruction
                    pointer IP in the INSTRUCTIONS vector.")
   (memory
    :initarg       :memory
    :initform      (make-hash-table :test #'eql)
    :type          memory
    :documentation "Represents the memory or tape, compact of cells,
                    each storing a signed integer datum."))
  (:documentation
    "The ``Interpreter'' class processes a sequence of instructions in
     order to endow effect to the same.
     ---
     Typically, the interpreter receives its instruction vector from a
     parser, which itself has been a lexer's recipient."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' which processes the
   INSTRUCTIONS."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-advance-ip (interpreter)
  "Advances the INTERPRETER's internal instruction pointer to the next
   instruction, updates the current instruction, and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions (1+ ip))
        (aref instructions (incf ip)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-move-ip-to (interpreter new-position)
  "Moves the INTERPRETER's internal instruction pointer to the one-based
   NEW-POSITION, updates its current instruction, and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-position))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (setf ip (1- new-position))
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-handle-operands-ab (interpreter a b)
  "Processes the first two operands, A and B, of an instruction
   utilizing the INTERPRETER and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type Operand     a))
  (declare (type Operand     b))
  (let ((cell-index  (operand-value b)))
    (declare (type integer cell-index))
    
    (with-slots (memory) interpreter
      (declare (type memory memory))
      
      (case (operand-type a)
        ;; a = "IN"?
        (:input
          (format T "~&Please input an ASCII character: ")
          (let ((input (read-char)))
            (declare (type character input))
            (clear-input)
            (setf (gethash cell-index memory)
                  (char-code input))))
        
        ;; a = "OUT"?
        (:output
          (format T "~c"
            (code-char (gethash cell-index memory 0))))
        
        ;; a is an integer?
        (:number
          (incf (gethash cell-index memory 0)
                (operand-value a)))
        
        ;; Invalid operand a.
        (otherwise
          (error "Invalid operand 'a': ~s." a)))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instructions contained in the INTERPRETER and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction memory) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (declare (type memory                 memory))
    
    (loop while current-instruction do
      (case (instruction-type current-instruction)
        ;; a b c d
        (:jump-if-equals
          (destructuring-bind (a b c d)
              (instruction-operands current-instruction)
            (declare (type Operand a))
            (declare (type Operand b))
            (declare (type Operand c))
            (declare (type Operand d))
            (let ((cell-index  (operand-value b))
                  (jump-target (operand-value c))
                  (test-value  (operand-value d)))
              (declare (type integer cell-index))
              (declare (type integer jump-target))
              (declare (type integer test-value))
              
              (interpreter-handle-operands-ab interpreter a b)
              
              ;; Handle goto.
              (if (= (gethash cell-index memory 0) test-value)
                (interpreter-move-ip-to interpreter jump-target)
                (interpreter-advance-ip interpreter)))))
        
        ;; a b c
        (:jump-if-not-zero
          (destructuring-bind (a b c)
              (instruction-operands current-instruction)
            (declare (type Operand a))
            (declare (type Operand b))
            (declare (type Operand c))
            (let ((cell-index  (operand-value b))
                  (jump-target (operand-value c)))
              (declare (type integer cell-index))
              (declare (type integer jump-target))
              
              (interpreter-handle-operands-ab interpreter a b)
              
              ;; Handle goto.
              (if (not (zerop (gethash cell-index memory 0)))
                (interpreter-move-ip-to interpreter jump-target)
                (interpreter-advance-ip interpreter)))))
        
        ;; a b
        (:do-not-jump
          (destructuring-bind (a b)
              (instruction-operands current-instruction)
            (declare (type Operand a))
            (declare (type Operand b))
            (interpreter-handle-operands-ab interpreter a b))
          (interpreter-advance-ip interpreter))
        
        (otherwise
          (error "Invalid instruction: ~s at position ~d."
            current-instruction ip)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Idego (code)
  "Interprets the piece of Idego CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Idego
  "
  72 1
  OUT 1
  29 1
  OUT 1
  7 1
  OUT 1
  OUT 1
  3 1
  OUT 1
  -111 1
  44 1
  OUT 1
  -12 1
  OUT 1
  55 1
  OUT 1
  24 1
  OUT 1
  3 1
  OUT 1
  -6 1
  OUT 1
  -8 1
  OUT 1
  -100 1
  33 1
  OUT 1
  ")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Pseudocode, with line numbers as sinistral apostilles:
;;   
;;   1 | cell[1] <- get user input      { Input must be either '0' or '1'. }
;;   2 | if cell[1] = 48, go to line 5  { Jump to print cell[1] = '0' on zero input. }
;;   3 | print cell[1]                  { Print cell[1] = '1'. }
;;   4 | if cell[1] = 49, go to line 3  { Return to print cell[1] = '1'. }
;;   5 | print cell[1]                  { Print cell[1] = '0' and terminate. }
(interpret-Idego
  "IN 1
   0 1 5 48
   OUT 1
   0 1 3 49
   OUT 1")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which ceases on an input of zero.
(interpret-Idego
  "IN  1
   OUT 1 1")
