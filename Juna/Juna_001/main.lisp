;; Author: Kaveh Yousefi
;; Date:   2022-02-21
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Juna"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, the keys of which conform to KEY-TYPE and the values to the
   VALUE-TYPE, both defaulting to ``T''."
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including ``format'' and ``write''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero, but unbounded along the positive axis."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction-type'' enumerates the recognized types of
   instructions, encompassing as an adscititous member a sentinel for
   an uninitialized or undefined object of this ilk."
  '(member
    :undefined  ;; Uninitialized/Unknown type.
    :nand       ;; L a b Y N
    :input      ;; L . b Y N
    :output))   ;; L a . Y N

;;; -------------------------------------------------------

(deftype operand-type ()
  "The ``operand-type'' enumerates the recognized types of operands,
   employed as arguments in instructions."
  '(member :integer :dot))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "A token avails as significant portion's encapsulation, produced by a
   prior analyzation of a piece of Juna source code."
  (type  (error "No token type specified.") :type keyword)
  (value NIL                                :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN type conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "No lexer source specified.")
    :type          string
    :documentation "The piece of Juna code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION of the
                    SOURCE."))
  (:documentation
    "Responsible for the analyzation of a piece of Juna code, the
     ``Lexer'' produces a sequence of tokens in the agency of its
     division into significant portions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (and (>= position 0)
                 (< position (length source)))
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' ordained to analyze the Juna
   SOURCE code."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun space-character-p (character)
  "Checks whether the CHARACTER constitutes a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next charaacter, if possible, and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (< position (1- (length source)))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Starting at the current position in the LEXER, skips zero or more
   adjacent spaces or tabs, relocating the position cursor to the first
   non-space character, and finally returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current position in the LEXER, skips all characters
   until an occurrence of a newline or the end of file (EOF) ceases its
   operation, finally returning the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop until (or (null character) (char= character #\Newline)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-integer (lexer)
  "Starting at the current position in the LEXER, consumes one or more
   digits and returns a token containing the parsed integer value."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :integer
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-dot (lexer)
  "Starting at the current position in the LEXER, expects a dot ('.')
   character, consumes the same, and returns its token representation."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (prog1
        (make-token :dot character)
        (lexer-advance lexer)))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon the exhaustion of the LEXER's source, this function constantly
   returns a new end-of-file (EOF) token upon each invocation."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (lexer-skip-spaces    lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\Newline)
          (prog1
            (make-token :newline character)
            (lexer-advance lexer)))
        
        ((char= character #\#)
          (lexer-skip-comment   lexer)
          (lexer-get-next-token lexer))
        
        ((digit-char-p character)
          (lexer-read-integer lexer))
        
        ((char= character #\.)
          (lexer-read-dot lexer))
        
        (T
          (error "Invalid character '~a' at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type value)))
  "The ``Operand'' class an object in the agency of an instruction's
   arguments."
  (type  (error "No operand type specified.") :type operand-type)
  (value NIL                                  :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor create-instruction (line-number)))
  "An ``Instruction'' reifies the concept of an operation."
  (type        :undefined :type instruction-type)
  ;; The instruction's line number "L".
  (line-number 0          :type non-negative-integer)
  ;; Index (address) of the register "a".
  (register-a  NIL        :type (or null Operand))
  ;; Index (address) of the register "b".
  (register-b  NIL        :type (or null Operand))
  ;; Target line number "Y".
  (line-Y      NIL        :type (or null Operand))
  ;; Target line number "N".
  (line-N      NIL        :type (or null Operand)))

;;; -------------------------------------------------------

(defun make-instruction (&key (line-number 0)
                              (register-a  (make-operand :integer 0))
                              (register-b  (make-operand :integer 0))
                              (line-Y      (make-operand :dot     0))
                              (line-N      (make-operand :dot     0)))
  "Creates and returns a new ``Instruction'' located at the LINE-NUMBER,
   operating on the operands REGISTER-A, REGISTER-B, LINE-Y, and LINE-N."
  (declare (type non-negative-integer line-number))
  (declare (type Operand              register-a))
  (declare (type Operand              register-b))
  (declare (type Operand              line-Y))
  (declare (type Operand              line-N))
  (let ((instruction (create-instruction line-number)))
    (declare (type Instruction instruction))
    (setf (instruction-register-a instruction) register-a)
    (setf (instruction-register-b instruction) register-b)
    (setf (instruction-line-Y     instruction) line-Y)
    (setf (instruction-line-N     instruction) line-N)
    ;; Determine the instruction type from register-a and register-b.
    (setf (instruction-type instruction)
      (cond
        ;; L a b Y N
        ((and (eq (operand-type register-a) :integer)
              (eq (operand-type register-b) :integer))
          :nand)
        ;; L . b Y N
        ((and (eq (operand-type register-a) :dot)
              (eq (operand-type register-b) :integer))
          :input)
        ;; L a . Y N
        ((and (eq (operand-type register-a) :integer)
              (eq (operand-type register-b) :dot))
          :output)
        ;; L . . Y N => Invalid.
        (T
          (error "Invalid operands: a = ~s, b = ~s."
            register-a register-b))))
    (the Instruction instruction)))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Instruction) stream)
  (declare (type Instruction instruction))
  (declare (type destination stream))
  (format stream "~&Instruction(~a, ~d ~a ~a ~a ~a)"
    (instruction-type        instruction)
    (instruction-line-number instruction)
    (operand-value (instruction-register-a instruction))
    (operand-value (instruction-register-b instruction))
    (operand-value (instruction-line-Y     instruction))
    (operand-value (instruction-line-N     instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "No lexer supplied for the parser.")
    :type          Lexer
    :documentation "The lexer responsible for supplying the tokens.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :EOF NIL)
    :type          Token
    :documentation "The last token requested from the LEXER."))
  (:documentation
    "Encumbered with the responsibility of assembling from a sequence
     of tokens an instruction vector, the ``Parser'' class acts as a
     mediator betwixt the lexer and the interpreter."))

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
  "Creates and returns a new ``Parser'' consuming its tokens from the
   specified LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation requesting the next token from
   the PARSER's lexer and storing it, ere returning the modified PARSER;
   otherwise an error is signaled."
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (if (token-type-p current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but received ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-optional-empty-lines (parser)
  "Skips zero or more empty lines in immediate adjacency, and returns
   the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop while (token-type-p current-token :newline) do
      (parser-eat parser :newline)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-line-number (parser)
  "Parses a line number using the PARSER, and returns it as a
   non-negative integer."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((line-number-token current-token))
      (declare (type Token line-number-token))
      (parser-eat parser :integer)
      (the non-negative-integer (token-value line-number-token)))))

;;; -------------------------------------------------------

(defun parser-parse-operand (parser)
  "Parses an operand using the PARSER, and returns a new ``Operand''
   object."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Operand
      (case (token-type current-token)
        (:integer
          (prog1
            (make-operand :integer (token-value current-token))
            (parser-eat parser :integer)))
        (:dot
          (prog1
            (make-operand :dot (token-value current-token))
            (parser-eat parser :dot)))
        (otherwise
          (error "Invalid operand token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses a single instruction line using the PARSER, and returns a new
   ``Instruction'' instance in the agency of its representation."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Instruction
      (prog1
        (make-instruction
          :line-number (parser-parse-line-number parser)
          :register-a  (parser-parse-operand     parser)
          :register-b  (parser-parse-operand     parser)
          :line-Y      (parser-parse-operand     parser)
          :line-N      (parser-parse-operand     parser))
        ;; Check whether the instruction line contains invalid trailing
        ;; content.
        (case (token-type current-token)
          (:newline
            (parser-skip-optional-empty-lines parser))
          (:eof
            NIL)
          (otherwise
            (error "Expected a newline or EOF following the ~
                    instruction, but encountered ~s."
              current-token)))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the tokens consumed by the PARSER and returns a vector of
   instructions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the (simple-array Instruction (*))
      (coerce
        (loop
          until   (token-type-p current-token :eof)
          do      (parser-skip-optional-empty-lines parser)
          collect (parser-parse-instruction parser))
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logical-not (bit)
  "Returns the logical NOT of the bit."
  (declare (type bit bit))
  (the bit (- 1 bit)))

;;; -------------------------------------------------------

(defun logical-nand (left-bit right-bit)
  "Returns the logical NAND of the LEFT-BIT and RIGHT-BIT."
  (declare (type bit left-bit))
  (declare (type bit right-bit))
  (the bit (logical-not (logand left-bit right-bit))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-instructions-in-table (instruction-vector
                                      instruction-table)
  "Transfers the instructions contained in the INSTRUCTION-VECTOR into
   the INSTRUCTION-TABLE by mapping each instruction's line number as a
   key to the instruction object itself, and returns no value."
  (declare (type (simple-array Instruction (*)) instruction-vector))
  (declare (type (hash-table-of non-negative-integer Instruction)
                 instruction-table))
  (loop
    for instruction
      of-type Instruction
      across  instruction-vector
    for line-number
      of-type non-negative-integer
      =       (instruction-line-number instruction)
    do
      (if (gethash line-number instruction-table)
        (error "Duplicate line number: ~d." line-number)
        (setf (gethash line-number instruction-table) instruction)))
  (values))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of non-negative-integer Instruction)
    :documentation "A representation of the instruction vector in the
                    form a hash table for efficient access by line
                    numbers.")
   (instruction-pointer
    :initarg       :instruction-pointer
    :initform      0
    :type          fixnum
    :documentation "The CURRENT-INSTRUCTION's line number.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null current-instruction)
    :documentation "The instruction at the INSTRUCTION-POINTER line.")
   (registers
    :initarg       :registers
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of non-negative-integer bit)
    :documentation "Maps the register indices to their values utilizing
                    a sparse representation.
                    ---
                    The registers' enumeration proceeds by means of
                    non-negative integer indices, with each register
                    storing a single bit."))
  (:documentation
    "The ``Interpreter'' class administers to an instruction sequence
     actual effect."))

;;; -------------------------------------------------------

(defun make-interpreter (instruction-vector)
  "Creates and returns a new ``Interpreter'' dedicated to the processing
   of the INSTRUCTION-VECTOR."
  (declare (type (simple-array Instruction (*)) instruction-vector))
  (let ((interpreter (make-instance 'Interpreter)))
    (declare (type Interpreter interpreter))
    (collect-instructions-in-table instruction-vector
      (slot-value interpreter 'instructions))
    (the Interpreter interpreter)))

;;; -------------------------------------------------------

(defun interpreter-find-entry-point (interpreter)
  "Attempts to set the INTERPRETER's instruction pointer to the first
   instruction line, index with zero, returning on success the modified
   INTERPRETER, otherwise signaling an error."
  (declare (type Interpreter interpreter))
  (with-slots (instructions instruction-pointer current-instruction)
      interpreter
    (declare (type (hash-table-of non-negative-integer Instruction)
                   instructions))
    (declare (type fixnum                instruction-pointer))
    (declare (type (or null Instruction) current-instruction))
    (multiple-value-bind (entry-instruction contains-entry-index-p)
        (gethash 0 instructions)
      (declare (type (or null Instruction) entry-instruction))
      (declare (type T                     contains-entry-index-p))
      (cond
        (contains-entry-index-p
          (setf instruction-pointer 0)
          (setf current-instruction entry-instruction))
        (T
          (error "No line with number 0 found.")))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-jump-to (interpreter target)
  (declare (type Interpreter interpreter))
  (declare (type Operand     target))
  (with-slots (instruction-pointer current-instruction) interpreter
    (declare (type fixnum                instruction-pointer))
    (declare (type (or null Instruction) current-instruction))
    (case (operand-type target)
      (:integer
        (setf instruction-pointer (operand-value target))
        (setf current-instruction
              (gethash instruction-pointer
                (slot-value interpreter 'instructions)))
        (unless current-instruction
          (error "No line with number ~d found." instruction-pointer)))
      
      (:dot
        (setf current-instruction NIL))
      
      (otherwise
        (error "Invalid jump to operand: ~s." target))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instructions governed by the INTERPRETER's castaldy,
   and returns the INTERPRETER."
  (declare (type Interpreter interpreter))
  ;; Locate the instruction pointer to the line with the index 0.
  (interpreter-find-entry-point interpreter)
  
  (with-slots (current-instruction registers) interpreter
    (declare (type (or null Instruction) current-instruction))
    (declare (type (hash-table-of non-negative-integer bit) registers))
    
    (labels
        ((register-at (operand)
          "Returns the value of the register addressed by the OPERAND."
          (declare (type Operand operand))
          (the bit (gethash (operand-value operand) registers 0)))
         
         ((setf register-at) (new-value operand)
          "Sets the value of the register addressed by the OPERAND to
           the NEW-VALUE."
          (declare (type Operand operand))
          (declare (type bit     new-value))
          (setf (gethash (operand-value operand) registers 0) new-value)
          (values))
         
         (register-a ()
          "Returns the value of the register amenable to the
           CURRENT-INSTRUCTION's register 'a'."
          (the bit
            (register-at
              (instruction-register-a current-instruction))))
         
         (register-b ()
          "Returns the value of the register amenable to the
           CURRENT-INSTRUCTION's register 'b'."
          (the bit
            (register-at
              (instruction-register-b current-instruction))))
         
         ((setf register-b) (new-value)
          "Sets the value of the register amenable to the
           CURRENT-INSTRUCTION's register 'b' to the NEW-VALUE and
           returns no value."
          (declare (type bit new-value))
          (setf
            (register-at (instruction-register-b current-instruction))
            new-value)
          (values)))
      
      (loop while current-instruction do
        (case (instruction-type current-instruction)
          (:nand
            (setf (register-b)
                  (logical-nand (register-a) (register-b)))
            (interpreter-jump-to interpreter
              (if (zerop (register-b))
                (instruction-line-Y current-instruction)
                (instruction-line-N current-instruction))))
          
          (:input
            (format T "~&Please input a bit: ")
            (let ((input (read)))
              (declare (type bit input))
              (setf (register-b) input))
            (interpreter-jump-to interpreter
              (if (zerop (register-b))
                (instruction-line-Y current-instruction)
                (instruction-line-N current-instruction))))
          
          (:output
            (write (register-a))
            (interpreter-jump-to interpreter
              (if (zerop (register-a))
                (instruction-line-Y current-instruction)
                (instruction-line-N current-instruction))))
          
          (otherwise
            (error "Invalid instruction: ~s." current-instruction))))))
    
    (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Juna (code)
  "Interprets the piece of Juna CODE and returns no value."
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

;; Truth-machine.
(interpret-Juna
  "0 . 0 1 2 #Set register 0 to inputted value. If it's 0, jump to line 1, else jump to line 2
   1 0 . . . #Print register 0 (value 0), then halt
   2 0 . 2 2 #Print register 0 (value 1), then repeat")

;;; -------------------------------------------------------

;; Print "Hi" in binary.
;; This program outputs the binary sequence
;;   0100100001101001,
;; composed of two bytes
;;   01001000 01101001
;; the higher-valued moeity of which
;;   01001000
;; encodes the decimal integer 72, corresponding to the ASCII character
;; "H", while the trailing compartment
;;   01101001
;; is tantamount a decimal 105, the character code of "i"; thus:
;;   01001000 01101001
;;       H       i
(interpret-Juna
  "
  0 1 1 1 1 #Set register 1 to 0 NAND 0 (1)
  1 0 . 2 2 #Printing
  2 1 . 3 3
  3 0 . 4 4
  4 0 . 5 5
  5 1 . 6 6
  6 0 . 7 7
  7 0 . 8 8
  8 0 . 9 9
  9 0 . 10 10
  10 1 . 11 11
  11 1 . 12 12
  12 0 . 13 13
  13 1 . 14 14
  14 0 . 15 15
  15 0 . 16 16
  16 1 . . .
  "
  )

;;; -------------------------------------------------------

;; Infinitely repeating cat program which accepts only bits (0 or 1).
(interpret-Juna
  "# Store the user input bit in register 0, and jump to line 1.
   0 . 0 1 1
   
   # Print the bit stored in register 0, and returns to line 0.
   1 0 . 0 0")
