;; Date: 2022-01-22
;; 
;; -> "https://esolangs.org/wiki/Jumpback"
;; -> "https://www.tutorialspoint.com/assembly_programming/assembly_addressing_modes.htm"
;;     o Describes the various addressing modes in assembly languages.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, the same defaulting to ``T''."
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

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines an unlimited list-based stack of zero
   or more elements, each of which conforms to the ELEMENT-TYPE, the
   same defaulting to ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and the
   associated value to the VALUE-TYPE, both defaulting to ``T''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type value)))
  "An ``Operand'' describes a datum in the role of a parameter to an
   instruction."
  (type  NIL :type (or null keyword))
  (value NIL :type T)) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Instruction ()
  ((type
    :initarg       :type
    :initform      (error "The instruction requires a type.")
    :type          keyword
    :documentation "The instruction type.")
   (operands
    :initarg       :operands
    :initform      (make-array 0
                     :element-type 'Operand
                     :adjustable   T
                     :fill-pointer 0)
    :type          (vector Operand *)
    :documentation "The parameters induced into this instruction
                    call."))
  (:documentation
    "The ``Instruction'' class encapsulates the data necessary for
     processing a Jumpback command, including its identifying type and
     the induced operands."))

;;; -------------------------------------------------------

(defun make-instruction (type &rest operands)
  "Creates and returns a new ``Instruction'' of the TYPE, optionally
   initialized with the variadic list of initial OPERANDS."
  (declare (type keyword           type))
  (declare (type (list-of Operand) operands))
  (let ((instruction (make-instance 'Instruction :type type)))
    (declare (type Instruction instruction))
    (dolist (operand operands)
      (declare (type Operand operand))
      (vector-push-extend operand
        (slot-value instruction 'operands)))
    (the Instruction instruction)))

;;; -------------------------------------------------------

(defun instruction-type (instruction)
  "Returns the INSTRUCTION type."
  (declare (type Instruction instruction))
  (the keyword (slot-value instruction 'type)))

;;; -------------------------------------------------------

(defun instruction-operands (instruction)
  "Returns the INSTRUCTION's operands."
  (declare (type Instruction instruction))
  (the (vector Operand *) (slot-value instruction 'operands)))

;;; -------------------------------------------------------

(defun instruction-operand (instruction index)
  "Returns the INSTRUCTION operand at the INDEX."
  (declare (type Instruction instruction))
  (declare (type fixnum      index))
  (the Operand (aref (slot-value instruction 'operands) index)))

;;; -------------------------------------------------------

(defun instruction-add-operand (instruction new-operand)
  "Appends to the INSTRUCTION's operands the NEW-OPERAND and returns
   the modified INSTRUCTION."
  (declare (type Instruction instruction))
  (declare (type Operand     new-operand))
  (vector-push-extend new-operand
    (slot-value instruction 'operands))
  (the Instruction instruction))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Instruction) stream)
  (declare (type Instruction                     instruction))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "Instruction(type=~s, operands=["
    (slot-value instruction 'type))
  (loop
    for operand
      of-type Operand
      across  (slot-value instruction 'operands)
    for first-element-p
      of-type boolean
      =       T
      then    NIL
    do
      (unless first-element-p
        (format stream ", "))
      (format stream "~s" operand))
  (format stream "])"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "A ``Token'' represents a significant constituent recognized in the
   Jumpback source code."
  (type  NIL :type (or null keyword))
  (value NIL :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning a
   ``boolean'' value of ``T'' on confirmation, and otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +INSTRUCTION-TOKENS+))

(defparameter +INSTRUCTION-TOKENS+ (make-hash-table :test #'equal)
  "Associates with each instruction name, as identified in the source
   code, a ``Token'' in the agency of its representation.")

(flet ((add-instruction (identifier)
        "Adds to the ``+INSTRUCTION-TOKENS+'' a new entry mapping the
         IDENTIFIER to an ``:instruction'' type ``Token'', and returns
         no value."
        (declare (type string identifier))
        (setf (gethash identifier +INSTRUCTION-TOKENS+)
              (make-token :instruction identifier))
        (values)))
  (add-instruction "ADD")
  (add-instruction "ADDEQ")
  (add-instruction "ADDNE")
  (add-instruction "JEQ")
  (add-instruction "JGE")
  (add-instruction "JLE")
  (add-instruction "JNE")
  (add-instruction "LBL")
  (add-instruction "MOV")
  (add-instruction "NEG")
  (add-instruction "PSH")
  (add-instruction "POP")
  (add-instruction "SHOW")
  (add-instruction "SUB")
  (add-instruction "SUBEQ")
  (add-instruction "SUBNE")
  (values))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "The lexer requires a source.")
    :type          string
    :documentation "The Jumpback code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position in the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the source."))
  (:documentation
    "The ``Lexer'' splits a piece of Jumpback code into a sequence of
     tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (and (plusp (length source))
                 (< position (length source)))
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' for operating on the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next character, if possible,
   updates the current character, and returns the modified LEXER."
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

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a space, returning a
   ``boolean'' value of ``T'' on confirmation or ``NIL'' on mismatch."
  (declare (type character character))
  (the boolean
    (not (null (member character '(#\Space) :test #'char=)))))

;;; -------------------------------------------------------

(defun get-instruction-token (identifier)
  "Returns the ``Token'' associated with the instruction IDENTIFIER,
   or ``NIL'' if no such entry exists."
  (declare (type string identifier))
  (the (or null Token) (gethash identifier +INSTRUCTION-TOKENS+)))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the LEXER's current position, skips zero or more spaces,
   relocates the position cursor to the first non-space character, and
   returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-newlines (lexer)
  "Starting at the LEXER's current position, reads zero or more adjacent
   newline characters and returns a new ``Token'' of the ``:newline''
   type containing the tally of consumed newlines."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (loop
        while   (and character (char= character #\Newline))
        do      (lexer-advance lexer)
        count   1
        into    number-of-newlines
        finally (return (make-token :newline number-of-newlines))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position, consumes an alphabetic
   identifier, locates the position cursor to the first non-alphabet
   character, and either returns an ``:instruction'' type token, or, if
   none is represented by the character sequence, a general
   ``:identifier'' token."
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
        (or (get-instruction-token  identifier)
            (make-token :identifier identifier))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position in the LEXER, reads an unsigned
   integer number and returns it."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the integer
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (loop while (and character (digit-char-p character)) do
            (write-char character digits)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   If the analyzed source code is exhausted, the LEXER constantly
   returns a fresh instance of an ``:eof'' type token."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ;; End of code.
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        
        ((char= character #\Newline)
          (lexer-read-newlines lexer))
        
        ((char= character #\,)
          (lexer-advance lexer)
          (make-token :comma ","))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        ((digit-char-p character)
          (make-token :operand
            (make-operand :literal-number
              (lexer-read-number lexer))))
        
        ((char= character #\$)
          (lexer-advance lexer)
          (make-token :operand
            (make-operand :immediate-address
              (lexer-read-number lexer))))
        
        ((char= character #\#)
          (lexer-advance lexer)
          (make-token :operand
            (make-operand :memory-address
              (lexer-read-number lexer))))
        
        ((char= character #\@)
          (lexer-advance lexer)
          (make-token :operand
            (make-operand :memory-indirect-address
              (lexer-read-number lexer))))
        
        (T
          (error "Invalid character ~s at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-instruction-type (instruction-name)
  "Returns the instruction type keyword associated with the
   INSTRUCTION-NAME."
  (declare (type string instruction-name))
  (the keyword
    (cond
      ((string= instruction-name "ADD")   :add)
      ((string= instruction-name "ADDEQ") :addeq)
      ((string= instruction-name "ADDNE") :addne)
      ((string= instruction-name "SUBEQ") :subeq)
      ((string= instruction-name "SUBNE") :subne)
      ((string= instruction-name "JEQ")   :jeq)
      ((string= instruction-name "JGE")   :jge)
      ((string= instruction-name "JLE")   :jle)
      ((string= instruction-name "JNE")   :jne)
      ((string= instruction-name "LBL")   :lbl)
      ((string= instruction-name "MOV")   :mov)
      ((string= instruction-name "NEG")   :neg)
      ((string= instruction-name "POP")   :pop)
      ((string= instruction-name "PSH")   :psh)
      ((string= instruction-name "SHOW")  :show)
      ((string= instruction-name "SUB")   :sub)
      (T (error "Cannot map the instruction ~s to a type."
           instruction-name)))))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "The parser requires a lexer.")
    :type          Lexer
    :documentation "The lexer responsible for delivering the tokens.")
   (current-token
    :initarg       :current-token
    :initform      NIL
    :type          (or null Token)
    :documentation "The token most recently requested from the LEXER."))
  (:documentation
    "The ``Parser'' generates from a sequence of ``Token'' objects an
     ``Instruction'' sequence."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' based upon the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the current token of the PARSER equals in its type
   the EXPECTED-TOKEN-TYPE, on confirmation loading the next token from
   the PARSER's lexer and returning the modified PARSER; on mismatch
   signaling an error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (if (token-type-p current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but encountered the ~
              token ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-operands (parser instruction)
  "Parses a sequence of zero or more instruction operands using the
   PARSER, adds these to the INSTRUCTION's operands, and returns the
   modified INSTRUCTION."
  (declare (type Parser      parser))
  (declare (type Instruction instruction))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (when (token-type-p current-token :operand)
      (instruction-add-operand instruction (token-value current-token))
      (parser-eat parser :operand)
      (loop while (token-type-p current-token :comma) do
        (parser-eat parser :comma)
        (cond
          ((token-type-p current-token :operand)
            (instruction-add-operand instruction
              (token-value current-token))
            (parser-eat parser :operand))
          (T
            (error "Expected an operand to follow the comma, but ~
                    encountered the token ~s."
              current-token))))))
  (the Instruction instruction))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses and returns an instruction using the PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((instruction
            (make-instruction
              (get-instruction-type
                (token-value current-token)))))
      (declare (type Instruction instruction))
      (parser-eat parser :instruction)
      (parser-parse-operands parser instruction)
      (the Instruction instruction))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Orders the PARSER to parse the tokens delivered by its internally
   maintained lexer and returns a vector of instructions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    
    (let ((instructions (make-array 0
                          :element-type 'Instruction
                          :adjustable   T
                          :fill-pointer 0)))
      (declare (type (vector Instruction *) instructions))
      
      (loop do
        (case (token-type current-token)
          
          ((NIL)
            (loop-finish))
          
          (:eof
            (loop-finish))
          
          (:instruction
            (vector-push-extend
              (parser-parse-instruction parser)
              instructions)
            
            (loop while (token-type-p current-token :newline) do
              (parser-eat parser :newline)
              
              (case (token-type current-token)
                ((NIL)
                  (loop-finish))
                (:eof
                  (loop-finish))
                (:instruction
                  (vector-push-extend
                    (parser-parse-instruction parser)
                    instructions))
                (:newline
                  (parser-eat parser :newline))
                (otherwise
                  (error "Expected an instruction token, but encountered ~s."
                    current-token)))))
          
          (:newline
            (parser-eat parser :newline))
          
          (otherwise
            (error "Invalid token: ~s." current-token))))
      
      (the (vector Instruction *) instructions))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "The interpreter requires an instruction sequence.")
    :type          (vector Instruction *)
    :documentation "The instruction sequence to process.")
   (instruction-pointer
    :initarg       :instruction-pointer
    :initform      0
    :type          fixnum
    :documentation "The index of the currently processed ``Instruction''
                    among the elements of the INSTRUCTIONS vector.")
   (address-stack
    :initarg       :address-stack
    :initform      NIL
    :type          (stack-of fixnum)
    :documentation "The address stack, maintaining instruction pointers
                    used as jumping labels, pushed unto by the 'lbl'
                    instruction and popped using one of the various jump
                    commands.")
   (memory
    :initarg       :memory
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer integer)
    :documentation "The program memory.")
   (value-stack
    :initarg       :value-stack
    :initform      NIL
    :type          (stack-of integer)
    :documentation "The value stack, utible as an auxiliary storage, and
                    operated upon by the 'PSH' instruction, which pushes
                    data unto it, and the 'POP' command, which pops
                    thereform."))
  (:documentation
    "The ``Interpreter'' represents the entity responsible for operating
     upon a sequence of instructions and imbue them with effect."))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' which operates on the
   INSTRUCTIONS."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun memory-at (interpreter address)
  "Returns the value residing in the INTERPRETER's memory at the
   specified ADDRESS."
  (declare (type Interpreter interpreter))
  (declare (type Operand     address))
  (declare (ignorable        interpreter))
  (with-slots (memory) interpreter
    (declare (type (hash-table-of integer integer) memory))
    (declare (ignorable                            memory))
    (let ((address-value (operand-value address)))
      (declare (type integer address-value))
      (the integer
        (case (operand-type address)
          (:literal-number
            address-value)
          (:immediate-address
            address-value)
          (:memory-address
            (gethash address-value memory))
          (:memory-indirect-address
            (let ((direct-address-value (gethash address-value memory)))
              (declare (type integer direct-address-value))
              (gethash direct-address-value memory)))
          (otherwise
            (error "Invalid address intended to read: ~s."
              address)))))))

;;; -------------------------------------------------------

(defun (setf memory-at) (source interpreter destination)
  "Sets the value residing in the INTERPRETER's memory at the
   DESTINATION address to the SOURCE, and returns the modified
   INTERPRETER."
  (declare (type integer     source))
  (declare (type Interpreter interpreter))
  (declare (type Operand     destination))
  (declare (ignorable        interpreter))
  (with-slots (memory) interpreter
    (declare (type (hash-table-of integer integer) memory))
    (declare (ignorable                            memory))
    (let ((destination-value (operand-value destination)))
      (declare (type integer destination-value))
      (the integer
        (case (operand-type destination)
          (:literal-number
            (error "Cannot write a literal destination address: ~s."
              destination))
          (:immediate-address
            (error "Cannot write an immediate destination address: ~s."
              destination))
          (:memory-address
            (setf (gethash destination-value memory)
                  source))
          (:memory-indirect-address
            (let ((direct-destination
                    (gethash destination-value memory)))
              (declare (type integer direct-destination))
              (setf (gethash direct-destination memory)
                    source)))
          (otherwise
            (error "Invalid address intended to write: ~s."
              destination))))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Moves the INTERPRETER's instruction pointer one instruction forward
   and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (incf (slot-value interpreter 'instruction-pointer))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-jump-to (interpreter new-address)
  "Moves the INTERPRETER's instruction pointer to the NEW-ADDRESS and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-address))
  (setf (slot-value interpreter 'instruction-pointer) new-address)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-jump-if (interpreter
                            predicate
                            left-operand
                            right-operand)
  "Jumps to the instruction located at the top of the INTERPRETER's
   address stack if and only if the PREDICATE is satisfied regarding the
   LEFT-OPERAND and RIGHT-OPERAND, otherwise remaining ineffectuous, in
   any case returning the INTERPRETER."
  (declare (type Interpreter                    interpreter))
  (declare (type (function (integer integer) *) predicate))
  (declare (type Operand                        left-operand))
  (declare (type Operand                        right-operand))
  (if (funcall predicate
        (memory-at interpreter left-operand)
        (memory-at interpreter right-operand))
    (interpreter-jump-to interpreter
      (pop (slot-value interpreter 'address-stack)))
    (interpreter-advance interpreter))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun check-instruction-operands (instruction
                                   expected-number-of-operands)
  "Checks whether the tally of operands stored in the INSTRUCTION
   matches the EXPECTED-NUMBER-OF-OPERANDS, throwing an error on
   mismatch, otherwise simply returning the INSTRUCTION."
  (declare (type Instruction   instruction))
  (declare (type (integer 0 *) expected-number-of-operands))
  (unless (= (length (instruction-operands instruction))
             expected-number-of-operands)
    (error "The instruction ~s expected ~d operands, but received ~d."
      (instruction-type instruction)
      expected-number-of-operands
      (length (instruction-operands instruction))))
  (the Instruction instruction))

;;; -------------------------------------------------------

(defgeneric dispatch-instruction (interpreter
                                  instruction-type
                                  instruction)
  (:documentation
    "Processes the INSTRUCTION identified by the INSTRUCTION-TYPE using
     the INTERPRETER, and returns no value."))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :add))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (incf (memory-at interpreter destination)
          (memory-at interpreter source)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :addeq))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1))
        (cmp         (instruction-operand instruction 2)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (declare (type Operand cmp))
    (when (= (memory-at interpreter source)
             (memory-at interpreter cmp))
      (incf (memory-at interpreter destination)
            (memory-at interpreter source))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :addne))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1))
        (cmp         (instruction-operand instruction 2)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (declare (type Operand cmp))
    (when (/= (memory-at interpreter source)
              (memory-at interpreter cmp))
      (incf (memory-at interpreter destination)
            (memory-at interpreter source))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :jeq))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((left-operand  (instruction-operand instruction 0))
        (right-operand (instruction-operand instruction 1)))
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (interpreter-jump-if interpreter #'= left-operand right-operand))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :jge))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((left-operand  (instruction-operand instruction 0))
        (right-operand (instruction-operand instruction 1)))
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (interpreter-jump-if interpreter #'> left-operand right-operand))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :jle))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((left-operand  (instruction-operand instruction 0))
        (right-operand (instruction-operand instruction 1)))
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (interpreter-jump-if interpreter #'< left-operand right-operand))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :jne))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((left-operand  (instruction-operand instruction 0))
        (right-operand (instruction-operand instruction 1)))
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (interpreter-jump-if interpreter #'/= left-operand right-operand))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :lbl))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 0)
  (push (slot-value interpreter 'instruction-pointer)
        (slot-value interpreter 'address-stack))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :mov))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (setf (memory-at interpreter destination)
          (memory-at interpreter source)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :neg))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 1)
  (let ((destination (instruction-operand instruction 0)))
    (setf (memory-at interpreter destination)
          (- (memory-at interpreter destination))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :pop))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 1)
  (let ((destination (instruction-operand instruction 0)))
    (setf (memory-at interpreter destination)
          (pop (slot-value interpreter 'value-stack))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :psh))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 1)
  (let ((source (instruction-operand instruction 0)))
    (push (memory-at interpreter source)
          (slot-value interpreter 'value-stack)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :show))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 1)
  (let ((address (instruction-operand instruction 0)))
    (declare (type Operand address))
    (write (memory-at interpreter address))
    (write #\Space :escape NIL))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :sub))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (decf (memory-at interpreter destination)
          (memory-at interpreter source)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :subeq))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1))
        (cmp         (instruction-operand instruction 2)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (declare (type Operand cmp))
    (when (= (memory-at interpreter source)
             (memory-at interpreter cmp))
      (decf (memory-at interpreter destination)
            (memory-at interpreter source))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :subne))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (check-instruction-operands instruction 2)
  (let ((destination (instruction-operand instruction 0))
        (source      (instruction-operand instruction 1))
        (cmp         (instruction-operand instruction 2)))
    (declare (type Operand destination))
    (declare (type Operand source))
    (declare (type Operand cmp))
    (when (/= (memory-at interpreter source)
              (memory-at interpreter cmp))
      (decf (memory-at interpreter destination)
            (memory-at interpreter source))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defun process-instruction (interpreter instruction)
  "Processes the INSTRUCTION using the INTERPRETER and returns the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (dispatch-instruction interpreter
    (instruction-type instruction)
    instruction)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instruction set maintained by the INTERPRETER and
   returns the INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions instruction-pointer) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 instruction-pointer))
    (loop while (< instruction-pointer (length instructions)) do
      (process-instruction interpreter
        (aref instructions instruction-pointer))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Jumpback (code)
  "Interprets the piece of Jumpback CODE and returns no value."
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

;; A countdown from 10 to 0.
(interpret-Jumpback
  "MOV #0, 10
   MOV #1, 0
   LBL
   SHOW #0
   SUB #0, 1
   JNE #0, #1")

;;; -------------------------------------------------------

;; Print an infinite Fibonacci sequence.
;; 
;; Concept:
;;   
;;   memory[0] = 0
;;   memory[1] = 1
;;   
;;   print memory[0]
;;   print memory[1]
;;   
;;   set label
;;   push memory[1] to stack
;;   memory[1] = memory[1] + memory[0]
;;   memory[0] = pop old memory[1] from stack
;;   print memory[1]
;;   jump to label
;;   
(interpret-Jumpback
  "MOV #0, 0
   MOV #1, 1
  
   SHOW #0
   SHOW #1
   
   LBL
   
   PSH #1
   ADD #1, #0
   POP #0
   SHOW #1
   
   JGE #1, 0")

;;; -------------------------------------------------------

;; Print Fibonacci sequence, but terminate after having displayed the
;; first Fibonacci number greater than or equal to 1000.
;; 
;; Concept:
;;   
;;   memory[0] = 0
;;   memory[1] = 1
;;   
;;   print memory[0]
;;   print memory[1]
;;   
;;   set label
;;   push memory[1] to stack
;;   memory[1] = memory[1] + memory[0]
;;   memory[0] = pop old memory[1] from stack
;;   print memory[1]
;;   
;;   if memory[1] < 1000 then
;;     jump to label
;;   end if
;;   
(interpret-Jumpback
  "MOV #0, 0
   MOV #1, 1
   
   SHOW #0
   SHOW #1
   
   LBL
   
   PSH #1
   ADD #1, #0
   POP #0
   SHOW #1
   
   JLE #1, 1000")

;;; -------------------------------------------------------

;; Print the first ten Fibonacci numbers.
;; 
;; Concept:
;;   
;;   memory[0] = 0
;;   memory[1] = 1
;;   { Number of Fibonacci numbers printed. Takes into account the }
;;   { two (2) initial displays of memory[0] and memory[1].            }
;;   memory[2] = 2
;;   
;;   print memory[0]
;;   print memory[1]
;;   
;;   set label
;;   push memory[1] to stack
;;   memory[1] = memory[1] + memory[0]
;;   memory[0] = pop old memory[1] from stack
;;   print memory[1]
;;   
;;   { Count the recently printed Fibonacci number. }
;;   memory[2] = memory[2] + 1
;;   
;;   if memory[2] != 10 then
;;     jump to label
;;   end if
;; 
;; In order to adjust the tally of printed Fibonacci numbers, please
;; modify the literal value "10" in the desinent instruction
;;   JNE #2, 10
;; to an integer value greater than or equal to three (3); for example:
;;   JNE #2, 45
;; 
(interpret-Jumpback
  "MOV #0, 0
   MOV #1, 1
   MOV #2, 2
   
   SHOW #0
   SHOW #1
   
   LBL
   
   PSH #1
   ADD #1, #0
   POP #0
   SHOW #1
   
   ADD #2, 1
   
   JNE #2, 10")
