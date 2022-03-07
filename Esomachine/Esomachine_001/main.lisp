;; Author: Kaveh Yousefi
;; Date:   2022-03-05
;; 
;; -> "https://esolangs.org/wiki/Esomachine"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE."
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
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE."
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
   including, without claim of exhaustion, ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "A ``Token'' represents a significant portion extracted from a piece
   of Esomachine source code."
  (type  (error "No token type specified.") :type keyword)
  (value NIL                                :type T))

;;; -------------------------------------------------------

(defun token-type-p (token &rest expected-types)
  "Checks whether the TOKEN type conforms to any member of the
   EXPECTED-TYPES, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token             token))
  (declare (type (list-of keyword) expected-types))
  (the boolean
    (not (null
      (member (token-type token) expected-types :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string Token) +IDENTIFIERS+))

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal)
  "Maintains a mapping of recognized Esomachine keywords, associated
   to representative tokens.")

;; Register the recognized keywords and associate them with tokens.
(flet ((add-identifier (name token)
        "Adds to the +IDENTIFIERS+ a new entry which associates the
         keyword NAME with the TOKEN, and returns no value."
        (declare (type string name))
        (declare (type Token  token))
        (setf (gethash name +IDENTIFIERS+) token)
        (values)))
  (add-identifier "INDEX_STATE"   (make-token :index-state   "INDEX_STATE"))
  (add-identifier "INDEX_SET"     (make-token :index-set     "INDEX_SET"))
  (add-identifier "HANDS_CONLANG" (make-token :hands-conlang "HANDS_CONLANG"))
  (add-identifier "HANDS_JUMP"    (make-token :hands-jump    "HANDS_JUMP"))
  (add-identifier "HANDS_EXPECT"  (make-token :hands-expect  "HANDS_EXPECT"))
  (add-identifier "OUTPUT"        (make-token :output        "OUTPUT"))
  (add-identifier "HANDS"         (make-token :hands         "HANDS"))
  (add-identifier "+"             (make-token :plus          "+"))
  (add-identifier "-"             (make-token :minus         "-"))
  (add-identifier "*"             (make-token :times         "*"))
  (add-identifier "/"             (make-token :divide        "/"))
  (add-identifier "NEGATIVE"      (make-token :negative      "NEGATIVE"))
  (add-identifier "POSITIVE"      (make-token :positive      "POSITIVE"))
  (add-identifier "ZERO"          (make-token :zero          "ZERO"))
  (add-identifier "DONTCARE"      (make-token :dontcare      "DONTCARE"))
  (values))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "No lexer source specified.")
    :type          string
    :documentation "The Esomachine code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class defines an entity responsible for the division
     of a piece of Esomachine source code into its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (< position (length source))
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which analyzes the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (char= character #\Space)
          (char= character #\Tab))))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next position in its maintained source, if
   possible, and returns the modified LEXER."
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
   adjacent spaces and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current position in the LEXER, treats all characters
   up to the end of the line or the end of the file as a comment, skips
   this content, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop until (or (null character) (char= character #\Newline)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position in the LEXER, reads a potentially
   signed integer and returns token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            ;; Check for a leading sign ("+" or "-").
            (when (find character "+-" :test #'char=)
              (write-char character digits)
              (lexer-advance lexer))
            ;; Consume digits.
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-digit-follows-p (lexer)
  "Checks whether the character following the LEXER's current position
   constitutes a decimal digit, returning a ``boolean'' value of ``T''
   on confirmation, otherwise ``NIL''."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the boolean
      (not (null (and character (digit-char-p character)))))))

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER tallies among the recognized
   constituents of an Esomachine identifier name, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (alpha-char-p character)
          (find character "_+-*/" :test #'char=))))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Starting at the current position in the LEXER, reads a string
   adhering to the syntax of an Esomachine identifier name, and returns
   it."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the string
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (loop
          while (and character (identifier-character-p character))
          do
          (write-char character identifier)
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current position in the LEXER, reads an identifier
   and returns a token representation thereof.
   ---
   If the consumed name does not match any identifier as defined by the
   Esomachine standard, an error is signaled."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((identifier (lexer-read-string lexer)))
      (declare (type string identifier))
      (the Token
        (or (gethash identifier +IDENTIFIERS+)
            (error "Unrecognized identifier: ~s." identifier))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns from the LEXER the next token.
   ---
   Upon its token stream's exhaustion, the LEXER continuously returns a
   fresh token of the ``:eof'' (end of file) type upon each invocation."
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
        
        ((char= character #\\)
          (lexer-skip-comment   lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\Newline)
          (prog1 (make-token :newline character)
            (lexer-advance lexer)))
        
        ((char= character #\[)
          (prog1 (make-token :left-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\])
          (prog1 (make-token :right-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\,)
          (prog1 (make-token :comma character)
            (lexer-advance lexer)))
        
        ((or (digit-char-p character)
             (and (find character "+-" :test #'char=)
                  (lexer-digit-follows-p lexer)))
          (lexer-read-number lexer))
        
        ((or (alpha-char-p character)
             (find character "+-" :test #'char=))
          (lexer-read-identifier lexer))
        
        (T
          (error "Invalid character '~a' at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementations of operands.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface describes the commonalities of objects
   which act in a capacity as arguments to instructions."
  (type (error "Missing operand type.") :type keyword))

;;; -------------------------------------------------------

(defstruct (Boolean-Operand
  (:include     Operand)
  (:constructor make-boolean-operand (state &aux (type :boolean))))
  "The ``Boolean-Operand'' models a Boolean value represented by the
   integer 0 and 1 and usually connoted with a cell's lock state, that
   is, appertaining to the Esomachine instruction 'INDEX_STATE'."
  (state 0 :type bit))

;;; -------------------------------------------------------

(defstruct (Arithmetic-Operand
  (:include Operand)
  (:constructor make-arithmetic-operand (operation
                                         &aux (type :arithmetic))))
  "The ``Arithmetic-Operand'' models the arithmetic operations names
   involved in the Esomachine instruction 'HANDS_CONLANG'."
  (operation NIL :type (or null keyword)))

;;; -------------------------------------------------------

(defstruct (Conditional-Operand
  (:include     Operand)
  (:constructor make-conditional-operand (condition
                                          &aux (type :conditional))))
  "The ``Conditional-Operand'' models the jump conditions entailed in
   the Esomachine instruction 'HANDS_JUMP'."
  (condition NIL :type (or null keyword)))

;;; -------------------------------------------------------

(defstruct (Numeric-Operand
  (:include     Operand)
  (:constructor make-numeric-operand (&aux (type :numeric))))
  "The ``Numeric-Operand'' provides an abstract base class for the
   representation of numbers and addresses, encompassing literal
   integers, the 'HANDS' keyword, and indirect addresses, denoted by
   brackets ('['...']').
   ---
   Each member of this trichotomy submits to the custody of a dedicated
   subclass of ``Numeric-Operand'', which please see below.")

;;; -------------------------------------------------------

(defstruct (Literal-Operand
  (:include     Numeric-Operand)
  (:constructor make-literal-operand (value &aux (type :literal))))
  "The ``Literal-Operand'' represents a literal integer in the role of
   a number of address."
  (value 0 :type integer))

;;; -------------------------------------------------------

(defstruct (Hands-Operand
  (:include     Numeric-Operand)
  (:constructor make-hands-operand (&aux (type :hands))))
  "The ``Hands-Operand'' represents the special number or address
   amenable to the constant identifier 'HANDS' and connected with the
   program's accumulator.")

;;; -------------------------------------------------------

(defstruct (Indirection-Operand
  (:include     Numeric-Operand)
  (:constructor make-indirection-operand (index
                                          &aux (type :indirection))))
  "The ``Indirection-Operand'' represents an instance of indirect
   addressing, visualized by brackets ('['...']') in the Esomachine
   source code, which itself contains one object of the
   ``Numeric-Operand'' subclasses as the item to construe as a cell
   index."
  (index NIL :type (or null Numeric-Operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (operands NIL))))
  "The ``Instruction'' class models an Esomachine instruction,
   contingently embracing a particular tally of operands."
  (type     (error "Missing instruction type.") :type keyword)
  (operands NIL :type (list-of Operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "No lexer for the parser specified.")
    :type          Lexer
    :documentation "The lexer responsible for the token purveyance.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recent token supplied by the LEXER."))
  (:documentation
    "The ``Parser'' implements a unit responsible for the assemblage of
     a series of tokens into a vector of instructions.
     ---
     The reproduction of empty lines proceeds by means of a particular
     instruction type, the no-operation (NOP), designated using the
     euonymous keyword ``:nop'', and intended to ascertain a
     preservation of the line numbering in the case of changes in the
     language's specification regarding vacant sections. The
     instruction identification mechanism permits a comfortable
     culling of these sentinel types from a collection resulting from
     a parsing process."))

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
  "Creates and returns a new ``Parser'' receiving its tokens from the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation requesting the next token from
   the internally maintained lexer, storing it in the PARSER, and
   returning the modified PARSER; otherwise an error is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (if (token-type-p current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type current-token))))

;;; -------------------------------------------------------

(defun parser-parse-number (parser)
  "Parses a number using the PARSER and returns a ``Numeric-Operand''
   encapsulation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Numeric-Operand
      (case (token-type current-token)
        ;; Literal integer.
        (:number
          (prog1
            (make-literal-operand (token-value current-token))
            (parser-eat parser :number)))
        
        ;; Use accumulator value.
        (:hands
          (prog1
            (make-hands-operand)
            (parser-eat parser :hands)))
        
        ;; Use value of cells[index].
        (:left-bracket
          (parser-eat parser :left-bracket)
          (let ((index (parser-parse-number parser)))
            (declare (type Numeric-Operand index))
            (parser-eat parser :right-bracket)
            (make-indirection-operand index)))
        
        (otherwise
          (error "Invalid number token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-boolean (parser)
  "Parses a bit value using the PARSER and returns a ``Boolean-Operand''
   encapsulation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Operand
      (case (token-type current-token)
        (:number
          (let ((value (token-value current-token)))
            (declare (type integer value))
            (parser-eat parser :number)
            (unless (<= 0 value 1)
              (error "Invalid boolean token: ~s." current-token))
            (make-boolean-operand value)))
        (otherwise
          (error "Invalid boolean token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-arithmetic-operand (parser)
  "Parses an arithmetic operation identifier using the PARSER and
   returns an ``Arithmetic-Operand'' encapsulation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Arithmetic-Operand
      (cond
        ((token-type-p current-token :plus :minus :times :divide)
          (prog1
            (make-arithmetic-operand (token-type current-token))
            (parser-eat parser (token-type current-token))))
        (T
          (error "Expected an arithmetic operand token, but ~
                  encountered ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-condition (parser)
  "Parses a jump condition using the PARSER and returns a
   ``Conditional-Operand'' encapsulation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Conditional-Operand
      (cond
        ((token-type-p current-token
                       :negative :positive :zero :dontcare)
          (prog1
            (make-conditional-operand (token-type current-token))
            (parser-eat parser (token-type current-token))))
        (T
          (error "Expected a condition operand token, but ~
                  encountered ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-expect-end-of-instruction (parser)
  "Checks whether the PARSER's current token represents a newline or
   end of file (EOF), compatible as a conclusion of an instruction,
   on confirmation requesting the next token from the internally managed
   lexer, storing it in the PARSER, and returning the modified PARSER;
   on mismatch an error is signaled."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (case (token-type current-token)
      (:eof
        NIL)
      (:newline
        (parser-eat parser :newline))
      (otherwise
        (error "Expected either a newline or end of file following the ~
                instruction, but encountered the token ~s."
          current-token))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses an instruction using the PARSER and returns an ``Instruction''
   encapsulation thereof."
  (declare (type Parser parser))
  
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (the Instruction
      (case (token-type current-token)
        
        (:newline
          (prog1
            (make-instruction :nop)
            (parser-eat parser :newline)))
        
        (:index-state
          (let ((state NIL)
                (index NIL))
            (declare (type (or null Boolean-Operand) state))
            (declare (type (or null Numeric-Operand) index))
            
            (parser-eat parser :index-state)
            (parser-eat parser :left-bracket)
            (setf state (parser-parse-boolean parser))
            (parser-eat parser :comma)
            (setf index (parser-parse-number parser))
            (parser-eat parser :right-bracket)
            
            (prog1
              (make-instruction :index-state (list state index))
              (parser-expect-end-of-instruction parser))))
        
        (:index-set
          (let ((cell-index NIL)
                (cell-value NIL))
            (declare (type (or null Numeric-Operand) cell-index))
            (declare (type (or null Numeric-Operand) cell-value))
            
            (parser-eat parser :index-set)
            (parser-eat parser :left-bracket)
            (setf cell-index (parser-parse-number parser))
            (parser-eat parser :comma)
            (setf cell-value (parser-parse-number parser))
            (parser-eat parser :right-bracket)
            
            (prog1
              (make-instruction :index-set (list cell-index cell-value))
              (parser-expect-end-of-instruction parser))))
        
        (:hands-conlang
          (let ((operation     NIL)
                (right-operand NIL))
            (declare (type (or null Arithmetic-Operand) operation))
            (declare (type (or null Numeric-Operand)    right-operand))
            
            (parser-eat parser :hands-conlang)
            (parser-eat parser :left-bracket)
            (setf operation (parser-parse-arithmetic-operand parser))
            (parser-eat parser :comma)
            (setf right-operand (parser-parse-number parser))
            (parser-eat parser :right-bracket)
            
            (prog1
              (make-instruction :hands-conlang
                (list operation right-operand))
              (parser-expect-end-of-instruction parser))))
        
        (:hands-jump
          (let ((condition   NIL)
                (line-number NIL))
            (declare (type (or null Conditional-Operand) condition))
            (declare (type (or null Numeric-Operand)     line-number))
            
            (parser-eat parser :hands-jump)
            (parser-eat parser :left-bracket)
            (setf condition (parser-parse-condition parser))
            (parser-eat parser :comma)
            (setf line-number (parser-parse-number parser))
            (parser-eat parser :right-bracket)
            
            (prog1
              (make-instruction :hands-jump
                (list condition line-number))
              (parser-expect-end-of-instruction parser))))
        
        (:hands-expect
          (parser-eat parser :hands-expect)
          (parser-eat parser :left-bracket)
          (parser-eat parser :right-bracket)
          (prog1
            (make-instruction :hands-expect)
            (parser-expect-end-of-instruction parser)))
        
        (:output
          (parser-eat parser :output)
          (parser-eat parser :left-bracket)
          (let ((cell-index (parser-parse-number parser)))
            (declare (type Numeric-Operand cell-index))
            (parser-eat parser :right-bracket)
            (prog1
              (make-instruction :output (list cell-index))
              (parser-expect-end-of-instruction parser))))
        
        (otherwise
          (error "Invalid instruction token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the tokens supplied by the PARSER's lexer and returns a vector
   of instructions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (coerce
      (loop
        until   (token-type-p current-token :eof)
        collect (parser-parse-instruction parser))
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-cell ()))
  "The ``Cell'' class represents a cell as a constituent of an
   Esomachine program's memory."
  (value    0 :type integer)
  (locked-p T :type boolean))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-nop-instructions (instructions)
  "Returns a vector based upon the INSTRUCTIONS with no-operations
   (NOPs) removed."
  (declare (type (vector Instruction *) instructions))
  (the (vector Instruction *)
    (remove :nop instructions :key #'instruction-type :test #'eq)))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (make-array 0
                     :element-type    'Instruction
                     :initial-element (make-instruction :nop))
    :type          (vector Instruction *)
    :documentation "The instructions processed by this interpreter.")
   (instruction-pointer
    :initarg       :instruction-pointer
    :initform      0
    :type          fixnum
    :documentation "The index of the CURRENT-INSTRUCTION among the
                    INSTRUCTIONS vector.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null Instruction)
    :documentation "The instruction selected by the INSTRUCTION-POINTER
                    from among the INSTRUCTIONS vector.")
   (accumulator
    :initarg       :accumulator
    :initform      0
    :type          integer
    :documentation "The accumulator.")
   (cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer Cell)
    :documentation "A sparse collection of cells."))
  (:documentation
    "The ``Interpreter'' applies itself to the evaluation of a sequence
     of instructions, embuing the same with actual effect."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (instructions instruction-pointer current-instruction)
      interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 instruction-pointer))
    (declare (type (or null Instruction)  current-instruction))
    (setf current-instruction
      (when (< instruction-pointer (length instructions))
        (aref instructions instruction-pointer))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' operating on a copy of the
   INSTRUCTIONS with all no-operations (NOPs) removed."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter
      :instructions (remove-nop-instructions instructions))))

;;; -------------------------------------------------------

(defgeneric resolve-operand (interpreter operand)
  (:documentation
    "Determines and returns the value of the OPERAND in the context of
     the INTERPRETER."))

;;; -------------------------------------------------------

(defmethod resolve-operand (interpreter (operand Arithmetic-Operand))
  (declare (type Interpreter        interpreter))
  (declare (type Arithmetic-Operand operand))
  (declare (ignore                  interpreter))
  (the keyword (arithmetic-operand-operation operand)))

;;; -------------------------------------------------------

(defmethod resolve-operand (interpreter (operand Boolean-Operand))
  "Returns for an OPERAND storing the bit zero a ``boolean'' value of
   ``NIL'', otherwise ``T''."
  (declare (type Interpreter     interpreter))
  (declare (type Boolean-Operand operand))
  (declare (ignore               interpreter))
  (the boolean (not (zerop (boolean-operand-state operand)))))

;;; -------------------------------------------------------

(defmethod resolve-operand (interpreter (operand Conditional-Operand))
  "Returns a keyword symbol identifier representing the conditional
   operand."
  (declare (type Interpreter         interpreter))
  (declare (type Conditional-Operand operand))
  (declare (ignore                   interpreter))
  (the keyword (conditional-operand-condition operand)))

;;; -------------------------------------------------------

(defmethod resolve-operand (interpreter (operand Literal-Operand))
  "Returns the integer value stored in the literal OPERAND."
  (declare (type Interpreter     interpreter))
  (declare (type Literal-Operand operand))
  (declare (ignore               interpreter))
  (the integer (literal-operand-value operand)))

;;; -------------------------------------------------------

(defmethod resolve-operand (interpreter (operand Hands-Operand))
  "Returns the integer value of the accumulator maintained by the
   INTERPRETER."
  (declare (type Interpreter   interpreter))
  (declare (type Hands-Operand operand))
  (declare (ignore             operand))
  (the integer (slot-value interpreter 'accumulator)))

;;; -------------------------------------------------------

(defmethod resolve-operand (interpreter (operand Indirection-Operand))
  "Returns the value of the cell maintained by the INTERPRETER and
   amenable to an index equal to the indirect addressing OPERAND's
   stored index component.
   ---
   The OPERAND's index, itself being a ``Numeric-Operand'' datum, is
   itself subjected to a recursive ``resolve-operand'' application, ere
   its thus yielded value assumes its role as a cell address."
  (declare (type Interpreter         interpreter))
  (declare (type Indirection-Operand operand))
  (let ((index (indirection-operand-index operand)))
    (declare (type Numeric-Operand index))
    (let ((index-value (resolve-operand interpreter index)))
      (declare (type integer index-value))
      (let ((cell-at-index
              (gethash index-value
                (slot-value interpreter 'cells))))
        (declare (type (or null Cell) cell-at-index))
        (the integer
          (if cell-at-index
            (cell-value cell-at-index)
            0))))))

;;; -------------------------------------------------------

(defun resolve-instruction-operands (interpreter)
  "If the INTERPRETER employs a current instruction, its operands are
   resolved and a list is returned containing the thus produced values
   in their correct order; otherwise the empty list is delivered."
  (declare (type Interpreter interpreter))
  (with-slots (current-instruction) interpreter
    (declare (type (or null Instruction) current-instruction))
    (the (list-of T)
      (when current-instruction
        (mapcar
          #'(lambda (operand)
              (declare (type Operand operand))
              (resolve-operand interpreter operand))
          (instruction-operands current-instruction))))))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Moves the INTERPRETER's instruction pointer to the next instruction,
   if possible, updates the current instruction, and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions instruction-pointer current-instruction)
      interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 instruction-pointer))
    (declare (type (or null Instruction)  current-instruction))
    (setf current-instruction
      (when (< instruction-pointer (1- (length instructions)))
        (aref instructions (incf instruction-pointer)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-jump-to (interpreter new-line)
  "Moves the INTERPRETER's instruction pointer to the one-indexed
   NEW-LINE, updates the current instruction, and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-line))
  (with-slots (instructions instruction-pointer current-instruction)
      interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 instruction-pointer))
    (declare (type (or null Instruction)  current-instruction))
    (cond
      ((<= 1 new-line (length instructions))
        (setf instruction-pointer (1- new-line))
        (setf current-instruction
          (aref instructions instruction-pointer)))
      (T
        (error "Invalid line number: ~d. Must be in [1, ~d.]."
          new-line (length instructions)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instructions maintained by the INTERPRETER and returns
   no value."
  (declare (type Interpreter interpreter))
  
  (with-slots (current-instruction accumulator) interpreter
    (declare (type (or null Instruction) current-instruction))
    (declare (type integer               accumulator))
    
    (labels
        ((ensure-cell-at (index)
          "Ensures that a cell at the INDEX exists, returning either the
           extant or the newly produced cell."
          (declare (type integer index))
          (with-slots (cells) interpreter
            (declare (type (hash-table-of integer Cell) cells))
            (let ((cell (gethash index cells)))
              (declare (type (or null Cell) cell))
              (unless cell
                (setf cell (make-cell))
                (setf (gethash index cells) cell))
              (the Cell cell))))
         
         (cell-at (index)
          "Returns the cell at the INDEX, creating one priorly if not
           already extant."
          (declare (type integer index))
          (ensure-cell-at index))
         
         (cell-value-at (index)
          "Returns the value of the cell at the INDEX, creating one
           priorly if not already extant, in which case the default of
           zero is employed as a response."
          (declare (type integer index))
          (the integer (cell-value (ensure-cell-at index))))
         
         ((setf cell-value-at) (new-value index)
          "Sets the value of the cell at the INDEX to the NEW-VALUE,
           creating one priorly if not already extant, in any case
           returning no value."
          (declare (type integer new-value))
          (declare (type integer index))
          (let ((cell (ensure-cell-at index)))
            (declare (type Cell cell))
            (if (cell-locked-p cell)
              (error "The cell at index ~d is locked." index))
              (setf (cell-value cell) new-value))
          (values)))
      
      (loop while current-instruction do
        (case (instruction-type current-instruction)
          (:index-state
            (destructuring-bind (state cell-index)
                (resolve-instruction-operands interpreter)
              (declare (type boolean state))
              (declare (type integer cell-index))
              (setf (cell-locked-p (cell-at cell-index))
                    (not state)))
            (interpreter-advance interpreter))
          
          (:index-set
            (destructuring-bind (cell-index new-value)
                (resolve-instruction-operands interpreter)
              (declare (type integer cell-index))
              (declare (type integer new-value))
              (setf (cell-value-at cell-index) new-value))
            (interpreter-advance interpreter))
          
          (:hands-conlang
            (destructuring-bind (operation right-operand)
                (resolve-instruction-operands interpreter)
              (declare (type keyword operation))
              (declare (type integer right-operand))
              (case operation
                (:plus
                  (incf accumulator right-operand))
                (:minus
                  (decf accumulator right-operand))
                (:times
                  (setf accumulator (* accumulator right-operand)))
                (:divide
                  (setf accumulator (round accumulator right-operand)))
                (otherwise
                  (error "Invalid arithmetic operation: ~s."
                    operation))))
            (interpreter-advance interpreter))
          
          (:hands-jump
            (destructuring-bind (condition line-number)
                (resolve-instruction-operands interpreter)
              (declare (type keyword       condition))
              (declare (type (integer 0 *) line-number))
              (cond
                ((and (eq condition :negative)
                      (minusp accumulator))
                  (interpreter-jump-to interpreter line-number))
                ((and (eq condition :positive)
                      (plusp accumulator))
                  (interpreter-jump-to interpreter line-number))
                ((and (eq condition :zero)
                      (zerop accumulator))
                  (interpreter-jump-to interpreter line-number))
                ((eq condition :dontcare)
                  (interpreter-jump-to interpreter line-number))
                (T
                  (interpreter-advance interpreter)))))
          
          (:hands-expect
            (format T "~&Please input an ASCII character: ")
            (let ((input (read-char)))
              (declare (type character input))
              (clear-input)
              (setf accumulator (char-code input)))
            (interpreter-advance interpreter))
          
          (:output
            (destructuring-bind (index)
                (resolve-instruction-operands interpreter)
              (declare (type integer index))
              (write-char (code-char (cell-value-at index))))
            (interpreter-advance interpreter))
          
          (otherwise
            (error "Invalid instruction: ~s." current-instruction))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Esomachine (code)
  "Interprets the piece of Esomachine CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of additional operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-output-code (text &key (destination T))
  "Generates the Esomachine code capable of outputting the TEXT and
   writes it to the DESTINATION, returning for a DESTINATION of ``NIL''
   a fresh string containing the program, otherwise yielding the ``NIL''
   value."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for character  of-type character across (reverse text)
        for cell-index of-type fixnum    from   1
        do
          (format destination "~&INDEX_STATE[1, ~d]" cell-index)
          (format destination "~&INDEX_SET[~d, ~d] ~
                                 \\ Set cell[~d] to ASCII code of the ~
                                    character '~c'."
            cell-index (char-code character) cell-index character)
        finally
          (let ((text-length (length text)))
            (declare (type fixnum text-length))
            (format destination "~&HANDS_CONLANG[+, ~d]" text-length)
            ;; The line with the number ((textLength * 2) + 2).
            (format destination "~&OUTPUT[HANDS]")
            (format destination "~&HANDS_CONLANG[-, 1]")
            (format destination "~&HANDS_JUMP[POSITIVE, ~d]"
              (+ (* text-length 2) 2))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-output-code text :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sets the value of the cell at index 1 to the ASCII code of the
;; character "A" and prints the cell value as an ASCII character.
(interpret-Esomachine
  "INDEX_STATE[1, 1]
   INDEX_SET[1, 65]
   OUTPUT[1]")

;;; -------------------------------------------------------

;; Sets the accumulator to the ASCII code of the character "A", writes
;; the accumulator value to the cell at index 1, and prints this cell's
;; value as an ASCII character.
(interpret-Esomachine
  "HANDS_CONLANG[+, 65]
   INDEX_STATE[1, 1]
   INDEX_SET[1, HANDS]
   OUTPUT[1]")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Esomachine
  "HANDS_EXPECT[]
   INDEX_STATE[1, 1]
   INDEX_SET[1, HANDS]
   OUTPUT[1]
   HANDS_JUMP[POSITIVE, 1]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Esomachine
  "HANDS_EXPECT[]          \\line 1
   INDEX_STATE[1, 1]       \\line 2
   INDEX_SET[1, HANDS]     \\line 3
   HANDS_CONLANG[-, 48]    \\ The ascii value of 0 \line 4
   HANDS_JUMP[ZERO, 8]     \\line 5
   OUTPUT[1]               \\line 6
   HANDS_JUMP[DONTCARE, 6] \\line 7
   OUTPUT[1]               \\line 8")

;;; -------------------------------------------------------

;; Print "Hello, World!"
;; 
;; This program initializes the first 13 cells with the ASCII codes of
;; the output message "Hello, World!" in reverse order:
;;   
;;   cell[1]  = "!"
;;   cell[2]  = "d"
;;   ...
;;   cell[12] = "e"
;;   cell[13] = "H"
;; 
;; It then sets the accumulator to the value 13, in concord with the
;; tally of letters (cells) to print, ere starting a jump-based
;; iteration in the course of which the cell indexed with the current
;; accumulator value
;;   
;;   cell[accumulator]
;; 
;; is printed, the accumulator reduced by one, and checked if still
;; positive. In the case of the predicate's satisfaction, the output and
;; deducation steps are repeated by jumping back to the respective line;
;; otherwise the jump instruction's exclusion terminates the program.
(interpret-Esomachine
  "INDEX_STATE[1, 1]
   INDEX_SET[1, 33]    \\ Set cell[1] to ASCII code of the character '!'.
   INDEX_STATE[1, 2]
   INDEX_SET[2, 100]   \\ Set cell[2] to ASCII code of the character 'd'.
   INDEX_STATE[1, 3]
   INDEX_SET[3, 108]   \\ Set cell[3] to ASCII code of the character 'l'.
   INDEX_STATE[1, 4]
   INDEX_SET[4, 114]   \\ Set cell[4] to ASCII code of the character 'r'.
   INDEX_STATE[1, 5]
   INDEX_SET[5, 111]   \\ Set cell[5] to ASCII code of the character 'o'.
   INDEX_STATE[1, 6]
   INDEX_SET[6, 87]    \\ Set cell[6] to ASCII code of the character 'W'.
   INDEX_STATE[1, 7]
   INDEX_SET[7, 32]    \\ Set cell[7] to ASCII code of the character ' '.
   INDEX_STATE[1, 8]
   INDEX_SET[8, 44]    \\ Set cell[8] to ASCII code of the character ','.
   INDEX_STATE[1, 9]
   INDEX_SET[9, 111]   \\ Set cell[9] to ASCII code of the character 'o'.
   INDEX_STATE[1, 10]
   INDEX_SET[10, 108]  \\ Set cell[10] to ASCII code of the character 'l'.
   INDEX_STATE[1, 11]
   INDEX_SET[11, 108]  \\ Set cell[11] to ASCII code of the character 'l'.
   INDEX_STATE[1, 12]
   INDEX_SET[12, 101]  \\ Set cell[12] to ASCII code of the character 'e'.
   INDEX_STATE[1, 13]
   INDEX_SET[13, 72]   \\ Set cell[13] to ASCII code of the character 'H'.
   HANDS_CONLANG[+, 13]
   OUTPUT[HANDS]         \\ Line 28
   HANDS_CONLANG[-, 1]
   HANDS_JUMP[POSITIVE, 28]")

;;; -------------------------------------------------------

;; Generate the Esomachine code for printing "Hello, World!" and write
;; it to the standard output.
(generate-output-code "Hello, World!")

;;; -------------------------------------------------------

;; Generate the Esomachine code for printing "Hello, World!", supply it
;; to the interpreter, and execute the program.
(interpret-Esomachine
  (generate-output-code "Hello, World!" :destination NIL))
