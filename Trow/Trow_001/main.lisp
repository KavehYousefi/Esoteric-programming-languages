;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the joke language "Trow",
;; invented by the Esolang user "Jaipack17", and intended to generate
;; text message with a certain mete of variability based upon randomized
;; placeholder insertions.
;; 
;; An output-only language, Trow programs are restricted in their
;; capacity to the generation of display text only, applying themselves
;; to this task by lists whose items are inserted in a random manner
;; into the output strings in lieu of their respective placeholders.
;; 
;; Please note that this implementation, in the dearth of an explicit
;; delineation regarding the subject of spacings, permits spaces and
;; linebreaks to be allotted rather liberally.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-19
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Trow"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
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
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype lexer-state ()
  "The ``lexer-state'' enumerates the possible sections in which a lexer
   may be residing during its analyzation process."
  '(member :default :output))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction-type'' type enumerates the recognized types of
   Trow instructions."
  '(member
    :list
    :output))

;;; -------------------------------------------------------

(deftype operand-type ()
  "The ``operand-type'' type enumerates the recognized types of
   instruction operands."
  '(member
    :list-name
    :list-item
    :literal
    :placeholder))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant portion in an analyzed
   piece of Trow code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Trow source code to tokenize.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the Trow SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the Trow
                    SOURCE.
                    ---
                    If the POSITION cursor has exhausted the SOURCE by
                    moving past its desinence, the CHARACTER is rendered
                    to the ``NIL'' sentinel.")
   (state
    :initarg       :state
    :initform      :default
    :type          lexer-state
    :documentation "Determines whether the lexer analyzes an output
                    statement section or the general surroundings."))
  (:documentation
    "The ``Lexer'' class provides a unit responsible for the
     analyzation of a piece of Trow code and its division into a
     sequence of tokens."))

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
  "Creates and returns a new ``Lexer'' intended for analyzation of the
   SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's location cursor to the next character in its
   source, if possible, updates the LEXER's state, and returns the
   modified LEXER."
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

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current location in the LEXER's source, skips zero or
   more adjacent whitespace characters and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current location in the LEXER's source, skips a line
   of comment and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop until (or (null character) (char= character #\Newline)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-list-name (lexer)
  "Starting at the current location in the LEXER's source, reads a list
   name and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (lexer-advance lexer)
    (the Token
      (make-token :list-name
        (with-output-to-string (text)
          (declare (type string-stream text))
          (loop do
            (case character
              ((NIL)
                (error "Unterminated list name."))
              (#\>
                (error "Invalid character in list name: \"~c\"."
                  character))
              (#\]
                (lexer-advance lexer)
                (loop-finish))
              (otherwise
                (write-char character text)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-read-list-item (lexer)
  "Starting at the current location in the LEXER's source, reads a list
   item and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (lexer-advance lexer)
    (the Token
      (make-token :list-item
        (with-output-to-string (text)
          (declare (type string-stream text))
          (loop do
            (case character
              ((NIL)
                (error "Unterminated list item."))
              (#\"
                (lexer-advance lexer)
                (loop-finish))
              (otherwise
                (write-char character text)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-start-output (lexer)
  "Starting at the current location in the LEXER's source, reads the
   start of an output section and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (prog1
        (make-token :start-output character)
        ;; Skip the introducing hyphen "-".
        (lexer-advance lexer)
        ;; Skip potential space following the hyphen "-".
        (when (and character (char= character #\Space))
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-read-output-literal (lexer)
  "Starting at the current location in the LEXER's source, reads a
   literal text fragment contributing to an output operation and returns
   a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character state) lexer
    (declare (type (or null character) character))
    (declare (type lexer-state         state))
    (the Token
      (make-token :output-literal
        (with-output-to-string (text)
          (declare (type string-stream text))
          (loop do
            (case character
              ;; Output ends with the source code.
              ((NIL)
                (setf state :default)
                (loop-finish))
              ;; Output ends.
              (#\Newline
                (setf state :default)
                (loop-finish))
              ;; Placeholder starts.
              (#\<
                (loop-finish))
              ;; Comment starts.
              (#\#
                (loop-finish))
              (otherwise
                (write-char character text)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-read-output-placeholder (lexer)
  "Starting at the current location in the LEXER's source, reads a
   placeholder fragment contributing to an output operation and returns
   a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character state) lexer
    (declare (type (or null character) character))
    (declare (type lexer-state         state))
    (lexer-advance lexer)
    (the Token
      (make-token :output-placeholder
        (with-output-to-string (text)
          (declare (type string-stream text))
          (loop do
            (case character
              ((NIL)
                (error "Unterminated output placeholder."))
              ;; Output ends.
              (#\Newline
                (setf state :default)
                (loop-finish))
              ;; Placeholder ends.
              (#\>
                (lexer-advance lexer)
                (loop-finish))
              (otherwise
                (write-char character text)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER."
  (declare (type Lexer lexer))
  (with-slots (character state) lexer
    (declare (type (or null character) character))
    (declare (type lexer-state         state))
    (case state
      (:default
        (cond
          ((null character)
            (make-token :eof NIL))
          
          ((whitespace-character-p character)
            (lexer-skip-whitespaces lexer)
            (lexer-get-next-token   lexer))
          
          ((char= character #\#)
            (lexer-skip-comment   lexer)
            (lexer-get-next-token lexer))
          
          ((char= character #\[)
            (lexer-read-list-name lexer))
          
          ((char= character #\")
            (lexer-read-list-item lexer))
          
          ((char= character #\-)
            (setf state :output)
            (lexer-start-output lexer))
          
          (T
            (error "Invalid character \"~c\" at position ~d."
              character (slot-value lexer 'position)))))
      
      (:output
        (case character
          ((NIL)
            (setf state :default)
            (make-token :eof NIL))
          
          (#\#
            (lexer-skip-comment   lexer)
            (lexer-get-next-token lexer))
          
          (#\Newline
            (setf state :default)
            (lexer-get-next-token lexer))
          
          (#\<
            (lexer-read-output-placeholder lexer))
          
          (otherwise
            (lexer-read-output-literal lexer))))
      
      (otherwise
        (error "Invalid state ~s with character \"~c\" at position ~d."
          state character (slot-value lexer 'position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type value)))
  "The ``Operand'' class represents an argument to a Trow instruction."
  (type  (error "Missing operand type.") :type operand-type)
  (value NIL                             :type (or null string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type)))
  "The ``Instruction'' class encapsulates the information requisite for
   the representation of a Trow statement."
  (type
    (error "Missing instruction type.")
    :type instruction-type)
  (operands
    NIL
    :type (list-of Operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The lexer responsible for the provision of tokens.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently obtained token from the LEXER."))
  (:documentation
    "The ``Parser'' class constitutes the responsible unit for the
     assemblage of an instruction vector from a sequence of tokens
     obtained from a lexer."))

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
  "Creates and returns a ``Parser'' which obtains its tokens from the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the current token stored in the PARSER conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation loading the next token into the
   PARSER and returning the modified PARSER, otherwise signaling an
   error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (if (token-type-p current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-list (parser)
  "Parses a list definition using the PARSER and returns an
   ``Instruction'' representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (let ((list-name-token current-token))
      (declare (type Token list-name-token))
      (parser-eat parser :list-name)
      
      (let ((list-definition (make-instruction :list))
            (items           NIL))
        (declare (type Instruction       list-definition))
        (declare (type (list-of Operand) items))
        
        (loop while (token-type-p current-token :list-item) do
          (push (make-operand :list-item (token-value current-token))
                items)
          (parser-eat parser :list-item))
        
        (setf items (nreverse items))
        
        ;; Prepend the list name as the incipient operand.
        (push
          (make-operand :list-name (token-value list-name-token))
          items)
        
        (setf (instruction-operands list-definition) items)
        
        (the Instruction list-definition)))))

;;; -------------------------------------------------------

(defun parser-parse-output (parser)
  "Parses an output operation using the PARSER and returns an
   ``Instruction'' representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (parser-eat parser :start-output)
    
    (let ((output    (make-instruction :output))
          (fragments NIL))
      (declare (type Instruction       output))
      (declare (type (list-of Operand) fragments))
      
      (loop do
        (case (token-type current-token)
          (:output-literal
            (push (make-operand :literal (token-value current-token))
                  fragments)
            (parser-eat parser :output-literal))
          
          (:output-placeholder
            (push (make-operand :placeholder
                    (token-value current-token))
                  fragments)
            (parser-eat parser :output-placeholder))
          
          (otherwise
            (loop-finish))))
      
      (setf (instruction-operands output)
            (nreverse fragments))
      
      (the Instruction output))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the Trow program represented by its tokens using the PARSER
   and returns a one-dimensional simple array of instructions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    
    (let ((instructions NIL))
      (declare (type (list-of Instruction) instructions))
      
      (loop do
        (case (token-type current-token)
          (:eof
            (loop-finish))
          
          (:start-output
            (push (parser-parse-output parser) instructions))
          
          (:list-name
            (push (parser-parse-list parser) instructions))
          
          (otherwise
            (loop-finish))))
      
      (parser-eat parser :eof)
      
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
    :documentation "A vector of instructions to process.")
   (lists
    :initarg       :lists
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string (vector string *))
    :documentation "Associates with each list name a vector of strings
                    representing its items."))
  (:documentation
    "The ``Interpreter'' class processes a vector of instructions in
     to apply actual effect to the same."))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns an ``Interpreter'' operating on the
   INSTRUCTIONS."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-get-random-item (interpreter list-name)
  "Returns a random list item associated with the LIST-NAME in the
   INTERPRETER.
   ---
   If the LIST-NAME exists but is associated with an empty sequence of
   items, the empty string \"\" is returned. If the LIST-NAME cannot be
   detected inside of the INTERPRETER, an error is signaled."
  (declare (type Interpreter interpreter))
  (declare (type string      list-name))
  (with-slots (lists) interpreter
    (declare (type hash-table lists))
    (multiple-value-bind (items contains-list-p)
        (gethash list-name lists)
      (declare (type (or null (vector string *)) items))
      (declare (type T                           contains-list-p))
      (the string
        (cond
          ((and contains-list-p
                (plusp (length items)))
            (aref items (random (length items))))
          ((and contains-list-p
                (zerop (length items)))
            "")
          (T
            (error "Unrecognized list name ~s." list-name)))))))

;;; -------------------------------------------------------

(defun extract-list-item-values (list-items-operands)
  "Extracts from the LIST-ITEMS-OPERANDS the list item strings and
   returns a vector containing these.
   ---
   An error of an unspecified type is signaled if any of the operands
   does not subsume into the ``:list-item'' type."
  (declare (type (list-of Operand) list-items-operands))
  (the (vector string *)
    (map '(vector string *)
      #'(lambda (item)
          (declare (type Operand item))
          (the string
            (if (eq (operand-type item) :list-item)
              (operand-value item)
              (error "Unexpected list item operand: ~s." item))))
      list-items-operands)))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Process the instructions stored in the INTERPRETER and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  
  (setf *random-state* (make-random-state T))
  
  (with-slots (instructions lists) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type hash-table             lists))
    
    (loop for instruction of-type Instruction across instructions do
      (case (instruction-type instruction)
        (:list
          (destructuring-bind (list-name &rest list-items)
              (instruction-operands instruction)
            (declare (type Operand           list-name))
            (declare (type (list-of Operand) list-items))
            (setf (gethash (operand-value list-name) lists)
                  (extract-list-item-values list-items))))
        
        (:output
          (format T "~&")
          
          (dolist (fragment (instruction-operands instruction))
            (declare (type Operand fragment))
            (case (operand-type fragment)
              (:literal
                (format T "~a" (operand-value fragment)))
              
              (:placeholder
                (format T "~a"
                  (interpreter-get-random-item interpreter
                    (operand-value fragment))))
              
              (otherwise
                (error "Invalid operand type for output in operand ~s."
                  fragment)))))
        
        (otherwise
          (error "Invalid instruction: ~s." instruction)))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Trow (code)
  "Interprets the piece of Trow CODE and returns no value."
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

;; "Randomized Hello World Program" example.
(interpret-Trow
  "[words]
   \"world\"
   \"programmers\"
   \"guys\"
   \"friends\"
   
   - Hello <words>!")

;;; -------------------------------------------------------

;; "Multiple Lists" example.
(interpret-Trow
  "[days]
   \"saturday\"
   \"sunday\"
   
   [seasons]
   \"winter\"
   \"summer\"
   
   - Today is <days>, a beautiful <seasons> morning!")

;;; -------------------------------------------------------

;; "Multiple Outputs" example.
(interpret-Trow
  "[food]
   \"pizza\"
   \"burger\"
   
   - I love eating <food>s!
   - <food>s are bad for our health!")

;;; -------------------------------------------------------

;; "Lists" example.
(interpret-Trow
  "[subjects]
   \"mathematics\"
   \"physics\"
   \"chemistry\"
   \"art\"")

;;; -------------------------------------------------------

;; "Print" example.
(interpret-Trow
  "[emotions]
   \"happy\"
   \"sad\"
   \"angry\"
   
   - Hey, this statement will be printed in the console! This statement does not include random items from lists!
   - I am so <emotions>. # In the output, <emotions> is replaced with a random item from the list.")

;;; -------------------------------------------------------

;; Output a text referencing an empty list.
(interpret-Trow
  "[foods]
   
   - I like <foods>.")
