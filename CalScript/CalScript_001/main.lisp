;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "CalScript", invented by the Esolang user "Vriskanon" in the
;; year 2015, the programs of which assume a design reminiscent of
;; risible intonations in multifarious variegations, while
;; simultaneously operating on a stack and a tape utilizing integer
;; data.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-20
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/CalScript"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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
   a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   entailing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype token ()
  "The ``token'' type enumerates the recognized variants of CalScript
   tokens."
  '(member :haa :hee :hoo :eof))

;;; -------------------------------------------------------

(deftype lexer ()
  "The ``lexer'' type defines a lexical analyzer, or lexer, as a niladic
   function which upon each query returns a token object."
  '(function () token))

;;; -------------------------------------------------------

(deftype token-list ()
  "The ``token-list'' type defines a list of zero or more tokens."
  '(list-of token))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of identifiers in a
   string form to representative tokens."
  '(hash-table-of string token))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized CalScript
   instructions."
  '(member
    :move-stack-top-to-bottom
    :move-stack-bottom-to-top
    :pop-from-stack
    :add-stack-items
    :subtract-stack-items
    :multiply-stack-items
    :divide-stack-items
    :if-stack-0-push-1
    :if-stack-0-push-0
    :print-stack
    :input-to-stack
    :move-pointer-left
    :move-pointer-right
    :increment-tape
    :decrement-tape
    :if-tape-0-set-1
    :if-tape-0-set-0
    :print-tape
    :input-to-tape
    :move-stack-top-to-tape
    :copy-stack-top-to-tape
    :move-tape-to-stack
    :copy-tape-to-stack
    :add-stack-to-tape
    :subtract-stack-from-tape
    :subtract-tape-from-stack))

;;; -------------------------------------------------------

(deftype instruction-table ()
  "The ``instruction-table'' type defines a mapping from a sequence of
   three or four command tokens to an instructino identifier, modeled as
   a hash table."
  '(hash-table-of token-list instruction))

;;; -------------------------------------------------------

(deftype instruction-set ()
  "The ``instruction-set'' type defines a list of zero or more
   CalScript instructions."
  '(list-of instruction))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a last-in first-out data structure
   intended to maintain signed integer values, and modeled as a list
   of the same element type."
  '(list-of integer))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines a linearly arrangemend memory, compact of
   integer-valued cells, their tally extending bilaterally into
   infinity."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN is of the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token token))
  (declare (type token expected-type))
  (the boolean (not (null (eq token expected-type)))))

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Checks whether the TOKEN serves to represent a command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token token))
  (the boolean
    (not (null (member token '(:haa :hee :hoo) :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (let ((table (make-hash-table :test #'equal)))
    (declare (type identifier-table table))
    (setf (gethash "HAA" table) :haa)
    (setf (gethash "HEE" table) :hee)
    (setf (gethash "HOO" table) :hoo)
    (the identifier-table table))
  "Associates each of the three recognized identifier tokens with a
   ``token'' object.")

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token representation of the IDENTIFIER, or signals an
   error upon a disrepondency."
  (declare (type string identifier))
  (the token
    (or (gethash identifier +IDENTIFIERS+)
        (error "Unrecognized identifier: ~s." identifier))))

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER represents a constituent admissive to an
   identifier name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\H #\A #\E #\O) :test #'char=)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a lexer responsible for analyzing the CalScript
   SOURCE.
   ---
   The thus generated entity constitutes a niladic function which upon
   each inquisition returns a ``token'' datum."
  (declare (type string source))
  (let ((position 0)
        (character
          (when (array-in-bounds-p source 0)
            (char source 0))))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    
    (labels
        ((advance ()
          "Moves the POSITION cursor to the next character in the
           SOURCE, if possible, updates the current CHARACTER, and
           returns no value."
          (setf character
            (when (array-in-bounds-p source (1+ position))
              (char source (incf position))))
          (values))
         
         (read-identifier ()
          "Starting at the current POSITION, reads a command identifier
           and returns the associated token."
          (get-identifier-token
            (with-output-to-string (identifier)
              (declare (type string-stream identifier))
              (loop
                while (and character (identifier-character-p character))
                do
                  (write-char character identifier)
                  (advance)))))
         
         (skip-spaces ()
          "Starting at the current POSITION, skips a sequence of zero or
           more adjacent whitespaces and returns no value."
          (loop
            while (and character (whitespace-character-p character))
            do    (advance))
          (values))
         
         (read-next-token ()
          "Returns the next token from the SOURCE."
          (the token
            (cond
              ((null character)
                :eof)
              
              ((whitespace-character-p character)
                (skip-spaces)
                (read-next-token))
              
              ((char= #\H)
                (read-identifier))
              
              (T
                (error "Invalid character ~s at position ~d."
                  character position))))))
      
      (the function
        #'(lambda ()
            (the token
              (read-next-token)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   an end-of-file token ``:eof''."
  (declare (type lexer lexer))
  (the token (funcall lexer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of instruction table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type instruction-table +INSTRUCTIONS+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTIONS+
  (let ((table (make-hash-table :test #'equalp)))
    (declare (type instruction-table table))
    
    (flet
        ((add-instruction (tokens instruction)
          "Associates the list of TOKENS with the INSTRUCTION and
           returns no value."
          (declare (type token-list tokens))
          (declare (type instruction   instruction))
          (setf (gethash tokens table) instruction)
          (values)))
      
      (add-instruction '(:haa :haa :haa)      :move-stack-top-to-bottom)
      (add-instruction '(:haa :haa :hee)      :move-stack-bottom-to-top)
      (add-instruction '(:haa :haa :hoo)      :pop-from-stack)
      
      (add-instruction '(:haa :hee :haa :haa) :add-stack-items)
      (add-instruction '(:haa :hee :haa :hee) :subtract-stack-items)
      (add-instruction '(:haa :hee :hee :haa) :multiply-stack-items)
      (add-instruction '(:haa :hee :hee :hee) :divide-stack-items)
      
      (add-instruction '(:haa :hoo :haa :haa) :if-stack-0-push-1)
      (add-instruction '(:haa :hoo :haa :hee) :if-stack-0-push-0)
      
      (add-instruction '(:haa :hoo :hee :haa) :print-stack)
      (add-instruction '(:haa :hoo :hee :hee) :input-to-stack)
      
      (add-instruction '(:hee :haa :haa)      :move-pointer-left)
      (add-instruction '(:hee :haa :hee)      :move-pointer-right)
      
      (add-instruction '(:hee :hee :haa)      :increment-tape)
      (add-instruction '(:hee :hee :hee)      :decrement-tape)
      
      (add-instruction '(:hee :hoo :haa :haa) :if-tape-0-set-1)
      (add-instruction '(:hee :hoo :haa :hee) :if-tape-0-set-0)
      
      (add-instruction '(:hee :hoo :hee :haa) :print-tape)
      (add-instruction '(:hee :hoo :hee :hee) :input-to-tape)
      
      (add-instruction '(:hoo :haa :haa)      :move-stack-top-to-tape)
      (add-instruction '(:hoo :haa :hee)      :copy-stack-top-to-tape)
      (add-instruction '(:hoo :hee :haa)      :move-tape-to-stack)
      (add-instruction '(:hoo :hee :hee)      :copy-tape-to-stack)
      
      (add-instruction '(:hoo :hoo :haa)      :add-stack-to-tape)
      (add-instruction '(:hoo :hoo :hee)      :subtract-stack-from-tape)
      (add-instruction '(:hoo :hoo :hoo)      :subtract-tape-from-stack))
    
    (the instruction-table table))
  "Associates each compound of three or four identifier tokens with the
   respective instruction.")

;;; -------------------------------------------------------

(defun get-instruction (tokens)
  "Returns the instruction associated with the TOKENS or ``NIL'', if no
   such correspondence is defined."
  (declare (type token-list tokens))
  (the (or null instruction)
    (gethash tokens +INSTRUCTIONS+)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          lexer
    :documentation "The lexer responsible for the conveyance of
                    tokens.")
   (current-token
    :initarg       :current-token
    :initform      :eof
    :type          token
    :documentation "The most recently obtained token from the LEXER.")
   (token-list
    :initarg       :token-list
    :initform      NIL
    :type          token-list
    :documentation "The current list of command tokens consumed from the
                    lexer, and intended to be used as keys for looking
                    up the matching instruction in the +INSTRUCTIONS+
                    table."))
  (:documentation
    "The ``Parser'' class applies itself to the assemblage of a
     sequence of instructions from a series of tokens supplied through a
     lexer."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type lexer lexer))
    (declare (type token current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the parser Parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which assembles the tokens
   produced by the LEXER."
  (declare (type lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat-command (parser)
  "Checks whether the PARSER's current token represents a command, on
   confirmation loading and storing the next token from the internally
   managed lexer, while returning the probed token; on a mismatch, an
   error of an unspecified type is signaled.
   ---
   Please note that this function returns the tested token, not the
   newly requested one."
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type lexer lexer))
    (declare (type token current-token))
    (the token
      (cond
        ((command-token-p current-token)
          (prog1 current-token
            (setf current-token (lexer-get-next-token lexer))))
        (T
          (error "Expected a command token, but encountered ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-build-token-list (parser)
  "Collates the three consecutive tokens, starting with the current one
   residing in the PARSER, in a list, stores it in the PARSER, and
   returns the thus generated token list."
  (declare (type Parser parser))
  (with-slots (current-token token-list) parser
    (declare (type token      current-token))
    (declare (type token-list token-list))
    (setf token-list
      (list
        (parser-eat-command parser)
        (parser-eat-command parser)
        (parser-eat-command parser)))
    (the token-list token-list)))

;;; -------------------------------------------------------

(defun parser-extend-token-list (parser)
  "Appends to the PARSER's current token list the next token, expected
   to represent a command, updates the PARSER< and returns the expanded
   token list."
  (declare (type Parser parser))
  (with-slots (current-token token-list) parser
    (declare (type token      current-token))
    (declare (type token-list token-list))
    (setf token-list
      (append token-list
        (list (parser-eat-command parser))))
    (the token-list token-list)))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses a CalScript instruction, composed of three of four consecutive
   command tokens, and returns an ``instruction'' representation
   thereof.
   ---
   An error of an unspecified type is signaled if neither the incipient
   three nor the expanded four components relate to a recognized
   instruction."
  (declare (type Parser parser))
  (with-slots (current-token token-list) parser
    (declare (type token      current-token))
    (declare (type token-list token-list))
    
    (parser-build-token-list parser)
    
    (the instruction
      (or
        ;; Do the current three tokens represent an instruction?
        (get-instruction token-list)
        ;; Do the current four tokens represent an instruction?
        (get-instruction (parser-extend-token-list parser))
        ;; The three/four tokens do not amount to an instruction.
        (error "Unrecognized token sequence: ~s." token-list)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Assembles the tokens generated by the PARSER's internally managed
   lexer into a list of instructions and returns the same."
  (declare (type Parser parser))
  
  (let ((instructions NIL))
    (declare (type instruction-set instructions))
    
    (with-slots (current-token token-list) parser
      (declare (type token      current-token))
      (declare (type token-list token-list))
      
      (loop do
        (case current-token
          (:eof
            (loop-finish))
          
          ((:haa :hee :hoo)
            (push (parser-parse-instruction parser) instructions))
          
          (otherwise
            (error "Invalid token: ~s." current-token)))))
    
    (the instruction-set (nreverse instructions))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing instruction list.")
    :type          instruction-set
    :documentation "The list of instructions to process.")
   (stack
    :initarg       :stack
    :initform      NIL
    :type          stack
    :documentation "The program stack used to maintain integer
                    objects.")
   (tape
    :initarg       :tape
    :initform      (make-hash-table :test #'eql)
    :type          tape
    :documentation "The program tape, realized as a hash table,
                    associating with signed integer cell indices the
                    cell values of the same type; each entry thus
                    amounts to one cell.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          integer
    :documentation "The tape's cell pointer, storing the index, or
                    hash table key, of the currently selected cell."))
  (:documentation
    "The ``Interpreter'' class provides an entity responsible for the
     induction of effect into an instruction sequence."))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' operating on the
   INSTRUCTIONS."
  (declare (type instruction-set instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Applies the INTERPRETER to its internally managed instructions and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  
  (with-slots (instructions stack tape pointer) interpreter
    (declare (type instruction-set instructions))
    (declare (type stack           stack))
    (declare (type tape            tape))
    (declare (type integer         pointer))
    
    (flet
        ((current-cell ()
          "Returns the value stored in the current cell."
          (the integer (gethash pointer tape 0)))
         
         ((setf current-cell) (new-value)
          "Stores the NEW-VALUE into the current cell and returns no
           value."
          (declare (type integer new-value))
          (setf (gethash pointer tape 0) new-value)
          (values)))
      
      (loop
        for instruction of-type instruction   in   instructions
        and ip          of-type (integer 0 *) from 0
        do
          (case instruction
            ;; HAA HAA HAA
            (:move-stack-top-to-bottom
              (when stack
                (let ((top-element (pop stack)))
                  (declare (type integer top-element))
                  (setf stack
                    (append stack (list top-element))))))
            
            ;; HAA HAA HEE
            (:move-stack-bottom-to-top
              (when stack
                (let ((bottom-element (first (last stack))))
                  (declare (type integer bottom-element))
                  (setf stack (nbutlast stack))
                  (push bottom-element stack))))
            
            ;; HAA HAA HOO
            (:pop-from-stack
              (pop stack))
            
            ;; HAA HEE HAA HAA
            (:add-stack-items
              (push (+ (pop stack) (pop stack)) stack))
            
            ;; HAA HEE HAA HEE
            (:subtract-stack-items
              (push (- (pop stack) (pop stack)) stack))
            
            ;; HAA HEE HEE HAA
            (:multiply-stack-items
              (push (* (pop stack) (pop stack)) stack))
            
            ;; HAA HEE HEE HEE
            (:divide-stack-items
              (push (round (pop stack) (pop stack)) stack))
            
            ;; HAA HOO HAA HAA
            (:if-stack-0-push-1
              (when stack
                (if (zerop (first stack))
                  (push 1 stack)
                  (push 0 stack))))
            
            ;; HAA HOO HAA HEE
            (:if-stack-0-push-0
              (when stack
                (if (zerop (first stack))
                  (push 0 stack)
                  (push 1 stack))))
            
            ;; HAA HOO HEE HAA
            (:print-stack
              (write-char (code-char (pop stack))))
            
            ;; HAA HOO HEE HEE
            (:input-to-stack
              (format T "~&Please input a character: ")
              (push (char-code (read-char)) stack)
              (clear-input))
            
            ;; HEE HAA HAA
            (:move-pointer-right
              (incf pointer))
            
            ;; HEE HAA HEE
            (:move-pointer-left
              (decf pointer))
            
            ;; HEE HEE HAA
            (:increment-tape
              (incf (current-cell)))
            
            ;; HEE HEE HEE
            (:decrement-tape
              (decf (current-cell)))
            
            ;; HEE HOO HAA HAA
            (:if-tape-0-set-1
              (setf (current-cell)
                (if (zerop (current-cell))
                  1
                  0)))
            
            ;; HEE HOO HAA HEE
            (:if-tape-0-set-0
              (setf (current-cell)
                (if (zerop (current-cell))
                  0
                  1)))
            
            ;; HEE HOO HEE HAA
            (:print-tape
              (write-char (code-char (current-cell))))
            
            ;; HEE HOO HEE HEE
            (:input-to-tape
              (format T "~&Please input a character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input))
            
            ;; HOO HAA HAA
            (:move-stack-top-to-tape
              (setf (current-cell)
                    (pop stack)))
            
            ;; HOO HAA HEE
            (:copy-stack-top-to-tape
              (setf (current-cell)
                    (first stack)))
            
            ;; HOO HEE HAA
            (:move-tape-to-stack
              (push (current-cell) stack)
              (setf (current-cell) 0))
            
            ;; HOO HEE HEE          
            (:copy-tape-to-stack
              (push (current-cell) stack))
            
            ;; HOO HOO HAA
            (:add-stack-to-tape
              (incf (current-cell)
                    (pop stack))
              (push (current-cell) stack))
            
            ;; HOO HOO HEE
            (:subtract-stack-from-tape
              (decf (current-cell)
                    (pop stack))
              (push (current-cell) stack))
            
            ;; HOO HOO HOO
            ;; 
            ;; Pseudocode:
            ;;   let topElement <- stack.pop()
            ;;   let difference <- topElement - currentCell.value
            ;;   stack.push(difference)
            ;;   currentCell.value <- difference
            (:subtract-tape-from-stack
              (decf (first stack)
                    (current-cell))
              (setf (current-cell)
                    (first stack)))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                instruction ip))))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-CalScript (code)
  "Interprets the piece of CalScript CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-text-program (text
                              &key (destination         NIL)
                                   (increment-separator #\Newline)
                                   (transfer-prefix     #\Newline)
                                   (output-prefix       #\Newline)
                                   (output-suffix       #\Newline))
  "Generates a CalScript program capable of printing the TEXT to the
   standard output, writes the thus produced code to the DESTINATION,
   and returns for a non-``NIL'' DESTINATION the ``NIL'' value,
   otherwise responding with a fresh string comprehending the result.
   ---
   The resulting program incorporates multiple CalScript facilities,
   including the
     
     - incrementing of the current cell value
     - moving of the current cell value unto the stack
     - printing and removal of the top stack value
   
   The following pseudocode shall delineate the approach:
   
     for each character c in the TEXT
       let d <- ASCII code for character c
       
       repeat d times
         increment the current tape cell
       end repeat
       
       { Simultaneously resets the cell value to zero (0). }
       move the current tape cell value to the top of the stack
       
       pop and print the top stack value as an ASCII character
     end for
   ---
   Several options permit the configuration of the program's design:
   
     INCREMENT-SEPARTOR:
       The content to interpose betwixt two tape cell increment
       instructions \"HEE HEE HAA\".
     
     TRANSFER-PREFIX:
       The content to prepend to the cell-to-stack value transfer
       instruction \"HOO HEE HAA\".
     
     OUTPUT-PREFIX:
       The content to prepend to the stack top print instruction
       \"HAA HOO HEE HAA\".
     
     OUTPUT-SUFFIX:
       The content to append to the stack top print instruction
       \"HAA HOO HEE HAA\".
   
   The ultimate output thus assumes a forbisen as this for each TEXT
   character:
   
     HEE HEE HAA
     <INCREMENT-SEPERATOR>
     HEE HEE HAA
     <INCREMENT-SEPERATOR>
     ...
     HEE HEE HAA
     <TRANSFER-PREFIX>
     HOO HEE HAA
     <OUTPUT-PREFIX>
     HAA HOO HEE HAA
     <OUTPUT-SUFFIX>"
  (declare (type string      text))
  (declare (type destination destination))
  (declare (type T           increment-separator))
  (declare (type T           transfer-prefix))
  (declare (type T           output-prefix))
  (declare (type T           output-suffix))
  
  (the (or null string)
    (if destination
      (flet
          ((print-increment-separator ()
            "Writes the INCREMENT-SEPARATOR to the DESTINATION and
             returns no value."
            (format destination "~a" increment-separator)
            (values))
           
           (print-transfer-prefix ()
            "Writes the TRANSFER-PREFIX to the DESTINATION and returns
             no value."
            (format destination "~a" transfer-prefix)
            (values))
           
           (print-output-prefix ()
            "Writes the OUTPUT-PREFIX to the DESTINATION and returns no
             value."
            (format destination "~a" output-prefix)
            (values))
           
           (print-output-suffix ()
            "Writes the OUTPUT-SUFFIX to the DESTINATION and returns no
             value."
            (format destination "~a" output-suffix)
            (values)))
      
        (loop for character of-type character across text do
          ;; Reproduce the CHARACTER's code in the current tape cell.
          (loop
            repeat (char-code character)
            for first-increment-p of-type boolean = T then NIL
            do
              (unless first-increment-p
                (print-increment-separator))
              (format destination "HEE HEE HAA"))
          
          (print-transfer-prefix)
          
          ;; Push the current cell value unto the stack and
          ;; concomitantly reset the current cell to zero (0).
          (format destination "HOO HEE HAA")
          
          (print-output-prefix)
          
          ;; Pop and print the top stack element.
          (format destination "HAA HOO HEE HAA")
          
          (print-output-suffix)))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program text
          :destination output
          :increment-separator increment-separator
          :transfer-prefix     transfer-prefix
          :output-prefix       output-prefix
          :output-suffix       output-suffix)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A one-time cat program.
(interpret-CalScript "HEE HOO HEE HEE
                      HEE HOO HEE HAA")

;;; -------------------------------------------------------

;; Print the text "Hello, World!" to the standard output.
(interpret-CalScript
  "
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA HEE HEE HAA 
  HOO HEE HAA
  HAA HOO HEE HAA
  ")

;;; -------------------------------------------------------

;; Generate a CalScript program capable of printing the text
;; "Hello, World!" and print the source code to the standard output.
(generate-text-program "Hello, World!"
  :destination         T
  :increment-separator #\Space
  :transfer-prefix     #\Newline
  :output-prefix       #\Space
  :output-suffix       #\Newline)

;;; -------------------------------------------------------

;; Generate a CalScript program capable of printing the text
;; "Hello, World!" and write the source code to a text file.
(with-open-file (output "CalScriptOutput.txt"
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create)
  (declare (type file-stream output))
  (generate-text-program "Hello, World!" :destination output))

;;; -------------------------------------------------------

;; Generate a CalScript program capable of printing the text
;; "Hello, World!" and interpret the source code.
(interpret-CalScript
  (generate-text-program "Hello, World!"))
