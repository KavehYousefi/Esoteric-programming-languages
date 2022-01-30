;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "cpy", invented by the Esolang useer "ZippyMagician".
;; 
;; Concept
;; =======
;; cpy, designed by the Esolang user "ZippyMagician", constitutes a
;; line-based esoteric programming language operating on a tape-like
;; memory by moving and copying its cell values and jumping betwixt
;; code lines.
;; 
;; == PROGRAMS OPERATE ON MEMORY CELLS ==
;; Program data is maintained in a global memory as a composite of an
;; arbitrary number of cells, each storing an unsigned byte, initialized
;; to zero (0), and responding to a signed integer index. The cells at
;; the position zero (0) and one (1) are assigned a particular purpose,
;; with the former providing for output and the latter furnished the
;; agency of querying user input.
;; 
;; == DATA CAN BE COPIED OR MOVED ==
;; Two modes of data transfer are capacitated: copying a value from one
;; cell to another and moving in the same fashion. While the first
;; action effectively duplicates the cell states, the second instruction
;; resets the source to zero as a concomitant to the overwriting of the
;; destination. In addition, the motion either decrements or increments
;; the destination cell by the amount of one (1) following the
;; operation: If the source address is greater than the destination
;; address, a deduction is applied, otherwise an increase eventuates.
;; 
;; == JUMPING AS A CONTROL FLOW ==
;; A third, very potent capability is manifested in the jump
;; instruction. Being a line-based language, with a single command as
;; the maximum of a row's allowance, a conditional jump to a desired
;; line can be specified, provided that the test address exceeds the
;; value of zero (0). This construct introduces simultaneously a
;; conditional and an iterative facility.
;; 
;; 
;; Architecture
;; ============
;; Data castaldy in cpy revolves around the concept of a tape-like
;; memory, arranged as a series of cells which respond to indices for
;; their selection, each containing an unsigned octet value. The cells
;; are initialized to zero (0).
;; 
;; The specification induces no imposition upon the cells' tally,
;; issuing however a recommendation concerning a minimum of 2^16 units.
;; Lacking an explicit constraint on the enumeration policy, positive as
;; well as negative subscripts shall be homologated to refer to the
;; memory cells.
;; 
;; 
;; Data Types
;; ==========
;; Data in the language is expressed either in the form of signed
;; integer literals, bifurcated in their utilization into addresses and
;; line indices, or in the guise of unsigned bytes stored as the memory
;; content. Character input and output defines an optional aspect of the
;; numeric data.
;; 
;; == INTEGERS DESIGNATE ADDRESSES ==
;; The most frequent application of the integer type is realized in the
;; statement of memory addresses. cpy's liberal design permits negative
;; indexing, as such integers may assume any size and magnitude.
;; 
;; == INTEGERS REFER TO LINE NUMBERS ==
;; In the context of the "jump" command, integer numbers experience
;; their agency as zero-based line index designators.
;; 
;; == UNSIGNED BYTES BUILD THE MEMORY ==
;; All data stored in a cpy program relies on the internally managed
;; memory, a series of cells arranged akin to a tape and amenable to
;; integer subscripts known as addresses, which please see. A cell
;; holds at any time a single unsigned byte of eight bits, initially
;; apportioned the value zero (0).
;; 
;; == CHARACTERS MAY OPERATE ON THE INTERFACE ==
;; The communication from the user to program and vice versa is
;; permitted to unfold either by numeric input and output or by
;; adminicle of ASCII characters in any direction.
;; 
;; == THE MEMORY IS A CELL VECTOR ==
;; The stewardship of program data is assigned to the purview of a
;; tape-like memory, composed of an arbitrary number of cells, each
;; storing an unsiged octet value and able to be selected by an address
;; in the guise of a signed integer.
;; 
;; 
;; Syntax
;; ======
;; cpy programs admit any character, yet administering attention only
;; to digits and operators, the latter represented by arrow-like
;; structures composed of hyphens "-" and angular brackets "<" and ">".
;; Instructions are separated by linebreaks.
;; 
;; == INSTRUCTIONS ==
;; The sole tokens apportioned significance in conjunction with effect
;; encompass operators and operands, compounded into instructions.
;; 
;; Operands always assume the form of signed integer numbers, that is,
;; a series of one or more digits, optionally preceded by a single minus
;; "-" or plus "+" sign.
;; 
;; Operators are expressed in a composition of hyphens "-", the less
;; than symbol "<" and greater than glyph ">", resolving to one of three
;; types: "->", "<-", and "<->".
;; 
;; All operations subsume into the binary ilk, arranged according to the
;; infix notation, with the left operand preceding the operator, this
;; itself introduced the right operand.
;; 
;; == NEWLINES ==
;; Besides instructions, being compounds of two operands surrounding one
;; operator, newline characters bear significance, but lack effect. Each
;; instruction must be contained on a source code line of its own,
;; permitted to share its vertical space with ignored contents; this
;; requires the separation of any two instructions by at least one
;; newline character.
;; 
;; == OTHER CHARACTERS ==
;; Any other character and character combination is vouchsafed
;; tolerance --- and negligence.
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (EBNF) description of the syntax,
;; barring any ignored characters, manifests in the following:
;;   
;;   program     := [ newlines ] ,
;;                  instruction ,
;;                  { newlines , instruction } ,
;;                  [ newlines ] ;
;;   instruction := integer , operator , integer ;
;;   operator    := "->" | "<-" | "<->" ;
;;   integer     := ["+" | "-"] , digit , { digit } ;
;;   digit       := "0" | "1" | "2" | "3" | "4"
;;               |  "5" | "6" | "7" | "8" | "9" ;
;;   newlines    := newline , { newline } ;
;;   newline     := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; cpy accommodates the programmer with three types of instructions:
;; the copying of cell data, the moving of the same, and a jumping
;; to a target line. The incipient twain involves in its capabilities
;; as a byproduct the capacity for input and output, while the move
;; command additionally decrements or increments the destination cell.
;; The jump operation introduces the ability to define conditional and
;; iterative effects.
;; 
;; == OVERVIEW ==
;; An apercu concerning the rather scant instruction set shall be
;; adduced in the following.
;; 
;;   Instruction  | Effect
;;   -------------+----------------------------------------------------
;;    {S} -> {D}  | Copies the value of the cell with the address {S}
;;                | to the cell with the address {D}, and resets the
;;                | value of the former cell to zero (0).
;;                | If the address {S} is greater than {D}, the value
;;                | of the cell at {D} is decremented by one; otherwise
;;                | it is incremented by one.
;;                | If {S} resolves to one (1), the user is queried for
;;                | an input to transfer to the cell at {D}. Depending
;;                | on the implementation, either a number is prompted
;;                | and directly written, or a character is queried and
;;                | its ASCII character utilized.
;;                | If {D} resolves to zero (0), the value, following
;;                | its deduction or increase, will be printed to the
;;                | standard output, either as a number or as the
;;                | associated ASCII character.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    {S} <-> {D} | Copies the value of the cell with the address {S}
;;                | to the cell with the address {D}.
;;                | If the address {S} is greater than {D}, the value
;;                | of the cell at {D} is decremented by one; otherwise
;;                | it is incremented by one.
;;                | If {S} resolves to one (1), the user is queried for
;;                | an input to copy to the cell at {D}. Depending
;;                | on the implementation, either a number is prompted
;;                | and directly written, or a character is queried and
;;                | its ASCII character utilized.
;;                | If {D} resolves to zero (0), the value, following
;;                | its deduction or increase, will be printed to the
;;                | standard output, either as a number or as the
;;                | associated ASCII character.
;;   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;    {L} <- {A}  | If the cell with the address {A} is greater than
;;                | zero (0), the instruction pointer relocates to the
;;                | start of the {L}-th line, with the line numbering
;;                | starting with zero (0). If the condition is not
;;                | satisfied, no effect is exercised.
;; 
;; 
;; Implementation
;; ==============
;; The interpretation of cpy code proceeds in concord with acquainted
;; principles, generating from the source program by a lexer's mediation
;; a stream of tokens, which are combined into instructions by a parser,
;; and finally processed using an interpreter.
;; 
;; == A CPY PROGRAM IS RENDERED TO TOKENS ==
;; Tokens encapsulate significant portions of a source program in a
;; form appropriate to the particular intention. A sequence of digits,
;; for instance, might be construed as an integer number. Each token
;; is a composite of two objects: a categorizing type and the recognized
;; value.
;; 
;; == THE LEXER PRODUCES TOKENS FROM SOURCE CODE ==
;; The analyzation and token generation process is an ambit allotted to
;; a lexer entity. Storing a reference to the source code, the current
;; location in the same, and the character residing at this position,
;; the lexer responds upon each query with the subsequently discovered
;; token.
;; 
;; In the case of cpy, a very restricted compass of possible tokens
;; already exhausts the contingency. The following table shall explicate
;; this set:
;; 
;;   Datum          | Token type | Token value
;;   ---------------+------------+---------------------------
;;    digit[1..*]   | :number    | The parsed integer number.
;;    "->"          | :operator  | :move
;;    "<->"         | :operator  | :copy
;;    "<-"          | :operator  | :jump
;;    newline       | :newline   | #\Newline
;;    end of source | :eof       | NIL
;; 
;; == THE PARSER: A COMBINATOR OF TOKENS ==
;; As a constructive entity, the parser generates from the lexer's token
;; stream a program, while concomitantly assaying the validity of its
;; receipts.
;; 
;; In its most abstract delineation, a parser produces an abstract
;; syntax tree, or abbreviated AST, a hierarchy of nodes encapsulating
;; the language constructs. This mete of intricacy would effectuate an
;; otiose instance of supererogation in the context of cpy, a language
;; kenspeckle with its utter simplicity. In its stead, a vector of
;; ``Instruction'' objects, designating the possible commands, as well
;; as empty lines in order to correctly map line indices for jumping
;; instructions, is constructed. The following correlations hold:
;; 
;;   cpy instruction | ``Instruction'' type
;;   ----------------+---------------------
;;    ->             | :move
;;    <->            | :copy
;;    <-             | :jump
;;    newline        | :nop
;; 
;; == THE INTERPRETER: THE INSTRUCTION VECTOR APPLIED ==
;; The instruction vector is finally transmitted to the interpreter, the
;; participating unit responsible for admittance of actual effect to the
;; such processed source code. The functioning is realized by avail of
;; three components associated with a cpy program:
;;   
;;   (1) The instructions vector, the cardinality and indexing of which
;;       correlates exactly to the lines of the cpy source code.
;;   (2) An instruction pointer storing the index into the instructions
;;       vector. Tallying commensurate to the cpy source code lines, an
;;       element of the instruction pointer is tantamount to a row in
;;       the source code. In ordinary operations, each applied
;;       instruction simply increases this index, thus relating to the
;;       next command. Merely the jump operation "<-" may, under its
;;       condition's satisfactions, redirect the pointer to a particular
;;       line index.
;;   (3) The memory, implemented as a sparse structure of the hash table
;;       kind, with each key assuming the role of a cell index and the
;;       associated table value representing the octet cell content. A
;;       theoretically infinite memory vector is thus modeled, where
;;       cells not explicitly defined default to the value zero (0).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-01-27
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Cpy"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, the keys of which conform to the KEY-TYPE, while the values
   assume the VALUE-TYPE, both defaulting to ``T''."
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

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   adjacent bits."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "A ``Token'' represents a significant portion of a cpy source code
   as a recognized and analyzed unit."
  (type  NIL :type (or null keyword))
  (value NIL :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "The lexer requires a source.")
    :type          string
    :documentation "The cpy program code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character pointed to by the POSITION in the
                    source."))
  (:documentation
    "The ``Lexer'' analyzes a piece of cpy code, discerns its tokens,
     and returns these upon querying."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (when (and (< position (length source)))
      (setf character (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' operating on the cpy SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character, if possible, and returns the
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

(defun lexer-read-integer (lexer)
  "Starting at the LEXER's current location, reads a contingently signed
   integer value, relocates the position cursor to the first character
   following the recognized number, and returns its value."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the integer
      (parse-integer
        (with-output-to-string (digits)
          (when (find character "+-" :test #'char=)
            (write-char character digits)
            (lexer-advance lexer))
          (loop while (and character (digit-char-p character)) do
            (write-char character digits)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-peek (lexer)
  "Returns the next character in the source maintained by the LEXER,
   or ``NIL'' if the source is exhausted, without relocating the LEXER's
   position cursor."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the (or null character)
      (when (< position (1- (length source)))
        (char source (1+ position))))))

;;; -------------------------------------------------------

(defun lexer-character-follows-p (lexer expected-character)
  "Checks whether, starting at the LEXER's current location, the
   EXPECTED-CHARACTER follows, on ascertainment returning a ``boolean''
   value of ``T'', otherwise ``NIL''.
   ---
   Note that this operation does not move the LEXER's position cursor,
   instead it only peeks into the next character."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((next-character (lexer-peek lexer)))
      (declare (type (or null character) next-character))
      (the boolean
        (not (null
          (and character
               (char= next-character expected-character))))))))

;;; -------------------------------------------------------

(defun lexer-digit-follows-p (lexer)
  "Checks whether, starting at the LEXER's current location, a digit
   character follows, on ascertainment returning a ``boolean'' value of
   ``T'', otherwise ``NIL''.
   ---
   Note that this operation does not move the LEXER's position cursor,
   instead it only peeks into the next character."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((next-character (lexer-peek lexer)))
      (declare (type (or null character) next-character))
      (the boolean
        (not (null
          (and character
               (digit-char-p next-character))))))))

;;; -------------------------------------------------------

(defun lexer-string-follows-p (lexer expected-string)
  "Checks whether, starting at the LEXER's current location, the
   EXPECTED-STRING's character occur in immediate succession, on
   ascertainment relocating its position cursor to the character in the
   LEXER's source immediately following the discovered sequence and
   returning a ``boolean'' value of ``T'', otherwise resting the cursor
   at the location at the start of this operation's invocation and
   returning ``NIL''."
  (declare (type Lexer  lexer))
  (declare (type string expected-string))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (let ((start-position position))
      (declare (type fixnum start-position))
      (the boolean
        (loop
          for expected-character
            of-type character
            across  expected-string
          do
            (cond
              ((and character (char= character expected-character))
                (lexer-advance lexer))
              (T
                (setf position  start-position)
                (setf character (char source position))
                (return NIL)))
          finally
            (return T))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   If the underlying source is exhausted, the LEXER returns upon each
   invocation a new instance of an ``:eof''-typed token, signifying the
   end of the code."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ;; End of code?
        ((null character)
          (make-token :eof NIL))
        
        ;; Number follows?
        ((digit-char-p character)
          (make-token :number (lexer-read-integer lexer)))
        
        ;; "+", digit?
        ((and (char= character #\+)
              (lexer-digit-follows-p lexer))
          (make-token :number (lexer-read-integer lexer)))
        
        ;; "-", digit?
        ((and (char= character #\-)
              (lexer-digit-follows-p lexer))
          (make-token :number (lexer-read-integer lexer)))
        
        ;; Move operator "->"?
        ((and (char= character #\-)
              (lexer-character-follows-p lexer #\>))
          (lexer-advance lexer)
          (lexer-advance lexer)
          (make-token :operator :move))
        
        ;; Copy operator "<->"?
        ((lexer-string-follows-p lexer "<->")
          (make-token :operator :copy))
        
        ;; Jump operator "<-"?
        ((lexer-string-follows-p lexer "<-")
          (make-token :operator :jump))
        
        ;; Newline?
        ((char= character #\Newline)
          (lexer-advance lexer)
          (make-token :newline #\Newline))
        
        ;; Skip non-instruction characters.
        (T
          (lexer-advance        lexer)
          (lexer-get-next-token lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type
                                  &optional
                                    (left-operand  0)
                                    (right-operand 0))))
  "An ``Instruction'' represents a compound of an operation together
   with its two operands."
  (type          NIL :type (or null keyword))
  (left-operand  0   :type integer)
  (right-operand 0   :type integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "The parser must be initialized with a lexer.")
    :type          Lexer
    :documentation "The lexer whose tokens shall be evaluated.")
   (current-token
    :initarg       :current-token
    :initform      NIL
    :type          (or null Token)
    :documentation "The last token queried from the lexer."))
  (:documentation
    "The ``Parser'' class builds from a stream of tokens, furnished by
     a ``Lexer'', a sequence of instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (setf (slot-value parser 'current-token)
    (lexer-get-next-token
      (slot-value parser 'lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' relying on tokens furnished by
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (if (eq (token-type current-token) expected-token-type)
      (setf current-token
        (lexer-get-next-token (slot-value parser 'lexer)))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the tokens obtained by the PARSER's internally managed lexer,
   and returns a vector of ``Instruction'' objects."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((instructions NIL))
      (declare (type list instructions))
      (loop do
        (case (token-type current-token)
          
          ((NIL)
            (loop-finish))
          
          (:eof
            (loop-finish))
          
          (:newline
            (parser-eat parser :newline)
            (push (make-instruction :nop) instructions))
          
          (:number
            (let ((left-operand  NIL)
                  (operator      NIL)
                  (right-operand NIL))
              (declare (type (or null Token) left-operand))
              (declare (type (or null Token) operator))
              (declare (type (or null Token) right-operand))
              
              (setf left-operand current-token)
              (parser-eat parser :number)
              
              (setf operator current-token)
              (parser-eat parser :operator)
              
              (setf right-operand current-token)
              (parser-eat parser :number)
              
              (case (token-type current-token)
                (:newline
                  (parser-eat parser :newline))
                (:eof
                  (parser-eat parser :eof))
                (otherwise
                  (error "Expected newline or EOF following ~
                          instruction, but encountered ~s."
                    current-token)))
              
              (push
                (make-instruction
                  (token-value operator)
                  (token-value left-operand)
                  (token-value right-operand))
                instructions)))
          
          (otherwise
            (error "Cannot parse the token ~s." current-token))))
      
      (the (vector Instruction *)
        (coerce (nreverse instructions) '(vector Instruction *))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (make-array 0
                     :element-type    'Instruction
                     :initial-element (make-instruction :nop))
    :type          (vector Instruction *)
    :documentation "Maintains a sequence of instructions, representing
                    empty lines by a NOP (no-operation).")
   (instruction-pointer
    :initarg       :instruction-pointer
    :initform      0
    :type          fixnum
    :documentation "The zero-based index into the current element of
                    the INSTRUCTIONS.")
   (memory
    :initarg       :memory
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer octet)
    :documentation "Represents the cpy program's memory by a sparse
                    structure, mapping to each cell index a byte value.
                    ---
                    The cpy specification recommends a minimum of
                    2^16 cells, but retains obmutescence about negative
                    indices, such that the contingently adscititious
                    liberty may avail this implementation's users."))
  (:documentation ""))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' operating on the
   INSTRUCTIONS."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the program maintained by the INTERPRETER and returns the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions instruction-pointer memory) interpreter
    (declare (type (vector Instruction *)        instructions))
    (declare (type fixnum                        instruction-pointer))
    (declare (type (hash-table-of integer octet) memory))
    
    (labels
        ((cell-at (address)
          "Returns the value of the cell amenable to the ADDRESS."
          (declare (type integer address))
          (the octet (gethash address memory 0)))
         
         ((setf cell-at) (new-value address)
          "Sets the value of the cell amenable to the ADDRESS to the
           NEW-VALUE and returns no value."
          (declare (type octet   new-value))
          (declare (type integer address))
          (setf (gethash address memory 0) new-value)
          (values))
         
         (clear-cell-at (address)
          "Sets the value of the cell amenable to the ADDRESS to the
           default value of zero (0) and returns no value."
          (declare (type integer address))
          (setf (cell-at address) 0)
          (values))
         
         (print-cell-at (address)
          "Prints the value of the cell amenable to the ADDRESS to the
           standard output and returns no value."
          (declare (type integer address))
          (write (code-char (cell-at address)) :escape NIL)
          (values))
         
         (ensure-octet (value)
          "Ensures that the integer VALUE occupies the valid byte range
           of [0, 255] by potentially clamping it to this interval and
           returns either the already suitable VALUE or its translated
           rendition."
          (declare (type integer value))
          (the octet (max (min value 255) 0)))
         
         (adjust-destination-cell (source-address destination-address)
          "Determines the relations betwixt the SOURCE-ADDRESS and the
           DESTINATION-ADDRESS, and if the former exceeds the latter,
           the value of the cell at the DESTINATION-ADDRESS is
           decremented by one, otherwise it is incremented by the same
           amount, in case returning no value."
          (declare (type integer source-address))
          (declare (type integer destination-address))
          (if (> source-address destination-address)
            (decf (cell-at destination-address))
            (incf (cell-at destination-address)))
          (values))
         
         (read-input ()
          "Prompts the user for a character, reads it, and returns its
           ASCII code as a byte value."
          (format T "~&Please input a character: ")
          (let ((input (read-char)))
            (declare (type character input))
            (clear-input)
            (the octet (ensure-octet (char-code input))))))
      
      (let ((current-instruction NIL))
        (declare (type (or null Instruction) current-instruction))
        
        (loop while (< instruction-pointer (length instructions)) do
          (setf current-instruction
            (aref instructions instruction-pointer))
          
          (case (instruction-type current-instruction)
            ;; [X] -> [Y]   { Write memory[X] to memory[Y]. }
            ;; [X] ->  0    { Write memory[X] to output. }
            ;;  1  -> [Y]   { Write input to memory[Y]. }
            ;;  1  ->  0    { Write input to output. }
            (:move
              (let ((source-address
                      (instruction-left-operand current-instruction))
                    (destination-address
                      (instruction-right-operand current-instruction)))
                (declare (type integer source-address))
                (declare (type integer destination-address))
                
                (cond
                  ;; 1 -> 0.
                  ((and (= source-address      1)
                        (= destination-address 0))
                    (let ((input (read-input)))
                      (declare (type octet input))
                      (setf (cell-at destination-address) input)
                      (adjust-destination-cell
                        source-address
                        destination-address)
                      (print-cell-at destination-address)
                      (clear-cell-at source-address)))
                  
                  ;; [X] -> 0
                  ((and (/= source-address      1)
                        (=  destination-address 0))
                    (setf (cell-at destination-address)
                          (cell-at source-address))
                    (adjust-destination-cell
                      source-address
                      destination-address)
                    (print-cell-at destination-address)
                    (clear-cell-at source-address))
                  
                  ;; 1 -> [Y].
                  ((and (=  source-address      1)
                        (/= destination-address 0))
                    (let ((input (read-input)))
                      (declare (type octet input))
                      (setf (cell-at destination-address) input)
                      (adjust-destination-cell
                        source-address
                        destination-address)
                      (clear-cell-at source-address)))
                  
                  ;; [X] -> [Y]
                  ((and (/= source-address      1)
                        (/= destination-address 0))
                    (setf (cell-at destination-address)
                          (cell-at source-address))
                    (adjust-destination-cell
                      source-address
                      destination-address)
                    (clear-cell-at source-address))
                  
                  (T
                    (error "Invalid combination of source address ~
                            ~d and destination address ~d."
                      source-address destination-address))))
              
              (incf instruction-pointer))
            
            ;; [X] <-> [Y]   { Write memory[X] to memory[Y]. }
            ;; [X] <->  0    { Write memory[X] to output. }
            ;;  1  <-> [Y]   { Write input to memory[Y]. }
            ;;  1  <->  0    { Write input to output. }
            (:copy
              (let ((source-address
                      (instruction-left-operand current-instruction))
                    (destination-address
                      (instruction-right-operand current-instruction)))
                (declare (type integer source-address))
                (declare (type integer destination-address))
                
                (cond
                  ;; 1 <-> 0.
                  ((and (= source-address      1)
                        (= destination-address 0))
                    (let ((input (read-input)))
                      (declare (type octet input))
                      (setf (cell-at destination-address) input)
                      (print-cell-at destination-address)))
                  
                  ;; [X] <-> 0
                  ((and (/= source-address      1)
                        (=  destination-address 0))
                    (setf (cell-at destination-address)
                          (cell-at source-address))
                    (print-cell-at destination-address))
                  
                  ;; 1 <-> [Y].
                  ((and (=  source-address      1)
                        (/= destination-address 0))
                    (let ((input (read-input)))
                      (declare (type octet input))
                      (setf (cell-at destination-address) input)))
                  
                  ;; [X] <-> [Y]
                  ((and (/= source-address      1)
                        (/= destination-address 0))
                    (setf (cell-at destination-address)
                          (cell-at source-address)))
                  
                  (T
                    (error "Invalid combination of source address ~
                            ~d and destination address ~d."
                      source-address destination-address))))
              
              (incf instruction-pointer))
            
            ;; X <- [Y]
            (:jump
              (let ((destination-row
                      (instruction-left-operand current-instruction))
                    (test-address
                      (instruction-right-operand current-instruction)))
                (declare (type integer destination-row))
                (declare (type integer test-address))
                (if (plusp (gethash test-address memory))
                  (setf instruction-pointer destination-row)
                  (incf instruction-pointer))))
            
            ;; An empty line.
            (:nop
              (incf instruction-pointer))
            
            (otherwise
              (error "Invalid instruction: ~s."
                current-instruction)))))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-cpy (code)
  "Interprets the piece of cpy CODE and returns no value."
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

;; Test moving.
(interpret-cpy "1->3
                3->0")

;;; -------------------------------------------------------

;; Test moving and output.
(interpret-cpy "1->3
                3->2
                2->0")

;;; -------------------------------------------------------

;; Cat program.
(interpret-cpy "1->3
                3->2
                2<->0
                0<-2")

;;; -------------------------------------------------------

;; Cat program which takes into account empty lines while jumping.
(interpret-cpy "
                1->3
                3->2
                2<->0
                
                1<-2")
