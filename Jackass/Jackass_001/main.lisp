;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Jackass", presented by the Esolang user "Phase" in the year
;; 2015, being, while inspired by Urban Mueller's "brainfuck", composed
;; of hexadecimal tokens only which apply themselves to the manipulation
;; of a stack.
;; 
;; 
;; Instructions
;; ============
;; Jackass's competence founds upon fifteen instructions reponsible for
;; a global stack's manipulation, each represented by a hexadecimal code
;; compact of one or more base-16 digits.
;; 
;; The instruction set's circumference registers a wide gamut of
;; capabilities:
;; 
;;   - Pushing and popping stack elements.
;;   - Swapping, rotating, and duplicating stack elements.
;;   - Basic arithmetics in the form of addition, subtraction,
;;     multiplication, division, and remainder computation.
;;   - Character-based and numeric input and output facilities.
;;   - A goto contruct for steering the control flow.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   01      | Pushes the next hexadecimal token to the top of the
;;           | stack.
;;   ..................................................................
;;   02      | Pops the top stack element.
;;   ..................................................................
;;   03      | Swaps the two top stack elements' positions.
;;   ..................................................................
;;   04      | Rotates the three stack elements, that is, relocates
;;           | the third highest element to the top of the stack, thus
;;           | effectively shifting down the two items previously above
;;           | it.
;;   ..................................................................
;;   05      | Pops the top stack element and relocates the instruction
;;           | pointer to the instruction at the zero-based position
;;           | equal to the deleted element's value.
;;   ..................................................................
;;   06      | Pushes a copy of the top stack element unto the top.
;;   ..................................................................
;;   07      | Peeks without removing the top stack element a and the
;;           | element b previously below it, and pushes the sum
;;           |   a + b
;;           | unto the stack.
;;   ..................................................................
;;   08      | Peeks without removing the top stack element a and the
;;           | element b previously below it, and pushes the difference
;;           |   a - b
;;           | unto the stack.
;;   ..................................................................
;;   09      | Peeks without removing the top stack element a and the
;;           | element b previously below it, and pushes the product
;;           |   a * b
;;           | unto the stack.
;;   ..................................................................
;;   0a      | Peeks without removing the top stack element a and the
;;           | element b previously below it, and pushes the
;;           | contingently rounded quotient
;;           |   a / b
;;           | unto the stack.
;;   ..................................................................
;;   0b      | Peeks without removing the top stack element a and the
;;           | element b previously below it, and pushes the remainder
;;           | r obtained by the division
;;           |   r = a mod b
;;           | unto the stack.
;;   ..................................................................
;;   0c      | Prints the top stack element as a decimal integer.
;;   ..................................................................
;;   0d      | Prints the character associated with the top stack
;;           | element, construed as an ASCII code.
;;   ..................................................................
;;   0e      | Queries the user for an unsigned decimal integer and
;;           | pushes it unto the stack.
;;   ..................................................................
;;   0f      | Queries the user for a string and, starting from its
;;           | left to its right end, pushes each of its characters'
;;           | ASCII codes unto the stack.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-21
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Jackass"
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
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype token ()
  "The ``token'' type defines a token as an unsigned non-negative
   integer."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a last-in first-out collection based upon
   a list of signed integer objects."
  '(list-of integer))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized operation types."
  '(member
    :push-next-value
    :pop
    :swap
    :rotate
    :goto
    :clone
    :add
    :subtract
    :multiply
    :divide
    :modulus
    :print-number
    :print-character
    :input-number
    :input-string))

;;; -------------------------------------------------------

(deftype jackass-program ()
  "The ``jackass-program'' type defines a Jackass program as a vector
   of zero or more ``Instruction'' objects."
  '(vector Instruction *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline #\Return #\Linefeed)
        :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Jackass source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The position into the SOURCE's current CHARACTER.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class' contribution encompasses the perception and
     extraction of significant elements from a piece of Jackass code,
     which are returned as a stream of tokens."))

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
  "Creates and returns a new ``Lexer'' which serves to analyze the
   SOURCE."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next character in its
   source, if possible, updates its internal state, and returns the
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
  "Starting at the current position into the LEXER's source, skips a
   sequence of zero or more adjacent whitespaces and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-expect-command-separator (lexer)
  "Expects the LEXER's current character to be either a whitespace, in
   which case all adjacent instances of the same are skipped, or the end
   of the source (EOF), on confirmation returning the contingently
   modified LEXER; otherwise signaling an error of an unspecified type."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (cond
      ((null character)
        NIL)
      ((whitespace-character-p character)
        (lexer-skip-whitespaces lexer))
      (T
        (error "Expected whitespace or end of file, ~
                but encountered ~s at position ~d."
          character position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-hexadecimal-number (lexer)
  "Starting at the current position into the LEXER's source, consumes an
   unsigned hexadecimal integer, parses the same, and returns its
   decimal value as a ``token''."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the token
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (loop while (and character (digit-char-p character 16)) do
            (write-char character digits)
            (lexer-advance lexer)))
        :radix 16))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to each request with
   the ``NIL'' value."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (the (or null token)
      (cond
        ((null character)
          NIL)
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        ((digit-char-p character 16)
          (lexer-read-hexadecimal-number lexer))
        (T
          (error "Unexpected character ~s at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (operand NIL))))
  "The ``Instruction'' class encapsulates the pertinent information for
   executing a Jackass operation, including an identifying command type
   specifier and an optional argument."
  (type    (error "Missing instruction type.") :type command)
  (operand NIL                                 :type (or null token)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-tokens (lexer)
  "Assembles the LEXER's tokens into a one-dimensional simple array of
   Jackass instructions, which are subsequently returned."
  (declare (type Lexer lexer))
  (let ((instructions NIL)
        (token        (lexer-get-next-token lexer)))
    (declare (type list            instructions))
    (declare (type (or null token) token))
    (flet
        ((advance ()
          "Requests the next token from the LEXER, stores it in the
           TOKEN, and returns no value."
          (setf token (lexer-get-next-token lexer))
          (values))
         
         (add-instruction (type &optional (operand NIL))
          "Creates a new ``Instruction'' of the specified type,
           optionally affiliated with an OPERAND, prepends it to the
           INSTRUCTIONS list, and returns no value."
          (push (make-instruction type operand) instructions)
          (values)))
      
      (loop while token do
        (case token
          (#x01
            (advance)
            (if token
              (add-instruction :push-next-value token)
              (error "Expected a number, but encountered EOF.")))
          (#x02
            (add-instruction :pop))
          (#x03
            (add-instruction :swap))
          (#x04
            (add-instruction :rotate))
          (#x05
            (add-instruction :goto))
          (#x06
            (add-instruction :clone))
          (#x07
            (add-instruction :add))
          (#x08
            (add-instruction :subtract))
          (#x09
            (add-instruction :multiply))
          (#x0a
            (add-instruction :divide))
          (#x0b
            (add-instruction :modulus))
          (#x0c
            (add-instruction :print-number))
          (#x0d
            (add-instruction :print-character))
          (#x0e
            (add-instruction :input-number))
          (#x0f
            (add-instruction :input-string))
          (otherwise
            (error "Invalid command code: ~x." token)))
        (advance)))
    
    (the (simple-array Instruction (*))
      (coerce (nreverse instructions)
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          jackass-program
    :documentation "The Jackass instructions to evaluate.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP), serving as the index
                    into the INSTRUCTIONS' currently active entry.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null Instruction)
    :documentation "The instruction under the instruction pointer (IP)
                    in the INSTRUCTIONS sequence.")
   (stack
    :initarg       :stack
    :initform      NIL
    :accessor      interpreter-stack
    :type          stack
    :documentation "A stack of signed integer numbers."))
  (:documentation
    "Appropriating the wike of a Jackass program evaluator, the
     ``Interpreter'' class processes a sequence of instructions,
     inducing actual effect into them."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type jackass-program       instructions))
    (declare (type fixnum                ip))
    (declare (type (or null Instruction) current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' whose responsibility
   assigns it to the evaluation of the Jackass INSTRUCTIONS."
  (declare (type jackass-program instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Moves the INTERPRETER's instruction pointer to the next instruction,
   if possible, updates its internal state, and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type jackass-program       instructions))
    (declare (type fixnum                ip))
    (declare (type (or null Instruction) current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions (1+ ip))
        (aref instructions (incf ip)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-goto (interpreter target)
  "Relocates the INTERPRETER's instruction pointer to the TARGET,
   updates its state, and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      target))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type jackass-program       instructions))
    (declare (type fixnum                ip))
    (declare (type (or null Instruction) current-instruction))
    (setf ip target)
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-instruction (interpreter
                                              instruction-type
                                              instruction)
  (:documentation
    "Processes the INSTRUCTION, identified by and dispatched on the
     INSTRUCTION-TYPE, by the INTERPRETER and returns no value."))

;;; -------------------------------------------------------

(defun interpreter-process-instruction (interpreter instruction)
  "Invokes the appropriate implementation of the generic function
   ``interpreter-dispatch-instruction'' by applying the INTERPRETER and
   the INSTRUCTINO, utilizing the INSTRUCTION type as the paravaunt
   dispatch criterion, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (interpreter-dispatch-instruction interpreter
    (instruction-type instruction) instruction)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :push-next-value))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (push (instruction-operand instruction)
        (interpreter-stack   interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :pop))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (pop (interpreter-stack   interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :swap))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (rotatef
    (first  (interpreter-stack   interpreter))
    (second (interpreter-stack   interpreter)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :rotate))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (rotatef
    (first  (interpreter-stack   interpreter))
    (third  (interpreter-stack   interpreter)))
  (rotatef
    (second (interpreter-stack   interpreter))
    (third  (interpreter-stack   interpreter)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :goto))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (interpreter-goto interpreter
    (pop (interpreter-stack interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :clone))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (push (first (interpreter-stack interpreter))
        (interpreter-stack interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :add))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (push (+ (first  (interpreter-stack interpreter))
           (second (interpreter-stack interpreter)))
        (interpreter-stack interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :subtract))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (push (- (first  (interpreter-stack interpreter))
           (second (interpreter-stack interpreter)))
        (interpreter-stack interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :multiply))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (push (* (first  (interpreter-stack interpreter))
           (second (interpreter-stack interpreter)))
        (interpreter-stack interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :divide))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (push (round (first  (interpreter-stack interpreter))
               (second (interpreter-stack interpreter)))
        (interpreter-stack interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :modulus))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (push (mod (first  (interpreter-stack interpreter))
             (second (interpreter-stack interpreter)))
        (interpreter-stack interpreter))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :print-number))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (format T "~d "
    (first
      (interpreter-stack interpreter)))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :print-character))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (format T "~d"
    (code-char
      (first
        (interpreter-stack interpreter))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :input-number))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (format T "~&Please input a number: ")
  (push (parse-integer (read-line))
        (interpreter-stack interpreter))
  (clear-input)
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :input-string))
     (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type command     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (declare (ignore           instruction))
  (format T "~&Please input a string: ")
  (let ((input (read-line)))
    (declare (type string input))
    (clear-input)
    (loop for character of-type character across input do
      (push (char-code character)
            (interpreter-stack interpreter))))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the Jackass program stored in the INTERPRETER and returns
   no value."
  (declare (type Interpreter interpreter))
  (loop while (slot-value interpreter 'current-instruction) do
    (interpreter-process-instruction interpreter
      (slot-value interpreter 'current-instruction)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Jackass (code)
  "Interprets the piece of Jackass CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parse-tokens
        (make-lexer code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time numeric cat program.
(interpret-Jackass "0e 0c")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program.
(interpret-Jackass "0e 0c 01 00 05")

;;; -------------------------------------------------------

;; Print the message "Hello, World!" by pushing each character's ASCII
;; code, printing its character representation, and subsequently popping
;; it.
(interpret-Jackass
  "
  01 48 0d 02
  01 65 0d 02
  01 6C 0d 02
  01 6C 0d 02
  01 6F 0d 02
  01 2C 0d 02
  01 20 0d 02
  01 57 0d 02
  01 6F 0d 02
  01 72 0d 02
  01 6C 0d 02
  01 64 0d 02
  01 21 0d 02
  ")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Apostils concerning the concept:
;; 
;;   Instruction indices occupy the range [0, 12].
;;     => REDUCER    = 12 = RETURN_POS - 1.
;;     => RETURN_POS = 13.
;;   
;;   -----------------------------------------------------------------------------------------------
;;    # |  Instruction    | Annotation                        | Stack (top element is on the left)
;;   ---------------------+-----------------------------------+-------------------------------------
;;    0 | input           |                                   | [input]
;;    1 | output          |                                   | [input]
;;    2 | push REDUCER    |                                   | [REDUCER input]
;;    3 | multiply        | product = REDUCER * input         | [product REDUCER input]
;;    4 | swap            | product <-> REDUCER               | [REDUCER product input]
;;    5 | pop             | REDUCER                           | [product input]
;;    6 | push RETURN_POS |                                   | [RETURN_POS product input]
;;    7 | subtract        | difference = RETURN_POS - product | [difference RETURN_POS product input]
;;    8 | swap            |                                   | [RETURN_POS difference product input]
;;    9 | pop             |                                   | [difference product input]
;;   10 | swap            |                                   | [product difference input]
;;   11 | pop             |                                   | [difference input]
;;   12 | goto            | difference                        | [input]
;;   ----------------------------------------------------------------------------------------------
(interpret-jackass
  "
  0e
  0c
  01 0c
  09
  03
  02
  01 0d
  08
  03
  02
  03
  02
  05
  ")
