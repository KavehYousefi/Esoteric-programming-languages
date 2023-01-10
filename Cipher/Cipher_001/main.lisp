;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Cipher", presented by the Esolang user "ChuckEsoteric08" in
;; the year 2022, the programs of which are encoded in a single number,
;; comprehending operations that manipulate an integer-valued stack.
;; 
;; 
;; Concept
;; =======
;; The Cipher programming language operates on a stack of signed
;; integers, applying to this cause a series of numeric commands, whose
;; entirety is encoded in a single numeric value.
;; 
;; 
;; Decoding
;; ========
;; Decoding of Cipher programs involves the expansion of its number form
;; into the embedded instructions.
;; 
;; A Cipher instruction sequence's obtention from its numeric encoding
;; constitutes the coefficiency of several stages, its inchoation
;; defined by deduction of the quantity five (5) and a division by
;; seventeen (17). The resulting number's digits are traversed pairwise,
;; with each consecutive twain multiplied by three (3) in order to
;; determine the Cipher command's numeric code. These items'
;; conquisition already presents the decoded instruction sequence.
;; 
;; The just elucidated decoding process is limned in this pseudocode:
;; 
;;   let cipherCommand = {"pushOne", "add", "subtract", "multiply",
;;                        "divide", "duplicate", "startLoop", "endLoop",
;;                        "output"}
;;   
;;   function getCommand (code)
;;     Input:
;;       code --- a positive integer Cipher command code
;;     
;;     Output:
;;       the cipherCommand associated with the CODE
;;     
;;     Calculation:
;;       if code = 33 then
;;         return "pushOne"
;;       else if code = 42 then
;;         return "add"
;;       else if code = 51 then
;;         return "subtract"
;;       else if code = 66 then
;;         return "multiply"
;;       else if code = 87 then
;;         return "divide"
;;       else if code = 90 then
;;         return "duplicate"
;;       else if code = 96 then
;;         return "startLoop"
;;       else if code = 99 then
;;         return "endLoop"
;;       else if code = 3 then
;;         return "output"
;;       else
;;         signal error: "Invalid command code: {code}."
;;       end if
;;   end function
;;   
;;   function decodeInstructions (cipherCode)
;;     Input:
;;       cipherCode --- the Cipher program conveyed as a number
;;     
;;     Output
;;       instructions --- a list of the decoded Cipher instructions,
;;                        with each member instructions[x] being an
;;                        element of the cipherCommand type
;;     
;;     Calculation:
;;       let instructions <- empty list
;;       let preparedCode <- (cipherCode - 5) / 17
;;       
;;       for i from 1 to number of digits in the preparedCode by 2 do
;;         let digitPair   <- preparedCode[i, i+1] * 3
;;         let instruction <- getCommand(digitPair)
;;         
;;         append instruction to instructions
;;       end for
;;       
;;       return instructions
;;   end function
;; 
;; 
;; Encoding
;; ========
;; The encoding concept in Cipher involves the transcription of an
;; instruction sequence into a numeric value.
;; 
;; This endeavor's constituents involve the division of each
;; instruction, always represented by a positive integer datum, by the
;; number three (3), the concatenation of the resulting digits into a
;; single quantity, which is multiplied by seventeen (17), and finally
;; the product's augmentation by five (5) into the concluding code.
;; 
;; The following pseudocode describes the encoding process in more
;; detail:
;; 
;;   let cipherCommand = {"pushOne", "add", "subtract", "multiply",
;;                        "divide", "duplicate", "startLoop", "endLoop",
;;                        "output"}
;;   
;;   function getCommandCode (command)
;;     Input:
;;       command --- a Cipher command
;;     
;;     Output:
;;       programCode --- the command's positive integer code
;;     
;;     Calculation:
;;       if command is "pushOne" then
;;         return 33
;;       else if command is "add" then
;;         return 42
;;       else if command is "subtract" then
;;         return 51
;;       else if command is "multiply" then
;;         return 66
;;       else if command is "divide" then
;;         return 87
;;       else if command is "duplicate" then
;;         return 90
;;       else if command is "startLoop" then
;;         return 96
;;       else if command is "endLoop" then
;;         return 99
;;       else if command is "output" then
;;         return 3
;;       else
;;         signal error "Unrecognized command: {command}."
;;       end if
;;   end function
;;   
;;   function encodeInstructions (instructions)
;;     Input:
;;       instructions --- a sequence of Cipher commands
;;     
;;     Output:
;;       The instructions encoded in a single non-negative integer
;;     
;;     Calculation:
;;       let digitsString <- empty string
;;       
;;       for command c in instructions do
;;         let commandCode   <- getCommandCode(c)
;;         let encodedCode   <- commandCode / 3
;;         let formattedCode <- convert encodedCode to string, padded to
;;                              two digits in length with leading zeroes
;;         
;;         append formattedCode to digitsString
;;       end
;;       
;;       let parsedDigits <- convert digitsString to integer
;;       let programCode  <- (parsedDigits * 17) + 5
;;       
;;       return programCode
;;   end function
;; 
;; 
;; Instructions
;; ============
;; Cipher's instruction set is compact of nine members, operating on a
;; global stack of integers.
;; 
;; The faculties comprehend basic arithmetics, a few direct stack
;; manipulation routines, an iteration facility, and a numeric output
;; command. Its status as an output-only language excludes it from the
;; provision of an input conduit.
;; 
;; == OVERVIEW ==
;; The following apercu shall educate about the language's operations in
;; in a concise manner:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   3       | Outputs the top stack element in its numeric form.
;;   ..................................................................
;;   33      | Pushes the value one (1) unto the stack.
;;   ..................................................................
;;   42      | Pops the elements "a" and "b" in this order, adds them,
;;           | and pushes the sum
;;           |   a + b
;;           | unto the stack.
;;   ..................................................................
;;   51      | Pops the elements "a" and "b" in this order, subtracts
;;           | them, and pushes the difference
;;           |   b - a
;;           | unto the stack.
;;   ..................................................................
;;   66      | Pops the elements "a" and "b" in this order, multiples
;;           | them, and pushes the product
;;           |   a * b
;;           | unto the stack.
;;   ..................................................................
;;   87      | Pops the elements "a" and "b" in this order, divides
;;           | them, and pushes the quotient
;;           |   b / a
;;           | unto the stack.
;;   ..................................................................
;;   90      | Duplicates the top stack element.
;;   ..................................................................
;;   96      | Starts a loop which repeats until the top stack element
;;           | equals zero (0).
;;   ..................................................................
;;   99      | Terminates the currently active loop.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-30
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Cipher"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, of zero or more entries, each element of which either
   represents the ``NIL'' value or a cons with an object of the KEY-TYPE
   in its left compartment, associated with a value of the VALUE-TYPE in
   the dextral moiety, both defaulting to ``T''."
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
                  (or (null element)
                      (typep element `(cons ,key-type ,value-type))))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

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

(deftype command ()
  "The ``command'' type enumerates the recognized Cipher instruction
   types."
  '(member
    :push-1
    :add
    :subtract
    :multiply
    :divide
    :duplicate
    :start-loop
    :end-loop
    :output))

;;; -------------------------------------------------------

(deftype command-code ()
  "The ``command-code'' type enumerates the recognized numeric command
   codes associates with the Cipher commands."
  '(member 33 42 51 66 87 90 96 99 3))

;;; -------------------------------------------------------

(deftype command-table ()
  "The ``command-table'' associates each Cipher command with its
   identifying code, and vice versa, employing an association list for
   this purpose."
  '(association-list-of command command-code))

;;; -------------------------------------------------------

(deftype cipher-program ()
  "The ``cipher-program'' type defines a plaintext Cipher program as a
   list of zero or more commands."
  '(list-of command))

;;; -------------------------------------------------------

(deftype operation-list ()
  "The ``operation-list'' type defines a list of zero or more
   ``Operation'' objects."
  '(list-of Operation))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based last-in-first-out storage of
   signed integer objects."
  '(list-of integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command table operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-table +COMMAND-CODES+))

;;; -------------------------------------------------------

(defparameter +COMMAND-CODES+
  '((:push-1     . 33)
    (:add        . 42)
    (:subtract   . 51)
    (:multiply   . 66)
    (:divide     . 87)
    (:duplicate  . 90)
    (:start-loop . 96)
    (:end-loop   . 99)
    (:output     .  3))
  "Associates the command objects with their representative numeric
   codes.")

;;; -------------------------------------------------------

(defun get-command (command-code)
  "Returns the command associated with the COMMAND-CODE, or signals an
   error of an unspecified type is no correspondence exists."
  (declare (type command-code))
  (the command
    (or (car (rassoc command-code +COMMAND-CODES+ :test #'=))
        (error "No command associated with code ~d." command-code))))

;;; -------------------------------------------------------

(defun get-command-code (command)
  "Returns the identifying code for the COMMAND, or signals an error of
   an unspecified type if no such correspondence exists."
  (declare (type command command))
  (the command-code
    (or (cdr (assoc command +COMMAND-CODES+ :test #'eq))
        (error "No command code defined for ~s." command))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of encoder.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encode-instructions-into-digits (instructions)
  "Converts each instruction in the INSTRUCTIONS vector into a two-digit
   number, based upon the command code divided by three, and returns a
   string compact of these digit twains in the given order."
  (declare (type cipher-program instructions))
  (the string
    (with-output-to-string (digits)
      (declare (type string-stream digits))
      (loop for instruction of-type command in instructions do
        (format digits "~2,'0d"
          (/ (get-command-code instruction) 3))))))

;;; -------------------------------------------------------

(defun encode-digits (digits)
  "Encodes the DIGITS, being a string representation of an unsigned
   integer number containing the command codes divided by three and
   padded two a length of two places, into the final, encoded integer
   object form."
  (declare (type string digits))
  (the (integer 0 *)
    (+ (* (parse-integer digits) 17) 5)))

;;; -------------------------------------------------------

(defun encode-instructions (instructions)
  "Encodes the Cipher INSTRUCTIONS as an unsigned integer object."
  (declare (type cipher-program instructions))
  (the (integer 0 *)
    (encode-digits
      (encode-instructions-into-digits instructions))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of decoder.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-digits (digits)
  "Extracts and returns from the DIGITS, being a string representation
   of Cipher program's numeric form, a list of commands."
  (declare (type string digits))
  (let ((number-of-digits (length digits)))
    (declare (type fixnum number-of-digits))
    (flet
        ((extract-digit-twain (start)
          "Beginning at the START position into the DIGITS, extracts the
           two consecutive digits and returns these as an integer
           object."
          (declare (type fixnum start))
          (the integer
            (parse-integer digits
              :start start
              :end   (min (+ start 2) number-of-digits)))))
      (the cipher-program
        (loop
          for position
            of-type fixnum
            from    0
            below   number-of-digits by 2
          collect
            (get-command
              (* (extract-digit-twain position)
                 3)))))))

;;; -------------------------------------------------------

(defun decode-number (number)
  "Extracts and returns from the Cipher program's numeric representation
   NUMBER a list of commands."
  (declare (type integer number))
  (the cipher-program
    (decode-digits
      (format NIL "~a"
        (abs (round (- number 5) 17))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operation".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operation
  (:constructor make-operation (type &optional (body NIL))))
  "The ``Operation'' class furnishes an intermediate representation (IR)
   of the Cipher instructions in the form defined by superior cohension,
   in particular combining loop start and end commands, as well as the
   comprehended statements, in a single operational unit, thus
   ultimately forming a simplified equivalent of an abstract syntax tree
   (AST)."
  (type (error "Missing operation type.") :type command)
  (body NIL                               :type operation-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Compiler".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Compiler
  (:constructor make-compiler (instructions
                               &aux
                                (instructions
                                  (copy-list instructions)))))
  "The ``Compiler'' class implements a transformator from the separate
   Cipher instructions into the more convenient ``Operation'' units,
   accommodating the interpreter with an enhanced command sequence."
  (instructions NIL :type cipher-program))

;;; -------------------------------------------------------

(defun compiler-peek-instruction (compiler)
  "Returns without removing the first COMPILER instruction, or ``NIL''
   if no such exists."
  (declare (type Compiler compiler))
  (the (or null command)
    (first (compiler-instructions compiler))))

;;; -------------------------------------------------------

(defun compiler-pop-instruction (compiler)
  "Removes and returns the first COMPILER instruction, or ``NIL'' if no
   such exists."
  (declare (type Compiler compiler))
  (the (or null command)
    (pop (compiler-instructions compiler))))

;;; -------------------------------------------------------

(defun compiler-compile-next-instruction (compiler)
  "Compiles the next instruction maintained by the COMPILER and returns
   an ``Operation'' representation thereof."
  (declare (type Compiler compiler))
  (let ((current-instruction (compiler-pop-instruction compiler)))
    (declare (type command current-instruction))
    (the Operation
      (case current-instruction
        (:start-loop
          (make-operation :start-loop
            (compiler-build-loop-body compiler)))
        
        (:end-loop
          (error "Unmatched loop end."))
        
        ((:push-1 :add :subtract :multiply :divide :duplicate :output)
          (make-operation current-instruction))
        
        (otherwise
          (error "Unrecognized command: ~s." current-instruction))))))

;;; -------------------------------------------------------

(defun compiler-build-loop-body (compiler)
  "Collects the instructions in the COMPILER, transformed into
   ``Operation'' objects, until an ``end-loop'' command has been
   encountered, and returns the thus produced list of operations."
  (declare (type Compiler compiler))
  (the operation-list
    (loop
      with body of-type operation-list = NIL
      do
        (case (compiler-peek-instruction compiler)
          ((NIL)
            (error "Unterminated loop construct."))
          (:end-loop
            (compiler-pop-instruction compiler)
            (loop-finish))
          (T
            (push (compiler-compile-next-instruction compiler) body)))
      finally
        (return (nreverse body)))))

;;; -------------------------------------------------------

(defun compiler-compile (compiler)
  "Transforms and returns the COMPILER's instructions as a list of
   ``Operation'' objects."
  (declare (type Compiler compiler))
  (the operation-list
    (loop
      while   (compiler-instructions compiler)
      collect (compiler-compile-next-instruction compiler))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (operations)))
  "The ``Interpreter'' class provides the means for the processing of a
   list of operations representing a Cipher program."
  (operations NIL :type operation-list)
  (stack      NIL :type stack))

;;; -------------------------------------------------------

(defun interpreter-stack-peek (interpreter)
  "Returns without removing the top element on the INTERPRETER's stack,
   or signals an error of an unspecified type if the same is empty."
  (declare (type Interpreter interpreter))
  (the integer
    (or (first (interpreter-stack interpreter))
        (error "Cannot pop from an empty stack."))))

;;; -------------------------------------------------------

(defun interpreter-stack-pop (interpreter)
  "Removes and returns the top element from the INTERPRETER's stack, or
   signals an error of an unspecified type if the same is empty."
  (declare (type Interpreter interpreter))
  (the integer
    (or (pop (interpreter-stack interpreter))
        (error "Cannot pop from an empty stack."))))

;;; -------------------------------------------------------

(defun interpreter-stack-push (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's stack and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-value))
  (push new-value (interpreter-stack interpreter))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defgeneric dispatch-operation (interpreter command operation)
  (:documentation
    "Processes the OPERATION, identified by the COMMAND type, employing
     the INTERPRETER's context, and returns no value."))

;;; -------------------------------------------------------

(defun process-operation (interpreter operation)
  "Invokes the suitable ``dispatch-operation'' method implementation for
   the OPERATION based upon its command type, utilizing the INTERPRETER
   as the incipient parameter, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Operation   operation))
  (dispatch-operation interpreter (operation-type operation) operation)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :push-1))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (declare (ignore           operation))
  (interpreter-stack-push interpreter 1)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :add))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (declare (ignore           operation))
  (let ((a (interpreter-stack-pop interpreter))
        (b (interpreter-stack-pop interpreter)))
    (declare (type integer a))
    (declare (type integer b))
    (interpreter-stack-push interpreter (+ a b)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :subtract))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (declare (ignore           operation))
  (let ((a (interpreter-stack-pop interpreter))
        (b (interpreter-stack-pop interpreter)))
    (declare (type integer a))
    (declare (type integer b))
    (interpreter-stack-push interpreter (- b a)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :multiply))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (declare (ignore           operation))
  (let ((a (interpreter-stack-pop interpreter))
        (b (interpreter-stack-pop interpreter)))
    (declare (type integer a))
    (declare (type integer b))
    (interpreter-stack-push interpreter (* a b)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :divide))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (declare (ignore           operation))
  (let ((a (interpreter-stack-pop interpreter))
        (b (interpreter-stack-pop interpreter)))
    (declare (type integer a))
    (declare (type integer b))
    (interpreter-stack-push interpreter (round b a)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :start-loop))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (loop until (zerop (interpreter-stack-peek interpreter)) do
    (dolist (statement (operation-body operation))
      (declare (type Operation statement))
      (process-operation interpreter statement)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-operation ((interpreter Interpreter)
                               (command     (eql :output))
                               (operation   Operation))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (type Operation   operation))
  (declare (ignore           command))
  (declare (ignore           operation))
  (format T "~d " (interpreter-stack-peek interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-operations (interpreter)
  (declare (type Interpreter interpreter))
  (dolist (operation (interpreter-operations interpreter))
    (declare (type Operation operation))
    (process-operation interpreter operation))
  (values))

;;; -------------------------------------------------------

(defun interpret-Cipher-instructions (instructions)
  "Interprets the Cipher INSTRUCTIONS and returns no value."
  (declare (type cipher-program instructions))
  (interpreter-process-operations
    (make-interpreter
      (compiler-compile
        (make-compiler instructions))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Cipher-string (code)
  "Interprets the piece of Cipher source CODE provided in the form of a
   string representing a signed integer and returns no value."
  (declare (type string code))
  (interpreter-process-operations
    (make-interpreter
      (compiler-compile
        (make-compiler
          (decode-digits code)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Cipher-number (code)
  "Interprets the piece of Cipher source CODE provided in the form of a
   signed integer and returns no value."
  (declare (type integer code))
  (interpreter-process-operations
    (make-interpreter
      (compiler-compile
        (make-compiler
          (decode-number code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Push the number one (1) twice, pop and add it, and print the sum (2).
(interpret-Cipher-instructions
  '(:push-1
    :push-1
    :add
    :output))

;;; -------------------------------------------------------

;; Push the number one (1) twice, pop and add it, and print the sum (2).
(interpret-Cipher-number
  (encode-instructions
    '(:push-1
      :push-1
      :add
      :output)))

;;; -------------------------------------------------------

;; Push the number one (1) twice, pop and add it, and print the sum (2).
(interpret-Cipher-number 188893822)

;;; -------------------------------------------------------

;; Print the numbers 4, 3, 2, 1.
;; This is equivalent to the instruction sequence
;;   :push-1
;;   :push-1
;;   :add
;;   :push-1
;;   :add
;;   :push-1
;;   :add
;;   :start-loop
;;   :output
;;   :push-1
;;   :subtract
;;   :end-loop
(interpret-Cipher-number 1888939893989434418899466)
