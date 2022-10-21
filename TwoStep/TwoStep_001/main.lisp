;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "TwoStep", designed by the Esolang user "Macgeek" in the
;; year 2006, which represents each instruction as a twain of two
;; characters forming a command name-argument compound.
;; 
;; Architecture
;; ============
;; The data castaldy in TwoStep amounts to a single memory unit in the
;; form of an unsigned byte value, hence occupying the integer range
;; [0, 255].
;; 
;; 
;; Data Types
;; ==========
;; All data in TwoStep resides in either of two forms, either as
;; integers or, in a subordinated agency, as characters.
;; 
;; == INTEGERS COMPREHEND BYTES AND DIGITS ==
;; The paravaunt participant in the program's data representation
;; constitutes integer numbers of two specific subtypes: bytes and
;; single decimal digits.
;; 
;; == THE BYTE TYPE MODELS THE PROGRAM MEMORY ==
;; The program memory as a resident of the octet range [0, 255] assumes
;; the prepotent role, albeit restricted in the capacity of direct
;; manipulations.
;; 
;; == DECIMAL DIGITS PARTICIPATE AS INSTRUCTION ARGUMENTS ==
;; A more explicit warkloom for the programmatic communication, single
;; decimal digits, apprehending integers from the interval [0, 9],
;; contribute during the course of a program's execution, induced as
;; instruction arguments, to the actual effects.
;; 
;; == CHARACTERS COMMUNICATE DURING INPUT AND OUTPUT ==
;; A secondary element of currency in the language, ASCII characters
;; apply themselves as the means of input and output items.
;; 
;; The memory, as a byte value, translates during the print instruction
;; to the corresponding ASCII character.
;; 
;; Traveling in the athwart airt, when inciting a user input, the
;; character eventuating from its obtention is converted into the
;; respective ASCII character code and transmitted into the memory.
;; 
;; 
;; Instructions
;; ============
;; TwoStep's instruction set enumerates an exact dozen of members,
;; distributed along the bailiwicks of arithmetics, control flow, and
;; input/output communication.
;; 
;; Any instruction is delineated by a composition of two characters, the
;; first moeity of which designates the command type, while the twain's
;; second compartment specifies the sole argument.
;; 
;; Even in cases of instructions independent from their argument, that
;; is, such ignoring the affiliated datum, the operand must be present.
;; 
;; An argument may either comprehend a single decimal digit or, if not
;; deployed by the instruction, any other character.
;; 
;; == OVERVIEW ==
;; The twelve instructions shall now be enumerated in a cursory manner:
;; 
;;   ------------------------------------------------------------------
;;   Command   | Argument x    | Effect
;;   ----------+---------------+---------------------------------------
;;   +         | decimal digit | Adds the digit {x} to the memory.
;;   ..................................................................
;;   -         | decimal digit | Subtracts the digit {x} to the memory.
;;   ..................................................................
;;   *         | decimal digit | Multiplies the memory by the digit
;;             |               | {x}.
;;   ..................................................................
;;   /         | decimal digit | If the digit {x} does not equal zero
;;             |               | (0), divides the memory by {x} and
;;             |               | contingently rounds the quotient.
;;             |               | Otherwise, no modification applies.
;;   ..................................................................
;;   >         | decimal digit | If the digit {x} is less than the 
;;             |               | memory, executes the next statement;
;;             |               | otherwise skips the same.
;;   ..................................................................
;;   <         | decimal digit | If the digit {x} is greater than the 
;;             |               | memory, executes the next statement;
;;             |               | otherwise skips the same.
;;   ..................................................................
;;   )         | decimal digit | If the digit {x} is less than the 
;;             |               | memory, executes the previous
;;             |               | statement; otherwise skips the next
;;             |               | statement.
;;   ..................................................................
;;   (         | decimal digit | If the digit {x} is greater than the 
;;             |               | memory, executes the previous;
;;             |               | statement; otherwise skips the next
;;             |               | statement.
;;   ..................................................................
;;   [         | any character | Outputs the ASCII character associated
;;             |               | with the value stored in the memory to
;;             |               | the standard output.
;;             |               | The argument {x} is ignored.
;;   ..................................................................
;;   ]         | any character | Queries the user for an ASCII
;;             |               | character and stores its character
;;             |               | code in the memory.
;;             |               | The argument {x} is ignored.
;;   ..................................................................
;;   .         | any character | Terminates the program.
;;             |               | The argument {x} is ignored.
;;   ..................................................................
;;     (space) | any character | A no-operation with no effect.
;;             |               | The argument {x} is ignored.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Date: 2022-10-18
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/TwoStep"
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

(deftype command ()
  "The ``command'' type enumerates the recognized TwoStep instruction
   types."
  '(member
    :add
    :subtract
    :divide
    :multiply
    :execute-next-if-less
    :execute-next-if-greater
    :execute-previous-if-less
    :execute-previous-if-greater
    :output
    :input
    :stop
    :no-operation))

;;; -------------------------------------------------------

(deftype digit ()
  "The ``digit'' type defines a single-digit decimal integer in the
   range [0, 9]."
  '(integer 0 9))

;;; -------------------------------------------------------

(deftype operand ()
  "The ``operand'' type enumerates the types of instruction arguments
   that can be expected in a TwoStep program, exhausted already by a
   general character or a digit."
  '(or character digit))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type defines a TwoStep instruction in terms of
   a cons cell, the left compartment of which contains the instruction
   type as a ``command'', while the dextral moeity comprehends the
   argument as an ``operand''."
  '(cons command operand))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   covering, as a consectary, the integer range [0, 255], and being
   appropriate for the representation of a TwoStep program's memory."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction (command operand)
  "Creates and returns a new ``instruction'' composed of the COMMAND and
   the OPERAND."
  (declare (type command command))
  (declare (type operand operand))
  (the instruction (cons command operand)))

;;; -------------------------------------------------------

(defun instruction-type (instruction)
  "Returns the command categorizing the INSTRUCTION."
  (declare (type instruction instruction))
  (the command (car instruction)))

;;; -------------------------------------------------------

(defun instruction-operand (instruction)
  "Returns the operand partaking in the INSTRUCTION."
  (declare (type instruction instruction))
  (the operand (cdr instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for-character (token)
  "Returns the instruction type associated with the TOKEN, or signals an
   error upon the absence of a correspondence."
  (declare (type character token))
  (the command
    (case token
      (#\+       :add)
      (#\-       :subtract)
      (#\/       :divide)
      (#\*       :multiply)
      (#\>       :execute-next-if-less)
      (#\<       :execute-next-if-greater)
      (#\)       :execute-previous-if-less)
      (#\(       :execute-previous-if-greater)
      (#\[       :output)
      (#\]       :input)
      (#\.       :stop)
      (#\Space   :no-operation)
      (otherwise (error "Unrecognized command token: ~s." token)))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns the instructions from the piece of TwoStep CODE
   as a one-dimensional simple array."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of instruction) instructions))
    
    (when (plusp (length code))
      (let ((position 0)
            (token    (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) token))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next location in the
               CODE, if possible, updates the current TOKEN, and returns
               no value."
              (setf token
                (when (array-in-bounds-p code (1+ position))
                  (char code (incf position))))
              (values))
             
             (read-command ()
              "Starting at the current POSITION, reads and consumes a
               command character and returns the associated ``command''
               object."
              (the command
                (prog1
                  (get-command-for-character token)
                  (advance))))
             
             (read-digit ()
              "Starting at the current POSITION, reads, consumes, and
               returns a single decimal digit."
              (the integer
                (cond
                  ((null token)
                    (error "Expected a digit, but encountered EOF at ~
                            position ~d." position))
                  ((digit-char-p token)
                    (prog1
                      (digit-char-p token)
                      (advance)))
                  (T
                    (error "Expected a digit, but encounered ~s at ~
                            position ~d."
                      token position)))))
             
             (read-character ()
              "Starting at the current POSITION, reads, consumes, and
               returns a single character."
              (the character
                (or (prog1
                      token
                      (advance))
                    (error "Expected a character, but encountered EOF ~
                            at position ~d."
                      position))))
             
             (collect-instruction (new-instruction)
              "Inserts the NEW-INSTRUCTION at the front of the
               INSTRUCTIONS list and returns no value."
              (declare (type instruction new-instruction))
              (push new-instruction instructions)
              (values)))
          
          (loop while token do
            (case token
              ((NIL)
                (loop-finish))
              
              ((#\+ #\- #\/ #\* #\> #\< #\) #\()
                (collect-instruction
                  (make-instruction
                    (read-command)
                    (read-digit))))
              
              ((#\[ #\] #\. #\Space)
                (collect-instruction
                  (make-instruction
                    (read-command)
                    (read-character))))
              
              (otherwise
                (error "Invalid character ~s at position ~d."
                  token position)))))))
    
    (the (simple-array instruction (*))
      (coerce (nreverse instructions)
        '(simple-array instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          (vector instruction *)
    :documentation "The instructions to evaluate.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The index into the current INSTRUCTIONS element.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null instruction)
    :documentation "The instruction at the position IP into the
                    INSTRUCTIONS vector.")
   (memory
    :initarg       :memory
    :initform      0
    :type          octet
    :documentation "The program memory."))
  (:documentation
    "The ``Interpreter'' class furnishes a processing unit for the
     evaluation of a vector of TwoStep instructions."))

;;; -------------------------------------------------------

(defun interpreter-memory (interpreter)
  "Returns the value of INTERPRETER's internally managed memory."
  (declare (type Interpreter interpreter))
  (the octet (slot-value interpreter 'memory)))

;;; -------------------------------------------------------

(defun (setf interpreter-memory) (new-value interpreter)
  "Sets the value of the INTERPRETER's internally managed memory to the
   NEW-VALUE, contingently preceded by wrapping the content into the
   valid byte range [0, 255], and returns the adjusted new memory
   datum."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'memory)
        (mod new-value
          (if (minusp new-value)
            255
            256)))
  (the octet (slot-value interpreter 'memory)))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots ``instructions'', ``ip'',
   and ``current-instruction'' to symbol macros for general access, as
   well as defining an additional symbol macro ``memory'' which
   furnishes the same access to the INTERPRETER's memory, executes the
   BODY forms, and returns the last evaluated form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (with-slots (instructions ip current-instruction)
           ,evaluated-interpreter
         (declare (type (vector instruction *) instructions))
         (declare (type fixnum                 ip))
         (declare (type (or null instruction)  current-instruction))
         (declare (ignorable                   instructions))
         (declare (ignorable                   ip))
         (declare (ignorable                   current-instruction))
         (flet
             ((advance-ip ()
               "Moves the instruction pointer IP to the next position in
                the INSTRUCTIONS vector, if possible, updates the
                CURRENT-INSTRUCTION, and returns no value."
               (setf current-instruction
                 (when (array-in-bounds-p instructions (1+ ip))
                   (aref instructions (incf ip))))
               (values))
              
              (recede-ip ()
               "Moves the instruction pointer IP to the previous
                position in the INSTRUCTIONS vector, if possible,
                updates the CURRENT-INSTRUCTION in the case of the new
                location's validity, and returns no value.
                ---
                Please note that, as opposed to the forward proceeding
                analogue ``advance-ip'', this function does not
                decrement the instruction pointer and update the
                CURRENT-INSTRUCTION if already located at the first
                position."
               (when (array-in-bounds-p instructions (1- ip))
                 (setf current-instruction
                   (aref instructions (decf ip))))
               (values)))
           
           (symbol-macrolet
               ((memory
                 (interpreter-memory ,evaluated-interpreter)))
             
             ,@body))))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' which processes the
   INSTRUCTIONS."
  (declare (type (vector instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter command instruction)
  (:documentation
    "Applies the INTERPRETER to the processing of the INSTRUCTION, upon
     whose COMMAND type the function dispatches, and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor (command &body body)
  "Defines an implementation of the ``process-instruction'' generic
   function, dispatching on the COMMAND and evaluating the BODY forms,
   wrapped inside of a ``with-interpreter'' macro invocation, finally
   returning no value."
  `(defmethod process-instruction ((interpreter Interpreter)
                                   (command     (eql ,command))
                                   instruction)
     (declare (type Interpreter interpreter))
     (declare (type command     command))
     (declare (type instruction instruction))
     (declare (ignore           command))
     (declare (ignorable        instruction))
     (with-interpreter (interpreter)
       ,@body
     (values))))

;;; -------------------------------------------------------

(define-instruction-processor :add
  (incf memory (instruction-operand instruction))
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :subtract
  (decf memory (instruction-operand instruction))
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :multiply
  (setf memory (* memory (instruction-operand instruction)))
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :divide
  (unless (zerop (instruction-operand instruction))
    (setf memory (round memory (instruction-operand instruction))))
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :execute-next-if-less
  (cond
    ((< (instruction-operand instruction) memory)
      (advance-ip))
    (T
      (advance-ip)
      (advance-ip))))

;;; -------------------------------------------------------

(define-instruction-processor :execute-next-if-greater
  (cond
    ((> (instruction-operand instruction) memory)
      (advance-ip))
    (T
      (advance-ip)
      (advance-ip))))

;;; -------------------------------------------------------

(define-instruction-processor :execute-previous-if-less
  (cond
    ((<= ip 0)
      (advance-ip))
    ((< (instruction-operand instruction) memory)
      (recede-ip))
    (T
      (advance-ip)
      (advance-ip))))

;;; -------------------------------------------------------

(define-instruction-processor :execute-previous-if-greater
  (cond
    ((<= ip 0)
      (advance-ip))
    ((> (instruction-operand instruction) memory)
      (recede-ip))
    (T
      (advance-ip)
      (advance-ip))))

;;; -------------------------------------------------------

(define-instruction-processor :output
  (write-char (code-char memory))
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :input
  (format T "~&Please input an ASCII character: ")
  (setf memory (char-code (read-char)))
  (clear-input)
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :no-operation
  (advance-ip))

;;; -------------------------------------------------------

(define-instruction-processor :stop
  (setf ip                  (length instructions))
  (setf current-instruction NIL))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Processes the INTERPRETER's instructions and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (loop while current-instruction do
      (process-instruction interpreter
        (instruction-type current-instruction) current-instruction)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-TwoStep (code)
  "Interprets the piece of TwoStep CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-TwoStep
  " H e l l o ,   W o r l d !  *0+6*2*6[ *0+5*5*4+1[ +7[ [ +3[ *0+5*9-1[ *0+8*4[ *0+9*9+6[ *0+5*5*4+9+2[ +3[ -6[ -8[ *0+8*4+1[ . ")

;;; -------------------------------------------------------

;; Print the letter "x" in an infinite loop.
(interpret-TwoStep "*0+6*4*5[ )0. ")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-TwoStep "] [ . ")
