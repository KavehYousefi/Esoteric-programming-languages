;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Whopper", invented by the Esolang user "_hyperdawg", and
;; based upon the manipulation of an integer stack by concise commands
;; with the purpose of applying arithmetic and printing operations.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-10-01
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Whopper"
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
  "The ``command'' type enumerates the recognized instruction types."
  '(member
    :push-number
    :print-number
    :print-character
    :input
    :halt
    :add
    :subtract
    :multiply
    :divide
    :modulo
    :print-newline))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type defines an instruction, composed of a
   command and an optional argument, in the form of a cons, the
   sinistral moeity of which contains the command type, while the
   dextral might be ``NIL'' or an integer."
  '(or (cons command null)
       (cons command integer)))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines an association list which maps
   command name strings to the respective ``command'' objects."
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
                  (typep element '(cons string command)))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction-set ()
  "The ``instruction-set'' type defines a list of zero or more
   instructions."
  '(list-of instruction))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based stack of zero or more integer
   numbers."
  '(list-of integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table    +IDENTIFIERS+))

(declaim (type (or null string)    *line*))
(declaim (type fixnum              *position*))
(declaim (type (or null character) *token*))
(declaim (type instruction-set     *instructions*))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '(("PSH" . :push-number)
    ("POP" . :print-number)
    ("PPC" . :print-character)
    ("INP" . :input)
    ("HLT" . :halt)
    ("ADD" . :add)
    ("SUB" . :subtract)
    ("MUL" . :multiply)
    ("DIV" . :divide)
    ("MOD" . :modulo)
    ("NLN" . :print-newline))
  "An association list which maps the recognized command names to the
   representative ``command'' objects.")

;;; -------------------------------------------------------

(defparameter *line* NIL
  "The currently processed line, or ``NIL'' if the lexer is not
   initialized yet.")

(defparameter *position* 0
  "The current index into the *LINE*.")

(defparameter *token* NIL
  "The character at the current *POSITION* into the *LINE*, or ``NIL''
   if either the lexer is not initialized yet, or the line has been
   processed completely.")

(defparameter *instructions* NIL
  "A list of the instructions extracted from the most recently processed
   piece of Whopper code.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-token-p ()
  "Checks whether the current *TOKEN* represents a space character, on
   confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (not (null (char= *token* #\Space)))))

;;; -------------------------------------------------------

(defun identifier-token-p ()
  "Checks whether the *TOKEN* represents a valid constituent for an
   identifier name, that is, a letter, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (alpha-char-p *token*)))))

;;; -------------------------------------------------------

(defun token-equals-p (expected-character)
  "Checks whether the current *TOKEN* represents the EXPECTED-CHARACTER,
   upon confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''.
   ---
   In the case of the *TOKEN* resolving to ``NIL'', that is, the lexer
   operating on an exhausted *LINE*, this function responds exactly as
   if the matching has been negative, by responding with ``NIL''."
  (declare (type character expected-character))
  (the boolean
    (not (null
      (and *token*
           (char= *token* expected-character))))))

;;; -------------------------------------------------------

(defun end-of-line-p ()
  "Checks whether the *POSITION* cursor has reached the end of the
   current line, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (null *token*)))

;;; -------------------------------------------------------

(defun move-to-start-of-line ()
  "Relocates the *POSITION* cursor to the start of the current *LINE*,
   updates the *TOKEN*, and returns no value."
  (setf *position* 0)
  (setf *token*
    (when (array-in-bounds-p *line* *position*)
      (char *line* *position*)))
  (values))

;;; -------------------------------------------------------

(defun advance ()
  "Moves the *POSITION* cursor to the next location in the *LINE*, if
   possible, updates the *TOKEN*, and returns no value."
  (setf *token*
    (when (array-in-bounds-p *line* (1+ *position*))
      (char *line* (incf *position*))))
  (values))

;;; -------------------------------------------------------

(defun move-to-end-of-line ()
  "Relocates the *POSITION* cursor to the end of the current *LINE*,
   thus effectively terminating its processing, and returns no value."
  (setf *position* (1- (length *line*)))
  (setf *token*    NIL)
  (values))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Starting at the current *POSITION* into the *LINE*, skips a sequence
   of zero or more abutting space characters and relocates the cursor at
   the first non-space position, returning no value."
  (loop while (and *token* (space-token-p)) do
    (advance))
  (values))

;;; -------------------------------------------------------

(defun eat-space ()
  "Expects the *TOKEN* to represent a space, on confirmation moving the
   *POSITION* past it and returning no value, otherwise signaling an
   error of an unspecified type."
  (cond
    ((end-of-line-p)
      (error "Expected a space at position ~d, but encountered the ~
              end of the line."
        *position*))
    ((space-token-p)
      (advance))
    (T
      (error "Expected a space at position ~d, but encountered ~c."
        *position* *token*)))
  (values))

;;; -------------------------------------------------------

(defun skip-comment ()
  "Expecting the *POSITION* cursor to be located at the colon (\":\")
   starting a comment, skips the comment *LINE*, moves the cursor to its
   end, and returns no value."
  (advance)
  (eat-space)
  (move-to-end-of-line)
  (values))

;;; -------------------------------------------------------

(defun get-command-for-identifier (identifier)
  "Returns the ``command'' associated with the IDENTIFIER, or signals an
   error of an unspecified type upon the absence of a correspondence."
  (declare (type string identifier))
  (let ((identifier (assoc identifier +IDENTIFIERS+ :test #'string=)))
    (declare (type (or null (cons string command)) identifier))
    (the command
      (if identifier
        (cdr identifier)
        (error "Unrecognized command identifier: ~s." identifier)))))

;;; -------------------------------------------------------

(defun read-command ()
  "Starting at the current *POSITION* into the *LINE*, reads an
   identifier string and returns the associated ``command''.
   ---
   An error of an unspecified type is signaled if the consumed detected
   identifier cannot be matched against a recognized command name."
  (the command
    (get-command-for-identifier
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (loop while (and *token* (alpha-char-p *token*)) do
          (write-char *token* identifier)
          (advance))))))

;;; -------------------------------------------------------

(defun read-number ()
  "Starting at the current *POSITION*, reads a signed integer number and
   returns its evaluated value."
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        
        ;; Read optional sign.
        (when (or (token-equals-p #\+) (token-equals-p #\-))
          (write-char *token* digits)
          (advance))
        
        ;; Read adjacent decimal digits.
        (loop while (and *token* (digit-char-p *token*)) do
          (write-char *token* digits)
          (advance))))))

;;; -------------------------------------------------------

(defun expect-end-of-line ()
  "Checks whether the portion of the *LINE*, starting at the current
   *POSITION* is vacant, or composed of spaces only, on confirmation
   returning no value, otherwise signaling an error of an unspecified
   type."
  (skip-spaces)
  (unless (end-of-line-p)
    (error "Expected the end of the line, ~
            but encountered ~c at position ~d."
      *token* *position*))
  (values))

;;; -------------------------------------------------------

(defun make-instruction (command &optional (argument NIL))
  "Creates and returns a new ``instruction'' compact of the COMMAND and
   an optional numeric ARGUMENT."
  (declare (type command           command))
  (declare (type (or null integer) argument))
  (the instruction (cons command argument)))

;;; -------------------------------------------------------

(defun clear-instructions ()
  "Removes all items from the *INSTRUCTIONS* list and returns no value."
  (setf *instructions* NIL)
  (values))

;;; -------------------------------------------------------

(defun set-line (new-line)
  "Sets the *LINE* to the NEW-LINE, updates both the *POSITION* cursor
   and the *TOKEN*, the content of the line, potentially inserting a new
   instruction at the front of the *INSTRUCTIONS* list, and returns no
   value."
  (declare (type string new-line))
  
  (setf *line* new-line)
  (move-to-start-of-line)
  (skip-spaces)
  
  (cond
    ;; Empty line?
    ;; => Terminate.
    ((end-of-line-p)
      NIL)
    
    ;; Comment line?
    ;; => Check comment validity, then terminate.
    ((token-equals-p #\:)
      (skip-comment))
    
    ;; Instruction line?
    ((identifier-token-p)
      (let ((command (read-command)))
        (declare (type command command))
        (cond
          ;; "PSH <number>" command found?
          ;; => Read numeric argument, then prepend new instruction.
          ((eq command :push-number)
            (eat-space)
            (let ((number (read-number)))
              (declare (type integer number))
              (expect-end-of-line)
              (push (make-instruction command number) *instructions*)))
          ;; Any other command found?
          ;; => Prepend as new instruction.
          (T
            (expect-end-of-line)
            (push (make-instruction command) *instructions*)))))
    
    ;; Invalid line?
    ;; => Signal an error.
    (T
      (error "Unexpected character ~c at position ~d."
        *token* *position*)))
  
  (values))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts from the piece of Whopper CODE a list of instructions and
   stores these into the *INSTRUCTIONS* sequence, purging any contingent
   content prior to this, and returns no value."
  (declare (type string code))
  (clear-instructions)
  (with-input-from-string (code-stream code)
    (declare (type string-stream code-stream))
    (loop
      for   line of-type (or null string) = (read-line code-stream NIL)
      while line
      do    (set-line line)))
  (setf *instructions* (nreverse *instructions*))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions ()
  "Processes the currently set Whopper *INSTRUCTIONS* and returns no
   value."
  (when *instructions*
    (let ((stack NIL))
      (declare (type stack stack))
      (labels
          ((pop-number ()
            "Pops the top integer element from the STACK and returns it.
             ---
             An error of an unspecified type is signaled if the stack is
             empty."
            (the integer
              (or (pop stack)
                  (error "Stack is empty."))))
           
           (pop-number-pair ()
            "Pops the two top integer elements from the STACK and
             returns these as multiple values, compact of
               (1) the erstwhile top element
               (2) the erstwhile second top element
             ---
             An error of an unspecified type is signaled if the stack
             contains less than two elements."
            (the (values integer integer)
              (if (>= (length stack) 2)
                (values (pop stack) (pop stack))
                (error "Too few elements on the stack: ~d."
                  (length stack)))))
           
           (apply-binary-operation (operation)
            "Pops the two top integer elements from the STACK, applies
             the binary OPERATION on the erstwhile top as the left
             operand and the erstwhile second top as the right operand,
             pushes the result unto the stack, and returns no value.
             ---
             An error of an unspecified type is signaled if the stack
             contains less than two elements."
            (declare (type (function (integer integer) integer)
                           operation))
            (multiple-value-bind (left right)
                (pop-number-pair)
              (declare (type integer left))
              (declare (type integer right))
              (push (funcall operation left right) stack))
            (values)))
        
        (loop
          for instruction of-type instruction   in   *instructions*
          and ip          of-type (integer 0 *) from 0
          do
            (destructuring-bind (command . argument) instruction
              (declare (type command           command))
              (declare (type (or null integer) argument))
              (declare (ignorable              argument))
              
              (case command
                (:push-number
                  (push argument stack))
                
                (:print-number
                  (format T "~d" (pop-number)))
                
                (:print-character
                  (format T "~c" (code-char (pop-number))))
                
                (:input
                  (format T "~&Please input an integer: ")
                  (let ((input (parse-integer (read-line))))
                    (declare (type integer input))
                    (clear-input)
                    (push input stack)))
                
                (:halt
                  (loop-finish))
                
                (:add
                  (apply-binary-operation #'+))
                
                (:subtract
                  (apply-binary-operation #'-))
                
                (:multiply
                  (apply-binary-operation #'*))
                
                (:divide
                  (apply-binary-operation #'round))
                
                (:modulo
                  (apply-binary-operation #'mod))
                
                (:print-newline
                  (format T "~%"))
                
                (otherwise
                  (error "Unrecognized instruction ~s at position ~d."
                    instruction ip))))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Whopper (code)
  "Interprets the piece of Whopper CODE and returns no value."
  (declare (type string code))
  (extract-instructions code)
  (process-instructions)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the text "hi".
(interpret-Whopper
  ": hi
   
   : Print the letter 'h'.
   PSH 10
   PSH 10
   MUL
   PSH 4
   ADD
   PPC
   
   : Print the letter 'i'.
   PSH 10
   PSH 10
   MUL
   PSH 5
   ADD
   PPC
   NLN")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-Whopper
  ": cat
   
   INP
   POP")
