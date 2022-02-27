;; Author: Kaveh Yousefi
;; Date:   2022-02-27
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Pops_Are_Overrated"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based stack of zero or more
   elements, each of which conforms to the ELEMENT-TYPE."
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

(deftype memory ()
  "The ``memory'' type defines the Pops Are Overrated program memory as
   a hash table of integer indices mapped to stacks maintaining
   integers themselves."
  '(hash-table-of integer (stack-of integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Pops-Are-Overrated (code)
  "Interprets the Pops Are Overrated CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (stacks    (make-hash-table :test #'eql))
          (pointer   0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type memory              stacks))
      (declare (type integer             pointer))
      
      (labels
          (
           ;; == LEXING OPERATIONS ================================== ;;
           
           (advance ()
            "Moves the POSITION cursor to the next character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION cursor to the previous character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (plusp position)
                (char code (decf position))))
            (values))
           
           (whitespace-character-p (candidate)
            "Checks whether the CANDIDATE represents a whitespace,
             returning on confirmation a ``boolean'' value of ``T'',
             otherwise ``NIL''."
            (declare (type character candidate))
            (the boolean
              (not (null
                (member candidate '(#\Space #\Tab #\Newline)
                  :test #'char=)))))
           
           (skip-whitespaces ()
            "Starting at the current POSITION, skips zero or more
             whitespaces, relocates the POSITION cursor to the first
             non-whitespace character, and returns no value."
            (loop
              while (and character (whitespace-character-p character))
              do    (advance))
            (values))
           
           
           ;; == MEMORY ACCESS OPERATIONS =========================== ;;
           
           (stack-at (index)
            "Returns the stack at the INDEX."
            (declare (type integer index))
            (the list (gethash index stacks)))
           
           ((setf stack-at) (new-stack index)
            "Sets the stack at the INDEX to the NEW-STACK and returns no
             value."
            (declare (type list    new-stack))
            (declare (type integer index))
            (setf (gethash index stacks NIL) new-stack)
            (values))
           
           (current-stack ()
            "Returns the currently selected stack."
            (the list (stack-at pointer)))
           
           ((setf current-stack) (new-stack)
            "Sets the currently selected stack to the NEW-STACK and
             returns no value."
            (declare (type list new-stack))
            (setf (stack-at pointer) new-stack)
            (values))
           
           
           ;; == STACK OPERATIONS =================================== ;;
           
           (pop-stack (stack &optional (default 0))
            "Removes and returns the top STACK element, or returns the
             DEFAULT value if the stack is empty."
            (declare (type (stack-of integer) stack))
            (declare (type integer            default))
            (the integer (or (pop stack) default)))
           
           (stack-first (stack &optional (default 0))
            "Returns without removing the first STACK element, or the
             DEFAULT value if the stack is empty."
            (declare (type (stack-of integer) stack))
            (declare (type integer            default))
            (the integer (or (first stack) default)))
           
           ((setf stack-first) (new-element stack)
            "Replaces the top STACK element with the NEW-ELEMENT, or
             pushes the NEW-ELEMENT if the STACK is empty, returning in
             any case no value."
            (declare (type integer            new-element))
            (declare (type (stack-of integer) stack))
            (setf (first stack) new-element)
            (values))
           
           (stack-second (stack &optional (default 0))
            "Returns without removing the second STACK element, or the
             DEFAULT value if the stack contains less than two elements."
            (declare (type (stack-of integer) stack))
            (declare (type integer            default))
            (the integer (or (second stack) default)))
           
           ((setf stack-second) (new-element stack)
            "Replaces the second STACK element with the NEW-ELEMENT, or
             inserts the NEW-ELEMENT at that position if the STACK
             contains less than two elements, returning in any case no
             value."
            (declare (type integer           new-element))
            (declare (type (stack-of integer) stack))
            (setf (second stack) new-element)
            (values))
           
           (stack-bottom (stack &optional (default 0))
            "Returns without removing the lowest STACK element, or the
             DEFAULT value if the stack is empty."
            (declare (type (stack-of integer) stack))
            (declare (type integer            default))
            (the integer (or (car (last stack)) default))))
        
        (loop do
          (cond
            ;; Terminate on encountering an end of file (EOF).
            ((null character)
              (loop-finish))
            
            ;; Skip spaces and newlines.
            ((whitespace-character-p character)
              (skip-whitespaces))
            
            ;; Push 0.
            ((char= character #\€)
              (push 0 (current-stack))
              (advance))
            
            ;; Multiply top of stack, then add digit.
            ((digit-char-p character)
              (setf (stack-first (current-stack))
                    (* (stack-first (current-stack)) 10))
              (incf (stack-first (current-stack))
                    (digit-char-p character))
              (advance))
            
            ;; Subtract.
            ((char= character #\+)
              (push
                (- (pop-stack (current-stack) 0)
                   (pop-stack (current-stack) 0))
                (current-stack))
              (advance))
            
            ;; Add.
            ((char= character #\-)
              (push
                (+ (pop-stack (current-stack) 0)
                   (pop-stack (current-stack) 0))
                (current-stack))
              (advance))
            
            ;; Multiply.
            ((char= character #\/)
              (push
                (* (pop-stack (current-stack) 1)
                   (pop-stack (current-stack) 1))
                (current-stack))
              (advance))
            
            ;; Divide.
            ((char= character #\*)
              (push
                (round (pop-stack (current-stack) 1)
                       (pop-stack (current-stack) 1))
                (current-stack))
              (advance))
            
            ;; Move top value to the next stack.
            ((char= character #\>)
              (let ((top-value (pop-stack (current-stack) 0)))
                (declare (type integer top-value))
                (push top-value (stack-at (1+ pointer))))
              (advance))
            
            ;; Move top value to the previous stack.
            ((char= character #\<)
              (let ((top-value (pop-stack (current-stack) 0)))
                (declare (type integer top-value))
                (push top-value (stack-at (1- pointer))))
              (advance))
            
            ;; Move the stack pointer one stack to the left.
            ((char= character #\£)
              (decf pointer)
              (advance))
            
            ;; Move the stack pointer one stack to the right.
            ((char= character #\¥)
              (incf pointer)
              (advance))
            
            ;; Duplicate the top stack element.
            ((char= character #\%)
              (push
                (stack-first (current-stack) 0)
                (current-stack))
              (advance))
            
            ;; Swap.
            ((char= character #\$)
              (rotatef
                (stack-first  (current-stack))
                (stack-second (current-stack)))
              (advance))
            
            ;; Move bottom most value up.
            ((char= character #\^)
              (let ((last-element (stack-bottom (current-stack) 0)))
                (declare (type integer last-element))
                (setf (current-stack)
                      (nbutlast (current-stack)))
                (push
                  last-element
                  (current-stack)))
              (advance))
            
            ;; Move top most value down.
            ((char= character #\v)
              (setf (current-stack)
                    (append (current-stack)
                            (list (pop-stack (current-stack) 0))))
              (advance))
            
            ;; Start loop.
            ((char= character #\()
              (cond
                ((zerop (stack-first (current-stack) 0))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case character
                      (#\(
                        (incf level)
                        (advance))
                      (#\)
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; End loop.
            ((char= character #\))
              (cond
                ((not (zerop (stack-first (current-stack) 0)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unmatched ')'."))
                      (#\(
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#\)
                        (incf level)
                        (recede))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            ;; Output as a number.
            ((char= character #\o)
              (write (stack-first (current-stack) 0))
              (advance))
            
            ;; Output as an ASCII character.
            ((char= character #\O)
              (write-char (code-char (stack-first (current-stack) 0)))
              (advance))
            
            ;; Input as a number.
            ((char= character #\i)
              (format T "~&Please input an integer number: ")
              (let ((input (read)))
                (declare (type integer input))
                (clear-input)
                (push input (current-stack)))
              (advance))
            
            ;; Input as an ASCII character and push its value.
            ((char= character #\I)
              (format T "~&Please input an ASCII character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (push (char-code input) (current-stack)))
              (advance))
            
            ;; Any other characters are prohibited.
            (T
              (error "Invalid character ~s at position ~d."
                character position)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-Pops-Are-Overrated "i(o)o")

;;; -------------------------------------------------------

;; Print "Hello world!".
(interpret-Pops-Are-Overrated "€72O€101O€108%OO€111O€32O€119O€111O€114O€108O€100O€33O")

;;; -------------------------------------------------------

;; Stack pop cheat.
(interpret-Pops-Are-Overrated "%+-")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Pops-Are-Overrated "IO(IO)")
