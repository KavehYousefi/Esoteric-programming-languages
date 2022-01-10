;; Date: 2022-01-09
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/SimpleScript"



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-SimpleScript (code)
  "Interprets the piece of SimpleScript CODE and returns no value.
   ---
   Please reck how, in the face of the CODE's ending march capacitated
   to act as an implicit loop closing bracket ']', as well as with
   respect to performance considerations, the activated loops experience
   their maintenance in a stack, each element of which represents the
   start index of the iteration's body, that is, the position
   immediately following the opening bracket '['. The recency of an
   iteration's encounter reverberates in its rank inside of this stack:
   The most recent loop, that is, the currently operating one, occupies
   the top, with a descent in location designating an accrue in the
   temporality of the encounter; reasonably, at the bottom of the stack
   wones the loop encountered first and yet remaining active. When a
   loop terminates, either by adminicle of an explicit closure (']') or
   the behest of the CODE's desinent boundary, the currently topmost
   loop is removed ('popped'), and the new stack head, if any extant,
   supersedes its role as active participant."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (stack     NIL)
          (register  0)
          (loops     NIL))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type (stack-of integer)  stack))
      (declare (type integer             register))
      (declare (type (stack-of fixnum)   loops))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character in the
             CODE, if possible, and updates the current CHARACTER,
             returning no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
            
            (move-to (new-position)
              "Moves the POSITION cursor to the NEW-POSITION, updates
               the current CHARACTER, and returns no value."
              (declare (type fixnum new-position))
              (setf position new-position)
              (setf character
                (when (< position (length code))
                  (char code position)))
              (values))
            
            (find-closing-bracket ()
              "Starting at the current POSITION, searches for a matching
               closing bracket ']' or the end of the CODE, returning
               either the position of the former or, if absent, the
               index designating the latter."
              (let ((return-position  position)
                    (bracket-position position))
                (declare (type fixnum return-position))
                (declare (type fixnum position))
                (loop with level of-type fixnum = 0 do
                  (case character
                    ((NIL)
                      (setf bracket-position position)
                      (loop-finish))
                    (#\[
                      (incf level)
                      (advance))
                    (#\]
                      (cond
                        ((zerop level)
                          (setf bracket-position position)
                          (loop-finish))
                        (T
                          (decf level)
                          (advance))))
                    (otherwise
                      (advance))))
                (setf position return-position)
                (the fixnum bracket-position))))
        
        (loop do
          (case character
            ;; End of CODE?
            ;; => Check whether pending loops remain.
            ((NIL)
              (cond
                ;; In loop and shall repeat?
                ((and loops (not (zerop register)))
                  (move-to (first loops)))
                
                ;; In loop but shall not repeat?
                ((and loops (zerop register))
                  (pop loops)
                  (loop-finish))
                
                ;; Not in loop?
                (T
                  (loop-finish))))
            
            ;; Push string's ASCII characters unto the stack.
            ((char= character #\")
              (let ((text ""))
                (declare (type string text))
                (setf text
                  (with-output-to-string (output)
                    (declare (type string-stream output))
                    (let ((start-position position))
                      (declare (type fixnum start-position))
                      (advance)
                      (loop do
                        (case character
                          ((NIL)
                            (error "Unterminated string at position ~d."
                              start-position))
                          (#\"
                            (advance)
                            (loop-finish))
                          (#\\
                            (advance)
                            (write-char character output)
                            (advance))
                          (otherwise
                            (write-char character output)
                            (advance)))))))
                (loop
                  for text-character of-type character across text
                  do  (push (char-code text-character) stack))))
            
            ;; Print stack and clear it.
            (#\a
              (loop while stack do
                (write-char (code-char (pop stack))))
              (advance))
            
            ;; Prompt character from user and write to the register.
            (#\b
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf register (char-code input)))
              (advance))
            
            ;; Prompt string from user and write to the stack.
            (#\c
              (format T "~&Please input a string: ")
              (let ((input (read-line)))
                (declare (type string input))
                (clear-input)
                (loop
                  for input-character of-type character across input
                  do  (push input-character stack)))
              (advance))
            
            ;; Pop from stack and write to register.
            (#\d
              (setf register (pop stack))
              (advance))
            
            ;; Print register as a number.
            (#\e
              (write register)
              (advance))
            
            ;; Print register as an ASCII character.
            (#\f
              (write-char (code-char register))
              (advance))
            
            ;; Push register to stack.
            (#\g
              (push register stack)
              (advance))
            
            ;; Pop stack value and add to register.
            (#\h
              (incf register (pop stack))
              (advance))
            
            ;; Pop stack value and add to register.
            (#\i
              (decf register (pop stack))
              (advance))
            
            ;; Repeat code betwixt '[' and ']' (or end of file), until
            ;; register equals zero.
            (#\[
              (cond
                ;; Register is zero?
                ;; => Skip region betwixt '[' and matching ']', or end
                ;;    of the CODE.
                ((zerop register)
                  (advance)
                  (move-to (find-closing-bracket))
                  (advance))
                
                ;; Register is nonzero?
                ;; => Store position following '[' as loop body start.
                (T
                  (advance)
                  (push position loops))))
            
            (#\]
              (cond
                ;; In loop and shall repeat?
                ((and loops (not (zerop register)))
                  (move-to (first loops)))
                
                ;; In loop but shall not repeat?
                ((and loops (zerop register))
                  (pop loops)
                  (advance))
                
                ;; Not in loop?
                (T
                  (advance))))
            
            
            ((#\Space #\Tab #\Newline)
              (advance))
            
            (otherwise
              (error "Invalid character ~s at position ~d."
                character position)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-SimpleScript "\"!dlroW ,olleH\"a")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-SimpleScript "bf")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-SimpleScript "bf[bf]")

;;; -------------------------------------------------------

;; Infinitely repeating cat program, lacking the closing loop bracket.
(interpret-SimpleScript "bf[bf")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Pseudocode:
;;   { Is used later on to transform digit character into integer. }
;;   Push value 48 to stack
;;   Store user input character in register
;;   { Transform input character into digit, e.g. "0" => 0, "1" => 1. }
;;   Reduce register by popped stack value
;;   While register != 0
;;     Print register as number
;;   End while
;;   Print register as number
;; 
(interpret-SimpleScript "\"0\" b i [e] e")
