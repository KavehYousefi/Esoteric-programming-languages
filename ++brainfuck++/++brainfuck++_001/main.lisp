;; Date: 2021-12-15
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/%2B%2Bbrainfuck%2B%2B"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each such consisting of a key of the KEY-TYPE and a value
   of the VALUE-TYPE, both of which default to the comprehensive ``T''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-++brainfuck++ (code)
  "Interprets the piece of ++brainfuck++ CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (memory    (make-hash-table :test #'eql))
          (pointer   0)
          (temporary 0))
      (declare (type fixnum                          position))
      (declare (type (or null character)             character))
      (declare (type (hash-table-of integer integer) memory))
      (declare (type integer                         pointer))
      (declare (type integer                         temporary))
      
      (setf *random-state* (make-random-state T))
      
      (labels
          ((advance ()
            "Moves the POSITION to the next character, if possible,
             updates the current CHARACTER, and returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION to the previous character, if possible,
             updates the current CHARACTER, and returns no value."
            (setf character
              (when (plusp position)
                (char code (decf position))))
            (values)))
        
        (loop do
          (case character
            ;; End of program.
            ((NIL)
              (loop-finish))
            
            (#\>
              (incf pointer)
              (advance))
            
            (#\<
              (decf pointer)
              (advance))
            
            (#\+
              (incf (gethash pointer memory 0))
              (advance))
            
            (#\-
              (decf (gethash pointer memory 0))
              (advance))
            
            (#\.
              (write-char (code-char (gethash pointer memory 0)))
              (advance))
            
            (#\,
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (gethash pointer memory) (char-code input)))
              (advance))
            
            ;; Skip past "}" if current cell equals zero.
            (#\{
              (cond
                ((zerop (gethash pointer memory 0))
                  (advance)
                  (loop with level of-type fixnum = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated '{'."))
                      (#\{
                        (incf level)
                        (advance))
                      (#\}
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
            
            ;; Jump back past matching "{" if current cell does not
            ;; equal zero.
            (#\}
              (cond
                ((not (zerop (gethash pointer memory 0)))
                  (recede)
                  (loop with level of-type fixnum = 0 do
                    (case character
                      ((NIL)
                        (error "Unmatched '}'."))
                      (#\}
                        (incf level)
                        (recede))
                      (#\{
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            ;; Copy the current cell value into the temporary variable.
            (#\c
              (setf temporary (gethash pointer memory 0))
              (advance))
            
            ;; Copy the temporary variable value into the current cell.
            (#\p
              (setf (gethash pointer memory) temporary)
              (advance))
            
            ;; Reset the current cell to zero (0).
            (#\o
              (setf (gethash pointer memory) 0)
              (advance))
            
            ;; Terminate the program.
            (#\e
              (loop-finish))
            
            ;; Print the current cell value as a number.
            (#\n
              (format T "~d" (gethash pointer memory 0))
              (advance))
            
            ;; Set the current cell value to a random integer in the
            ;; range [1, 1000].
            (#\r
              (setf (gethash pointer memory) (1+ (random 1000)))
              (advance))
            
            ;; Skip comment characters.
            (otherwise
              (advance)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrates the employment of the temporary variable and the
;; printing using both the character and number output.
(interpret-++brainfuck++ "++c>pn>p.")

;;; -------------------------------------------------------

;; Prints a character code followed by its associated character.
(interpret-++brainfuck++ "rcn>p-.")

;;; -------------------------------------------------------

;; Prints the numbers, prefixed by a character, in a loop.
(interpret-++brainfuck++ "rc+>p++>p+++{<.n}")

;;; -------------------------------------------------------

;; Print "Hello World!".
(interpret-++brainfuck++ "++++++++{>++++{>++>+++>+++>+<<<<-}>+>+>->>+{<}<-}>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-++brainfuck++ ",.{,.}")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Memory structure:
;;   memory[0] --- user input
;;   memory[1] --- deduction cell, gradually increases memory[1] until
;;             --- it reaches the numeric value of the ASCII digit
;;             --- which the user has supplied.
(interpret-++brainfuck++ ",>++++++++++++++++++++++++++++++++++++++++++++++++{<->-}<{n}n")
