;; Date: 2021-12-16
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/King_Ethan"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, the keys of which are of the KEY-TYPE and the values of the
   VALUE-TYPE, both of which default to the comprehensive ``T''."
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

(defun interpret-King-Ethan (code)
  "Interprets the piece of King Ethan CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          
          (memory    (make-hash-table :test #'eql))
          (pointer   0))
      (declare (type fixnum                          position))
      (declare (type (or null character)             character))
      (declare (type (hash-table-of integer integer) memory))
      (declare (type integer                         pointer))
      
      (labels
          ((advance ()
            "Moves the POSITION to the next character, if possible,
             updates the CHARACTER, and returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION to the previous character, if possible,
             updates the CHARACTER, and returns no value."
            (setf character
              (when (plusp position)
                (char code (decf position))))
            (values))
           
           (string-follows (expected-string)
            "Checks whether, starting at the current POSITION, the
             characters constituting the EXPECTED-STRING follow, on
             confirmation relocating the POSITION to the character
             immediately following the match and returning a ``boolean''
             value of ``T'', otherwise returning the POSITION to its
             state before this operation call and returning ``NIL''."
            (declare (type string expected-string))
            (let ((return-position position))
              (declare (type fixnum return-position))
              (the boolean
                (loop
                  for expected-character
                    of-type character
                    across  expected-string
                  do
                    (cond
                      ((or (null character)
                           (char/= character expected-character))
                        (setf position  return-position)
                        (setf character (char code position))
                        (return NIL))
                      (T
                        (advance)))
                  finally
                    (return T))))))
        
        (loop do
          (cond
            ;; End of program.
            ((null character)
              (loop-finish))
            
            ;; Move pointer left.
            ((char= character #\[)
              (decf pointer)
              (advance))
            
            ;; Move pointer right.
            ((char= character #\])
              (incf pointer)
              (advance))
            
            ;; Increment current cell.
            ((char= character #\+)
              (incf (gethash pointer memory 0))
              (advance))
            
            ;; Decrement current cell.
            ((char= character #\-)
              (decf (gethash pointer memory 0))
              (advance))
            
            ;; Input.
            ((char= character #\,)
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (declare (ignorable      input))
                (clear-input)
                (cond
                  ;; Command is ",>&"?
                  ;; => Store input in current cell.
                  ((string-follows ",>&")
                    (setf (gethash pointer memory) (char-code input)))
                  
                  ;; Command is ",>."?
                  ;; => Write input directly to output.
                  ((string-follows ",>.")
                    (write-char input))
                  
                  ;; Any other character follows?
                  ;; => Input is not employed.
                  (T
                    (advance)))))
            
            ;; Output.
            ((char= character #\.)
              (advance))
            
            ;; Skip past "}" if current cell equals zero.
            ((char= character #\{)
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
            
            ;; Jump back to "{" if current cell does not equal zero.
            ((char= character #\})
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
            
            ;; Send current cell to ...
            ((char= character #\&)
              (cond
                ;; Command is "&>."?
                ;; => Output current cell.
                ((string-follows "&>.")
                  (write-char (code-char (gethash pointer memory 0))))
                
                ;; The current cell without destination is ineffectuous.
                (T
                  (advance))))
            
            ;; The journey operator without left side is ineffectuous.
            ((char= character #\>)
              (advance))
            
            ;; Skip comments.
            (T
              (advance)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "A".
(interpret-King-Ethan "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++&>.")

;;; -------------------------------------------------------

;; Print "Hello World!"
(interpret-King-Ethan "++++++++{]++++{]++]+++]+++]+[[[[-}]+]+]-]]+{[}[-}]]&>.]---&>.+++++++&>.&>.+++&>.]]&>.[-&>.[&>.+++&>.------&>.--------&>.]]+&>.]++&>.")

;;; -------------------------------------------------------

;; Infinite cat program as a translation of brainfuck's "+[,.]".
(interpret-King-Ethan "+{,>&&>.}")

;;; -------------------------------------------------------

;; Realize an infinite cat program by exploiting the journey/destination
;; mechanism of King Ethan: The input "," is directly send to the
;; destination output "." --- without the mediation of a cell.
(interpret-King-Ethan "+{,>.}")
