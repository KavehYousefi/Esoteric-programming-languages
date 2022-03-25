;; Author: Kaveh Yousefi
;; Date:   2022-03-25
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/SPL"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to ``T''."
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
  "The ``memory'' type defines the SPL program memory as a hash table
   which associates with each integer cell index another integer cell
   value."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized directions for
   traversing a piece of SPL code."
  '(member :forward :backward))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-SPL (code)
  "Interprets the piece of SPL CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position    0)
          (character   (char code 0))
          (memory      (make-hash-table :test #'eql))
          (pointer     0)
          (accumulator 0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type memory              memory))
      (declare (type integer             pointer))
      (declare (type integer             accumulator))
      
      (labels
          ((advance ()
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
              (when (array-in-bounds-p code (1- position))
                (char code (decf position))))
            (values))
           
           (skip-comment (direction)
            "Starting at the current POSITION and expecting to be
             commorant at the start of a comment, skips the comment
             while moving in the DIRECTION, relocates the POSITION
             cursor to the first character following the comment
             portion, and returns no value."
            (declare (type direction direction))
            (case direction
              (:forward
                (advance)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated comment."))
                    (#\#
                      (advance)
                      (loop-finish))
                    (otherwise
                      (advance)))))
              (:backward
                (recede)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated comment."))
                    (#\#
                      (recede)
                      (loop-finish))
                    (otherwise
                      (recede)))))
              (otherwise
                (error "Invalid direction: ~s." direction)))
            (values))
           
           (skip-string (direction)
            "Starting at the current POSITION and expecting to be
             commorant at the start of a string, skips the string while
             moving in the DIRECTION, relocates the POSITION cursor to
             the first character following the string portion portion,
             and returns no value."
            (declare (type direction direction))
            (case direction
              (:forward
                (advance)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated string."))
                    (#\"
                      (advance)
                      (loop-finish))
                    (otherwise
                      (advance)))))
              (:backward
                (recede)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated string."))
                    (#\"
                      (recede)
                      (loop-finish))
                    (otherwise
                      (recede)))))
              (otherwise
                (error "Invalid direction: ~s." direction)))
            (values))
           
           (read-string ()
            "Starting at the current POSITION and expecting to be
             commorant at the start of a string, consume and returns its
             content, relocating the POSITION cursor to the character
             immediately following the terminating quote."
            (advance)
            (the string
              (with-output-to-string (characters)
                (declare (type string-stream characters))
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated string."))
                    (#\"
                      (advance)
                      (loop-finish))
                    (otherwise
                      (write-char character characters)
                      (advance)))))))
           
           (prompt-input ()
            "Reads from the standard input an integer input and returns
             the same."
            (the integer
              (loop
                for     input of-type T = (read)
                until   (integerp input)
                finally (return input)))))
        
        (loop do
          (case character
            ;; End of program. => Terminate program.
            ((NIL)
              (loop-finish))
            
            ;; Move the memory pointer one cell to the right.
            (#\>
              (incf pointer)
              (advance))
            
            ;; Move the memory pointer one cell to the left.
            (#\<
              (decf pointer)
              (advance))
            
            ;; Output the current cell's integer value.
            (#\.
              (format T "~d" (gethash pointer memory 0))
              (advance))
            
            ;; Input an integer and store it in the current cell.
            (#\,
              (let ((input (prompt-input)))
                (declare (type integer input))
                (clear-input)
                (setf (gethash pointer memory) input))
              (advance))
            
            ;; Jump past the matching "]" if the accumulator is zero.
            (#\[
              (cond
                ((zerop accumulator)
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated '['."))
                      (#\[
                        (incf level)
                        (advance))
                      (#\]
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (#\"
                        (skip-string :forward))
                      (#\#
                        (skip-comment :forward))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Jump back past the matching "[" if the accumulator does
            ;; not equal zero.
            (#\]
              (cond
                ((zerop accumulator)
                  (advance))
                (T
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated ']'."))
                      (#\]
                        (incf level)
                        (recede))
                      (#\[
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#\"
                        (skip-string :backward))
                      (#\#
                        (skip-comment :backward))
                      (otherwise
                        (recede)))))))
            
            ;; Increase the current cell value by one.
            (#\+
              (incf (gethash pointer memory 0))
              (advance))
            
            ;; Decrease the current cell value by one.
            (#\-
              (decf (gethash pointer memory 0))
              (advance))
            
            ;; Load the current cell value into the accumulator.
            (#\^
              (setf accumulator (gethash pointer memory 0))
              (advance))
            
            ;; Store the user input in the accumulator.
            (#\$
              (let ((input (prompt-input)))
                (declare (type integer input))
                (clear-input)
                (setf accumulator input))
              (advance))
            
            ;; Set the current cell value to its remainder when divided
            ;; by the accumulator.
            (#\%
              (setf (gethash pointer memory)
                    (rem (gethash pointer memory 0) accumulator))
              (advance))
            
            ;; Multiply the current cell value by the accumulator.
            (#\m
              (setf (gethash pointer memory)
                    (* (gethash pointer memory 0) accumulator))
              (advance))
            
            ;; Add the accumulator to the current cell value.
            (#\a
              (incf (gethash pointer memory 0) accumulator)
              (advance))
            
            ;; Subtract the accumulator from the current cell value.
            (#\s
              (decf (gethash pointer memory 0) accumulator)
              (advance))
            
            ;; Divide the current cell value by the accumulator.
            (#\d
              (setf (gethash pointer memory)
                    (round (gethash pointer memory 0) accumulator))
              (advance))
            
            ;; Terminate the program.
            (#\&
              (loop-finish))
            
            ;; Print a string literal.
            (#\"
              (let ((string (read-string)))
                (declare (type string string))
                (format T "~a" string)))
            
            ;; Skip a comment.
            (#\#
              (skip-comment :forward))
            
            (otherwise
              (error "Invalid character '~a' at position ~d."
                character position)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World", followed by a linebreak.
(interpret-SPL "\"Hello World
\"")

;;; -------------------------------------------------------

;; Ask the user for a number and print the numbers counting down to
;; zero.
(interpret-SPL "\"n: \",^[.\" \"-^].\"
\"")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-SPL ",.^[\"
\",.^]")
