;; Author: Kaveh Yousefi
;; Date:   2022-03-22
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BurgerFlipper"



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
  "The ``memory'' type defines a representation of the program memory,
   based on a hash table which associates signed integer keys in the
   agency of cell indices to signed integer values construed as the
   cell values."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BurgerFlipper (code)
  "Interprets the piece of BurgerFlipper CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (memory    (make-hash-table :test #'eql))
          (pointer   0)
          (flipped-p NIL))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type memory              memory))
      (declare (type integer             pointer))
      (declare (type boolean             flipped-p))
      
      (labels
          ((advance ()
            "Moves to POSITION cursor to the next character in the CODE,
             if possible, updates the current CHARACTER, and returns no
             value."
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
           
           (command-character-p (primary-character
                                 &optional
                                 (flipped-character primary-character))
            "Based on the FLIPPED-P state of the program, checks whether
             the CHARACTER either equals the PRIMARY-CHARACTER, if the
             state is not flipped, or the FLIPPED-CHARACTER, if the
             state is flipped, returning on parity a ``boolean'' value
             of ``T'', otherwise ``NIL''."
            (declare (type character primary-character))
            (declare (type character flipped-character))
            (the boolean
              (not (null
                (or
                  (and (char= character primary-character)
                       (not flipped-p))
                  (and (char= character flipped-character)
                       flipped-p))))))
           
           (flip ()
            "Flips the FLIPPED-P state and returns no value."
            (setf flipped-p (not flipped-p))
            (values)))
        
        (loop do
          (cond
            ;; End of program.
            ((null character)
              (loop-finish))
            
            ;; Move memory pointer right by one cell.
            ((command-character-p #\> #\f)
              (incf pointer)
              (flip)
              (advance))
            
            ;; Move memory pointer left by one cell.
            ((command-character-p #\< #\b)
              (decf pointer)
              (flip)
              (advance))
            
            ;; Increment.
            ((command-character-p #\+ #\a)
              (incf (gethash pointer memory 0))
              (flip)
              (advance))
            
            ;; Decrement.
            ((command-character-p #\- #\s)
              (decf (gethash pointer memory 0))
              (flip)
              (advance))
            
            ;; Output.
            ((command-character-p #\. #\o)
              (write-char (code-char (gethash pointer memory 0)))
              (flip)
              (advance))
            
            ;; Input.
            ((command-character-p #\, #\i)
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (gethash pointer memory)
                      (char-code input)))
              (flip)
              (advance))
            
            ;; Potentially skip code until matching "]".
            ((command-character-p #\[)
              (cond
                ((zerop (gethash pointer memory 0))
                  (advance)
                  
                  (loop with level of-type integer = 0 do
                    (case character
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
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Potentially return to matching "[".
            ((command-character-p #\])
              (cond
                ((zerop (gethash pointer memory 0))
                  (advance))
                (T
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case character
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
                      (otherwise
                        (recede)))))))
            
            ;; Ignore any other character.
            (T
              (advance)))))))
  
  (format T "~&Thank you burger flipper!")
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-BurgerFlipper ",o[,o]")

;;; -------------------------------------------------------

;; Increment 10 bytes by 2.
(interpret-BurgerFlipper "+a>a+f+a>a+f+a>a+f+a>a+f+a>a+")

;;; -------------------------------------------------------

;; Print "Hello World!".
(interpret-BurgerFlipper "+a+a+a+a+a[>a+a+a+a>a+a+a+a+a+f+a+f+b<b<s]>a+o>a.a+a+a+a.o+a+o>a+o<b+a+a+a+a+a+a+a+o>o+a+o-s-s-s.s-s-s-s-o>a.")
