;; Author: Kaveh Yousefi
;; Date:   2022-03-21
;; 
;; Sources:
;;  -> "https://esolangs.org/wiki/DirectBrainFUCK"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-DirectBrainFUCK (code)
  "Interprets the piece of DirectBrainFUCK CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (memory    (make-array 64
                       :element-type    'integer
                       :initial-element 0))
          (pointer   0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type (vector integer 64) memory))
      (declare (type (integer 0 63)      pointer))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character, if
             possible, updates the current CHARACTER, and returns no
             value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (read-cell-index ()
            "Starting at the current POSITION, reads an unsigned integer
             value, moves the POSITION cursor to the first non-digit
             character, and returns the parsed number."
            (the (integer 0 *)
              (parse-integer
                (with-output-to-string (digits)
                  (declare (type string-stream digits))
                  (loop
                    while (and character (digit-char-p character))
                    do
                      (write-char character digits)
                      (advance)))))))
        
        (loop do
          (case character
            ;; End of code.
            ((NIL)
              (loop-finish))
            
            ;; Move the memory pointer one cell to the left.
            (#\<
              (when (>= pointer 1)
                (decf pointer))
              (advance))
            
            ;; Move the memory pointer one cell to the right.
            (#\>
              (when (<= pointer 62)
                (incf pointer))
              (advance))
            
            ;; Set the memory pointer to the cell index.
            (#\m
              (advance)
              (let ((cell-index (read-cell-index)))
                (declare (type (integer 0 *) cell-index))
                (if (<= 1 cell-index 64)
                  (setf pointer (1- cell-index))
                  (error "Invalid cell index: ~d." cell-index))))
            
            ;; Prompt the user for an ASCII character and store its
            ;; code.
            (#\,
              (format T "~&Please input a character: ")
              (setf (aref memory pointer)
                    (char-code (read-char)))
              (clear-input)
              (advance))
            
            ;; Print the ASCII character corresponding to the value of
            ;; the current cell.
            (#\.
              (write-char (code-char (aref memory pointer)))
              (advance))
            
            ;; Ignore any other character.
            (otherwise
              (advance)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-DirectBrainFUCK ",.")

;;; -------------------------------------------------------

;; One-time cat program using random access.
(interpret-DirectBrainFUCK ">>,<<m3.")
