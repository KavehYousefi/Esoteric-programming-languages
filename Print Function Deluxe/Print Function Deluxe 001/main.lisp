;; Date: 2021-12-28
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Print_Function_Deluxe"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-lines (code)
  "Splits the CODE into lines and returns these as a vector of strings."
  (declare (type string code))
  (the (vector string)
    (with-input-from-string (code-stream code)
      (declare (type string-stream code-stream))
      (loop
        for line
          of-type (or null string) = (read-line code-stream NIL)
        while line
          collect line
          into    lines
        finally (return (coerce lines '(vector string)))))))

;;; -------------------------------------------------------

(defun interpret-Print-Function-Deluxe (code)
  "Interprets the piece of Print Function Deluxe CODE and returns no
   value."
  (declare (type string code))
  
  (let* ((lines      (collect-lines code))
         (line-index 0)
         (line       (aref lines 0))
         (column     0)
         (character  (char line 0)))
    (declare (type (vector string)     lines))
    (declare (type fixnum              line-index))
    (declare (type string              line))
    (declare (type fixnum              column))
    (declare (type (or null character) character))
    
    (let ((accumulator 0))
      (declare (type integer accumulator))
      
      (flet
          ((advance ()
            "Moves the COLUMN pointer to the next character in the
             current line, if possible, and return no value."
            (setf character
              (when (< column (1- (length line)))
                (char line (incf column))))
            (values))
           
           (go-to-line (line-number)
            "Moves the COLUMN pointer to the start of the line with the
             one-based LINE-NUMBER, returning no value."
            (declare (type (integer 1 *) line-number))
            (setf line-index (1- line-number))
            (setf line       (aref lines line-index))
            (setf column     0)
            (setf character  (char line column))
            (values)))
        
        (loop do
          (cond
            ((null character)
              (loop-finish))
            
            ;; Print the next three characters.
            ((char= character #\p)
              (advance)
              (loop repeat 3 do
                (write-char character)
                (advance)))
            
            ;; Prompt input and use it as an argument.
            ((char= character #\$)
              (advance)
              (case character
                ((NIL)
                  (loop-finish))
                
                ;; Print input.
                (#\p
                  (advance)
                  (format T "~&Please input three characters to print: ")
                  (let ((input (read-line)))
                    (declare (type string input))
                    (clear-input)
                    (loop for index of-type (integer 0 4) from 0 below 3 do
                      (write-char
                        (if (< index (length input))
                          (char input index)
                          #\Space)))))
                
                ;; Set accumulator to input (three characters).
                (#\%
                  (advance)
                  (format T "~&Please input three digits to set ~
                               the accumulator to: ")
                  (setf accumulator (parse-integer (read-line) :radix 36))
                  (clear-input))
                
                ;; Conditionally go to line, reading line number from
                ;; input.
                (#\=
                  (advance)
                  (format T "~&Please input a digit determining the ~
                               line offset: ")
                  (let ((line-offset (digit-char-p (read-char) 36)))
                    (declare (type (integer 0 35) line-offset))
                    (clear-input)
                    (go-to-line (+ line-index line-offset 1))))
                
                ;; Increment accumulator by input.
                (#\+
                  (advance)
                  (format T "~&Please input a digit determining the ~
                               accumulator increment: ")
                  (let ((increment (digit-char-p (read-char) 36)))
                    (declare (type (integer 0 35) increment))
                    (clear-input)
                    (incf accumulator increment)))
                
                ;; Decrement accumulator by input.
                (#\-
                  (advance)
                  (format T "~&Please input a digit determining the ~
                               accumulator decrement: ")
                  (let ((decrement (digit-char-p (read-char) 36)))
                    (declare (type (integer 0 35) decrement))
                    (clear-input)
                    (incf accumulator decrement)))
                
                ;; Set accumulator to input (one character).
                (#\&
                  (advance)
                  (format T "~&Please enter a digit determining the ~
                               new accumulator value: ")
                  (setf accumulator (digit-char-p (read-char) 36))
                  (clear-input))
                
                ;; Unconditionally go to line equal to input.
                (#\@
                  (advance)
                  (format T "~&Please enter a digit determining the ~
                               new line number: ")
                  (let ((new-line-number (digit-char-p (read-char) 36)))
                    (clear-input)
                    (go-to-line new-line-number)))
                
                ;; Ignore commands without argument.
                (otherwise
                  NIL)))
            
            ;; Set accumulator to the next three characters.
            ((char= character #\%)
              (advance)
              (setf accumulator
                (parse-integer
                  (with-output-to-string (digits)
                    (declare (type string-stream digits))
                    (loop repeat 3 do
                      (write-char character digits)
                      (advance)))
                  :radix 36)))
            
            ;; Utilize the accumulator as an argument.
            ((char= character #\^)
              (advance)
              
              ;; Print the accumulator.
              (case character
                (#\p
                  (write-char (code-char accumulator))
                  (advance))
                
                ;; Set the accumulator to itself (one character).
                (#\%
                  (setf accumulator accumulator)
                  (advance))
                
                ;; Conditionally skip the next character, reading line
                ;; number from accumulator.
                (#\=
                  (cond
                    ((evenp accumulator)
                      (go-to-line (+ line-index accumulator 1)))
                    (T
                      (advance))))
                
                ;; Increment the accumulator by itself.
                (#\+
                  (incf accumulator accumulator)
                  (advance))
                
                ;; Decrement the accumulator by itself.
                (#\-
                  (decf accumulator accumulator)
                  (advance))
                
                ;; Set the accumulator to itself (three characters).
                (#\&
                  (setf accumulator accumulator)
                  (advance))
                
                ;; Go to line number equal to accumulator.
                (#\@
                  (go-to-line accumulator))
                
                ;; Ignore commands without argument.
                (otherwise
                  NIL)))
            
            ;; Conditionally go to line, reading from the accumulator.
            ((char= character #\=)
              (advance)
              (cond
                ((evenp accumulator)
                  (let ((line-offset (digit-char-p character 36)))
                    (declare (type (integer 0 35) line-offset))
                    (advance)
                    (go-to-line (+ line-index line-offset 1))))
                (T
                  (advance))))
            
            ;; Increment the accumulator by next character.
            ((char= character #\+)
              (advance)
              (incf accumulator (digit-char-p character 36))
              (advance))
            
            ;; Decrement the accumulator by next character.
            ((char= character #\-)
              (advance)
              (decf accumulator (digit-char-p character 36))
              (advance))
            
            ;; Double the accumulator.
            ((char= character #\*)
              (advance)
              (setf accumulator (* accumulator 2)))
            
            ;; Set accumulator to random number.
            ((char= character #\?)
              (advance)
              (setf accumulator (random 36)))
            
            ;; Set accumulator to next character.
            ((char= character #\&)
              (advance)
              (setf accumulator (digit-char-p character 36))
              (advance))
            
            ;; Unconditionally go to line, reading next character.
            ((char= character #\@)
              (advance)
              (let ((new-line-number (digit-char-p character 36)))
                (declare (type (integer 0 35) new-line-number))
                (advance)
                (go-to-line new-line-number)))
            
            ;; End the program.
            ((char= character #\.)
              (loop-finish))
            
            (T
              (error "Invalid character ~s at column ~d of line ~d."
                character column line-index)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Print-Function-Deluxe "pHelplo,p Woprldp!  ")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Print-Function-Deluxe "$p")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Print-Function-Deluxe "$p@1")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Print-Function-Deluxe "$&=1@3
p0  .
p1  @3")

;;; -------------------------------------------------------

;; The "Truth-machine" modified in order to work with the accumulator as
;; an argument to the print function "p".
;; 
;; To this end, the accumulator, after having received a user input,
;; increments the same by a value of 48, replacing the actual input with
;; the ASCII code for "0" or "1". When printing using the accumulator as
;; an argument, the corresponding ASCII character "0" or "1" is
;; displayed.
(interpret-Print-Function-Deluxe "$&+Z+D=1@3
^p.
^p@3")
