;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an Alivefish interpreter based upon the
;; simplistic principle of evaluating the user input directly, destitute
;; of preceding compilations into any other form.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Alivefish"
;;   -> "https://esolangs.org/wiki/Deadfish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype output-mode ()
  "The ``output-mode'' type enumerates the valid accumulator output
   states."
  '(member :number :letter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 52) +LETTERS+))

;;; -------------------------------------------------------

(defparameter +LETTERS+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Associates, by its positions' adminicle, letters with integer
   numbers.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance functions.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-letter-for-number (number)
  "Returns the letter corresponding to the NUMBER, or ``NIL'' if no
   association can be established."
  (declare (type (integer 0 *) number))
  (the (or null character)
    (when (array-in-bounds-p +LETTERS+ number)
      (schar +LETTERS+ number))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Alivefish ()
  "Starts the Alivefish interpreter, returning no value upon its
   completion."
  (let ((accumulator 0)
        (running-p   T)
        (output-mode :number))
    (declare (type integer     accumulator))
    (declare (type boolean     running-p))
    (declare (type output-mode output-mode))
    
    (loop
      while running-p
      with  input of-type (or null string) = NIL
      do
        (format T "~&>> ")
        (setf input (read-line))
        (clear-input)
        
        (when (and input (plusp (length input)))
          (let ((position 0)
                (token    (char input 0)))
            (declare (type fixnum              position))
            (declare (type (or null character) token))
            
            (flet
                ((advance ()
                  "Moves the POSITION cursor to the next character in
                   the INPUT, if possible, updates the current TOKEN,
                   and returns no value."
                  (setf token
                    (when (array-in-bounds-p input (1+ position))
                      (char input (incf position))))
                  (values))
                 
                 (minus-follows-p ()
                  "Checks whether the character succeeding the current
                   POSITION in the INPUT constitutes a minus (\"-\"),
                   returning on confirmation a ``boolean'' value of
                   ``T'', otherwise ``NIL''."
                  (the boolean
                    (not (null
                      (and
                        (array-in-bounds-p input (1+ position))
                        (char= (char input (1+ position)) #\-)))))))
              
              (loop while token do
                (when (or (= accumulator -1) (= accumulator 256))
                  (setf accumulator 0))
                
                (case token
                  ;; Deadfish command "increment".
                  (#\i
                    (incf accumulator))
                  
                  ;; Deadfish command "decrement".
                  (#\d
                    (decf accumulator))
                  
                  ;; Deadfish command "output".
                  (#\o
                    (case output-mode
                      (:number
                        (format T "~d" accumulator))
                      (:letter
                        (format T "~a"
                          (or (get-letter-for-number accumulator)
                              "")))
                      (otherwise
                        (error "Invalid output mode: ~s."
                          output-mode))))
                  
                  ;; Deadfish command "square".
                  (#\s
                    (setf accumulator (* accumulator accumulator)))
                  
                  ;; Deadfish command "halt" (non-standard).
                  (#\h
                    (setf running-p NIL)
                    (loop-finish))
                  
                  ;; Possible Alivefish command "square root".
                  (#\v
                    (cond
                      ;; "v-" detected?
                      ;; => Square root command.
                      ((minus-follows-p)
                        (advance)
                        (setf accumulator (isqrt accumulator)))
                      ;; "v" without adjacent "-"?
                      ;; => Unrecognized command.
                      (T
                        (terpri))))
                  
                  ;; Alivefish command "number to letter".
                  (#\l
                    (setf output-mode :letter))
                  
                  ;; Alivefish command "letter to number".
                  (#\n
                    (setf output-mode :number))
                  
                  ;; Alivefish command "output space".
                  (#\Space
                    (format T "~c" #\Space))
                  
                  ((#\Newline #\Tab)
                    NIL)
                  
                  (otherwise
                    (terpri)))
                
                (advance)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "HELLO WORLD".
;; To employ this example, please invoke the interpreter using the
;; function call below and enter the code
;;   iisiiilondddloniiiiiiilooniiilon iiiiiiiilonddddddddloniiilonddddddlonddddddddlo
;; as its input.
(interpret-Alivefish)
