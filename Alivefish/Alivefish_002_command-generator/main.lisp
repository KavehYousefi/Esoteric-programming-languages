;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an Alivefish interpreter based upon the
;; generation of dedicated command objects from user inputs, which
;; subsequently constitute the tokens of currency in the evalution of
;; the thus delineated instruction sequence.
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

(deftype command ()
  "The ``command'' type enumerates the recognized commands.
   ---
   Please note the admission of several adscititious specimens in this
   set as the means for handling special input cases."
  '(member
    :increment
    :decrement
    :output
    :square
    :halt
    :square-root
    :number-to-letter
    :letter-to-number
    :output-space
    :whitespace
    :nop
    :eof))

;;; -------------------------------------------------------

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
;; -- Implementation of class "Input".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input ()
  ((source
    :initarg       :source
    :initform      NIL
    :type          (or null string)
    :documentation "The most recent command sequence supplied by the
                    user.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE.
                    ---
                    Upon the SOURCE's exhaustion, the sentinal value
                    ``NIL'' applies itself to this state's
                    designation."))
  (:documentation
    "The ``Input'' class maintains a representation of a user's input,
     that is, a sequence of zero or more characters which might contain
     Alivefish instructions."))

;;; -------------------------------------------------------

(defun make-input ()
  "Creates and returns an empty ``Input''."
  (the Input (make-instance 'Input)))

;;; -------------------------------------------------------

(defun input-set-to (input new-source)
  "Sets the INPUT's data to the NEW-SOURCE, resetting all position and
   character information, and returns the modified INPUT."
  (declare (type Input            input))
  (declare (type (or null string) new-source))
  (with-slots (source position character) input
    (declare (type (or null string)    source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf source   new-source)
    (setf position 0)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Input input))

;;; -------------------------------------------------------

(defun input-empty-p (input)
  "Checks whether the INPUT is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Input input))
  (with-slots (source) input
    (declare (type (or null string) source))
    (the boolean
      (not (null
        (or (null source)
            (zerop (length source))))))))

;;; -------------------------------------------------------

(defun input-advance (input)
  "Moves the INPUT's position cursor to the next character in its
   SOURCE, if possible, updates the current character, and returns the
   modified INPUT."
  (declare (type Input input))
  (with-slots (source position character) input
    (declare (type (or null string)    source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Input input))

;;; -------------------------------------------------------

(defun input-minus-follows-p (input)
  "Checks whether the character succeeding the current position into the
   INPUT's source constitutes a minus (\"-\"), returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Input input))
  (with-slots (source position character) input
    (declare (type (or null string) source))
    (declare (type fixnum           position))
    (the boolean
      (not (null
        (and
          (array-in-bounds-p source (1+ position))
          (char= (char source (1+ position)) #\-)))))))

;;; -------------------------------------------------------

(defun input-get-next-command (input)
  "Returns from the INPUT the next command.
   ---
   Ensuing from its source's exhaustion, the INPUT returns upon each
   invocation a response of the ``:eof'' kind."
  (declare (type Input input))
  (with-slots (character) input
    (declare (type (or null character) character))
    (the command
      (prog1
        (case character
          ((NIL) :eof)
          (#\i   :increment)
          (#\d   :decrement)
          (#\o   :output)
          (#\s   :square)
          (#\h   :halt)
          
          (#\v
            (cond
              ((input-minus-follows-p input)
                (input-advance input)
                :square-root)
              (T
                :nop)))
          (#\l               :number-to-letter)
          (#\n               :letter-to-number)
          (#\Space           :output-space)
          
          ((#\Tab #\Newline) :whitespace)
          (otherwise         :nop))
        (input-advance input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Alivefish ()
  "Starts the Alivefish interpreter, returning no value upon its
   completion."
  (let ((accumulator 0)
        (running-p   T)
        (output-mode :number)
        (input       (make-input)))
    (declare (type integer     accumulator))
    (declare (type boolean     running-p))
    (declare (type output-mode output-mode))
    (declare (type Input       input))
    
    (loop while running-p do
      (format T "~&>> ")
      (input-set-to input (read-line))
      (clear-input)
      
      (unless (input-empty-p input)
        (loop
          for   command of-type command = (input-get-next-command input)
          until (eq command :eof)
          do
            (when (or (= accumulator -1) (= accumulator 256))
              (setf accumulator 0))
            
            (case command
              ;; Deadfish command "increment".
              (:increment
                (incf accumulator))
              
              ;; Deadfish command "decrement".
              (:decrement
                (decf accumulator))
              
              ;; Deadfish command "output".
              (:output
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
              (:square
                (setf accumulator (* accumulator accumulator)))
              
              ;; Deadfish command "halt" (non-standard).
              (:halt
                (setf running-p NIL)
                (loop-finish))
              
              ;; Possible Alivefish command "square root".
              (:square-root
                (setf accumulator (isqrt accumulator)))
              
              ;; Alivefish command "number to letter".
              (:number-to-letter
                (setf output-mode :letter))
              
              ;; Alivefish command "letter to number".
              (:letter-to-number
                (setf output-mode :number))
              
              ;; Alivefish command "output space".
              (:output-space
                (format T "~c" #\Space))
              
              (:whitespace
                NIL)
              
              (:nop
                (terpri))
              
              (otherwise
                (error "Unrecognized command: ~s." command)))))))
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
