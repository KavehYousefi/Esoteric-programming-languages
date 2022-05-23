;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; A reformulation of the argument names may be produced in order to
;; countenance a more lucid comprehension:
;;   
;;   3 [augend/base] [addend] [cellIndex]
;;   
;;   2 [cellIndex] [lineNumber]
;; 
;; 
;; For the instruction twain, the following pseudocode explications
;; hold:
;;   
;;   { Command "3 [x] [y] [z]". }
;;   if x = 0 then
;;     cell[z] <- 0
;;   else
;;     cell[z] <- x + y
;;     print cell[z]
;;   end if
;;   
;;   { Command "2 [x] [y]". }
;;   if cell[x] = 0 then
;;     cell[x] <- input
;;   else
;;     go to command at index y
;;   end if
;; 
;; Note that the commands are conjectured to be enumerated starting with
;; one (1), as an input of zero, obtained by using "2 [x] [y]" to store
;; such in the cell[x], would at a repeated execution incite a new input
;; instead recognizing the zero-valued cell[x] as in the user's desired
;; state.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-12
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/ISEW2C"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE."
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
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE."
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

(deftype command-name ()
  "The ``command-name'' type enumerates the recognized command
   identifiers."
  '(integer 2 3))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance functions.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returing on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun sign-character-p (character)
  "Checks whether the CHARACTER represents a numeric sign, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (find character "+-" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (name &rest parameters)))
  "The ``Command'' class encapsulates the information requisite for
   modeling an operation in the ISEW2C language."
  (name       (error "Missing command name.") :type command-name)
  (parameters NIL                             :type (list-of integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer (source
                            &aux (character
                                   (read-char source NIL NIL)))))
  "The ``Lexer'' class supplies an entity capable of recognizing the
   significant portions of a piece of ISEW2C source code."
  (source    (error "Missing lexer source.") :type stream)
  (character NIL                             :type (or null character)))

;;; -------------------------------------------------------

(defun lexer-read-next-character (lexer)
  "Consumes the next character from the LEXER's source, returning the
   yielded object, which may either be a character or ``NIL'', if the
   source is exhausted."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
        (read-char (lexer-source lexer) NIL NIL))
  (the (or null character) (lexer-character lexer)))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current position into the LEXER's source, skips zero
   or more adjacent whitespaces and returns the modified LEXER."
  (declare (type Lexer lexer))
  (loop
    while (and (lexer-character lexer)
               (whitespace-character-p (lexer-character lexer)))
    do    (lexer-read-next-character lexer))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position into the LEXER's source, reads and
   returns a signed integer value, contingently clearing it from
   preceding whitespaces."
  (declare (type Lexer lexer))
  
  (lexer-skip-whitespaces lexer)
  
  (unless (lexer-character lexer)
    (error "Expected a digit or a sign, but encountered EOF."))
  
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        
        (when (sign-character-p (lexer-character lexer))
          (write-char (lexer-character lexer) digits)
          (lexer-read-next-character lexer))
        
        (unless (lexer-character lexer)
          (error "Expected a digit, but encountered EOF."))
        
        (loop
          while (and (lexer-character lexer)
                     (digit-char-p (lexer-character lexer)))
          do
            (write-char (lexer-character lexer) digits)
            (lexer-read-next-character lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (lexer)
  "Extracts and returns a vector of commands from the LEXER's source."
  (declare (type Lexer lexer))
  
  (let ((commands NIL))
    (declare (type (list-of Command) commands))
    
    (loop while (lexer-character lexer) do
      (lexer-skip-whitespaces lexer)
      
      (unless (lexer-character lexer)
        (loop-finish))
      
      (let ((command-name (lexer-read-number lexer)))
        (declare (type integer command-name))
        (case command-name
          ;; 3 [x] [y] [z]
          (3
            (let ((x (lexer-read-number lexer))
                  (y (lexer-read-number lexer))
                  (z (lexer-read-number lexer)))
              (declare (type integer x))
              (declare (type integer y))
              (declare (type integer z))
              (push (make-command 3 x y z) commands)))
          
          ;; 2 [x] [y]
          (2
            (let ((x (lexer-read-number lexer))
                  (y (lexer-read-number lexer)))
              (declare (type integer x))
              (declare (type integer y))
              (push (make-command 2 x y) commands)))
          
          (otherwise
            (error "Invalid command name: ~s."
              (lexer-character lexer))))))
    
    (the (simple-array Command (*))
      (coerce (nreverse commands) '(simple-array Command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-commands (commands)
  "Processes the COMMANDS vector and returns no value."
  (declare (type (vector Command *) commands))
  
  (when (plusp (length commands))
    (let ((ip      0)
          (command (aref commands 0))
          (memory  (make-hash-table :test #'eql)))
      (declare (type fixnum                          ip))
      (declare (type (or null Command)               command))
      (declare (type (hash-table-of integer integer) memory))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next command,
             updates the current COMMAND, and returns no value.
             ---
             If the instruction pointer progress beyond the march of
             valid commands, that is, beyond [0, |commands| - 1],
             the current COMMAND is changed to ``NIL'' in order to
             signify the program's termination."
            (incf ip)
            (setf command
              (when (array-in-bounds-p commands ip)
                (aref commands ip)))
            (values))
           
           (jump-to (command-number)
            "Relocates the instruction pointer IP to the one-based
             COMMAND-NUMBER, concomitantly updating the current COMMAND,
             and returns no value.
             ---
             If the COMMAND-NUMBER designates an invalid command
             position, that is, a value beyond the range
             [1, (length commands)], the instruction pointer still
             assumes the respective value, but the current COMMAND
             changes to ``NIL'' in order to signify the termination of
             the program."
            (declare (type (integer 1 *) command-number))
            (setf ip (1- command-number))
            (setf command
              (when (array-in-bounds-p commands ip)
                (aref commands ip)))
            (values)))
        
        (loop while command do
          (case (command-name command)
            ;; 3 [x] [y] [z]
            (3
              (destructuring-bind (x y z)
                  (command-parameters command)
                (declare (type integer x))
                (declare (type integer y))
                (declare (type integer z))
                (cond
                  ((zerop x)
                    (setf (gethash z memory) 0))
                  (T
                    (setf (gethash z memory) (+ x y))
                    (format T "~d " (gethash z memory)))))
              (advance))
            
            ;; 2 [x] [y]
            (2
              (destructuring-bind (x y)
                  (command-parameters command)
                (declare (type integer x))
                (declare (type integer y))
                (cond
                  ((zerop (gethash x memory 0))
                    (format T "~&Please input an integer: ")
                    (let ((input (parse-integer (read-line))))
                      (declare (type integer input))
                      (clear-input)
                      (setf (gethash x memory) input))
                    (advance))
                  (T
                    (jump-to y)))))
            
            (otherwise
              (error "Invalid command: ~s." command)))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-ISEW2C (code)
  "Interprets the piece of ISEW2C CODE and returns no value."
  (declare (type string code))
  (with-input-from-string (source code)
    (declare (type string-stream source))
    (process-commands
      (extract-commands
        (make-lexer source))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the number sequence 1, 2, 3, 4, 5.
(interpret-ISEW2C
  "3 1 0 0
   3 2 0 0
   3 3 0 0
   3 4 0 0
   3 5 0 0")

;;; -------------------------------------------------------

;; Set the value of cell[0] to 1 and print it infinitely.
(interpret-ISEW2C
  "3 1 0 0
   2 0 1")

;;; -------------------------------------------------------

;; Receive an integer input and store it at cell[0].
(interpret-ISEW2C
  "2 0 1")
