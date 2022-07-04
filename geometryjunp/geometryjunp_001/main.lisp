;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "geometryjunp", invented by the Esolang user "Threesodas".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-07-02
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Geometryjunp"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a hash table which maps fixnum keys
   to values of the same type, both referring to instruction pointer
   positions into a geometryjunp code, with a key either locating the
   position of a loop start instruction, associated with the position
   immediately following the matching loop end, or that of a loop end
   instruction, associated with the position immediately following the
   matching loop start."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines a linearly arranged memory composed of
   cells capable of storing an signed integer datum, while being
   amenable to an index of the same type."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based last-in first-out (LIFO)
   container, each element of which conforms to the ELEMENT-TYPE, the
   same defaults to the comprehensive ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized instruction types."
  '(member
    :start-program
    :end-program
    :increment
    :decrement
    :start-loop
    :end-loop
    :output
    :input
    :move-left
    :move-right))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Creates and returns a jump table based upon the piece of geometryjunp
   CODE."
  (declare (type string code))
  (let ((loop-starts NIL)
        (jump-table  (make-hash-table :test #'eql)))
    (declare (type (stack-of fixnum) loop-starts))
    (declare (type jump-table        jump-table))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0
      do
        (case token
          ;; Start of loop.
          (#\⦾
            (push position loop-starts))
          ;; End of loop.
          (#\■
            (cond
              (loop-starts
                (let ((start-position (pop loop-starts)))
                  (declare (type fixnum start-position))
                  (setf (gethash position jump-table)
                        (1+ start-position))
                  (setf (gethash start-position jump-table)
                        (+ position))))
              (T
                (error "Unmatched loop end instruction at position ~d."
                  position))))
          (otherwise
            NIL)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun interpret-geometryjunp (code)
  "Interprets the piece of geometryjunp CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((program-started-p NIL)
          (ip                0)
          (token             (char code 0))
          (jump-table        (build-jump-table code))
          (tape              (make-hash-table :test #'eql))
          (pointer           0))
      (declare (type boolean             program-started-p))
      (declare (type fixnum              ip))
      (declare (type (or null character) token))
      (declare (type jump-table          jump-table))
      (declare (type tape                tape))
      (declare (type integer             pointer))
      
      (flet
          ((check-if-program-is-started ()
            "Checks whether the program has been started, returning no
             value on confirmation, otherwise signaling an error of an
             unspecified type."
            (unless program-started-p
              (error "The program has not yet started."))
            (values))
           
           (advance-ip ()
            "Moves the instruction pointer IP to the next character in
             the CODE, if possible, updates the current TOKEN, and
             returns no value."
            (setf token
              (when (array-in-bounds-p code (1+ ip))
                (char code (incf ip))))
            (values))
           
           (move-ip-to (new-position)
            "Moves the instruction pointer IP to the NEW-POSITION,
             updates the current TOKEN, and returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf token
              (when (array-in-bounds-p code ip)
                (char code ip)))
            (values))
           
           (current-cell ()
            "Returns the value of the currently selected cell."
            (the integer (gethash pointer tape 0)))
           
           ((setf current-cell) (new-value)
            "Sets the value of the currently selected cell to the
             NEW-VALUE and returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer tape 0) new-value)
            (values)))
        
        (loop do
          (case token
            ;; End of file.
            ((NIL)
              (loop-finish))
            
            ;; Start program.
            (#\□
              (cond
                (program-started-p
                  (error "Encountered a program start command at the ~
                          position ~d; but the program is already ~
                          started."
                    ip))
                (T
                  (setf program-started-p T)
                  (advance-ip))))
            
            ;; Terminate program.
            (#\]
              (check-if-program-is-started)
              (loop-finish))
            
            ;; Increment current cell value.
            (#\_
              (check-if-program-is-started)
              (incf (current-cell))
              (advance-ip))
            
            ;; Decrement current cell value.
            (#\▲
              (check-if-program-is-started)
              (decf (current-cell))
              (advance-ip))
            
            ;; Start loop.
            (#\⦾
              (check-if-program-is-started)
              (if (zerop (current-cell))
                (move-ip-to (gethash ip jump-table))
                (advance-ip)))
            
            ;; End loop.
            (#\■
              (check-if-program-is-started)
              (if (zerop (current-cell))
                (advance-ip)
                (move-ip-to (gethash ip jump-table))))
            
            ;; Print current cell value as an ASCII character.
            (#\›
              (check-if-program-is-started)
              (format T "~c" (code-char (current-cell)))
              (advance-ip))
            
            ;; Input ASCII character and store its code in current cell.
            (#\»
              (check-if-program-is-started)
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (current-cell) (char-code input)))
              (advance-ip))
            
            ;; Switch to the left cell.
            (#\▴
              (check-if-program-is-started)
              (decf pointer)
              (advance-ip))
            
            ;; Switch to the right cell.
            (#\◢
              (check-if-program-is-started)
              (incf pointer)
              (advance-ip))
            
            ;; Skip any other character as comment.
            (otherwise
              (advance-ip)))))))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of geometryjunp code generator.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of command character) +INSTRUCTION-TOKENS+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-TOKENS+
  (make-hash-table :test #'eq :size 10)
  "Associates with each ``command'' the respective geometryjunp token
   character.")

;;; -------------------------------------------------------

(setf (gethash :start-program +INSTRUCTION-TOKENS+) #\□)
(setf (gethash :end-program   +INSTRUCTION-TOKENS+) #\])
(setf (gethash :increment     +INSTRUCTION-TOKENS+) #\_)
(setf (gethash :decrement     +INSTRUCTION-TOKENS+) #\▲)
(setf (gethash :start-loop    +INSTRUCTION-TOKENS+) #\⦾)
(setf (gethash :end-loop      +INSTRUCTION-TOKENS+) #\■)
(setf (gethash :output        +INSTRUCTION-TOKENS+) #\›)
(setf (gethash :input         +INSTRUCTION-TOKENS+) #\»)
(setf (gethash :move-left     +INSTRUCTION-TOKENS+) #\▴)
(setf (gethash :move-right    +INSTRUCTION-TOKENS+) #\◢)

;;; -------------------------------------------------------

(defun convert-geometryjunp-instructions-to-code
    (instructions
     &key (destination NIL))
  "Generates the geometryjunp code equivalent to the INSTRUCTIONS and
   writes it to the DESTINATION, returning ``NIL'' for a non-``NIL''
   DESTINATION, otherwise producing and returning a fresh string
   comprehending the result."
  (declare (type (list-of command) instructions))
  (declare (type destination       destination))
  (the (or null string)
    (if destination
      (dolist (instruction instructions)
        (declare (type command instruction))
        (format destination "~c"
          (gethash instruction +INSTRUCTION-TOKENS+)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-geometryjunp-instructions-to-code instructions
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-geometryjunp converter.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character character)
               +BRAINFUCK-DECODER-TABLE+))

;;; -------------------------------------------------------

(defparameter +BRAINFUCK-DECODER-TABLE+
  (make-hash-table :test #'eql :size 8)
  "Associates with each brainfuck instruction token the corresponding
   geometryjunp instruction token.")

;;; -------------------------------------------------------

(setf (gethash #\< +BRAINFUCK-DECODER-TABLE+) #\▴)
(setf (gethash #\> +BRAINFUCK-DECODER-TABLE+) #\◢)
(setf (gethash #\+ +BRAINFUCK-DECODER-TABLE+) #\_)
(setf (gethash #\- +BRAINFUCK-DECODER-TABLE+) #\▲)
(setf (gethash #\. +BRAINFUCK-DECODER-TABLE+) #\›)
(setf (gethash #\, +BRAINFUCK-DECODER-TABLE+) #\»)
(setf (gethash #\[ +BRAINFUCK-DECODER-TABLE+) #\⦾)
(setf (gethash #\] +BRAINFUCK-DECODER-TABLE+) #\■)

;;; -------------------------------------------------------

(defun convert-brainfuck-to-geometryjunp (brainfuck-code
                                          &key (destination NIL))
  "Converts the piece of BRAINFUCK-CODE to the equivalent geometryjunp
   program and writes it to the DESTINATION, returning for a non-``NIL''
   DESTINATION ``NIL'', otherwise generating and returning a fresh
   string containing the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        initially
          (format destination "□")
        for brainfuck-token
          of-type character
          across  brainfuck-code
        do
          (format destination "~c"
            (gethash brainfuck-token +BRAINFUCK-DECODER-TABLE+
              brainfuck-token))
        finally
          (format destination "]"))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-geometryjunp brainfuck-code
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program which terminates on an input of the
;; null character.
(interpret-geometryjunp "□»⦾›»■]")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Memory layout:
;;   tape[0] <- input
;;   tape[1] <- copy of input; shall later hold either zero (0) or one (1)
;;   tape[2] <- copy of input; used for later printing of the user input
;;   tape[3] <- 48; used reduce tape[1] to zero or one
(interpret-geometryjunp "□»›⦾◢_◢_▴▴▲■◢◢◢________________________________________________⦾▴▴▲◢◢▲■▴▴⦾◢›▴■]")

;;; -------------------------------------------------------

;; Generate the geometryjunp source code for a truth-machine from its
;; instructions.
;; 
;; Memory layout:
;;   tape[0] <- input
;;   tape[1] <- copy of input; shall later hold either zero (0) or one (1)
;;   tape[2] <- copy of input; used for later printing of the user input
;;   tape[3] <- 48; used reduce tape[1] to zero or one
(convert-geometryjunp-instructions-to-code
  (list
    :start-program
    
    :input
    :output
    
    ;; Set
    ;;   tape[1] <- copyOfInput
    ;;   tape[2] <- copyOfInput
    :start-loop
    :move-right
    :increment
    :move-right
    :increment
    :move-left
    :move-left
    :decrement
    :end-loop
    
    ;; Set
    ;;  tape[3] <- 48
    :move-right
    :move-right
    :move-right
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    :increment
    
    ;; Set
    ;;   tape[1] <- 0/1
    ;;   tape[3] <- 0
    :start-loop
    :move-left
    :move-left
    :decrement
    :move-right
    :move-right
    :decrement
    :end-loop
    
    ;; Go to tape[2] (= input).
    :move-left
    ;; Go to tape[1] (= 0/1)
    :move-left
    
    ;; Print tape[2] (= input).
    ;; Contingently repeat, depending on the value of tape[1] (= 0/1).
    :start-loop
    :move-right
    :output
    :move-left
    :end-loop
    
    :end-program))

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program into geometryjunp print
;; the resulting code
;;   □________⦾◢____⦾◢__◢___◢___◢_▴▴▴▴▲■◢_◢_◢▲◢◢_⦾▴■▴▲■◢◢›◢▲▲▲›_______››___›◢◢›▴▲›▴›___›▲▲▲▲▲▲›▲▲▲▲▲▲▲▲›◢◢_›◢__›]
;; to the standard output.
(convert-brainfuck-to-geometryjunp
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  :destination T)

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program into geometryjunp and
;; execute the resulting program.
(interpret-geometryjunp
  (convert-brainfuck-to-geometryjunp
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))
