;; Author: Kaveh Yousefi
;; Date:   2022-03-02
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/%3D,-%26~"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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
  "The ``stack-of'' type defines a list-based stack of zero or more
   elements, each of which conforms to the ELEMENT-TYPE, default to
   ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and the
   associated value to the VALUE-TYPE, both defaulting to ``T''."
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, without a claim of exhaustion, ``format'', ``write'',
   and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized =,-&~ commands."
  '(member
    :push-0
    :add-1
    :add-10
    :add-100
    :negate
    :move-up
    :move-down
    :pop
    :swap
    :duplicate
    :jump-forward
    :jump-back
    :plus
    :minus
    :multiply
    :divide
    :input-number
    :input-character
    :output-number
    :output-character))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string instruction) +INSTRUCTION-TABLE+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-TABLE+ (make-hash-table :test #'equal)
  "A mapping of tokens to =,-&~ instructions.")

;;; -------------------------------------------------------

(flet ((add-instruction (token instruction)
        "Associates the TOKEN with the =,-&~ INSTRUCTION in the
         +INSTRUCTION-TABLE+ and returns no value."
        (declare (type string      token))
        (declare (type instruction instruction))
        (setf (gethash token +INSTRUCTION-TABLE+) instruction)
        (values)))
  (add-instruction "-"  :push-0)
  (add-instruction "="  :add-1)
  (add-instruction "≡"  :add-10)
  (add-instruction "~"  :add-100)
  (add-instruction "∽"  :negate)
  (add-instruction "∸"  :move-up)
  (add-instruction "--" :move-down)
  (add-instruction "-=" :pop)
  (add-instruction "-≡" :swap)
  (add-instruction "-~" :duplicate)
  (add-instruction "-∽" :jump-forward)
  (add-instruction "-∸" :jump-back)
  (add-instruction "=-" :plus)
  (add-instruction "==" :minus)
  (add-instruction "=≡" :multiply)
  (add-instruction "=~" :divide)
  (add-instruction "=∽" :input-number)
  (add-instruction "=∸" :input-character)
  (add-instruction "≡-" :output-number)
  (add-instruction "≡=" :output-character)
  (values))

;;; -------------------------------------------------------

(defun get-instruction-for (token)
  "Returns the instruction associated with the TOKEN, or signals an
   error if no such association could be established."
  (declare (type string token))
  (multiple-value-bind (instruction contains-token-p)
      (gethash token +INSTRUCTION-TABLE+)
    (declare (type (or null instruction) instruction))
    (declare (type T                     contains-token-p))
    (the instruction
      (if contains-token-p
        instruction
        (error "Unrecognized instruction token: ~s." token)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun instruction-character-p (character)
  "Checks whether the CHARACTER represents a valid =,-&~ instruction
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (find character "-=≡~∽∸" :test #'char=)))))

;;; -------------------------------------------------------

(defun parse-instructions (code)
  "Analyzes the piece of =,-&~ CODE and returns a vector of instructions
   in representation of the contained program."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of instruction) instructions))
    
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (labels
            ((advance ()
              "Moves the POSITION to the next character, if possible,
               updates the current CHARACTER, and returns no value."
              (setf character
                (when (< position (1- (length code)))
                  (char code (incf position))))
              (values))
             
             (move-to (new-position)
              "Relocates the POSITION cursor to the NEW-POSITION,
               updates the current CHARACTER, and returns no value."
              (declare (type fixnum new-position))
              (setf position  new-position)
              (setf character (char code position))
              (values))
             
             (skip-whitespaces ()
              "Starting at the current POSITION, skips zero or more
               whitespaces, and returns no value."
              (loop
                while (and character (whitespace-character-p character))
                do    (advance))
              (values))
             
             (read-instruction ()
              "Starting at the current POSITION, reads a command token
               and returns the associated =,-&~ instruction."
              (the instruction
                (get-instruction-for
                  (with-output-to-string (identifier)
                    (declare (type string-stream identifier))
                    (loop
                      while
                        (and character
                             (instruction-character-p character))
                      do
                        (write-char character identifier)
                        (advance)))))))
          
          (loop do
            (cond
              ((null character)
                (loop-finish))
              
              ((whitespace-character-p character)
                (skip-whitespaces))
              
              ((instruction-character-p character)
                (push (read-instruction) instructions))
              
              (T
                (error "Invalid character ~s at position ~d."
                  character position)))))))
    
    (the (simple-array instruction (*))
      (coerce
        (nreverse instructions)
        '(simple-array instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Processes the =,-&~ INSTRUCTIONS and returns no value."
  (declare (type (simple-array instruction (*)) instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (stack       NIL))
      (declare (type fixnum                ip))
      (declare (type (or null instruction) instruction))
      (declare (type (stack-of integer)    stack))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the current INSTRUCTION reference, and
             returns no value."
            (setf instruction
              (when (< ip (1- (length instructions)))
                (aref instructions (incf ip))))
            (values))
           
           (recede ()
            "Moves the instruction pointer IP to the previous
             instruction, if possible, updates the current INSTRUCTION
             reference, and returns no value."
            (setf instruction
              (when (plusp ip)
                (aref instructions (decf ip))))
            (values)))
        
        (loop do
          (case instruction
            
            ((NIL)
              (loop-finish))
            
            (:push-0
              (push 0 stack)
              (advance))
            
            (:add-1
              (if (first stack)
                (incf (first stack) 1)
                (push 1 stack))
              (advance))
            
            (:add-10
              (if (first stack)
                (incf (first stack) 10)
                (push 10 stack))
              (advance))
            
            (:add-100
              (if (first stack)
                (incf (first stack) 100)
                (push 100 stack))
              (advance))
            
            (:negate
              (when stack
                (setf (first stack) 
                      (- (first stack))))
              (advance))
            
            (:move-up
              (when stack
                (let ((last-value (car (last stack))))
                  (declare (type integer last-value))
                  (setf stack (nbutlast stack))
                  (push last-value stack)))
              (advance))
            
            (:move-down
              (when stack
                (let ((first-value (pop stack)))
                  (declare (type integer first-value))
                  (setf stack (append stack (list first-value)))))
              (advance))
            
            (:pop
              (pop stack)
              (advance))
            
            (:swap
              (when (>= (length stack) 2)
                (rotatef (first stack)
                         (second stack)))
              (advance))
            
            (:duplicate
              (when stack
                (push (first stack) stack))
              (advance))
            
            (:jump-forward
              (cond
                ((zerop (or (first stack) 0))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case instruction
                      ((NIL)
                        (error "Unmatched forward jump '-∽'."))
                      (:jump-forward
                        (incf level)
                        (advance))
                      (:jump-back
                        (advance)
                        (if (zerop level)
                          (loop-finish)
                          (decf level)))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            (:jump-back
              (cond
                ((not (zerop (or (first stack) 0)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case instruction
                      ((NIL)
                        (error "Unmatched backward jump '-∸'."))
                      (:jump-forward
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (:jump-back
                        (incf level)
                        (recede))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            (:plus
              (push (+ (pop stack) (pop stack)) stack)
              (advance))
            
            (:minus
              (push (- (pop stack) (pop stack)) stack)
              (advance))
            
            (:multiply
              (push (* (pop stack) (pop stack)) stack)
              (advance))
            
            (:divide
              (push (round (pop stack) (pop stack)) stack)
              (advance))
            
            (:input-number
              (format T "~&Please input an integer: ")
              (let ((input (read)))
                (declare (type integer input))
                (clear-input)
                (push input stack))
              (advance))
            
            (:input-character
              (format T "~&Please input a Unicode character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (push (char-code input) stack))
              (advance))
            
            (:output-number
              (write (pop stack))
              (advance))
            
            (:output-character
              (write-char (code-char (pop stack)))
              (advance))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                instruction ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-|=,-&~| (code)
  "Interprets the piece of =,-&~ CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (parse-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of additional operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of instruction string) +TOKEN-TABLE+))

;;; -------------------------------------------------------

(defparameter +TOKEN-TABLE+ (make-hash-table :test #'eq)
  "A mapping of =,-&~ instructions to tokens.")

;;; -------------------------------------------------------

(flet ((add-instruction (instruction token)
        "Associates the the =,-&~ INSTRUCTION with the TOKEN in the
         +TOKEN-TABLE+ and returns no value."
        (declare (type instruction instruction))
        (declare (type string      token))
        (setf (gethash instruction +TOKEN-TABLE+) token)
        (values)))
  (add-instruction :push-0           "-")
  (add-instruction :add-1            "=")
  (add-instruction :add-10           "≡")
  (add-instruction :add-100          "~")
  (add-instruction :negate           "∽")
  (add-instruction :move-up          "∸")
  (add-instruction :move-down        "--")
  (add-instruction :pop              "-=")
  (add-instruction :swap             "-≡")
  (add-instruction :duplicate        "-~")
  (add-instruction :jump-forward     "-∽")
  (add-instruction :jump-back        "-∸")
  (add-instruction :plus             "=-")
  (add-instruction :minus            "==")
  (add-instruction :multiply         "=≡")
  (add-instruction :divide           "=~")
  (add-instruction :input-number     "=∽")
  (add-instruction :input-character  "=∸")
  (add-instruction :output-number    "≡-")
  (add-instruction :output-character "≡=")
  (values))

;;; -------------------------------------------------------

(defun get-instruction-token-for (instruction)
  "Returns the token associated with the =,-&~ INSTRUCTION, or signals
   an error if no correspondence could be established."
  (declare (type instruction instruction))
  (the string
    (or
      (gethash instruction +TOKEN-TABLE+)
      (error "No token associated with instruction ~s." instruction))))

;;; -------------------------------------------------------

(defun generate-code-from-instructions (instructions
                                        &key (destination T))
  "Creates from the INSTRUCTIONS the code for the corresponding =,-&~
   program, writes it to the DESTINATION, and returns either ``NIL'',
   if the DESTINATION is non-NIL, otherwise a new string containing the
   thus produced code."
  (declare (type (vector instruction *) instructions))
  (declare (type destination            destination))
  (the (or null string)
    (if destination
      (loop
        for instruction of-type instruction across instructions
        for first-run-p of-type boolean     =      T then NIL
        do
          (unless first-run-p
            (write-char #\Space destination))
          (write-string
            (get-instruction-token-for instruction)
            destination))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-code-from-instructions instructions
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-|=,-&~| "=∽ -∽ -~ ≡- -∸ ≡-")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-|=,-&~| "=∸ -~ ≡= -∽ =∸ -~ ≡= -∸")

;;; -------------------------------------------------------

;; Print: 1.
(interpret-|=,-&~|
  (generate-code-from-instructions
    (make-array 8
      :element-type 'instruction
      :initial-contents
        '(:push-0 :add-1
          :push-0 :add-10
          :push-0 :add-100
          :move-up
          :output-number))
    :destination NIL))

;;; -------------------------------------------------------

;; Print the first N Fibonacci numbers, with N being the user input.
;; 
;; Concept:
;;   :input-number     ;; N      (N)
;;   :push-0           ;; a      (a N)
;;   :duplicate        ;;        (a a N)
;;   :output-number    ;;        (a N)
;;   
;;   :push-0           ;; b      (b a N)
;;   :add-1            ;; b=1    (b a N)
;;   :duplicate        ;;        (a a N)
;;   :output-number    ;;        (a N)
;;   
;;   :duplicate        ;;        (b b a N)
;;   :move-down        ;;        (b a N b)
;;   
;;   :plus             ;; c=a+b  (c N b)
;;   :duplicate        ;;        (c c N b)
;;   :output-number    ;;        (c N b)
;;   
;;   :push-0           ;; Print space (Unicode code point = 32).
;;   :add-10
;;   :add-10
;;   :add-10
;;   :add-1
;;   :add-1
;;   :output-character
;;   
;;   :move-up          ;;        (b c N)
;;   :push-0           ;;        (0 b c N)
;;   :add-1            ;;        (1 b c N)
;;   :move-up          ;;        (N 1 b c)
;;   :minus            ;; N=N-1  (N b c)
;;   
;;   :jump-forward     ;;        (N b c)
;;   :move-down        ;;        (b c N)
;;   :swap             ;;        (c b N)
;;   :duplicate        ;;        (c c b N)
;;   :move-down        ;;        (c b N c)
;;   :plus             ;; c=c+b  (c* N c)
;;   :duplicate        ;;        (c* c* N c)
;;   :output-number    ;;        (c* N C)
;;   :move-down        ;;        (N c c*)
;;   :push-0           ;;        (0 N c c*)
;;   :add-1            ;;        (1 N c c*)
;;   :swap             ;;        (N 1 c c*)
;;   :minus            ;; N=N-1  (N c c*)
;;   :jump-backward
;; 
;; The produced and interpreted =,-&~ code constitutes
;;   =∽ - -~ ≡- - ≡ ≡ ≡ = = ≡= - = -~ ≡- - ≡ ≡ ≡ = = ≡= -~ -- =- -~ ≡- -
;;   ≡ ≡ ≡ = = ≡= ∸ - = ∸ == -∽ -- -≡ -~ -- =- -~ ≡- - ≡ ≡ ≡ = = ≡= -- -
;;   = -≡ == -∸
;; 
(interpret-|=,-&~|
  (generate-code-from-instructions
    (coerce
      '(:input-number
        :push-0
        :duplicate
        :output-number
        :push-0
        :add-10
        :add-10
        :add-10
        :add-1
        :add-1
        :output-character
        
        :push-0
        :add-1
        :duplicate
        :output-number
        :push-0
        :add-10
        :add-10
        :add-10
        :add-1
        :add-1
        :output-character
        
        :duplicate
        :move-down
        
        :plus
        :duplicate
        :output-number
        
        :push-0
        :add-10
        :add-10
        :add-10
        :add-1
        :add-1
        :output-character
        
        :move-up
        :push-0
        :add-1
        :move-up
        :minus
        
        :jump-forward
        :move-down
        :swap
        :duplicate
        :move-down
        :plus
        :duplicate
        :output-number
        :push-0
        :add-10
        :add-10
        :add-10
        :add-1
        :add-1
        :output-character
        :move-down
        :push-0
        :add-1
        :swap
        :minus
        :jump-back)
      '(vector instruction *))
    :destination NIL))
