;; Author: Kaveh Yousefi
;; Date:   2022-03-07
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Trueclone"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE."
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

(deftype signed-integer ()
  "The ``signed-integer'' type defines a 16-bit signed integer,
   compatible with a memory cell's requirement."
  '(signed-byte 16))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines an integer-indexed tape represented by a
   sparse structure in the form of a hash table, mapping to the index
   keys the 16-bit signed integer cell values."
  '(hash-table-of integer signed-integer))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized trueclone
   instructions."
  '(member
    :add
    :sub
    :set
    :left
    :right
    :in
    :out
    :jump
    :jinz
    :jits
    :jinn
    :jitn))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type signed-integer +MINIMUM-CELL-VALUE+))
(declaim (type signed-integer +MAXIMUM-CELL-VALUE+))

;;; -------------------------------------------------------

(defparameter +MINIMUM-CELL-VALUE+ -32768
  "The minimum value permissive for storage in a 16-bit signed integer
   cell.")

(defparameter +MAXIMUM-CELL-VALUE+  32767
  "The maximum value permissive for storage in a 16-bit signed integer
   cell.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular functions.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clamp-cell-value (value)
  "Ascertains the VALUE's residence inside of the valid cell value range
   [+MINIMUM-CELL-VALUE+, +MAXIMUM-CELL-VALUE+], which concords with the
   ``signed-integer'' specification."
  (declare (type integer value))
  (the signed-integer
    (max +MINIMUM-CELL-VALUE+
      (min value +MAXIMUM-CELL-VALUE+))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string instruction) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equalp)
  "Associates with the recognized trueclone instruction names the
   respective ``instruction'' objects.")

;;; -------------------------------------------------------

(flet ((add-identifier (name instruction)
        "Associates the trueclone instruction NAME with the INSTRUCTION
         object and returns no value."
        (declare (type string      name))
        (declare (type instruction instruction))
        (setf (gethash name +IDENTIFIERS+) instruction)
        (values)))
  (add-identifier "ADD"   :add)
  (add-identifier "SUB"   :sub)
  (add-identifier "SET"   :set)
  (add-identifier "LEFT"  :left)
  (add-identifier "RIGHT" :right)
  (add-identifier "IN"    :in)
  (add-identifier "OUT"   :out)
  (add-identifier "JUMP"  :jump)
  (add-identifier "JINZ"  :jinz)
  (add-identifier "JITS"  :jits)
  (add-identifier "JINN"  :jinn)
  (add-identifier "JITN"  :jitn)
  (values))

;;; -------------------------------------------------------

(defun matches-identifier-p (string)
  "Checks whether the STRING matches a trueclone instruction name,
   returning on confirmation the associated ``instruction'', otherwise
   ``NIL''."
  (declare (type string string))
  (the (or null instruction) (gethash string +IDENTIFIERS+)))

;;; -------------------------------------------------------

(defun binary-digit-p (character)
  "Checks whether the CHARACTER represents a binary digit, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean (not (null (digit-char-p character 2)))))

;;; -------------------------------------------------------

(defun interpret-trueclone (code)
  "Interprets the piece of trueclone CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          ;; The buffer is employed in progressively checking a portion
          ;; of the CODE, collected into the buffer, for equality with
          ;; the recognized trueclone keywords (instructions).
          (buffer    (make-array 0
                       :element-type 'character
                       :adjustable   T
                       :fill-pointer 0))
          (memory    (make-hash-table :test #'eql))
          (pointer   0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type string              buffer))
      (declare (type memory              memory))
      (declare (type integer             pointer))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character, if
             possible, updates the CHARACTER, and returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (move-to (new-position)
            "Moves the POSITION cursor to the NEW-POSITION, updates the
             current character, and returns no value."
            (declare (type fixnum new-position))
            (setf position new-position)
            (setf character
              (when (< position (length code))
                (char code new-position)))
            (values))
           
           
           (clear-buffer ()
            "Clears the BUFFER from any content and returns no value."
            (setf (fill-pointer buffer) 0)
            (values))
           
           (read-identifier ()
            "Starting at the current POSITION, reads an identifier and
             returns two values: (1) the consumed identifier and (2) its
             ``instruction'' representation."
            (clear-buffer)
            (the (values string instruction)
              (loop
                while (and character (alpha-char-p character))
                do
                  (format buffer "~c" character)
                  (advance)
                  (let ((instruction (matches-identifier-p buffer)))
                    (declare (type (or null instruction) instruction))
                    (when instruction
                      (return (values buffer instruction))))
                finally
                  (error "Invalid identifier ~s." buffer))))
           
           (read-parameter ()
            "Starting at the current POSITION, reads a binary number of
             one or more digits, and returns its decimal integer value."
            (unless (and character (binary-digit-p character))
              (error "Expected a binary digit, but encountered the ~
                      character '~c' at position ~d."
                character position))
            (the (integer 0 *)
              (parse-integer
                (with-output-to-string (digits)
                  (declare (type string-stream digits))
                  (loop
                    while (and character (binary-digit-p character))
                    do
                      (write-char character digits)
                      (advance)))
                :radix 2)))
           
           
           (current-cell ()
            "Returns the value of the current cell."
            (the signed-integer (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Sets the value of the current cell to NEW-VALUE and returns
             no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0)
                  (clamp-cell-value new-value))
            (values)))
        
        (loop do
          (cond
            ((null character)
              (loop-finish))
            
            ((alpha-char-p character)
              (multiple-value-bind (token instruction)
                  (read-identifier)
                (declare (type (or null string)      token))
                (declare (type (or null instruction) instruction))
                
                (case instruction
                  ;; Add to the current cell value the parameter.
                  (:add
                    (incf (current-cell) (read-parameter)))
                  
                  ;; Subtract from the current cell value the parameter.
                  (:sub
                    (decf (current-cell) (read-parameter)))
                  
                  ;; Set the value of the current cell to the parameter.
                  (:set
                    (setf (current-cell) (read-parameter)))
                  
                  ;; Move the memory pointer to the right.
                  (:left
                    (decf pointer))
                  
                  ;; Move the memory pointer to the left.
                  (:right
                    (incf pointer))
                  
                  ;; Prompt a character and store its ASCII code.
                  (:in
                    (format T "~&Please input an ASCII character: ")
                    (let ((input (read-char)))
                      (declare (type character input))
                      (clear-input)
                      (setf (current-cell) (char-code input))))
                  
                  ;; Print the current cell value as an ASCII character.
                  (:out
                    (write-char (code-char (current-cell))))
                  
                  ;; Jump unconditionally.
                  (:jump
                    (let ((target (read-parameter)))
                      (declare (type (integer 0 *) target))
                      (declare (ignorable          target))
                      (move-to target)))
                  
                  ;; Jump if current cell value is not zero.
                  (:jinz
                    (let ((target (read-parameter)))
                      (declare (type (integer 0 *) target))
                      (declare (ignorable          target))
                      (unless (zerop (current-cell))
                        (move-to target))))
                  
                  ;; Jump if current cell is zero.
                  (:jits
                    (let ((target (read-parameter)))
                      (declare (type (integer 0 *) target))
                      (declare (ignorable          target))
                      (when (zerop (current-cell))
                        (move-to target))))
                  
                  ;; Jump if current cell is negative.
                  (:jinn
                    (let ((target (read-parameter)))
                      (declare (type (integer 0 *) target))
                      (declare (ignorable          target))
                      (when (minusp (current-cell))
                        (move-to target))))
                  
                  ;; Jump if current cell is positive.
                  (:jitn
                    (let ((target (read-parameter)))
                      (declare (type (integer 0 *) target))
                      (declare (ignorable          target))
                      (when (plusp (current-cell))
                        (move-to target))))
                  
                  ;; Unrecognized instruction? => Signal error.
                  (otherwise
                    (error "Invalid instruction: ~s." token)))))
            
            ;; No other characters are homologated.
            (T
              (error "Invalid character '~c' at position ~d."
                character position)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-trueclone "INOUTJINZ0")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-trueclone "INOUTSUB110000JITS0000000001000010ADD110000OUTJUMP0000000000101011")
