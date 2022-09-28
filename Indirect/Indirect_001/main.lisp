;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Indirect", designed by the Esolang user "Quojil" in the
;; year 2014, and intended as a derivative of Urban Mueller's
;; "brainfuck", substituting the sire's octuple instruction set by three
;; commands which manipulate a numeric "instruction bay" for producing a
;; variety of 16 possible operations.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-27
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Indirect"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(deftype nybble ()
  "The ``nybble'' type defines an unsigned sequence of four bits' tally,
   occupying the integer range [0, 15], and meet for replicating the
   instruction bay's 16 operation codes."
  '(unsigned-byte 4))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   occupying the integer range [0, 255], and meet for representing a
   cell in the Indirect language's octuple data memory."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype data-memory ()
  "The ``data-memory'' type defines a one-dimensional simple array of
   eight octets, suitable for representing the Indirect language's data
   memory."
  '(simple-array octet (8)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the 16 Indirect instructions."
  '(member
    :move-cell-pointer-left
    :increment-current-cell
    :decrement-current-cell
    :reset-current-cell
    :input/output
    :open/close-file
    :skip-if-zero
    :logical-and
    :logical-nand
    :logical-or
    :logical-nor
    :logical-xor
    :logical-not
    :sleep
    :stop
    :nop))

;;; -------------------------------------------------------

(deftype io-mode ()
  "The ``io-mode'' type enumerates the modes for handling input and
   output in an Indirect program."
  '(member :standard :file))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   comprehending, for instance, ``format'' or ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array instruction (16)) +OPERATIONS+))

;;; -------------------------------------------------------

(defparameter +OPERATIONS+
  (make-array 16
    :element-type 'instruction
    :initial-contents
      '(:move-cell-pointer-left
        :increment-current-cell
        :decrement-current-cell
        :reset-current-cell
        :input/output
        :open/close-file
        :skip-if-zero
        :logical-and
        :logical-nand
        :logical-or
        :logical-nor
        :logical-xor
        :logical-not
        :sleep
        :stop
        :nop)
    :adjustable   NIL
    :fill-pointer NIL)
  "Associates each of the 16 operation codes from the instruction memory
   with the respective Indirect instruction.
   ---
   The operation codes are indirectly encoded in the zero-based indices
   of this vector, thus mapping automatically to the instructions
   residing at their positions.")

;;; -------------------------------------------------------

(defun get-instruction-for-operation-code (operation-code)
  "Returns the instruction affiliated with the OPERATION-CODE."
  (declare (type nybble operation-code))
  (the instruction
    (or (aref +OPERATIONS+ operation-code)
        (error "Unrecognized operation code: ~d." operation-code))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns a one-dimensional simple array of instructions
   from the piece of Indirect CODE."
  (declare (type string code))
  
  (let ((instructions       NIL)
        (instruction-memory 0))
    (declare (type (list-of instruction) instructions))
    (declare (type nybble                instruction-memory))
    
    (flet
        ((increment-instruction-memory ()
          "Increments the instruction memory by one, contingently
           wrapping around upon a transgression of the upper march, and
           returns no value."
          (setf instruction-memory (mod (1+ instruction-memory) 16))
          (values))
         
         (decrement-instruction-memory ()
          "Decrements the instruction memory by one, contingently
           wrapping around upon a transgression of the lower march, and
           returns no value."
          (if (zerop instruction-memory)
            (setf instruction-memory 15)
            (decf instruction-memory))
          (values))
         
         (run-operation ()
          "Inserts the Indirect instruction corresponding to the
           instruction memory's value at the front of the INSTRUCTIONS
           list and returns no value."
          (push (get-instruction-for-operation-code instruction-memory)
                instructions)
          (values)))
      
      (loop for token of-type character across code do
        (case token
          (#\+       (increment-instruction-memory))
          (#\-       (decrement-instruction-memory))
          (#\!       (run-operation))
          (otherwise NIL))))
    
    (the (simple-array instruction (*))
      (coerce (nreverse instructions)
        '(simple-array instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 2)     +IO-FILE-PATH+))
(declaim (type (or null file-stream) +IO-FILE-STREAM+))

;;; -------------------------------------------------------

(defparameter +IO-FILE-PATH+   "io"
  "The file path of the \"i/o\" file on the system.")

(defparameter +IO-FILE-STREAM+ NIL
  "The file stream created by opening the \"i/o\" file.
   ---
   Upon the file's closure this stream assumes the value ``NIL''.")

;;; -------------------------------------------------------

(defun open-io-file ()
  "Opens the i/o file and returns no value."
  (setf +IO-FILE-STREAM+
    (open +IO-FILE-PATH+
      :direction         :io
      :element-type      'character
      :if-does-not-exist :create
      :if-exists         :supersede))
  (values))

;;; -------------------------------------------------------

(defun close-io-file ()
  "Closes the i/o file, if not already closed, and returns no value."
  (when +IO-FILE-STREAM+
    (close +IO-FILE-STREAM+))
  (values))

;;; -------------------------------------------------------

(defun read-from-io-file ()
  "Reads the desinent byte from the i/o file and returns it.
   ---
   If the file is empty, this function responds with the end-of-file
   sentinel zero (0), affiliated with the \"Null\" character."
  (let ((file-size (file-length +IO-FILE-STREAM+)))
    (declare (type (integer 0 *) file-size))
    (the octet
      (cond
        ;; The file length can be determined and is positive?
        ((plusp file-size)
          (file-position +IO-FILE-STREAM+ (1- file-size))
          (char-code (read-char +IO-FILE-STREAM+ NIL 0)))
        ;; The file length cannot be determined, but the file position
        ;; can?
        ;; => Relocate the file position cursor unto the last character.
        ((file-position +IO-FILE-STREAM+)
          ;; The file is not empty?
          ;; => Relocate to the desinent position.
          (when (plusp (file-position +IO-FILE-STREAM+))
            (file-position +IO-FILE-STREAM+
              (1- (file-position +IO-FILE-STREAM+))))
          (char-code (read-char +IO-FILE-STREAM+ NIL 0)))
        ;; Neither the file length can be determined nor the file
        ;; position?
        ;; => Manually search for the desinent character in the file.
        (T
          (file-position +IO-FILE-STREAM+ 0)
          (loop
            for current-character
              of-type (or null character)
              = (read-char +IO-FILE-STREAM+ NIL #\Null)
            while
              (peek-char T +IO-FILE-STREAM+ NIL NIL)
            finally
              (return
                (char-code current-character))))))))

;;; -------------------------------------------------------

(defun append-to-io-file (character)
  "Appends the CHARACTER to the i/o file's content and returns no
   value."
  (declare (type character character))
  (write-char character +IO-FILE-STREAM+)
  (values))

;;; -------------------------------------------------------

(defun make-data-memory ()
  "Creates and returns a new ``data-memory'' object."
  (the data-memory
    (make-array 8
      :element-type    'octet
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the Indirect INSTRUCTIONS and returns no value."
  (declare (type (vector instruction *) instructions))
  
  (let ((skips-next-instruction-p NIL)
        (io-mode                  :standard)
        (data-memory              (make-data-memory))
        (pointer                  0))
    (declare (type boolean     skips-next-instruction-p))
    (declare (type io-mode     io-mode))
    (declare (type data-memory data-memory))
    (declare (type octet       pointer))
    
    (labels
        ((current-cell ()
          "Returns the current cell's value."
          (the integer
            (aref data-memory pointer)))
         
         ((setf current-cell) (new-value)
          "Sets the current cell's content to the NEW-VALUE,
           contingently wrapping the input around into the byte range
           [0, 255], and returns no value."
          (declare (type integer new-value))
          (setf (aref data-memory pointer)
                (mod new-value 256))
          (values))
         
         (next-cell ()
          "Returns the value of the cell succeeding the current one."
          (the integer
            (aref data-memory (1+ pointer))))
         
         (perform-logical-operation (operation)
          "Performs the specified logical bitwise OPERATION on the
           current cell value as the left and the next cell value as its
           right operand, stores the result into the current cell, and
           returns no value.
           ---
           The OPERATION must be compatible with Common Lisp's built-in
           function ``boole''."
          (declare (type T operation))
          (setf (current-cell)
                (boole operation (current-cell) (next-cell)))
          (values))
         
         (move-left ()
          "Moves the data memory cell pointer one step to the left,
           wrapping around the left march if necessary, and returns no
           value."
          (setf pointer
            (if (zerop pointer)
              (1- (array-total-size data-memory))
              (1- pointer)))
          (values))
         
         (toggle-io-mode ()
          "Changes the input/output mode IO-MODE to the respective
           opposite state and returns no value."
          (case io-mode
            (:standard
              (open-io-file)
              (setf io-mode :file))
            (:file
              (close-io-file)
              (setf io-mode :standard))
            (otherwise (error "Invalid I/O mode: ~s." io-mode)))
          (values))
         
         (read-input ()
          "Depending on the input/output mode IO-MODE, sets the current
           cell value either by prompting a user input character, or by
           reading the desinent byte from the i/o file, in any case
           returning no value."
          (case io-mode
            (:standard
              (format T "~&Please enter a character: ")
              (setf (current-cell) (char-code (read-char)))
              (clear-input))
            (:file
              (setf (current-cell) (read-from-io-file)))
            (otherwise
              (error "Invalid I/O mode: ~s." io-mode)))
          (values))
         
         (write-output ()
          "Depending on the input/output mode IO-MODE, either writes the
           ASCII character associated with the current cell value to the
           standard output, or appends the same to the i/o file, in any
           case returning no value."
          (case io-mode
            (:standard
              (write-char (code-char (current-cell))))
            (:file
              (append-to-io-file (code-char (current-cell))))
            (otherwise
              (error "Invalid I/O mode: ~s." io-mode)))
          (values)))
      
      (loop for instruction of-type instruction across instructions do
        (cond
          (skips-next-instruction-p
            (setf skips-next-instruction-p NIL))
          (T
            (case instruction
              (:move-cell-pointer-left
                (incf pointer))
              
              (:increment-current-cell
                (incf (current-cell)))
              
              (:decrement-current-cell
                (decf (current-cell)))
              
              (:reset-current-cell
                (setf (current-cell) 0))
              
              (:input/output
                (if (zerop (current-cell))
                  (read-input)
                  (write-output)))
              
              (:open/close-file
                (toggle-io-mode))
              
              (:skip-if-zero
                (when (zerop (current-cell))
                  (setf skips-next-instruction-p T)))
              
              (:logical-and
                (perform-logical-operation boole-and))
              
              (:logical-nand
                (perform-logical-operation boole-nand))
              
              (:logical-or
                (perform-logical-operation boole-ior))
              
              (:logical-nor
                (perform-logical-operation boole-nor))
              
              (:logical-xor
                (perform-logical-operation boole-xor))
              
              (:logical-not
                (setf (current-cell)
                      (lognot (current-cell))))
              
              (:sleep
                (sleep (current-cell)))
              
              (:stop
                (loop-finish))
              
              (:nop
                NIL)
              
              (otherwise
                (error "Invalid instruction: ~s." instruction))))))))
  
  (close-io-file)
  (values))

;;; -------------------------------------------------------

(defun interpret-Indirect (code)
  "Interprets the piece of Indirect CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code generator.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-operation-code-for-instruction (instruction)
  "Returns the operation code associated with the INSTRUCTION."
  (declare (type instruction instruction))
  (the nybble
    (or (position instruction +OPERATIONS+ :test #'eq)
        (error "Unrecognized instruction: ~s." instruction))))

;;; -------------------------------------------------------

(defun generate-Indirect-code (instructions
                               &key (destination NIL))
  "Generates an Indirect program capable of reproducing the INSTRUCTIONS
   and writes it to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise responding with a fresh
   string containing the result."
  (declare (type (vector instruction *) instructions))
  (declare (type destination            destination))
  
  (the (or null string)
    (if destination
      (let ((instruction-memory 0))
        (declare (type nybble instruction-memory))
        
        (labels
            ((run-operation ()
              "Writes to the DESTINATION the instruction bay command
               for running the current operation (\"!\") and returns no
               value."
              (format destination "!")
              (values))
             
             (increment-instruction-memory (number-of-steps)
              "Writes to the DESTINATION the NUMBER-OF-STEPS tally of
               increment (\"+\") commands and returns no value."
              (declare (type nybble number-of-steps))
              (loop repeat number-of-steps do
                (format destination "+"))
              (values))
             
             (decrement-instruction-memory (number-of-steps)
              "Writes to the DESTINATION the NUMBER-OF-STEPS tally of
               decrement (\"-\") commands and returns no value."
              (declare (type nybble number-of-steps))
              (loop repeat number-of-steps do
                (format destination "-"))
              (values))
             
             (generate-operation-code (desired-operation-code)
              "Writes to the DESTINATION the sequence of increment or
               decrement commands necessary to reach the
               DESIRED-OPERATION-CODE from the current
               INSTRUCTION-MEMORY state, appends an operation execution
               command (\"!\"), updates the INSTRUCTION-MEMORY to the
               DESIRED-OPERATION-CODE, and returns no value."
              (declare (type nybble desired-operation-code))
              (cond
                ;; Must increment instruction memory.
                ((< instruction-memory desired-operation-code)
                  (increment-instruction-memory
                    (- desired-operation-code instruction-memory)))
                ;; Must decrement instruction memory.
                ((> instruction-memory desired-operation-code)
                  (decrement-instruction-memory
                    (- instruction-memory desired-operation-code)))
                ;; Instruction memory contains correct operation code.
                (T
                  NIL))
              (run-operation)
              (setf instruction-memory desired-operation-code)
              (values)))
          
          (loop
            for previous-instruction
              of-type (or null instruction)
              =       NIL
              then    current-instruction
            for current-instruction
              of-type instruction
              across  instructions
            do
              (cond
                ;; Previous and current instruction match?
                ((and previous-instruction
                      (eq previous-instruction current-instruction))
                  (run-operation))
                ;; Previous and current instruction differ?
                (T
                  (generate-operation-code
                    (get-operation-code-for-instruction
                      current-instruction)))))))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-Indirect-code instructions
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increment the current cell by two, wait two seconds, and terminate
;; the program.
(interpret-Indirect
  "+!! increment current cell by two
   ++++++++++++! sleep for two second seconds
   +! terminate")

;;; -------------------------------------------------------

;; Print the letter "A", wait for two second, and terminate the program.
(interpret-Indirect
  "Operation 0: Increment current cell by 65
   +!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   Operation 4: Print current cell as ASCII character
   +++!
   
   Operation 2: Decrement current cell by 63
   --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   Operation 13: Sleep for two seconds
   +++++++++++!
   
   Operation 14: Terminate
   +!")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Indirect "++++!!")

;;; -------------------------------------------------------

;; Skip the user input prompt, increment the current cell by one, and
;; output the current cell value.
(interpret-Indirect "++++++!
                     --!
                     ---!
                     +++!")

;;; -------------------------------------------------------

;; Generate the Indirect code for incrementing the first cell, sleeping,
;; and then terminating the program, and write it to the standard
;; output.
(generate-Indirect-code
  (coerce
    '(:increment-current-cell
      :increment-current-cell
      :sleep
      :stop)
    '(simple-array instruction (4)))
  :destination T)

;;; -------------------------------------------------------

;; Generate an Indirect program which stores a user input in the "i/o"
;; file, copies it into the cell at the program data memory's desinence,
;; and outputs it to the standard console.
;; 
;; The result amounts to:
;;   ++++!+!-!----!++++!+!-!
(generate-Indirect-code
  (coerce
    '(;; Prompt user input and store in cell[0].
      :input/output
      ;; Switch to file input/output mode.
      :open/close-file
      ;; Write cell[0] to i/o file.
      :input/output
      ;; Move to cell[7].
      :move-cell-pointer-left
      ;; Load persisted user input from i/o file and store in cell[7].
      :input/output
      ;; Switch to standard input/output mode.
      :open/close-file
      ;; Print value of cell[7] (= cell[0] = user input) to standard
      ;; output.
      :input/output)
    '(simple-array instruction (*))))

;;; -------------------------------------------------------

;; Generate an Indirect program which stores a user input in the "i/o"
;; file, copies it into the cell at the program data memory's desinence,
;; and outputs it to the standard console, and concomitantly execute
;; the thus produced piece of source code.
(interpret-indirect
  (generate-Indirect-code
    (coerce
      '(;; Prompt user input and store in cell[0].
        :input/output
        ;; Switch to file input/output mode.
        :open/close-file
        ;; Write cell[0] to i/o file.
        :input/output
        ;; Move to cell[7].
        :move-cell-pointer-left
        ;; Load persisted user input from i/o file and store in cell[7].
        :input/output
        ;; Switch to standard input/output mode.
        :open/close-file
        ;; Print value of cell[7] (= cell[0] = user input) to standard
        ;; output.
        :input/output)
      '(simple-array instruction (*)))))
