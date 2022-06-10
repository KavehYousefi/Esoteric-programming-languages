;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Instructions
;; ============
;;   
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   !       | memory[N] <- memory[N] + 1, with N = number of
;;           | exclamation marks in an undisturbed succession.
;;           | Examples:
;;           |   !   --- Increment memory[1].
;;           |   !!! --- Increment memory[3].
;;   ..................................................................
;;   !?!     | Start or terminate a loop based on memory[1].
;;   ..................................................................
;;   ?       | Query a byte from the user and store at memory[1].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-07
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Repetition_Legitimizes"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer in the range
   [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype nonnegative-integer ()
  "The ``nonnegative-integer'' type defines an integer in the range
   [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned, byte-sized integer occupying
   the range [0, 255]."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized command names."
  '(member
    :increment
    :input
    :loop))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional argument)))
  "The ``Instruction'' class models a statement in a Repetitions
   Legitimizes program, optionally accompanied by an argument."
  (type     (error "Missing instruction type.")
            :type command)
  (argument NIL
            :type (or null positive-integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents the program storage, arranged in the
   similitude of a tape, and composed a contingently infinite tally of
   byte-valued cells.
   ---
   While the referencing of cells conceptually starts with the index one
   (1), the underlying vector, of course, constitutes a zero-based
   entity."
  (cells (make-array 1 :element-type 'octet :initial-element 0)
         :type (vector octet *)))

;;; -------------------------------------------------------

(defun wrap-byte (number)
  "Wraps the NUMBER in the byte range [0, 255] and returns a
   contingently adjusted ``octet'' variant of the input NUMBER."
  (declare (type nonnegative-integer number))
  (the octet (mod number 256)))

;;; -------------------------------------------------------

(defun memory-increment (memory cell-index)
  "Increments the MEMORY cell at the one-based INDEX and returns the
   modified MEMORY."
  (declare (type Memory            memory))
  (declare (type positive-integer cell-index))
  (let ((zero-based-index (1- cell-index)))
    (declare (type nonnegative-integer zero-based-index))
    ;; Expand the MEMORY's internal array if too small for the
    ;; CELL-INDEX.
    (when (>= zero-based-index (length (memory-cells memory)))
      (setf (memory-cells memory)
        (adjust-array
          (memory-cells memory)
          cell-index
          :element-type    'octet
          :initial-element 0)))
    (setf (aref (memory-cells memory) zero-based-index)
          (wrap-byte
            (1+ (aref (memory-cells memory) zero-based-index)))))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-get-first-cell (memory)
  "Returns the value of the first MEMORY cell."
  (declare (type Memory memory))
  (the octet (aref (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun memory-set-first-cell (memory new-value)
  "Sets the value of the first MEMORY cell to the NEW-VALUE,
   contingently preceding the modification with an adjustment into the
   byte range, and returns the modified MEMORY."
  (declare (type Memory memory))
  (declare (type octet  new-value))
  (setf (aref (memory-cells memory) 0)
        (wrap-byte new-value))
  (the Memory memory))

;;; -------------------------------------------------------

(defun print-memory-as-text (memory &key (destination T))
  "Prints the ASCII characters corresponding to the MEMORY's cell values
   to the DESTINATION, returning for a non-``NIL'' DESTINATION the
   ``NIL'' value, otherwise a fresh string containing the result."
  (declare (type Memory      memory))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for cell-value of-type octet across (memory-cells memory) do
        (format T "~c" (code-char cell-value)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (print-memory-as-text memory :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of Repetition Legitimizes CODE a vector of
   instructions."
  (declare (type string code))
  
  (let ((instructions NIL)
        (position     0)
        (character    (when (plusp (length code))
                        (char code 0))))
    (declare (type (list-of Instruction) instructions))
    (declare (type fixnum                position))
    (declare (type (or null character)   character))
    
    (labels
        ((advance ()
          "Moves the POSITION cursor to the next character in the CODE,
           if possible, updates the current CHARACTER, and returns no
           value."
          (setf character
            (when (array-in-bounds-p code (1+ position))
              (char code (incf position))))
          (values))
         
         (count-exclamation-marks ()
          "Starting at the current POSIITION in the CODE, counts and
           returns the number of successive exclamation marks."
          (the nonnegative-integer
            (loop
              while (and character (char= character #\!))
              do    (advance)
              count 1)))
         
         (exclamation-mark-p (index)
          "Checks whether the character at the INDEX in the CODE
           represents an exclamation mark, returning on confirmation a
           ``boolean'' value of ``T'', otherwise ``NIL''."
          (declare (type fixnum index))
          (the boolean
            (not (null
              (and (array-in-bounds-p code index)
                   (char= (char code index) #\!))))))
         
         (surrounded-by-exclamation-marks-p ()
          "Checks whether both characters immediately abutting the
           POSITION sinistrally and dextrally in the CODE represent
           exclamation marks, returning on confirmation a ``boolean''
           value of ``T'', otherwise ``NIL''."
          (the boolean
            (not (null
              (and (exclamation-mark-p (1- position))
                   (exclamation-mark-p (1+ position))))))))
      
      (loop do
        (case character
          ((NIL)
            (loop-finish))
          
          ((#\Space #\Tab #\Newline)
            (advance))
          
          (#\!
            (let ((cell-index (count-exclamation-marks)))
              (declare (type positive-integer cell-index))
              (push (make-instruction :increment cell-index)
                    instructions)))
          
          (#\?
            (cond
              ((surrounded-by-exclamation-marks-p)
                (push (make-instruction :loop) instructions))
              (T
                (push (make-instruction :input) instructions)))
            (advance))
          
          (otherwise
            (error "Invalid character '~c' at position ~d."
              character position)))))
    
    (the (simple-array Instruction (*))
      (coerce (nreverse instructions)
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Evaluates the INSTRUCTIONS and returns no value."
  (declare (type (simple-array Instruction (*)) instructions))
  (let ((memory (make-memory)))
    (declare (type Memory memory))
    
    (when (plusp (length instructions))
      (let ((ip          0)
            (instruction (aref instructions 0))
            (loop-start  NIL))
        (declare (type fixnum                ip))
        (declare (type (or null Instruction) instruction))
        (declare (type (or null fixnum)      loop-start))
        
        (labels
            ((advance-ip ()
              "Moves the instruction pointer IP to the next position in
               the INSTRUCTIONS vector, if possible, updates the current
               INSTRUCTION, and returns no value."
              (setf instruction
                (when (array-in-bounds-p instructions (1+ ip))
                  (aref instructions (incf ip))))
              (values))
             
             (move-ip-to (new-position)
              "Moves the instruction pointer IP to the NEW-POSITION,
               updates the current INSTRUCTION, and returns no value."
              (declare (type fixnum new-position))
              (setf ip new-position)
              (setf instruction
                (when (array-in-bounds-p instructions ip)
                  (aref instructions ip)))
              (values))
             
             (move-to-next-loop ()
              "Moves the instruction pointer IP to the next ``:loop''
               instruction in the INSTRUCTIONS vector, starting at the
               current IP position, and returns no value."
              (loop do
                (if instruction
                  (case (instruction-type instruction)
                    (:loop
                      (loop-finish))
                    (otherwise
                      (advance-ip)))
                  (error "Unmatched loop.")))
              (values)))
          
          (loop while instruction do
            (case (instruction-type instruction)
              (:increment
                (let ((cell-index (instruction-argument instruction)))
                  (declare (type positive-integer cell-index))
                  (memory-increment memory cell-index))
                (advance-ip))
              
              (:input
                (format T "~&Please input a byte: ")
                (let ((input (parse-integer (read-line))))
                  (declare (type octet input))
                  (memory-set-first-cell memory input))
                (advance-ip))
              
              (:loop
                (cond
                  ;; Outside of a loop and first cell = 0?
                  ;; => Skip loop.
                  ((and (not loop-start)
                        (zerop (memory-get-first-cell memory)))
                    (advance-ip)
                    (move-to-next-loop)
                    (advance-ip))
                  
                  ;; Outside of a loop and first cell != 0?
                  ;; => Start loop.
                  ((and (not loop-start)
                        (not (zerop (memory-get-first-cell memory))))
                    (setf loop-start ip)
                    (advance-ip))
                  
                  ;; Inside of a loop and first cell = 0?
                  ;; => Repeat loop.
                  ((and loop-start
                        (zerop (memory-get-first-cell memory)))
                    (move-ip-to loop-start)
                    (advance-ip))
                  
                  ;; Inside of a loop and first cell != 0?
                  ;; => Exit loop.
                  ((and loop-start
                        (not (zerop (memory-get-first-cell memory))))
                    (setf loop-start NIL)
                    (advance-ip))
                  
                  (T
                    (error "Unexpected combination of loop state ~s ~
                            and first cell value ~d."
                      loop-start (memory-get-first-cell memory)))))
              
              (otherwise
                (error "Invalid instruction ~s at position ~d."
                  instruction ip)))))))
    
    (print-memory-as-text memory))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Repetition-Legitimizes (code)
  "Interprets the piece of Repetition Legitimizes CODE and returns no
   value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code generator.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-code-for-text (text &key (destination NIL))
  "Generates a Repetition Legitimizes program capable of reproducing the
   ASCII codes of the TEXT in its memory and writes it to the
   DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise a fresh string containing the result."
  (declare (type string      text))
  (declare (type destination destination))
  
  (the (or null string)
    (if destination
      (labels
          ((get-cell-address (cell-index)
            "Returns a string representing the CELL-INDEX by containing
             the equinumerant tally of exclamation marks."
            (declare (type positive-integer cell-index))
            (the string
              (with-output-to-string (exclamations)
                (declare (type string-stream exclamations))
                (loop repeat cell-index do
                  (write-char #\! exclamations)))))
           
           (set-cell-to-value (cell-index cell-value)
            "Writes to the DESTINATION the Repetition Legitimizes code
             requisite to set the cell at the CELL-INDEX to the
             CELL-VALUE and returns no value."
            (declare (type positive-integer cell-index))
            (declare (type octet            cell-value))
            (let ((cell-address (get-cell-address cell-index)))
              (declare (type string cell-address))
              (loop repeat cell-value do
                (format destination "~a " cell-address)))
            (values))
           
           (reproduce-character (cell-index character)
            "Writes to the DESTINATION the Repetition Legitimizes code
             requisite to set the cell at the CELL-INDEX to the ASCII
             code of the CHARACTER and returns no value."
            (declare (type positive-integer cell-index))
            (declare (type character        character))
            (set-cell-to-value cell-index (char-code character))
            (values)))
        
        (loop
          for cell-index     of-type fixnum    from   1
          for text-character of-type character across text
          do  (reproduce-character cell-index text-character)))
      
      (with-output-to-string (content)
        (declare (type string-stream content))
        (generate-code-for-text text :destination content)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Repetition-Legitimizes "?")

;;; -------------------------------------------------------

;; Count the number of times this loop has been executed.
;; During each iteration, the user must enter an input of zero (0) in
;; order to repeat the loop, otherwise its execution will cease.
;; Following its patration, the second cell will hold the ASCII
;; character whose code is tantamount to the repetition tally, including
;; the incipient run.
(interpret-Repetition-Legitimizes "! !!!?!!! !! ? !!!?!!!")

;;; -------------------------------------------------------

;; Create and return a string representing the Repetition Legitimizes
;; program which reproduces in its memory the ASCII codes of the text
;; "Hello, World!".
(generate-code-for-text "Hello, World!")

;;; -------------------------------------------------------

;; Create a string representing the Repetition Legitimizes program which
;; reproduces in its memory the ASCII codes of the text "Hello, World!",
;; and interpret the thus generated program.
(interpret-Repetition-Legitimizes
  (generate-code-for-text "Hello, World!"))
