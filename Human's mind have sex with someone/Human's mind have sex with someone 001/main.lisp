;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Human's mind have sex with someone", introduced by the
;; Esolang user "MiroslavRD" in the year 2019 as a trivial substitution
;; of Urban Mueller's "brainfuck", retaining all characteristics except
;; for the origin's single-character instruction identifiers which are
;; bartered for complete phrases comprehending their effect's
;; description.
;; 
;; 
;; Instructions
;; ============
;; Human's mind have sex with someone's instruction set is limned as a
;; verbatim appropriation from its octuple brainfuck cleronomy,
;; agnominated, however, in a dioristic fashion.
;; 
;; == OVERVIEW ==
;; As a warkloom to a basic comprehension of the operational facilities,
;; the following apercu shall impart the requisites:
;; 
;;   ------------------------------------------------------------------
;;   Command                        | Effect
;;   -------------------------------+----------------------------------
;;   Move the pointer to the right  | Moves the cell pointer one step
;;                                  | to the right.
;;   ..................................................................
;;   Move the pointer to the left   | Moves the cell pointer one step
;;                                  | to the left.
;;   ..................................................................
;;   Increment the memory cell      | Increments the current cell value
;;   under the pointer              | by one.
;;   ..................................................................
;;   Decrement the memory cell      | Decrements the current cell value
;;   under the pointer              | by one.
;;   ..................................................................
;;   Output the character signified | Prints the character associated
;;   by the cell at the pointer     | with the current cell value
;;                                  | construed as an ASCII code.
;;   ..................................................................
;;   Input the character signified  | Queries the user for an input
;;   by the cell at the pointer     | ASCII character and stores its
;;                                  | character code in the current
;;                                  | cell.
;;   ..................................................................
;;   Jump past the matching right   | If the current cell value equals
;;   bracket if the cell under the  | zero, moves the instruction
;;   pointer is 0                   | pointer forward to position
;;                                  | immediately following the
;;                                  | matching back jump command.
;;                                  | Otherwise, proceeds as usual.
;;   ..................................................................
;;   Jump back to the matching left | If the current cell value does
;;   bracket if the cell under the  | not equal zero, moves the
;;   pointer is not 0               | instruction pointer back to the
;;                                  | position immediately following
;;                                  | the matching forward jump
;;                                  | command.
;;                                  | Otherwise, proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-26
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Human%27s_mind_have_sex_with_someone"
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
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, of zero or more entries, each element of which either resolves
   to the ``NIL'' value or to a cons whose left department, or key,
   conforms to the KEY-TYPE, while the associated value, commorant in
   the dextral moeity, conforms to the VALUE-TYPE, both defaulting to
   the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for element of-type T in (the list candidate)
              always
                (or (null element)
                    (typep element `(cons ,key-type ,value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump positions
   in an instruction vector to the matching back jump locations, and
   vice versa, implemented as a hash table which maps fixnum keys to
   values of the same type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Human's mind have sex
   with someone instruction variants."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type represents a Human's mind have sex with someone
   program as a vector of zero or more commands."
  '(vector command *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a mapping of cell
   indices to cell values, both assuming signed integer objects as keys
   or values in a hash table."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of identifiers.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of string command) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '(("Move the pointer to the right"               . :move-right)
    ("Move the pointer to the left"                . :move-left)
    ("Increment the memory cell under the pointer" . :increment)
    ("Decrement the memory cell under the pointer" . :decrement)
    ("Output the character signified by the cell at the pointer" . :output)
    ("Input the character signified by the cell at the pointer"  . :input)
    ("Jump past the matching right bracket if the cell under the pointer is 0"   . :jump-forward)
    ("Jump back to the matching left bracket if the cell under the pointer is not 0" . :jump-back))
  "An association list which maps the operation identifier names to the
   respective keywords, and vice versa.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for-token (token)
  "Returns the ``command'' associated with the identifier TOKEN, or
   ``NIL'' in the case of a disrespondency."
  (declare (type string token))
  (the (or null command)
    (cdr (assoc token +IDENTIFIERS+ :test #'string=))))

;;; -------------------------------------------------------

(defun get-token-for-command (command)
  "Returns the identifier token associated with the COMMAND, or ``NIL''
   in the case of a disrespondency."
  (declare (type command command))
  (the (or null string)
    (car (rassoc command +IDENTIFIERS+ :test #'eq))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun probe-token (source token start)
  "Checks whether, anchored at the START position in the SOURCE, the
   TOKEN applies, returning on confirmation the position immediately
   following the matching portion's end, otherwise ``NIL''."
  (declare (type string source))
  (declare (type string token))
  (declare (type fixnum start))
  (let ((end (min (+ start (length token))
                  (length source))))
    (declare (type fixnum end))
    (the (or null fixnum)
      (when (string= source token :start1 start :end1 end)
        end))))

;;; -------------------------------------------------------

(defun probe-command (source command start)
  "Checks whether, anchored at the START position in the SOURCE, the
   COMMAND's string representation applies, returning on confirmation
   the position immediately following the matching portion's end,
   otherwise ``NIL''."
  (declare (type string  source))
  (declare (type command command))
  (declare (type fixnum  start))
  (the (or null fixnum)
    (probe-token source
      (or (get-token-for-command command)
          (error "Invalid command: ~s." command))
      start)))

;;; -------------------------------------------------------

(defun search-command (source start)
  "Beginning at the START position in the SOURCE, searches for the next
   command, on success returning two values: (1) the detected command
   and (2) the position in the SOURCE immediately following the matching
   portion; otherwise, simply responds with ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((command NIL)
        (end     NIL))
    (declare (type (or null command) command))
    (declare (type (or null fixnum)  end))
    (flet ((test-command (candidate)
            "Checks whether the CANDIDATE command occurs at the START
             position, updates the END and COMMAND, and returns the
             END."
            (declare (type command candidate))
            (setf end (probe-command source candidate start))
            (when end
              (setf command candidate))
            (the (or null fixnum) end)))
      (case (char source start)
        (#\D
          (test-command :decrement))
        (#\I
          (or (test-command :increment)
              (test-command :input)))
        (#\J
          (or (test-command :jump-forward)
              (test-command :jump-back)))
        (#\M
          (or (test-command :move-right)
              (test-command :move-left)))
        (#\O
          (test-command :output))
        (otherwise
          NIL)))
    (the (values command fixnum)
      (if (and command end)
        (values command end)
        (error "No command found starting at the position ~d."
          start)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline #\Return #\Linefeed)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position in the SOURCE, skips a sequence of
   zero or more adjacent whitespaces, returning the position of the
   first non-whitespace in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     end of-type fixnum from start by 1
      while   (and (array-in-bounds-p source end)
                   (whitespace-character-p (char source end)))
      finally (return end))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extract and returns from the piece of Human's mind have sex with
   someone CODE's the comprehended instructions and returns these in a
   one-dimensional simple array of command objects."
  (declare (type string code))
  (let ((instructions NIL)
        (start        0))
    (declare (type (list-of command) instructions))
    (declare (type fixnum            start))
    (flet ((eof-p ()
            "Checks whether the START position is inside of the CODE's
             boundaries, returning on confirmation a ``boolean'' value
             of ``T'', otherwise ``NIL''."
            (the boolean
              (not (array-in-bounds-p code start)))))
      (loop
        do    (setf start (skip-whitespaces code start))
        until (eof-p)
        do
          (unless (eof-p)
            (multiple-value-bind (command end)
                (search-command code start)
              (declare (type command command))
              (declare (type fixnum  end))
              (push command instructions)
              (setf start end)))))
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Computes the jump table for the INSTRUCTIONS, associating the forward
   jump locations into the same with the respective back jump positions,
   and vice versa."
  (declare (type program instructions))
  (let ((jump-table        (make-hash-table :test #'eql))
        (forward-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-positions))
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0 by 1
      do
        (case instruction
          (:jump-forward
            (push position forward-positions))
          (:jump-back
            (if forward-positions
              (let ((forward-position (pop forward-positions))
                    (back-position    position))
                (declare (type fixnum forward-position))
                (declare (type fixnum back-position))
                (setf (gethash forward-position jump-table)
                      back-position)
                (setf (gethash back-position    jump-table)
                      forward-position))
              (error "Unterminated back jump at position ~d."
                position)))
          (otherwise
            NIL)))
    (when forward-positions
      (error "Unterminated forward jumps at positions ~{~d~^, ~}."
        forward-positions))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Interprets the piece of Human's mind have sex with someone
   INSTRUCTIONS and returns no value."
  (declare (type program instructions))
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (jump-table          (build-jump-table instructions))
          (memory              (make-hash-table :test #'eql))
          (pointer             0))
      (declare (type fixnum            ip))
      (declare (type (or null command) current-instruction))
      (declare (type jump-table        jump-table))
      (declare (type memory            memory))
      (declare (type integer           pointer))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the INSTRUCTIONS, if possible, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to-opposite-boundary ()
            "Expecting to be located at a jump instruction, relocates
             the instruction pointer IP to the opposite boundary,
             updates the CURRENT-INSTRUCTION, and returns no value."
            (setf ip (gethash ip jump-table))
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (current-cell ()
            "Returns the current cell value."
            (the integer
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the current cell and returns no
             value."
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (loop while current-instruction do
          (case current-instruction
            ((NIL)
              (loop-finish))
            
            (:move-right
              (incf pointer))
            
            (:move-left
              (decf pointer))
            
            (:increment
              (incf (current-cell)))
            
            (:decrement
              (decf (current-cell)))
            
            (:output
              (write-char
                (code-char
                  (current-cell))))
            
            (:input
              (format T "~&Please input an ASCII character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input))
            
            (:jump-forward
              (when (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (:jump-back
              (unless (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                current-instruction ip)))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-|Human's mind have sex with someone| (code)
  "Interprets the piece of Human's mind have sex with someone CODE and
   returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World!".
(interpret-|Human's mind have sex with someone|
  "Increment the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerJump past the matching right bracket if the cell under the pointer is 0Move the pointer to the rightIncrement the memory cell under the pointerMove the pointer to the rightIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerMove the pointer to the rightIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerMove the pointer to the rightIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerMove the pointer to the leftMove the pointer to the leftMove the pointer to the leftMove the pointer to the leftDecrement the memory cell under the pointerJump back to the matching left bracket if the cell under the pointer is not 0Move the pointer to the rightMove the pointer to the rightMove the pointer to the rightIncrement the memory cell under the pointerIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerMove the pointer to the rightIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerOutput the character signified by the cell at the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerMove the pointer to the leftMove the pointer to the leftIncrement the memory cell under the pointerIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerMove the pointer to the rightIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerMove the pointer to the rightOutput the character signified by the cell at the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerOutput the character signified by the cell at the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerDecrement the memory cell under the pointerOutput the character signified by the cell at the pointerMove the pointer to the leftMove the pointer to the leftIncrement the memory cell under the pointerOutput the character signified by the cell at the pointerMove the pointer to the leftOutput the character signified by the cell at the pointer")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on the input of the
;; null character.
(interpret-|Human's mind have sex with someone|
  "Input the character signified by the cell at the pointer
   Output the character signified by the cell at the pointer
   Jump past the matching right bracket if the cell under the pointer is 0
   Input the character signified by the cell at the pointer
   Output the character signified by the cell at the pointer
   Jump back to the matching left bracket if the cell under the pointer is not 0")
