;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "NullScript 2", invented by the Esolang user "a stone
;; arachnid".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-23
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/NullScript_2"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
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
   entries, associating with each key of the KEY-TYPE a value of the
   VALUE-TYPE, both defaulting to the comprehensive ``T''."
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
  "The ``jump-table'' type defines a hash table mapping in the form of
   fixnum keys and values jump source and destination positions,
   represented as indices into an instruction vector.
   ---
   While not strictly discriminating betwixt a jump start point (\"{\")
   and a terminator (\"}\"), the keys themselves provide an identifying
   characteristic, as these are unique indices into an instruction
   sequence.
   ---
   A jump start index associates with the instruction index immediately
   following the terminating jump bracket, or the size of the
   instruction vector if no such pairing exists. A jump termination
   index associates with the index of the matching jump start, or with
   the start of the instructions, zero (0) if no correspondence existed
   in the first place. Each entry thus resolves to one of these:
     jumpStartIndex => jumpEndIndex + 1 or instructionVectorLength
     jumpEndIndex   => jumpStartIndex   or 0"
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized NullScript 2
   instruction types."
  '(member
    ;; "tcell group" commands:
    :decrement-cell
    :increment-cell
    :square-cell
    :print-cell-number
    :print-cell-character
    :reset-cell
    
    ;; "stdin group" commands:
    :input-cell-number
    
    ;; "meta group" commands:
    :move-left
    :move-right
    :copy-cell-to-queue
    :copy-queue-to-cell
    
    ;; "math group" commands:
    :add-queue
    :subtract-queue
    :multiply-queue
    :divide-queue
    :remainder-queue
    
    ;; "jump group" commands:
    :jump-forward
    :jump-back
    :jump-to-end))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, for instance, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 25) +DEFAULT-PROMPT-MESSAGE+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-PROMPT-MESSAGE+
  "Please input an integer: "
  "The default text to display when querying the user for an input.")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts and returns from the piece of NullScript 2 CODE the
   instructions."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    
    (when (plusp (length code))
      (loop
        for character of-type character across code
        and position  of-type fixnum    from   0
        do
          (case character
            (#\[
              (push :decrement-cell instructions))
            
            (#\]
              (push :increment-cell instructions))
            
            (#\;
              (push :square-cell instructions))
            
            (#\.
              (push :print-cell-number instructions))
            
            (#\,
              (push :print-cell-character instructions))
            
            (#\~
              (push :reset-cell instructions))
            
            
            (#\&
              (push :input-cell-number instructions))
            
            
            (#\<
              (push :move-left instructions))
            
            (#\>
              (push :move-right instructions))
            
            (#\*
              (push :copy-cell-to-queue instructions))
            
            (#\'
              (push :copy-queue-to-cell instructions))
            
            
            (#\+
              (push :add-queue instructions))
            
            (#\-
              (push :subtract-queue instructions))
            
            (#\x
              (push :multiply-queue instructions))
            
            (#\/
              (push :divide-queue instructions))
            
            (#\"
              (push :remainder-queue instructions))
            
            
            (#\{
              (push :jump-forward instructions))
            
            (#\}
              (push :jump-back instructions))
            
            (#\q
              (push :jump-to-end instructions))
            
            
            ((#\Space #\Tab #\Newline)
              NIL)
            
            (otherwise
              (error "Invalid character \"~c\" at position ~d."
                character position)))))
    
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Creates and returns a jump table which associates jump start and end
   points, represented by their indices into the INSTRUCTIONS.
   ---
   Each entry of the thus generated mappings consists of a fixnum key
   and a fixnum value, without explicity distinguishment betwixt jump
   start and end types.
   ---
   A jump start associates its position in the INSTRUCTIONS either with
   the index immediately following the matching terminating bracket or
   the first position outside of the INSTRUCTIONS vector if no pairing
   exists for it. A jump end associates its position in the INSTRUCTIONS
   either with the position of the matching starting bracket or the
   index of the first INSTRUCTIONS, that is, zero (0), if no pairing
   exists for it."
  (declare (type (vector command *) instructions))
  
  (let ((jump-table  (make-hash-table :test #'eql))
        (jump-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-starts))
    
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0
      do
        (case instruction
          (:jump-forward
            (push position jump-starts))
          
          (:jump-back
            (cond
              (jump-starts
                (let ((start-position (pop jump-starts)))
                  (declare (type fixnum start-position))
                  
                  ;; Connect the jump start to the position following
                  ;; the matching jump end.
                  (setf (gethash start-position jump-table)
                        (1+ position))
                  
                  ;; Connect the jump end to the jump start position.
                  (setf (gethash position jump-table)
                        start-position)))
              ;; No matching "{"?
              ;; => Jump to program start.
              (T
                (setf (gethash position jump-table) 0))))
          
          (otherwise
            NIL)))
    
    ;; Unmatched jump starts "{"?
    ;; => Connect these to the end of the INSTRUCTIONS vector.
    (when jump-starts
      (let ((instruction-end-position (length instructions)))
        (declare (type fixnum instruction-end-position))
        (dolist (unmatched-start-position jump-starts)
          (declare (type fixnum unmatched-start-position))
          (setf (gethash unmatched-start-position jump-table)
                instruction-end-position))))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-tape ()))
  "The ``Tape'' class provides a memory composed of 256 integer-valued
   cells in linear arrangement, amenable to a non-negative integer
   index, and operated upon by a cell pointer, or simply \"pointer\",
   responsible for the selection of exactly one cell as active entity at
   one instant.
   ---
   No inherent march's imposition governs a cell's value; however, some
   operations may induce or require the conformance to a particular
   constraint, especially the ASCII character code range of [0, 255] for
   input and output facilities.
   ---
   The cell pointer may be translated sinistrally or dextrally by one
   step at a time, capacitated to wrap around to the athwart laterality
   if ordered in its motion to transgress any of the two tape
   boundaries."
  (cells
    (make-array 256
      :element-type    'integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)
    :type (vector integer 256))
  (pointer
    0
    :type (integer 0 255)))

;;; -------------------------------------------------------

(defun tape-current-cell (tape)
  "Returns the value of the TAPE's active cell."
  (declare (type Tape tape))
  (the integer (aref (tape-cells tape) (tape-pointer tape))))

;;; -------------------------------------------------------

(defun (setf tape-current-cell) (new-value tape)
  "Sets the value of the TAPE's active cell to the NEW-VALUE and returns
   the modified TAPE."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf (aref (tape-cells tape) (tape-pointer tape))
        new-value)
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-left (tape)
  "Moves the TAPE's pointer one cell to the left, contingently wrapping
   around to the dextral side if necessary, and returns the modified
   TAPE."
  (declare (type Tape tape))
  (setf (tape-pointer tape)
        (mod (1- (tape-pointer tape)) 255))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-right (tape)
  "Moves the TAPE's pointer one cell to the right, contingently wrapping
   around to the sinistral side if necessary, and returns the modified
   TAPE."
  (declare (type Tape tape))
  (setf (tape-pointer tape)
        (mod (1+ (tape-pointer tape)) 255))
  (the Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Queue".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Queue
  (:constructor make-queue ()))
  "The ``Queue'' class implements a queue with a fixed capacity,
   amenable to the reception of any type of element.
   ---
   Adjusted to the expected use as a parameter queue, the maximum size
   of eight elements permits a rather prodigious ilk of realization as a
   simple list. As a consectary thereof, insertions incur a preventable
   performance of O(n), deliberately bartered for simplicity in the
   implementation."
  (elements NIL :type (list-of integer))
  (capacity 8   :type (integer 0 *)))

;;; -------------------------------------------------------

(defun queue-empty-p (queue)
  "Checks whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean (null (queue-elements queue))))

;;; -------------------------------------------------------

(defun queue-full-p (queue)
  "Checks whether the QUEUE is full, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (not (null
      (>= (length (queue-elements queue))
      (queue-capacity queue))))))

;;; -------------------------------------------------------

(defun queue-enqueue (queue new-element)
  "Appends the NEW-ELEMENT to the rear of the QUEUE and returns the
   modified QUEUE.
   ---
   If the QUEUE has already reached its capacity, the element at its
   front is removed prior to the insertion."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (when (queue-full-p queue)
    (pop (queue-elements queue)))
  (setf (queue-elements queue)
        (append (queue-elements queue)
                (list new-element)))
  (the Queue queue))

;;; -------------------------------------------------------

(defun queue-dequeue (queue)
  "Removes and returns the element at the QUEUE's front."
  (declare (type Queue queue))
  (the (or null integer)
    (unless (queue-empty-p queue)
      (pop (queue-elements queue)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions
                             &key (prompt-message
                                    +DEFAULT-PROMPT-MESSAGE+))
  "Processes the NullScript 2 INSTRUCTIONS and returns no value.
   ---
   A custom prompt message, in the form of any object, which will be
   displayed in its textual form, may be specified in order to notify
   the user about the requirement of an input. If set to ``NIL'', no
   notification will be displayed."
  (declare (type (vector command *) instructions))
  (declare (type T                  prompt-message))
  
  (when (plusp (length instructions))
    (let ((tape            (make-tape))
          (parameter-queue (make-queue))
          (ip              0)
          (instruction     (aref instructions 0))
          (jump-table      (build-jump-table instructions)))
      (declare (type Tape              tape))
      (declare (type Queue             parameter-queue))
      (declare (type fixnum            ip))
      (declare (type (or null command) instruction))
      (declare (type jump-table        jump-table))
      
      (labels
          ((advance-ip ()
            "Moves the instruction pointer IP to the instruction in the
             INSTRUCTIONS, if possible, updates the current INSTRUCTION,
             and returns no value."
            (setf instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (move-ip-to (new-position)
            "Relocates the instruction pointer IP to the NEW-POSITION
             in the INSTRUCTIONS, updates the current INSTRUCTION, and
             returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (jump-to-opposite-label ()
            "Relocates the instruction pointer IP to the position
             associated with the current jump boundary's opposite march,
             updates the current INSTRUCTION, and returns no value."
            (let ((destination (gethash ip jump-table)))
              (declare (type fixnum destination))
              (move-ip-to destination))
            (values))
           
           (jump-to-end-of-program ()
            "Moves the instruction pointer IP beyond the end of the
             INSTRUCTIONS, sets the current INSTRUCTION to ``NIL'', and
             returns no value."
            (move-ip-to (length instructions))
            (values))
           
           (read-input ()
            "Queries the user for an integer number and returns it,
             contingently preceded by the display of a prompt message."
            (when prompt-message
              (format T "~&~a" prompt-message))
            (the integer
              (prog1
                (parse-integer (read-line))
                (clear-input))))
           
           (dequeue-or-input ()
            "Either removes and returns the parameter queue's front
             element, or, if the queue is empty, queries the user for an
             input and responds with the same."
            (the integer
              (if (queue-empty-p parameter-queue)
                (read-input)
                (queue-dequeue parameter-queue)))))
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:decrement-cell
              (decf (tape-current-cell tape))
              (advance-ip))
            
            (:increment-cell
              (incf (tape-current-cell tape))
              (advance-ip))
            
            (:square-cell
              (setf (tape-current-cell tape)
                    (* (tape-current-cell tape)
                       (tape-current-cell tape)))
              (advance-ip))
            
            (:print-cell-number
              (format T "~d" (tape-current-cell tape))
              (advance-ip))
            
            (:print-cell-character
              (format T "~c" (code-char (tape-current-cell tape)))
              (advance-ip))
            
            (:reset-cell
              (setf (tape-current-cell tape) 0)
              (advance-ip))
            
            
            (:input-cell-number
              (setf (tape-current-cell tape)
                    (read-input))
              (advance-ip))
            
            
            (:move-left
              (tape-move-left tape)
              (advance-ip))
            
            (:move-right
              (tape-move-right tape)
              (advance-ip))
            
            (:copy-cell-to-queue
              (queue-enqueue parameter-queue (tape-current-cell tape))
              (advance-ip))
            
            (:copy-queue-to-cell
              (setf (tape-current-cell tape)
                    (dequeue-or-input))
              (advance-ip))
            
            
            (:add-queue
              (setf (tape-current-cell tape)
                (+ (dequeue-or-input)
                   (dequeue-or-input)))
              (advance-ip))
            
            (:subtract-queue
              (setf (tape-current-cell tape)
                (+ (dequeue-or-input)
                   (dequeue-or-input)))
              (advance-ip))
            
            (:multiply-queue
              (setf (tape-current-cell tape)
                (* (dequeue-or-input)
                   (dequeue-or-input)))
              (advance-ip))
            
            (:divide-queue
              (setf (tape-current-cell tape)
                (round (dequeue-or-input)
                       (dequeue-or-input)))
              (advance-ip))
            
            (:remainder-queue
              (setf (tape-current-cell tape)
                (mod (dequeue-or-input)
                     (dequeue-or-input)))
              (advance-ip))
            
            
            (:jump-forward
              (cond
                ((zerop (tape-current-cell tape))
                  (jump-to-opposite-label))
                (T
                  (advance-ip))))
            
            (:jump-back
              (jump-to-opposite-label))
            
            (:jump-to-end
              (jump-to-end-of-program))
            
            
            (otherwise
              (error "Invalid instruction ~s at instruction pointer ~
                      position ~d."
                instruction ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-NullScript-2 (code
                               &key (prompt-message
                                      +DEFAULT-PROMPT-MESSAGE+))
  "Interprets the piece of NullScript 2 CODE and returns no value.
   ---
   A custom PROMPT-MESSAGE, in the form of any object, which will be
   displayed in its textual form, may be specified in order to notify
   the user about the requirement of an input. If set to ``NIL'', no
   notification will be displayed."
  (declare (type string code))
  (declare (type T      prompt-message))
  (process-instructions
    (extract-instructions code)
    :prompt-message prompt-message)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of NullScript 2 code generator.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of command character) +COMMAND-IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +COMMAND-IDENTIFIERS+ (make-hash-table :test #'eq)
  "Associates with each NullScript 2 command the representative token
   character.")

;;; -------------------------------------------------------

(flet ((add-command-identifier (command identifier)
        "Associates the COMMAND with the IDENTIFIER token and returns no
         value."
        (declare (type command   command))
        (declare (type character identifier))
        (setf (gethash command +COMMAND-IDENTIFIERS+) identifier)
        (values)))
  (add-command-identifier :decrement-cell       #\[)
  (add-command-identifier :increment-cell       #\])
  (add-command-identifier :square-cell          #\;)
  (add-command-identifier :print-cell-number    #\.)
  (add-command-identifier :print-cell-character #\,)
  (add-command-identifier :reset-cell           #\~)
  (add-command-identifier :input-cell-number    #\&)
  (add-command-identifier :move-left            #\<)
  (add-command-identifier :move-right           #\>)
  (add-command-identifier :copy-cell-to-queue   #\*)
  (add-command-identifier :copy-queue-to-cell   #\')
  (add-command-identifier :add-queue            #\+)
  (add-command-identifier :subtract-queue       #\-)
  (add-command-identifier :multiply-queue       #\x)
  (add-command-identifier :divide-queue         #\/)
  (add-command-identifier :remainder-queue      #\")
  (add-command-identifier :jump-forward         #\{)
  (add-command-identifier :jump-back            #\})
  (add-command-identifier :jump-to-end          #\q)
  (values))

;;; -------------------------------------------------------

(defun get-command-identifier (command)
  "Returns the identifier character associated with the COMMAND, or the
   ``NIL'' value if no association could be established."
  (declare (type command command))
  (the (or null character)
    (gethash command +COMMAND-IDENTIFIERS+)))

;;; -------------------------------------------------------

(defun convert-NullScript-2-instructions-to-code
    (instructions
     &key (instruction-separator "")
          (destination           NIL))
  "Creates the NullScript 2 code equivalent to the INSTRUCTIONS, with
   each two instruction occurrences separated by the
   INSTRUCTION-SEPARATOR, and writes it to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise producing
   and returning a fresh string containing the result."
  (declare (type (vector command *) instructions))
  (declare (type T                  instruction-separator))
  (declare (type destination        destination))
  (the (or null string)
    (if destination
      (loop
        for instruction         of-type command across instructions
        and position            of-type fixnum  from   0
        and first-instruction-p of-type boolean =      T then NIL
        do
          (unless first-instruction-p
            (format destination "~a" instruction-separator))
          (let ((identifier (get-command-identifier instruction)))
            (declare (type (or null character) identifier))
            (if identifier
              (format destination "~c" identifier)
              (error "Invalid instruction ~s at position ~d."
                instruction position))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-NullScript-2-instructions-to-code instructions
          :instruction-separator instruction-separator
          :destination           output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the text "Hello, World!" using a single cell in conjunction
;; with the inherent cell reset facility.
(interpret-NullScript-2
  "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],~
   ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-NullScript-2 "&.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on an integer input
;; of zero (0).
(interpret-NullScript-2 "]{&.}")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-NullScript-2 "&.{.}")

;;; -------------------------------------------------------

;; Fibonacci sequence generator, whose tally of productions is expected
;; as a nonnegative integer input. This number does not include the two
;; initial Fibonacci numbers 0 and 1, which will be printed in any case.
;; 
;; Memory layout:
;;   tape[0] = N = tally of Fibonacci numbers following 0 and 1
;;   tape[1] = F(n-2)
;;   tape[2] = F(n-1)
;;   tape[3] = F(n)
;;   tape[4] = ASCII character code for space (" ")
(interpret-NullScript-2
  ">>>>]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]<<<<&>.>>>,<<<>].>>,<<<<{>*>*>+.>,<*<*'<'<[}")

;;; -------------------------------------------------------

;; Fibonacci sequence generator with custom prompt text ">> ".
(interpret-NullScript-2
  ">>>>]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]<<<<&>.>>>,<<<>].>>,<<<<{>*>*>+.>,<*<*'<'<[}"
  :prompt-message ">> ")

;;; -------------------------------------------------------

;; Fibonacci sequence generator, whose tally of productions is expected
;; as a nonnegative integer input. This number does not include the two
;; initial Fibonacci numbers 0 and 1, which will be printed in any case.
;; 
;; Memory layout:
;;   tape[0] = N = tally of Fibonacci numbers following 0 and 1
;;   tape[1] = F(n-2)
;;   tape[2] = F(n-1)
;;   tape[3] = F(n)
;;   tape[4] = ASCII character code for space (" ")
(process-instructions
  (coerce
    '(;; tape[4] = 32 = ASCII code of space character (" ").
      :move-right
      :move-right
      :move-right
      :move-right
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      
      ;; Move to tape[0].
      :move-left
      :move-left
      :move-left
      :move-left
      ;; tape[0] <- input.
      :input-cell-number
      
      ;; Print tape[1] = 0.
      :move-right
      :print-cell-number
      
      ;; Print space (tape[4]) and return to tape[1].
      :move-right
      :move-right
      :move-right
      :print-cell-character
      :move-left
      :move-left
      :move-left
      
      ;; tape[2] <- 1
      :move-right
      :increment-cell
      ;; Print tape[2] = 1.
      :print-cell-number
      
      ;; Print space (tape[4]) and return to tape[2].
      :move-right
      :move-right
      :print-cell-character
      :move-left
      :move-left
      
      ;; Move to tape[0] (= N).
      :move-left
      :move-left
      
      ;; Start calculating loop.
      :jump-forward
      
      ;; Move to tape[1] (= F(n-2)).
      :move-right
      :copy-cell-to-queue
      ;; Move to tape[2] (= F(n-1)).
      :move-right
      :copy-cell-to-queue
      ;; Move to tape[3] (= F(n)).
      :move-right
      :add-queue
      
      ;; Print tape[3] = F(n).
      :print-cell-number
      
      ;; Print space (tape[4]) and return to tape[3].
      :move-right
      :print-cell-character
      :move-left
      
      ;; Queue shall become ( F(n-1) ).
      :copy-cell-to-queue
      ;; Move to tape[2].
      :move-left
      ;; Queue shall become ( F(n-1) , F(n-2) ).
      :copy-cell-to-queue
      ;; Queue: (F (n-2) ).
      :copy-queue-to-cell
      ;; Move to tape[3].
      :move-left
      ;; Queue become vacant: ().
      :copy-queue-to-cell
      
      ;; Move to tape[0] = N.
      :move-left
      ;; Set N = N - 1.
      :decrement-cell
      
      ;; Repeat if N > 0.
      :jump-back)
    '(vector command *)))

;;; -------------------------------------------------------

;; Generate the NullScript 2 program corresponding to the supplied
;; Fibonacci sequence generator instructions, with each two instructions
;; separated by a single space:
;;   > > > > ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
;;   ] ] < < < < & > . > > > , < < < > ] . > > , < < < < { > * > * > + .
;;   > , < * < * ' < ' < [ }
(convert-NullScript-2-instructions-to-code
  (coerce
    '(;; tape[4] = 32 = ASCII code of space character (" ").
      :move-right
      :move-right
      :move-right
      :move-right
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      :increment-cell
      
      ;; Move to tape[0].
      :move-left
      :move-left
      :move-left
      :move-left
      ;; tape[0] <- input.
      :input-cell-number
      
      ;; Print tape[1] = 0.
      :move-right
      :print-cell-number
      
      ;; Print space (tape[4]) and return to tape[1].
      :move-right
      :move-right
      :move-right
      :print-cell-character
      :move-left
      :move-left
      :move-left
      
      ;; tape[2] <- 1
      :move-right
      :increment-cell
      ;; Print tape[2] = 1.
      :print-cell-number
      
      ;; Print space (tape[4]) and return to tape[2].
      :move-right
      :move-right
      :print-cell-character
      :move-left
      :move-left
      
      ;; Move to tape[0] (= N).
      :move-left
      :move-left
      
      ;; Start calculating loop.
      :jump-forward
      
      ;; Move to tape[1] (= F(n-2)).
      :move-right
      :copy-cell-to-queue
      ;; Move to tape[2] (= F(n-1)).
      :move-right
      :copy-cell-to-queue
      ;; Move to tape[3] (= F(n)).
      :move-right
      :add-queue
      
      ;; Print tape[3] = F(n).
      :print-cell-number
      
      ;; Print space (tape[4]) and return to tape[3].
      :move-right
      :print-cell-character
      :move-left
      
      ;; Queue shall become ( F(n-1) ).
      :copy-cell-to-queue
      ;; Move to tape[2].
      :move-left
      ;; Queue shall become ( F(n-1) , F(n-2) ).
      :copy-cell-to-queue
      ;; Queue: (F (n-2) ).
      :copy-queue-to-cell
      ;; Move to tape[3].
      :move-left
      ;; Queue become vacant: ().
      :copy-queue-to-cell
      
      ;; Move to tape[0] = N.
      :move-left
      ;; Set N = N - 1.
      :decrement-cell
      
      ;; Repeat if N > 0.
      :jump-back)
    '(vector command *))
  :instruction-separator #\Space)
