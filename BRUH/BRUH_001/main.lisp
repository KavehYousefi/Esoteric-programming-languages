;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BRUH", invented by the Esolang user "Lemonz".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-07-06
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BRUH"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to ``T''."
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
  "The ``jump-table'' type defines a hash table associating fixnum keys
   to values of the same type, the former represents either a jump start
   or jump end position in an instruction sequence, mapped to the
   position of the matching opposite boundary."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines an sink for output operations,
   entailing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized instruction variants."
  '(member
    :add-1
    :subtract-1
    :square
    :insert-accumulator-left
    :insert-accumulator-right
    :move-right
    :move-left
    :jump-forward
    :jump-back
    :repeat-command
    :input-number
    :output-character
    :output-number
    :end-program))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string command) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal :size 13)
  "Associates each command identifier token with a ``command'' object.")

;;; -------------------------------------------------------

(setf (gethash "BBB"  +IDENTIFIERS+) :add-1)
(setf (gethash "BBR"  +IDENTIFIERS+) :subtract-1)
(setf (gethash "BBU"  +IDENTIFIERS+) :square)
(setf (gethash "BUR"  +IDENTIFIERS+) :insert-accumulator-left)
(setf (gethash "BUU"  +IDENTIFIERS+) :insert-accumulator-right)
(setf (gethash "RBR"  +IDENTIFIERS+) :move-right)
(setf (gethash "RBU"  +IDENTIFIERS+) :move-left)
(setf (gethash "UUU"  +IDENTIFIERS+) :jump-forward)
(setf (gethash "URU"  +IDENTIFIERS+) :jump-back)
(setf (gethash "UBR"  +IDENTIFIERS+) :repeat-command)
(setf (gethash "RRB"  +IDENTIFIERS+) :input-number)
(setf (gethash "RRU"  +IDENTIFIERS+) :output-character)
(setf (gethash "RBB"  +IDENTIFIERS+) :output-number)
(setf (gethash "RHHB" +IDENTIFIERS+) :end-program)

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER represents a command name constituents,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (find character "BRU" :test #'char=)))))

;;; -------------------------------------------------------

(defun command-terminator-p (character)
  "Checks whether the CHARACTER represents a valid successor to a
   conceited preceding command identifier, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (or null character) character))
  (the boolean
    (not (null
      (or (null character)
          (char= character #\H))))))

;;; -------------------------------------------------------

(defun filler-character-p (character)
  "Checks whether the CHARACTER represents a non-command character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (find character "RBUH" :test #'char=))))

;;; -------------------------------------------------------

(defun get-command-for (identifier)
  "Returns the ``command'' associated with the IDENTIFIER.
   ---
   An error is signaled if no matching association exists."
  (declare (type string identifier))
  (the command
    (or (gethash identifier +IDENTIFIERS+)
        (error "Unrecognized command name: ~s." identifier))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of BRUH CODE a one-dimensional
   simple array of instructions."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character in the
               CODE, if possible, updates the current CHARACTER, and
               returns no value."
              (setf character
                (when (array-in-bounds-p code (1+ position))
                  (char code (incf position))))
              (values))
             
             (move-to (new-position)
              "Moves the POSITION cursor to the NEW-POSITION, updates
               the current CHARACTER, and returns no value."
              (declare (type fixnum new-position))
              (setf position new-position)
              (setf character
                (when (array-in-bounds-p code position)
                  (char code position)))
              (values))
             
             (string-follows-p (expected-string)
              "Starting at the current POSITION, checks whether the
               following characters amount to the EXPECTED-STRING, on a
               match moving the POSITION cursor to the first character
               following the matching portion and returning a
               ``boolean'' result of ``T'', otherwise relocating the
               cursor to the position assumed before the invocation of
               this operation and returning ``NIL''."
              (declare (type string expected-string))
              (the boolean
                (let ((return-position position))
                  (declare (type fixnum return-position))
                  (loop
                    for expected-character
                      of-type character
                      across  expected-string
                    do
                      (cond
                        ((and character
                              (char= character expected-character))
                          (advance))
                        (T
                          (move-to return-position)
                          (return NIL)))
                    finally
                      (return T)))))
             
             (read-command ()
              "Starting at the current POSITION, reads a command
               identifier and returns the associated instruction."
              (the command
                (get-command-for
                  (with-output-to-string (identifier)
                    (declare (type string-stream identifier))
                    (loop
                      while (and character
                                 (identifier-character-p character))
                      do
                        (write-char character identifier)
                        (advance))))))
             
             (skip-fillers ()
              "Skips zero or more non-command characters and returns no
               value."
              (loop
                while (and character (filler-character-p character))
                do    (advance))
              (values))
             
             (expect-command-terminator ()
              "Checks whether the current CHARACTER constitutes a valid
               epilogue to an imputed preceding instruction token, on
               confirmation returning no value, otherwise signaling an
               error of an unspecified type."
              (skip-fillers)
              (if (command-terminator-p character)
                (advance)
                (error "Expected a command terminator, but encountered ~s."
                  character))
              (values))
             
             (skip-inline-comment ()
              "Starting at the current POSITION, skips all content until
               the first occurrence of the inline comment terminator
               sequence \"HH\" and returns no value."
              (loop do
                (cond
                  ((null character)
                    (error "Unterminated inline comment."))
                  ((char= character #\Newline)
                    (error "Unterminated inline comment."))
                  ((string-follows-p "HH")
                    (loop-finish))
                  (T
                    (advance))))
              (values))
             
             (skip-multiline-comment ()
              "Starting at the current POSITION, skips all content until
               the first occurrence of the multi-line comment terminator
               sequence \"HHH\" and returns no value."
              (loop do
                (cond
                  ((null character)
                    (error "Unterminated inline comment."))
                  ((string-follows-p "HHH")
                    (loop-finish))
                  (T
                    (advance))))
              (values))
             
             (skip-comment ()
              "Starting at the current POSITION, skips all content until
               the first occurrence of the matching comment terminator
               sequence and returns no value."
              (cond
                ((string-follows-p "HHH")
                  (skip-multiline-comment))
                ((string-follows-p "HH")
                  (skip-inline-comment))
                (T
                  (error "Expected comment starter at position ~d, ~
                          but instead encountered \"~c\"."
                    position character)))
              (values)))
          
          (loop while character do
            (cond
              ((null character)
                (loop-finish))
              
              ((string-follows-p "RHHB")
                (push :end-program instructions)
                (expect-command-terminator))
              
              ((identifier-character-p character)
                (push (read-command) instructions)
                (expect-command-terminator))
              
              ((char= character #\H)
                (skip-comment))
              
              (T
                (advance)))))))
    
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Accumulator".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Accumulator
  "The ``Accumulator'' class models a cell in a memory, intended to
   store a signed integer datum of unbounded magnitude."
  (value    0   :type integer)
  (previous NIL :type (or null Accumulator))
  (next     NIL :type (or null Accumulator)))

;;; -------------------------------------------------------

(defmethod print-object ((accumulator Accumulator) stream)
  (declare (type Accumulator accumulator))
  (declare (type destination stream))
  (format stream "Accumulator(~d)" (accumulator-value accumulator)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((current-accumulator
    :initarg       :current-accumulator
    :initform      (make-accumulator)
    :type          Accumulator
    :documentation "The currently active accumulator."))
  (:documentation
    "The ``Memory'' class implements a storage in the form of linearly
     arranged accumulators, each maintaining a scalar signed integer,
     operated upon by a pointer intended to memorize the currently
     selected unit.
     ---
     While initially encompassing a single, zero-valued accumulator,
     by insertions to the current entity's sinistral and dextral
     laterality, a potentially infinite tally of accumulators may be
     established.
     ---
     A pointer, starting with the incipient accumulator, at any instant
     maintains the active constituent, amenable to operations for its
     translation and queries targeted at the marked unit."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory'', initialized with a single
   accumulator which is set to zero."
  (the Memory (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-insert-left (memory)
  "Inserts a new accumulator to the left of the MEMORY's pointer and
   returns the modified MEMORY."
  (declare (type Memory memory))
  (let ((new-accumulator (make-accumulator)))
    (declare (type Accumulator new-accumulator))
    (with-slots (current-accumulator) memory
      (declare (type Accumulator current-accumulator))
      ;; If the CURRENT-ACCUMULATOR possesses a predecessor, relate the
      ;; NEW-ACCUMULATOR to the same.
      (let ((old-predecessor
              (accumulator-previous current-accumulator)))
        (declare (type (or null Accumulator) old-predecessor))
        (when old-predecessor
          (setf (accumulator-next old-predecessor)
                new-accumulator)
          (setf (accumulator-previous new-accumulator)
                old-predecessor)))
      ;; Connect the CURRENT-ACCUMULATOR and the NEW-ACCUMULATOR.
      (setf (accumulator-previous current-accumulator)
            new-accumulator)
      (setf (accumulator-next new-accumulator)
            current-accumulator)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-insert-right (memory)
  "Inserts a new accumulator to the right of the MEMORY's pointer and
   returns the modified MEMORY."
  (declare (type Memory memory))
  (let ((new-accumulator (make-accumulator)))
    (declare (type Accumulator new-accumulator))
    (with-slots (current-accumulator) memory
      (declare (type Accumulator current-accumulator))
      (let ((old-successor (accumulator-next current-accumulator)))
        (declare (type (or null Accumulator) old-successor))
        (when old-successor
          (setf (accumulator-previous old-successor)   new-accumulator)
          (setf (accumulator-next     new-accumulator) old-successor)))
      (setf (accumulator-next current-accumulator)
            new-accumulator)
      (setf (accumulator-previous new-accumulator)
            current-accumulator)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's accumulator pointer one step to the left, if
   possible, and returns the modified MEMORY.
   ---
   If the translation to the left is possible, an error of an
   unspecified type is signaled."
  (declare (type Memory memory))
  (with-slots (current-accumulator) memory
    (declare (type Accumulator current-accumulator))
    (if (accumulator-previous current-accumulator)
      (setf current-accumulator
            (accumulator-previous current-accumulator))
      (error "Cannot move left on the memory: Already on leftmost ~
              accumulator.")))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's accumulator pointer one step to the right, if
   possible, and returns the modified MEMORY.
   ---
   If the translation to the right is possible, an error of an
   unspecified type is signaled."
  (declare (type Memory memory))
  (with-slots (current-accumulator) memory
    (declare (type Accumulator current-accumulator))
    (if (accumulator-next current-accumulator)
      (setf current-accumulator (accumulator-next current-accumulator))
      (error "Cannot move right on the memory: Already on rightmost ~
              accumulator.")))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-accumulator (memory)
  "Returns the MEMORY's current accumulator value."
  (declare (type Memory memory))
  (the integer
    (accumulator-value
      (slot-value memory 'current-accumulator))))

;;; -------------------------------------------------------

(defun (setf memory-current-accumulator) (new-value memory)
  "Sets the MEMORY's current accumulator to the NEW-VALUE and returns
   the modified MEMORY."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (accumulator-value (slot-value memory 'current-accumulator))
        new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current accumulator by a quantity of one and
   returns the modified MEMORY."
  (declare (type Memory memory))
  (incf (accumulator-value (slot-value memory 'current-accumulator)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current accumulator by a quantity of one and
   returns the modified MEMORY."
  (declare (type Memory memory))
  (decf (accumulator-value (slot-value memory 'current-accumulator)))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-square (memory)
  "Squares the MEMORY's current accumulator and returns the modified
   MEMORY."
  (declare (type Memory memory))
  (with-slots (current-accumulator) memory
    (declare (type Accumulator current-accumulator))
    (setf (accumulator-value current-accumulator)
          (* (accumulator-value current-accumulator)
             (accumulator-value current-accumulator))))
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Creates and returns a jump table based upon the INSTRUCTIONS."
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
                  (setf (gethash start-position jump-table) position)
                  (setf (gethash position jump-table) start-position)))
              (T
                (error "Unmatched jump back instruction at position ~d."
                  position))))
          (otherwise
            NIL)))
    
    (when jump-starts
      (error "Unmatched jump forward instructions at positions ~
              ~{~d~^, ~}."
        jump-starts))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the BRUH INSTRUCTIONS and returns no value."
  (declare (type (vector command *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip                   0)
          (current-instruction  (aref instructions 0))
          (previous-instruction NIL)
          (jump-table           (build-jump-table instructions))
          (memory               (make-memory)))
      (declare (type fixnum            ip))
      (declare (type (or null command) current-instruction))
      (declare (type (or null command) previous-instruction))
      (declare (type jump-table        jump-table))
      (declare (type Memory            memory))
      
      (labels
          ((advance-ip ()
            "Moves the instruction pointer IP to the next instruction in
             the INSTRUCTIONS vector, if possible, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to ()
            "Moves the instruction pointer IP, expected to be located on
             a forward or back jumping instruction, to the associated
             opposite index in the INSTRUCTIONS vector, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf ip (gethash ip jump-table))
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (evaluate-instruction (instruction)
            "Evaluates the INSTRUCTION and returns no value."
            (declare (type (or null command) instruction))
            (case instruction
              ((NIL)
                NIL)
              
              (:add-1
                (memory-increment memory))
              
              (:subtract-1
                (memory-decrement memory))
              
              (:square
                (memory-square memory))
              
              (:insert-accumulator-left
                (memory-insert-left memory))
              
              (:insert-accumulator-right
                (memory-insert-right memory))
              
              (:move-right
                (memory-move-right memory))
              
              (:move-left
                (memory-move-left memory))
              
              (:jump-forward
                (when (zerop (memory-current-accumulator memory))
                  (jump-to)))
              
              (:jump-back
                (unless (zerop (memory-current-accumulator memory))
                  (jump-to)))
              
              (:repeat-command
                (when (and previous-instruction
                           (plusp (memory-current-accumulator memory)))
                  (loop repeat (memory-current-accumulator memory) do
                    (evaluate-instruction previous-instruction))))
              
              (:input-number
                (format T "~&Please input an integer: ")
                (let ((input (parse-integer (read-line))))
                  (declare (type integer input))
                  (clear-input)
                  (setf (memory-current-accumulator memory) input)))
              
              (:output-character
                (write-char
                  (code-char
                    (memory-current-accumulator memory))))
              
              (:output-number
                (format T "~d" (memory-current-accumulator memory)))
              
              (:end-program
                (setf current-instruction NIL))
              
              (otherwise
                (error "Unrecognized instruction: ~s." instruction)))
            
            (setf previous-instruction instruction)
            
            (values)))
      
      (loop while current-instruction do
        (evaluate-instruction current-instruction)
        (advance-ip)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-BRUH (code)
  "Interprets the piece of BRUH CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file loader.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-file-extension (pathname)
  "Checks whether the PATHNAME designates a file with the required
   extension or type \"bruh\", returning on confirmation no value,
   otherwise signaling an error of an unspecified type."
  (declare (type (or pathname stream string) pathname))
  (unless (pathname-type pathname)
    (error "The source file ~s does not possess the requisite ~
            file extension \"bruh\"."
      pathname))
  (values))

;;; -------------------------------------------------------

(defun get-stream-content (stream)
  "Returns the content of the STREAM as a simple string.
   ---
   Please note that the STREAM might be modified by this operation."
  (declare (type stream stream))
  (let ((buffer (make-string (file-length stream))))
    (declare (type simple-string buffer))
    (read-sequence buffer stream)
    (the simple-string buffer)))

;;; -------------------------------------------------------

(defgeneric load-BRUH-script (source)
  (:documentation
    "Loads the BRUH script from the file designated by the SOURCE,
     executes the contained program, and returns no value.
     ---
     An error occurs if the SOURCE can be associated with a file name,
     but the same does not possess an extension of \"bruh\"."))

;;; -------------------------------------------------------

(defmethod load-BRUH-script ((source stream))
  "Loads the BRUH script from the file STREAM, executes it, and returns
   no value."
  (declare (type stream source))
  (check-file-extension source)
  (interpret-BRUH
    (get-stream-content source))
  (values))

;;; -------------------------------------------------------

(defmethod load-BRUH-script ((source string))
  "Loads the BRUH script from the file designated by the SOURCE name,
   executes it, and returns no value."
  (check-file-extension source)
  (with-open-file (input-stream source
                   :direction         :input
                   :element-type      'character
                   :if-does-not-exist :error)
    (declare (type file-stream input-stream))
    (interpret-BRUH
      (get-stream-content input-stream)))
  (values))

;;; -------------------------------------------------------

(defmethod load-BRUH-script ((source pathname))
  "Loads the BRUH script from the file designated by the SOURCE
   pathname, executes it, and returns no value."
  (declare (type pathname source))
  (check-file-extension source)
  (with-open-file (input-stream source
                   :direction         :input
                   :element-type      'character
                   :if-does-not-exist :error)
    (declare (type file-stream input-stream))
    (interpret-BRUH
      (get-stream-content input-stream)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adder.
(interpret-BRUH
"HH Add an accumulator HH
BUUH
HH Input HH
RRBHRBRHRRBHRBUH
HH Adding HH
UUUH
BBRHRBRHBBBHRBUH
URUH
HH Output HH
RBRHRBBHRHHB")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-BRUH
  "RRBHUUUHRBBHURUHRBBHRHHB")

;;; -------------------------------------------------------

;; Doubles the user input number by utilizing the previous command
;; repetition instruction "UBR".
(interpret-BRUH
"HH Input a positive integer. HH
RRBH
HH Increment the current cell. HH
BBBH
HH Repeat the previous command. HH
UBRH
HHH
Decrement the accumulator in order to set it to the input times two.
HHH
BBRH
BBRH
HH Output the accumulator. HH
RBBH
RHHB")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on an input of
;; zero.
(interpret-BRUH
  "RRBH
   RBBH
   UUUH
   RRBH
   RBBH
   URUH
   RHHB")
