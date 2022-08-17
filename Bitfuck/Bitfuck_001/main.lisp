;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Bitfuck", introduced by the Esolang user "Bataais"
;; (referred to as Michael Gianfreda in the user page), as well as a
;; converter from Sam Hughes's inspiring "Boolfuck" to Bitfuck.
;; 
;; Instructions
;; ============
;; Its heritage from brainfuck manifests in Bitfuck's command
;; repository, the same retains the basic functionality while curtailing
;; their tally by one, with the cell value increment ("+") and decrement
;; ("-") operations having been conflated into a single bit toggle ("*")
;; construct, thus adjusting the intrinsics to the bit level in lieu of
;; the original byte realm.
;; 
;; == OVERVIEW ==
;; Bitfuck's instruction set embraces a septuple membership, which shall
;; be summarized in the following table:
;;   
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   *       | Toggles the bit stored in the current cell.
;;   ..................................................................
;;   <       | Moves the memory pointer one cell to the left.
;;   ..................................................................
;;   >       | Moves the memory pointer one cell to the right.
;;   ..................................................................
;;   !!      | Writes the bit stored at the current cell into the
;;           | output buffer at the buffer's cursor position.
;;           | If, following this operation, the output buffer contains
;;           | exactly eight bits, these are interpreted as an ASCII
;;           | code and the associated character is printed to the
;;           | standard output. The output buffer is subsequently
;;           | purged, with its cursor set to the least significant
;;           | position zero (0).
;;           | If, following this operation, the output buffer yet
;;           | contains less than eight bits, the buffer's cursor is
;;           | moved to the next higher bit position.
;;           | Please note that, at the end of any Bitfuck program, if
;;           | the buffer still contains data, its content is printed
;;           | in the above explicated way. Unset bits in the most
;;           | significant positions are padded with zeros in order to
;;           | build an eight-bit byte, or octet.
;;   ..................................................................
;;   !*      | If the input buffer is exhausted, that is, its most
;;           | significant bit has been already returned, prompts an
;;           | ASCII character from the user, writes its ASCII code
;;           | into the input buffer, sets the buffer's cursor to its
;;           | octet's least significant position, and stores the least
;;           | significant bit in the current cell.
;;           | If the input buffer is not yet exhausted, that is, its
;;           | most significant bit has not already been queried,
;;           | stores the bit under the cursor of the input buffer in
;;           | the current cell and moves the buffer's cursor to the
;;           | next higher bit position.
;;   ..................................................................
;;   !<      | If the current cell's bit value equals zero (0), moves
;;           | the instruction pointer forward beyond the matching
;;           | ending instruction "!>".
;;           | Otherwise, simply moves the instruction pointer to the
;;           | next position.
;;   ..................................................................
;;   !>      | Moves the instruction pointer back to the matching
;;           | starting instruction "!<".
;; 
;; Bitfuck's Boolfuck cleronomy conditions an inquisition into the
;; relations with its inspiration, which shall be the following
;; juxtaposition's cynosure:
;;   
;;   Bitfuck command | Boolfuck command | Command name
;;   ----------------+------------------+------------------------------
;;   *               | +                | Toggle bit
;;   ..................................................................
;;   <               | <                | Move left
;;   ..................................................................
;;   >               | >                | Move right
;;   ..................................................................
;;   !!              | ;                | Output bit
;;   ..................................................................
;;   !*              | ,                | Input bit
;;   ..................................................................
;;   !<              | [                | Jump forward
;;   ..................................................................
;;   !>              | ]                | Jump back
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-17
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Bitfuck"
;;   -> "https://esolangs.org/wiki/Boolfuck"
;;       o Esolang specification of the inspiring Boolfuck programming
;;         language.
;;   -> "http://samuelhughes.com/boof/"
;;       o Original specification of the inspiring Boolfuck programming
;;         language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, without exhaustion, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

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
  "The ``hash-table-of'' defines a hash table of zero or more entries,
   each key of which conforms to the KEY-TYPE, associated with a value
   of the VALUE-TYPE, both defaulting to the comprehensive ``T''."
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

(deftype bit-table ()
  "The ``bit-table'' type defines a hash table composed of zero or more
   entries which associate with integer keys bit values, the former of
   which represent indices to the actual information stored in the
   latter."
  '(hash-table-of integer bit))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a hash table which associates fixnum
   keys with values of the same type, with the keys being the jump start
   points, while the values assume the role of the targets in the same
   source."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Bitfuck instruction
   types."
  '(member
    :toggle
    :move-left
    :move-right
    :output
    :input
    :start-loop
    :end-loop))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type character +NULL-CHARACTER+))

;;; -------------------------------------------------------

(defparameter +NULL-CHARACTER+ (code-char 0)
  "Represents the null character, associated with an ASCII character
   code of zero (0).")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts and returns from the piece of Bitfuck CODE a one-dimensional
   simple array of Bitfuck commands representing the program's
   instructions."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (flet
            ((advance ()
              "Moves the POSITION cursor to the next character in the
               CODE, if possible, updates the current CHARACTER, and
               returns no value."
              (setf character
                (when (array-in-bounds-p code (1+ position))
                  (char code (incf position))))
              (values))
             
             (collect-instruction (instruction)
              "Inserts to the front of the INSTRUCTIONS list the
               INSTRUCTION and returns no value."
              (declare (type command instruction))
              (push instruction instructions)
              (values)))
          
          (loop while character do
            (case character
              ((NIL)
                (loop-finish))
              
              (#\*
                (collect-instruction :toggle)
                (advance))
              
              (#\<
                (collect-instruction :move-left)
                (advance))
              
              (#\>
                (collect-instruction :move-right)
                (advance))
              
              (#\!
                (advance)
                
                (case character
                  ((NIL)
                    (loop-finish))
                  
                  ;; "!!"
                  (#\!
                    (collect-instruction :output)
                    (advance))
                  
                  ;; "!*"
                  (#\*
                    (collect-instruction :input)
                    (advance))
                  
                  ;; "!<"
                  (#\<
                    (collect-instruction :start-loop)
                    (advance))
                  
                  ;; "!>"
                  (#\>
                    (collect-instruction :end-loop)
                    (advance))
                  
                  ;; The "!" token does not introduce a command.
                  (otherwise
                    NIL)))
              
              ;; Any other character designates a comment.
              (otherwise
                (advance)))))))
    
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Input".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input ()
  ((bits
    :initarg       :bits
    :initform      0
    :type          octet
    :documentation "The ASCII code of the most recent character supplied
                    by the user.")
   (cursor
    :initarg       :cursor
    :initform      0
    :type          (integer 0 8)
    :documentation "The index into the currently active bit of the BITS.
                    ---
                    The CURSOR proceeds from the least significant to
                    the most significant position.")
   (exhausted-p
    :initarg       :exhausted-p
    :initform      T
    :type          boolean
    :documentation "Determines whether the BITS are exhausted, that is,
                    the desinent bit has been queried from this
                    ``Input'', in which case the cursor perforce must
                    have reached the most significant position of the
                    BITS."))
  (:documentation
    "The ``Input'' class provides a buffer for the maintenance of a user
     input bit sequence.
     ---
     Operating on a bit level, as opposed to brainfuck's byte-centric
     principle, a user-supplied character is reused eight times by
     Bitfuck --- one time for each of its eight bits, which compose the
     entered ASCII character's code. A corollary of this, the most
     recent input byte must be memorized, indagated from the least
     significant to the most significant bit, and marked, succeeding the
     desinent position's consumption, as requisite of a new octet. This
     service constitutes the ``Input'' class' contribution."))

;;; -------------------------------------------------------

(defun make-input ()
  "Creates and returns a new ``Input''."
  (the Input (make-instance 'Input)))

;;; -------------------------------------------------------

(defun input-set-to (input new-bits)
  "Sets the INPUT's bit sequence to the NEW-BITS, resetting its state
   for future queries, and returns the modified INPUT."
  (declare (type Input input))
  (declare (type octet new-bits))
  (with-slots (bits cursor exhausted-p) input
    (declare (type octet         bits))
    (declare (type (integer 0 8) cursor))
    (declare (type boolean       exhausted-p))
    (setf bits        new-bits)
    (setf cursor      0)
    (setf exhausted-p NIL))
  (the Input input))

;;; -------------------------------------------------------

(defun input-exhausted-p (input)
  "Checks whether the INPUT is exhausted, that is, all bits of the most
   recent bit sequence have been queried, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Input input))
  (the boolean (slot-value input 'exhausted-p)))

;;; -------------------------------------------------------

(defun input-get-next-bit (input)
  "Returns from the INPUT the next bit, moves its cursor to the
   subsequent position, and returns the modified INPUT.
   ---
   If the INPUT is already exhausted, the default value of zero (0) will
   be returned."
  (declare (type Input input))
  (with-slots (bits cursor exhausted-p) input
    (declare (type octet         bits))
    (declare (type (integer 0 8) cursor))
    (declare (type boolean       exhausted-p))
    (the bit
      (if exhausted-p
        0
        (prog1
          (ldb (byte 1 cursor) bits)
          (if (< cursor 7)
            (incf cursor)
            (setf exhausted-p T)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Output".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Output ()
  ((bits
    :initarg       :bits
    :initform      0
    :type          octet
    :documentation "A collection of at most eight bits, generated by
                    receiving from the least to the most significant
                    position its values.")
   (size
    :initarg       :size
    :initform      0
    :type          (integer 0 8)
    :documentation "The number of bits gathered hitherto in the BITS
                    sequence.")
   (output-pending-p
    :initarg       :output-pending-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether, since the last insertion of a
                    bit, the BITS have been printed. If ``T'', an output
                    is lacking; if ``NIL'', no bits have been specified
                    since the last printing operation."))
  (:documentation
    "The ``Output'' class provides a buffer for the collection of bits
     into a byte in order to realize its interpretation as an ASCII code
     and to output the octet in character form."))

;;; -------------------------------------------------------

(defun make-output ()
  "Creates and returns a new ``Output''."
  (the Output (make-instance 'Output)))

;;; -------------------------------------------------------

(defun output-append (output new-bit)
  "Adds to the current OUTPUT position the NEW-BIT and returns the
   modified OUTPUT."
  (declare (type Output output))
  (declare (type bit    new-bit))
  (with-slots (bits size output-pending-p) output
    (declare (type octet         bits))
    (declare (type (integer 0 8) size))
    (declare (type boolean       output-pending-p))
    (setf (ldb (byte 1 size) bits) new-bit)
    (setf output-pending-p         T)
    (incf size))
  (the Output output))

;;; -------------------------------------------------------

(defun output-complete-p (output)
  "Checks whether a complete byte has been specified in the OUTPUT,
   which would render it eligible for representing an eight-bit ASCII
   character, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Output output))
  (with-slots (size) output
    (declare (type (integer 0 8) size))
    (the boolean (not (null (>= size 8))))))

;;; -------------------------------------------------------

(defun output-print (output &optional (destination T))
  "Prints the ASCII character associated with the OUTPUT's internally
   managed byte to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise producing and delivering a
   fresh string containing the result.
   ---
   If the OUTPUT's octet has not yet been completed, that is, less than
   eight bits have been specified since the last purging, the wanting
   bits on the most significant laterality are padded with zero-valued
   bits."
  (declare (type Output      output))
  (declare (type destination destination))
  (with-slots (bits output-pending-p) output
    (declare (type octet   bits))
    (declare (type boolean output-pending-p))
    (setf output-pending-p NIL)
    (the (or null string)
      (format destination "~c" (code-char bits)))))

;;; -------------------------------------------------------

(defun output-clear (output)
  "Resets the OUTPUT by setting its internally managed bits to zeroes,
   their tally to zero, and marking the suspension of printing as false,
   and returns the modified OUTPUT."
  (declare (type Output output))
  (with-slots (bits size output-pending-p) output
    (declare (type octet         bits))
    (declare (type (integer 0 8) size))
    (declare (type boolean       output-pending-p))
    (setf bits             0)
    (setf size             0)
    (setf output-pending-p NIL))
  (the Output output))

;;; -------------------------------------------------------

(defun output-pending-p (output)
  "Checks whether a printing of the OUTPUT's internally managed bits is
   pending, that is, since the OUTPUT's creation or its last purge and
   subsequent insertions, no printing has taken place, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Output output))
  (the boolean (slot-value output 'output-pending-p)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :type          bit-table
    :documentation "Models a sparse array with signed integer indices
                    as keys, each associated with exactly one bit as the
                    cell value.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          integer
    :documentation "The index (key) of the currently selected cell among
                    the CELLS hash table."))
  (:documentation
    "The ``Tape'' class models a linearly arranged memory of cells, each
     a single bit's salvatory, and in their tally unlimited toward both
     lateralities, operating in conjunction with a pointer as the
     current cell's selector."))

;;; -------------------------------------------------------

(defun make-tape ()
  "Creates and returns an empty ``Tape''."
  (the Tape (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun tape-move-left (tape)
  "Moves the TAPE's pointer one cell to the left and returns the
   modified TAPE.
   ---
   If no cell in the direction exists yet, a zero-valued entity is
   generated."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-right (tape)
  "Moves the TAPE's pointer one cell to the right and returns the
   modified TAPE.
   ---
   If no cell in the direction exists yet, a zero-valued entity is
   generated."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-toggle-current-bit (tape)
  "Toggles the TAPE's current cell value and returns the modified TAPE."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type bit-table cells))
    (declare (type integer   pointer))
    (setf (gethash pointer cells 0)
          (- 1 (gethash pointer cells 0))))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-get-current-cell (tape)
  "Returns the bit stored in the TAPE's current cell."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type bit-table cells))
    (declare (type integer   pointer))
    (the bit (nth-value 0 (gethash pointer cells 0)))))

;;; -------------------------------------------------------

(defun tape-set-current-cell (tape new-value)
  "Sets the value of the TAPE's current cell to the NEW-VALUE and
   returns the modified TAPE."
  (declare (type Tape tape))
  (declare (type bit  new-value))
  (with-slots (cells pointer) tape
    (declare (type bit-table cells))
    (declare (type integer   pointer))
    (setf (gethash pointer cells 0) new-value))
  (the Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Builds and returns the loops' jump table, connecting matching loop
   start and end locations inside of the INSTRUCTIONS.
   ---
   Each entry of the resulting hash table of fixnum keys and values,
   these being indices into the INSTRUCTIONS vector, associates either
   with a loop start position the location immediately following its
   matching closing statement, or with a loop end position the exact
   location of its matching start statement. No discrimination betwixt
   keys identifying starts and terminators are being met, as the
   INSTRUCTIONS index itself determines the role, if of interest at
   all."
  (declare (type (vector command *) instructions))
  
  (let ((jump-table  (make-hash-table :test #'eql))
        (loop-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) loop-starts))
    
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0
      do
        (case instruction
          (:start-loop
            (push position loop-starts))
          
          (:end-loop
            (cond
              ;; Is the current loop end associated with a loop start?
              ;; => Connect these in the JUMP-TABLE.
              (loop-starts
                (let ((start-position (pop loop-starts)))
                  (declare (type fixnum start-position))
                  ;; Connect the loop start with the position following
                  ;; its matching end.
                  (setf (gethash start-position jump-table)
                        (1+ position))
                  ;; Connect the loop end with the position of its
                  ;; matching start.
                  (setf (gethash position jump-table) start-position)))
              ;; A loop end without a start detected?
              ;; => Signal an error.
              (T
                (error "Unmatched \"!>\" at instruction pointer ~
                        position ~d."
                  position))))
          
          (otherwise
            NIL)))
    
    ;; Do unmatched loop starts exist?
    ;; => Signal an error.
    (when loop-starts
      (error "Unmatched \"!<\" at instruction pointer positions ~
              ~{~d~^, ~}."
        loop-starts))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the Bitfuck INSTRUCTIONS and returns no value."
  (declare (type (vector command *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (tape        (make-tape))
          (input       (make-input))
          (output      (make-output))
          (jump-table  (build-jump-table instructions)))
      (declare (type fixnum            ip))
      (declare (type (or null command) instruction))
      (declare (type Tape              tape))
      (declare (type Input             input))
      (declare (type Output            output))
      (declare (type jump-table        jump-table))
      
      (labels
          ((advance-ip ()
            "Moves the instruction pointer IP to the instruction in the
             INSTRUCTIONS vector, if possible, updates the current
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
           
           (move-ip-to-opposite-bracket ()
            "Moves the instruction pointer IP to the position opposite
             of the loop demarcation associated with the current IP
             location and returns no value."
            (move-ip-to
              (gethash ip jump-table))
            (values)))
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:toggle
              (tape-toggle-current-bit tape)
              (advance-ip))
            
            (:move-left
              (tape-move-left tape)
              (advance-ip))
            
            (:move-right
              (tape-move-right tape)
              (advance-ip))
            
            (:output
              (output-append output
                (tape-get-current-cell tape))
              
              (when (output-complete-p output)
                (output-print output)
                (output-clear output))
              
              (advance-ip))
            
            (:input
              (when (input-exhausted-p input)
                (fresh-line)
                (input-set-to input
                  (char-code
                    (read-char *standard-input* NIL +NULL-CHARACTER+)))
                (clear-input))
              
              (tape-set-current-cell tape
                (input-get-next-bit input))
              
              (advance-ip))
            
            (:start-loop
              (cond
                ((zerop (tape-get-current-cell tape))
                  (move-ip-to-opposite-bracket))
                (T
                  (advance-ip))))
            
            (:end-loop
              (move-ip-to-opposite-bracket))
            
            (otherwise
              (error "Invalid instruction \"~s\" at instruction ~
                      pointer position ~d."
                instruction ip))))
        
        (when (output-pending-p output)
          (output-print output)
          (output-clear output)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Bitfuck (code)
  "Interprets the piece of Bitfuck CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Bitfuck code generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-Bitfuck-instructions-to-code
    (instructions
     &key (instruction-separator "")
          (destination           NIL))
  "Writes to the DESTINATION the code representing the Bitfuck
   INSTRUCTIONS, with each two instructions separated by the
   INSTRUCTION-SEPARATOR, and returns for a non-``NIL'' DESTINATION
   ``NIL'', otherwise producing and returning a fresh string containing
   the result."
  (declare (type (vector command *) instructions))
  (declare (type destination        destination))
  (the (or null string)
    (if destination
      (loop
        for instruction         of-type command across instructions
        and first-instruction-p of-type boolean = T then NIL
        do
          (unless first-instruction-p
            (format destination "~a" instruction-separator))
          (case instruction
            (:toggle     (format destination "*"))
            (:move-left  (format destination "<"))
            (:move-right (format destination ">"))
            (:output     (format destination "!!"))
            (:input      (format destination "!*"))
            (:start-loop (format destination "!<"))
            (:end-loop   (format destination "!>"))
            (otherwise   (error "Invalid instruction: ~s."
                           instruction))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-Bitfuck-instructions-to-code instructions
          :instruction-separator instruction-separator
          :destination           output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Boolfuck to Bitfuck converter.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-Boolfuck-to-Bitfuck (boolfuck-code
                                    &key (retain-comments-p T)
                                         (destination       NIL))
  "Converts the piece of BOOLFUCK-CODE into an equivalent Bitfuck
   program and the writes the same to the DESTINATION, producing for a
   non-``NIL'' DESTINATION a result of ``NIL'', otherwise generating and
   returning fresh string containing the output.
   ---
   If the parameter RETAIN-COMMENTS-P resolves to ``T'', adscititious
   characters representing comments in the source code, except for the
   Bitfuck command tokens \"*\" and \"!\", are transferred into the
   result; otherwise, merely instructions are translated, obviating any
   other contents' inclusion."
  (declare (type string      boolfuck-code))
  (declare (type boolean     retain-comments-p))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for character of-type character across boolfuck-code do
        (case character
          (#\+       (format destination "*"))
          (#\<       (format destination "<"))
          (#\>       (format destination ">"))
          (#\;       (format destination "!!"))
          (#\,       (format destination "!*"))
          (#\[       (format destination "!<"))
          (#\]       (format destination "!>"))
          ((#\* #\!) NIL)
          (otherwise (when retain-comments-p
                       (format destination "~c" character)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-Boolfuck-to-Bitfuck boolfuck-code
          :retain-comments-p retain-comments-p
          :destination       output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "H".
(interpret-Bitfuck "!! !! !! * !! * !! !! * !! * !!")

;;; -------------------------------------------------------

;; Print "Hello, world!".
(interpret-Bitfuck
  "!!!!!!*!!*!!!!*!!*!!
   *!!*!!*!!*!!!!*!!!!*!!
   !!!!*!!!!*!!*!!!!*!!
   !!!!*!!!!*!!*!!!!*!!
   *!!!!!!!!*!!*!!!!*!!
   !!!!*!!!!*!!*!!*!!!!
   !!!!!!!!!!*!!*!!!!
   *!!!!!!*!!*!!!!!!*!!
   *!!!!!!!!*!!*!!!!*!!
   !!*!!*!!!!*!!!!!!*!!
   !!!!*!!!!*!!*!!!!*!!
   !!!!*!!*!!!!*!!!!*!!
   *!!*!!!!!!!!*!!*!!!!
   !!*!!*!!*!!")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Bitfuck
  "* !< > !* !! !* !! !* !! !* !! !* !! !* !! !* !! !* !! < !>")

;;; -------------------------------------------------------

;; Create and print to the standard output the Bitfuck code representing
;; the infinitely repeating cat program instructions.
(convert-Bitfuck-instructions-to-code
  (coerce
    '(:toggle
      :start-loop
      :move-right
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :move-left
      :end-loop)
    '(vector command *))
  :destination           T
  :instruction-separator #\Space)

;;; -------------------------------------------------------

;; Interpret the Bitfuck infinitely repeating cat program instructions.
(process-instructions
  (coerce
    '(:toggle
      :start-loop
      :move-right
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :input
      :output
      :move-left
      :end-loop)
    '(vector command *)))

;;; -------------------------------------------------------

;; Convert the Boolfuck "Hello, world!" program into Bitfuck and print
;; the result to the standard output.
(convert-Boolfuck-to-Bitfuck
  ";;;+;+;;+;+;
  +;+;+;+;;+;;+;
  ;;+;;+;+;;+;
  ;;+;;+;+;;+;
  +;;;;+;+;;+;
  ;;+;;+;+;+;;
  ;;;;;+;+;;
  +;;;+;+;;;+;
  +;;;;+;+;;+;
  ;+;+;;+;;;+;
  ;;+;;+;+;;+;
  ;;+;+;;+;;+;
  +;+;;;;+;+;;
  ;+;+;+;"
  :destination T)

;;; -------------------------------------------------------

;; Convert the Boolfuck "Hello, world!" program into Bitfuck and
;; interpret the result.
(interpret-Bitfuck
  (convert-Boolfuck-to-Bitfuck
    ";;;+;+;;+;+;
     +;+;+;+;;+;;+;
     ;;+;;+;+;;+;
     ;;+;;+;+;;+;
     +;;;;+;+;;+;
     ;;+;;+;+;+;;
     ;;;;;+;+;;
     +;;;+;+;;;+;
     +;;;;+;+;;+;
     ;+;+;;+;;;+;
     ;;+;;+;+;;+;
     ;;+;+;;+;;+;
     +;+;;;;+;+;;
     ;+;+;+;"))

;;; -------------------------------------------------------

;; Output a reversed copy of the input.
;; The printing of the user input characters in athwart direction
;; proceeds immediately after entering the null character, associated
;; with an ASCII code of zero (0).
(interpret-Bitfuck
  ">!*>!*>!*>!*>!*>!*>!*>!*<<<<<<<<
  >>>>>>>>>*<<<<<<<<*!<>*!><!<<!>>>>>>>>>>!<*<<<<<<<<!<>!>*<!<*<!>
  >>>>>>>>>
  >!*>!*>!*>!*>!*>!*>!*>!*<<<<<<<<
  >>>>>>>>>*<<<<<<<<*!<>*!><!<<!>>>>>>>>>>!><!<*<!>
  <<<<<<<<<
  >>>>>>>>>*<<<<<<<<*!<>*!><!<<!>>>>>>>>>>!<*<<<<<<<<!<>!>*<!<*<!>
  >!!>!!>!!>!!>!!>!!>!!>!!<<<<<<<<
  <<<<<<<<<
  >>>>>>>>>*<<<<<<<<*!<>*!><!<<!>>>>>>>>>>!><!<*<!>")
