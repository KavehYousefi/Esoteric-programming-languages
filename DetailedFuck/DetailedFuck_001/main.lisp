;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "DetailedFuck", invented by the Esolang user "Lebster" in
;; the year 2020, and devised as a more expressive variant of Urban
;; Mueller's "brainfuck", as well routines for the translation betwixt
;; these two specimens.
;; 
;; Concept
;; =======
;; DetailedFuck constitutes an equivalent of the esoteric programming
;; language brainfuck, the single-character instructions of which have
;; been substituted by phrases of nimious capacity with the purpose of
;; augmenting expressiveness.
;; 
;; == CONVERSION OF BRAINFUCK TO DETAILEDFUCK ==
;; Being based on a trivial substitution, converting from brainfuck to
;; DetailedFuck merely involves the substitution of the former's
;; instructions with the associated analogues of the latter.
;; The following table conveys the mapping in this direction:
;; 
;;   brainfuck instruction | DetailedFuck equivalent
;;   ----------------------+-------------------------------------------
;;   >                     | MOVE THE MEMORY POINTER ONE CELL TO THE
;;                         | RIGHT
;;   ..................................................................
;;   <                     | MOVE THE MEMORY POINTER ONE CELL TO THE
;;                         | LEFT
;;   ..................................................................
;;   +                     | INCREMENT THE CELL UNDER THE MEMORY
;;                         | POINTER BY ONE
;;   ..................................................................
;;   -                     | DECREMENT THE CELL UNDER THE MEMORY
;;                         | POINTER BY ONE
;;   ..................................................................
;;   ,                     | REPLACE THE CELL UNDER THE MEMORY
;;                         | POINTER'S VALUE WITH THE ASCII CHARACTER
;;                         | CODE OF USER INPUT
;;   ..................................................................
;;   .                     | PRINT THE CELL UNDER THE MEMORY POINTER'S
;;                         | VALUE AS AN ASCII CHARACTER
;;   ..................................................................
;;   [                     | IF THE CELL UNDER THE MEMORY POINTER'S
;;                         | VALUE IS ZERO INSTEAD OF READING THE NEXT
;;                         | COMMAND IN THE PROGRAM JUMP TO THE
;;                         | CORRESPONDING COMMAND EQUIVALENT TO THE ]
;;                         | COMMAND IN BRAINFUCK
;;   ..................................................................
;;   ]                     | IF THE CELL UNDER THE MEMORY POINTER'S
;;                         | VALUE IS NOT ZERO INSTEAD OF READING THE
;;                         | NEXT COMMAND IN THE PROGRAM JUMP TO THE
;;                         | CORRESPONDING COMMAND EQUIVALENT TO THE [
;;                         | COMMAND IN BRAINFUCK
;; 
;; Note that brainfuck tacitly ignores any character not defined as an
;; instruction, an expression of tolerance that is usually appropriated
;; to state commands in the source code. The reservation of the two
;; substantial character "!" and "#" in DetailedFuck imposes a risk if
;; occurring as comments in the brainfuck code. This in conjunction with
;; no explicit assurance of non-instruction characters being toleraded
;; in DetailedFuck code, it is safe and advised to skip content not
;; appertaining to brainfuck commands.
;; 
;; == CONVERSION OF DETAILEDFUCK TO BRAINFUCK ==
;; The facility of the transcription model propagates through the
;; athwart direction. When converting DetailedFuck instructions to
;; brainfuck, each instruction from the source must be substituted by
;; the destination language's equivalent. The following tabular
;; representation conforms to this perspective. Please note that the
;; linebreaks are not part of the DetailedFuck instruction set; instead
;; a single whitespace connects the words.
;; 
;;   DetailedFuck instruction                    | brainfuck equivalent
;;   --------------------------------------------+---------------------
;;   MOVE THE MEMORY POINTER ONE CELL TO THE     | >
;;   RIGHT                                       |
;;   ..................................................................
;;   MOVE THE MEMORY POINTER ONE CELL TO THE     | <
;;   LEFT                                        |
;;   ..................................................................
;;   INCREMENT THE CELL UNDER THE MEMORY POINTER | +
;;   BY ONE                                      |
;;   ..................................................................
;;   DECREMENT THE CELL UNDER THE MEMORY POINTER | -
;;   BY ONE                                      |
;;   ..................................................................
;;   REPLACE THE CELL UNDER THE MEMORY POINTER'S | ,
;;   VALUE WITH THE ASCII CHARACTER CODE OF USER |
;;   INPUT                                       |
;;   ..................................................................
;;   PRINT THE CELL UNDER THE MEMORY POINTER'S   | .
;;   VALUE AS AN ASCII CHARACTER                 |
;;   ..................................................................
;;   IF THE CELL UNDER THE MEMORY POINTER'S      | [
;;   VALUE IS ZERO INSTEAD OF READING THE NEXT   |
;;   COMMAND IN THE PROGRAM JUMP TO THE          |
;;   CORRESPONDING COMMAND EQUIVALENT TO THE ]   |
;;   COMMAND IN BRAINFUCK                        |
;;   ..................................................................
;;   IF THE CELL UNDER THE MEMORY POINTER'S      | ]
;;   VALUE IS NOT ZERO INSTEAD OF READING THE    |
;;   NEXT COMMAND IN THE PROGRAM JUMP TO THE     |
;;   CORRESPONDING COMMAND EQUIVALENT TO THE [   |
;;   COMMAND IN BRAINFUCK 
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-10-08
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/DetailedFuck"
;;   -> "https://stackoverflow.com/questions/4366668/str-replace-in-common-lisp"
;;       o Implements a substring replacement function in Common Lisp.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more entries, each
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

(deftype command ()
  "The ``command'' type enumerates the recognized DetailedFuck, and, by
   extension, the brainfuck, instruction types."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :input
    :output
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype located-instruction ()
  "The ``located-instruction'' type defines a compound holding the
   position of a command detected in a DetailedFuck source code string,
   together with the command object itself, encapsulates in this order
   as the two parts of a cons."
  '(cons fixnum command))

;;; -------------------------------------------------------

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines a sequence of instructions as
   either a one-dimensional simple array of commands or a vector of the
   same."
  '(or (simple-array command (*))
       (vector       command  *)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump positions
   inside of a piece of source code to their respective back jump
   indices, and vice versa, manifesting in a hash table that associates
   fixnums to fixnums."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a hash table which
   maps the cell indices, represented as signed integers, to the cell
   values of the same type."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines an output sink for print operations,
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string command)
               +DETAILEDFUCK-TOKEN-TABLE+))

;;; -------------------------------------------------------

(defparameter +DETAILEDFUCK-TOKEN-TABLE+
  (make-hash-table :test #'equal)
  "Associates each DetailedFuck command token with the respective
   command object.")

;;; -------------------------------------------------------

(flet ((register-DetailedFuck-token (detailedFuck-token command)
        "Associates the DETAILEDFUCK-TOKEN with the COMMAND in the
         +DETAILEDFUCK-TOKEN-TABLE+ and returns no value."
        (declare (type string  detailedFuck-token))
        (declare (type command command))
        (setf (gethash detailedFuck-token +DETAILEDFUCK-TOKEN-TABLE+)
              command)
        (values)))
  (register-DetailedFuck-token
    "MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT"
    :move-right)
  (register-DetailedFuck-token
    "MOVE THE MEMORY POINTER ONE CELL TO THE LEFT"
    :move-left)
  (register-DetailedFuck-token
    "INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE"
    :increment)
  (register-DetailedFuck-token
    "DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE"
    :decrement)
  (register-DetailedFuck-token
    "REPLACE THE CELL UNDER THE MEMORY POINTER'S VALUE WITH THE ASCII CHARACTER CODE OF USER INPUT"
    :input)
  (register-DetailedFuck-token
    "PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER"
    :output)
  (register-DetailedFuck-token
    "IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK"
    :jump-forward)
  (register-DetailedFuck-token
    "IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK"
    :jump-back)
  (values))

;;; -------------------------------------------------------

(defun get-command-for-token (token)
  "Returns the command associated with the DetailedFuck TOKEN, or
   signals an error of an unspecified type if no correspondence holds."
  (declare (type string token))
  (the command
    (or (gethash token +DETAILEDFUCK-TOKEN-TABLE+)
        (error "No command associated with the token ~s." token))))

;;; -------------------------------------------------------

(defun find-instruction-locations (source token)
  "Searches for all occurrences of the TOKEN in the SOURCE and returns
   a list of cons, each instance of which contains in its left part the
   TOKEN's location in the SOURCE, while the right moeity entails the
   command associated with the same."
  (declare (type string source))
  (declare (type string token))
  (let ((detections   NIL)
        (token-length (length token)))
    (declare (type (list-of located-instruction) detections))
    (declare (type fixnum                        token-length))
    (loop
      with current-token-position  of-type (or null fixnum) = 0
      with previous-token-position of-type fixnum           = 0
      do
        (setf current-token-position
          (search token source
            :start2 previous-token-position
            :test   #'string=))
        (cond
          (current-token-position
            (push (cons current-token-position
                        (get-command-for-token token))
                  detections)
            (setf previous-token-position
              (+ current-token-position
                 token-length)))
          (T
            (loop-finish))))
    (the (list-of located-instruction) detections)))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of DetailedFuck CODE a
   one-dimensional simple array of instructions."
  (declare (type string code))
  (the instruction-vector
    (loop
      for detailedFuck-token
        of-type string
        being the hash-keys in +DETAILEDFUCK-TOKEN-TABLE+
      append
        (find-instruction-locations code detailedFuck-token)
      into
        located-instructions
      finally
        (return
          (map '(simple-array command (*)) #'cdr
            (sort located-instructions #'< :key #'car))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Creates and returns a jump table for the INSTRUCTIONS, associating
   with each forward jump operation's index in the same the
   corresponding back jump location, and vice versa."
  (declare (type instruction-vector instructions))
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    
    (loop
      for brainfuck-token of-type command across instructions
      and position        of-type fixnum  from   0
      do
        (case brainfuck-token
          (:jump-forward
            (push position forward-jump-points))
          
          (:jump-back
            (cond
              (forward-jump-points
                (let ((forward-jump-position (pop forward-jump-points)))
                  (declare (type fixnum forward-jump-position))
                  (setf (gethash forward-jump-position jump-table)
                        position)
                  (setf (gethash position jump-table)
                        forward-jump-position)))
              (T
                (error "Unmatched back jump point at position ~d."
                  position))))
          
          (otherwise
            NIL)))
    
    (when forward-jump-points
      (error "Unmatched forward jump points at positions ~{~d~^, ~}."
        forward-jump-points))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the DetailedFuck INSTRUCTIONS and returns no value."
  (declare (type instruction-vector instructions))
  
  (when (plusp (length instructions))
    (let ((ip         0)
          (command    (aref instructions 0))
          (jump-table (build-jump-table instructions))
          (memory     (make-hash-table :test #'eql))
          (pointer    0))
      (declare (type fixnum            ip))
      (declare (type (or null command) command))
      (declare (type jump-table        jump-table))
      (declare (type memory            memory))
      (declare (type integer           pointer))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the INSTRUCTIONS vector, if possible, updates the current
             COMMAND, and returns no value."
            (setf command
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (move-to (new-position)
            "Relocates the instruction pointer IP to the NEW-POSITION in
             the INSTRUCTIONS vector, updates the current COMMAND, and
             returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf command
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (jump-to-opposite-boundary ()
            "Expecting the instruction pointer IP to be located at a
             forward or back jump instruction, relocates it to the
             opposite boundary, updates the current COMMAND, and returns
             no value."
            (move-to (gethash ip jump-table))
            (values)))
        
        (symbol-macrolet
            ((current-cell
              (the integer
                (gethash pointer memory 0))))
          
          (loop while command do
            (case command
              ((NIL)
                (loop-finish))
              
              (:move-right
                (incf pointer))
              
              (:move-left
                (decf pointer))
              
              (:increment
                (incf current-cell))
              
              (:decrement
                (decf current-cell))
              
              (:output
                (write-char (code-char current-cell)))
              
              (:input
                (format T "~&Please input an ASCII character: ")
                (setf current-cell
                      (char-code (read-char)))
                (clear-input))
              
              (:jump-forward
                (when (zerop current-cell)
                  (jump-to-opposite-boundary)))
              
              (:jump-back
                (unless (zerop current-cell)
                  (jump-to-opposite-boundary)))
              
              (otherwise
                NIL))
            
            (advance))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-DetailedFuck (code)
  "Interprets the piece of DetailedFuck CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-DetailedFuck converter.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-DetailedFuck-token-for-brainfuck (brainfuck-token)
  "Returns the DetailedFuck command token corresponding to the
   BRAINFUCK-TOKEN, or ``NIL'' if the latter does not represent a
   brainfuck command."
  (declare (type character brainfuck-token))
  (the (or null string)
    (case brainfuck-token
      (#\>       "MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT")
      (#\<       "MOVE THE MEMORY POINTER ONE CELL TO THE LEFT")
      (#\+       "INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE")
      (#\-       "DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE")
      (#\,       "REPLACE THE CELL UNDER THE MEMORY POINTER'S VALUE WITH THE ASCII CHARACTER CODE OF USER INPUT")
      (#\.       "PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER")
      (#\[       "IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK")
      (#\]       "IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK")
      (otherwise NIL))))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-DetailedFuck (brainfuck-code
                                          &key (destination NIL))
  "Creates a DetailedFuck program equivalent to the BRAINFUCK-CODE and
   writes its source code to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string containing the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for brainfuck-token of-type character across brainfuck-code
        and first-token-p   of-type boolean   =      T then NIL
        do
          (let ((detailedFuck-token
                  (get-DetailedFuck-token-for-brainfuck
                    brainfuck-token)))
          (declare (type (or null string) detailedFuck-token))
          (when detailedFuck-token
            (unless first-token-p
              (format destination "~&"))
            (format destination "~a" detailedFuck-token))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-DetailedFuck brainfuck-code
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of DetailedFuck-to-brainfuck converter.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-brainfuck-token-for-command (command)
  "Returns the brainfuck token associated with the COMMAND, or signals
   an error of an unspecified type if no correspondence exists."
  (declare (type command command))
  (the character
    (case command
      (:move-right   #\>)
      (:move-left    #\<)
      (:increment    #\+)
      (:decrement    #\-)
      (:input        #\,)
      (:output       #\.)
      (:jump-forward #\[)
      (:jump-back    #\])
      (otherwise     (error "Invalid command: ~s." command)))))

;;; -------------------------------------------------------

(defun convert-instructions-to-brainfuck (instructions
                                          &key (destination NIL))
  "Converts the INSTRUCTIONS into an equivalent brainfuck program and
   writes its source code to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type instruction-vector instructions))
  (declare (type destination        destination))
  (the (or null string)
    (if destination
      (loop for instruction of-type command across instructions do
        (format destination "~c"
          (get-brainfuck-token-for-command instruction)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-instructions-to-brainfuck instructions
          :destination output)))))

;;; -------------------------------------------------------

(defun convert-DetailedFuck-to-brainfuck (detailedFuck-code
                                          &key (destination NIL))
  "Converts the piece of DETAILEDFUCK-CODE into an equivalent brainfuck
   program and writes its source code to the DESTINATION, returning for
   a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding
   with a fresh string comprehending the result."
  (declare (type string      detailedFuck-code))
  (declare (type destination destination))
  (the (or null string)
    (convert-instructions-to-brainfuck
      (extract-instructions detailedFuck-code)
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!" to the standard output.
(interpret-DetailedFuck
  "
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  ")

;;; -------------------------------------------------------

;; Convert the brainfuck code capable of printing "Hello World!" into
;; the equivalent DetailedFuck program and execute it.
(interpret-DetailedFuck
  (convert-brainfuck-to-DetailedFuck
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-DetailedFuck
  "
  REPLACE THE CELL UNDER THE MEMORY POINTER'S VALUE WITH THE ASCII CHARACTER CODE OF USER INPUT
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
  REPLACE THE CELL UNDER THE MEMORY POINTER'S VALUE WITH THE ASCII CHARACTER CODE OF USER INPUT
  PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
  IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
  ")

;;; -------------------------------------------------------

;; Convert an infinite cat program from brainfuck to DetailedFuck and
;; print the resulting program to the standard output.
(convert-brainfuck-to-DetailedFuck
  ",.[,.]"
  :destination T)

;;; -------------------------------------------------------

;; Convert an infinite cat program from brainfuck to DetailedFuck and
;; interpret the resulting program.
(interpret-DetailedFuck
  (convert-brainfuck-to-DetailedFuck
    ",.[,.]"))
