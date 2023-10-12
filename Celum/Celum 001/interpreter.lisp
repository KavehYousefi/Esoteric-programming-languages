;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the accommodation of the ``Interpreter'' class,
;; to whom the responsibility is attributed to process a parsed Celum
;; program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Celum program.")
    :type          Program
    :documentation "The Celum program to process.")
   (commands
    :initform      (make-command-iterator)
    :type          Command-Iterator
    :documentation "Maintains a cursor into the commands of the
                    PROGRAM's currently processed line.")
   (tape
    :initform      (make-tape)
    :type          Tape
    :documentation "The bilaterally infinite tape of bit-valued cells
                    comprising the program memory.")
   (bit-flag
    :initform      0
    :type          bit
    :documentation "The program's bit flag.")
   (input-buffer
    :initform      (make-input)
    :type          Input
    :documentation "Mantains the most recent user input character's
                    bits for transferrals unto the TAPE.")
   (output-buffer
    :initform      (make-output)
    :type          Output
    :documentation "Maintains an output buffer which upon each print
                    request appropriates the TAPE's central bit, and
                    stores it into its next bit location, upon an
                    octet's patration printing the character
                    corresponding to the bits, in the agency of a
                    decimal ASCII code, to the standard output.")
   (terminated-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the interpreter has been
                    terminated explicitly by the failure to find a line
                    with a desired label."))
  (:documentation
    "The ``Interpreter'' class' wike is delineated by its administration
     of actual effect to a parsed Celum program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Sets the INTERPRETER's internally managed command iterator to the
   program's current line and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program commands) interpreter
    (declare (type Program          program))
    (declare (type Command-Iterator commands))
    (when (program-current-line program)
      (command-iterator-set-to commands
        (line-commands
          (program-current-line program)))))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' dedicated to the Celum
   PROGRAM's execution."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-to-next-line (interpreter)
  "Advances the INTERPRETER's cursor to the start of the next line in
   its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program commands) interpreter
    (declare (type Program          program))
    (declare (type Command-Iterator commands))
    (declare (ignorable             commands))
    (program-advance program)
    (when (program-current-line program)
      (command-iterator-set-to commands
        (line-commands
          (program-current-line program)))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-command (interpreter)
  "Advances the INTERPRETER's command iterator to the next command on
   its program's current line, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (commands) interpreter
    (declare (type Command-Iterator commands))
    (unless (command-iterator-exhausted-p commands)
      (command-iterator-advance commands)))
  (values))

;;; -------------------------------------------------------

(defun advance-interpreter (interpreter)
  "Attempts to move the INTERPRETER's command iterator to the next
   instruction on the current line, upon the same's exhaustion,
   endeavors to advance to the start of the next line in its program, in
   any case returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (commands) interpreter
    (declare (type Command-Iterator commands))
    (if (command-iterator-exhausted-p commands)
      (advance-to-next-line    interpreter)
      (advance-to-next-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defun go-to-line (interpreter destination)
  "Relocates the INTERPRETER's program line cursor to the DESTINATION
   line, updates the internally managed command iterator, and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Line        destination))
  (with-slots (program commands) interpreter
    (declare (type Program          program))
    (declare (type Command-Iterator commands))
    (declare (ignorable             commands))
    (setf (program-current-line program) destination)
    (command-iterator-set-to commands
      (line-commands
        (program-current-line program))))
  (values))

;;; -------------------------------------------------------

(defun stop-interpreter (interpreter)
  "Terminates the INTERPRETER's workings immediately and returns no
   value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'terminated-p) T)
  (values))

;;; -------------------------------------------------------

(defun execution-completed-p (interpreter)
  "Determines whether the INTERPRETER's execution of its internally
   managed Celum program is terminated, either explicitly, or founded
   upon its line's complete exhaustion, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (or (slot-value interpreter 'terminated-p)
        (program-exhausted-p
          (slot-value interpreter 'program)))))

;;; -------------------------------------------------------

(defun get-bit-flag (interpreter)
  "Returns the INTERPRETER's bit flag value."
  (declare (type Interpreter interpreter))
  (the bit
    (slot-value interpreter 'bit-flag)))

;;; -------------------------------------------------------

(defun invert-bit-flag (interpreter)
  "Inverts the INTERPRETER's bit flag value and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (bit-flag) interpreter
    (declare (type bit bit-flag))
    (setf bit-flag (- 1 bit-flag)))
  (values))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the currently processed command, located inside of the
   INTERPRETER's active program line, or ``NIL'' if the program is
   exhausted."
  (declare (type Interpreter interpreter))
  (the (or null Command)
    (command-iterator-current-command
      (slot-value interpreter 'commands))))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Processes the COMMAND in the INTERPRETER's context and return no
     value."))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Cellular-Automaton-Command))
  (declare (type Interpreter                interpreter))
  (declare (type Cellular-Automaton-Command command))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (apply-elementary-cellular-automaton
      (cellular-automaton-command-rule command)
      tape))
  (advance-interpreter interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Input-Command))
  (declare (type Interpreter   interpreter))
  (declare (type Input-Command command))
  (declare (ignore             command))
  (with-slots (input-buffer tape) interpreter
    (declare (type Input input-buffer))
    (declare (type Tape  tape))
    ;; All bits from the INPUT-BUFFER have been consumed?
    ;; => Query for next character.
    (when (input-exhausted-p input-buffer)
      (format T "~&>> ")
      (finish-output)
      (input-set-to input-buffer
        (char-code
          (read-char)))
      (clear-input))
    ;; Store the current INPUT-BUFFER bit in the TAPE's center.
    (setf (tape-middle-cell tape)
      (input-get-next-bit input-buffer)))
  (advance-interpreter interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Output-Command))
  (declare (type Interpreter    interpreter))
  (declare (type Output-Command command))
  (declare (ignore              command))
  (with-slots (output-buffer tape) interpreter
    (declare (type Output output-buffer))
    (declare (type Tape   tape))
    (declare (ignorable   tape))
    ;; Insert the tape's central bit into the OUTPUT-BUFFER.
    (output-append output-buffer
      (tape-middle-cell tape))
    ;; If the OUTPUT-BUFFER is complete, print and reset the same.
    (when (output-complete-p output-buffer)
      (output-print output-buffer)
      (output-reset output-buffer)))
  (advance-interpreter interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Skip-Command))
  (declare (type Interpreter  interpreter))
  (declare (type Skip-Command command))
  (declare (ignore            command))
  (if (zerop (get-bit-flag interpreter))
    (advance-to-next-line interpreter)
    (advance-interpreter  interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Search-Prefix-Above-Command))
  (declare (type Interpreter                 interpreter))
  (declare (type Search-Prefix-Above-Command command))
  (declare (ignore                           command))
  (with-slots (program tape) interpreter
    (declare (type Program program))
    (declare (type Tape    tape))
    (tape-invert-middle-cell tape)
    (let ((matching-line
            (search-upwards-for-bit program
              (tape-middle-cell tape))))
      (declare (type (or null Line) matching-line))
      (if matching-line
        (go-to-line interpreter matching-line)
        (signal-missing-prefix-line-error program
          (tape-middle-cell tape)))))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Search-Prefix-Below-Command))
  (declare (type Interpreter                 interpreter))
  (declare (type Search-Prefix-Below-Command command))
  (declare (ignore                           command))
  (with-slots (program tape) interpreter
    (declare (type Program program))
    (declare (type Tape    tape))
    (tape-invert-middle-cell tape)
    (let ((matching-line
            (search-downwards-for-bit program
              (tape-middle-cell tape))))
      (declare (type (or null Line) matching-line))
      (if matching-line
        (go-to-line interpreter matching-line)
        (signal-missing-prefix-line-error program
          (tape-middle-cell tape)))))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Search-Label-Command))
  (declare (type Interpreter          interpreter))
  (declare (type Search-Label-Command command))
  (invert-bit-flag interpreter)
  (with-slots (program) interpreter
    (declare (type Program program))
    (let ((matching-line
            (find-label program
              (search-label-command-name command))))
      (declare (type (or null Line) matching-line))
      (if matching-line
        (go-to-line interpreter matching-line)
        (stop-interpreter interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Celum program under the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (execution-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Celum (code)
  "Interprets the piece of Celum source code and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))
