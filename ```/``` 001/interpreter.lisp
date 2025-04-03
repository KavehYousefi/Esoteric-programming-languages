;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the interpreter, the parcery of its bailiwick
;; manifesting in the execution of a parsed ``` program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "No program for the interpreter specified.")
    :reader        interpreter-program
    :type          program
    :documentation "The executable ``` as a vector of instructions.")
   (memory
    :initarg       :memory
    :initform      (make-default-memory)
    :reader        interpreter-memory
    :type          Memory
    :documentation "The program memory to operate on."))
  (:documentation
    "The ``Interpreter'' class receives the dever's assignment that
     pertains to the execution of a ``` program furnished as an ordered
     sequence of instructions, amenable to zero-based indices."))

;;; -------------------------------------------------------

(defun make-interpreter (program
                         &optional (memory (make-default-memory)))
  "Creates and returns a fresh ``Interpreter'' whose dedication relates
   to the ``` PROGRAM's evaluation, and whose data castaldy is assigned
   to the optional MEMORY instance, which defaults to an empty
   standard."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter
      :program program
      :memory  memory)))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction referenced by the instruction pointer (IP)
   cell in the INTERPRETER's memory."
  (declare (type Interpreter interpreter))
  (the Instruction
    (aref (interpreter-program interpreter)
      (get-ip-position
        (interpreter-memory interpreter)))))

;;; -------------------------------------------------------

(defun handle-input/output (interpreter)
  "Handles a behest involving the issuance of output or request input,
   acutated by the intention to modulate the cell at the address two
   (2) in the INTERPRETER's program memory instance and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (case (get-io-mode memory)
      ;; Read from cells spanning the range 4--24.
      (:output
        (write-char
          (code-char
            (read-unicode-code-point-from-memory memory 4))))
      
      ;; Store in cells spanning the range 4--24.
      (:input
        (format T "~&>> ")
        (finish-output)
        (write-unicode-code-point-to-memory memory 4
          (char-code
            (read-char NIL NIL #\Null)))
        (clear-input))
      
      ;; Unrecognized input/output mode selector?
      ;; => No causatum.
      (:none
        NIL)
      
      ;; Unexpected state?
      ;; => Signal error.
      (otherwise
        (error "Invalid input/output mode ~s in cell 3 with ~
                value ~d."
          (get-io-mode memory)
          (cell-value  memory 3))))
    
    (disable-io memory)
    (advance-ip memory))
  (values))

;;; -------------------------------------------------------

(defun execute-instruction (interpreter instruction)
  "Executes the INSTRUCTION in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    
    (multiple-value-bind (destination-address source-value)
        (get-destination-and-source instruction memory)
      (declare (type integer destination-address))
      (declare (type integer source-value)
               (ignorable    source-value))
      
      (cond
        ;; Cell #1 is non-zero, and no attempt to modify the same?
        ;; => No causatum.
        ((and (not (execution-enabled-p memory))
              (not (address-of-execution-cell-p destination-address)))
          (advance-ip memory))
        
        ;; Cell #1 is non-zero, but attempt to modify the same?
        ;; => Modify value of cell #1.
        ((and (not (execution-enabled-p memory))
              (address-of-execution-cell-p destination-address))
          (setf (cell-value memory destination-address) source-value)
          (advance-ip memory))
        
        ;; Input/output request (cell #2)?
        ((address-of-io-switch-cell-p destination-address)
          (handle-input/output interpreter))
        
        ;; Non-special cell manipulation behest?
        (T
          (setf (cell-value memory destination-address) source-value)
          
          ;; The instruction pointer (IP) cell has been modified?
          ;; => Do not simply advance towards next instruction.
          (unless (address-of-ip-cell-p destination-address)
            (advance-ip memory))))))
  
  (values))

;;; -------------------------------------------------------

(defun program-has-completed-p (interpreter)
  "Determines whether the INTERPRETER has completed its program's
   execution, a diorism conflating with the responsible instruction
   pointer's (IP) transcendence beyond the valid bournes, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (get-ip-position
            (interpreter-memory interpreter))
          (length
            (interpreter-program interpreter))))))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the ``` program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-has-completed-p interpreter) do
    (execute-instruction interpreter
      (get-current-instruction interpreter)))
  (values))

;;; -------------------------------------------------------

(defun |interpret-```| (code)
  "Interprets the piece of ``` source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program
        (make-lexer code))))
  (values))
