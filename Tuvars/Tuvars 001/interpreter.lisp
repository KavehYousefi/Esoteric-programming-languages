;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the interpreter, that component entalented with
;; the responsibility to evaluate a sequence of Tuvars instructions and
;; impart actual effect to these.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
    (program
     &key (console (make-default-text-console)))))
  "The ``Interpreter'' class is apportioned the dever of accompassing
   actual operative vallidom to an executable Tuvars program."
  (program     (error "Missing program for interpreter.")
               :type      tuvars-program
               :read-only T)
  ;; The zero-based index into the PROGRAM vector.
  (ip          0
               :type      fixnum
               :read-only NIL)
  ;; The zero-based line number set by the contemporaneous goto command.
  (jump-target NIL
               :type      (or null integer)
               :read-only NIL)
  (variable-a  0
               :type      real
               :read-only NIL)
  (variable-b  0
               :type      real
               :read-only NIL)
  (console     (error "Missing console.")
               :type      Console
               :read-only T))

;;; -------------------------------------------------------

(defun get-variable-accessor (variable-name)
  "Returns for the variable amenable to the VARIABLE-NAME the symbolic
   accessor function designator."
  (declare (type character variable-name))
  (the symbol
    (case variable-name
      (#\a       'interpreter-variable-a)
      (#\b       'interpreter-variable-b)
      (otherwise (error "Unrecognized variable: ~s." variable-name)))))

;;; -------------------------------------------------------

(defun variable-value (interpreter variable-name)
  "Returns the value of the variable registered with the VARIABLE-NAME
   at the INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type character   variable-name))
  (the real
    (funcall
      (get-variable-accessor variable-name)
      interpreter)))

;;; -------------------------------------------------------

(defun (setf variable-value) (new-value interpreter variable-name)
  "Stores the NEW-VALUE in the variable registered with the
   VARIABLE-NAME at the INTERPRETER and returns no value.."
  (declare (type real        new-value))
  (declare (type Interpreter interpreter))
  (declare (type character   variable-name))
  (funcall
    (fdefinition
      (list 'setf
        (get-variable-accessor variable-name)))
    new-value
    interpreter)
  (values))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer, either by translating
   it to the subsequent position in its instruction sequence, or
   alternatively, if a \"goto\" operation has imposed siccan request, by
   relocating it to the respective jump destination, in any case
   returning no value."
  (declare (type Interpreter interpreter))
  (if (interpreter-jump-target interpreter)
    (shiftf
      (interpreter-ip          interpreter)
      (interpreter-jump-target interpreter)
      NIL)
    (incf (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the INTERPRETER's program has completed its
   execution, which is the case if the instruction pointer (IP)'s
   location violates the admitted bournes, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value
      (not
        (array-in-bounds-p
          (interpreter-program interpreter)
          (interpreter-ip      interpreter))))))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Clear-Command))
  (declare (type Interpreter   interpreter))
  (declare (type Clear-Command command))
  (declare (ignore             command))
  (clear-console
    (interpreter-console interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Close-Command))
  (declare (type Interpreter   interpreter))
  (declare (type Close-Command command))
  (declare (ignore             command))
  (close-console
    (interpreter-console interpreter))
  (setf (interpreter-ip interpreter)
    (length
      (interpreter-program interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Goto-Command))
  (declare (type Interpreter  interpreter))
  (declare (type Goto-Command command))
  (setf
    (interpreter-jump-target  interpreter)
    (1- (goto-command-line-number command)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     If-Command))
  (declare (type Interpreter interpreter))
  (declare (type If-Command  command))
  (when (zerop
          (variable-value interpreter
            (if-command-variable command)))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     If-Not-Command))
  (declare (type Interpreter    interpreter))
  (declare (type If-Not-Command command))
  (unless (zerop
            (variable-value interpreter
              (if-not-command-variable command)))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Print-Character-Command))
  (declare (type Interpreter             interpreter))
  (declare (type Print-Character-Command command))
  (insert-character
    (interpreter-console interpreter)
    (code-char
      (round
        (variable-value interpreter
          (print-character-command-variable command)))))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Print-Linebreak-Command))
  (declare (type Interpreter             interpreter))
  (declare (type Print-Linebreak-Command command))
  (declare (ignore                       command))
  (insert-linebreak
    (interpreter-console interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Print-Number-Command))
  (declare (type Interpreter          interpreter))
  (declare (type Print-Number-Command command))
  (insert-number
    (interpreter-console interpreter)
    (variable-value interpreter
      (print-number-command-variable command)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Print-String-Command))
  (declare (type Interpreter          interpreter))
  (declare (type Print-String-Command command))
  (insert-string
    (interpreter-console interpreter)
    (print-string-command-message command))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Read-Command))
  (declare (type Interpreter  interpreter))
  (declare (type Read-Command command))
  (format T "~&>> ")
  (finish-output)
  (setf
    (variable-value interpreter
      (read-command-variable command))
    (char-code
      (read-char)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Title-Command))
  (declare (type Interpreter   interpreter))
  (declare (type Title-Command command))
  (set-title
    (interpreter-console interpreter)
    (title-command-text command))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Tuvars program maintained by the INTERPRETER and
   returns no value."
  (declare (type Interpreter interpreter))
  (start-console
    (interpreter-console interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (aref
        (interpreter-program interpreter)
        (interpreter-ip      interpreter)))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Tuvars (code)
  "Interprets the piece of Tuvars source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))
