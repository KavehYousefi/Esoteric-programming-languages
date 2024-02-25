;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the interpreter, that entity to whom the dever
;; is allotted to impart veridical effect to a parsed
;; "Thief, Police and the Building" program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          Program
    :documentation "The \"Thief, Police and the Building\" program
                    which comprehends the configurations and commands.")
   (current-floor
    :initform      0
    :accessor      current-floor
    :type          integer
    :documentation "The floor currently occupied by the thief.")
   (program-state-machine
    :initform      (make-program-state-machine)
    :reader        get-program-state-machine
    :type          Program-State-Machine
    :documentation "Maintains the program's state for a continuously
                    observation of its validity in regards to its
                    commands.")
   (current-direction
    :initform      :up
    :accessor      current-direction
    :type          move-direction
    :documentation "The movement direction in the elevator or stair
                    room.")
   (bag
    :initform      (make-array 0
                     :element-type    'character
                     :initial-element #\Null
                     :adjustable      T
                     :fill-pointer    T)
    :accessor      bag
    :type          string
    :documentation "A dynamic string which gathers the robbed rooms'
                    characters."))
  (:documentation
    "The ``Interpreter'' class is encumbered with the onus of
     accompassing actual operative expression to a
     \"Thief, Police and the Building\" program."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' dedicated to the
   \"Thief, Police and the Building\" PROGRAM's execution."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun validate-current-floor (interpreter)
  "Determines whether the current floor, maintained by the INTERPRETER,
   constitutes a valid location in its underlying house plan, returning
   on confirmation no value, otherwise signaling an error of an
   unspecified type."
  (declare (type Interpreter interpreter))
  (with-slots (program current-floor) interpreter
    (declare (type Program program))
    (declare (type integer current-floor))
    (validate-floor-number
      (get-house-plan program)
      current-floor))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Empights the thief on the initial floor and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program current-floor) interpreter
    (declare (type Program program))
    (declare (type integer current-floor))
    (validate-floor-number
      (get-house-plan    program)
      (get-initial-floor program))
    (setf current-floor
      (get-initial-floor program)))
  (values))

;;; -------------------------------------------------------

(defun get-distance-traveled-in-elevator (interpreter duration)
  "Returns the signed offset in floors traveled by the elevator, the
   celerity of which is defined in the INTERPRETER's program, when
   operated the DURATION number of seconds."
  (declare (type Interpreter interpreter))
  (declare (type integer     duration))
  (the integer
    (* duration
      (get-elevator-speed
        (get-program interpreter)))))

;;; -------------------------------------------------------

(defun get-distance-traveled-in-stair-room (interpreter duration)
  "Returns the signed offset in floors climbed by the thief, the
   celerity of which is defined in the INTERPRETER's program, when
   operated the DURATION number of seconds."
  (declare (type Interpreter interpreter))
  (declare (type integer     duration))
  (the integer
    (* duration
      (get-stair-room-speed
        (get-program interpreter)))))

;;; -------------------------------------------------------

(defun use-elevator (interpreter duration)
  "Relocates the thief to the floor reached from his current location
   as defined by the INTERPRETER's elevator speed in conjunction with
   the operation DURATION in second, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     duration))
  (case (current-direction interpreter)
    (:up
      (incf
        (current-floor interpreter)
        (get-distance-traveled-in-elevator interpreter duration)))
    (:down
      (decf
        (current-floor interpreter)
        (get-distance-traveled-in-elevator interpreter duration)))
    (otherwise
      (error "Invalid current direction: ~s."
        (current-direction interpreter))))
  (validate-current-floor interpreter)
  (values))

;;; -------------------------------------------------------

(defun use-stair-room (interpreter duration)
  "Relocates the thief to the floor reached from his current location
   as defined by the INTERPRETER's stair room speed in conjunction with
   the operation DURATION in second, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     duration))
  (case (current-direction interpreter)
    (:up
      (incf
        (current-floor interpreter)
        (get-distance-traveled-in-stair-room interpreter duration)))
    (:down
      (decf
        (current-floor interpreter)
        (get-distance-traveled-in-stair-room interpreter duration)))
    (otherwise
      (error "Invalid current direction: ~s."
        (current-direction interpreter))))
  (validate-current-floor interpreter)
  (values))

;;; -------------------------------------------------------

(defun update-program-state (interpreter command)
  "Updates the program state as maintained by the INTERPRETER in
   response to the COMMAND's processing and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (change-program-state
    (get-program-state-machine interpreter)
    command)
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Enter-Elevator-Command))
  (declare (type Interpreter            interpreter))
  (declare (type Enter-Elevator-Command command))
  (update-program-state interpreter command)
  (setf
    (current-direction interpreter)
    (get-elevator-direction command))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Enter-Stair-Room-Command))
  (declare (type Interpreter              interpreter))
  (declare (type Enter-Stair-Room-Command command))
  (update-program-state interpreter command)
  (setf
    (current-direction interpreter)
    (get-climb-direction command))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Operate-Elevator-Command))
  (declare (type Interpreter              interpreter))
  (declare (type Operate-Elevator-Command command))
  (update-program-state interpreter command)
  (use-elevator interpreter
    (get-duration-in-elevator command))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Climb-Stair-Room-Command))
  (declare (type Interpreter              interpreter))
  (declare (type Climb-Stair-Room-Command command))
  (update-program-state interpreter command)
  (use-stair-room interpreter
    (get-duration-in-stair-room command))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Leave-Command))
  (declare (type Interpreter   interpreter))
  (declare (type Leave-Command command))
  (update-program-state interpreter command)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Rob-Room-Command))
  (declare (type Interpreter      interpreter))
  (declare (type Rob-Room-Command command))
  (update-program-state interpreter command)
  (vector-push-extend
    (get-room-content
      (get-house-plan
        (get-program interpreter))
      (current-floor   interpreter)
      (get-room-number command))
    (bag interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     Encounter-Police-Command))
  (declare (type Interpreter              interpreter))
  (declare (type Encounter-Police-Command command))
  (update-program-state interpreter command)
  (format T "~&~a"
    (bag interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the \"Thief, Police and the Building\" program governed by
   the INTERPRETER's castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (dolist (command (get-commands (get-program interpreter)))
    (declare (type Command command))
    (process-command interpreter command))
  (values))

;;; -------------------------------------------------------

(defun interpret-Thief-Police-and-the-Building (code)
  "Interprets the piece of \"Thief, Police and the Building\" source
   CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-instance 'Interpreter :program
      (parse-result-output
        (apply-parser
          (parse-program)
          (make-initial-parse-state code)))))
  (values))
