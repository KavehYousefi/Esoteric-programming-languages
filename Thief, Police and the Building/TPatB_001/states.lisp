;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the states capacitated in their assumption by
;; a "Thief, Police and the Building" program, the purpose and
;; vindication of its infrastructure realized in the determination of
;; the command's feasibility in dependence upon the currently governing
;; circumstances.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program-State".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program-State
  (:constructor make-program-state (name &rest transitions)))
  "The ``Program-State'' class encapsulates a program state, which
   appertains to the thief's actions."
  (name        (error "Missing action state name.")
               :type      keyword
               :read-only T)
  (transitions NIL
               :type      transition-table
               :read-only NIL))

;;; -------------------------------------------------------

(defun get-next-state (current-state attempted-command)
  "Returns the program state reached from the CURRENT-STATE by mediation
   of the ATTEMPTED-COMMAND, or responds with ``NIL'' if no such
   transition could be retrieved."
  (declare (type Program-State current-state))
  (declare (type Command      attempted-command))
  (the (or null Program-State)
    (cdr
      (assoc
        (get-command-type attempted-command)
        (program-state-transitions current-state)
        :test #'eq))))

;;; -------------------------------------------------------

(defmethod print-object ((state Program-State) (stream T))
  (declare (type Program-State state))
  (declare (type destination  stream))
  (format stream "(Program-State ~s :recognized-commands ~s)"
    (program-state-name state)
    (mapcar #'car
      (program-state-transitions state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of program states.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Program-State +IN-FLOOR+))
(declaim (type Program-State +IN-ELEVATOR+))
(declaim (type Program-State +IN-STAIR-ROOM+))
(declaim (type Program-State +IN-CUSTODY+))

;;; -------------------------------------------------------

(defparameter +IN-FLOOR+
  (make-program-state :in-floor)
  "Represents the state of the thief's presence in the floor, as
   counterdistinguished from the elevator or stair room.")

(defparameter +IN-ELEVATOR+
  (make-program-state :in-elevator)
  "Represents the state of the thief's presence in the elevator.")

(defparameter +IN-STAIR-ROOM+
  (make-program-state :in-stair-room)
  "Represents the state of the thief's presence in the stair room.")

(defparameter +IN-CUSTODY+
  (make-program-state :in-custody)
  "Represents the state of the police's arrival, a concomitant to the
   program's conclusion.")

;;; -------------------------------------------------------

(psetf
  (program-state-transitions +IN-FLOOR+)
  (list
    (cons :rob-room         +IN-FLOOR+)
    (cons :enter-elevator   +IN-ELEVATOR+)
    (cons :enter-stair-room +IN-STAIR-ROOM+)
    (cons :encounter-police +IN-CUSTODY+))
  
  (program-state-transitions +IN-ELEVATOR+)
  (list
    (cons :operate-elevator +IN-ELEVATOR+)
    (cons :leave            +IN-FLOOR+))
  
  (program-state-transitions +IN-STAIR-ROOM+)
  (list
    (cons :climb-stair-room +IN-STAIR-ROOM+)
    (cons :leave            +IN-FLOOR+)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program-State-Machine".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-State-Machine ()
  ((current-state
    :initform      +IN-FLOOR+
    :reader        get-program-state
    :type          Program-State
    :documentation "The current program state."))
  (:documentation
    "The ``Program-State-Machine'' class is apportioned the dever of a
     \"Thief, Police and the Building\" program state's castaldy,
     whence ensues the capability to verify or invalidate commands based
     upon the current circumstance."))

;;; -------------------------------------------------------

(defun make-program-state-machine ()
  "Creates and returns a new ``Program-State-Machine''."
  (the Program-State-Machine
    (make-instance 'Program-State-Machine)))

;;; -------------------------------------------------------

(defun change-program-state (state-machine command)
  "Transitions from the STATE-MACHINE's current state into that one
   affiliated with the COMMAND and returns no value."
  (declare (type Program-State-Machine state-machine))
  (declare (type Command               command))
  (with-slots (current-state) state-machine
    (declare (type Program-State current-state))
    (let ((next-state (get-next-state current-state command)))
      (declare (type (or null Program-State) next-state))
      (if next-state
        (setf current-state next-state)
        (error "Cannot process the command ~s in the current state ~s."
          command
          (program-state-name current-state)))))
  (values))
