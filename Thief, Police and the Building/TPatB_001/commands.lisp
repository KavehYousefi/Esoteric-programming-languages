;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the command representations of a
;; "Thief, Police and the Building" program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command ()
  ((type
    :initarg       :type
    :initform      (error "Missing command type.")
    :reader        get-command-type
    :type          command-type
    :documentation "The categorizing kind of this command."))
  (:documentation
    "The ``Command'' abstract class accoutres a foundry for all classes
     the telos of which desiderates the representation of an operation
     desumed from the \"Thief, Police and the Building\" programming
     language."))

;;; -------------------------------------------------------

(defclass Enter-Elevator-Command (Command)
  ((direction
    :initarg       :direction
    :initform      (error "Missing direction.")
    :reader        get-elevator-direction
    :type          direction
    :documentation "The direction into which the elevator moves."))
  (:default-initargs
    :type :enter-elevator)
  (:documentation
    "The ``Enter-Elevator-Command'' class serves in the encapsulation
     of the \"He gets into the elevator and gets {direction}\" command,
     the same apprizes about the thief's ingress into an elevator and
     his choice of a motion direction."))

;;; -------------------------------------------------------

(defclass Enter-Stair-Room-Command (Command)
  ((direction
    :initarg       :direction
    :initform      (error "Missing direction.")
    :reader        get-climb-direction
    :type          direction
    :documentation "The direction into which the thief climbs."))
  (:default-initargs
    :type :enter-stair-room)
  (:documentation
    "The ``Enter-Stair-Room-Command'' class serves in the encapsulation
     of the \"He gets into the stair room and gets {direction}\"
     command, the same apprizes about the thief's ingress into an
     elevator and his choice of a motion direction."))

;;; -------------------------------------------------------

(defclass Operate-Elevator-Command (Command)
  ((duration
    :initarg       :duration
    :initform      (error "Missing duration.")
    :reader        get-duration-in-elevator
    :type          positive-integer
    :documentation "The number of seconds which the thief remains in the
                    elevator."))
  (:default-initargs
    :type :operate-elevator)
  (:documentation
    "The ``Operate-Elevator-Command'' class serves in the encapsulation
     of the \"He stays in the elevator for {duration}s\" command, the
     same apprizes about the thief's exploitation of the elevator for
     the specified number of seconds."))

;;; -------------------------------------------------------

(defclass Climb-Stair-Room-Command (Command)
  ((duration
    :initarg       :duration
    :initform      (error "Missing duration.")
    :reader        get-duration-in-stair-room
    :type          positive-integer
    :documentation "The number of seconds which the thief remains in the
                    stair room."))
  (:default-initargs
    :type :climb-stair-room)
  (:documentation
    "The ``Climb-Stair-Room-Command'' class serves in the encapsulation
     of the \"He stays in the stair room for {duration}s\" command, the
     same apprizes about the thief's movement inside of the stair room
     for the specified number of seconds."))

;;; -------------------------------------------------------

(defclass Leave-Command (Command)
  ()
  (:default-initargs
    :type :leave)
  (:documentation
    "The ``Leave-Command'' class serves in the encapsulation of the
     \"He gets out\" command, the same instructs the departure from an
     elevator or a stair room."))

;;; -------------------------------------------------------

(defclass Rob-Room-Command (Command)
  ((room-number
    :initarg       :room-number
    :initform      (error "Missing room number.")
    :reader        get-room-number
    :type          positive-integer
    :documentation "The number of the room on the current floor to
                    intrude and rob."))
  (:default-initargs
    :type :rob-room)
  (:documentation
    "The ``Rob-Room-Command'' class serves in the encapsulation of the
     \"He climbs into {roomNumber}-th room and steals\" command, the
     same apprizes about the thief's intrusion of the room amenable to
     the positive integer-valued number."))

;;; -------------------------------------------------------

(defclass Encounter-Police-Command (Command)
  ()
  (:default-initargs
    :type :encounter-police)
  (:documentation
    "The ``Encounter-Police-Command'' class serves in the encapsulation
     of the \"The police have come\" command, the same informs about
     the police's arrival and the epiphenomenal presentation of the
     thief's booty."))
