;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements an encapsulation of a parsed
;; "Thief, Police and the Building" program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((initial-floor
    :initarg       :initial-floor
    :initform      (error "Missing initial floor.")
    :reader        get-initial-floor
    :type          integer
    :documentation "The inicipial floor occupied by the thief.")
   (elevator-speed
    :initarg       :elevator-speed
    :initform      (error "Missing elevator speed (SoE).")
    :reader        get-elevator-speed
    :type          integer
    :documentation "The number of floors traveled by the elevator in
                    one second.")
   (stair-room-speed
    :initarg       :stair-room-speed
    :initform      (error "Missing stair room speed (SoS).")
    :reader        get-stair-room-speed
    :type          integer
    :documentation "The number of floors climbed by the thief in one
                    second.")
   (house-plan
    :initarg       :house-plan
    :initform      (error "Missing house plan.")
    :reader        get-house-plan
    :type          House-Plan
    :documentation "The conformation of the building to rob.")
   (commands
    :initarg       :commands
    :initform      (error "Missing commands.")
    :reader        get-commands
    :type          command-list
    :documentation "The behests imposed the program."))
  (:documentation
    "The ``Program'' class serves in the encapsulation of a
     \"Thief, Police and the Building\" program's properties,
     encompassing the incipial configurations, house plan, and
     instruction sequence."))
