;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file defines the condition types partaking of the "5" program's
;; processing in its entirety and serving the purpose of anomalous
;; circumstances' communication to the responsible entities.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition 5-Error (error)
  ()
  (:documentation
    "The ``5-Error'' condition type serves as the firmament to all
     conditions whose purpose's commorancy can be detected in the
     representation of an anomalous situation encountered during a
     \"5\" program's evaluation or execution."))

;;; -------------------------------------------------------

(define-condition Empty-Queue-Error (5-Error)
  ((offended-queue
    :initarg       :offended-queue
    :initform      (error "Missing offended queue.")
    :reader        empty-queue-error-offended-queue
    :type          Queue
    :documentation "The queue whose attempted perquisition or
                    modulation by the OFFENDING-ACTION served as this
                    anomaly's etiology.")
   (offending-action
    :initarg       :offending-action
    :initform      "remove from"
    :reader        empty-queue-error-offending-action
    :type          string
    :documentation "A compendious description of the action whose trial
                    on the OFFENDED-QUEUE in its empty state served as
                    this anomaly's etiology."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Queue-Error condition))
      (declare (type destination       stream))
      (format stream "Cannot ~a an empty queue."
        (empty-queue-error-offending-action condition))
      (values)))
  (:documentation
    "The ``Empty-Queue-Error'' condition type serves in the apprizal
     about an anomalous situation whose emergence proceeds from an
     illicitly attempted indagation or modification of an empty
     queue."))

;;; -------------------------------------------------------

(define-condition Invalid-Input-Error (5-Error)
  ((offending-input
    :initarg       :offending-input
    :initform      (error "Missing offending input.")
    :reader        invalid-input-error-offending-input
    :type          string
    :documentation "The input whose inappropriate conformation has
                    served to instigate this error."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Input-Error condition))
      (declare (type destination         stream))
      (format stream "The input ~s cannot be parsed in a meaningful ~
                      manner, as it does not correspond to an integer ~
                      number, nor does it designate a single character."
        (invalid-input-error-offending-input condition))
      (values)))
  (:documentation
    "The ``Invalid-Input-Error'' condition type serves in the apprizal
     about an anomalous situation emerging from the provision of an
     input whose construction does neither subsume into an integer
     number's definition nor a single character."))

;;; -------------------------------------------------------

(define-condition Empty-Call-Stack-Error (5-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :type          Call-Stack
    :documentation "The call stack whose top element's removal while
                    empight in a state of any content's carency has
                    instigated this anomalous situation."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Call-Stack-Error condition))
      (declare (ignore                      condition))
      (declare (type destination            stream))
      (format stream "Cannot remove from an empty call stack.")
      (values)))
  (:documentation
    "The ``Empty-Call-Stack-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology emerges from
     the trial to remove an element from an already empty call stack."))

;;; -------------------------------------------------------

(define-condition Unmatched-Jump-Point-Error (5-Error)
  ((position
    :initarg       :position
    :initform      (error "Missing jump point position.")
    :reader        unmatched-jump-point-error-position
    :type          fixnum
    :documentation "The zero-based index of the jump start or end
                    instruction whose carency of a counterpoint is
                    peccant of this anomaly's instigation.
                    ---
                    The POSITION and DESCRIPTION slots both partake of
                    the ultimate error message as constituents in the
                    following forbisen, where the succedaneous tmemata
                    is underlined via a catena of asterisk (\"*\"):
                      Unmatched DESCRIPTION at position POSITION.
                                ***********             ********")
   (description
    :initarg       :description
    :initform      "jump point"
    :reader        unmatched-jump-point-error-description
    :type          string
    :documentation "A compendious description of this anomaly's
                    etiology.
                    ---
                    The POSITION and DESCRIPTION slots both partake of
                    the ultimate error message as constituents in the
                    following forbisen, where the succedaneous tmemata
                    is underlined via a catena of asterisk (\"*\"):
                      Unmatched DESCRIPTION at position POSITION.
                                ***********             ********"))
  (:report
    (lambda (condition stream)
      (declare (type Unmatched-Jump-Point-Error condition))
      (declare (type destination                stream))
      (format stream "Unmatched ~a at position ~d."
        (unmatched-jump-point-error-description condition)
        (unmatched-jump-point-error-position    condition))))
  (:documentation
    "The ``Unmatched-Jump-Point-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology emerges from
     the detection of a jump start or end point deprived of its
     complementing moiety."))

;;; -------------------------------------------------------

(define-condition Unterminated-Comment-Error (5-Error)
  ((start-point
    :initarg       :start-point
    :initform      (error "Missing start point.")
    :reader        unterminated-comment-error-start-point
    :type          fixnum
    :documentation "The zero-based position of the offending comment's
                    opening parenthesis, \"(\", in the program."))
  (:report
    (lambda (condition stream)
      (declare (type Unterminated-Comment-Error condition))
      (declare (type destination                stream))
      (format stream "Unterminated comment which started at the ~
                      position ~d."
        (unterminated-comment-error-start-point condition))
      (values)))
  (:documentation
    "The ``Unterminated-Comment-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology emerges from
     the detection of a comment inflicted with a carency in the
     terminating symbol, the closing parenthesis \")\"."))
