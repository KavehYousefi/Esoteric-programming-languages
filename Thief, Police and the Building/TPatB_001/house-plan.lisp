;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements a representation of the building subjected to
;; the thief's maleficence.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Floor-Plan".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Floor-Plan
  (:constructor make-floor-plan (ground-floor-p rooms)))
  "The ``Floor-Plan'' class serves in the encapsulation of a floor's
   conformation, which comprehends a twissel of informations, namely, a
   flag determining its membership among the ground flag and a list of
   one or more rooms, each represented by its encompassed character."
  (ground-floor-p (error "Missing ground floor flag.")
                  :type      boolean
                  :read-only T)
  (rooms          (error "Missing floor room characters.")
                  :type      (list-of character)
                  :read-only T))

;;; -------------------------------------------------------

(defun floor-plan-size (floor-plan)
  "Returns the number of rooms comprising the FLOOR-PLAN."
  (the positive-integer
    (length
      (floor-plan-rooms floor-plan))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of floor plan operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun floor-plan-sizes-match-p (floor-plans)
  "Determines whether the FLOOR-PLANS' room tally is equal among all
   members, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type (list-of Floor-Plan) floor-plans))
  (the boolean
    (not (null
      (apply #'=
        (mapcar #'floor-plan-size floor-plans))))))

;;; -------------------------------------------------------

(defun supputate-number-of-rooms-per-floor (floor-plans)
  "Returns the number of rooms per floor from the FLOOR-PLANS,
   concomitantly determining whether their room tally is exactly equal
   in all cases, on divergences signaling an error of an unspecified
   type."
  (declare (type (list-of Floor-Plan) floor-plans))
  (the (integer 0 *)
    (if floor-plans
      (or (and (floor-plan-sizes-match-p floor-plans)
               (floor-plan-size
                 (first floor-plans)))
          (error "Mismatch in number of floor rooms."))
      (error "Empty floor plan."))))

;;; -------------------------------------------------------

(defun check-floor-plan-flags (top-floor bottom-floor floor-plans)
  "Determines whether the FLOOR-PLANS' ground floor flags match their
   locations as allotted by the TOP-FLOOR and BOTTOM-FLOOR's ranges,
   returning on confirmation no value, otherwise signaling an error of
   an unspecified type."
  (declare (type non-negative-integer top-floor))
  (declare (type non-positive-integer bottom-floor))
  (declare (type (list-of Floor-Plan) floor-plans))
  (flet
      ((floor-plan-flag-matches-p (floor-number floor-plan)
        "Determines whether the FLOOR-PLAN's ground floor flag, in the
         context of its affiliation with the FLOOR-NUMBER, constitutes
         a covenable statement, returning on confirmation a ``boolean''
         value of ``T'', otherwise ``NIL''."
        (declare (type integer    floor-number))
        (declare (type Floor-Plan floor-plan))
        (the boolean
          (not (null
            (or (and (zerop floor-number)
                     (floor-plan-ground-floor-p floor-plan))
                (and (not (zerop floor-number))
                     (not (floor-plan-ground-floor-p floor-plan)))))))))
    (loop
      for floor-number
        of-type integer
        from    top-floor
        to      bottom-floor
      for floor-plan
        of-type Floor-Plan
        in      floor-plans
      unless (floor-plan-flag-matches-p floor-number floor-plan) do
        (error "The floor plan ~s, affiliated with the ~
                floor number ~d, carries an invalid ~
                ground floor flag."
          floor-plan floor-number)))
  (values))

;;; -------------------------------------------------------

(defun check-number-of-floor-plans (floor-plans number-of-floors)
  "Determines whether the FLOOR-PLANS matches in its tally the available
   NUMBER-OF-FLOORS, returning upon confirmation no value, otherwise
   signaling an error of an unspecified type."
  (declare (type (list-of Floor-Plan) floor-plans))
  (declare (type positive-integer     number-of-floors))
  (let ((number-of-floor-plans (length floor-plans)))
    (declare (type positive-integer number-of-floor-plans))
    (cond
      ((< number-of-floor-plans number-of-floors)
        (error "Too few floor plans communicated: ~
                Expected ~d, but obtained ~d."
          number-of-floors number-of-floor-plans))
      ((> number-of-floor-plans number-of-floors)
        (error "Too many floor plans communicated: ~
                Expected ~d, but obtained ~d."
          number-of-floors number-of-floor-plans))
      (T
        NIL)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "House-Plan".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass House-Plan ()
  ((top-floor
    :initarg       :top-floor
    :initform      (error "Missing top floor.")
    :type          non-negative-integer
    :documentation "The number of the top floor.")
   (bottom-floor
    :initarg       :bottom-floor
    :initform      (error "Missing bottom floor.")
    :type          non-positive-integer
    :documentation "The number of the bottom floor.")
   (number-of-rooms-per-floor
    :initarg       :number-of-rooms-per-floor
    :initform      (error "Missing number of rooms per floor.")
    :type          positive-integer
    :documentation "The tally of rooms per floor.")
   (number-of-floors
    :initarg       :number-of-floors
    :initform      (error "Missing number of floors.")
    :type          positive-integeer
    :documentation "The tally of floors comprising the builiding.")
   (rooms
    :initarg       :rooms
    :initform      (error "Missing rooms.")
    :type          (simple-array character (* *))
    :documentation "The entirety of the available rooms expressed in a
                    two-dimensional Cartesian ordonnance, with the
                    rows corresponding to the floors, and each row's
                    columns to the rooms alloted to the respective
                    storey."))
  (:documentation
    "The ``House-Plan'' type encapsulates the building's architecture,
     which comprises the floors, their enumeration, and rooms."))

;;; -------------------------------------------------------

(defun build-house-plan (top-floor bottom-floor floor-plans)
  "Creates and returns a new ``House-Plan'' for the building specified
   by its TOP-FLOOR, BOTTOM-FLOOR, and FLOOR-PLANS."
  (declare (type non-negative-integer top-floor))
  (declare (type non-positive-integer bottom-floor))
  (declare (type (list-of Floor-Plan) floor-plans))
  
  (check-floor-plan-flags top-floor bottom-floor floor-plans)
  
  (let ((number-of-rooms-per-floor
          (supputate-number-of-rooms-per-floor floor-plans))
        (number-of-floors
          (1+ (- top-floor bottom-floor))))
    (declare (type positive-integer number-of-rooms-per-floor))
    (declare (type positive-integer number-of-floors))
    
    (check-number-of-floor-plans floor-plans number-of-floors)
    
    (the House-Plan
      (make-instance 'House-Plan
        :top-floor                 top-floor
        :bottom-floor              bottom-floor
        :number-of-rooms-per-floor number-of-rooms-per-floor
        :number-of-floors          number-of-floors
        :rooms
          (make-array
            (list number-of-floors number-of-rooms-per-floor)
            :element-type     'character
            :initial-contents (mapcar #'floor-plan-rooms floor-plans)
            :adjustable       NIL
            :fill-pointer     NIL)))))

;;; -------------------------------------------------------

(defun valid-floor-number-p (house floor-number)
  "Determines whether the FLOOR-NUMBER represents a valid floor location
   in the HOUSE plan, returning in confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type House-Plan house))
  (declare (type integer    floor-number))
  (the boolean
    (not (null
      (<= (slot-value house 'bottom-floor)
          floor-number
          (slot-value house 'top-floor))))))

;;; -------------------------------------------------------

(defun validate-floor-number (house floor-number)
  "Determines whether the FLOOR-NUMBER represents a valid floor location
   in the HOUSE plan, returning on confirmation no value, otherwise
   signaling an error of an unspecified type."
  (declare (type House-Plan house))
  (declare (type integer    floor-number))
  (unless (valid-floor-number-p house floor-number)
    (error "The floor number ~d does not designate a valid storey ~
            in the building with the top floor of ~d and the ~
            bottom floor of ~d."
      floor-number
      (slot-value house 'top-floor)
      (slot-value house 'bottom-floor)))
  (values))

;;; -------------------------------------------------------

(defun get-index-of-floor (house floor-number)
  "Returns the zero-based index of the floor amenable to the
   FLOOR-NUMBER in the HOUSE plan's two-dimensional array, which
   designates the respective row.
   ---
   Effectively, the index constitutes a tantamount to the referenced
   floor's distance from the top floor."
  (declare (type House-Plan house))
  (declare (type integer    floor-number))
  (the fixnum
    (- (slot-value house 'top-floor)
       floor-number)))

;;; -------------------------------------------------------

(defun get-room-content (house floor-number room-number)
  "Returns the character stored in the one-based ROOM-NUMBER-th room
   located in the floor specified by the FLOOR-NUMBER of the HOUSE."
  (declare (type House-Plan       house))
  (declare (type integer          floor-number))
  (declare (type positive-integer room-number))
  (the character
    (aref
      (slot-value house 'rooms)
      (get-index-of-floor house floor-number)
      (1- room-number))))
