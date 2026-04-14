;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the operations devoted to the accommodation of
;; ponibility and orientation via a set of eligible location and
;; direction conspection and manipulation facilities.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the directional operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-an-ascending-mirror-to-the-direction (direction)
  "Returns the airt obtained by a collision of the DIRECTION with a
   mirror thilk ascends in a sinistrodextral mode.
   ---
   By a visualization's adminiculum, the respective mirror's design
   might be limned in the following guise;
     /"
  (declare (type direction direction))
  (the direction
    (case direction
      (:left     :down)
      (:right    :up)
      (:up       :right)
      (:down     :left)
      (otherwise (error "Invalid direction: ~s." direction)))))

;;; -------------------------------------------------------

(defun apply-a-descending-mirror-to-the-direction (direction)
  "Returns the airt obtained by a collision of the DIRECTION with a
   mirror which descends in a sinistrodextral mode.
   ---
   By a visualization's adminiculum, the respective mirror's design
   might be limned in the following guise;
     \\"
  (declare (type direction direction))
  (the direction
    (case direction
      (:left     :up)
      (:right    :down)
      (:up       :left)
      (:down     :right)
      (otherwise (error "Invalid direction: ~s." direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the location operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun specify-a-location (x y)
  "Creates and returns a fresh ``location'' whose horizontal coordinate
   is desumed from X, and whose vertical positioning constitutes the
   Y parameter's dation."
  (declare (type fixnum x))
  (declare (type fixnum y))
  (the location
    (complex x y)))

;;; -------------------------------------------------------

(defun disassemble-the-location (location)
  "Returns the LOCATION's coordinates and two values:
     (1) The LOCATION's x-coordinate.
     (2) The LOCATION's y-coordinate."
  (declare (type location location))
  (the (values fixnum fixnum)
    (values
      (realpart location)
      (imagpart location))))

;;; -------------------------------------------------------

(defun get-the-location-y-coordinate (location)
  "Returns the LOCATION's y-coordinate."
  (declare (type location location))
  (the fixnum
    (imagpart location)))

;;; -------------------------------------------------------

(defun translate-the-location-by (location x-offset y-offset)
  "Creates and returns a fresh ``location'' obtained by a relative
   motation along the horizontal axis by the X-OFFSET and along the
   vertical axis by the Y-OFFSET."
  (declare (type location location))
  (declare (type fixnum   x-offset))
  (declare (type fixnum   y-offset))
  (the location
    (complex
      (+ (realpart location) x-offset)
      (+ (imagpart location) y-offset))))

;;; -------------------------------------------------------

(defun translate-the-location-into (location direction)
  "Creates and returns a fresh ``location'' obtained by a relative
   motation into the DIRECTION by one step."
  (declare (type location  location))
  (declare (type direction direction))
  (the location
    (case direction
      (:left     (translate-the-location-by location -1  0))
      (:right    (translate-the-location-by location +1  0))
      (:up       (translate-the-location-by location  0 -1))
      (:down     (translate-the-location-by location  0 +1))
      (otherwise (error "Invalid direction: ~s." direction)))))
