;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the Seas's programs' instruction pointer (IP),
;; nevened here, in one conspectuity's ludibund adhibition upon the
;; general subject, the \"submarine\", forecause its operative bailiwick
;; does not levitate aboon the program grid "sea" vale's visceral's.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the submarine (instruction pointer).       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Submarine
  (:constructor hire-a-submarine ()))
  "The ``Submarine'' class applies itself to the representation of a
   Seas program's instruction pointer (IP) in the guise of a
   \"submarine\"."
  (position  (specify-a-location 0 0) :type location  :read-only NIL)
  (direction :right                   :type direction :read-only NIL))

;;; -------------------------------------------------------

(defun move-the-submarine (submarine)
  "Moves the SUBMARINE one step into its currently specified direction
   and returns no value."
  (declare (type Submarine submarine))
  (setf (submarine-position submarine)
    (translate-the-location-into
      (submarine-position  submarine)
      (submarine-direction submarine)))
  (values))

;;; -------------------------------------------------------

(defun redirect-the-submarine-on-an-ascending-mirror (submarine)
  "Adjusts the SUBMARINE's direction in response to its collision with
   a mirror thilk ascends in a sinistrodextral mode and returns no
   value."
  (declare (type Submarine submarine))
  (setf (submarine-direction submarine)
    (apply-an-ascending-mirror-to-the-direction
      (submarine-direction submarine)))
  (values))

;;; -------------------------------------------------------

(defun redirect-the-submarine-on-a-descending-mirror (submarine)
  "Adjusts the SUBMARINE's direction in response to its collision with
   a mirror thilk descends in a sinistrodextral mode and returns no
   value."
  (declare (type Submarine submarine))
  (setf (submarine-direction submarine)
    (apply-a-descending-mirror-to-the-direction
      (submarine-direction submarine)))
  (values))

;;; -------------------------------------------------------

(defun turn-the-submarine-to (submarine new-direction)
  "Changes the SUBMARINE's orientation to the NEW-DIRECTION and returns
   no value."
  (declare (type Submarine submarine))
  (declare (type direction new-direction))
  (setf (submarine-direction submarine) new-direction)
  (values))
