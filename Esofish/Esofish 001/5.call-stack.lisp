;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements a "5" program's call stack, its dedication that
;; airted at the castaldy of the instruction pointer's (IP) locations
;; for the contingency of future relocations.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Call-Stack".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Call-Stack ()
  ((elements
    :initform      NIL
    :type          (list-of integer)
    :documentation "A list of the instruction pointer (IP) positions."))
  (:documentation
    "The ``Call-Stack'' class furnishes an implementation of a stack
     dedicated to the castaldy of instruction pointer (IP) positions."))

;;; -------------------------------------------------------

(defun make-an-empty-call-stack ()
  "Creates and returns a fresh, at its inchoation vacant,
   ``Call-Stack''."
  (the Call-Stack
    (make-instance 'Call-Stack)))

;;; -------------------------------------------------------

(defun push-onto-the-call-stack (stack new-element)
  "Inserts the NEW-ELEMENT at the call STACK's top position and returns
   no value."
  (declare (type Call-Stack stack))
  (declare (type integer    new-element))
  (push new-element
    (slot-value stack 'elements))
  (values))

;;; -------------------------------------------------------

(defun pop-from-the-call-stack (stack)
  "Removes and returns the top element from the call STACK.
   ---
   If the STACK is empty at the instant of this program's invocation,
   an error of the type ``Empty-Call-Stack-Error'' is signaled."
  (declare (type Call-Stack stack))
  (the integer
    (or (pop (slot-value stack 'elements))
        (error 'Empty-Call-Stack-Error :offended-stack stack))))
