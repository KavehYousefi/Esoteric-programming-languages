;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the Seas memory component patefying in the
;; stack of signed integer objects.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory's integer stack.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Integer-Stack
  (:constructor prepare-an-empty-integer-stack ()))
  "The ``Integer-Stack'' class furnishes the entelechia of a stack
   dedicated to the castaldy of an arbitrary account of signed integer
   elements, its firmament derived from a singly linked list."
  (elements NIL :type (list-of integer) :read-only NIL))

;;; -------------------------------------------------------

(defun integer-stack-is-empty-p (stack)
  "Determines whether the integer STACK is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Integer-Stack stack))
  (the boolean
    (null
      (integer-stack-elements stack))))

;;; -------------------------------------------------------

(defun determine-whether-the-integer-stack-is-empty (stack)
  "Determines whether the integer STACK is empty, on confirmation
   signaling an error of the type ``Empty-Stack-Error''; otherwise
   returning the STACK in its verbatim, unmodulated form."
  (declare (type Integer-Stack stack))
  (the Integer-Stack
    (if (integer-stack-is-empty-p stack)
      (error 'Empty-Stack-Error :offended-stack stack)
      stack)))

;;; -------------------------------------------------------

(defun push-onto-the-integer-stack (stack new-element)
  "Pushes the NEW-ELEMENT onto the integer STACK and returns no value."
  (declare (type Integer-Stack stack))
  (declare (type integer       new-element))
  (push new-element
    (integer-stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun pop-from-the-integer-stack (stack)
  "Removes and returns the integer STACK's top element."
  (declare (type Integer-Stack stack))
  (the integer
    (pop
      (integer-stack-elements
        (determine-whether-the-integer-stack-is-empty stack)))))

;;; -------------------------------------------------------

(defun pop-a-twissel-from-the-integer-stack (stack)
  "Removes and returns the integer STACK's two top elements as the
   following values:
     (1) The erstwhile top element on the STACK.
     (2) The erstwhile second-to-top element on the STACK.
   ---
   If the STACK entails at the instant of this operation's instigation
   less than two elements, an error of the type ``Empty-Stack-Error''
   will be signaled during the illicit access' trial."
  (declare (type Integer-Stack stack))
  (the (values integer integer)
    (values
      (pop-from-the-integer-stack stack)
      (pop-from-the-integer-stack stack))))

;;; -------------------------------------------------------

(defun discard-the-integer-stack-top (stack)
  "Removes the integer STACK's top element and returns no value."
  (declare (type Integer-Stack stack))
  (pop-from-the-integer-stack stack)
  (values))

;;; -------------------------------------------------------

(defun reverse-the-integer-stack (stack)
  "Reverses the order of the elements under the integer STACK's castaldy
   and returns no value."
  (declare (type Integer-Stack stack))
  (setf (integer-stack-elements stack)
    (nreverse
      (integer-stack-elements stack)))
  (values))

;;; -------------------------------------------------------

(defun raise-the-integer-stack-bottom (stack)
  "Relocates the integer STACK's bottom element to the top position and
   returns no value.
   ---
   If the STACK is empty at the instant of this operation's instigation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (determine-whether-the-integer-stack-is-empty stack)
  (let ((bottom-element
          (first
            (last
              (integer-stack-elements stack)))))
    (declare (type integer bottom-element))
    (setf (integer-stack-elements stack)
      (nbutlast
        (integer-stack-elements stack)))
    (push-onto-the-integer-stack stack bottom-element))
  (values))

;;; -------------------------------------------------------

(defun lower-the-integer-stack-top (stack)
  "Relocates the integer STACK's top element to the bottom position and
   returns no value.
   ---
   If the STACK is empty at the instant of this operation's instigation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (let ((top-element (pop-from-the-integer-stack stack)))
    (declare (type integer top-element))
    (setf (integer-stack-elements stack)
      (cons top-element
        (integer-stack-elements stack))))
  (values))

;;; -------------------------------------------------------

(defun swap-the-integer-stack-top-elements (stack)
  "Swaps the integer STACK's two top elements' positions and returns no
   value.
   ---
   If the STACK entails at the instant of this operation's instigation
   less than two elements, an error of the type ``Empty-Stack-Error''
   will be signaled during the illicit access' trial."
  (declare (type Integer-Stack stack))
  (let ((erstwhile-top-element (pop-from-the-integer-stack stack))
        (new-top-element       (pop-from-the-integer-stack stack)))
    (declare (type integer erstwhile-top-element))
    (declare (type integer new-top-element))
    (push-onto-the-integer-stack stack erstwhile-top-element)
    (push-onto-the-integer-stack stack new-top-element))
  (values))

;;; -------------------------------------------------------

(defun duplicate-the-integer-stack-top (stack)
  "Duplicates the integer STACK's top element by pushing a copy thereof
   onto itself.
   ---
   If the STACK is empty at the instant of this operation's instigation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (let ((top-element (pop-from-the-integer-stack stack)))
    (declare (type integer top-element))
    (push-onto-the-integer-stack stack top-element)
    (push-onto-the-integer-stack stack top-element))
  (values))

;;; -------------------------------------------------------

(defun clear-the-integer-stack (stack)
  "Purges all elements from the integer STACK and returns no value."
  (declare (type Integer-Stack stack))
  (setf (integer-stack-elements stack) NIL)
  (values))
