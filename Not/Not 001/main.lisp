;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Not", invented by the Esolang user "Gggfr" and presented
;; on July 18th, 2024, its indicium appertaining to the operation on a
;; stack of strings which merely admit the symbol "1" in an arbitrary
;; positive account of frequencies, ligated into a coefficiency with an
;; infinite looping construct as the aefauld control flow mechanism.
;; 
;; 
;; Concept
;; =======
;; The Not programming language's foundry proceeds from the manipulation
;; of a stack acting as a salvatory to string objects, their composition
;; a catena of one or more instances of the digit one (1), enjoying an
;; amplification in this operation by perpetual loops.
;; 
;; 
;; Instructions
;; ============
;; A septuple cardinality governs the Not programming language's
;; instruction set, in its amplectation included such to push the
;; constant one (1), concatenate the stack's top elements, entertain
;; outputs, and engage in a perpetual loop.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer a cursory mete of gnarity
;; anent the language's operative features:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   1       | Pushes the digit 1 unto the stack.
;;   ..................................................................
;;   +       | Pops the top stack element, "left", and the new top
;;           | element "right", concatenates these two elements as
;;           | strings with "left" providing the sinistral and "right"
;;           | the dextral moiety, and pushes the result onto the
;;           | stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following holds:
;;           |   let left        <- pop from stack
;;           |   let right       <- pop from stack
;;           |   let combination <- append right to left
;;           |   push combination onto stack
;;           |---------------------------------------------------------
;;           | If the stack cannot affort at least two elements at the
;;           | instant of this operation's invocation, an error of the
;;           | type "EmptyStackError" is signaled during the abortive
;;           | attempt.
;;   ..................................................................
;;   =       | Duplicates the top stack element by pushing a copy of
;;           | the same unto the stack.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   #       | Pops the top stack element and prints the same to the
;;           | standard output.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   N       | Prints a newline character to the standard output.
;;   ..................................................................
;;   [       | Commences an infinite loop whose closure is demarcated
;;           | the matching "]".
;;           |---------------------------------------------------------
;;           | If no matching "]" token can be detected in a later
;;           | position of the program, an error of the type
;;           | "UnmatchedLoopEndpointsError" is signaled.
;;   ..................................................................
;;   ]       | Designates the desinence of an infinite loop instigated
;;           | by the prevenient matching "[".
;;           |---------------------------------------------------------
;;           | If no matching "[" token can be detected, an error of
;;           | the type "UnmatchedLoopEndpointsError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand has been developed in the programming
;; language Common Lisp, operating immediately on the input Not code in
;; its raw string form.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-09-02
;; 
;; Sources:
;;   [esolang2024Not]
;;   The Esolang contributors, "Not", September 1st, 2024
;;   URL: "https://esolangs.org/wiki/Not"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type (type-name
                               (candidate-name &rest lambda-list)
                               &body body)
  "Defines a derived type norned by the TYPE-NAME, its formal parameters
   appropriated verbatim from the LAMBDA-LIST, the subject of the
   docimasy being agnominated by the CANDIDATE-NAME, evaluates the BODY
   forms with access to the candidate, expecting the desinent BODY form
   to return in its primary value a non-``NIL'' response if the probed
   object shall be interpreted as covenable with the predicate,
   otherwise imposing a ``NIL'' result.
   ---
   The first BODY form, upon its resolution to a string object, will be
   adhibited a construe the established type's documentation string, and
   will be appropriated for this particular purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (declare (ignorable   ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (or (eq element-type '*)
        (every
          #'(lambda (current-element)
              (declare (type T current-element))
              (typep current-element element-type))
          (the list candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Not-Error (simple-error)
  ()
  (:documentation
    "The ``Not-Error'' condition type furnishes a substratum for all
     conditions dedicated to the communication of anomalous situations
     instigated during a Not program's evaluation."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Not-Error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves to communicate the
     aborted attempt to retrieve or delete an empty Not program stack's
     top element.")
  (:default-initargs
    :format-control "Cannot peek into or pop from an empty stack."))

;;; -------------------------------------------------------

(define-condition Unmatched-Loop-Endpoints-Error (Not-Error)
  ((offending-position
    :initarg       :offending-position
    :initform      (error "Missing offending position.")
    :reader        unmatched-loop-endpoints-error-offending-position
    :type          fixnum
    :documentation "The position in the Not program at which a loop
                    boundary without match wones.")
   (offending-side
    :initarg       :offending-side
    :initform      (error "Missing offending side.")
    :reader        unmatched-loop-endpoints-error-offending-side
    :type          (member :start :end)
    :documentation "Determines whether the OFFENDING-POSITION affiliates
                    with a loop start or end token."))
  (:documentation
    "The ``Unmatched-Loop-Endpoints-Error'' condition type serves in
     the communication of an anomalous situation whose etiology
     appertains an unmatched loop end point."))

;;; -------------------------------------------------------

(define-condition Incomplete-Loop-Error (Not-Error)
  ()
  (:documentation
    "The ``Incomplete-Loop-Error'' condition type serves to signal that
     an attempt has been committed to request some property of a loop
     definition not yet delivered to patration."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signal-empty-stack-error ()
  "Signals an error of the type ``Empty-Stack-Error'' in order to
   communicate the attempt to peek into or pop from an empty stack."
  (error 'Empty-Stack-Error))

;;; -------------------------------------------------------

(defun signal-unmatched-loop-endpoints-error (offending-position
                                              offending-side)
  "Signals an error of the type ``Unmatched-Loop-Endpoints-Error'' in
   order to communicate an unmatched loop endpoint along the
   OFFENDING-SIDE at the OFFENDING-POSITION in a Not program."
  (declare (type fixnum               offending-position))
  (declare (type (member :start :end) offending-side))
  (error 'Unmatched-Loop-Endpoints-Error
    :offending-position offending-position
    :offending-side     offending-side
    :format-control     "Unmatched loop ~(~a~) point at position ~d."
    :format-arguments
      (list
        (symbol-name offending-side)
        offending-position)))

;;; -------------------------------------------------------

(defun signal-incomplete-loop-error ()
  "Signals an error of the type ``Incomplete-Loop-Error''."
  (error 'Incomplete-Loop-Error))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of iteration bournes validator.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-loop-bournes (code)
  "Analyzes the piece of Not source CODE anenst its loop boundaries'
   consistency, on confirmation returning no value; otherwise, upon an
   unmatched march's detection, signals an error of the type
   ``Unmatched-Loop-Endpoints-Error''."
  (declare (type string code))
  (let ((loop-start-points NIL))
    (declare (type (list-of fixnum) loop-start-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from 0 by 1
      
      if (char= token #\[) do
        (push position loop-start-points)
      else if (char= token #\]) do
        (if loop-start-points
          (pop loop-start-points)
          (signal-unmatched-loop-endpoints-error position :end))
      end
      
      finally
        (when loop-start-points
          (signal-unmatched-loop-endpoints-error
            (first loop-start-points)
            :start))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program stack.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Stack
  (:constructor make-empty-stack ()))
  "The ``Stack'' class furnishes an implementation of the Not program
   stack as a last in, first out storage of strings."
  (elements NIL :type (list-of string) :read-only NIL))

;;; -------------------------------------------------------

(defun peek-into-stack (stack)
  "Returns without removing the STACK's top element, if such exists;
   otherwise signals an error of the type ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the string
    (or (first (stack-elements stack))
        (signal-empty-stack-error))))

;;; -------------------------------------------------------

(defun pop-from-stack (stack)
  "Removes and returns the STACK's top element, if such exists;
   otherwise signals an error of the type ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the string
    (or (pop (stack-elements stack))
        (signal-empty-stack-error))))

;;; -------------------------------------------------------

(defun push-onto-stack (stack new-element)
  "Inserts the NEW-ELEMENT at the STACK's top position and returns no
   value."
  (declare (type Stack  stack))
  (declare (type string new-element))
  (push new-element
    (stack-elements stack))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of loop state.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Loop-State
  (:constructor make-inactive-loop-state ()))
  "The ``Loop-State'' class serves in the castaldy of a Not program's
   active loop construct's state."
  (iterating-p NIL :type boolean :read-only NIL)
  (start-point 0   :type fixnum  :read-only NIL))

;;; -------------------------------------------------------

(defun program-loops-p (state)
  "Determines whether the loop STATE wones in a looping mode, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Loop-State state))
  (the boolean
    (loop-state-iterating-p state)))

;;; -------------------------------------------------------

(defun get-loop-start-point (state)
  "Returns the loop start point affiliated with the loop STATE, or
   signals an error of the type ``Incomplete-Loop-Error'' upon its
   activation flag's disrespondency."
  (declare (type Loop-State state))
  (the fixnum
    (if (loop-state-iterating-p state)
      (loop-state-start-point state)
      (signal-incomplete-loop-error))))

;;; -------------------------------------------------------

(defun start-loop (state new-start-point)
  "Marks the loop STATE's as looping by setting its start point to the
   NEW-START-POINT."
  (declare (type Loop-State state))
  (declare (type fixnum     new-start-point))
  (psetf (loop-state-start-point state) new-start-point
         (loop-state-iterating-p state) T)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merge-strings (left-moiety right-moiety)
  "Appends the RIGHT-MOIETY to the LEFT-MOIETY and returns the resulting
   string.
   ---
   Neither the LEFT-MOIETY nor the RIGHT-MOIETY will be modified."
  (declare (type string left-moiety))
  (declare (type string right-moiety))
  (the string
    (format NIL "~a~a" left-moiety right-moiety)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Not (code)
  "Interprets the piece of Not source CODE and returns no value."
  (declare (type string code))
  (validate-loop-bournes code)
  (let ((ip         0)
        (stack      (make-empty-stack))
        (loop-state (make-inactive-loop-state)))
    (declare (type fixnum     ip))
    (declare (type Stack      stack))
    (declare (type Loop-State loop-state))
    (symbol-macrolet ((current-token
                        (the character
                          (char code ip))))
      (declare (type character current-token))
      (loop while (< ip (length code)) do
        (case current-token
          ((#\Newline #\Space #\Tab)
            NIL)
          (#\1
            (push-onto-stack stack "1"))
          (#\+
            (push-onto-stack stack
              (merge-strings
                (pop-from-stack stack)
                (pop-from-stack stack))))
          (#\=
            (push-onto-stack stack
              (peek-into-stack stack)))
          (#\#
            (format T "~a"
              (pop-from-stack stack))
            (finish-output))
          (#\N
            (terpri)
            (finish-output))
          (#\[
            (start-loop loop-state ip))
          (#\]
            (setf ip
              (get-loop-start-point loop-state)))
          (otherwise
            (error "Invalid symbol \"~c\" at position ~d."
              current-token ip)))
        (incf ip))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perpetually print instances of the digit one (1), each line entailing
;; an additional tally thereof as juxtaposed to its prevenient row,
;; commencing with the incipial output of an aefauld "1".
(interpret-Not "1#N1[1+=#N]")
