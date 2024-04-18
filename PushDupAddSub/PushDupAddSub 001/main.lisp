;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "PushDupAddSub", invented by the Esolang user
;; "ChuckEsoteric08" and presented on December 17th, 2023, invested with
;; those competences requisite to the manipulation of an integer-valued
;; stack, in conjunction with a conditional iterance construct.
;; 
;; 
;; Concept
;; =======
;; The PushDupAddSub programming language's foundry ensues from its
;; sextuple instruction set, the same permits the manipulation of a
;; stack accommodating an arbitrary tally of signed integer numbers.
;; 
;; == THE MEMORY: A STACK OF SIGNED INTEGERS ==
;; The language's singular storage component is realized in a stack of
;; integers, bourneless both in its capacity, as well as in the
;; admission of the maintained elements' signum and magnitude.
;; 
;; == INSTRUCTIONS: SINGLE SYMBOLS ==
;; The six operations admitted to a programmer's avail ostend their
;; services by an aefauld symbol's deployment each.
;; 
;; 
;; Instructions
;; ============
;; A sextuple cardinality governs the PushDupAddSub programming
;; language's operative department, admitting basic arithmetics, the
;; rudimentary memory management, appertaing to the stack, and an
;; aefauld iterance construct.
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity's conveyance shall be the following
;; apercu's dever.
;; 
;; Please note the designation of succedaneous segments in the table by
;; a catena of underlining asterisks ("*"), such parcels are expected to
;; be superseded by actual PushDupAddSub code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   1        | Pushes the number 1 onto the stack.
;;   ..................................................................
;;   :        | Duplicates the top stack element on the stack.
;;   ..................................................................
;;   +        | Pops the top stack element, "a", and the new top
;;            | element "b", supputates the sum a + b, and pushes the
;;            | result onto the stack.
;;            |--------------------------------------------------------
;;            | In a pseudocode diction it holds:
;;            |   let a <- pop from stack
;;            |   let b <- pop from stack
;;            |   let c <- a + b
;;            |   push c
;;   ..................................................................
;;   -        | Pops the top stack element, "a", and the new top
;;            | elements "b", supputates the difference b - a, and
;;            | pushes the result onto the stack.
;;            |--------------------------------------------------------
;;            | In a pseudocode diction it holds:
;;            |   let a <- pop from stack
;;            |   let b <- pop from stack
;;            |   let c <- b - a
;;            |   push c
;;   ..................................................................
;;   /        | Pops the top stack element, "a", and the new top
;;            | element "b", supputates both the quotient c = b / a and
;;            | the remainder d = b modulo a, pushes the remainder d
;;            | onto the stack, and subsequently the quotient c.
;;            |--------------------------------------------------------
;;            | In a pseudocode diction it holds:
;;            |   let a <- pop from stack
;;            |   let b <- pop from stack
;;            |   let c <- b /      a
;;            |   let d <- b modulo a
;;            |   push d
;;            |   push c
;;            |--------------------------------------------------------
;;            | Please heed the order of the result transfer, with the
;;            | quotient ultimately being empight aboon the remainder,
;;            | that is, by a visual illustration:
;;            | 
;;            |           Stack content
;;            |           ==============
;;            |   top>>>> | b / a      |
;;            |           | b modulo a |
;;            |           | ...        |
;;            |   bottom> | ...        |
;;            |           +------------+
;;   ..................................................................
;;   [ code ] | Repeats, while the top stack element, which is peeked,
;;     ****   | not removed, equals zero (0), the {code}.
;;            |--------------------------------------------------------
;;            | {code} must be a sequence of zero or more instructions.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-07
;; 
;; Sources:
;;   [esolang2023pushdupaddsub]
;;   The Esolang contributors, "PushDupAddSub", December 17th, 2023
;;   URL: "https://esolangs.org/wiki/PushDupAddSub"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a new derived type whose agnomination ensues from the
   TYPE-NAME, while its formal parameters are appropriated from the
   LAMBDA-LIST, exercising in its BODY forms a docimasy of the probed
   object as stevened by the CANDIDATE-VARIABLE, the desinent form's
   primary value, if returning a generalized boolean \"true\" value,
   ascertains its eligibility, while a \"false\" response refutes the
   same."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(or (and (stringp (first body)) (pop body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type stack-of (candidate &optional (element-type T))
  "The ``stack-of'' type defines a list-based stack composed of zero or
   more elements conforming to the ELEMENT-TYPE, the default chosen as
   the comprehensive ``T''."
  (flet ((matches-element-type-p (element)
          "Determines whether the ELEMENT conforms to the imposed
           ELEMENT-TYPE, returning on confirmation a ``boolean'' value
           of ``T'', otherwise ``NIL''."
          (declare (type T element))
          (the boolean
            (not (null
              (typep element element-type))))))
    (and
      (listp candidate)
      (every #'matches-element-type-p
        (the list candidate)))))

;;; -------------------------------------------------------

(deftype program-stack ()
  "The ``program-stack'' defines a list-based stack of signed integer
   numbers covenable for the requisites of a PushDupAddSub program."
  '(stack-of integer))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   T)
                                                 (value-type T))
  "The ``hash-table-of'' type defines a hash table the componency of
   which enumerates an arbitrary membership of entries, each key among
   the same complies with the KEY-TYPE and affiliates with a value
   subsuming into the VALUE-TYPE, for both imposed the comprehensive
   default of ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for probed-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value probed-value)
      always
        (and (typep probed-key   key-type)
             (typep probed-value value-type)))))

;;; -------------------------------------------------------

(deftype operation ()
  "The ``operation'' type enumerates the recognized variants of
   PushDupAddSub instructions."
  '(member
    :push-1
    :add
    :subtract
    :divide
    :duplicate
    :start-loop
    :end-loop))

;;; -------------------------------------------------------

(deftype pushdupaddsub-program ()
  "The ``pushdupaddsub-program'' type defines an executable
   PushDupAddSub program as a one-dimensional simple array of
   ``operation'' objects."
  '(simple-array operation (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral association betwixt a
   loop's start and end point by adminiculum of the respective locations
   inside of the PushDupAddSub program, realized in a hash table whose
   keys and values both represent the positions as fixnum numbers."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-operations (code)
  "Extracts from the piece of PushDupAddSub source CODE the incorporated
   operations and returns a one-dimensional simple array comprehending
   these."
  (declare (type string code))
  (the pushdupaddsub-program
    (coerce
      (loop
        for token    of-type character across code
        and position of-type fixnum    from   0 by 1
        append
          (case token
            (#\1 (list :push-1))
            (#\+ (list :add))
            (#\- (list :subtract))
            (#\/ (list :divide))
            (#\: (list :duplicate))
            (#\[ (list :start-loop))
            (#\] (list :end-loop))
            ((#\Newline #\Space #\Tab) NIL)
            (otherwise
              (error "Invalid character \"~c\" at position ~d."
                token position))))
      '(simple-array operation (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (program)
  "Computes and returns for the PushDupAddSub PROGRAM a jump table which
   associates the loop start and end point locations in a bilateral
   fashion."
  (declare (type pushdupaddsub-program program))
  (let ((jump-table   (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table        jump-table))
    (declare (type (stack-of fixnum) start-points))
    (loop
      for operation of-type operation across program
      and position  of-type fixnum    from   0 by 1
      
      if (eq operation :start-loop) do
        (push position start-points)
      else if (eq operation :end-loop) do
        (if start-points
          (let ((start-point (pop start-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf (gethash start-point jump-table) end-point
                   (gethash end-point   jump-table) start-point))
          (error "Unmatched loop end point at position ~d." position))
      end
      
      finally
        (when start-points
          (error "Unmatched loop start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points) start-points)))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-stack (stack)
  "Prints the state of the program STACK to the standard output and
   returns no value."
  (declare (type program-stack stack))
  (format T "~&[Top> ~{~d~^, ~} <Bottom]" stack)
  (values))

;;; -------------------------------------------------------

(defun execute-program (program)
  "Executes the PushDupAddSub PROGRAM and returns no value.
   ---
   A remedy to the PushDupAddSub language's lacuna regarding an output
   facility, this process prints at each secle's patration the entirety
   of the stack's state to the standard output, succeeded by a delay of
   approximately 0.5 seconds in order to entertain an encheson for the
   display's perusal."
  (declare (type pushdupaddsub-program program))
  (let ((ip         0)
        (jump-table (compute-jump-table program))
        (stack      NIL))
    (declare (type fixnum        ip))
    (declare (type jump-table    jump-table))
    (declare (type program-stack stack))
    (flet
        ((push-on-stack (element)
          "Pushes the ELEMENT unto the STACK and returns no value."
          (declare (type integer element))
          (push element stack)
          (values))
         
         (pop-from-stack ()
          "Removes the STACK's top element and returns it, or, upon
           its vacancy, signals an error of an unspecified type."
          (the integer
            (or (and stack (pop stack))
                (error "Cannot pop from an empty stack.")))))
      
      (symbol-macrolet
          ((program-completed-p
            (the boolean
              (not (array-in-bounds-p program ip))))
           (current-operation
            (the operation
              (aref program ip)))
           (top-of-stack
            (the integer
              (or (and stack (first stack))
                  (error "Cannot peek into an empty stack.")))))
        (declare (type boolean   program-completed-p))
        (declare (type operation current-operation))
        (declare (type integer   top-of-stack))
        
        (loop until program-completed-p do
          (case current-operation
            (:push-1
              (push-on-stack 1))
            
            (:add
              (push-on-stack
                (+ (pop-from-stack)
                   (pop-from-stack))))
            
            (:subtract
              (let ((subtrahend (pop-from-stack))
                    (minuend    (pop-from-stack)))
                (declare (type integer subtrahend))
                (declare (type integer minuend))
                (push-on-stack
                  (- minuend subtrahend))))
            
            (:divide
              (let ((divisor  (pop-from-stack))
                    (dividend (pop-from-stack)))
                (declare (type integer divisor))
                (declare (type integer dividend))
                (multiple-value-bind (quotient remainder)
                    (floor dividend divisor)
                  (declare (type integer quotient))
                  (declare (type integer remainder))
                  (push-on-stack remainder)
                  (push-on-stack quotient))))
            
            (:duplicate
              (push-on-stack top-of-stack))
            
            (:start-loop
              (unless (zerop top-of-stack)
                (setf ip
                  (gethash ip jump-table))))
            
            (:end-loop
              (when (zerop top-of-stack)
                (setf ip
                  (gethash ip jump-table))))
            
            (otherwise
              (error "Unrecognized operation ~s at position ~d."
                current-operation ip)))
          
          (print-stack stack)
          (finish-output)
          (sleep 0.5)
          
          (incf ip)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-PushDupAddSub (code)
  "Interprets the piece of PushDupAddSub source CODE and returns no
   value."
  (declare (type string code))
  (execute-program
    (extract-operations code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Push the value one (1), increment by four (4) to the new state (5),
;; and decrement it by one (1) back to four (4).
(interpret-PushDupAddSub
  "1
   1+1+1+1+
   ::/-+")

;;; -------------------------------------------------------

;; Counter which infinitely increases by one.
;; 
;; The stack, during the while loop's first step, always contains the
;; current counter state as its top element.
(interpret-PushDupAddSub
  "1
   1:1/-+
   [
     +1+
     1:1/-+
   ]")
