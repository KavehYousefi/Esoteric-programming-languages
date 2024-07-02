;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ADxc", invented by the Esolang user "A" and presented on
;; August 19th, 2019, the programs of which operate on a stack of
;; real-valued numbers by adminiculum of instructions represented via
;; single characters.
;; 
;; 
;; Concept
;; =======
;; The ADxc programming language, its agnomination derived from a
;; quadruple of instructions entalented with superior chevisance,
;; applies itself to an theoretically infinitely large stack's
;; manipulation, the membership inwith the same exhausted by integers
;; and floating-point numbers, with each command token expressed by an
;; aefauld character's investement.
;; 
;; == EVERY COMMAND IS REPRESENTED BY A SINGLE CHARACTER ==
;; A sire to compendiousness in conception, the language apportions to
;; any of its 16 operative warklumes an aefauld character, the
;; competences thus reified in acquisition homologating insertions on
;; the program stack, basic arithmetics, and a bitwise left shift.
;; 
;; == PROGRAMS OPERATE ON A STACK OF REAL NUMBERS ==
;; The program memory's representation proceeds by means of a stack,
;; admitting an arbitrary account of real-valued elements, the same
;; thus span both integers and floats.
;; 
;; == THE PROGRAM TERMINATES WITH THE TOP ELEMENT'S OUTPUT ==
;; An action allocated to each program's desinence, the top stack
;; element, in the case of vacancy holding no purview over the memory,
;; displays in its verbatim numeric form on the standard output.
;; 
;; 
;; Architecture
;; ============
;; ADxc's program memory comprises an aefauld structure, the stack, this
;; being a salvatory to a hypothetically infinite enumeration of real
;; numbers, which is traditionally partitioned into integers and
;; floating-point objects.
;; 
;; 
;; Data Types
;; ==========
;; The common object type being the wide expanse of real values, a
;; twissel of numeric species bifurcates the language's type system,
;; namely, integers and floating-point number, both of unbridled
;; vastness in their magnitudes and the inclusion of both positive and
;; negative dispansion; for the float class, no impositions concerning
;; the precision has been levied.
;; 
;; == INSTRUCTIONS ==
;; Every instruction is represented by a single character, subjected to
;; a distinguishment in the case, if applicable.
;; 
;; == WHITESPACES ==
;; A compernage to the instruction tokens, whitespaces, in their diorism
;; enhalsing the space, horizontal tab, and newline entities, contribute
;; the sole admissible content.
;; 
;; == COMMENTS ==
;; No provisions for comments participate in this language rendition.
;; 
;; == GRAMMAR ==
;; The language donet shall be the cynosure of a formulation imbued with
;; paravaunt stringency by utilization of the Extended Backus-Naur Form
;; (EBNF):
;; 
;;   program    := { command | whitespace } ;
;;   command    := "A"
;;              |  "x"
;;              |  "0"
;;              |  "1"
;;              |  "2"
;;              |  "3"
;;              |  "4"
;;              |  "5"
;;              |  "6"
;;              |  "7"
;;              |  "8"
;;              |  "9"
;;              |  "D"
;;              |  "l"
;;              |  ";"
;;              |  "c"
;;              ;
;;   whitespace := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; ADxc instruction set enumerates a 16-membership cardinality, its
;; compass admissive of stack insertion facilities and such of basic
;; arithmetic and logical capacitation.
;; 
;; A program's conclusion always concurs with the top stack element's
;; display on the standard output, if such item's existence can be
;; ascertained.
;; 
;; == OVERVIEW ==
;; A cursory apercu's dation shall be a foundational mete of gnarity's
;; communication by the following table's adminiculum:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   A       | Pushes the value 0.5 onto the stack.
;;   ..................................................................
;;   x       | Pushes the value 1 onto the stack.
;;   ..................................................................
;;   0       | Pushes the value 0 onto the stack.
;;   ..................................................................
;;   1       | Pushes the value 1 onto the stack.
;;   ..................................................................
;;   2       | Pushes the value 2 onto the stack.
;;   ..................................................................
;;   3       | Pushes the value 3 onto the stack.
;;   ..................................................................
;;   4       | Pushes the value 4 onto the stack.
;;   ..................................................................
;;   5       | Pushes the value 5 onto the stack.
;;   ..................................................................
;;   6       | Pushes the value 6 onto the stack.
;;   ..................................................................
;;   7       | Pushes the value 7 onto the stack.
;;   ..................................................................
;;   8       | Pushes the value 8 onto the stack.
;;   ..................................................................
;;   9       | Pushes the value 9 onto the stack.
;;   ..................................................................
;;   D       | Pops the top element from the stack, multiplies it by
;;           | two, and pushes the product onto the stack.
;;           |---------------------------------------------------------
;;           | Upon the stack's vacancy at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   l       | Pops the top element from the stack, divides it by two,
;;           | and pushes the quotient onto the stack.
;;           |---------------------------------------------------------
;;           | Upon the stack's vacancy at this operation's invocation,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;;   ;       | Pops all elements from the stack, accumulates these, and
;;           | pushes the sum onto the stack.
;;           |---------------------------------------------------------
;;           | For an empty stack, no causatum is applied.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   if stack is not empty
;;           |     let sum <- 0
;;           |     while stack is not empty
;;           |       let currentElement <- pop from stack
;;           |       sum                <- sum + currentElement
;;           |     end while
;;           |     push sum
;;           |   end if
;;   ..................................................................
;;   c       | Pops the top element, here norned "offset", from the
;;           | stack, pops the new top element, "b", from the stack,
;;           | performs a bitwise left shift of "b" by the "offset",
;;           | and pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following holds:
;;           |   let offset <- round(pop from stack)
;;           |   let b      <- round(pop from stack)
;;           |   let result <- bitwise shift b by offset
;;           |   push result onto stack
;;           |---------------------------------------------------------
;;           | If "offset" constitutes a floating-point number, it is
;;           | rounded to the nearest integral value ere its
;;           | commission.
;;           |---------------------------------------------------------
;;           | If "b" constitutes a floating-point number, it is
;;           | rounded to the nearest integral value ere its
;;           | commission.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements,
;;           | an error of the type "EmptyStackError" is signaled.
;;   ..................................................................
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The simplicity's commorancy in the ADxc language disencumbers it from
;; mickleness in the points of anomalous inroads; maugre this status,
;; a few ambiguous points may be extrapolated from the protolog.
;; 
;; == HOW SHALL THE LANGUAGE RESPOND TO AN EMPTY STACK? ==
;; Several operations' dever relies on the existence of at least one
;; stack element as implicit argument; an absence as a contingency,
;; however, is destitute of any formalized attendence.
;; 
;; It has been adjudged to impose a dedicated error's infliction upon
;; the vacancy or paucity of stack elements, communicated via an
;; "EmptyStackError", and in its gravity sufficiently peisant so as to
;; terminate the program with immediate effect.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-07-02
;; 
;; Sources:
;;   [esolang2020ADxc]
;;   The Esolang contributors, "ADxc", May 27th, 2020
;;   URL: "https://esolangs.org/wiki/ADxc"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun real-valued-list-p (candidate)
  "Determines whether the CANDIDATE represents a list exclusively
   admissive of zero or more real-valued element, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (the boolean
    (not (null
      (and
        (listp candidate)
        (every #'realp
          (the list candidate)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stack ()
  "The ``stack'' type defines a list-based stack compact of zero or more
   numeric members desumed from the set of real values, which in its
   diorism amplects both the integral and floating-point species."
  '(satisfies real-valued-list-p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ADxc-Error (error)
  ()
  (:documentation
    "The ``ADxc-Error'' condition type edifies a common substratum for
     all conditions nuncupated to the communication of anomalous
     circumstances arising during the evaluation of an ADxc program."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (ADxc-Error simple-error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves to signal an
     anomalous situation begotten by the illicit attempt to pop from or
     peek into an empty stack.")
  (:default-initargs
    :format-control "Cannot pop from or peek into an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integral-number-p (number)
  "Determines whether the NUMBER represents an integral value, that is,
   either a veridical integer number or either a float or rational
   object whose fractional part equals zero (0), and as thus reckons as
   superfluous, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type real number))
  (the boolean
    (zerop
      (nth-value 1
        (truncate number)))))

;;; -------------------------------------------------------

(defun downgrade-to-integer-if-possible (number)
  "If the NUMBER represents either an integer object or a float or
   rational number with a zero-valued fractional part, returns a
   contingently rounded integral variant of the input; otherwise
   responds with the verbatim NUMBER."
  (declare (type real number))
  (the real
    (cond
      ((integerp number)
        number)
      ((integral-number-p number)
        (nth-value 0
          (round number)))
      (T
        number))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((stack
    :initform      NIL
    :accessor      interpreter-stack
    :type          stack
    :documentation "The program stack."))
  (:documentation
    "The ``Interpreter'' class is apportioned that dever to accompass
     actual efficacy to an ADxc program."))

;;; -------------------------------------------------------

(defun push-onto-stack (interpreter new-element)
  "Pushes the NEW-ELEMENT onto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type real        new-element))
  (push new-element
    (interpreter-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (interpreter)
  "Removes and returns the top element from the INTERPRETER's stack,
   if possible, otherwise, upon the storage's vacancy, signals an error
   of the type ``Empty-Stack-Error''."
  (declare (type Interpreter interpreter))
  (the real
    (if (interpreter-stack interpreter)
      (pop (interpreter-stack interpreter))
      (error 'Empty-Stack-Error))))

;;; -------------------------------------------------------

(defun peek-into-stack (interpreter)
  "Returns without removing the top element from the INTERPRETER's
   stack, or, upon its vacancy, signals an error of the type
   ``Empty-Stack-Error''."
  (declare (type Interpreter interpreter))
  (the real
    (if (interpreter-stack interpreter)
      (first (interpreter-stack interpreter))
      (error 'Empty-Stack-Error))))

;;; -------------------------------------------------------

(defun print-top-of-stack (interpreter)
  "Prints the top stack element of the INTERPRETER's stack, if not
   empty, to the standard output, in any case returning no value."
  (declare (type Interpreter interpreter))
  (when (interpreter-stack interpreter)
    (format T "~&~d"
      (first
        (interpreter-stack interpreter))))
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value.")
  
  ;; == Constant insertion operations. =================================
  
  (:method ((interpreter Interpreter) (command (eql #\A)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 0.5)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\x)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 1)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\0)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 0)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\1)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 1)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\2)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 2)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\3)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 3)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\4)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 4)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\5)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 5)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\6)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 6)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\7)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 7)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\8)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 8)
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\9)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter 9)
    (values))
  
  
  ;; == Arithmetic operations. =========================================
  
  (:method ((interpreter Interpreter) (command (eql #\D)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter
      (downgrade-to-integer-if-possible
        (* 2 (pop-from-stack interpreter))))
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\l)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (push-onto-stack interpreter
      (downgrade-to-integer-if-possible
        (/ (pop-from-stack interpreter) 2)))
    (values))
  
  (:method ((interpreter Interpreter) (command (eql #\;)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (when (interpreter-stack interpreter)
      (setf (interpreter-stack interpreter)
        (list
          (downgrade-to-integer-if-possible
            (reduce #'+
              (interpreter-stack interpreter))))))
    (values))
  
  
  ;; == Logical insertion operations. ==================================
  
  (:method ((interpreter Interpreter) (command (eql #\c)))
    (declare (type Interpreter interpreter))
    (declare (type character   command))
    (declare (ignore           command))
    (let ((shift-size (pop-from-stack interpreter)))
      (declare (type real shift-size))
      (push-onto-stack interpreter
        (ash
          (round (pop-from-stack interpreter))
          (round shift-size))))
    (values))
  
  
  ;; == Orra operations. ===============================================
  
  (:method ((interpreter Interpreter) (command character))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type character   command))
    (unless (whitespace-character-p command)
      (error "Unrecognized command \"~c\"." command))
    (values)))

;;; -------------------------------------------------------

(defun interpret-ADxc (code)
  "Interprets the piece of ADxc source CODE and returns no value."
  (declare (type string code))
  (let ((interpreter (make-instance 'Interpreter)))
    (declare (type Interpreter interpreter))
    (loop for token of-type character across code do
      (process-command interpreter token))
    (print-top-of-stack interpreter))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate and output the number four (4), employing multiplication,
;; bit shifting, accumulation, and division.
(interpret-ADxc "ADxc123;l")

;;; -------------------------------------------------------

;; Generate the sum of the first five terms of the geometric series,
;; that is:
;;   1 + 1/2 + 1/4 + 1/8 + 1/16 = 31/16 = 1.9375
(interpret-ADxc "1 A Al All Alll ;")
