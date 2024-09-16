;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Mountain", invented by the Esolang user "ChuckEsoteric08"
;; and presented on November 30th, 2023, the kenspeckle proprium of
;; which ostends in the weftage of its code, limning the simulacrum of
;; mountains, while its manipulations relate to a stack of signed
;; integer numbers.
;; 
;; 
;; Concept
;; =======
;; The Mountain programming language admits to its design as effective
;; constituents merely a twissel of characters suggestive in their shape
;; of mountains, namely "ʌ" and "ʍ", producing all causata in relation
;; to a stack composed of signed integer elements.
;; 
;; == PROGRAMS LIMN MOUNTAINS ==
;; The symbols "ʌ" and "ʍ" are apportioned with the only expressive
;; effect in a program, while adscititious characters may be inserted,
;; but appropriate merely a commentary rank.
;; 
;; The following tabular exposition shall impart further gnarity with
;; the two significant symbols, apprizing about their Unicode code
;; points, as well as their official identifications.
;; 
;;   --------------------------------------------------
;;   Symbol | Code point | Name
;;   -------+------------+-----------------------------
;;   ʌ      | U+028C     | Latin Small Letter Turned V
;;   ..................................................
;;   ʍ      | U+028D     | Latin Small Letter Turned W
;;   --------------------------------------------------
;; 
;; Whitespaces, maugre tolerance's adhibition, should be subjected to
;; banishment in a pursuit to endow programs with augmented aesthetics.
;; 
;; == PROGRAMS OPERATE ON A STACK OF INTEGER NUMBERS ==
;; A stack of hypothetically infinite expanse governs the data
;; compartment of the Mountain programming language, homologating the
;; storage of signed integers, without any bournes along both extrema.
;; 
;; 
;; Instructions
;; ============
;; Each member of the octuple instruction set establishes a conjuncture
;; of "ʌ" and "ʍ" instances, a preponderance among the same eschews
;; arguments, exempted from this species is the push instruction
;; "ʌʍ...ʍ".
;; 
;; Unrecognized operations and symbols do not partake of any causata's
;; induction.
;; 
;; == OVERVIEW ==
;; A compendious apercu's dever shall be the requisite acquaintance's
;; dation anent the language's operational features.
;; 
;; Please heed that succedaneous segments are amplected by a jumelle of
;; braces, "{...}", the entire compound of which ought to be superseded
;; by actual Mountain code in the progrma's ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Mnemonic | Effect
;;   ------------+----------+------------------------------------------
;;   ʌʍ{number}ʍ | psh      | Pushes the positive integer {number},
;;               |          | represented by the tally of "ʌ" tokens,
;;               |          | unto the program stack.
;;               |          |------------------------------------------
;;               |          | {number} must be a sequence of one or
;;               |          | more "ʌ" characters, their tally
;;               |          | determines the positive integer value to
;;               |          | push unto the stack.
;;   ..................................................................
;;   ʌʍʍ         | neg      | Negates the top stack element.
;;               |          |------------------------------------------
;;               |          | If the stack is prior to this operation,
;;               |          | an error of the type "EmptyStackError" is
;;               |          | signaled.
;;   ..................................................................
;;   ʌʌʍ         | jmp      | Pops the three top stack elements "n",
;;               |          | "a", and "b".
;;               |          |   - If it holds: (a = b) and (n > 0),
;;               |          |     redirects the instruction pointer
;;               |          |     (IP) to the one-based instruction.
;;               |          |   - If it holds: (a = b) and (n <= 0),
;;               |          |     immediately terminates the program.
;;               |          |   - If it holds: (a != b), does not
;;               |          |     accompass any causatum.
;;               |          |------------------------------------------
;;               |          | Please note that the instructions in a
;;               |          | Mountain program are enumerated starting
;;               |          | with the index one (1).
;;               |          |------------------------------------------
;;               |          | If an insufficient number of elements
;;               |          | reside on the stack prior to this
;;               |          | operation, an error of the type
;;               |          | "EmptyStackError" is signaled.
;;   ..................................................................
;;   ʌʌʍʍ        | add      | Pops the two top stack elements "a" and
;;               |          | "b", calculates the sum, and pushes the
;;               |          | same unto the stack.
;;               |          |------------------------------------------
;;               |          | If an insufficient number of elements
;;               |          | reside on the stack prior to this
;;               |          | operation, an error of the type
;;               |          | "EmptyStackError" is signaled.
;;   ..................................................................
;;   ʌʍʍʍ        | rot      | Removes the stack's bottom element and
;;               |          | pushes it unto the top.
;;               |          |------------------------------------------
;;               |          | If the stack is prior to this operation,
;;               |          | an error of the type "EmptyStackError" is
;;               |          | signaled.
;;   ..................................................................
;;   ʌʌʌʍ        | dup      | Duplicates the top stack element.
;;               |          |------------------------------------------
;;               |          | If the stack is prior to this operation,
;;               |          | an error of the type "EmptyStackError" is
;;               |          | signaled.
;;   ..................................................................
;;   ʌʌʍʍʍ       | out      | Pops the top stack element and prints the
;;               |          | character whose ASCII code equals the
;;               |          | same to the standard output.
;;               |          |------------------------------------------
;;               |          | If the stack is prior to this operation,
;;               |          | an error of the type "EmptyStackError" is
;;               |          | signaled.
;;   ..................................................................
;;   ʌʍʍʍʍ       | inp      | Queries the standard input for an ASCII
;;               |          | character and pushes its ASCII code unto
;;               |          | the stack.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-06
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023Mountain]
;;   The Esolang contributors, "Mountain", November 30th, 2023
;;   URL: "https://esolangs.org/wiki/Mountain"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integral object greater than
   zero (0), but without any natural upper extremum, that is, an
   occupant of the integer interval [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral object greater
   than or equal to zero (0), but without any natural upper extremum,
   which is tantamount to the integer interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype mountain-program ()
  "The ``mountain-program'' type defines an executable Mountain program
   as a vector compact of zero or more ``Instruction'' objects."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines the Mountain program stack as a list-based
   last-in first-out (LIFO) collection of signed integer numbers,
   bourneless along both marches."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every #'integerp
              (the list candidate)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:copier NIL))
  "The ``Instruction'' interface establishes a substratum for all
   classes conjoined in their pursuit to replicate Mountain
   instructions.")

;;; -------------------------------------------------------

(defstruct (Psh-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Psh-Instruction'' class serves in the encapsulation of a
   Mountain \"psh\" operation, incorporating the positive integral
   number to convey."
  (value (error "Missing value.") :type positive-integer :read-only T))

;;; -------------------------------------------------------

(defstruct (Neg-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Neg-Instruction'' class serves in the encapsulation of a
   Mountain \"neg\" operation.")

;;; -------------------------------------------------------

(defstruct (Jmp-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Jmp-Instruction'' class serves in the encapsulation of a
   Mountain \"jmp\" operation.")

;;; -------------------------------------------------------

(defstruct (Add-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Add-Instruction'' class serves in the encapsulation of a
   Mountain \"add\" operation.")

;;; -------------------------------------------------------

(defstruct (Rot-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Rot-Instruction'' class serves in the encapsulation of a
   Mountain \"rot\" operation.")

;;; -------------------------------------------------------

(defstruct (Dup-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Dup-Instruction'' class serves in the encapsulation of a
   Mountain \"dup\" operation.")

;;; -------------------------------------------------------

(defstruct (Out-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Out-Instruction'' class serves in the encapsulation of a
   Mountain \"out\" operation.")

;;; -------------------------------------------------------

(defstruct (Inp-Instruction
  (:include Instruction)
  (:copier  NIL))
  "The ``Inp-Instruction'' class serves in the encapsulation of a
   Mountain \"inp\" operation.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-possible-instruction (source start)
  "Proceeding from the START position into the SOURCE, seeks and returns
   the position of the nearest instruction identifier constituent, or
   responds with the SOURCE's length, which signifies the search's
   exhaustion, if none such could be detected."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position #\ʌ source :start start :test #'char=)
        (length source))))

;;; -------------------------------------------------------

(defun character-at-equals-p (source position expected-character)
  "Determines whether a character at the POSITION in the SOURCE exists
   and concomitantly matches the EXPECTED-CHARACTER, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the boolean
    (not (null
      (and (array-in-bounds-p source position)
           (char= (char source position) expected-character))))))

;;; -------------------------------------------------------

(defun string-located-at-p (source start expected-string)
  "Determines whether, proceeding from the START position into the
   SOURCE, the subsequent characters replicate the EXPECTED-STRING,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type string expected-string))
  (the boolean
    (not (null
      (string= source expected-string
        :start1 start
        :end1   (min (+ start (length expected-string))
                     (length source)))))))

;;; -------------------------------------------------------

(defun parse-psh-argument (source start)
  "Proceeding from the START position into the SOURCE, consumes zero or
   more \"psh\" arguments, that is, \"ʌ\" instances in immediate
   succession, and returns their tally."
  (declare (type string source))
  (declare (type fixnum start))
  (the non-negative-integer
    (loop
      for   position of-type fixnum from start below (length source)
      while (character-at-equals-p source position #\ʌ)
      count 1)))

;;; -------------------------------------------------------

(defun parse-psh-instruction (source start)
  "Determines whether, proceeding from the START position into the
   SOURCE, a \"psh\" instruction, identified by the character sequence
   \"ʌʍ...ʍ\", follows, returning on confirmation two values:
     (1) A ``Psh-Instruction'' encapsulation of the interpreted
         instruction, if the operation could be ascertained, or ``NIL''
         otherwise.
     (2) The position into the SOURCE immediately succeeding the matched
         section, if the operation could be ascertained, or the START
         location itself upon its absence."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (or null Psh-Instruction) fixnum)
    (if (string-located-at-p source start "ʌʍʌ")
      (let ((position     (+ start 2))
            (psh-argument 0))
        (declare (type fixnum               position))
        (declare (type non-negative-integer psh-argument))
        (setf psh-argument (parse-psh-argument source position))
        (incf position     psh-argument)
        (if (and (plusp psh-argument)
                 (character-at-equals-p source position #\ʍ))
          (values
            (make-psh-instruction :value psh-argument)
            (1+ position))
          (values NIL start)))
      (values NIL start))))

;;; -------------------------------------------------------

(defun parse-instruction (source start)
  "Parses the operation commencing at the START position into the
   SOURCE, if possible, and returns two values:
     (1) If a Mountain instruction commences at the START position, an
         ``Instruction'' encapsulation thereof, otherwise ``NIL''.
     (2) If a Mountain instruction could be parsed, the position in the
         SOURCE immediately succeeding its occupied section, otherwise
         the position immediately following the START location."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position start))
    (declare (type fixnum position))
    (flet
        ((probe-instruction (identifier constructor)
          "Matches the IDENTIFIER against the SOURCE, proceeding from
           the START position, on confirmation invoking the CONSTRUCTOR
           function and returning its yielded its ``Instruction''
           object, while concomitantly updating the POSITION cursor to
           the location in the SOURCE immediately succeeding the matched
           section; otherwise the ``NIL'' value is returned."
          (declare (type string                    identifier))
          (declare (type (function () Instruction) constructor))
          (the (or null Instruction)
            (when (string-located-at-p source start identifier)
              (prog1
                (funcall constructor)
                (setf position (+ start (length identifier)))))))
         
         (probe-psh-instruction ()
          "Matches the \"psh\" operation identifier, \"ʌʍ...ʍ\", against
           the SOURCE, proceeding from the START position, on
           confirmation returning the parsed ``Psh-Instruction'', while
           concomitantly updating the POSITION cursor to the location in
           the SOURCE immediately succeeding the matched section;
           otherwise the ``NIL'' value is returned."
          (multiple-value-bind (instruction new-position)
              (parse-psh-instruction source start)
            (declare (type (or null Psh-Instruction) instruction))
            (declare (type fixnum                    new-position))
            (when instruction
              (prog1 instruction
                (setf position new-position)))))
          
         (apply-default-case ()
          "Imputing the case of no instruction's presence at the START
           position, return the ``NIL'' value, while concomitantly
           incrementing the POSITION cursor to the location immediately
           succeeding the START index."
          (incf position)
          NIL))
      
      (the (values (or null Instruction) fixnum)
        (values
          (or (probe-instruction     "ʌʍʍʍʍ" #'make-inp-instruction)
              (probe-instruction     "ʌʍʍʍ"  #'make-rot-instruction)
              (probe-instruction     "ʌʍʍ"   #'make-neg-instruction)
              (probe-instruction     "ʌʌʍʍʍ" #'make-out-instruction)
              (probe-instruction     "ʌʌʍʍ"  #'make-add-instruction)
              (probe-instruction     "ʌʌʍ"   #'make-jmp-instruction)
              (probe-instruction     "ʌʌʌʍ"  #'make-dup-instruction)
              (probe-psh-instruction)
              (apply-default-case))
          position)))))

;;; -------------------------------------------------------

(defun extract-instructions (source)
  "Extracts from the piece of Mountain SOURCE code the incorporated
   instructions and returns a one-dimensional simple array of their
   ``Instruction'' representations."
  (declare (type string source))
  (let ((position (locate-possible-instruction source 0)))
    (declare (type fixnum position))
    (flet
        ((collect-instruction (instruction new-position)
          "If the INSTRUCTION is non-``NIL'', returns a singleton list
           with the same as its aefault element, otherwise responds with
           an empty list, in any case updating the POSITION cursor to
           the location in the SOURCE of the next possible instruction
           commencing from the NEW-POSITION."
          (declare (type (or null Instruction) instruction))
          (declare (type fixnum                new-position))
          (the (or null (cons Instruction null))
            (prog1
              (when instruction
                (list instruction))
              (setf position
                (locate-possible-instruction source new-position))))))
      (the mountain-program
        (coerce
          (loop while (< position (length source)) append
            (multiple-value-call #'collect-instruction
              (parse-instruction source position)))
          '(simple-array Instruction (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Stack-Error (simple-error)
  ()
  (:default-initargs
    :format-control "Cannot peek into pop from an empty stack.")
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves to apprize about an
     anomalous situation instigated by the attempt to peek into or pop
     from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          mountain-program
    :documentation "The Mountain program to evaluate.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          integer
    :documentation "The current instruction pointer (IP) location.")
   (jump-target
    :initform      NIL
    :accessor      jump-target
    :type          (or null integer)
    :documentation "The contingent next index for the instruction
                    pointer (IP) in the case of a prevenient \"jmp\"
                    operation's invocation.")
   (stack
    :initform      NIL
    :accessor      program-stack
    :type          stack
    :documentation "The program stack.")
   (program-halted-p
    :initform      NIL
    :accessor      program-halted-p
    :type          boolean
    :documentation "Determines whether the program has terminated."))
  (:documentation
    "The ``Interpreter'' class is encumbered with the dever of imparting
     actual effect to a Mountain program."))

;;; -------------------------------------------------------

(defun check-if-program-halted (interpreter)
  "Determines whether the Mountain program consigned to the
   INTERPRETER's castaldy has been completely processed or prematurely
   aborted by a jump operation, on confirmation adjusting the respective
   flag in the INTERPRETER, in any case returns no value."
  (declare (type Interpreter interpreter))
  (setf (program-halted-p interpreter)
    (or
      (program-halted-p interpreter)
      (not
        (array-in-bounds-p
          (get-program interpreter)
          (program-ip  interpreter)))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (check-if-program-halted interpreter)
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' dedicated to the Mountain
   PROGRAM's evaluation."
  (declare (type mountain-program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the next
   instruction, which is either its immediate successor in the program,
   or, upon a intermediate jump target's specification, the respective
   destination, and returns no value."
  (declare (type Interpreter interpreter))
  (if (jump-target interpreter)
    (shiftf (program-ip  interpreter) (jump-target interpreter) NIL)
    (incf   (program-ip interpreter)))
  (check-if-program-halted interpreter)
  (values))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the currently processed instruction in the INTERPRETER's
   program."
  (declare (type Interpreter interpreter))
  (the Instruction
    (aref
      (get-program interpreter)
      (program-ip  interpreter))))

;;; -------------------------------------------------------

(defun push-on-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-value))
  (push new-value
    (program-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (interpreter)
  "Removes and returns the top element on the INTERPRETER's stack, or
   signals an error of the type ``Empty-Stack-Error'' upon its vacancy."
  (declare (type Interpreter interpreter))
  (the integer
    (or (pop (program-stack interpreter))
        (error 'Empty-Stack-Error))))

;;; -------------------------------------------------------

(defun negate-top-element (interpreter)
  "Negates the top element on the INTERPRETER's stack and returns no
   value, or signals an error of the type ``Empty-Stack-Error'' upon its
   vacancy."
  (declare (type Interpreter interpreter))
  (if (program-stack interpreter)
    (setf (first (program-stack interpreter))
      (- (first (program-stack interpreter))))
    (error 'Empty-Stack-Error))
  (values))

;;; -------------------------------------------------------

(defun rotate-stack-bottom (interpreter)
  "Removes the bottom element from the INTERPRETER's stack, pushes it
   unto its top, and returns no value, or signals an error of the type
   ``Empty-Stack-Error'' upon its vacancy."
  (declare (type Interpreter interpreter))
  (if (program-stack interpreter)
    (let ((bottom-element (car (last (program-stack interpreter)))))
      (declare (type integer bottom-element))
      (setf (program-stack interpreter)
        (nbutlast
          (program-stack interpreter)))
      (push-on-stack interpreter bottom-element))
    (error 'Empty-Stack-Error))
  (values))

;;; -------------------------------------------------------

(defun duplicate-top-stack-element (interpreter)
  "Duplicates the top element on the INTERPRETER's stack and returns no
   value, or signals an error of the type ``Empty-Stack-Error'' upon its
   vacancy."
  (declare (type Interpreter interpreter))
  (if (program-stack interpreter)
    (push
      (first (program-stack interpreter))
      (program-stack interpreter))
    (error 'Empty-Stack-Error))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Processes the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Psh-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Psh-Instruction instruction))
  (push-on-stack interpreter
    (psh-instruction-value instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Neg-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Neg-Instruction instruction))
  (declare (ignore               instruction))
  (negate-top-element interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Jmp-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Jmp-Instruction instruction))
  (declare (ignore               instruction))
  (let ((n (pop-from-stack interpreter))
        (a (pop-from-stack interpreter))
        (b (pop-from-stack interpreter)))
    (declare (type integer n))
    (declare (ignorable    n))
    (declare (type integer a))
    (declare (type integer b))
    (cond
      ((and (= a b) (plusp n))
        (setf (jump-target interpreter) (1- n)))
      ((and (= a b) (<= n 0))
        (setf (program-halted-p interpreter) T))
      (T
        NIL)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Add-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Add-Instruction instruction))
  (declare (ignore               instruction))
  (let ((a (pop-from-stack interpreter))
        (b (pop-from-stack interpreter)))
    (declare (type integer a))
    (declare (type integer b))
    (push-on-stack interpreter (+ b a)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Rot-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Rot-Instruction instruction))
  (declare (ignore               instruction))
  (rotate-stack-bottom interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Dup-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Dup-Instruction instruction))
  (declare (ignore               instruction))
  (duplicate-top-stack-element interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Out-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Out-Instruction instruction))
  (declare (ignore               instruction))
  (format T "~c"
    (code-char
      (pop-from-stack interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Inp-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Inp-Instruction instruction))
  (declare (ignore               instruction))
  (format T "~&>> ")
  (finish-output)
  (push-on-stack interpreter
    (char-code
      (read-char)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Executes the Mountain program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-halted-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Mountain (code)
  "Interprets the piece of Mountain source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat.
(interpret-Mountain "ʌʍʍʍʍʌʌʍʍʍ")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
;; 
;; The following pseudocode formulation applies to the program:
;; 
;;   ---------------------------------------
;;   Instruction no. | Instruction mnemonic
;;   ----------------+----------------------
;;   1               | inp
;;   .......................................
;;   2               | out
;;   .......................................
;;   3               | psh 1
;;   .......................................
;;   4               | psh 1
;;   .......................................
;;   5               | psh 1
;;   .......................................
;;   6               | jmp 1, 1, 1
;;   ---------------------------------------
(interpret-Mountain "ʌʍʍʍʍʌʌʍʍʍʌʍʌʍʌʍʌʍʌʍʌʍʌʌʍ")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The following pseudocode formulation applies to the program:
;; 
;;   ----------------------------------------------
;;   Instruction no. | Instruction mnemonic
;;   ----------------+-----------------------------
;;   1               | inp
;;   ..............................................
;;   2               | dup
;;   ..............................................
;;   3               | out
;;   ..............................................
;;   4               | dup
;;   ..............................................
;;   5               | psh 49 (= "1")
;;   ..............................................
;;   6               | psh 2  (back jump position)
;;   ..............................................
;;   7               | jmp 2, in, 1
;;   ----------------------------------------------
(interpret-Mountain
  "ʌʍʍʍʍ
   ʌʌʌʍ
   ʌʌʍʍʍ
   ʌʌʌʍ
   ʌʍʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʍ
   ʌʍʌʌʍ
   ʌʌʍ")

;;; -------------------------------------------------------

;; The aboon truth-machine deprieved of the supererogative whitespaces:
(interpret-Mountain
  "ʌʍʍʍʍʌʌʌʍʌʌʍʍʍʌʌʌʍʌʍʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʌʍʌʍʌʌʍʌʌʍ")
