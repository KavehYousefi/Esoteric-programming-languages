;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ℕDQ", also nevened "NDQ", invented by the Esolang user
;; "yayimhere", its proprium, accounting for a cleronomy of the previent
;; language "DQ" from the same creative provenance, reifies in the
;; coefficiency of two stacks, both admitting merely the integer number
;; one (1), these being the subjects of several stack operations and
;; control flow mechanisms' efforts.
;; 
;; 
;; Concept
;; =======
;; The ℕDQ programming language, its agnomination wisting of a
;; poecilonym in "NDQ", and imparted with the proprium of a stack
;; twissel's champarty, both competent merely in an arbitrary tally of
;; one-valued (1) integers' castaldy, employs the basic last-in
;; first-out operations in coefficiency with a conditional mechanism
;; for its operative causata.
;; 
;; == ℕDQ: DQ + MORE ==
;; Its gendrure as a progenitor of the DQ programming language serves to
;; invests ℕDQ with a firmament of stack manipulation facilities; the
;; capacitation of its active stack's exchange, as well as a conditional
;; block execution, and an immediate termination behest, account for its
;; peculium adventicium.
;; 
;; == TWO STACKS OPERATE IN CHAMPARTY ==
;; The most dioristic expression of its haecceity, ℕDQ deploys two
;; stacks whose tolerance does not extend beyond the admission of the
;; integeral number one (1).
;; 
;; At the execution's inchoation, both stacks assume a vacant status,
;; the first constituting the currently active one, entalented with a
;; responsiveness to all manipulative involvements. A dedicated
;; instruction, "$", exists for the switching to the hitherto inactive
;; stack, and vice versa.
;; 
;; == A MORIBUND PROGRAM PRINTS ITS STACKS ==
;; At the program's conclusion, as an desinent act, both stacks'
;; contents in their entirety will be printed to the standard output.
;; 
;; 
;; Syntax
;; ======
;; An ℕDQ program's conformation, from a syntactical exercise of one's
;; conspectuity, appropriates an ordered sequence of zero or more lines,
;; everichon among these lending a commorancy to an arbitrary account of
;; instructions, in their preponderance represented by an aefauld
;; symbol, with the conditional block statement's exemption.
;; 
;; == THE PROGRAM: A SEQUENCE OF LINES ==
;; Every non-blank horizontal expansion comprehends one or more
;; operations, implicitly enumerated commencing with one (1). Vacant
;; lines do not contribute to this tally.
;; 
;; == THE INSTRUCTION: NILADIC SYMBOLS OR BLOCKS ==
;; With the exception of the conditional block statement, all
;; instructions in this language subsume into a single character
;; representation, destitute of arguments. The block specimen, however,
;; is demarcated by a jumelle of brackets, "[" and "]", with an
;; immediate integral operand to its dextral closure.
;; 
;; Any non-operative token enjoys a mete of tolerance equipollent to
;; its neglect, thus serving as a no-operation, or NOP.
;; 
;; == COMMENTS ==
;; No express provision for commentary supererogation partakes of the
;; language's dations; natheless, non-instruction tokens remain without
;; epiphenomenal value, and may thus be appropriated for descanting
;; purposes.
;; 
;; 
;; Instructions
;; ============
;; ℕDQ instruction set enumerates a septuple cardinality, a quadruple
;; subset thereof desumed, in a modulated form, from its DQ cleronomy,
;; while three operative warklumes for the twin stack's castaldy and the
;; conditional as well as unconditional control flow manipulation
;; constitute a personal novelty.
;; 
;; == OVERVIEW ==
;; A fundamental mete of nortelry's dation anent the ℕDQ language's
;; operative compass shall be the following apercu's dever.
;; 
;; Please heed the demarcation of succedaneous tmemata by adminiculum
;; of a catena of asterisks ("*"), their respective occurrencies
;; intended for the supersession by actual ℕDQ code fragments.
;; 
;;   ------------------------------------------------------------------
;;   Command            | Effect
;;   -------------------+----------------------------------------------
;;   0                  | Clears the current stack.
;;                      |----------------------------------------------
;;                      | This operation constitutes a modified
;;                      | appropriation from DQ.
;;   ..................................................................
;;   1                  | Pushes the number one (1) onto the current
;;                      | stack.
;;                      |----------------------------------------------
;;                      | This operation constitutes a modified
;;                      | appropriation from DQ.
;;   ..................................................................
;;   D                  | Duplicates the current stack's top element.
;;                      |----------------------------------------------
;;                      | This operation constitutes a modified
;;                      | appropriation from DQ.
;;   ..................................................................
;;   Q                  | Pops and discards the current stack's top
;;                      | element.
;;                      |----------------------------------------------
;;                      | This operation constitutes a modified
;;                      | appropriation from DQ.
;;   ..................................................................
;;   $                  | Sets the contemporaneously inactive stack as
;;                      | the current one.
;;                      |----------------------------------------------
;;                      | This operation constitutes a novel
;;                      | introduction not desumed from DQ.
;;   ..................................................................
;;   [statements]target | If the current stack is not empty, executes
;;    ********** ****** | the {statements} once; otherwise omits the
;;                      | same and relocates the instruction pointer
;;                      | (IP) to the start of the line specified by
;;                      | one-based {target} index.
;;                      |----------------------------------------------
;;                      | {statements} must be an ordered sequence of
;;                      | zero or more instructions.
;;                      |----------------------------------------------
;;                      | {target} must be a one-based line number.
;;                      | Given a tally "n" of lines constituting the
;;                      | executing program, the following holds:
;;                      |   (1) If 1 <= {target} <= n, relocates the
;;                      |       instruction pointer (IP) to the
;;                      |       {target}-th line.
;;                      |   (2) If {target} < 1, then relocates the
;;                      |       instruction pointer (IP) to the first
;;                      |       line, that is, the index one (1).
;;                      |   (3) If {target} > n, immediately terminates
;;                      |       the program.
;;                      |----------------------------------------------
;;                      | This operation constitutes a novel
;;                      | introduction not desumed from DQ.
;;   ..................................................................
;;   U                  | Immediately terminates the program.
;;                      |----------------------------------------------
;;                      | This operation constitutes a novel
;;                      | introduction not desumed from DQ.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, the entirety of its labor a
;; multipartite partage, differentiating into the inchoate segregation
;; of the source code string into lines of characters, each such
;; subsequence's conversion into a dedicated encapsulation of
;; instruction objects, and the assemblage into a composite enumerating
;; these operative lines as a comprehensive vector's elements.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-12-15
;; 
;; Sources:
;;   [esolang2024DQ]
;;   The Esolang contributors, "DQ", July 22nd, 2024
;;   URL: "https://esolangs.org/wiki/DQ"
;;   
;;   [esolang2024ℕDQ]
;;   The Esolang contributors, "ℕDQ", August 25th, 2024
;;   URL: "https://esolangs.org/wiki/%E2%84%95DQ"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines an ordered sequence of
   ℕDQ instructions, amenable to a zero-based index, and realized in
   the mold of a one-dimensional simple array of ``Instruction''
   instances."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member among these complying with the ELEMENT-TYPE, thilk is
   governed by the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an ℕDQ program as a one-dimensional
   simple array of code lines."
  '(simple-array Code-Line (*)))

;;; -------------------------------------------------------

(deftype integral-one ()
  "The ``one'' type defines a throughout parvipotent subset of the
   integer numbers, admitting merely the member one (1)."
  '(integer 1 1))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a program stack as a list-based last in,
   first out salvatory, dedicated to the castaldy of the integral number
   one, modeled via the ``integral-one''."
  '(list-of integral-one))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Produces a veridical Boolean tantamount of the OBJECT when construed
   as a \"generalized boolean\", returning for a non-``NIL'' input a
   ``boolean'' value of ``T''; otherwise, for ``NIL'' OBJECT, responds
   with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface establishes a firmament as the unifying
   principle of all classes intended to represent ℕDQ operations.")

;;; -------------------------------------------------------

(defstruct (Switch-Stack-Instruction
  (:include Instruction))
  "The ``Halt-Instruction'' class dedicates its existency to the
   representation of the \"$\" active stack exchange operation, in this
   telos' pursuit being disencumbered from any further data's
   inclusion.")

;;; -------------------------------------------------------

(defstruct (Clear-Stack-Instruction
  (:include Instruction))
  "The ``Clear-Stack-Instruction'' class pursues the representation of
   the \"0\" stack clearance operation.")

;;; -------------------------------------------------------

(defstruct (Push-1-Instruction
  (:include Instruction))
  "The ``Push-1-Instruction'' class pursues the representation of the
   \"1\" one (1) pushing operation.")

;;; -------------------------------------------------------

(defstruct (Duplicate-Instruction
  (:include Instruction))
  "The ``Duplicate-Instruction'' class pursues the representation of the
   \"D\" top stack element duplication operation.")

;;; -------------------------------------------------------

(defstruct (Discard-Instruction
  (:include Instruction))
  "The ``Discard-Instruction'' class pursues the representation of the
   \"Q\" top stack element discardment operation.")

;;; -------------------------------------------------------

(defstruct (Block-Start-Instruction
  (:include Instruction))
  "The ``Block-Start-Instruction'' class serves in the encapsulation of
   a conditional block's inchoation marker, that is, the \"[\" token,
   its circumference amplecting as the aefauld object of deliberation
   the position on the same line designating the concluding moeity,
   \"]n\"."
  (end-point 0 :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defstruct (Block-End-Instruction
  (:include Instruction))
  "The ``Block-End-Instruction'' class serves in the encapsulation of a
   conditional block's terminating marker, that is, the \"]n\" token,
   its circumference exhausted by the target line number to navigate to
   upon the complex' omission."
  (destination (error "Missing block destination.")
               :type      fixnum
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Halt-Instruction
  (:include Instruction))
  "The ``Halt-Instruction'' class dedicates its existency to the
   representation of the \"U\" halt operation, in this telos' pursuit
   being disencumbered from any further data's inclusion.")

;;; -------------------------------------------------------

(defstruct (NOP-Instruction
  (:include Instruction))
  "The ``NOP-Instruction'' class applies itself to the representation of
   an ineffectuous operation, or no-operation (NOP), an equivalency in
   its vacuous nature as its physical epiphnomenon.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Code-Line".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Code-Line
  (:constructor make-empty-code-line ()))
  "The ``Code-Line'' class serves in the representation of a single
   line of instructions participating in a ℕDQ program."
  (instructions (make-array 0
                  :element-type     'Instruction
                  :initial-contents NIL
                  :adjustable       T
                  :fill-pointer     T)
                :type      (vector Instruction *)
                :read-only T))

;;; -------------------------------------------------------

(defun add-instruction-to-line (line new-instruction)
  "Appends the NEW-INSTRUCTION to the code LINE's rear and returns no
   value."
  (declare (type Code-Line   line))
  (declare (type Instruction new-instruction))
  (vector-push-extend new-instruction
    (code-line-instructions line))
  (values))

;;; -------------------------------------------------------

(defun code-line-length (line)
  "Returns the tally of instructions comprising the code LINE."
  (declare (type Code-Line line))
  (the fixnum
    (length
      (code-line-instructions line))))

;;; -------------------------------------------------------

(defun code-line-instruction-at (line index)
  "Returns the instruction located at the zero-based INDEX in the
   code LINE."
  (declare (type Code-Line line))
  (declare (type fixnum    index))
  (the Instruction
    (aref (code-line-instructions line) index)))

;;; -------------------------------------------------------

(defun code-line-is-empty-p (line)
  "Determines whether the code LINE constitutes a vacuous entity, either
   deprieved of any content or, alternatively, affording a woning to
   no-operation instructions only, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Code-Line line))
  (the boolean
    (get-boolean-value-of
      (or (zerop (length (code-line-instructions line)))
          (every #'nop-instruction-p
            (code-line-instructions line))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditional block ends connector.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-conditional-block-ends (line)
  "Stores the matching zero-based ``Block-End-Instruction'' positions
   into the code LINE's ``Block-Start-Instruction'' instances in order
   to facilitate the omission of failing conditonal tmemata and returns
   the modified LINE."
  (declare (type Code-Line line))
  (let ((start-points NIL))
    (declare (type (list-of Block-Start-Instruction) start-points))
    (loop
      for instruction-index
        of-type fixnum
        from    0
        below   (code-line-length line)
      for current-instruction
        of-type Instruction
        =       (code-line-instruction-at line instruction-index)
      do
        (typecase current-instruction
          (Block-Start-Instruction
            (push current-instruction start-points))
          (Block-End-Instruction
            (if start-points
              (let ((start-point (pop start-points)))
                (declare (type Block-Start-Instruction start-point))
                (setf (block-start-instruction-end-point start-point)
                      instruction-index))
              (error "Unmatched conditional block terminator at ~
                      index ~d."
                instruction-index)))
          (otherwise NIL))))
  (the Code-Line line))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab entity, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the location immediately
   succeeding the omitted parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun expect-character (source position desideratum)
  "Determines whether the character at the POSITION into the SOURCE
   concurs with the DESIDERATUM, returning on confirmation the index
   immediately succeeding the POSITION; otherwise signals an error of
   an unspecified type."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character desideratum))
  (the fixnum
    (cond
      ((not (array-in-bounds-p source position))
        (error "The index ~d does not designated a valid position ~
                into the source."
          position))
      ((char/= (char source position) desideratum)
        (error "Expected the character \"~c\" at position ~d, but ~
                encountered \"~c\"."
          desideratum position
          (char source position)))
      (T
        (1+ position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (string fixnum Code-Line) fixnum)
                parse-conditional-block))

;;; -------------------------------------------------------

(defun parse-statement (source start instructions)
  "Proceeding from the START position into the SOURCE, reads and parses
   a single ℕDQ statement, stores thilk in the INSTRUCTIONS, and
   returns the position into the SOURCE immediately succeeding the
   consumed parcel."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type Code-Line instructions))
  (the fixnum
    (case (char source start)
      (#\0
        (add-instruction-to-line instructions
          (make-clear-stack-instruction))
        (1+ start))
      (#\1
        (add-instruction-to-line instructions
          (make-push-1-instruction))
        (1+ start))
      (#\D
        (add-instruction-to-line instructions
          (make-duplicate-instruction))
        (1+ start))
      (#\Q
        (add-instruction-to-line instructions
          (make-discard-instruction))
        (1+ start))
      (#\$
        (add-instruction-to-line instructions
          (make-switch-stack-instruction))
        (1+ start))
      (#\U
        (add-instruction-to-line instructions
          (make-halt-instruction))
        (1+ start))
      (#\[
        (parse-conditional-block source start instructions))
      (#\]
        (error "Unmatched \"]\" token at position ~d." start))
      (otherwise
        (add-instruction-to-line instructions
          (make-nop-instruction))
        (1+ start)))))

;;; -------------------------------------------------------

(defun read-line-number (source start)
  "Proceeding from the START position into the SOURCE, reads an unsigned
   integer literal and returns two values:
     (1) The extracted line number as an unsigned integer value.
     (2) The position into the SOURCE immediately succeeding the
         consumed parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (integer 0 *) fixnum)
    (parse-integer source :start start :end
      (or (position-if-not #'digit-char-p source :start start)
          (length source)))))

;;; -------------------------------------------------------

(defun parse-statement-block (source start instructions)
  "Proceeding from the START position into the SOURCE, parses a
   conditional block's statements, ensuing from the instigating \"[\"
   token, and terminating in, but including, the matching \"]\"
   counterpart, inserts the extracted statements in their correct order
   into the INSTRUCTIONS, and returns the position of the concluding
   \"]\" position."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type Code-Line instructions))
  (let ((position start))
    (declare (type fixnum position))
    (the fixnum
      (loop
        ;; Source exhausted, but no closing "]" detected?
        if (>= position (length source)) do
          (error "Unterminated statement block at position ~d."
            position)
        ;; Closing "]" detected?
        else if (char= (char source position) #\]) do
          (return position)
        ;; Body statement detected?
        else do
          (setf position
            (parse-statement source position instructions))))))

;;; -------------------------------------------------------

(defun parse-block-end (source start instructions)
  "Proceeding from the START position into the SOURCE, parses a
   conditional block's desinent bourne, \"]n\", appends the resulting
   instruction into the INSTRUCTIONS sequence, and returns no value."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type Code-Line instructions))
  (multiple-value-bind (target-line-number new-position)
      (read-line-number source
        (skip-spaces source
          (expect-character source start #\])))
    (declare (type (integer 0 *) target-line-number))
    (declare (type fixnum        new-position))
    (add-instruction-to-line instructions
      (make-block-end-instruction :destination target-line-number))
    (the fixnum new-position)))

;;; -------------------------------------------------------

(defun parse-conditional-block (source start instructions)
  "Proceeding from the START position into the SOURCE, parses a
   conditional block, encompassing its inchoating mark, \"[\", the
   body statements, and its closure segment, \"]n\", appends these all
   in their correct order to the INSTRUCTIONS, and returns no value."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type Code-Line instructions))
  (let ((current-position (expect-character source start #\[)))
    (declare (type fixnum current-position))
    (add-instruction-to-line instructions
      (make-block-start-instruction))
    (the fixnum
      (parse-block-end source
        (parse-statement-block source current-position instructions)
        instructions))))

;;; -------------------------------------------------------

(defun parse-code-line (source)
  "Parses the instructions ensconced in the SOURCE string and returns
   a ``Code-Line'' representation thereof."
  (declare (type string source))
  (let ((code-line (make-empty-code-line)))
    (declare (type Code-Line code-line))
    (loop
      with position of-type fixnum = 0
      while (< position (length source)) do
        (setf position
          (parse-statement source position code-line)))
    (the Code-Line code-line)))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of ℕDQ SOURCE code and returns a vector of its
   lines."
  (declare (type string source))
  (with-input-from-string (input-stream source)
    (declare (type string-stream input-stream))
    (the program
      (coerce
        (loop
          for input-line
            of-type (or null string)
            =       (read-line input-stream NIL NIL)
          
          while input-line
            for program-line
              of-type Code-line
              =       (parse-code-line input-line)
            unless (code-line-is-empty-p program-line)
              collect
                (connect-conditional-block-ends program-line))
        '(simple-array Code-Line (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction pointer (IP).                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction-Pointer
  (:constructor make-instruction-pointer ())
  (:conc-name ip-))
  "The ``Instruction-Pointer'' class furnishes an implementation of a
   two-dimensional instruction pointer (IP) or program counter (PC), its
   bailiwick the castaldy of a line-column composite, both represented
   by their zero-based indices."
  (line-number   0 :type fixnum :read-only NIL)
  (column-number 0 :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defun ip-move-forward (instruction-pointer)
  "Advances the INSTRUCTION-POINTER to the next column, retaining its
   contemporaneous line index, and returns no value."
  (declare (type Instruction-Pointer instruction-pointer))
  (incf (ip-column-number instruction-pointer))
  (values))

;;; -------------------------------------------------------

(defun ip-move-to-column (instruction-pointer new-column-number)
  "Relocates the INSTRUCTION-POINTER to the NEW-COLUMN-NUMBER, retaining
   its contemporaneous line index, and returns no value."
  (declare (type Instruction-Pointer instruction-pointer))
  (declare (type fixnum              new-column-number))
  (setf (ip-column-number instruction-pointer) new-column-number)
  (values))

;;; -------------------------------------------------------

(defun ip-move-to-line (instruction-pointer new-line-number)
  "Relocates the INSTRUCTION-POINTER to the NEW-LINE-NUMBER,
   concomitantly resetting its column index to zero (0), and returns no
   value."
  (declare (type Instruction-Pointer instruction-pointer))
  (declare (type fixnum              new-line-number))
  (psetf (ip-line-number   instruction-pointer) new-line-number
         (ip-column-number instruction-pointer) 0)
  (values))

;;; -------------------------------------------------------

(defun ip-move-down-one-line (instruction-pointer)
  "Relocates the INSTRUCTION-POINTER one line down, concomitantly
   resetting its column index to zero (0), and returns no value."
  (declare (type Instruction-Pointer instruction-pointer))
  (incf (ip-line-number   instruction-pointer) 1)
  (setf (ip-column-number instruction-pointer) 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((stacks
    :initform      (make-array 2
                     :element-type     'stack
                     :initial-contents (list NIL NIL)
                     :adjustable       NIL
                     :fill-pointer     NIL)
    :accessor      memory-stacks
    :type          (simple-array stack (2))
    :documentation "The twissel of integer stacks.")
   (active-stack-index
    :initform      0
    :accessor      memory-active-stack-index
    :type          (integer 0 1)
    :documentation "The index of the currently active member among the
                    STACKS array."))
  (:documentation
    "The ``Memory'' class is apportioned the dever of the ℕDQ program
     memory's castaldy, its componency exhausted by a stack twissel."))

;;; -------------------------------------------------------

(defun memory-first-stack (memory)
  "Returns the MEMORY's first stack."
  (declare (type Memory memory))
  (the stack
    (aref (memory-stacks memory) 0)))

;;; -------------------------------------------------------

(defun memory-second-stack (memory)
  "Returns the MEMORY's second stack."
  (declare (type Memory memory))
  (the stack
    (aref (memory-stacks memory) 1)))

;;; -------------------------------------------------------

(defun memory-active-stack (memory)
  "Returns the MEMORY's currently active stack instance."
  (declare (type Memory memory))
  (the stack
    (aref (memory-stacks memory)
      (memory-active-stack-index memory))))

;;; -------------------------------------------------------

(defun (setf memory-active-stack) (new-content memory)
  "Sets the state of the MEMORY's active stack to the NEW-CONTENT and
   returns no value."
  (declare (type Memory memory))
  (setf
    (aref (memory-stacks memory)
      (memory-active-stack-index memory))
    new-content)
  (values))

;;; -------------------------------------------------------

(defun memory-switch-active-stack (memory)
  "Switches the MEMORY's currently active to its compernage and returns
   no value."
  (declare (type Memory memory))
  (setf (memory-active-stack-index memory)
    (rem (1+ (memory-active-stack-index memory))
      (length (memory-stacks memory))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        interpreter-program
    :type          program
    :documentation "The ℕDQ program to execute.")
   (ip
    :initform      (make-instruction-pointer)
    :accessor      interpreter-ip
    :type          Instruction-Pointer
    :documentation "The two-dimensional instruction pointer (IP).")
   (shall-halt-program-p
    :initform      NIL
    :accessor      interpreter-shall-halt-program-p
    :type          boolean
    :documentation "A Boolean flag which determines whether an ℕDQ
                    halt instruction (\"U\") has been encountered,
                    communicating the desire to conclude the execution
                    immediately.")
   (memory
    :initform      (make-instance 'Memory)
    :accessor      interpreter-memory
    :type          Memory
    :documentation "The program memory comprehending the stack twain."))
  (:documentation
    "The ``Interpreter'' class furnishes an entity whose capacitation
     wones in the execution of an ℕDQ program, supplied as a vector of
     code lines."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' nuncupated to the ℕDQ
   program's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun current-column-number (interpreter)
  "Returns the zero-based index of the INTERPRETER'S current program
   line."
  (declare (type Interpreter interpreter))
  (the fixnum
    (ip-column-number
      (interpreter-ip interpreter))))

;;; -------------------------------------------------------

(defun current-line-number (interpreter)
  "Returns the zero-based index into the INTERPRETER's current program
   line."
  (declare (type Interpreter interpreter))
  (the integer
    (ip-line-number
      (interpreter-ip interpreter))))

;;; -------------------------------------------------------

(defun current-line (interpreter)
  "Returns the program line currently processed by the INTERPRETER."
  (declare (type Interpreter interpreter))
  (the Code-Line
    (aref (interpreter-program interpreter)
      (current-line-number interpreter))))

;;; -------------------------------------------------------

(defun current-line-length (interpreter)
  "Returns the number of instructions woning in the INTERPRETER's
   currently processed program line."
  (declare (type Interpreter interpreter))
  (the fixnum
    (code-line-length
      (current-line interpreter))))

;;; -------------------------------------------------------

(defun current-line-completed-p (interpreter)
  "Determines whether the current line into the INTERPRETER's ℕDQ
   program has been processed in its entirety, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (current-column-number interpreter)
          (current-line-length   interpreter)))))

;;; -------------------------------------------------------

(defun program-length (interpreter)
  "Returns the tally of lines comprising the INTERPRETER's ℕDQ
   program."
  (declare (type Interpreter interpreter))
  (the fixnum
    (length
      (interpreter-program interpreter))))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the ℕDQ program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (current-line-number interpreter)
          (program-length      interpreter)))))

;;; -------------------------------------------------------

(defun program-terminated-p (interpreter)
  "Determines whether the INTERPRETER's operations shall cease, either
   because of an explicit behest involving siccan desideratum, or the
   implict fact of its instruction pointer's (IP) transcendence beyond
   the valid program bournes, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (or (interpreter-shall-halt-program-p interpreter)
        (program-completed-p              interpreter))))

;;; -------------------------------------------------------

(defun current-instruction (interpreter)
  "Returns the INTERPRETER's currently processed instruction."
  (declare (type Interpreter interpreter))
  (the Instruction
    (code-line-instruction-at
      (current-line          interpreter)
      (current-column-number interpreter))))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position on its current program line, contingently changing to the
   subsequent row, if the current one is exhausted, and returns no
   value."
  (declare (type Interpreter interpreter))
  (ip-move-forward
    (interpreter-ip interpreter))
  (when (current-line-completed-p interpreter)
    (ip-move-down-one-line
      (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-line (interpreter destination)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   inchoation of the one-based DESTINATION line number and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type integer     destination))
  (ip-move-to-line
    (interpreter-ip interpreter)
    (if (>= destination 1)
      (1- destination)
      0))
  (values))

;;; -------------------------------------------------------

(defun current-stack (interpreter)
  "Returns the INTERPRETER's current stack."
  (declare (type Interpreter interpreter))
  (the stack
    (memory-active-stack
      (interpreter-memory interpreter))))

;;; -------------------------------------------------------

(defun (setf current-stack) (new-content interpreter)
  "Updates the INTERPRETER's current stack state to the NEW-CONTENT and
   returns no value."
  (declare (type Interpreter interpreter))
  (setf (memory-active-stack
          (interpreter-memory interpreter))
        new-content)
  (values))

;;; -------------------------------------------------------

(defun current-stack-is-empty-p (interpreter)
  "Determines whether the INTERPRETER's current stack is empty,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (null (current-stack interpreter))))

;;; -------------------------------------------------------

(defun clear-current-stack (interpreter)
  "Clears the INTERPRETER's current stack and returns no value."
  (declare (type Interpreter interpreter))
  (setf (current-stack interpreter) NIL)
  (values))

;;; -------------------------------------------------------

(defun switch-current-stack (interpreter)
  "Switches the INTERPRETER's current stack to the next instance and
   returns no value."
  (declare (type Interpreter interpreter))
  (memory-switch-active-stack
    (interpreter-memory interpreter))
  (values))

;;; -------------------------------------------------------

(defun print-stacks (interpreter)
  "Prints the INTERPRETER's stack twissel to the standard output and
   returns no value."
  (declare (type Interpreter interpreter))
  (format T "~&First stack: [top> ~{~d~^, ~} <bottom]"
    (memory-first-stack
      (interpreter-memory interpreter)))
  (format T "~&Second stack: [top> ~{~d~^, ~} <bottom]"
    (memory-second-stack
      (interpreter-memory interpreter)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter)
            (instruction Clear-Stack-Instruction))
    (declare (type Interpreter             interpreter))
    (declare (type Clear-Stack-Instruction instruction))
    (clear-current-stack         interpreter)
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Push-1-Instruction))
    (declare (type Interpreter        interpreter))
    (declare (type Push-1-Instruction instruction))
    (push 1 (current-stack interpreter))
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Duplicate-Instruction))
    (declare (type Interpreter           interpreter))
    (declare (type Duplicate-Instruction instruction))
    (unless (current-stack-is-empty-p interpreter)
      (push (first (current-stack interpreter))
        (current-stack interpreter)))
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Discard-Instruction))
    (declare (type Interpreter         interpreter))
    (declare (type Discard-Instruction instruction))
    (pop (current-stack interpreter))
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Switch-Stack-Instruction))
    (declare (type Interpreter              interpreter))
    (declare (type Switch-Stack-Instruction instruction))
    (declare (ignore                        instruction))
    (switch-current-stack        interpreter)
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Block-Start-Instruction))
    (declare (type Interpreter             interpreter))
    (declare (type Block-Start-Instruction instruction))
    (if (current-stack-is-empty-p interpreter)
      (jump-to-line interpreter
        (block-start-instruction-end-point instruction))
      (advance-to-next-instruction interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Block-End-Instruction))
    (declare (type Interpreter           interpreter))
    (declare (type Block-End-Instruction instruction))
    (declare (ignore                     instruction))
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Halt-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (type Halt-Instruction instruction))
    (declare (ignore                instruction))
    (setf (interpreter-shall-halt-program-p interpreter) T)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction NOP-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type NOP-Instruction instruction))
    (declare (ignore               instruction))
    (advance-to-next-instruction interpreter)
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the ℕDQ program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-terminated-p interpreter) do
    (process-instruction interpreter
      (current-instruction interpreter)))
  (print-stacks interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpret-ℕDQ (code)
  "Interprets the piece of ℕDQ source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Push 1 twice, duplicate the top of the stack, and discard the same.
(interpret-ℕDQ "11DQ")

;;; -------------------------------------------------------

;; Push 1 twice, duplicate the top of the stack, but terminate the
;; program ere it is capacitated to discard the top item.
(interpret-ℕDQ "11DUQ")

;;; -------------------------------------------------------

;; Transfer two "1" elements from the second to the first stack,
;; expanding the target to five members, and finally skip the desinent
;; line by conditional execution.
(interpret-ℕDQ
  "111$11
   [Q$1$]6
   [Q$1$]6
   [Q$1$]6
   1111111111")
