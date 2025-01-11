;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Braine", invented by the Esolang user "A" and presented on
;; July 10th, 2018, the concept of which founds upon a byte value's
;; incrementation and a conditional goto facility operating in
;; dependence upon its current state, the operational entirety tallying
;; a mere two specimens.
;; 
;; 
;; Concept
;; =======
;; The Braine programming language subsumes into a very rudimentary
;; ilk of operative tongue, which wists of an aefauld unsigned byte
;; only, amenable to incrementations until overflows, and amplified in
;; its competences by a jump instruction whose actuation responds to
;; an imposed state of this octet, operated upon a sequence of zero or
;; more lines, each admitting zero or more instructions.
;; 
;; 
;; Architecture
;; ============
;; Braine's architecture is restricted to a single byte, commorant in
;; the non-negative integer range [0, 255].
;; 
;; == THE MEMORY: AN AEFAULD BYTE ==
;; The complete storage capacity is realized in a single octet, an
;; occupant of the closed interval [0, 255], and initially equal to zero
;; (0). The "+" instruction permits a gradual incrementation which, upon
;; its upper extremum's transcendence, resorts from 255 to the minimum
;; of zero (0).
;; 
;; 
;; Data Types
;; ==========
;; Braine's type system is exhausted by a single member: the octet
;; species.
;; 
;; The byte, an unsigned commorant of the [0, 255] range, is committed
;; to the representation of the program memory.
;; 
;; 
;; Syntax
;; ======
;; From a syntactial exercise of one's conspection, a Braine program
;; maintains an ordered sequence of lines, every one among these a
;; commorancy to zero or more instructions.
;; 
;; 
;; Instructions
;; ============
;; A mere twissel's participation exhausts the language's operative
;; competences, this admission realized in the current byte state's
;; incrementation and a conditional line jumping proceeding from a
;; a particular form therein.
;; 
;; == OVERVIEW ==
;; An apercu shall be administered in a pursuit to convey a basic
;; comprehension of the Braine instructions.
;; 
;; Please note the following:
;; 
;;   (1) Placeholder sections in the description are underlined with a
;;       series of asterisks ("*"), hence they require a substitution by
;;       valid code.
;;   (2) Line numbers are enumerated starting with the index one (1),
;;       but are not encumbered with an upper extremum.
;; 
;;   ------------------------------------------------------------------
;;   Command      | Effect
;;   -------------+----------------------------------------------------
;;   +            | Increments the current byte by one. If the byte
;;                | value exceeds the maximum of 255, it overflows to
;;                | the minimum of zero (0).
;;   ..................................................................
;;   guard:target | If the current byte value equals the {byte},
;;   ***** ****** | moves the instruction pointer (IP) to the start
;;                | of {target}-th line in the program.
;;                |----------------------------------------------------
;;                | The {guard} must be an unsigned byte literal, that
;;                | is, a member of the integral interval [0, 255].
;;                |----------------------------------------------------
;;                | The {target} must be a line number, that is, an
;;                | integer greater than or equal to one.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, its concept the extraction of
;; Braine source code lines, their conversion into dedicated instruction
;; encapsulations, and a collation into a comprehending vector, upon
;; thilk the ultimate execution stage is administered.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-10
;; 
;; Sources:
;;   [esolang2018Braine]
;;   URL: "https://esolangs.org/w/index.php?title=Braine&oldid=56566"
;;   Notes:
;;     - First rendition of the Braine programming language
;;       specification.
;;     - Significant because it substitutes the aefauld "+" instruction
;;       by a twain ">" and "<", similar to brainfuck, and more lucid
;;       to the concept's apprehension. The ">" command, in particular,
;;       states:
;;         "Increment the current byte and move the pointer right.
;;          Overflows to 0 when gets 255."
;;       While the portion relating to the pointer motion is elided from
;;       the most recent treatise iteration.
;;   
;;   [esolang2019Braine]
;;   The Esolang contributors, "Braine", 2019
;;   URL: "https://esolangs.org/w/index.php?title=Braine&oldid=68175"
;;   Notes:
;;     - Establishes the specification of the Braine programming
;;       language.
;;     - The most recent edition can be found under
;;       -> "https://esolangs.org/wiki/Braine"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype line-vector ()
  "The ``line-vector'' type defines a one-dimensional simple array of
   program lines."
  '(simple-array Program-Line (*)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   adjacent bits, thus maintaining its woning in the closed integer
   interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface establishes a foundry for all classes
   pursuing the representation of Braine instructions.")

;;; -------------------------------------------------------

(defstruct (Increment-Instruction
  (:include Instruction))
  "The ``Increment-Instruction'' class serves in the representation of
   the Braine \"+\" instruction, dedicated to the incrementation of the
   current byte.")

;;; -------------------------------------------------------

(defstruct (Goto-Instruction
  (:include     Instruction)
  (:constructor make-goto-instruction (guard destination)))
  "The ``Goto-Instrunction'' class serves in the encapsulation of the
   Braine \"s:n\" instruction, dedicated to a conditional jumping in
   the program."
  (guard       0 :type octet   :read-only T)
  (destination 0 :type integer :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program line.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program-Line
  (:constructor make-program-line
    (list-of-instructions
     &aux (instructions
            (coerce list-of-instructions
              '(simple-array Instruction (*)))))))
  "The ``Program-Line'' class furnishes an encapsulation of a
   random-access collection of zero or more Braine instructions."
  (instructions (error "Missing instructions.")
                :type      (simple-array Instruction (*))
                :read-only T))

;;; -------------------------------------------------------

(defun program-line-instruction-at (line index)
  "Returns the instruction located at the zero-based INDEX into the
   program LINE."
  (declare (type Program-Line line))
  (the Instruction
    (aref (program-line-instructions line) index)))

;;; -------------------------------------------------------

(defun program-line-size (line)
  "Returns the tally of instructions comprising the program LINE."
  (declare (type Program-Line line))
  (the fixnum
    (length
      (program-line-instructions line))))

;;; -------------------------------------------------------

(defun valid-program-line-instruction-index-p (line probed-index)
  "Determines whether the PROBED-INDEX represents a valid zero-based
   position into the program LINE, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Program-Line line))
  (declare (type fixnum       probed-index))
  (the boolean
    (not (null
      (array-in-bounds-p
        (program-line-instructions line)
        probed-index)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Braine program.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((lines
    :initarg       :lines
    :initform      (error "Missing program lines.")
    :reader        program-lines
    :type          line-vector
    :documentation "A vector of the Braine program's effective lines."))
  (:documentation
    "The ``Program'' class serves in the representation of a parsed
     Braine program as a vector of its effective lines, these themselves
     compositions of the entailed instructions."))

;;; -------------------------------------------------------

(defun make-program (lines)
  "Creates and returns a fresh ``Program'' compact of the LINES."
  (declare (type line-vector lines))
  (the Program
    (make-instance 'Program :lines lines)))

;;; -------------------------------------------------------

(defun program-line-at (program line-number)
  "Returns the PROGRAM line amenable to the zero-based LINE-NUMBER."
  (declare (type Program program))
  (declare (type fixnum  line-number))
  (the Program-Line
    (with-slots (lines) program
      (declare (type line-vector lines))
      (aref lines line-number))))

;;; -------------------------------------------------------

(defun program-instruction-at (program line-number instruction-index)
  "Returns the instruction located at the zero-based INSTRUCTION-INDEX
   in PROGRAM line amenable to the zero-based LINE-NUMBER."
  (declare (type Program program))
  (declare (type fixnum  line-number))
  (declare (type fixnum  instruction-index))
  (the Instruction
    (program-line-instruction-at
      (program-line-at program line-number)
      instruction-index)))

;;; -------------------------------------------------------

(defun valid-program-line-number-p (program probed-line-number)
  "Determines whether the PROBED-LINE-NUMBER represents a valid
   zero-based index into the Braine PROGRAM's lines, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  probed-line-number))
  (the boolean
    (with-slots (lines) program
      (declare (type line-vector lines))
      (not (null
        (array-in-bounds-p lines probed-line-number))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-spaces (source start)
  "Proceeding from the START positon into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position in the
   SOURCE immediately succeeding the last thus consumed space
   character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (space-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun end-of-source-p (source position)
  "Determines whether the POSITION specifies a location outside of the
   SOURCE, by mediation of this fact being tantamount to the SOURCE's
   exhaustion, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (array-in-bounds-p source position))))

;;; -------------------------------------------------------

(defun expect-colon (source position)
  "Determines whether a colon (\":\") character resides at the POSITION
   into the SOURCE, on confirmation simply returning no value, otherwise
   signaling an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum position))
  (cond
    ((end-of-source-p source position)
      (error "Expected a colon (\":\") at position ~d, ~
              but encountered end of source."
        position))
    ((char/= (char source position) #\:)
      (error "Expected a colon (\":\") at position ~d, ~
              but encountered the character \"~c\"."
        position
        (char source position)))
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defun read-unsigned-integer-number (source start)
  "Proceeding from the START position into the SOURCE, reads an unsigned
   integer number and returns two values:
     (1) The detected and parsed unsigned integer object.
     (2) The position into the SOURCE immediately succeeding the
         matching tmema."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (integer 0 *) fixnum)
    (parse-integer source :start start :end
      (or (position-if-not #'digit-char-p source :start start)
          (length source)))))

;;; -------------------------------------------------------

(defun read-line-number (source start)
  "Proceeding from the START position into the SOURCE, reads an unsigned
   integer number of any positive length and returns two values:
     (1) The parsed integer value, representative of a line number.
     (2) The position into the SOURCE immediately succeeding the
         matching section."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values integer fixnum)
    (read-unsigned-integer-number source start)))

;;; -------------------------------------------------------

(defun expect-end-of-line (source start)
  "Determines whether, proceeding from the START position into the
   SOURCE, no content, except for contingent spaces, follows, returning
   on confirmation no value; otherwise an error of an unspecified type
   is signaled."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-spaces source start)))
    (declare (type fixnum position))
    (unless (end-of-source-p source position)
      (error "Expected end of line, but encountered the character ~
              \"~c\" at position ~d."
        (char source position)
        position)))
  (values))

;;; -------------------------------------------------------

(defun validate-byte (candidate)
  "Determines whether the CANDIDATE represents a valid unsigned byte
   number, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type integer candidate))
  (the octet
    (or (and (typep candidate 'octet)
             candidate)
        (error "The value ~d does not represent an unsigned byte."
          candidate))))

;;; -------------------------------------------------------

(defun read-byte-value (source start)
  "Proceeding from the START position into the SOURCE, reads an unsigned
   byte value, that is, a commorant of the closed interval [0, 255], and
   returns two values:
     (1) The detected octet value.
     (2) The position into the SOURCE immediately succeeding the
         matched parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values octet fixnum)
    (cond
      ((end-of-source-p source start)
        (error "Expected an unsigned byte at the position ~d, ~
                but encountered the end of the source."
          start))
      ((digit-char-p (char source start))
        (multiple-value-bind (parsed-number new-position)
            (read-unsigned-integer-number source start)
          (declare (type (integer 0 *) parsed-number))
          (declare (type fixnum        new-position))
          (values
            (validate-byte parsed-number)
            new-position)))
      (T
        (error "Expected an unsigned byte at the position ~d, ~
                but encountered the character \"~c\"."
          start
          (char source start))))))

;;; -------------------------------------------------------

(defun read-goto-instruction (source start)
  "Proceeding from the START position into the SOURCE, reads a goto
   instruction and returns two values:
     (1) An ``instruction'' representation of the command.
     (2) The position into the SOURCE immediately succeeding the
         matching section.
   ---
   This operation expects the START location to reside exactly at an
   instruction's inchoation, devoid of contingently preceding spaces,
   and ascertained to not amount to a blank SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((predicate-byte (read-byte-value source start))
        (position       start))
    (declare (type octet  predicate-byte))
    (declare (type fixnum position))
    ;; Skip the already consumed byte's position.
    (incf position)
    ;; Ascertain and consume the colon (":").
    (setf position (skip-spaces source position))
    (expect-colon source position)
    (incf position)
    ;; Ascertain and consume the line number.
    (setf position (skip-spaces source position))
    (multiple-value-bind (operand new-position)
        (read-line-number source position)
      (declare (type (integer 0 *) operand))
      (declare (type fixnum        new-position))
      (the (values Instruction fixnum)
        (values
          (make-goto-instruction predicate-byte operand)
          new-position)))))

;;; -------------------------------------------------------

(defun read-instruction (source start)
  "Proceeding from the START position into the SOURCE, reads an
   instruction and returns two values:
     (1) An ``instruction'' representation of the detected command.
     (2) The position into the SOURCE immediately succeeding the
         matching section.
   ---
   This operation expects the START location to reside exactly at an
   instruction's inchoation, devoid of contingently preceding spaces,
   and ascertained to not amount to a blank SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Instruction fixnum)
    (case (char source start)
      (#\+
        (values
          (make-increment-instruction)
          (1+ start)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        (read-goto-instruction source start))
      (otherwise
        (error "Expected an instruction, but encountered the ~
                character \"~c\" at position ~d."
          (char source start)
          start)))))

;;; -------------------------------------------------------

(defun read-program-line (line)
  "Returns the Braine instructions contained in the LINE as a
   ``Program-Line'', or responds with ``NIL'' for a blank input."
  (declare (type string line))
  (the (or null Program-Line)
    (loop
      with position
        of-type fixnum
        =       (skip-spaces line 0)
      while
        (< position (length line))
      collect
        (multiple-value-bind (next-instruction new-position)
            (read-instruction line position)
          (declare (type Instruction next-instruction))
          (declare (type fixnum      new-position))
          (prog1 next-instruction
            (setf position
              (skip-spaces line new-position))))
        into instructions
      finally
        (return
          (when instructions
            (make-program-line instructions))))))

;;; -------------------------------------------------------

(defun extract-instructions (source)
  "Extracts from the Braine SOURCE its contained lines and instructions
   and returns a ``Program'' encapsulation thereof."
  (declare (type string source))
  (with-input-from-string (source-stream source)
    (declare (type string-stream))
    (the Program
      (loop
        for current-line
          of-type (or null string)
          =       (read-line source-stream NIL NIL)
        while current-line
        for current-program-line
          of-type (or null Program-Line)
          =       (read-program-line current-line)
        when current-program-line
          collect current-program-line
          into    program-lines
        finally
          (return
            (make-program
              (coerce program-lines
                '(simple-array Program-Line (*)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction pointer (IP).                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction-Pointer
  (:constructor make-instruction-pointer ())
  (:conc-name   ip-))
  "The ``Instruction-Pointer'' class realizes a two-dimensional
   instruction pointer (IP), its diorism encompassing the twissel of a
   zero-based line number and the zero-based index of the command inwith
   thilk the pointer resides."
  (line-number       0 :type integer :read-only NIL)
  (instruction-index 0 :type integer :read-only NIL))

;;; -------------------------------------------------------

(defun advance-ip-to-next-instruction (ip)
  "Advances the instruction pointer IP to the next position and returns
   no value."
  (declare (type Instruction-Pointer ip))
  (incf (ip-instruction-index ip))
  (values))

;;; -------------------------------------------------------

(defun advance-ip-to-next-line (ip)
  "Advances the instruction pointer IP to the start of the next line and
   returns no value."
  (declare (type Instruction-Pointer ip))
  (incf (ip-line-number       ip))
  (setf (ip-instruction-index ip) 0)
  (values))

;;; -------------------------------------------------------

(defun move-ip-to-line (ip new-line-number)
  "Moves the instruction pointer IP to the start of the line amenable
   to the zero-based NEW-LINE-NUMBER and returns no value."
  (declare (type Instruction-Pointer ip))
  (psetf (ip-line-number       ip) new-line-number
         (ip-instruction-index ip) 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (real 0 *) +OUTPUT-DELAY-IN-SECONDS+))

;;; -------------------------------------------------------

(defparameter +OUTPUT-DELAY-IN-SECONDS+ 0.1
  "The tally of seconds to wait succeeding each printing of the
   interpreter's state.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing interpreter program.")
    :reader        interpreter-program
    :type          Program
    :documentation "The Braine program as a vector of instruction
                    lines.")
   (ip
    :initform      (make-instruction-pointer)
    :reader        interpreter-ip
    :type          Instruction-Pointer
    :documentation "The two-dimensional instruction pointer (IP),
                    its componency a castaldy of the zero-based current
                    line index into the PROGRAM and the zero-based
                    current instruction index into the selected line's
                    command vector.")
   (current-byte
    :initform      0
    :reader        get-current-byte
    :type          octet
    :documentation "The register, or \"current byte\"."))
  (:documentation
    "The ``Interpreter'' class is apportioned that dever to accompass
     actual efficacy to a Braine program's static production as a
     sequence of instruction lines."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Braine
   PROGRAM's execution."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun get-current-line-number (interpreter)
  "Returns the zero-based line index into the INTERPRETER's program."
  (declare (type Interpreter interpreter))
  (the fixnum
    (ip-line-number
      (interpreter-ip interpreter))))

;;; -------------------------------------------------------

(defun get-current-line (interpreter)
  "Returns the currently processed line into the INTERPRETER's program."
  (declare (type Interpreter interpreter))
  (the Program-Line
    (program-line-at
      (interpreter-program     interpreter)
      (get-current-line-number interpreter))))

;;; -------------------------------------------------------

(defun get-current-instruction-index (interpreter)
  "Returns the zero-based index of the contemporaneously selected
   instruction in the current line of the INTERPRETER's program."
  (declare (type Interpreter interpreter))
  (the fixnum
    (ip-instruction-index
      (interpreter-ip interpreter))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the currently processed instruction commorant in the
   INTERPRETER program's contemporaneously selected line."
  (declare (type Interpreter interpreter))
  (the Instruction
    (program-instruction-at
      (interpreter-program           interpreter)
      (get-current-line-number       interpreter)
      (get-current-instruction-index interpreter))))

;;; -------------------------------------------------------

(defun current-line-is-completed-p (interpreter)
  "Determines whether the INTERPRETER's current program line has been
   processed in its entirety, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (valid-program-line-instruction-index-p
        (get-current-line              interpreter)
        (get-current-instruction-index interpreter)))))

;;; -------------------------------------------------------

(defun program-is-completed-p (interpreter)
  "Determines whether the INTERPRETER's program has been processed in
   its entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (valid-program-line-number-p
        (interpreter-program     interpreter)
        (get-current-line-number interpreter)))))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   instruction on its current line, if possible, otherwise descends to
   the next lower row's incipiency, in any case returning no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip) interpreter
    (declare (type Instruction-Pointer ip))
    (advance-ip-to-next-instruction ip)
    (when (current-line-is-completed-p interpreter)
      (advance-ip-to-next-line ip)))
  (values))

;;; -------------------------------------------------------

(defun go-to-line (interpreter new-line-number)
  "Relocates the INTERPRETER's instruction pointer (IP) to the start of
   the line amenable to the one-based NEW-LINE-NUMBER and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-line-number))
  (with-slots (ip) interpreter
    (declare (type Instruction-Pointer ip))
    (move-ip-to-line ip
      (1- new-line-number)))
  (values))

;;; -------------------------------------------------------

(defun increment-current-byte (interpreter)
  "Increments the INTERPRETER's memory, the \"current byte\", by one,
   contingently wrapping around its state from the inclusive upper
   bourne of 255 to the lower extremum of zero (0), and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (current-byte) interpreter
    (declare (type octet current-byte))
    (setf current-byte
      (mod (1+ current-byte) 256)))
  (values))

;;; -------------------------------------------------------

(defun print-current-state (interpreter)
  "Prints the state of the INTERPRETER to the standard output and
   returns no value."
  (declare (type Interpreter interpreter))
  (format T "~&Line number: ~d, current byte = ~d"
    (1+ (get-current-line-number interpreter))
    (get-current-byte interpreter))
  (sleep +OUTPUT-DELAY-IN-SECONDS+)
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Processes the Braine INSTRUCTION in the INTERPRETER's context and
     returns no value.")
  
  (:method ((interpreter Interpreter)
            (instruction Increment-Instruction))
    (declare (type Interpreter           interpreter))
    (declare (type Increment-Instruction instruction))
    (increment-current-byte      interpreter)
    (advance-to-next-instruction interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Goto-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (type Goto-Instruction instruction))
    (if (= (get-current-byte interpreter)
           (goto-instruction-guard instruction))
      (go-to-line interpreter
        (goto-instruction-destination instruction))
      (advance-to-next-instruction interpreter))
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Braine program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-is-completed-p interpreter) do
    (print-current-state interpreter)
    (process-instruction interpreter
      (get-current-instruction interpreter)))
  (print-current-state interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpret-Braine (code)
  "Interprets the piece of Braine source CODE and returns no value.
   ---
   As a prevenience to any instruction's execution, and as an ultimity
   prior to the interpretation's patration, the program state, its
   amplectation laid around the contemporaneous line number and memory
   byte value, is issued to the standard output."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increment the byte state to two (2), repeat once because of this
;; condition, and terminate the program.
(interpret-Braine
  "++
   2:1")
