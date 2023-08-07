;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deadfish 3", invented by the Esolang user "A" and presented
;; on June 23rd, 2019, inspired by Jonathan Todd Skinner's original
;; "Deadfish" language, while the derivative's dioristic attribute wones
;; in its incorporation of a "clipboard" as a readable and writable
;; string storage, capable of printing as well as an interpretation as
;; a piece of Deadfish 3 code itself in order to dynamically inject
;; subprograms.
;; 
;; 
;; Concept
;; =======
;; Deadfish 3, a derivative of Deadfish 2 by the Esolang user "m654"
;; that augmented the original Deadfish language by string input and
;; output facilities, administers a further stratum of supererogation by
;; introducing the "clipboard" concept: a string storage for input,
;; output, and dynamic code insertion.
;; 
;; == THE CLIPBOARD: A STRING STORAGE ==
;; Peisant consequences are elicited by the clipboard's competences: Its
;; capabilities to embrace both literal strings and user input proceed
;; in mimicry of an operation system's eponymous temporary salvatory.
;; A rather sensible epiphenomenon of its definition, the verbatim
;; output permits arbitrary test messages.
;; 
;; A second, infinitely more potent dation of its interpretation, the
;; clipboard content's copying transfer, or "pasting", into the
;; currently executing Deadfish 3's source code invests the language
;; with the possibility of dynamic code insertion, evaluation, and
;; procession.
;; 
;; == CLIPBOARD-PASTED PROGRAMS: COHERENT UNITS ==
;; A significant object of deliberation reacts in this dynamic code
;; generation, forecause the pasted clipboard content --- while
;; visually, aiblins --- a mere sequence of characters, must be
;; respected in its coherence and considered as a unit:
;; 
;;   (1) The clipboard-inserted code, in its surrounding program's
;;       environment, establishes a single command. If, a forbisen
;;       stated, the skip instruction (":") accompasses, the complete
;;       pasted section is skipped, not merely its incipient token.
;;   (2) The clipboard code defines an independently executing program.
;;       The ramification of this piece of gnarity answers in particular
;;       to the halting command "h", which terminates the currently
;;       active program or subprogram. If executed in the main program,
;;       the complete interpretation procedure ceases; its encounter in
;;       a clipboard-pasted segment, on the other hand, stops this
;;       subprogram, while the ensconcing execution level perpetuates.
;; 
;; 
;; Architecture
;; ============
;; Deadfish 3's architecture enumerates a componancy of two distinct
;; storage entities: the integer-valued accumulator, appropriated from
;; its entheus, and a newly introduced "clipboard" for communicating
;; strings.
;; 
;; == THE ACCUMULATOR: AN INTEGER STORAGE ==
;; A dation by its stock-father, the accumulator as an integer castaldy
;; experiences a promoted competence in its capacity for unbounded
;; negative values and arbitrary positive acquisitions.
;; 
;; The original Deadfish implementation involves the imposition to
;; relapse into the zero-valued state in two cases:
;; 
;;   (1) The accumulator value being equal to -1.
;;   (2) The accumulator value being equal to 256.
;; 
;; The premier situation, in conjunction with gradual decrementation as
;; the sole medium for deductions, renders a negative state's assumption
;; a thing of inconceivable attainment. Deadfish 3 diverges from this
;; deportment by homologating any state in the accumulator.
;; 
;; The second diorism appertaining to the memory handling, its resetting
;; for a positive value of 256, is siclike suppressed in the scion:
;; Deadfish 3, again, does not incorporate such a rule.
;; 
;; == THE CLIPBOARD: A STRING STORAGE ==
;; The clipboard salvatory supplies a novelty in its maintenance of a
;; string datum, amenable to the reception of strings in literal or user
;; input form, and their verbatim display on the standard output. More
;; peisant an expression thereof, the content may be pasted directly
;; into the executing Deadfish 3 program, evaluated as a subprogram, and
;; operated accordingly.
;; 
;; 
;; Instructions
;; ============
;; Deadfish 3's instruction set tallies a duodecimal account of members,
;; encompassing basic arithmetics, input and output, subprogram
;; generation and management, as well as control flow stewardry.
;; 
;; == OVERVIEW ==
;; An apercu shall administer a foundational acquaintance with the
;; language's operations, ere certain aspects are to enjoy the dation of
;; further gnarity.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator by one.
;;   ..................................................................
;;   d       | Decrements the accumulator by one.
;;   ..................................................................
;;   s       | Squares the accumulator value, that is, sets it to the
;;           | product of its value multiplied by itself.
;;   ..................................................................
;;   n       | Resets the accumulator to its default value of zero (0).
;;   ..................................................................
;;   o       | Outputs the accumulator value verbatim as a number.
;;   ..................................................................
;;   c       | Outputs the accumulator value in character form, that
;;           | is, print the character whose ASCII code equals the
;;           | value.
;;   ..................................................................
;;   "text"  | Copies the string {text} to the clipboard.
;;    ****   | 
;;   ..................................................................
;;   r       | Queries the standard input for a line of characters and
;;           | stores this string in the clipboard.
;;   ..................................................................
;;   O       | Outputs the clipboard content as a string.
;;   ..................................................................
;;   @       | Pastes the content of the clipboard after this command's
;;           | position.
;;           | Please heed that the coherence of the thus inserted code
;;           | segment must be preserved as an independent program.
;;   ..................................................................
;;   :       | If the accumulator value equals zero (0), execute the
;;           | next command; otherwise skip the same.
;;           | Please heed that clipboard-pasted instructions are
;;           | considered as a single command in this context.
;;   ..................................................................
;;   h       | Terminates the currently active program or subprogram,
;;           | the latter of which refers to code generated by
;;           | pasting and interpreting the clipboard content.
;;   ------------------------------------------------------------------
;; 
;; == CLIPBOARD INPUT AND OUTPUT RELATIONS ==
;; An illustration shall ostend a visualization of the quadruple
;; commands directly involved with the clipboard's handling:
;; 
;;   +-------+                   +-------+
;;   | "..." |                   |   r   |
;;   +-------+                   +-------+
;;       | Copy string               | Read input and
;;       | to clipboard              | store in clipboard
;;       V                           V
;;   +-----------------------------------+
;;   |                                   |
;;   |             CLIPBOARD             |
;;   |                                   |
;;   +-----------------------------------+
;;       | Output clipboard          | Paste clipboard
;;       | content                   | as code into program
;;       V                           V
;;   +-------+                   +-------+
;;   |   O   |                   |   @   |
;;   +-------+                   +-------+
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The confrontation with the lucid explications of the Deadfish 3
;; language currently disencumbers the protolog from any major
;; frailties.
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been realized in Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-04
;; 
;; Sources:
;;   [esolang2019Deadfish2]
;;   The Esolang contributors, "Deadfish 3", 14 August 2019
;;   URL: "https://esolangs.org/wiki/Deadfish_2"
;;   Notes:
;;     - The Deadfish 2 programming language specification.
;;   
;;   [esolang2023Deadfish3]
;;   The Esolang contributors, "Deadfish 3", 15 May 2023
;;   URL: "https://esolangs.org/wiki/Deadfish_3"
;;   Notes:
;;     - The Deadfish 3 programming language specification.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   that conform to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized variants of
   Deadfish 3 commands."
  '(member
    :increment-accumulator
    :decrement-accumulator
    :square-accumulator
    :output-accumulator-number
    :output-accumulator-character
    :output-clipboard
    :reset-accumulator
    :read-into-clipboard
    :copy-to-clipboard
    :paste-from-clipboard
    :execute-if-zero
    :halt
    :nop))

;;; -------------------------------------------------------

(deftype command-argument ()
  "The ``command-argument'' type defines the types admissive as an
   argument to a Deadfish 3's ``Command'' representation."
  '(or null string))

;;; -------------------------------------------------------

(deftype command-vector ()
  "The ``command-vector'' type defines a vector of zero or more
   ``Command'' objects."
  '(vector Command *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (type argument)))
  "The ``Command'' class attends to the encapsulation of a Deadfish 3
   command, delineated by its type and an optional argument."
  (type     (error "Missing command type.") :type command-type)
  (argument NIL                             :type command-argument))

;;; -------------------------------------------------------

(defun make-command-vector (commands)
  "Creates and returns a command vector from the list of COMMANDS."
  (declare (type (list-of Command) commands))
  (the command-vector
    (make-array (length commands)
      :element-type     'Command
      :initial-contents commands
      :adjustable       NIL
      :fill-pointer     NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts and returns the commands from a piece of Deadfish 3 source
   CODE."
  (declare (type string code))
  (let ((commands NIL)
        (position 0))
    (declare (type (list-of Command) commands))
    (declare (type fixnum            position))
    (flet ((get-current-token ()
            "Returns the character at the current POSITION into the
             CODE, or ``NIL'' if the location transgresses the CODE's
             boundaries."
            (the (or null character)
              (when (array-in-bounds-p code position)
                (char code position))))
           
           (advance ()
            "Moves the POSITION cursor to the next location in the CODE
             and returns no value."
            (incf position)
            (values))
           
           (collect-command (type &optional (argument NIL))
            "Creates a new ``Command'' specifed by the mandatory TYPE
             and an optional ARGUMENT, inserts the same at the COMMANDS
             list's head, and returns no value."
            (declare (type command-type     type))
            (declare (type command-argument argument))
            (push (make-command type argument) commands)
            (values)))
      
      (loop while (< position (length code)) do
        (case (get-current-token)
          ((NIL)
            (loop-finish))
          
          (#\i
            (collect-command :increment-accumulator)
            (advance))
          
          (#\d
            (collect-command :decrement-accumulator)
            (advance))
          
          (#\s
            (collect-command :square-accumulator)
            (advance))
          
          (#\o
            (collect-command :output-accumulator-number)
            (advance))
          
          (#\c
            (collect-command :output-accumulator-character)
            (advance))
          
          (#\O
            (collect-command :output-clipboard)
            (advance))
          
          (#\n
            (collect-command :reset-accumulator)
            (advance))
          
          (#\r
            (collect-command :read-into-clipboard)
            (advance))
          
          (#\"
            (advance)
            (collect-command :copy-to-clipboard
              (with-output-to-string (inserted-code)
                (declare (type string-stream inserted-code))
                (loop do
                  (case (get-current-token)
                    ((NIL)
                      (error "Unterminated string literal."))
                    (#\"
                      (advance)
                      (loop-finish))
                    (otherwise
                      (write-char (get-current-token) inserted-code)
                      (advance)))))))
          
          (#\@
            (collect-command :paste-from-clipboard)
            (advance))
          
          (#\:
            (collect-command :execute-if-zero)
            (advance))
          
          (#\h
            (collect-command :halt)
            (advance))
          
          (otherwise
            (collect-command :nop)
            (advance)))))
    
    (the command-vector
      (make-command-vector
        (nreverse commands)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Program) (values)) program-execute))
(declaim (ftype (function (command-vector Interpreter)
                          (values Program &optional))
                make-program))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((accumulator
    :initform      0
    :accessor      interpreter-accumulator
    :type          integer
    :documentation "The signed-integer-valued accumulator.")
   (clipboard
    :initform      ""
    :accessor      interpreter-clipboard
    :type          string
    :documentation "The clipboard content."))
  (:documentation
    "The ``Interpreter'' class is dedicated to the application of effect
     to a sequence of Deadfish 3 commands, maintaining the program
     memory and execution state."))

;;; -------------------------------------------------------

(defun make-interpreter ()
  "Creates and returns a new ``Interpreter''."
  (the Interpreter
    (make-instance 'Interpreter)))

;;; -------------------------------------------------------

(defun interpreter-execute (interpreter commands)
  "Executes the Deadfish 3 COMMANDS in the INTERPRETER's context and
   returns no value."
  (declare (type Interpreter    interpreter))
  (declare (type command-vector commands))
  (fresh-line)
  (program-execute
    (make-program commands interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program
  (:constructor make-program (commands interpreter)))
  "The ``Program'' class represents a coherent sequence of Deadfish 3
   instructions in an exectuable environment.
   ---
   This self-sufficiency homologates the creation of subprograms,
   begotten by pasting clipboard content as codes, which operate as
   independent units."
  (commands     (error "Missing commands.")    :type command-vector)
  (ip           0                              :type fixnum)
  (terminated-p NIL                            :type boolean)
  (interpreter  (error "Missing interpreter.") :type Interpreter))

;;; -------------------------------------------------------

(defun program-finished-p (program)
  "Determines whether the PROGRAM has finished, wether by an explicit
   halt command's mediation, or by its command vector's exhaustion,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Program program))
  (the boolean
    (not (null
      (or (program-terminated-p program)
          (>= (program-ip program)
              (length (program-commands program))))))))

;;; -------------------------------------------------------

(defun program-advance (program)
  "Moves the PROGRAM's instruction pointer (IP) to the next position in
   its command vector and returns no value."
  (declare (type Program program))
  (incf (program-ip program))
  (values))

;;; -------------------------------------------------------

(defun program-current-command (program)
  "Returns the command at the PROGRAM's instruction pointer (IP), or
   ``NIL'' if the command vector is exhausted."
  (declare (type Program program))
  (the (or null Command)
    (when (array-in-bounds-p
            (program-commands program)
            (program-ip       program))
      (aref
        (program-commands program)
        (program-ip program)))))

;;; -------------------------------------------------------

(defun program-skip-nop-commands (program)
  "Proceeding from the current position into the PROGRAM's commands
   vector, skips a sequence of zero or more accolent no-operation (NOP)
   instructions and returns no value."
  (declare (type Program program))
  (loop
    for current-command
      of-type (or null Command)
      =       (program-current-command program)
    while (and current-command
               (eq (command-type current-command) :nop))
    do (program-advance program))
  (values))

;;; -------------------------------------------------------

(defun program-execute (program)
  "Executes the Deadfish 3 PROGRAM and returns no value."
  (declare (type Program program))
  (loop
    until (program-finished-p program)
    for   command of-type Command = (program-current-command program)
    do
      (case (command-type command)
        ((NIL)
          (loop-finish))
        
        (:increment-accumulator
          (incf (interpreter-accumulator (program-interpreter program)))
          (program-advance program))
        
        (:decrement-accumulator
          (decf (interpreter-accumulator (program-interpreter program)))
          (program-advance program))
        
        (:square-accumulator
          (setf (interpreter-accumulator (program-interpreter program))
            (* (interpreter-accumulator (program-interpreter program))
               (interpreter-accumulator (program-interpreter program))))
          (program-advance program))
        
        (:output-accumulator-number
          (format T "~&~d"
            (interpreter-accumulator
              (program-interpreter program)))
          (program-advance program))
        
        (:output-accumulator-character
          (format T "~c"
            (code-char
              (interpreter-accumulator
                (program-interpreter program))))
          (program-advance program))
        
        (:output-clipboard
          (format T "~&~a"
            (interpreter-clipboard
              (program-interpreter program)))
          (program-advance program))
        
        (:reset-accumulator
          (setf (interpreter-accumulator (program-interpreter program))
                0)
          (program-advance program))
        
        (:read-into-clipboard
          (setf (interpreter-clipboard (program-interpreter program))
            (read-line))
          (program-advance program))
        
        (:copy-to-clipboard
          (setf (interpreter-clipboard (program-interpreter program))
            (command-argument command))
          (program-advance program))
        
        (:paste-from-clipboard
          (program-execute
            (make-program
              (extract-commands
                (interpreter-clipboard
                  (program-interpreter program)))
              (program-interpreter program)))
          (program-advance program))
        
        (:execute-if-zero
          (program-advance program)
          (unless (zerop
                    (interpreter-accumulator
                      (program-interpreter program)))
            (program-skip-nop-commands program)
            (program-advance           program)))
        
        (:halt
          (setf (program-terminated-p program) T)
          (program-advance program))
        
        (:nop
          (program-advance program))
        
        (otherwise
          (error "Invalid command ~s at position ~d."
            (program-current-command program)
            (program-ip              program)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Deadfish-3 (code)
  "Interprets the piece of Deadfish 3 source CODE and returns no value."
  (declare (type string code))
  (interpreter-execute
    (make-interpreter)
    (extract-commands code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello world".
(interpret-Deadfish-3 "iisiiiisiiiiiiiiciiiiiiiiiiiiiiiiiiiiiiiiiiiiiciiiiiiicciiicdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddcdddddddddddddddddddddsddcddddddddciiicddddddcddddddddch")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Deadfish-3 "rOh")

;;; -------------------------------------------------------

;; Increment the accumulator by one in the main program, insert through
;; the clipboard a subprogram which further increments the accumulator
;; thrice to the value of four (4) and prints its state, but which halts
;; ere its desinent accrementation, and finally output the clipboard
;; content itself.
(interpret-Deadfish-3 "i\"iiiohio\"@O")

;;; -------------------------------------------------------

;; Increment the accumulator to one (1), thus skipping the clipboard
;; code's insertion, which would increment the accumulator thrice, and
;; print the accumulator, whose value has remained at one.
(interpret-Deadfish-3 "\"iii\" i : @o")

;;; -------------------------------------------------------

;; This program simulates a truth-machine by employing dynamic code
;; insertion for prosecuting the manipulation of the accumulator, and
;; latter naiting the same technique for recursively printing the value
;; "1".
;; 
;; The procedure following an explicating text output, adheres to the
;; following principles:
;; 
;;   ------------------------------------------------------------------
;;   Step | Commands  | Actions
;;   -----+-----------+------------------------------------------------
;;   1    | d         | Set the accumulator to -1.
;;        |           | This accomplishes for a user input code of "i",
;;        |           | which shall represent the truth-machine input
;;        |           | of "1" (one), the tacit incrementing to zero
;;        |           | (0) for the future conditionals.
;;   ..................................................................
;;   2    | r         | Query the user for a string input to copy to
;;        |           | the clipboard. The user shall respond with a
;;        |           | blank string for an equivalent of  the
;;        |           | truth-machine input "0". In order to simulate
;;        |           | "1", a reply with a single "i" shall be
;;        |           | committed.
;;   ..................................................................
;;   3    | @         | Paste and evaluate the user input string from
;;        |           | the clipboard. It it comprehends no accumulator
;;        |           | manipulation operations, the truth-machine
;;        |           | value of zero (0) is simulated by an
;;        |           | accumulator state of -1. Otherwise, if the user
;;        |           | has issued an "i" command, the accumulator is
;;        |           | incremented to zero (0), the equivalent of a
;;        |           | truth-machine's "1".
;;   ..................................................................
;;   4    | :\"iod@\" | If the accumulator assumes a value of zero (0),
;;        |           | forecause the user has supplied an "i" input to
;;        |           | the clipboard, the command sequence \"iod@\"
;;        |           | supplants the clipboard content, the same
;;        |           | prints the number "1" by intermittently raising
;;        |           | its value from zero (0) to one (1), outputting
;;        |           | the numeric value, returning it to -1, and
;;        |           | recursively inserting and pasting the same
;;        |           | code sequence \"iod@\" for repeated printing.
;;        |           | Note that this command sequence is not pasted
;;        |           | yet, and thus does no accompass the program.
;;        |           | If the clipboard is empty, as the user has
;;        |           | issued a blank response, this section is
;;        |           | skipped.
;;   ..................................................................
;;   5    | :@        | If the accumulator assumes a value of zero (0),
;;        |           | forecause the user has supplied an "i" input to
;;        |           | the clipboard, the command sequence \"iod@\",
;;        |           | stored in the clipboard in the preceding step
;;        |           | (4), is pasted, the same prints the number "1"
;;        |           | by intermittently raising its value from zero
;;        |           | (0) to one (1), outputting the numeric value,
;;        |           | returning it to -1, and recursively inserting
;;        |           | and pasting the same code sequence \"iod@\" for
;;        |           | repeated printing.
;;        |           | If the clipboard is empty, as the user has
;;        |           | issued a blank response, this section is
;;        |           | skipped.
;;   ..................................................................
;;   6    | no        | This section is only reached if the user has
;;        |           | issued no input, tantamount to the
;;        |           | truth-machine case of "0". The accumulator is
;;        |           | reset to zero (0) and printed, ere the program
;;        |           | naturally ceases its operation.
;;   ------------------------------------------------------------------
;; 
;; Please note that in Deadfish 3 implementation which employ recursion
;; for clipboard-based subprogram executions, such as this one at hand,
;; the probability for a stack overflow looms very cogently.
(interpret-Deadfish-3
  "\"Please input 'i' to simulate '1', otherwise nothing: \" O
   d r@ \"iod@\" :@ no")
