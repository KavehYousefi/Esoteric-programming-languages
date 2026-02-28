;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deadfish+", invented by the Esolang user "Tommyaweosme"
;; and presented on June 29th, 2025, its haecceity's patefaction
;; discernible in the extension of Jonathan Todd Skinner's "Deadfish"
;; by a more potent integer-valued accumulator, a stack comprehending
;; elements of such species, and two control flow duction operations.
;; 
;; 
;; Concept
;; =======
;; The Deadfish+ programming language constitutes a derivative and
;; extension of Deadfish, superior in its operative competences,
;; including, as a paravaunt polymechany, an iterance construct, and
;; the memory's composition, deloying, besides a more potent
;; accumulator, a stack of integer numbers.
;; 
;; == DEADFISH+: DEADFISH + FOUR TELOI ==
;; Deadfish+, edified upon the firmament accommodated by Deadfish,
;; extends its entheus' capacities in the bailiwicks covering the
;; control flow conduction as well as the memory architecture in order
;; to satisfy a tesseratomy of telea:
;; 
;;   (1) The achievement of Turing-completeness.
;;   
;;   (2) The development on Deadfish's substratum.
;;   
;;   (3) The retention of simplicity.
;;   
;;   (4) The expression of instructions exclusively in letters.
;; 
;; == DEADFISH+ PROGRAMS OPERATE IN A PERPETUAL CYCLE ==
;; A sience the kenspeckle Deadfish nomothesy, Deadfish+'s appropriation
;; does not encumber itself with the wite of its dioristic execution
;; model's renegation; pursing the selfsame olamic designment in the
;; gestion:
;; 
;;   (1) PROGRAM ACQUISITION:
;;       In its incipiency, the interpreter directs a request at the
;;       standard input for a sequence comprehending zero or more
;;       characters, the preposition to whom manifests in the prompt
;;       message
;;       
;;         >>
;;       
;;       The character sequence whose gendrure represents the query's
;;       response serves as the quesited program string to evaluate.
;;   
;;   (2) PROGRAM EXECUTION:
;;       The instructions participating in the received program are
;;       subjected to their evaluation and their allied epiphenomena's
;;       immediate execution.
;;       
;;       Unrecognized symbol, such thole an eloignment from the native
;;       instruction set and the advenient function definitions, serve
;;       in a newline output's issuance each.
;;   
;;   (3) REPETITION:
;;       A sequela obtained from the prevenient execution, the secle's
;;       iterum application emerges, returning to the step -> (1) as
;;       the inchoacy's reduplication.
;; 
;; == UNRECOGNIZED CHARACTERS INSTIGATE A NEWLINE OUTPUT ==
;; Any symbol to whom attendance no causatum's bailiwick, either from
;; the native instruction set, or the, its ponibility the encounter's
;; instant, a bespoke function agnomination, may be supputated a nexible
;; fact, will serve in a newline character's issuance to the standard
;; output conduit, rather than the actual abortive error whose
;; participation would install many cotidian programming language's
;; respondency.
;; 
;; == THE MEMORY: TWYFORKED INTO ACCUMULATOR AND STACK ==
;; Deadfish+'s memory model ostends a componency endowed with higher
;; sophistication as juxtaposed with its entheus: the traditional
;; accumulator, and an adscititious integer-valued stack.
;; 
;; == THE MEMORY'S ACCUMULATOR: A SCALAR REGISTER ==
;; The scalar integer accumulator, now disencumbered from the peculiar
;; resetting rule for the values -1 and 256, homologates any magnitude
;; and polarity in its datum's constitution.
;; 
;; == THE MEMORY'S STACK: A SALVATORY TO INTEGER NUMBERS ==
;; An enhaused capacity's pernor, the stack furnishes a last in, first
;; out species of data castaldy, the integral elements of which wist of
;; no disqualification's tholance anenst their magnitude's mountenance.
;; 
;; 
;; Instructions
;; ============
;; The Deadfish+ programming language's instruction set enumerates a
;; duodecimal accompt in membership, a quadruple subset the
;; stock-father's vouchsafement, manumitted, however, from the dioristic
;; accumulator resetting nomothesy, while concomitantly the novel
;; polymechanies propine such capacitations as to homologate the
;; collaboration atwixen the accumulator and the stack, the latter's
;; exclusive manipulation, input obtainal, immediate program
;; terminations, as well as a "while" iterance construct.
;; 
;; == OVERVIEW ==
;; The alow apercu's dation shall be satisfied in a requisite mete of
;; nortelry's adhibition anent the language's operative dations.
;; 
;; Please heed the demarcation of succedaneous tmemata by catenas
;; composed of asterisks ("*"), the dispansions of whom are expected to
;; be substituted by actual Deadfish+ code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ==================================================================
;;   ARITHMETICS AND ASSIGNMENTS
;;   ------------------------------------------------------------------
;;   i              | Increments the accumulator value by one (1).
;;   ..................................................................
;;   d              | Decrements the accumulator value by one (1).
;;   ..................................................................
;;   s              | Squares the accumulator value; scilicet, sets its
;;                  | state to its current value multiplied by itself.
;;   ..................................................................
;;   c              | Resets the accumulator to its inchoate state of
;;                  | zero (0).
;;   ==================================================================
;;   STACK MANIPULATION
;;   ------------------------------------------------------------------
;;   w              | Swaps the positions of the two top stack
;;                  | elements.
;;                  |--------------------------------------------------
;;                  | If the stack cannot accommodate at least two
;;                  | elements in the course of this operation's
;;                  | execution, an abortive error of the type
;;                  | "EmptyStackError" is signaled during the
;;                  | violating attempt.
;;   ..................................................................
;;   t              | Duplicates the top stack element.
;;                  |--------------------------------------------------
;;                  | If the stack is empty at the instant of this
;;                  | operation's invocation, an abortive error of the
;;                  | type "EmptyStackError" is signaled.
;;   ==================================================================
;;   ACCUMULATOR-STACK COLLABORATION
;;   ------------------------------------------------------------------
;;   o              | Pushes the accumulator value onto the stack.
;;   ..................................................................
;;   r              | Pops the top stack element and sets the
;;                  | accumulator value to the same.
;;                  |--------------------------------------------------
;;                  | If the stack is empty at the instant of this
;;                  | operation's invocation, an abortive error of the
;;                  | type "EmptyStackError" is signaled.
;;   ==================================================================
;;   INPUT AND OUTPUT INTERCOURSE
;;   ------------------------------------------------------------------
;;   p              | Pops the top stack element and prints the
;;                  | character whose ASCII code concurs with the same
;;                  | to the standard output conduit.
;;                  |--------------------------------------------------
;;                  | If the stack is empty at the instant of this
;;                  | operation's invocation, an abortive error of the
;;                  | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   j              | Queries the standard input conduit for a
;;                  | character and pushes its ASCII code onto the
;;                  | stack.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   f statements g | Pops the top stack element; if the same equals
;;     **********   | zero (0), executes the {statements}, and repeats
;;                  | this entire process, until the zero test fails.
;;                  |--------------------------------------------------
;;                  | {statements} must be a sequence enumerating zero
;;                  | or more Deadfish+ instructions.
;;                  |--------------------------------------------------
;;                  | If the stack is empty at the instant of this
;;                  | operation's invocation, during any of its cycles,
;;                  | an abortive error of the type "EmptyStackError"
;;                  | is signaled.
;;   ..................................................................
;;   h              | Immediately halts the program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes a project in the
;; programming language Common Lisp, each program string's evaluation
;; limining a bipartite complex:
;; 
;;   (1) ASSEMBLAGE OF ABSTRACT SYNTAX TREE (AST):
;;       Each instruction, depending upon its actual haecceity, either
;;       an aefauld character in vallidom, or an intricacy of
;;       composition, is administered a transformation into a conable
;;       abstract syntax tree (AST) node, inwith whose capture the
;;       fundament information wones, and whose highest echolon always
;;       accommodates a program node's woning.
;;       
;;       It is significant to note, as a kenspeckle choice, that a
;;       Procrustean mode of realization, thilk establishes a single
;;       ``AST-Node'' class for the moult variation of the tree's
;;       possible constituents, has been peracted, the proprium to each
;;       species of represented language construct the "node kind". The
;;       particular case of the "while" loop's designment, the same
;;       tharfs a filiation's accommodation for its subordinated
;;       statements, is fulfilled by the contingency for a list of
;;       child nodes' specification.
;;   
;;   (2) ABSTRACT SYNTAX TREE (AST) EVALUATION:
;;       Empight in the Deadfish+ interpreter's context, the abstract
;;       syntax tree experiences a traversal, commencing from its
;;       top program node's locality, with a concomitant evaluation's
;;       application actuated on the respective node in order to
;;       accompass the expected epiphenomena.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-02-27
;; 
;; Sources:
;;   [esolang:2025:Deadfish+]
;;   The Esolang contributors, "Deadfish+", June 29th, 2025
;;   URL: "https://esolangs.org/wiki/Deadfish%2B"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype node-kind ()
  "The ``node-kind'' type enumerates the recognized variation on
   abstract syntax tree (AST) node species."
  '(member
    :increment
    :decrement
    :square
    :clear
    :copy-accumulator
    :copy-stack
    :swap
    :duplicate
    :print
    :input
    :while
    :halt
    :nop
    :program))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list whose composition is edified upon
   an arbitrary accompt of members, each such subsuming into the
   ELEMENT-TYPE, for thilk is defined the generic sentinel ``*'' as the
   default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list compact of zero or
   more ``AST-Node'' objects."
  '(list-of AST-node))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the abstract syntax tree (AST) node class. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AST-Node
  (:constructor make-an-ast-node (kind &optional (children NIL))))
  "The ``AST-Node'' class serves in the accommodation of a Deadfish+
   language construct in an abstract syntax tree (AST) node's
   plasmature, its diorism a jumelle of the specifying node kind, and
   an optional ordered list of its child nodes."
  (kind     (error "No node kind has been communicated.")
            :type      node-kind
            :read-only T)
  (children NIL
            :type      node-list
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-the-strings (&rest strings)
  "Concatenate the list of STRINGS, each attiguous twissel's intermede
   wisting of no advenient content, into a singular character sequence,
   thilk is subsequently returned."
  (declare (type (list-of string) strings))
  (the string
    (format NIL "~{~a~}" strings)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global lexer variables.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source*))
(declaim (type fixnum        *source-length*))
(declaim (type fixnum        *position*))
(declaim (type character     *token*))
(declaim (type boolean       *source-is-exhausted-p*))

;;; -------------------------------------------------------

(defparameter *source*
  ""
  "The piece of Deadfish+ source code to evaluate.")

(defparameter *source-length*
  0
  "The tally of characters comprising the ``*SOURCE*'' string.")

(defparameter *position*
  0
  "The current zero-based index into the ``*SOURCE*'' string.")

(define-symbol-macro *token*
  (the character
    (schar *source* *position*)))

(define-symbol-macro *source-is-exhausted-p*
  (the boolean
    (not (array-in-bounds-p *source* *position*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexer.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun change-the-source-to (new-source)
  "Changes the global ``*SOURCE*'' string to the NEW-SOURCE, resets the
   pertinent state variables subsumed into the lexical analyzer's
   participation, and returns no value."
  (declare (type string new-source))
  (psetf
    *source*   (coerce new-source 'simple-string)
    *position* 0)
  (setf *source-length* (length *source*))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () AST-Node) parse-a-while-loop))

;;; -------------------------------------------------------

(defun expect-the-character (expected-character)
  "Determines whether the current ``*SOURCE*'' character concurs with
   the EXPECTED-CHARACTER, on confirmation advancing the ``*POSITION*''
   cursor to the subsequent character; otherwise an error of an
   unspecified type is signaled without further epiphenomena's
   actuation."
  (declare (type character expected-character))
  (cond
    ((null *token*)
      (error "The character \"~c\" was expected at the position ~d, ~
              but the source is already exhausted."
        expected-character *position*))
    ((char/= *token* expected-character)
      (error "The character \"~c\" was expected at the position ~d, ~
              but the entity \"~c\" has been encountered instead."
        expected-character *position* *token*))
    (T
      (incf *position*)))
  (values))

;;; -------------------------------------------------------

(defun parse-a-simple-instruction (node-kind)
  "Parses a simple, that is, atomic instruction and returns an
   ``AST-Node'' representation thereof, the kind of whom shall be
   the NODE-KIND's dation, while concomitantly advances the
   ``*POSITION*'' cursor to the next character in the ``*SOURCE*''."
  (declare (type keyword node-kind))
  (the AST-Node
    (prog1
      (make-an-ast-node node-kind)
      (incf *position*))))

;;; -------------------------------------------------------

(defun parse-the-next-instruction ()
  "Parses the next Deadfish+ instruction and either returns a conable
   ``AST-Node'' representation thereof; otherwise produces the ``NIL''
   sentinel."
  (the (or null AST-Node)
    (unless *source-is-exhausted-p*
      (case *token*
        (#\i
          (parse-a-simple-instruction :increment))
        (#\d
          (parse-a-simple-instruction :decrement))
        (#\s
          (parse-a-simple-instruction :square))
        (#\c
          (parse-a-simple-instruction :clear))
        (#\o
          (parse-a-simple-instruction :copy-accumulator))
        (#\p
          (parse-a-simple-instruction :print))
        (#\r
          (parse-a-simple-instruction :copy-stack))
        (#\w
          (parse-a-simple-instruction :swap))
        (#\t
          (parse-a-simple-instruction :duplicate))
        (#\h
          (parse-a-simple-instruction :halt))
        (#\j
          (parse-a-simple-instruction :input))
        (#\f
          (parse-a-while-loop))
        (otherwise
          (parse-a-simple-instruction :nop))))))

;;; -------------------------------------------------------

(defun parse-a-while-loop ()
  "Parses a \"while\" iterance construct and returns a conable
   ``AST-Node'' representation of its constitution."
  (expect-the-character #\f)
  (the AST-Node
    (make-an-ast-node :while
      (loop
        if *source-is-exhausted-p* do
          (error "A currently processed \"while\" loop has been ~
                  aborted by the source's exhaustion at the ~
                  position ~d."
            *position*)
        else if (char= *token* #\g) do
          (incf *position*)
          (loop-finish)
        else collect
          (parse-the-next-instruction)
        end))))

;;; -------------------------------------------------------

(defun parse-the-program ()
  "Parses a Deadfish+ program and returns a conable ``AST-Node''
   representation of its conformation."
  (the AST-Node
    (make-an-ast-node :program
      (loop
        for next-instruction
          of-type (or null T)
          =       (parse-the-next-instruction)
        while   next-instruction
        collect next-instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ()
  (:documentation
    "The ``Halt-Condition'' condition type serves in the apprizal about
     the optation to terminate the currently operating program in an
     orderly fashion."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (simple-error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology wones in the attempt
     to indagate or remove from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type integer           *accumulator*))
(declaim (type (list-of integer) *stack*))

;;; -------------------------------------------------------

(defparameter *accumulator*
  0
  "The memory's accumulator component.")

(defparameter *stack*
  NIL
  "The memory's stack component.")

;;; -------------------------------------------------------

(defun restore-the-interpreter-to-its-pristine-state ()
  "Restores the interpreter to its pristine state, peregal in its owelty
   to that prevenient to the first program's execution, and returns no
   value."
  (psetf
    *accumulator* 0
    *stack*       NIL)
  (values))

;;; -------------------------------------------------------

(defun query-for-a-line-of-code ()
  "Queries the standard input conduit for a line of Deadfish+ code
   and returns the same."
  (format T "~&>> ")
  (finish-output)
  (the string
    (prog1
      (read-line NIL NIL "")
      (clear-input))))

;;; -------------------------------------------------------

(defun pop-from-the-stack ()
  "Removes and returns the top ``*STACK*'' element.
   ---
   Upon the ``*STACK*'''s deprehension in a state of vacancy at this
   operation's invocation, an ``Empty-Stack-Error'' will be signaled."
  (the integer
    (or (pop *stack*)
        (error 'Empty-Stack-Error
          :format-control "You cannot pop from an empty stack."))))

;;; -------------------------------------------------------

(defun peek-into-the-stack ()
  "Returns without removing the top ``*STACK*'' element.
   ---
   Upon the ``*STACK*'''s deprehension in a state of vacancy at this
   operation's invocation, an ``Empty-Stack-Error'' will be signaled."
  (the integer
    (or (first *stack*)
        (error 'Empty-Stack-Error
          :format-control "You cannot peek into an empty stack."))))

;;; -------------------------------------------------------

(defun push-onto-the-stack (new-element)
  "Inserts the NEW-ELEMENT at the ``*STACK*'''s top position and returns
   no value."
  (declare (type integer new-element))
  (push new-element *stack*)
  (values))

;;; -------------------------------------------------------

(defun visit-the-node (node)
  "Evaluates the NODE, accompassing the causata alligated to its
   haecceity, and returns no value."
  (declare (type AST-Node node))
  (case (ast-node-kind node)
    (:program
      (dolist (current-statement (ast-node-children node))
        (declare (type AST-Node current-statement))
        (visit-the-node current-statement)))
    
    (:increment
      (incf *accumulator*))
    
    (:decrement
      (decf *accumulator*))
    
    (:square
      (setf *accumulator* (* *accumulator* *accumulator*)))
    
    (:clear
      (setf *accumulator* 0))
    
    (:copy-accumulator
      (push-onto-the-stack *accumulator*))
    
    (:copy-stack
      (setf *accumulator*
        (pop-from-the-stack)))
    
    (:swap
      (let ((top-element   (pop-from-the-stack))
            (lower-element (pop-from-the-stack)))
        (declare (type integer top-element))
        (declare (type integer lower-element))
        (push-onto-the-stack top-element)
        (push-onto-the-stack lower-element)))
    
    (:duplicate
      (push-onto-the-stack
        (peek-into-the-stack)))
    
    (:print
      (format T "~c"
        (code-char
          (pop-from-the-stack)))
      (finish-output))
    
    (:input
      (format T "~&Please enter a character: ")
      (finish-output)
      (push-onto-the-stack
        (char-code
          (prog1
            (read-char NIL NIL #\Null)
            (clear-input)))))
    
    (:while
      (loop while (zerop (pop-from-the-stack)) do
        (dolist (current-statement (ast-node-children node))
          (declare (type AST-Node current-statement))
          (visit-the-node current-statement))))
    
    (:halt
      (signal 'Halt-Condition))
    
    (:nop
      (format T "~%"))
    
    (otherwise
      (error "The node kind ~s cannot be comprehended."
        (ast-node-kind node))))
  (values))

;;; -------------------------------------------------------

(defun execute-a-single-deadfish+-program (code)
  "Executes the piece of Deadfish+ source CODE and returns no value."
  (declare (type string code))
  (change-the-source-to code)
  (restore-the-interpreter-to-its-pristine-state)
  (handler-case
      (visit-the-node
        (parse-the-program))
    (Halt-Condition ()
      NIL))
  (values))

;;; -------------------------------------------------------

(defun launch-the-deadfish+-interpreter
    (&optional (initial-code "" initial-code-supplied-p))
  "Instigates the Deadfish+ interpreter's execution, optionally naiting
   the INITIAL-CODE as the incipial program to execute, whence a
   repeating gestion compact of a code line request, its evaluation,
   and ensuing actuation transpires, finally, if halted, returning no
   value."
  (declare (type string initial-code))
  (declare (type T      initial-code-supplied-p))
  (restore-the-interpreter-to-its-pristine-state)
  (handler-case
      (loop
        for current-code
          of-type string
          =       (if initial-code-supplied-p
                    initial-code
                    (query-for-a-line-of-code))
          then    (query-for-a-line-of-code)
        do
          (change-the-source-to current-code)
          (visit-the-node
            (parse-the-program)))
    (Halt-Condition ()
      NIL))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output conduit.
(execute-a-single-deadfish+-program
  (concatenate-the-strings
    "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiop"
    "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiop"
    "iiiiiiiotpp"
    "iiiop"
    "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddop"
    "ddddddddddddop"
    "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiop"
    "iiiiiiiiiiiiiiiiiiiiiiiiop"
    "iiiop"
    "ddddddop"
    "ddddddddop"
    "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddop"))

;;; -------------------------------------------------------

;; One-time cat program.
(execute-a-single-deadfish+-program "jp")

;;; -------------------------------------------------------

;; Repeating cat program which perpetuates infinitely.
(execute-a-single-deadfish+-program "ofjpog")

;;; -------------------------------------------------------

;; Truth-machine.
(execute-a-single-deadfish+-program
  "jtrdddddddddddddddddddddddddddddddddddddddddddddddddotfwtpwtgrp")

;;; -------------------------------------------------------

(launch-the-deadfish+-interpreter)
