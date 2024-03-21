;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "●", invented by the Esolang user "AnotherUser05" and
;; presented on February 23rd, 2024, the programs of which assumes the
;; guise of disciform symbols in their conation to manipulate two
;; conjoined registers, one being a data source, the other its
;; recipient.
;; 
;; 
;; Concept
;; =======
;; The ● programming language extends the language author's product
;; begotten in prevenience, ○ by utilizing circular symbols as a means
;; for two registers' manipulation.
;; 
;; == ● = ○ + MORE ==
;; The ● programming language, extrapolating a concept whose induction
;; is indebted to the forbisen produced by its predecessor, ○, employs
;; a set of twelve symbols chosen to replicate a discoidal phenotype in
;; its telos' pursuit to manipulate a twain of registers, one temporary,
;; the other main, whose champarty accompasses the operative
;; consequences.
;; 
;; == ● > ○ ==
;; The ● programming language avails itself with instruction identifiers
;; whose appearance, paregal to the ideal incarnated in the entheus
;; language ○, assume a circular shape, desumed from the magnamity that
;; is yclept the Unicode character repertoire.
;; 
;; == THE MEMORY: TWO REGISTERS IN CONJUNCTION ==
;; The program memory's componency enumerates a jumelle of registers,
;; both an unbounded integer scalar's salvatory each, one being a
;; temporary instrument, while the other provides the actual expressive
;; intercourse moiety.
;; 
;; == THE TEMPORARY REGISTER: A DATA GENERATOR ==
;; Paravail in its commerce along the control flow mechanisms, but
;; paravaunt in all further aspects' entalented capacitations, the
;; temporary register embodies the chief data creation provenance,
;; embracing in its bailiwick such competences as random number
;; generations and user input reception.
;; 
;; == THE MAIN REGISTER: A DATA RECIPIENT ==
;; The main register's respondency, barring a twissel of exemptions,
;; does not engage in a direct modulation's homologation of its content;
;; in lieu of that expecting the temporary register to transmit its
;; most recent state for inquisitions and printing purposes.
;; 
;; 
;; Instructions
;; ============
;; An amalgam of ○'s treble and its personal potentials, the ●
;; programming language tallies a duodeximal account of instructions,
;; spanning the realms of basic arithmetics, random numbers, input and
;; output communications, two conditional skipping constructs, as well
;; as an aefauld jump-based looping facility.
;; 
;; == OVERVIEW ==
;; The following apercu shall impart a cursory mete of gnarity towards
;; the operative features:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ○       | Sets the temporary register to a random integer value
;;           | chosen from the closed interval [0, 255].
;;           |---------------------------------------------------------
;;           | This instruction has been appropriated verbatim from
;;           | the predecessor language "○".
;;   ..................................................................
;;   ●       | Sets the temporary register to random integer value
;;           | chosen from the closed interval [-255, 0].
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◌       | Resets the temporary register to the default state of
;;           | zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◍       | Queries the standard input for a signed or unsigned
;;           | integer number and stores the same in the temporary
;;           | register.
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   °       | Prints the main register value to the standard output.
;;           |---------------------------------------------------------
;;           | This instruction has been appropriated verbatim from
;;           | the predecessor language "○".
;;   ..................................................................
;;   ◯       | Increments the main register value by the most recently
;;           | generated value.
;;           |---------------------------------------------------------
;;           | This instruction has been appropriated verbatim from
;;           | the predecessor language "○".
;;   ..................................................................
;;   ◓       | Increments the main register value by one (1).
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◒       | Decrements the main register value by one (1).
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◉       | If the main register value does not equal zero (0),
;;           | skips the next instruction; otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◎       | If the main register value equals zero (0), skips the
;;           | next instruction; otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◖       | Demarcates a loop section's commencement, amplecting in
;;           | in its circumference all instruction betwixt this and
;;           | the matching "◗" token.
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
;;   ..................................................................
;;   ◗       | Relocates the instruction pointer (IP) to the position
;;           | of the matching "◖" instruction.
;;           |---------------------------------------------------------
;;           | This instruction constitutes an autochthonous
;;           | contribution of the "●" language.
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
;; Date:   2024-03-18
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2024●]
;;   The Esolang contributors, "●", February 23rd, 2024
;;   URL: "https://esolangs.org/wiki/%E2%97%8F"
;;   
;;   [esolang2014○]
;;   The Esolang contributors, "○", March 15th, 2024
;;   URL: "https://esolangs.org/wiki/%E2%97%8B"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type (type-name
                              (candidate-variable &rest lambda-list)
                              &body body)
  "Defines a new derived type utilizing the ``deftype'' infrastructure
   in coefficiency with the ``satisfies'' predicate, the thus generated
   species being stevened by the TYPE-NAME, employing the LAMBDA-LIST
   for its parameters, its implementation ensuing from the BODY forms,
   being granted access to the probed subject as the CANDIDATE-VARIABLE,
   with the last BODY form providing the docimasy's result, responding
   with a generalized boolean \"true\" value upon its eligibility for
   the predicate, otherwise delivering ``NIL''."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(or (and (stringp (first body))
                 (pop body))
            (format NIL "Defines the derived type ``~a''." type-name))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-custom-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list endowed with zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for
   which holds the default of the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based stack endowed with zero or
   more elements, each member of which conforms to the ELEMENT-TYPE, for
   which holds the default of the comprehensive ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (indicator-type T)
                                        (value-type     T))
  "The ``association-list-of'' type defines an association list, or
   alist, whose circumference tallies zero or more entries, each such a
   cons whose sinistral compartment accommodates the indicators, or
   keys, subsuming into the INDICATOR-TYPE, while the dextral moeity
   contributes the values, each such complying to the VALUE-TYPE, for
   both definitions holds the default species of the comprehensive
   ``T''."
  `(list-of (cons ,indicator-type ,value-type)))

;;; -------------------------------------------------------

(define-custom-type hash-table-of (candidate
                                   &optional (key-type   T)
                                             (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each instance among which
   establishes a jumelle of a key conforming to the KEY-TYPE and its
   associated value, this compliant with the VALUE-TYPE, for both holds
   the default of the comprehensive ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and (typep key   key-type)
             (typep value value-type)))))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variants of ●
   instructions."
  '(member
    ;; Commands inherited from "○".
    :set-to-non-negative-random-value
    :increment-by-last-value
    :print
    ;; Commands autochthonous to "●".
    :set-to-non-positive-random-number
    :set-to-zero
    :input
    :increment
    :decrement
    :skip-if-not-zero
    :skip-if-zero
    :start-loop
    :end-loop))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' associates the recognized instruction tokens
   with suitable representations, realized as an assocation list (alist)
   whose indicators (keys) assume the identifier ``character''s, while
   the values are provided by the ``command'' specimens."
  '(association-list-of character command))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines an association of loop start and end
   points in a bilateral mode by adminiculum of their locations in the
   respective ● program's instruction sequence, manifested in a hash
   table whose keys and values both assume fixnum components."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable ● program as a
   one-dimensional simple array of ``command'' objects."
  '(simple-array command (*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '((#\○ . :set-to-non-negative-random-value)
    (#\◯ . :increment-by-last-value)
    (#\° . :print)
    (#\● . :set-to-non-positive-random-number)
    (#\◌ . :set-to-zero)
    (#\◍ . :input)
    (#\◓ . :increment)
    (#\◒ . :decrement)
    (#\◉ . :skip-if-not-zero)
    (#\◎ . :skip-if-zero)
    (#\◖ . :start-loop)
    (#\◗ . :end-loop))
  "Maps the recognized operation identifier tokens to representative
   command objects.")

;;; -------------------------------------------------------

(defun get-command-entry (token)
  "Returns the entry for the command TOKEN in the +IDENTIFIERS+, or, if
   the same does not affiliate with an operation, responds with
   ``NIL''."
  (declare (type character token))
  (the (or null (cons character command))
    (assoc token +IDENTIFIERS+ :test #'char=)))

;;; -------------------------------------------------------

(defun command-token-p (candidate)
  "Determines whether the CANDIDATE represents a command token,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (get-command-entry candidate)))))

;;; -------------------------------------------------------

(defun get-command (identifier)
  "Returns the command associated with the IDENTIFIER token, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type character identifier))
  (the command
    (or (cdr (get-command-entry identifier))
        (error "Unrecognized command identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (source)
  "Extracts from the ● SOURCE a one-dimensional simple array of its
   incorporated commands and returns this sequence."
  (declare (type string source))
  (the program
    (map '(simple-array command (*)) #'get-command
      (remove-if-not #'command-token-p source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-jump-table ()
  "Creates and returns an initially empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-loop-bournes (jump-table start-point end-point)
  "Associates the loop START-POINT and END-POINT in a bilateral mode in
   the JUMP-TABLE and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table (program)
  "Generates and returns for the ● PROGRAM a jump table."
  (declare (type program program))
  (let ((jump-table   (make-empty-jump-table))
        (start-points NIL))
    (declare (type jump-table        jump-table))
    (declare (type (stack-of fixnum) start-points))
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0 by 1
      
      if (eq command :start-loop) do
        (push position start-points)
      else if (eq command :end-loop) do
        (if start-points
          (connect-loop-bournes jump-table (pop start-points) position)
          (error "Unmatched loop end point at position ~d." position))
      end
      
      finally
        (when start-points
          (error "Unmatched loop start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points) start-points)))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-peer (jump-table source-point)
  "Returns for the SOURCE-POINT in the JUMP-TABLE the obverse position,
   or signals an error of an unspecified type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     source-point))
  (the fixnum
    (or (gethash source-point jump-table)
        (error "Opposite point associated with the jump point ~
                at the position ~d."
          source-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          program
    :documentation "The ● program, represented by a vector of its
                    instructions, intended for an evaluation.")
   (jump-table
    :initform      (make-empty-jump-table)
    :reader        get-jump-table
    :type          jump-table
    :documentation "Connects the loop start and end points in the
                    PROGRAM by mediation of their locations therein.")
   (ip
    :initform      0
    :type          fixnum
    :accessor      program-ip
    :documentation "The current instruction pointer (IP) location in
                    the PROGRAM.")
   (last-value
    :initform      0
    :accessor      last-value
    :type          integer
    :documentation "The value generated by the most recent command.")
   (register
    :initform      0
    :accessor      register
    :type          integer
    :documentation ""))
  (:documentation
    "The ``Interpreter'' class applies itself to the adhibition of
     actual effect to a ● program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the jump table for the INTERPRETER's program, stores it
   in the same, prepares the globally active random number generator,
   and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-table)
    (build-jump-table
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' nuncupated to the ●
   program's evaluation."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (incf (program-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the ● program governed by the INTERPRETER has been
   processed entirely, returning on confirmation a ``boolean'' value of
   ``T'', otherewise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (array-in-bounds-p
        (get-program interpreter)
        (program-ip  interpreter)))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command currently processed by the INTERPRETER."
  (declare (type Interpreter interpreter))
  (the command
    (aref
      (get-program interpreter)
      (program-ip  interpreter))))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-command-processor (command (interpreter-variable)
                                    &body body)
  "Defines an implementation of the generic function
   ``process-command'', the first formal parameter's agnomination is
   desumed from the INTERPRETER-VARIABLE, while the second parameter's
   assumes an automatically generated stevening, dispatching on the
   committed COMMAND type via an ``eql''-equiparation, inserting the
   BODY forms in the method, ensuing therefrom no return value."
  (let ((command-variable (gensym)))
    (declare (type symbol command-variable))
    `(defmethod process-command
         ((interpreter       ,interpreter-variable)
          (,command-variable (eql ,command)))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type command     ,command-variable))
       (declare (ignore           ,command-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-command-processor :set-to-non-negative-random-value (interpreter)
  (setf (last-value interpreter)
    (random 256)))

;;; -------------------------------------------------------

(define-command-processor :increment-by-last-value (interpreter)
  (incf (register interpreter)
    (last-value interpreter)))

;;; -------------------------------------------------------

(define-command-processor :print (interpreter)
  (format T "~d "
    (register interpreter)))

;;; -------------------------------------------------------

(define-command-processor :set-to-non-positive-random-number
                          (interpreter)
  (setf (last-value interpreter)
    (- (random 256))))

;;; -------------------------------------------------------

(define-command-processor :set-to-zero (interpreter)
  (setf (last-value interpreter) 0))

;;; -------------------------------------------------------

(define-command-processor :input (interpreter)
  (format T "~&>> ")
  (finish-output)
  (setf (last-value interpreter)
    (parse-integer
      (read-line NIL NIL 0)))
  (clear-input))

;;; -------------------------------------------------------

(define-command-processor :increment (interpreter)
  (incf (register interpreter)))

;;; -------------------------------------------------------

(define-command-processor :decrement (interpreter)
  (decf (register interpreter)))

;;; -------------------------------------------------------

(define-command-processor :skip-if-not-zero (interpreter)
  (unless (zerop (register interpreter))
    (advance-ip interpreter)))

;;; -------------------------------------------------------

(define-command-processor :skip-if-zero (interpreter)
  (when (zerop (register interpreter))
    (advance-ip interpreter)))

;;; -------------------------------------------------------

(define-command-processor :start-loop (interpreter))

;;; -------------------------------------------------------

(define-command-processor :end-loop (interpreter)
  (setf (program-ip interpreter)
    (get-jump-peer
      (get-jump-table interpreter)
      (program-ip     interpreter))))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the ● program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter))
    (advance-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-● (code)
  "Interprets the piece of ● source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (extract-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-● "◍◯◖°◎◗")

;;; -------------------------------------------------------

;; Variation on the Fixed Repeating Output which prints the current
;; counter value, commencing from the user input decremented by one (1).
;; 
;; For instance, an input of 5 generates the sequence:
;;   4 3 2 1 0 0
(interpret-● "◍◯◖◒°◎◗°")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-● "◍◯°")

;;; -------------------------------------------------------

;; Repeating numeric cat program which terminates on a user input of
;; the value zero (0).
;; 
;; Please heed that negative numbers are not homologated as inputs.
;; 
;; Please also note how the inner loop
;;   ◖◎◒◎◗
;; resets the main register to the default state of zero (0).
(interpret-● "◖ ◖◎◒◎◗ ◍◯◎°◎◗")

;;; -------------------------------------------------------

;; Countdown from the inclusive user input number to inclusive zero (0).
;; The user input must be greater than or equal to zero (0).
(interpret-● "◍◯°◖◒°◎◗")
