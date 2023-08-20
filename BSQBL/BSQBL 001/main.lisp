;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BSQBL", invented by the Esolang user "ChuckEsoteric08" and
;; presented on June 7th, 2023, the foundation of which refers to Urban
;; Mueller's language "brainfuck", whose tape, however, is supplanted by
;; a queue.
;; 
;; 
;; Concept
;; =======
;; BSQBL's entheus originiates in the esoteric programming language
;; brainfuck, the kenspeckle attribute of the same maintains its
;; commorancy in the simplistic design --- deploying a mere octuple of
;; instructions ---, while yet concomitantly subsumed into the
;; Turing-complete category. BSQBL desumes this humility in operational
;; expanse, but deviates from brainfuck's invinite tape of bytes for an
;; equally unbounded queue of integers.
;; 
;; 
;; Architecture
;; ============
;; BSQBL employs an infinite queue of integer elements in order to
;; fulfil its wike's requisites.
;; 
;; 
;; Data Types
;; ==========
;; A bivial furcation's adhibition segregates the type system into the
;; signed integers of paravant excellence, whereas the character species
;; occupies a parhedral wike along the communication conduits.
;; 
;; 
;; Syntax
;; ======
;; Homogeneity governs BSQBL's donat, as commands are represented by
;; single characters; any other content is administered negligence, and
;; as such may pursue a commentary telos.
;; 
;; == INSTRUCTIONS ==
;; Any of the nine instructions in the language is represented by a
;; single character from the ASCII repertoire.
;; 
;; == WHITESPACES ==
;; Whitespaces, a term whose amplectation extends across the space,
;; horizontal tab, and newline, are homologated to occupy the program's
;; intermede liberally, as with any non-command token.
;; 
;; == COMMENTS ==
;; Concomitant to its desistance from accommodating a dedicated descant
;; syntax, every character not representative of a command token is
;; simply ignored, thus bearing the potential for an private adversaria.
;; 
;; == GRAMMAR ==
;; BSQBL's eath donet shall now be a formulation in the Extended
;; Backus-Naur Form's cynosure:
;; 
;;   program    := { command | nonCommand } ;
;;   nonCommand := caharacter - command ;
;;   command    := ">" | "+" | "-" | "<" | '"' | "," | "." | "[" | "]" ;
;; 
;; 
;; Instructions
;; ============
;; BSQBL, founded upon a queue rather than a tape, engages anenst the
;; operational aspects in an equiparation of its brainfuck stock-father;
;; an amplification accompassing therein merely where the data
;; structure's diorisms imposes a special mandate in order to achieve
;; the desirable efficacy.
;; 
;; == OVERVIEW ==
;; A foundational gnarity concerning the language's operations shall be
;; adhibited in the following apercu.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Enqueues the value zero (0).
;;   ..................................................................
;;   +       | Dequeues the front element, increments it by one, and
;;           | enqueues the new value.
;;   ..................................................................
;;   -       | Dequeues the front element, decrements it by one, and
;;           | enqueues the new value.
;;   ..................................................................
;;   <       | Dequeues the front element and enqueues it.
;;   ..................................................................
;;   "       | Dequeues the front element and enqueues its two times.
;;   ..................................................................
;;   ,       | Queries the standard input for an ASCII character and
;;           | enqueues its character code.
;;   ..................................................................
;;   .       | Dequeues the front element and stores the character
;;           | whose ASCII code corresponds to the same to the standard
;;           | output.
;;   ..................................................................
;;   [       | Dequeues the front element; if its equals zero,
;;           | moves the instruction pointer (IP) forward to the
;;           | position immediately succeeding the matching "]"
;;           | command. Otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | Moves the instruction pointer (IP) back to the matching
;;           | "[" command.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The specification's lucid explications, in conjunction with the
;; applicable noscitur a sociis nature vouchsafed by its conceptual
;; ligation to brainfuck, extinguish most occasions for ambiguities; a
;; few remnants of this marring ilk shall, however, be adduced in the
;; following sections.
;; 
;; == WHAT RESPONSE SHALL BE EXPECTED FROM AN EMPTY QUEUE? ==
;; Entalented with the capacity to remove its foremost element, the
;; BSQBL's memory, the queue, may assume a state of complete vacancy,
;; the mode of its amenability to element requests is in the protolog
;; inflicted with desuetude.
;; 
;; Two possible alternative may describe the ensuing deportment:
;; 
;;   (1) An attempt to query or remove the front element of an empty
;;       queue instigates an error.
;;   (2) An attempt to query or remove the front element of an empty
;;       queue produces a default value, usually zero (0).
;; 
;; It has been adjudged, in order to accommodate a scheme that vouches
;; for a basic mete of sanity in the implementation, to resort to the
;; first (1) option: If a program endeavors to receive or remove an
;; an empty queue's first element, an error of the type
;; "EmptyQueueError" will be signaled, terminating the execution in an
;; irregular manner.
;; 
;; == DOES BSQBL'S MEMORY OBEY BRAINFUCK'S BYTE CONSTRAINT? ==
;; The BSQBL's lealty to brainfuck in a mickle tally of aspects levies
;; the question of the mete of its verisimilitude in the most dioristic
;; ambitus, the queue that supplants the entheus' tape, in terms of the
;; ensconced elements. brainfuck employs an infinite series of unsigned
;; bytes that wrap around upon their transcendence beyond the impounding
;; range [0, 255]; BSQBL's element types, on the other hand, are not
;; explicitly addressed in the protolog.
;; 
;; It has been adjudged to refrain from implanting brainfuck's octet
;; principle into BSQBL, such that the latter's queue engages in a
;; castaldy of signed integers admissible to any magnitude; thus a
;; supererogative establishment of impositions shall be obviated.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp.
;; 
;; The foundational data structure, the queue, has been desumed from the
;; explications adduced by Michael T. Goodrich et al.
;; [goodrich2014datastructure6th] for the singly linked list, whose
;; concinnity with the intended purpose homologates its appropriation
;; (see [goodrich2014datastructure6th], pages 126--127).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-19
;; 
;; Sources:
;;   [esolang2023BSQBL]
;;   The Esolang contributors, "BSQBL", June 7th, 2023
;;   URL: "https://esolangs.org/wiki/BSQBL"
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures and Algorithms in Java", sixth edition, 2014
;;   Notes:
;;     - Pages 126--127: Demonstrates the implementation of the queue
;;                       abstract data type by mediation of a singly
;;                       linked list.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type (type-name
                                   (candidate-variable
                                    &rest parameters)
                                  &body body)
  "Defines a derived type utilizing the ``deftype'' infrastructure in
   champarty with the ``satisfies'' clause, its type identifier denoted
   by the TYPE-NAME, its argument list limned by the optional
   PARAMETERS, and its probed object consigned by an argument of the
   agnomination CANDIDATE-VARIABLE to the automatically accommodated
   predicate function, that evaluates the BODY forms, returning the
   desinent form's results.
   ---
   If the BODY forms' incipient member constitutes a string, the same is
   construed as a documentation string for the ``deftype'' declaration,
   and assigned to this purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@parameters)
       ,(when (stringp (first body))
          (pop body))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, such
   as ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(define-predicated-type association-list-of
    (candidate &optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of zero or more entries, each such represented by a
   cons whose first moeity represeents the entry key and assumes the
   KEY-TYPE, associated with the value of the VALUE-TYPE in the second
   compartment, both defaulting to the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element `(cons ,key-type ,value-type)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries whose keys conform to the KEY-TYPE and associate with
   values of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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
  "The ``command'' type enumerates the recognized variants of BSQBL
   operations."
  '(member
    :enqueue-zero
    :increment
    :decrement
    :push-front-to-back
    :push-front-to-back-twice
    :input
    :output
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype command-table ()
  "The ``command-table'' type defines a mapping of command tokens to
   representative objects, manifesting in an association list, or alist,
   which affiliates characters to ``command'' objects."
  '(association-list-of character command))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable BSQBL program as a vector
   of zero or more ``command''s."
  '(vector command *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump positions
   in a BSQBL program to the respective back jump points, and vice
   versa, supplied as a hash table that associates fixnum keys to fixnum
   values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   that conforms to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of singly linked node.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor make-node (element next)))
  "The ``Node'' class models a singly linked node, intended for the
   utilization in the context of the ``Queue'' class, maintaining a
   single integer datum and an optional pointer to the successor node."
  (element 0    :type integer)
  (next    Node :type (or null Node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of queue.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Queue) *) signal-empty-queue-error))

;;; -------------------------------------------------------

(defclass Queue ()
  ((head
    :initform      NIL
    :accessor      queue-head
    :type          (or null Node)
    :documentation "The front element of the queue.")
   (tail
    :initform      NIL
    :accessor      queue-tail
    :type          (or null Node)
    :documentation "The rear element of the queue.")
   (size
    :initform      0
    :accessor      queue-size
    :type          (integer 0 *)
    :documentation "The number of elements in the queue.
                    ---
                    The HEAD and TAIL nodes, employed for storing actual
                    content may contribute to this tally."))
  (:documentation
    "The ``Queue'' class models a singly linked queue implementation,
     intended to maintains an arbitrary account of integer objects."))

;;; -------------------------------------------------------

(defun make-queue ()
  "Creates and returns an empty ``Queue''."
  (the Queue
    (make-instance 'Queue)))

;;; -------------------------------------------------------

(defun queue-empty-p (queue)
  "Determines whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (not (null
      (zerop (queue-size queue))))))

;;; -------------------------------------------------------

(defun queue-get-first (queue)
  "Returns without removing the QUEUE's front element, or signals an
   error of the type ``Empty-Queue-Error'' upon its vacancy."
  (declare (type Queue queue))
  (the integer
    (if (queue-empty-p queue)
      (signal-empty-queue-error queue)
      (node-element
        (queue-head queue)))))

;;; -------------------------------------------------------

(defun queue-enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the QUEUE's back and returns no value."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (let ((new-node (make-node new-element NIL)))
    (declare (type Node new-node))
    (if (queue-empty-p queue)
      (setf (queue-head queue) new-node)
      (setf (node-next (queue-tail queue)) new-node))
    (setf (queue-tail queue) new-node))
  (incf (queue-size queue))
  (values))

;;; -------------------------------------------------------

(defun queue-dequeue (queue)
  "Removes and returns the QUEUE's front element, or signals an error of
   the type ``Empty-Queue-Error'' upon its vacancy."
  (declare (type Queue queue))
  (the integer
    (if (queue-empty-p queue)
      (signal-empty-queue-error queue)
      (let ((removed-element
              (node-element
                (queue-head queue))))
        (declare (type integer removed-element))
        (setf (queue-head queue)
          (node-next
            (queue-head queue)))
        (decf (queue-size queue))
        ;; The particular case of a now empty linked list.
        (when (zerop (queue-size queue))
          (setf (queue-tail queue) NIL))
        (the integer removed-element)))))

;;; -------------------------------------------------------

(defmethod print-object ((queue Queue) stream)
  (declare (type Queue       queue))
  (declare (type destination stream))
  (format stream "(Queue")
  (unless (queue-empty-p queue)
    (loop
      for current-node
        of-type (or null Node)
        =       (queue-head queue)
        then    (node-next current-node)
      while current-node
      do
        (format stream " ~d"
          (node-element current-node))))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Queue-Error (error)
  ((offended-queue
    :initarg       :offended-queue
    :initform      (error "Missing offended queue.")
    :reader        empty-queue-error-offended-queue
    :type          Queue
    :documentation "The empty queue which has been queried for its
                    absent front element."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Queue-Error condition))
      (declare (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot access an empty queue's front element.")))
  (:documentation
    "The ``Empty-Queue-Error'' condition serves to signal the attempt
     to access an empty queue's front element for indagation or
     removal."))

;;; -------------------------------------------------------

(defun signal-empty-queue-error (offended-queue)
  "Signals an error of the type ``Empty-Queue-Error'' which references
   the OFFENDED-QUEUE."
  (declare (type Queue offended-queue))
  (error 'Empty-Queue-Error :offended-queue offended-queue))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of command table.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-table +COMMANDS+))

;;; -------------------------------------------------------

(defparameter +COMMANDS+
  '((#\> . :enqueue-zero)
    (#\+ . :increment)
    (#\- . :decrement)
    (#\< . :push-front-to-back)
    (#\" . :push-front-to-back-twice)
    (#\, . :input)
    (#\. . :output)
    (#\[ . :jump-forward)
    (#\] . :jump-back))
  "Associates the recognized command tokens with representative
   ``command'' objects.")

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Determines whether the TOKEN represents a command identifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (assoc token +COMMANDS+ :test #'char=)))))

;;; -------------------------------------------------------

(defun get-command (token)
  "Returns the command associated with the TOKEN, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type character token))
  (the command
    (or (cdr (assoc token +COMMANDS+ :test #'char=))
        (error "No command token: ~s." token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts from the piece of BSQBL source CODE a one-dimensional simple
   array of its commands and returns the same."
  (declare (type string code))
  (the program
    (coerce
      (loop
        for token of-type character across code
        when (command-token-p token)
          collect (get-command token))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-jump-table ()
  "Creates and returns an empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun calculate-jump-table (program)
  "Generates and returns for the BSQBL PROGRAM a jump table which
   associates the forward jump command locations in the same with that
   positions of the matching back jumps, and vice versa."
  (declare (type program program))
  (let ((jump-table          (make-jump-table))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0 by 1
      if (eq command :jump-forward) do
        (push position forward-jump-points)
      else if (eq command :jump-back) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched \"]\" at position ~d." position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched \"[\"s at positions ~{~d~^, ~}."
            forward-jump-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun jump-table-get-opposite (jump-table position)
  "Returns the location of the jump point opposite to the POSITION
   registered at the JUMP-TABLE, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     position))
  (the fixnum
    (or (gethash position jump-table)
        (error "No jump table end point associated with the ~
                position ~d."
          position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-program (program)
  "Interprets the BSQBL PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip           0)
        (jump-table   (calculate-jump-table program))
        (memory       (make-queue)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Queue      memory))
    
    (symbol-macrolet
        ((program-finished-p
          (the boolean
            (not (null
              (>= ip (length program))))))
         
         (current-command
          (the command
            (aref program ip))))
      
      (loop until program-finished-p do
        (case current-command
          ;; >
          (:enqueue-zero
            (queue-enqueue memory 0))
          
          ;; +
          (:increment
            (queue-enqueue memory
              (1+ (queue-dequeue memory))))
          
          ;; -
          (:decrement
            (queue-enqueue memory
              (1- (queue-dequeue memory))))
          
          ;; <
          (:push-front-to-back
            (queue-enqueue memory
              (queue-dequeue memory)))
          
          ;; "
          (:push-front-to-back-twice
            (let ((front-element (queue-dequeue memory)))
              (declare (type integer front-element))
              (queue-enqueue memory front-element)
              (queue-enqueue memory front-element)))
          
          ;; ,
          (:input
            (format T "~&>> ")
            (force-output)
            (let ((input (read-char *standard-input* NIL #\Null)))
              (declare (type character input))
              (clear-input *standard-input*)
              (queue-enqueue memory
                (char-code input))))
          
          ;; .
          (:output
            (format T "~c"
              (code-char
                (queue-dequeue memory))))
          
          ;; [
          (:jump-forward
            (when (zerop (queue-dequeue memory))
              (setf ip
                (jump-table-get-opposite jump-table ip))))
          
          ;; ]
          (:jump-back
            (setf ip
              (1- (jump-table-get-opposite jump-table ip))))
          
          (otherwise
            (error "Invalid command at ~s position ~d."
              current-command ip)))
        
        (incf ip))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-BSQBL (code)
  "Interprets the piece of BSQBL source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (extract-commands code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-BSQBL ",\"[.,\"]")

;;; -------------------------------------------------------

;; Output the letter "H".
(interpret-BSQBL ">+++++++\"\"[-+<+<+<+<+<+<+<+<+\"<]<+<+<.")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The following table shall serve in a cursory adhibition of gnarity
;; concerning the truth-machines working.
;; 
;; Please note that the queue elements are illustrated in with the front
;; to the rear element advancing in a sinistrodextral airt, that is,
;; the leftmost element in the display determines the queue front, while
;; the rightmost designates the rear.
;; 
;;   ---------------------------------------------------------------
;;   Action (pseudocode)   | Command | Queue state succeeding action
;;   ----------------------+---------+------------------------------
;;   a <- input (48 or 49) | ,       | a
;;   copy a                | "       | a a
;;   output a              | .       | a
;;                         |         |
;;   b <- 0                | >       | a b=0
;;   b to front            | <       |
;;   b <- b + 48           | +<...   | b+48 a
;;                         |         |
;;   copy b                | "       | a b b
;;   b to front            | <       | b b a
;;                         |         |
;;   while b != 0 do       | [       | b a
;;     b <- b - 1          | -       | a b-1
;;     a <- a - 1          | -       | b-1 a-1
;;     copy b              | "       | a-1 b-1 b-1
;;     a to back           | <       | b-1 b-1 a-1
;;   end while             | ]       | b-1 b-1 a-1
;;                         |         |
;;   a to front            | <       | a b
;;                         |         |
;;   while a != 0 do       | [       | b
;;     copy b              | "       | b=0 b=0
;;     b <- b+49           | +<...   | b=49 b=0
;;     output b            | .       | b=0
;;     enqueue 0           | >       | b=0 a=0
;;     a to front          | <       | a=0 b=0
;;     a <- 1              | +<      | a=1 b=0
;;   end while             | ]       | a=1 b=0
;;   ---------------------------------------------------------------
(interpret-BSQBL
  ",
   \"
   .
   
   >
   <
   +<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<
   
   \"
   <
   
   [
     -
     -
     \"
     <
   ]
   
   <
   
   [
   \"
   +<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<+<
   .
   >
   <
   +<
   ]
  ")
