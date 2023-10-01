;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "A Queue which can't grow", invented by the Esolang user
;; "ChuckEsoteric08" and presented on May 27th, 2023, its diorism's
;; manifestation an integer-valued queue of fixed cardinality,
;; initialized once via user inputs, and operated upon by a perpetually
;; cycling loop which never persistently modulates its memory's size
;; during its modifications.
;; 
;; 
;; Concept
;; =======
;; The "A Queue which can't grow" programming language is defined by
;; a memory composed of a single queue, initialized in an incipient
;; stage by user input numbers, and operated upon in a perpetual loop by
;; commands which modify the content, yet ultimately retain the
;; user-specified size.
;; 
;; == PROGRAM: INITIALIZE ONCE, REPEAT ALWAYS ==
;; Every "A Queue which can't grow" program inchoates with its memory
;; queue's initially vacant population by a series of
;; semicolon-separated integer numbers committed via the standard input,
;; ere its infinite loop, involving the command executions, commences.
;; 
;; The entirety of this process shall be listed alow:
;; 
;;   (1) INITIALIZATION
;;       The user is queried via the standard input conduit for a
;;       sequence of zero or more signed integer numbers, each twain
;;       segregated by exactly one semicolon, thus conformant with the
;;       format
;;         number_1 ; number_2 ; ... ; number_i ; ... ; number_N
;;       These items are subsequently enqueued, in their specification
;;       order, into the initially empty memory queue.
;;   
;;   (2) OPERATION CYCLE
;;       The program enters a perpetual iteration, executing the stated
;;       instructions once for every cycle.
;; 
;; == THE QUEUE: THE CENTRAL STORAGE ENTITY ==
;; A queue of signed integer numbers constitutes the singular storage
;; implement, the cardinality of which, succeeding its initialization by
;; user inputs, never veridically changes.
;; 
;; 
;; Architecture
;; ============
;; The euonym that constitutes the language's agnomination,
;; "A Queue which can't grow", abstains from any deceptions, as its
;; aefauld memory representative is realized in a queue of signed
;; integer numbers, initialized at a program's inchoation by user
;; provisions, and only altered in its content, never persistently in
;; its size.
;; 
;; 
;; Data Types
;; ==========
;; A paregal in its homogeneity to the architecture, the language's type
;; system recognized a single specimen only: signed integers of any
;; magnitude, their castaldy consigned to the eponymous queue's wike.
;; 
;; 
;; Syntax
;; ======
;; The "A Queue which can't grow" programming language, regarding a
;; conspectuity upon its syntaxis, occurs as a composition of zero or
;; more single-character commands, optionally formatted via whitespaces,
;; but not admissive to any other content.
;; 
;; == INSTRUCTIONS ==
;; A treble of instructions exist, each represented by a single
;; character, and destitute of a parameter's requisitum.
;; 
;; Any other content, besides the command identifiers and whitespaces,
;; is inflicted with interdiction and will instigate an error upon an
;; encounter.
;; 
;; == WHITESPACES ==
;; Whitespaces, a term whose perimeter embraces spaces, horizontal tabs,
;; and newlines, are subject to a homologation at any location and in
;; every mete reckoned appropriate to the programmer's own requisites.
;; 
;; == COMMENTS ==
;; No provision for comments wones in the current language rendition.
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (ENBF) description shall embue the
;; syntactical treatise with enhanced formality:
;; 
;;   program         := { whitespace | command } ;
;;   command         := increment | decrementOrJump | return;
;;   increment       := "|" ;
;;   decrementOrJump := "/" ;
;;   return          := "\" ;
;;   whitespace      := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A kenspeckle niggardliness wones in the language's operational
;; compartment, its competences intrined by an aefauld incrementation
;; instruction, a conditional decrementation/jump facility, and the
;; latter's twissel for referencing during the control flow incitement.
;; 
;; == OVERVIEW ==
;; The following apercu shall impart a foundational mete of gnarity
;; concerning the language's operational facilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   |       | Dequeues the first queue element, increments it by one,
;;           | and enqueues the result.
;;           |---------------------------------------------------------
;;           | An error of the type "EmptyQueueError" is signaled if
;;           | the queue is empty at the time of this operation's
;;           | invocation.
;;   ..................................................................
;;   /       | Dequeues an element, and proceeds in one of the
;;           | following modes:
;;           |   - If the dequeued element does not equal zero (0),
;;           |     decrements it by one, and enqueues the result.
;;           |   - If the dequeued element equals zero (0), enqueues
;;           |     the value zero (0, and moves the instruction pointer
;;           |     (IP) forward to the position of the matching "\"
;;           |     command.
;;           |---------------------------------------------------------
;;           | An error of the type "EmptyQueueError" is signaled if
;;           | the queue is empty at the time of this operation's
;;           | invocation.
;;   ..................................................................
;;   \       | Defines a jump destination for the preceding "/"
;;           | command, while itself accompassing no causatum.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The curtailed nature appertaining to the language's original
;; specifications inflicts the same with a certain set of ambiguities,
;; a subset of which shall be the following treatise's cynosure.
;; 
;; == ARE BALANCED JUMP POINTS MANDATED? ==
;; The twifaced causata commorant in the "/" command, namely, its
;; deduction or conditional goto mechanism, are embued with a
;; contingency --- but not a requisitum --- to rely on a matching "\"
;; token for its destination's referral. It is, however, not expressed
;; whether such a "/" and "\" twissen perforce ought to occur in
;; conjunction.
;; 
;; It has been adjudged to not impose a mandate upon a balanced vinculum
;; betwixt "/" and "\" commands, involving the homologation of either
;; species to participate with an independent encheson. This conjecture
;; founds upon two instances of ideation:
;; 
;;   (1) The jump destination command "\" is expressed in the protolog
;;       as a NOP (no-operation), and thus only utible in relation with
;;       a "/" counterpart.
;;   (2) The "/" may not conclude in a jump effect at all, rendering an
;;       imperative arbitrary and unvindicated.
;; 
;; == ARE NEGATIVE INTEGER VALUES HOMOLOGATED? ==
;; During its operative phase, "A Queue which can't grow"'s dioristic
;; instruction set effectively obviates the assumption of negative
;; integer values by the distinguishing manner of its deduction command
;; "/", the same only reduces the queue's foremost member if and only if
;; the same does not equal zero. For strictly non-negative integer
;; elements, this behavior ascertains no descent below this threshold;
;; if, on the other hand, the inchoate user provision of elements
;; incorporates negative specifications, the queue will be confronted
;; with such a use case.
;; 
;; It has been adjudged to homologate negative user input numbers,
;; encumbering the provider with the onus of a program's continued
;; sanity.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-29
;; 
;; Sources:
;;   [esolang2023AQueuewhichcan'tgrow]
;;   The Esolang contributors, "A Queue which can't grow",
;;     August 9th, 2023
;;   URL: "https://esolangs.org/wiki/A_Queue_which_can%27t_grow"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the same
   defaults to the generic ``*'' sentinel."
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

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table consisting of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic ``*'' sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
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
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of jump sources to their
   matching destinations, represented by their positions in a
   \"A Queue which can't grow\" program, and realized as a hash table
   which associates fixnum objects."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of queue.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Queue ()
  ((head
    :initform      (cons 0 NIL)
    :type          (list-of integer)
    :documentation "The queue head list stores the collection's elements
                    in its tail, while the first position serves as a
                    safeguard for updating references.
                    ---
                    The last head cons is referenced by the TAIL, which
                    realizes the actual tail-consing.")
   (tail
    :initform      NIL
    :type          (list-of integer)
    :documentation "The tail at any instant maintains a reference to the
                    desinent HEAD cons in order to enable efficient
                    insertions at the rear, capacitated by the
                    tail-consing principle.
                    ---
                    Upon an insertion at the queue rear, which is
                    tantamount to an appendage at the TAIL's end,
                    actually the referenced last HEAD cons is extented,
                    requiring no traversal of the complete HEAD list."))
  (:documentation
    "The ``Queue'' class furnishes an implementation of the queue
     abstract data type (ADT), the foundry of its existence a list,
     utilizing, for enhanced performance, tail-consing in order to
     insert content to its rear."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((queue Queue) &key)
  "Connects the QUEUE's tail pointer to the desinent cons of its head
   list in order to promote tail-consing, and returns no value."
  (declare (type Queue queue))
  (setf (slot-value queue 'tail)
    (last (slot-value queue 'head)))
  (values))

;;; -------------------------------------------------------

(defun make-queue ()
  "Creates and returns a new empty ``Queue''."
  (the Queue
    (make-instance 'Queue)))

;;; -------------------------------------------------------

(defun empty-queue-p (queue)
  "Determines whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (null (rest (slot-value queue 'head)))))

;;; -------------------------------------------------------

(defun enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the QUEUE's rear and returns no value."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (with-slots (tail) queue
    (declare (type (list-of integer) tail))
    (setf (rest tail) (list new-element))
    (setf tail        (rest tail)))
  (values))

;;; -------------------------------------------------------

(defun dequeue (queue)
  "Removes and returns the element at the QUEUE's front, or signals an
   error of the type ``Empty-Queue-Error'' if the same is vacant."
  (with-slots (head tail) queue
    (declare (type (list-of integer) head))
    (declare (type (list-of integer) tail))
    (the integer
      (cond
        ((empty-queue-p queue)
          (error 'Empty-Queue-Error :offended-queue queue))
        (T
          (setf head (rest head))
          (first head))))))

;;; -------------------------------------------------------

(defmethod print-object ((queue Queue) stream)
  (declare (type Queue       queue))
  (declare (type destination stream))
  (format stream "(Queue~{~^ ~d~^, ~})"
    (rest (slot-value queue 'head))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Queue-Error (error)
  ((offended-queue
    :initarg       :offended-queue
    :initform      (error "Missing offended queue.")
    :reader        empty-queue-error-offended-queue
    :type          Queue
    :documentation "The queue which, at the instant of the dequeuing
                    operation, has been empty."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Queue-Error condition))
      (declare (type destination       stream))
      (format stream "Cannot dequeue from the empty queue ~s."
        (empty-queue-error-offended-queue condition))))
  (:documentation
    "The ``Empty-Queue-Error'' serves to signal an anamolous situation
     instigated by the attempt to dequeue from an empty queue."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-input (input)
  "Parses a sequence of zero or more semicolon-separated signed integer
   elements and returns these in a list."
  (declare (type string input))
  (the (list-of integer)
    (loop
      for start
        of-type (or null fixnum)
        =       0
        then    (min (1+ end) (length input))
      for end
        of-type (or null fixnum)
        =       (or (position #\; input :start start :test #'char=)
                    (length input))
      while
        (< start (length input))
      collect
        (parse-integer input :start start :end end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (code
                           &key (mandate-balanced-jump-points-p NIL))
  "Computes and returns for the piece of \"A Queue which can't grow\"
   source CODE the jump table, which connects the forward jump (\"/\")
   and back jump (\"\\\") commands by mediation of the locations inside
   of the CODE."
  (declare (type string  code))
  (declare (type boolean mandate-balanced-jump-points-p))
  (let ((jump-table        (make-hash-table :test #'eql))
        (jump-start-points NIL))
    (declare (type hash-table       jump-table))
    (declare (type (list-of fixnum) jump-start-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      if (char= token #\/) do
        (push position jump-start-points)
      else if (char= token #\\) do
        (cond
          ;; Has matching start point "/"?
          (jump-start-points
            (let ((start-point (pop jump-start-points))
                  (end-point   position))
              (declare (type fixnum start-point))
              (declare (type fixnum end-point))
              (setf (gethash start-point jump-table) end-point)
              (setf (gethash end-point   jump-table) start-point)))
          ;; No matching start point "/", but not required?
          ;; => Ignore.
          ((and (null jump-start-points)
                (not  mandate-balanced-jump-points-p))
            NIL)
          ;; No matching start point "/", but required?
          (T
            (error "Unmatched \"\\\" command at position ~d."
              position)))
      finally
        (if (and jump-start-points
                 mandate-balanced-jump-points-p)
          (error "Unmatched \"/\" commands at positions ~{~d~^, ~}."
            jump-start-points)))
    (the hash-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-queue-to-user-input (queue)
  "Queries the standard input for a sequence of zero or more integer
   numbers, each twissen segregated by a semicolon (\";\"), appends
   these items to the QUEUE, and returns no value."
  (declare (type Queue queue))
  (format T "~&>> ")
  (finish-output)
  (dolist (initial-value
            (parse-input
              (read-line)))
    (declare (type integer initial-value))
    (enqueue queue initial-value))
  (values))

;;; -------------------------------------------------------

(defun interpret-A-Queue-which-cant-grow (code
                                          &key (print-status-p T)
                                               (cycle-delay    1))
  "Interprets the piece of \"A Queue which can't grow\" source CODE and
   returns no value.
   ---
   Several configurations, advenient in their nature and forinsecal to
   the stringent language specifications, are accommodated as the
   warklumes for augmented operation:
     ------------------------------------------------------------------
     Parameter      | Causatum
     ---------------+--------------------------------------------------
     print-status-p | A ``boolean'' flag which, when set to ``T'',
                    | will, at every program cycle's commencement,
                    | display the memory queue's content on the
                    | on the standard output. Defaults to ``T''.
     ..................................................................
     cycle-delay    | A non-negative ``real'' number which determines
                    | how many seconds the program, ere transitioning
                    | from the current into the subsequent cycle, shall
                    | wait. Defaults to 0.
     ------------------------------------------------------------------"
  (declare (type string     code))
  (declare (type boolean    print-status-p))
  (declare (type (real 0 *) cycle-delay))
  (let ((jump-table (compute-jump-table code
                      :mandate-balanced-jump-points-p NIL))
        (queue      (make-queue)))
    (declare (type jump-table jump-table))
    (declare (type Queue      queue))
    
    (initialize-queue-to-user-input queue)
    
    (loop with ip of-type fixnum = 0 do
      
      (loop while (< ip (length code)) do
        (when print-status-p
          (format T "~&Queue: ~a" queue))
        
        (case (char code ip)
          ((#\Newline #\Space #\Tab)
            NIL)
          
          (#\|
            (enqueue queue
              (1+ (dequeue queue))))
          
          (#\/
            (let ((first-element (dequeue queue)))
              (declare (type integer first-element))
              (cond
                ((zerop first-element)
                  (enqueue queue 0)
                  (setf ip
                    (or (gethash ip jump-table)
                        (error "No jump destination associated with ~
                                the position ~d."
                          ip))))
                (T
                  (enqueue queue
                    (1- first-element))))))
          
          (#\\
            NIL)
          
          (otherwise
            (error "Invalid token \"~c\" at position ~d."
              (char code ip) ip)))
        
        (incf ip))
      
      ;; Reset the instruction pointer (IP) for the next cycle.
      (setf ip 0)
      ;; Wait for the specified amount betwixt two cyles.
      (sleep cycle-delay)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Counter which counts from the user input up to infinity.
(interpret-A-Queue-which-cant-grow "|")

;;; -------------------------------------------------------

;; Adder.
;; 
;; This program expects two numbers, and repeatedly increments the
;; second one by the first input, thus producing their sum.
(interpret-A-Queue-which-cant-grow "/|\\")
