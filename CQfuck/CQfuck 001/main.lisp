;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "CQfuck", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 14th, 2023, the diorism of which wones in its
;; modification of Urban Mueller's "brainfuck" in the aspect of the goto
;; control flow mechanism to relay its mechanics to a call queue,
;; derived from the Esolang user "Koen"'s eponymous procedure invocation
;; management structure.
;; 
;; 
;; Concept
;; =======
;; The CQfuck's languages competences in all aspects submit to the
;; governance by an equivalency to its inspiration, the language
;; brainfuck, the novel proprium's entelechy commorant merely in the
;; mode of its forward and back jump facilities' operation --- the
;; ancestor being unrestrained in this ambitus, while the descendant
;; expressly subscribes to a call queue for its motions.
;; 
;; == THE CALL QUEUE: A FIFO STORAGE FOR GOTO POSITIONS ==
;; CQfuck's construe of the call queue exhibits a specialization of the
;; queue notion for the instruction pointer (IP) locations' castaldy in
;; the context of the goto facilities.
;; 
;; The queue data structure accomplishes its wike in a first-in
;; first-out (FIFO) mode, inserting new elements at its rear during the
;; so-called "enqueue" operation, and removing extant members from its
;; front in what is denoted as the "dequeue" activity.
;; 
;; The call queue, conceived by the Esolang user "Koen" in the year
;; 2013, constitutes an appropriation of this sequence species for an
;; application as a call stack alternative, capacitating the governance
;; over procedure invocations, their helming, and conclusion.
;; 
;; 
;; Instructions
;; ============
;; A paregal of its brainfuck entheus in both aspects of equinumerant
;; cardinality and equipollence, CQfuck's acquisition in the modes that
;; appertain to the donet's exposition and operations merely ostends a
;; divergence by the forward and back jump instructions, "[" and "]",
;; the former, in lieu of the current cell value's inspection for a
;; zero-valued state, unconditionally enqueues the contemporaneous
;; instruction pointer (IP) location at the call queue's front, while
;; the twissel's complementary moiety, upon a non-zero cell value,
;; rather than to incline towards an indeterminancy in the warklumes of
;; its matching "[" token's retrieval, explicitly queries the call queue
;; by dequeuing its front element, and relocating the instruction
;; pointer to the selfsame position.
;; 
;; == OVERVIEW ==
;; A foundational gnarity anent the operational facilities shall be the
;; following tabular illustration's dation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the memory's current cell value by one.
;;           | Upon the transgression of its upper bourne of 255, the
;;           | the cell state is reset to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the memory's current cell value by one.
;;           | Upon the transgression of its lower bourne of zero (0),
;;           | the cell state is reset to the maximum of 255.
;;   ..................................................................
;;   >       | Translates the memory's cell pointer one step to the
;;           | right.
;;   ..................................................................
;;   <       | Translates the memory's cell pointer one step to the
;;           | left.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the memory's current cell.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code equals the
;;           | memory's current cell value to the standard output.
;;   ..................................................................
;;   [       | Enqueues the current instruction pointer (IP) position
;;           | at the call queue's rear.
;;   ..................................................................
;;   ]       | If the current cell value equals zero (0), dequeues the
;;           | front element from the call queue, and moves the
;;           | instruction pointer (IP) to this obtained value.
;;           | If the current cell value does not equal zero (0),
;;           | dequeues the front call queue element and simply
;;           | discards it, advancing to the next position.
;;           |---------------------------------------------------------
;;           | If the call queue is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyCallQueueError" is signaled.
;;           |---------------------------------------------------------
;;           | Please note that, in the affirmative case involving the
;;           | instruction pointer's relocation to the dequeued call
;;           | queue element, the thus reached command will always be
;;           | a forward jump instruction, "[". This command must
;;           | subsequently be processed as defined under its personal
;;           | treatise above in order to memorize its position in the
;;           | call queue --- it may not be skipped, as is frequently
;;           | the wont with brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; CQfuck's brevity in regard to its protolog encumbers this provenance
;; with some mete of ambiguities, a subset thereof, selected by the
;; criterion of ponderance, shall be elucidated below.
;; 
;; == DOES THE FORWARD JUMP COMMAND "[" OPERATE CONDITIONALLY? ==
;; The treatise on the language's most peisant invention, the jump
;; facility twissel "[" and "]", mentions for the former an enqueuing of
;; the instruction pointer location, whereas the latter explicitly
;; applies in a conditional fashion, dequeuing from the call queue, and
;; either relocating the instruction pointer to its value for a non-zero
;; current cell value, or, in the athwart case, discarding the same and
;; proceeding in the usual manner. A statement concerning the forward
;; jump's operation with dependence upon the acquainted zero-cell
;; predicate eludes the description.
;; 
;; It is important to note that, committing to a stringent adherence to
;; the suggested unconditional nature of the "[" instruction, its
;; execution, as counterdistinguished from brainfuck, imposes at least
;; one cycle, as no jumping to the corresponding "]" token intervenes in
;; the process.
;; 
;; It has been adjudged to maintain the ipsissima verba presentation
;; adduced in the CQfuck specification, an imputation whence ensues a
;; coercive first execution's guarantee, invalidating the contingency
;; for a program segment's complete omission.
;; 
;; 
;; Implementation
;; ==============
;; This project has been realized in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-17
;; 
;; Sources:
;;   [esolang2018CallQueue]
;;   The Esolang contributors, "Call Queue", May 9th, 2018
;;   URL: "https://esolangs.org/wiki/Call_Queue"
;;   
;;   [esolang2023CQfuck]
;;   The Esolang contributors, "CQfuck", September 28th, 2023
;;   URL: "https://esolangs.org/wiki/CQfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic ``*'' sentinel."
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
  "The ``hash-table-of'' type defines a hash table composed of zero or
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

(deftype position-list ()
  "The ``position-list'' type defines a list composed of zero or more
   instruction pointer (IP) positions, each such represented by a fixnum
   value."
  '(list-of fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte object composed of eight
   accolent bits, and thus spanning the integral range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse vector of unsigned
   byte-valued cells, represented by a hash table, the integer keys of
   which maintain the explicitly specified cell's indices, associating
   with the ``octet'' cell values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   embracing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Call-Queue".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Call-Queue ()
  ((elements
    :initform      NIL
    :type          position-list
    :documentation "A list of memorized instruction pointer (IP)
                    positions."))
  (:documentation
    "The ``Call-Queue'' class encapsulates the notion of call queue in
     the context of CQfuck, dedicated to the castaldy of instruction
     pointer (IP) positions in a queue in order to accompass the
     operations of the goto, or jump, facilities."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Call-Queue-Error (simple-error)
  ((offended-queue
    :initarg       :offended-queue
    :initform      (error "Missing offended queue.")
    :type          Call-Queue
    :documentation "The call queue which, at the occasion of a peeking
                    or dequeuing attempt, has been empty."))
  (:default-initargs
    :format-control "Cannot dequeue from an empty call queue.")
  (:documentation
    "The ``Empty-Call-Queue-Error'' serves in the apprizal about an
     anomalous situation elicited by the attempt to inspect or remove
     an empty call queue's front element."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of call queue operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-call-queue ()
  "Creates and returns an empty ``Call-Queue''."
  (the Call-Queue
    (make-instance 'Call-Queue)))

;;; -------------------------------------------------------

(defun enqueue-ip (queue ip)
  "Inserts the IP position at the call QUEUE's rear and returns no
   value."
  (declare (type Call-Queue queue))
  (declare (type fixnum     ip))
  (with-slots (elements) queue
    (declare (type position-list elements))
    (setf elements
      (append elements
        (list ip))))
  (values))

;;; -------------------------------------------------------

(defun dequeue-ip (queue)
  "Removes and returns the instruction pointer (IP) position stored at
   the call QUEUE's front, or signals an error of the type
   ``Empty-Call-Queue-Error'' upon the QUEUE's vacancy."
  (declare (type Call-Queue queue))
  (with-slots (elements) queue
    (declare (type position-list elements))
    (the fixnum
      (if elements
        (pop elements)
        (error 'Empty-Call-Queue-Error :offended-queue queue)))))

;;; -------------------------------------------------------

(defmethod print-object ((queue Call-Queue) stream)
  (declare (type Call-Queue  queue))
  (declare (type destination stream))
  (format stream "(Call-Queue ~:[is empty~;~:*~{~d~^, ~}~])"
    (slot-value queue 'elements)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-table
    :documentation "A sparse vector of unsigned byte-valued cells, the
                    keys of which designate the cell indices, mapping to
                    the cell values.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, storing the current cell's index
                    by mediation of its key in the CELLS hash table."))
  (:documentation
    "The ``Memory'' class models the CQfuck program memory as a
     bilaterally infinite dispansion of unsigned byte-valued cells,
     operated upon by cell pointer, the same at any instant designates
     the currently active entity among these, the sole actor entalented
     with an amenability to indagations and manipulations."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns an empty ``Memory''."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type cell-table cells))
    (declare (type integer    pointer))
    (the octet
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceded by a wrapping around into the valid unsigned byte range of
   [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (cells pointer) memory
    (declare (type cell-table cells))
    (declare (type integer    pointer))
    (setf (gethash pointer cells 0)
          (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and
   returns no value."
  (declare (type Memory memory))
  (decf (slot-value memory 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-CQfuck (code)
  "Interprets the piece of CQfuck source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (call-queue (make-call-queue))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type Call-Queue call-queue))
    (declare (type Memory     memory))
    (loop while (< ip (length code)) do
      (let ((token (char code ip)))
        (declare (type character token))
        (case token
          (#\+
            (incf (current-cell memory))
            (incf ip))
          (#\-
            (decf (current-cell memory))
            (incf ip))
          (#\>
            (move-cell-pointer-right memory)
            (incf ip))
          (#\<
            (move-cell-pointer-left memory)
            (incf ip))
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell memory)
                  (char-code
                    (read-char)))
            (clear-input)
            (incf ip))
          (#\.
            (format T "~c"
              (code-char
                (current-cell memory)))
            (incf ip))
          (#\[
            (enqueue-ip call-queue ip)
            (incf ip))
          (#\]
            (cond
              ((zerop (current-cell memory))
                (dequeue-ip call-queue)
                (incf ip))
              (T
                (setf ip
                  (dequeue-ip call-queue)))))
          (otherwise
            (incf ip))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-CQfuck "[,.]")

;;; -------------------------------------------------------

;; Print the message "HI".
(interpret-CQfuck "+++++++[->++++++++++<]>++.+.")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Memory layout:
;;   memory[0]: Input of either "0" (= ASCII code 48) or "1" (= ASCII
;;              code 49), reduced to value of 0 while copying to
;;              memory[1] and memory[2].
;;   memory[1]: Copy of memory[0], reduced to 0 or 1 in order to govern
;;              the output loop.
;;   memory[2]: Copy of memory[0], retained for printing character
;;              "0" (= ASCII code 48) or "1" (ASCII code 49).
;;   memory[3]: Employed in order to reduce memory[1] to 0 or 1 in order
;;              for the same ot helm the output loop.
(interpret-CQfuck
  ",
   [->+>+<<]
   >>>++++++++[-<<------>>]
   <<[>.<]")
