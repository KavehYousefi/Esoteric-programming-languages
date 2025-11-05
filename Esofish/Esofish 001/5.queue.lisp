;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the "5" program memory's second data salvatory,
;; a queue of no natural bournes in its membership's mickleness, to
;; whom the capacitation for signed integer numbers' castaldy
;; constitutes the telos' vouchsafement.
;; 
;; Maugre its official agnomination as a "queue", the expectancy of
;; supererogation in its facilities imposes the participation in a
;; double-ended queue's (deque) propria as a sequela. As a consectary,
;; the intrinsics' reification registers a doubly linked list
;; designment.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "DLNode".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (DLNode
  (:print-function
    (lambda (node stream current-depth)
      (declare (type DLNode      node))
      (declare (type destination stream))
      (declare (type integer     current-depth)
               (ignore           current-depth))
      (format stream "~:[x~;<~]-[~d]-~:[x~;>~]"
        (dlnode-previous node)
        (dlnode-value    node)
        (dlnode-next     node))
      (values))))
  "The ``DLNode'' class serves in the encapsulation of a doubly linked
   list node's diorism, thilk, in addition to the ensconced integer
   datum, a vinculum to an optional predecessor and a tantamount along
   the athwart laterality."
  (value    0   :type integer          :read-only T)
  (previous NIL :type (or null DLNode) :read-only NIL)
  (next     NIL :type (or null DLNode) :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Queue".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Queue ()
  ((header
    :initform      (make-dlnode)
    :type          DLNode
    :documentation "A sentinel node which demarcates the queue's front
                    end, always preceding any other node, including the
                    official first member.
                    ---
                    The champarty in which the HEADER and TRAILER nodes
                    engage furnishes a commodity for simplified
                    modifications, disencumbered from the intricacies of
                    ``NIL'' sentinels.")
   (trailer
    :initform      (make-dlnode)
    :type          DLNode
    :documentation "A sentinel node which demarcates the queue's rear
                    end, always succeeding any other node, including the
                    official desinent member.
                    ---
                    The champarty in which the HEADER and TRAILER nodes
                    engage furnishes a commodity for simplified
                    modifications, disencumbered from the intricacies of
                    ``NIL'' sentinels.")
   (size
    :initform      0
    :type          (integer 0 *)
    :documentation "The tally of elements partaking of this queue.
                    ---
                    Please heed that the HEADER and TRAILER nodes, being
                    engaged in the agency of sentinels, do not
                    contribute to this accompt."))
  (:documentation
    "The ``Queue'' class serves in a first-in first-out (FIFO) data
     structure's furnishment, the tolerance apportioned to its
     membership that of signed integer numbers.
     ---
     Please heed that, maugre its official agnomination as a queue, this
     salvatory's entelechy, as an sequela begotten from the \"5\"
     programming language's dioristic requisites, incorporates several
     competences forinsecal to the autochthonous queue abstract data
     type (ADT) species, and desumed from the double-ended queue (deque)
     type's haecceity.
     ---
     A phenotypical expression of this twifaced proprium, the concrete
     implementation relies on a doubly linked list, more peisant in its
     intrinsic ordonnance than a queue's traditional singly linked
     solution, yet, for the sake of efficiency in access, expecting a
     vindication as a bairn of sapience."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((queue Queue) &key)
  "Alligates the QUEUE's header and trailer sentinel nodes in a
   bilateral mode and returns no value."
  (declare (type Queue queue))
  (with-slots (header trailer) queue
    (declare (type DLNode header))
    (declare (type DLNode trailer))
    (psetf
      (dlnode-next     header)  trailer
      (dlnode-previous trailer) header))
  (values))

;;; -------------------------------------------------------

(defun make-an-empty-queue ()
  "Creates a fresh ``Queue'' whose status at this inchoate instant
   amounts to such of a vacancy."
  (the Queue
    (make-instance 'Queue)))

;;; -------------------------------------------------------

(defun queue-is-empty-p (queue)
  "Determines whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (resolve-to-a-boolean-value
      (zerop
        (slot-value queue 'size)))))

;;; -------------------------------------------------------

(defun insert-a-node-betwixt (queue new-element predecessor successor)
  "Inserts a fresh node containing the NEW-ELEMENT into the QUEUE,
   located betwixt the PREDECESSOR and the SUCCESSOR nodes, establishes
   and modifies the requisite vincula for this telos, and returns no
   value."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (declare (type DLNode  predecessor))
  (declare (type DLNode  successor))
  (let ((new-node (make-dlnode :value new-element)))
    (declare (type DLNode new-node))
    (psetf
      (dlnode-next     predecessor) new-node
      (dlnode-previous new-node)    predecessor
      (dlnode-next     new-node)    successor
      (dlnode-previous successor)   new-node))
  (incf (slot-value queue 'size))
  (values))

;;; -------------------------------------------------------

(defun remove-a-node (queue delendum)
  "Disconnects the DELENDUM from its predecessor and successor nodes,
   redirects the vincula betwixt this twissel, and returns the thus
   removed DELENDUM node's value."
  (declare (type Queue  queue))
  (declare (type DLNode delendum))
  (let ((predecessor (dlnode-previous delendum))
        (successor   (dlnode-next     delendum)))
    (declare (type DLNode predecessor))
    (declare (type DLNode successor))
    (psetf
      (dlnode-next     predecessor) successor
      (dlnode-previous successor)   predecessor)
    (decf (slot-value queue 'size))
    (the integer
      (dlnode-value delendum))))

;;; -------------------------------------------------------

(defun add-to-the-queue-front (queue new-element)
  "Prepends the NEW-ELEMENT to the QUEUE's front and returns no value."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (with-slots (header) queue
    (declare (type DLNode header))
    (insert-a-node-betwixt queue new-element header
      (dlnode-next header)))
  (values))

;;; -------------------------------------------------------

(defun add-to-the-queue-rear (queue new-element)
  "Appends the NEW-ELEMENT to the QUEUE's rear, tantamount to its
   \"enqueuing\", and returns no value."
  (declare (type Queue   queue))
  (declare (type integer new-element))
  (with-slots (trailer) queue
    (declare (type DLNode trailer))
    (insert-a-node-betwixt queue new-element
      (dlnode-previous trailer)
      trailer))
  (values))

;;; -------------------------------------------------------

(defun remove-from-the-queue-front (queue)
  "Removes and returns the element at the QUEUE's front, this being a
   tantamount to a \"dequeuing\"."
  (declare (type Queue queue))
  (the integer
    (if (queue-is-empty-p queue)
      (error "Cannot remove from an empty queue.")
      (remove-a-node queue
        (dlnode-next
          (slot-value queue 'header))))))

;;; -------------------------------------------------------

(defun remove-from-the-queue-rear (queue)
  "Removes and returns the element at the QUEUE's rear."
  (declare (type Queue queue))
  (the integer
    (if (queue-is-empty-p queue)
      (error "Cannot remove from an empty queue.")
      (remove-a-node queue
        (dlnode-previous
          (slot-value queue 'trailer))))))

;;; -------------------------------------------------------

(defun move-the-queue-front-to-its-rear (queue)
  "Relocates the QUEUE's front element to the rear position and returns
   no value."
  (declare (type Queue queue))
  (add-to-the-queue-rear queue
    (remove-from-the-queue-front queue))
  (values))

;;; -------------------------------------------------------

(defun move-the-queue-rear-to-its-front (queue)
  "Relocates the QUEUE's rear element to the front position and returns
   no value."
  (declare (type Queue queue))
  (add-to-the-queue-front queue
    (remove-from-the-queue-rear queue))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((queue Queue) (stream T))
  (declare (type Queue       queue))
  (declare (type destination stream))
  (loop
    for current-node
      of-type DLNode
      =       (dlnode-next (slot-value queue 'header))
      then    (dlnode-next current-node)
    and has-a-prevenient-element-p
      of-type boolean
      =       NIL
      then    T
    until
      (eq current-node (slot-value queue 'trailer))
    do
      (format stream "~@[, ~*~]~d"
        has-a-prevenient-element-p
        (dlnode-value current-node)))
  (the Queue queue))
