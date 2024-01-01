;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the token queue, a first-in first-out storage
;; entity for tokens' castaldy, homologating a stillatium accumulation
;; of these objects.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "SLNode".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (SLNode
  (:constructor make-slnode (element next)))
  "The ``SLNode'' class represents a singly linked node, composed of an
   element as the payload and a reference to the successor node."
  (element NIL :type (or null Token)  :read-only T)
  (next    NIL :type (or null SLNode) :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Queue".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token-Queue
  (:constructor make-token-queue ()))
  "The ``Token-Queue'' class applies itself to the maintenance of zero
   or more ``Token'' objects in a first-in first-out (FIFO) sequence,
   inserting new members at the rear, while admitting an indagation and
   removal from the front only."
  (head NIL :type (or null SLNode) :read-only NIL)
  (tail NIL :type (or null SLNode) :read-only NIL)
  (size 0   :type (integer 0 *)    :read-only NIL))

;;; -------------------------------------------------------

(defun token-queue-empty-p (queue)
  "Determines whether the token QUEUE is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Queue queue))
  (the boolean
    (not (null
      (zerop (token-queue-size queue))))))

;;; -------------------------------------------------------

(defun token-queue-get-first (queue)
  "Returns the node at the token QUEUE's head, or ``NIL'' if none such
   exists."
  (declare (type Token-Queue queue))
  (the (or null SLNode)
    (token-queue-head queue)))

;;; -------------------------------------------------------

(defun token-queue-add-last (queue token)
  "Accommodates a new node for the token, inserts at the token QUEUE's
   rear, and returns the thus produced node instance."
  (declare (type Token-Queue queue))
  (declare (type Token       token))
  (let ((new-node (make-slnode token NIL)))
    (declare (type SLNode new-node))
    (if (token-queue-empty-p queue)
      (setf (token-queue-head queue)               new-node)
      (setf (slnode-next (token-queue-tail queue)) new-node))
    (setf (token-queue-tail queue) new-node)
    (incf (token-queue-size queue))
    (the SLNode new-node)))

;;; -------------------------------------------------------

(defun token-queue-get-successor (queue node)
  "Returns the node succeeding the specified NODE in the token QUEUE, or
   ``NIL'' if none such exists."
  (declare (type Token-Queue queue))
  (declare (ignore           queue))
  (declare (type SLNode      node))
  (the (or null SLNode)
    (slnode-next node)))
