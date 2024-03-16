;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Quinary Bueue", invented by the Esolang user
;; "ChuckEsoteric08" and presented on January 3rd, 2024, the diorism
;; maintaining its commorancy in the specimen manifests in a quadruple
;; instruction set entalented with such capacitation as to modulate a
;; queue of bit values, and concomitantly offering a jump-based control
;; flow mechanism.
;; 
;; 
;; Concept
;; =======
;; The Quinary Bueue programming language operates on a binary level,
;; its the quadruple single-character instructions nuncupated to the
;; manipulation of a queue of bit numbers.
;; 
;; 
;; Instructions
;; ============
;; An operational tetrad purview characterizes the Quinary Bueue
;; instruction set, provided a twissel of bit enqueuing commands, as
;; as well as a patration of this quartet via a jumelle of forward/back
;; jump facilities.
;; 
;; == OVERVIEW ==
;; The operative competences' elucidation shall be the bailiwick of the
;; following tabulation effort:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Enqueues the value zero (0) in the memory.
;;   ..................................................................
;;   1       | Enqueues the value one (1) in the memory.
;;   ..................................................................
;;   [       | Dequeues the memory's front element; if its value equals
;;           | zero (0), moves the instruction pointer (IP) forward to
;;           | the position immediately succeeding the matching "]"
;;           | instruction; otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | If the memory queue was empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyQueueError" is signaled.
;;   ..................................................................
;;   ]       | Moves the instruction pointer (IP) back to the position
;;           | occupied by the matching "[" instruction.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-04
;; 
;; Sources:
;;   [esolang2024QuinaryBueue]
;;   The Esolang contributors, "Quinary Bueue", January 4th, 2024
;;   URL: "https://esolangs.org/wiki/Quinary_Bueue"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (indicator-type T)
                                        (value-type     T))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of zero or more entries, each key, or indicator, of
   which assumes the INDICATOR-TYPE and affiliates with a value of the
   VALUE-TYPE, for both holds the comprehensive ``T'' as a default."
  `(list-of (cons ,indicator-type ,value-type)))

;;; -------------------------------------------------------

(deftype bit-list ()
  "The ``bit-list'' type defines a list composed of zero or more bit
   values."
  '(list-of bit))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping betwixt forward and back
   jump points in a Quinary Bueue program, mediated by their zero-based
   indices into the code, and realized by an association list, or alist,
   whose fixnum indicators and values desumed from the same species
   represent these locations."
  '(association-list-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   same embrace, without the claim of exhaustion, the function
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Binary-Queue".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-Queue ()
  ((head-pointer
    :initform      (list 0)
    :accessor      head-pointer
    :type          (list-of bit)
    :documentation "A reference to the first cons in the element list.
                    ---
                    Forecause lists in Common Lisp are already
                    represented by a pointer to the first cons cell,
                    this HEAD-POINTER actually comprehends the entire
                    list of elements.")
   (tail-pointer
    :initform      NIL
    :accessor      tail-pointer
    :type          (list-of bit)
    :documentation "A reference to the desinent cons in the
                    HEAD-POINTER."))
  (:documentation
    "The ``Binary-Queue'' class implements a queue dedicated to the
     castaldy of bit values, contingently bourneless in their
     capacity."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((queue Binary-Queue) &key)
  "Stores a reference to the QUEUE's desinent cons in the TAIL-POINTER
   and returns no value."
  (declare (type Binary-Queue queue))
  (setf (tail-pointer queue)
        (head-pointer queue))
  (values))

;;; -------------------------------------------------------

(defun make-empty-binary-queue ()
  "Creates and returns an initially vacant ``Binary-Queue''."
  (the Binary-Queue
    (make-instance 'Binary-Queue)))

;;; -------------------------------------------------------

(defun empty-queue-p (queue)
  "Determines whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Binary-Queue queue))
  (let ((queue-head-rest (rest (head-pointer queue))))
    (declare (type bit-list queue-head-rest))
    (the boolean
      (null queue-head-rest))))

;;; -------------------------------------------------------

(defun enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the binary QUEUE's rear and returns no
   value."
  (declare (type Binary-Queue queue))
  (declare (type bit          new-element))
  (let ((inserted-cons (list new-element)))
    (declare (type bit-list inserted-cons))
    (setf (rest (tail-pointer queue))
          inserted-cons)
    (setf (tail-pointer queue)
          (rest (tail-pointer queue))))
  (values))

;;; -------------------------------------------------------

(defun dequeue (queue)
  "Returns the bit at the binary QUEUE's front, or signals an error of
   the type ``Empty-Queue-Error'' upon its vacancy."
  (declare (type Binary-Queue queue))
  (the bit
    (cond
      ((empty-queue-p queue)
        (error 'Empty-Queue-Error :offended-queue queue))
      (T
        (setf (head-pointer queue)
              (rest (head-pointer queue)))
        (first (head-pointer queue))))))

;;; -------------------------------------------------------

(defmethod print-object ((queue Binary-Queue) stream)
  (declare (type Binary-Queue                    queue))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "(Binary-Queue~{ ~d~^,~})"
    (rest (head-pointer queue))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Queue-Error (error)
  ((offended-queue
    :initarg       :offended-queue
    :initform      (error "Missing offended queue.")
    :reader        empty-queue-error-offended-queue
    :type          Binary-Queue
    :documentation "The queue whose vacancy during a peek or dequeue
                    operation has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Queue-Error condition))
      (declare (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot peek or dequeue from an empty queue.")))
  (:documentation
    "The ``Empty-Queue-Error'' condition type serves to signal an error
     instigated by the attempt to query or remove an empty queue's front
     element."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-points (code)
  "Creates and returns a nexus betwixt the jump points present in the
   piece of Quinary Bueue source CODE whose encapsulation proceeds from
   a ``jump-table''."
  (declare (type string code))
  (let ((jump-table   NIL)
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (dotimes (position (length code))
      (case (char code position)
        (#\[
          (push position start-points))
        (#\]
          (if start-points
            (let ((start-point (pop start-points))
                  (end-point   position))
              (declare (type fixnum start-point))
              (declare (type fixnum end-point))
              (push (cons start-point end-point)   jump-table)
              (push (cons end-point   start-point) jump-table))
            (error "Unmatched \"]\" instruction at position ~d."
              position)))
        (otherwise
          NIL)))
    (when start-points
      (error "Unmatched \"[\" instruction~p at position~:p ~{~d~^, ~}."
        (length start-points) start-points))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table position)
  "Returns the obverse jump point affiliated with the POSITION in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     position))
  (the fixnum
    (or (cdr (assoc position jump-table :test #'=))
        (error "No jump end point associated with the position ~d."
          position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Quinary-Bueue (code)
  "Interprets the piece of Quinary Bueue source CODE, finally prints the
   memory's ultimate state, and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (compute-jump-points code))
        (memory     (make-empty-binary-queue)))
    (declare (type fixnum       ip))
    (declare (type jump-table   jump-table))
    (declare (type Binary-Queue memory))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\0
          (enqueue memory 0)
          (incf ip))
        (#\1
          (enqueue memory 1)
          (incf ip))
        (#\[
          (when (zerop (dequeue memory))
            (setf ip
              (get-jump-destination jump-table ip)))
          (incf ip))
        (#\]
          (setf ip
            (get-jump-destination jump-table ip)))
        (otherwise
          (incf ip))))
    (format T "~&~a" memory))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Quinary-Bueue
  "00
   [1]10
   [1]0
   [1]10
   [1]0
   [1]10
   [1]0
   [
   [1]0
   [1]10
   [1]00[1]0[[1]]0
   [1]0
   [1]10
   [1]0
   ]0
   [1]0")
