;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the implementation of a Celum program line as a
;; class, augmenting the mere encapsulation duties, the same
;; accommodate a space for the prefix bit, the optional label name, and
;; a list of commands, two references, one to the predecessor line, the
;; other a dedication to the successor, conforming in this notion with
;; a node in a linked list.
;; 
;; The linkage information capacitates the dation of a convenient
;; warklume for navigating across the line, not only in the context of
;; the interpretation stage, but, a fortiori, for prefix bit and label
;; identifier search algorithms.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Line".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Line
  (:constructor make-line (prefix label-name commands)))
  "The ``Line'' class represents a parsed line of Celum source code,
   composed of a bit-valued prefix, an optional label name, and zero or
   more commands."
  (prefix      0   :type bit)
  (label-name  ""  :type string)
  (commands    NIL :type command-list)
  (predecessor NIL :type (or null Line))
  (successor   NIL :type (or null Line)))

;;; -------------------------------------------------------

(defun line-prefix-matches-p (line expected-prefix)
  "Determines whether the LINE's prefix bit equals the EXPECTED-PREFIX<
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Line line))
  (declare (type bit  expected-prefix))
  (the boolean
    (not (null
      (= (line-prefix line)
         expected-prefix)))))

;;; -------------------------------------------------------

(defun line-label-matches-p (line expected-label-name)
  "Determines whether the LINE's label matches the EXPECTED-LABEL-NAME,
   equiparating the two in a case-insensitive manner, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line   line))
  (declare (type string expected-label-name))
  (the boolean
    (not (null
      (string-equal expected-label-name
        (line-label-name line))))))

;;; -------------------------------------------------------

(defun first-line-p (line)
  "Determines whether the LINE constitutes the first one in its
   sequence, which conflates with its absence of a predecessor,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Line line))
  (the boolean
    (null (line-predecessor line))))

;;; -------------------------------------------------------

(defun last-line-p (line)
  "Determines whether the LINE constitutes the desinent one in its
   sequence, which conflates with its absence of a successor, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line line))
  (the boolean
    (null (line-successor line))))

;;; -------------------------------------------------------

(defmethod print-object ((line Line) stream)
  (declare (type Line        line))
  (declare (type destination stream))
  (format stream "(Line :prefix ~s ~
                        :label ~s ~
                        :commands ~s ~
                        :first-p ~a ~
                        :last-p ~a)"
    (line-prefix     line)
    (line-label-name line)
    (line-commands   line)
    (first-line-p    line)
    (last-line-p     line)))
