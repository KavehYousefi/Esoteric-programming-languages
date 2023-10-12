;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the realization of a Celum program in the form
;; of a collection of lines, consanguineous in its conception to a
;; linked list, the nodes of which are emulated by the ``Line''
;; objects' linkage information.
;; 
;; A ``Program'' instance maintains, in addition to the ordered line
;; sequence, a reference to the currently active line.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program
  (:constructor make-program (lines
                              &aux (current-line (first lines)))))
  "The ``Program'' class encapsulates a sequence of lines, realized as
   a linked structure, and a cursor selecting the currently active
   mmber among these."
  (lines        (error "Missing lines.") :type (list-of Line))
  (current-line NIL                      :type (or null Line)))

;;; -------------------------------------------------------

(defun program-previous-line (program)
  "Returns the line above the PROGRAM's current one, or ``NIL'' if none
   such exists."
  (declare (type Program program))
  (the (or null Line)
    (when (program-current-line program)
      (line-predecessor
        (program-current-line program)))))

;;; -------------------------------------------------------

(defun program-next-line (program &key (wrap-around-p NIL))
  "Returns the line below the PROGRAM's current one, for a PROGRAM
   residing at its desinence either relapsing to the first line, if
   WRAP-AROUND-P resolves to ``T'', or ``NIL'' if it assumes ``NIL''."
  (declare (type Program program))
  (declare (type boolean wrap-around-p))
  (the (or null Line)
    (when (program-current-line program)
      (or
        (line-successor
          (program-current-line program))
        (when wrap-around-p
          (first (program-lines program)))))))

;;; -------------------------------------------------------

(defun program-line-after (program line &key (wrap-around-p NIL))
  "Returns the line below the specified PROGRAM LINE, for a PROGRAM
   residing at its desinence either relapsing to the first line, if
   WRAP-AROUND-P resolves to ``T'', or ``NIL'' if it assumes ``NIL''."
  (declare (type Program program))
  (declare (type boolean wrap-around-p))
  (the (or null Line)
    (or
      (line-successor line)
      (when wrap-around-p
        (first (program-lines program))))))

;;; -------------------------------------------------------

(defun program-advance (program)
  "Advances the PROGRAM's line cursor to the line below the current one,
   or sets it to ``NIL'' upon the PROGRAM's exhaustion."
  (declare (type Program program))
  (setf (program-current-line program)
    (program-next-line program :wrap-around-p NIL))
  (values))

;;; -------------------------------------------------------

(defun program-exhausted-p (program)
  "Determines whether the PROGRAM is exhausted, that is, its line cursor
   has been translated beyond the defined boundaries, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (null (program-current-line program))))
