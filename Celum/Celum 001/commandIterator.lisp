;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the contribution of the ``Command-Iterator''
;; class, an adminicular warkloom for a list of ``Command'' objects'
;; convenient traversal, the contribution of which shall be,
;; especially, accosted with gratitude by the interpreter, whose duty
;; it constitutes to process the lines' operative components.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command-Iterator".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command-Iterator ()
  ((commands
    :initarg       :commands
    :initform      NIL
    :type          command-list
    :documentation "A list of the commands to iterate."))
  (:documentation
    "The ``Command-Iterator'' class is assigned the bailiwick of
     furnishing an iterator for a list of ``Command'' objects."))

;;; -------------------------------------------------------

(defun make-command-iterator (&optional (commands NIL))
  "Creates and returns a new ``Command-Iterator'', optionally dedicated
   to the initial COMMANDS as its subject."
  (declare (type command-list commands))
  (the Command-Iterator
    (make-instance 'Command-Iterator :commands commands)))

;;; -------------------------------------------------------

(defun command-iterator-current-command (iterator)
  "Returns the command ITERATOR's currently selected command, or ``NIL''
   upon its exhaustion."
  (declare (type Command-Iterator iterator))
  (the (or null Command)
    (car (slot-value iterator 'commands))))

;;; -------------------------------------------------------

(defun command-iterator-exhausted-p (iterator)
  "Determines whether the command ITERATOR has reached its desinent
   command, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Command-Iterator iterator))
  (the boolean
    (null
      (rest (slot-value iterator 'commands)))))

;;; -------------------------------------------------------

(defun command-iterator-advance (iterator)
  "Advances the command ITERATOR to the next command in its substrate
   list, if possible, and returns no value."
  (declare (type Command-Iterator iterator))
  (with-slots (commands) iterator
    (declare (type command-list commands))
    (when commands
      (setf commands
        (rest commands))))
  (values))

;;; -------------------------------------------------------

(defun command-iterator-set-to (iterator new-commands)
  "Supersedes the ITERATOR's maintained command list by the
   NEW-COMMANDS, resets its state, and returns no value."
  (declare (type Command-Iterator iterator))
  (declare (type command-list     new-commands))
  (setf (slot-value iterator 'commands) new-commands)
  (values))
