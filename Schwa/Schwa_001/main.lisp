;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-27
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Schwa"
;;   -> "https://esolangs.org/wiki/FakeScript"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype state ()
  "The ``state'' type enumerates the valid states that a Schwa program
   may assume."
  '(integer 1 2))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Schwa command names."
  '(member
    :change-state-to-2
    :get-input
    :output
    :change-state-to-1
    :start-loop
    :end-loop))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of (cons character state) command)
               +COMMANDS+))

;;; -------------------------------------------------------

(defparameter +COMMANDS+ (make-hash-table :test #'equal)
  "Associates with a tuple (character, state) a Schwa command, thus
   permitting the former compound to respond with the latter during the
   interpretation of a Schwa program.")

;;; -------------------------------------------------------

;; Build the +COMMANDS+ table.
(flet ((add-command (character state command)
        "Associates the combination of the CHARACTER and STATE with the
         COMMAND and returns no value."
        (declare (type character character))
        (declare (type state     state))
        (declare (type command   command))
        (setf (gethash (cons character state) +COMMANDS+) command)
        (values)))
  (add-command #\a 1 :change-state-to-2)
  (add-command #\a 2 :get-input)
  (add-command #\ə 1 :output)
  (add-command #\ə 2 :change-state-to-1)
  (add-command #\e 1 :start-loop)
  (add-command #\e 2 :end-loop)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance functions.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for (character state)
  "Returns the command associated with the combination of the CHARACTER
   and STATE.
   ---
   If no command can be mapped to the inputs, an error of an unspecified
   type is signaled."
  (declare (type character character))
  (declare (type state     state))
  (the command
    (or (gethash (cons character state) +COMMANDS+)
        (error "Invalid command combination of character '~c' ~
                and state ~d."
          character state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Schwa (code)
  "Interprets the piece of Schwa CODE and returns no value.
   ---
   This interpreter employs an internally managed stacks of currently
   operating loops, designated by the euonym \"LOOPS\", which, upon each
   encounter of an iteration start, receives at its top the position in
   the CODE immediately following the detection's occasion --- the start
   of loop's body. The repetition of the currently active loop resolves
   to a relocation of the position cursor to the top LOOPS element, that
   is, a return to the loop body beginning; a termination eventuates the
   element's removal. If further loops exist, the process is hence
   applied recursively."
  (declare (type string code))
  
  (when (plusp (length code))
    (let* ((position  0)
           (character (char code position))
           (state     1))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type state               state))
      
      (let ((input NIL)         ;; The last user input.
            (loops NIL))        ;; The currently running loops.
        (declare (type T                input))
        (declare (type (list-of fixnum) loops))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character in the
               CODE, if possible, updates the current CHARACTER, and
               returns no value."
              (setf character
                (when (array-in-bounds-p code (1+ position))
                  (char code (incf position))))
              (values))
             
             (move-to (new-position)
              "Relocates the POSITION cursor to the NEW-POSITION in the
               CODE, updates the current CHARACTER, and returns no
               value."
              (declare (type fixnum new-position))
              (setf position new-position)
              (setf character
                (when (array-in-bounds-p code position)
                  (char code position)))
              (values))
             
             (check-if-loops-exist ()
              "Checks whether at least one running loop exists,
               signifying that the LOOPS stack is not empty, on
               confirmation returning no value, otherwise signaling an
               unspecified error."
              (unless loops
                (error "No running loops available."))
              (values))
             
             (check-if-all-loops-are-terminated ()
              "Checks whether all loops have terminated, signifying the
               LOOP stack's vacancy, on confirmation returning no value,
               otherwise signaling an unspecified error."
              (when loops
                (error "There still exist running loops."))
              (values))
             
             (input-equals-ə-p ()
              "Checks whether the INPUT committed by the user equals the
               sentinel value \"ə\", returning on confirmation a
               ``boolean'' value of ``T'', otherwise ``NIL''."
              (the boolean
                (not (null (and input (string= input "ə")))))))
          
          (loop while character do
            (let ((command (get-command-for character state)))
              (declare (type command command))
              (case command
                (:change-state-to-2
                  (setf state 2)
                  (advance))
                
                (:get-input
                  (format T "~&Please input a value: ")
                  (setf input (read-line))
                  (clear-input)
                  (advance))
                
                (:output
                  (when input
                    (format T "~a" input))
                  (advance))
                
                (:change-state-to-1
                  (setf state 1)
                  (advance))
                
                (:start-loop
                  (cond
                    ;; INPUT = "ə"?
                    ;; => Skip loop.
                    ((input-equals-ə-p)
                      (advance)
                      (loop until (char= character #\e) do
                        (advance))
                      (advance))
                    ;; INPUT != "ə"?
                    ;; => Start loop.
                    (T
                      (push (1+ position) loops)
                      (advance))))
                
                (:end-loop
                  (check-if-loops-exist)
                  (cond
                    ;; INPUT = "ə"?
                    ;; => Terminate loop.
                    ((input-equals-ə-p)
                      (pop loops)
                      (advance))
                    ;; INPUT != "ə"?
                    ;; => Repeat loop.
                    (T
                      (move-to (first loops)))))
                
                (otherwise
                  (error "Invalid character '~c' at position ~d."
                    character position)))))
          
          (check-if-all-loops-are-terminated))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program, which terminates only if presented
;; with a user input of "ə".
(interpret-Schwa "eəaaəəae")
