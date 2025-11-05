;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file furnishes an Esofish interpreter, reliant in its services'
;; provision upon the "5-programming-language" package's opiferous
;; vouchsafements.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :esofish)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\",
   producing for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE, either
   producing a fresh instance or, upon its compliance with this type,
   responding with the unmodified input itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the "5" language mode operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designates-a-5-mode-p (candidate)
  "Determines whether the CANDIDATE number designates a variation of the
   \"5\" programming language, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type integer candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (typep candidate '5-mode))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Esofish interpreter.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Esofish-Interpreter ()
  ((current-program
    :initarg       :current-program
    :initform      ""
    :type          simple-string
    :documentation "The current line of Esofish source code to
                    execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The zero-based position into the CURRENT-PROGRAM.")
   (accumulator
    :initform      0
    :type          integer
    :documentation "The program memory as a signed integer
                    accumulator."))
  (:documentation
    "The ``Esofish-Interpreter'' class is assigned the onus of an
     Esofish program's evaluation in order to entalent the same with
     actual efficacy."))

;;; -------------------------------------------------------

(defun make-an-esofish-interpreter (&optional (initial-program ""))
  "Creates and returns a fresh ``Esofish-Interpreter'' whose dedication
   at its inchoation appertains to the optional INITIAL-PROGRAM's
   processing."
  (declare (type string initial-program))
  (the Esofish-Interpreter
    (make-instance 'Esofish-Interpreter
      :current-program
        (convert-into-a-simple-string initial-program))))

;;; -------------------------------------------------------

(defun advance-to-the-next-esofish-symbol (interpreter)
  "Advances the Esofish INTERPRETER's instruction pointer (IP) to the
   subsequent position in its source, if possible, and returns no
   value."
  (declare (type Esofish-Interpreter interpreter))
  (incf (slot-value interpreter 'ip)))

;;; -------------------------------------------------------

(defun conclude-the-current-esofish-program (interpreter)
  "Relocates the Esofish INTERPRETER's instruction pointer (IP) to the
   end of its program, thus terminating its execution, and returns no
   value."
  (declare (type Esofish-Interpreter interpreter))
  (setf (slot-value interpreter 'ip)
    (length
      (slot-value interpreter 'current-program)))
  (values))

;;; -------------------------------------------------------

(defun request-the-current-esofish-symbol (interpreter)
  "Returns the symbol commorant in the Esofish INTERPRETER's source at
   the current instruction pointer (IP) position."
  (declare (type Esofish-Interpreter interpreter))
  (the character
    (char
      (slot-value interpreter 'current-program)
      (slot-value interpreter 'ip))))

;;; -------------------------------------------------------

(defun current-esofish-program-is-exhausted-p (interpreter)
  "Determines whether the Esofish INTERPRETER's source has been
   processed in its entirety, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Esofish-Interpreter interpreter))
  (the boolean
    (not
      (array-in-bounds-p
        (slot-value interpreter 'current-program)
        (slot-value interpreter 'ip)))))

;;; -------------------------------------------------------

(defun query-for-a-line-of-esofish-code (interpreter)
  "Queries the standard input conduit for a line of Esofish code, stores
   the same in the INTERPRETER, and returns no value."
  (declare (type Esofish-Interpreter interpreter))
  (format T "~&>> ")
  (finish-output)
  (with-slots (current-program ip) interpreter
    (declare (type simple-string current-program))
    (declare (type fixnum        ip))
    (psetf
      current-program (convert-into-a-simple-string
                        (read-line NIL NIL ""))
      ip              0))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defun normalize-the-esofish-accumulator (interpreter)
  "Applies the dioristic Deadfish rule, thilk expresses the stipulation
   that an accumulator value of -1 or 256 ought to relapse to the
   inchoate state of zero (0), to the Esofish INTERPRETER's internal
   accumulator, and returns no value."
  (declare (type Esofish-Interpreter interpreter))
  (with-slots (accumulator) interpreter
    (declare (type integer accumulator))
    (when (or (= accumulator -1)
              (= accumulator 256))
      (setf accumulator 0)))
  (values))

;;; -------------------------------------------------------

(defun execute-the-esofish-accumulator-as-a-5-program (interpreter)
  "Construes the Esofish INTERPRETER's current accumulator state as the
   identifier of a \"5\" programming language variant, executes the
   source, commencing from the contemporaneous inclusive instruction
   pointer (IP) location into the same, as a program in this \"5\"
   language variant, subsequently relocates the IP position to the
   source's end, and returns no value."
  (declare (type Esofish-Interpreter interpreter))
  (with-slots (accumulator current-program ip) interpreter
    (declare (type integer       accumulator))
    (declare (type simple-string current-program)
             (ignorable          current-program))
    (declare (type fixnum        ip)
             (ignorable          ip))
    (when (designates-a-5-mode-p accumulator)
      (interpret-the-5-code
        (subseq current-program ip)
        :mode accumulator))
    (conclude-the-current-esofish-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun process-the-current-esofish-symbol (interpreter)
  "Evaluates the Esofish INTERPRETER's currently selected symbol and
   returns no value."
  (declare (type Esofish-Interpreter interpreter))
  (with-slots (accumulator) interpreter
    (declare (type integer accumulator))
    (case (request-the-current-esofish-symbol interpreter)
      (#\d
        (decf accumulator))
      (#\i
        (incf accumulator))
      (#\o
        (format T "~d~%" accumulator))
      (#\s
        (setf accumulator
          (* accumulator accumulator)))
      (#\r
        (advance-to-the-next-esofish-symbol             interpreter)
        (execute-the-esofish-accumulator-as-a-5-program interpreter))
      (otherwise
        (terpri))))
  (advance-to-the-next-esofish-symbol interpreter)
  (normalize-the-esofish-accumulator  interpreter)
  (values))

;;; -------------------------------------------------------

(defun process-the-current-esofish-program (interpreter)
  "Processes the Esofish INTERPRETER's current program and returns no
   value."
  (declare (type Esofish-Interpreter interpreter))
  (loop until (current-esofish-program-is-exhausted-p interpreter) do
    (process-the-current-esofish-symbol interpreter))
  (values))

;;; -------------------------------------------------------

(defun start-the-esofish-interpreter (interpreter)
  "Launches the Esofish INTERPRETER, perpetually requesting and
   processing lines of user inputs, returning, upon an eventual
   abortion, no value."
  (declare (type Esofish-Interpreter interpreter))
  (loop do
    (process-the-current-esofish-program interpreter)
    (query-for-a-line-of-esofish-code    interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-esofish (&optional (initial-code ""))
  "Launches the Esofish interpreter, optionally conveying an inicipial
   instruction sequence in the form of the INITAIL-CODE, and
   subsequently engaging in a perpetuation which request and processes
   lines of user input, finally, if aborted in some fashion, returning
   no value."
  (declare (type string initial-code))
  (start-the-esofish-interpreter
    (make-an-esofish-interpreter initial-code))
  (values))
