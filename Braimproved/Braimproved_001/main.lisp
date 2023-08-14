;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Braimproved", invented by the Esolang user "Cinnamony" and
;; presented on June 21st, 2023, the foundation of which constitutes
;; several enhancement applied to Urban Mueller's "brainfuck" in order
;; to accommodate numeric input and output, more competence in control
;; flows, and a certain treat's jocular dation.
;; 
;; 
;; Concept
;; =======
;; Founded upon brainfuck's command and memory layout, the former of
;; which naits a very constraiend, yet Turing-complete instruction set,
;; whereas the latter adheres to a bilaterally infinite extent of
;; octet-valued cells, each the salvatory to a single datum in the
;; integral range [0, 255], Braimproved supplements enhanced facilities
;; for input, output, control flow, and a jocular donut dispenser.
;; 
;; 
;; Instructions
;; ============
;; A conspicuous augmentation of its brainfuck cleronomy's octuple
;; instruction set entalents Braimproved with fourteen members, several
;; of which encompass extension of extant facilities, whereas a twain
;; operates without an immediately palpable entheus.
;; 
;; == OVERVIEW ==
;; A foundational acquaintance with the language's instructions shall
;; now be imparted by an apercu.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   +       | Increments the current cell by one.
;;   ..................................................................
;;   -       | Decrements the current cell by one.
;;   ..................................................................
;;   ,       | Queries the user for a character and stores its ASCII
;;           | code in the current cell.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds with
;;           | tthe current cell value to the standard output.
;;   ..................................................................
;;   ?       | Queries the user for an signed or unsigned integer
;;           | number and stores it in the current cell.
;;   ..................................................................
;;   !       | Prints the cell value in its numeric form to the
;;           | standard output.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   (       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching ")" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   )       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "(" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   O       | Provides you, the programmer, with a donut.
;;           | This might constitute a symbolic or virtual dation, if
;;           | impounded by the operating machine's incompetence.
;;   ..................................................................
;;   h       | Immediately terminates the program.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-14
;; 
;; Sources:
;;   [esolang2023Braimproved]
;;   The Esolang contributors, "Braimproved", 21st June, 2023
;;   URL: "https://esolangs.org/wiki/Braimproved"
;;   
;;   [vogel2022signeddist]
;;   Theia Vogel, "Signed distance functions in 46 lines of Python",
;;     December 18th, 2022
;;   URL: "https://vgel.me/posts/donut/"
;;   Notes:
;;     - Demonstrates the display of a 2D and 3D donut, in the latter
;;       case animated, on the console using ASCII art.
;;     - The solution employs the method of signed distance functions.
;;     - Direct hyperlink: -> "https://vgel.me/posts/donut/#A_2D_donut".
;;   
;;   [zucconi2016volrendsdf]
;;   Alan Zucconi, "Volumetric Renering: Signed Distance Functions",
;;     July 1st, 2016
;;   URL: "https://www.alanzucconi.com/2016/07/01/
;;         signed-distance-functions/"
;;   Notes:
;;     - Describes signed distance functions for 3D rendering.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest further-parameters)
     &body body)
  "Establishes a new ``deftype'' type definition based upon the
   ``satisfies'' construct, the agnomination of which is desumed from
   the TYPE-NAME, optionally amplecting in its parameter list the
   FURTHER-PARAMETERS, while the anonymous predicate function designates
   its probed object via the CANDIDATE-VARIABLE name, executing the BODY
   forms, and returning the desinent evaluated form's result.
   ---
   If the incipient BODY form constitutes a string, the same is assumed
   to provide a documentation string to the type definition, and is
   hence relocated to the location immediately following the ``deftype''
   parameter list, meanwhile the remaining BODY forms specify the
   predicate body."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@further-parameters)
       ,(when (stringp (first body))
          (pop body))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-predicated-type association-list-of (candidate
                                             &optional
                                               (indicator-type T)
                                               (value-type     T))
  "The ``association-list-of'' defines an association list, or alist,
   composed of zero or more entries, each indicator, or key, of which
   conforms to the INDICATOR-TYPE and associates with value of the
   VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element `(cons ,indicator-type ,value-type)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional
                                         (key-type   T)
                                         (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being   the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and (typep key   key-type)
             (typep value value-type)))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Braimproved commands."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :input-character
    :output-character
    :input-number
    :output-number
    :jump-forward-if-zero
    :jump-back-if-non-zero
    :jump-forward-if-non-zero
    :jump-back-if-zero
    :give-donut
    :halt-program))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping betwixt command names
   and their representative command objects in the form of an
   association list, or alist, the keys, or indicators, of which specify
   the identifiers and associate with the ``command'' correspondences."
  '(association-list-of character command))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines a Braimproved program as a vector of
   zero or more ``command'' objects."
  '(vector command *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines an association of forward and back
   jump positions inside of a Braimproved program in the form of a hash
   table whose keys and value comprehend the connecting indices."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type represents an infinite vector of integer cells
   in the form of a sparse array, realized by a hash table whose keys
   answer to the cell indices, associated with the octet-valued cell
   data."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, such
   as ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifiers.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '((#\> . :move-right)
    (#\< . :move-left)
    (#\+ . :increment)
    (#\- . :decrement)
    (#\, . :input-character)
    (#\. . :output-character)
    (#\? . :input-number)
    (#\! . :output-number)
    (#\[ . :jump-forward-if-zero)
    (#\] . :jump-back-if-non-zero)
    (#\( . :jump-forward-if-non-zero)
    (#\) . :jump-back-if-zero)
    (#\O . :give-donut)
    (#\h . :halt-program))
  "Associates the recognized command identifier with representative
   ``command'' objects.")

;;; -------------------------------------------------------

(defun command-name-p (token)
  "Determines whether the TOKEN is affiliated with a Braimproved
   command, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (assoc token +IDENTIFIERS+ :test #'char=)))))

;;; -------------------------------------------------------

(defun get-command (token)
  "Returns the command associated with the TOKEN, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type character token))
  (the command
    (or (cdr (assoc token +IDENTIFIERS+ :test #'char=))
        (error "No command token: ~s." token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts and returns from the Braimproved source CODE a
   one-dimensional simple array of instructions."
  (declare (type string code))
  (the program
    (coerce
      (loop
        for     token of-type character across code
        when    (command-name-p token)
        collect (get-command token))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (program)
  "Builds and returns for the Braimproved PROGRAM the jump table which
   associates the forward and backward jump positions in the same."
  (declare (type program program))
  (let ((jump-table           (make-hash-table :test #'eql))
        (zero-jump-starts     NIL)    ;; "[" and "]".
        (non-zero-jump-starts NIL))   ;; "(" and ")".
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) zero-jump-starts))
    (declare (type (list-of fixnum) non-zero-jump-starts))
    
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0 by 1
      do
        (case command
          (:jump-forward-if-zero
            (push position zero-jump-starts))
          
          (:jump-back-if-non-zero
            (if zero-jump-starts
              (let ((start-point (pop zero-jump-starts))
                    (end-point   position))
                (declare (type fixnum start-point))
                (declare (type fixnum end-point))
                (setf (gethash start-point jump-table) end-point)
                (setf (gethash end-point   jump-table) start-point))
              (error "Unmatched \"]\" at position ~d." position)))
          
          (:jump-forward-if-non-zero
            (push position non-zero-jump-starts))
          
          (:jump-back-if-zero
            (if non-zero-jump-starts
              (let ((start-point (pop non-zero-jump-starts))
                    (end-point   position))
                (declare (type fixnum start-point))
                (declare (type fixnum end-point))
                (setf (gethash start-point jump-table) end-point)
                (setf (gethash end-point   jump-table) start-point))
              (error "Unmatched \")\" at position ~d." position)))
          
          (otherwise
            NIL)))
    
    (the jump-table
      (cond
        (zero-jump-starts
          (error "Unmatched \"[\"s at positions ~{~d~^, ~}."
            zero-jump-starts))
        (non-zero-jump-starts
          (error "Unmatched \"(\"s at positions ~{~d~^. ~}."
            non-zero-jump-starts))
        (T
          jump-table)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of donut printer.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun donut (radius thickness x y)
  "Calculates and returns the signed distance of the X- and
   Y-coordinates from a donut specified by its RADIUS and THICKNESS,
   with the following conventions holding:
     (a) If the supputated distance is less than or equal to zero (0),
         the point (X, Y) resides inside of the donut.
     (b) If the supputated distance is greater than zero (0), the point
         (X, Y) lies outside of the donut."
  (declare (type real radius))
  (declare (type real thickness))
  (declare (type real x))
  (declare (type real y))
  (the real
    (- (abs (- (sqrt (+ (* x x) (* y y))) radius))
       (/ thickness 2))))

;;; -------------------------------------------------------

(defun donut-contains-point-p (donut-radius donut-thickness x y)
  "Determines whether the X- and Y-coordinates designate a point inside
   of the DONUT specified by the DONUT-RADIUS and the DONUT-THICKNESS,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type real donut-radius))
  (declare (type real donut-thickness))
  (declare (type real x))
  (declare (type real y))
  (the boolean
    (not (null
      (<= (donut donut-radius donut-thickness x y) 0)))))

;;; -------------------------------------------------------

(defun transform-into-viewport (x y viewport-width viewport-height)
  "Applies a transformation to the X- and Y- coordinates which
   capacitates their mapping into a viewport specified by the
   VIEWPORT-WIDTH and VIEWPORT-HEIGHT, and returns two values:
     (1) The transformed x-coordinate.
     (2) The transformed y-coordinate."
  (declare (type integer x))
  (declare (type integer y))
  (declare (type integer viewport-width))
  (declare (type integer viewport-height))
  (the (values real real)
    (values
      (- (* (/ x viewport-width) 2) 1)
      (* (- (* (/ y viewport-height) 2) 1)
         (* 2 (/ viewport-height viewport-width))))))

;;; -------------------------------------------------------

(defun draw-donut (donut-radius donut-thickness
                   viewport-width viewport-height
                   &key (destination T))
  "Prints a donut specified by the DONUT-RADIUS and DONUT-THICKNESS in a
   rectangle viewport delimited horizontally by the VIEWPORT-WIDTH and
   vertically by its VIEWPORT-HEIGHT to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type real        donut-radius))
  (declare (type real        donut-thickness))
  (declare (type integer     viewport-width))
  (declare (type integer     viewport-height))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (flet ((draw-point (mapped-x mapped-y)
              "Depending on whether the MAPPED-X and MAPPED-Y
               coordinates are located inside of the donut, either
               prints a hash sign that represents the shape, or a space
               that simulates the exterior area, to the DESTINATION, and
               returns no value."
              (declare (type real mapped-x))
              (declare (type real mapped-y))
              (the character
                (if (donut-contains-point-p
                      donut-radius donut-thickness mapped-x mapped-y)
                  #\#
                  #\Space))))
        (dotimes (y viewport-height)
          (declare (type integer y))
          (dotimes (x viewport-width)
            (declare (type integer x))
            (multiple-value-bind (mapped-x mapped-y)
                (transform-into-viewport
                  x y viewport-width viewport-height)
              (declare (type real mapped-x))
              (declare (type real mapped-y))
              (write-char
                (draw-point mapped-x mapped-y)
                destination)))
          (write-char #\Newline destination)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (draw-donut
          donut-radius donut-thickness viewport-width viewport-height
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Halt-Condition".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ()
  (:documentation
    "The ``Halt-Condition'' type serves in the apprizal about the
     program's intention to halt."))

;;; -------------------------------------------------------

(defun signal-halt-condition ()
  "Signals a ``Halt-Condition'', intended to express the program's
   desire to immediately terminate its operation."
  (signal 'Halt-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
    (program
     &aux (jump-table (build-jump-table program)))))
  "The ``Interpreter'' class establishes an entity responsible for the
   adhibition of effect to a Braimproved program."
  (program      (error "Missing program.")    :type program)
  (ip           0                             :type fixnum)
  (jump-table   (error "Missing jump table.") :type jump-table)
  (memory       (make-hash-table :test #'eql) :type memory)
  (cell-pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun interpreter-current-cell (interpreter)
  "Returns the byte value stored in the INTERPRETER's current cell."
  (declare (type Interpreter interpreter))
  (the octet
    (gethash
      (interpreter-cell-pointer interpreter)
      (interpreter-memory       interpreter)
      0)))

;;; -------------------------------------------------------

(defun (setf interpreter-current-cell) (new-value interpreter)
  "Stores the NEW-VALUE, contingently succeeding a wrapping into the
   valid byte range of [0, 255] in the INTERPRETER's current cell and
   returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (setf
    (gethash
      (interpreter-cell-pointer interpreter)
      (interpreter-memory       interpreter)
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun interpreter-jump (interpreter)
  "Expected to currently reside at a forward or back jump command, moves
   the INTERPRETER's instruction pointer to the opposite boundary and
   returns no value.
   ---
   If no matching jump point can be determined, an error of an
   unspecified type is signaled."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (or (gethash (interpreter-ip interpreter)
          (interpreter-jump-table interpreter))
        (error "No jump end point associated with the position ~d."
          (interpreter-ip interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-finished-p (interpreter)
  "Determines whether the INTERPRETER's operation is finished, that is,
   its desinent command has been processed, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length (interpreter-program interpreter)))))))

;;; -------------------------------------------------------

(defgeneric interpreter-process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-command-processor (command (interpreter-variable)
                                    &body body)
  "Defines an implementation of the generic function
   ``interpreter-process-command'', employing as the first variable an
   ``Interpreter'' with the parameter name INTERPRETER-VARIABLE, as its
   second an automatically generated command parameter agnomination
   which dispatches on its ``eql''-equality with the COMMAND, evaluating
   the BODY forms, and returning no value."
  (let ((command-variable (gensym)))
    (declare (type symbol command-variable))
    `(defmethod interpreter-process-command
         ((,interpreter-variable Interpreter)
          (,command-variable     (eql ,command)))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type command     ,command-variable))
       (declare (ignore           ,command-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-command-processor :move-right (interpreter)
  (incf (interpreter-cell-pointer interpreter)))

;;; -------------------------------------------------------

(define-command-processor :move-left (interpreter)
  (decf (interpreter-cell-pointer interpreter)))

;;; -------------------------------------------------------

(define-command-processor :increment (interpreter)
  (incf (interpreter-current-cell interpreter)))

;;; -------------------------------------------------------

(define-command-processor :decrement (interpreter)
  (decf (interpreter-current-cell interpreter)))

;;; -------------------------------------------------------

(define-command-processor :input-character (interpreter)
  (setf (interpreter-current-cell interpreter)
    (char-code
      (prog2
        (format T "~&Please input an ASCII character: ")
        (read-char *standard-input* NIL #\Null)
        (clear-input *standard-input*)))))

;;; -------------------------------------------------------

(define-command-processor :output-character (interpreter)
  (write-char
    (code-char
      (interpreter-current-cell interpreter))))

;;; -------------------------------------------------------

(define-command-processor :input-number (interpreter)
  (setf (interpreter-current-cell interpreter)
    (prog2
      (format T "~&Please input an integer number: ")
      (parse-integer
        (read-line *standard-input* NIL "0"))
      (clear-input))))

;;; -------------------------------------------------------

(define-command-processor :output-number (interpreter)
  (format T "~d"
    (interpreter-current-cell interpreter)))

;;; -------------------------------------------------------

(define-command-processor :jump-forward-if-zero (interpreter)
  (when (zerop (interpreter-current-cell interpreter))
    (interpreter-jump interpreter)))

;;; -------------------------------------------------------

(define-command-processor :jump-back-if-non-zero (interpreter)
  (unless (zerop (interpreter-current-cell interpreter))
    (interpreter-jump interpreter)))

;;; -------------------------------------------------------

(define-command-processor :jump-forward-if-non-zero (interpreter)
  (unless (zerop (interpreter-current-cell interpreter))
    (interpreter-jump interpreter)))

;;; -------------------------------------------------------

(define-command-processor :jump-back-if-zero (interpreter)
  (when (zerop (interpreter-current-cell interpreter))
    (interpreter-jump interpreter)))

;;; -------------------------------------------------------

(define-command-processor :give-donut (interpreter)
  (draw-donut 0.4 0.3 80 20 :destination T)
  (format T "~&~%~80:@<~a~>" "== FOR YOU =="))

;;; -------------------------------------------------------

(define-command-processor :halt-program (interpreter)
  (signal-halt-condition))

;;; -------------------------------------------------------

(defun interpreter-execute (interpreter)
  "Executes the Braimproved program stored in the INTERPRETER and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-finished-p interpreter) do
    (handler-case
      (progn
        (interpreter-process-command interpreter
          (aref
            (interpreter-program interpreter)
            (interpreter-ip      interpreter)))
        (incf (interpreter-ip interpreter)))
      (Halt-Condition ()
        (loop-finish))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Braimproved (code)
  "Interprets the piece of Braimproved source CODE and returns no
   value."
  (declare (type string code))
  (interpreter-execute
    (make-interpreter
      (extract-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello world!".
(interpret-Braimproved
  "++++++++++[>++++++++++<-]>+>+++++++++[>++++++++<-]>.<<.>++++++++++++[<<+++++++++>>-]<<..>>+++++++++++[>>++++++++++<<-]>>+.>++++++++[>++++<-]>.+<<++++++++.--------.+++.
<<<<.>-.>>>>>.")

;;; -------------------------------------------------------

;; Infinitely repeating textual cat program.
(interpret-Braimproved "+[,.]")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program which terminates on an input
;; equal to zero (0).
(interpret-Braimproved "+[?!]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Braimproved "?(!h)[!]")

;;; -------------------------------------------------------

;; Kiwiscript: Print "kiwi" in minuscles.
(interpret-Braimproved
  "++++++++++[>+++++<-]>+++[<+>-]<[>++>++>++<<<-]>+>->+++++++++++++<<.>.>.<.")
