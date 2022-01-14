;; Date: 2022-01-14
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/JR"
;;   -> "https://esolangs.org/wiki/Deadfish"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a data sink for output operations,
   compatible, for instance, with ``format'' and ``write''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype deadfish-instruction-set ()
  "The ``deadfish-instruction-set'' type defines the recognized variants
   of the Deadfish instruction set."
  '(member :standard :XKCD))

;;; -------------------------------------------------------

(deftype output-format ()
  "The ``output-format'' type defines the data type of output format
   applied during the conversion of Deadfish source program to the more
   potent JR language.
   ---
   Deadfish restricts its output to the numeric value of its
   accumulator, while JR grants the programmer the choice betwixt the
   former variant and a character output, the latter of which construes
   the current cell value with its ASCII code."
  '(member :numeric :character))

;;; -------------------------------------------------------

(deftype deadfish-command ()
  "The ``deadfish-command'' type defines a set of command identifiers
   which in an abstract fashion associate with the actual tokens in a
   piece of Deadfish code."
  '(member :increment :decrement :square :output :whitespace :unknown))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Console".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Console ()
  ()
  (:documentation
    "The ``Console'' interface describes a data sink intended for the
     output of information from a program to the user side."))

;;; -------------------------------------------------------

(defgeneric console-print-number (console number)
  (:documentation
    "Prints the NUMBER to the CONSOLE and returns the modified
     CONSOLE."))

;;; -------------------------------------------------------

(defgeneric console-print-character (console character)
  (:documentation
    "Prints the CHARACTER to the CONSOLE and returns the modified
     CONSOLE."))

;;; -------------------------------------------------------

(defgeneric console-print-string (console string)
  (:documentation
    "Prints the STRING's characters in an unquoted form to the CONSOLE
     and returns the modified CONSOLE."))

;;; -------------------------------------------------------

(defgeneric console-clear (console)
  (:documentation
    "Clears the CONSOLE's content and returns the modified CONSOLE."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Standard-Console".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Standard-Console (Console)
  ((destination
    :initarg       :destination
    :initform      T
    :type          destination
    :documentation "The data sink to write numbers or characters to.")
   (clearing-scroll-size
    :initarg       :clearing-scroll-size
    :initform      10
    :accessor      standard-console-clearing-scroll-size
    :type          (integer 0 *)
    :documentation "The number of newlines to print to the DESTINATION
                    in order to simulate the clearing of this console."))
  (:documentation
    "The ``Standard-Console'' class provides an output commodity which
     manipulates a traditional Common Lisp destination --- either the
     standard output, a stream, or a dynamic string --- in order to
     convey information to the user.
     ---
     Being based upon the notion of the output conduit's perpetual
     extension, as counterdistinguished to the maintenance of a
     random-access data structure, a veridical clearing of the console
     exceeds the underlying subtrate's capacities. Instead, a
     configurable number of newlines avail as separators, in the best
     case transporting the data to conceal, present inside of the
     visible window, to a space which no longer provides a venue on the
     undesired portion.
     ---
     If the user, for instance, operates upon a terminal of 15 lines
     capacity, the ``Standard-Console'' instance should be initialized
     anenst its ``clearing-scroll-size'' property to apply 15 newlines
     as a means for shifting the current output upwards and outside of
     the visible area. Of course, the predicament of adjustable
     terminals, or windows, cannot be meliorated by this static console
     attribute, and programmer is encumbered with the onus of invoking
     the '`standard-console-clearing-scroll-size'' function if optating
     an adjustment to such external influences."))

;;; -------------------------------------------------------

(defun make-standard-console (&key (destination T)
                                   (clearing-scroll-size 10))
  "Creates and returns a ``Standard-Console'' operating on the
   DESTINATION as its data sink, and utilizing the CLEARING-SCROLL-SIZE
   as the number of newline to simulate its clearing."
  (declare (type destination   destination))
  (declare (type (integer 0 *) clearing-scroll-size))
  (the Standard-Console
    (make-instance 'Standard-Console
      :destination          destination
      :clearing-scroll-size clearing-scroll-size)))

;;; -------------------------------------------------------

(defmethod console-print-number ((console Standard-Console)
                                 (number  integer))
  (declare (type Standard-Console console))
  (declare (type integer          number))
  (format (slot-value console 'destination) "~d" number)
  (the Standard-Console console))

;;; -------------------------------------------------------

(defmethod console-print-character ((console   Standard-Console)
                                    (character character))
  (declare (type Standard-Console console))
  (declare (type character        character))
  (format (slot-value console 'destination) "~a" character)
  (the Standard-Console console))

;;; -------------------------------------------------------

(defmethod console-print-string ((console Standard-Console)
                                 (string  string))
  (declare (type Standard-Console console))
  (declare (type string           string))
  (format (slot-value console 'destination) "~a" string)
  (the Standard-Console console))

;;; -------------------------------------------------------

(defmethod console-clear ((console Standard-Console))
  (declare (type Standard-Console console))
  (format (slot-value console 'destination) "~v%"
    (slot-value console 'clearing-scroll-size))
  (the Standard-Console console))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((memory
    :initarg       :memory
    :initform      (make-array 8
                     :element-type    'integer
                     :initial-element 0)
    :type          (vector integer 8)
    :documentation "The 8-cell array storing the JR program's data.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          (integer 0 7)
    :documentation "A pointer into the MEMORY, referencing its currently
                    active cell.")
   (copy-of-code
    :initarg       :copy-of-code
    :initform      (make-array 0
                     :element-type 'character
                     :adjustable   T
                     :fill-pointer 0)
    :type          string
    :documentation "A buffer to store the processed JR code into, as a
                    provision in the case of a quine's request.")
   (quine-requested-p
    :initarg       :quine-requested-p
    :initform      NIL
    :type          boolean
    :documentation "A flag which determines whether, at the end of the
                    program, the complete processed source code shall be
                    printed, producing a quine.")
   (console
    :initarg       :console
    :initform      (make-standard-console)
    :type          Console
    :documentation "The console to airt the output to."))
  (:documentation
    "The ``Interpreter'' class encapsulates all information requisite
     for interpreting a series of JR commands without loss of
     information."))

;;; -------------------------------------------------------

(defun make-interpreter (&key (console (make-standard-console)))
  "Creates and returns a new ``Interpreter'' employing the CONSOLE for
   its output operations."
  (declare (type Console console))
  (the Interpreter (make-instance 'Interpreter :console console)))

;;; -------------------------------------------------------

(defun interpreter-process-commands (interpreter code)
  "Interprets the piece of JR CODE, using the CONSOLE as an output
   conduit for printing operations, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type string      code))
  
  (with-slots (memory pointer copy-of-code quine-requested-p console)
      interpreter
    (declare (type (vector integer 8) memory))
    (declare (type (integer 0 7)      pointer))
    (declare (type string             copy-of-code))
    (declare (type boolean            quine-requested-p))
    (declare (type Console            console))
    
    (flet
        ((current-cell ()
          "Returns the value stored in the MEMORY cell at the POINTER."
          (the integer
            (aref memory pointer)))
         
         ((setf current-cell) (new-value)
          "Sets the value stored in the MEMORY cell at the POINTER, and
           returns the NEW-VALUE."
          (declare (type integer new-value))
          (setf (aref memory pointer)
            (cond
              ((= new-value  -1) 0)
              ((= new-value 256) 0)
              (T                 new-value)))
          (the integer (aref memory pointer)))
         
         (memorize-character (character)
          "Stores the CHARACTER into the COPY-OF-CODE, necessary for
           providing a quine, and returns no value."
          (declare (type character character))
          (vector-push-extend character copy-of-code)
          (values)))
      
      (loop for token of-type character across code do
        (memorize-character token)
        
        (case token
          ;; End of file. => Terminate.
          ((NIL)
            (loop-finish))
          
          ;; Decrement the current cell.
          (#\[
            (decf (current-cell)))
          
          ;; Increment the current cell.
          (#\]
            (incf (current-cell)))
          
          ;; Square the current cell.
          (#\;
            (setf (current-cell)
                  (* (current-cell)
                     (current-cell))))
          
          ;; Print the current cell as a number.
          (#\.
            (console-print-number console (current-cell)))
          
          ;; Print the current cell as an ASCII character.
          (#\,
            (console-print-character console
              (code-char (current-cell))))
          
          ;; Reset the current cell to zero.
          (#\@
            (setf (current-cell) 0))
          
          ;; Print the program source code.
          (#\!
            (setf quine-requested-p T))
          
          ;; Clear the console.
          (#\~
            (console-clear console))
          
          ;; Move the cell pointer to the left.
          (#\<
            (when (plusp pointer)
              (decf pointer)))
          
          ;; Move the cell pointer to the right.
          (#\>
            (when (< pointer (1- (length memory)))
              (incf pointer)))
          
          ;; Tolerate whitespaces and layout elements.
          ((#\Newline #\Space #\Tab)
            NIL)
          
          ;; According to the rules of Deadfish an error incites the
          ;; printing of a newline character.
          (otherwise
            (console-print-character console #\Newline))))
      
      (when quine-requested-p
        (console-print-string console copy-of-code))))
  
  (the Interpreter interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of main operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-JR (interpreter code)
  "Interprets the piece of JR CODE using the INTERPRETER, and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type string      code))
  (interpreter-process-commands interpreter code)
  (values))

;;; -------------------------------------------------------

(defun execute-JR-shell (interpreter)
  "Executes the JR shell using the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (loop do
    (format T "~&>> ")
    (let ((input (read-line)))
      (declare (type (or null string) input))
      (clear-input)
      (unless input
        (loop-finish))
      (interpreter-process-commands interpreter input)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-JR
  (make-interpreter)
  "
  ]]];[;]]]]]]]],
  @]];]]];[[[[[
  >]]];];],]]]]]]],,]]],
  <,[[[[[[[[[[[[,
  >>]]];]];[[,
  <,]]],[[[[[[,
  @]]];];,
  <],
  ")

;;; -------------------------------------------------------

;; Quine.
(interpret-JR (make-interpreter) "!")

;;; -------------------------------------------------------

;; Demonstrates the computational peculiarity of the underlying
;; "Deadfish" heritage, which for the values -1 and 256 simulates an
;; overflow by setting the accumulator --- or, in the case of JR, the
;; current cell, to zero. The Deadfish equivalent comprises:
;;   iissso
;; The output should be:
;;   0
(interpret-JR (make-interpreter) "]];;;.")

;;; -------------------------------------------------------

(execute-JR-shell (make-interpreter))
