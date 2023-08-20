;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "JR", devised by the Esolang user "King Ethan" on April
;; 25th, 2018, and designed as an extension of Jonathan Todd Skinner's
;; "Deadfish" language, both in the aspects of its operations, which
;; incorporate character output and console clearing, and its memory's
;; architecture that barters the parvipotent scalar accumulator for an
;; array of octuple componency.
;; 
;; 
;; Instructions
;; ============
;; JR's nature establishes an augmentation of Deafish's quadruple
;; competences, targeting the architecture and output conduit.
;; 
;; == OVERVIEW ==
;; An apercu shall serve in the administration of a cursory mete of
;; gnarity concerning the language's potentials:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   [       | Decrements the current cell by one.
;;   ..................................................................
;;   ]       | Increments the current cell by one.
;;   ..................................................................
;;   ;       | Squares the current cell value.
;;   ..................................................................
;;   .       | Prints the current cell in its verbatim numeric form to
;;           | the standard output.
;;   ..................................................................
;;   ,       | Prints the character corresponding to the current cell
;;           | value, when construed as an ASCII code, to the standard
;;           | output.
;;   ..................................................................
;;   @       | Resets the current cell to zero (0).
;;   ..................................................................
;;   !       | Prints the source of the program, that is, provides a
;;           | quine.
;;   ..................................................................
;;   ~       | Clears the console.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   >       | Moves the cell pointer one step to the right.
;;   ------------------------------------------------------------------
;; 
;; == JR AND DEADFISH ==
;; The faculty for equiparation with Deadfish commorant to JR shall now
;; be limned in a tabular exposition:
;; 
;;   -------------
;;   JR | Deadfish
;;   ---+---------
;;   [  | d
;;   .............
;;   ]  | i
;;   .............
;;   ;  | s
;;   .............
;;   .  | o
;;   .............
;;   ,  | (None)
;;   .............
;;   @  | (None)
;;   .............
;;   !  | (None)
;;   .............
;;   ~  | (None)
;;   .............
;;   <  | (None)
;;   .............
;;   >  | (None)
;;   -------------
;;   
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-01-14
;; 
;; Sources:
;;   [esolang2020JR]
;;   The Esolang contributors, "JR", June 7th, 2020
;;   URL: "https://esolangs.org/wiki/JR"
;;   
;;   [esolang2023Deadfish]
;;   The Esolang contributors, "Deadfish", August 13th, 2023
;;   URL: "https://esolangs.org/wiki/Deadfish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  "The ``output-format'' type defines the options for the printing
   commands applicable during the conversion of a Deadfish source
   program to the more potent JR target language.
   ---
   Deadfish restricts its output to the numeric value of its
   accumulator, while JR grants the programmer the choice betwixt the
   former variant and a character output, the latter of which construes
   the current cell value with as an ASCII code that shall be answered
   by its symbolic equivalent."
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
     attribute, and the programmer is encumbered with the onus of
     invoking the ``standard-console-clearing-scroll-size'' function if
     optating an adjustment to such external influences."))

;;; -------------------------------------------------------

(defun make-standard-console (&key (destination          T)
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
  "Interprets the piece of JR CODE using the INTERPRETER and returns no
   value."
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
  "Interprets the piece of JR CODE using the INTERPRETER and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type string      code))
  (interpreter-process-commands interpreter code)
  (values))

;;; -------------------------------------------------------

(defun execute-JR-shell (interpreter)
  "Executes the JR shell using the INTERPRETER and returns no value."
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
;; -- Implementation of Deadfish-to-JR converter.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-deadfish-command-for (instruction-set character)
  "Returns the ``deadfish-command'' associated in the INSTRUCTION-SET
   with the CHARACTER."
  (declare (type deadfish-instruction-set instruction-set))
  (declare (type character                character))
  (the deadfish-command
    (case instruction-set
      (:standard
        (case character
          (#\i                       :increment)
          (#\d                       :decrement)
          (#\s                       :square)
          (#\o                       :output)
          ((#\Newline #\Space #\Tab) :whitespace)
          (otherwise                 :unknown)))
      (:XKCD
        (case character
          (#\x                       :increment)
          (#\d                       :decrement)
          (#\k                       :square)
          (#\c                       :output)
          ((#\Newline #\Space #\Tab) :whitespace)
          (otherwise                 :unknown)))
      (otherwise
        (error "Invalid instruction set: ~s." instruction-set)))))

;;; -------------------------------------------------------

(defun convert-Deadfish-to-JR (deadfish-code
                               &key (destination     T)
                                    (instruction-set :standard)
                                    (output-format   :numeric))
  "Converts the piece of DEADFISH-CODE, stated using the
   INSTRUCTION-SET, to the equivalent JR code, and prints the result to
   the DESTINATION."
  (declare (type string                   deadfish-code))
  (declare (type destination              destination))
  (declare (type deadfish-instruction-set instruction-set))
  (declare (type output-format            output-format))
  
  (if destination
    (loop
      for token    of-type character across deadfish-code
      and position of-type fixnum    from   0 by 1
      do
      (case (get-deadfish-command-for instruction-set token)
        (:increment
          (write-char #\] destination))
        
        (:decrement
          (write-char #\[ destination))
        
        (:square
          (write-char #\; destination))
        
        (:output
          (case output-format
            (:numeric   (write-char #\. destination))
            (:character (write-char #\, destination))
            (otherwise  (error "Invalid output format: ~s."
                          output-format))))
        
        (:whitespace
          (write-char token destination))
        
        (otherwise
          (error "Invalid character in the Deadfish code at ~
                  position ~d: ~s."
            position token))))
    
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-Deadfish-to-JR deadfish-code
          :destination     output
          :instruction-set instruction-set
          :output-format   output-format)))))




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
;; current cell --- to zero. The Deadfish equivalent comprises:
;;   iissso
;; The output should be:
;;   0
(interpret-JR (make-interpreter) "]];;;.")

;;; -------------------------------------------------------

;; Execute the JR shell. Please note that such a program does not offer
;; the contingency for interruption, that is, the shell must be closed
;; by manual termination of some kind.
(execute-JR-shell (make-interpreter))

;;; -------------------------------------------------------

;; Print to the standard output the JR code
;;   ]];;;.
(convert-Deadfish-to-JR "iissso")

;;; -------------------------------------------------------

;; Print to the standard output the JR code
;;   ]];;;,
;; Note that the desinent JR command produced states the character print
;; operation "," in lieu of the Deadfish equivalent "." --- a corollary
;; of specifying the ``:output-format :character'' option.
(convert-Deadfish-to-JR "iissso" :output-format :character)

;;; -------------------------------------------------------

;; Print to the standard output the JR code
;;   ]];;;.
(convert-Deadfish-to-JR "xxkkkc" :instruction-set :XKCD)

;;; -------------------------------------------------------

;; Executes the equivalent JR code
;;   ]];;;.
(interpret-JR (make-interpreter)
  (convert-Deadfish-to-JR "iissso" :destination NIL))
