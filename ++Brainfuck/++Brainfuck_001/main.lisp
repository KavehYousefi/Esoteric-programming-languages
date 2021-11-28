;; Date: 2021-11-28
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/%2B%2BBrainfuck"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, the keys of which are of the KEY-TYPE and the values of the
   VALUE-TYPE, both of which default to the comprehensive ``T''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-++Brainfuck (code)
  "Interprets the ++Brainfuck CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      
      (labels
          ((advance ()
            "Advances the POSITION in the CODE by one step, if viable,
             and returns no value."
            (if (< position (1- (length code)))
              (setf character (char code (incf position)))
              (setf character NIL))
            (values))
           
           (recede ()
            "Moves the POSITION in the CODE back by one step, if viable,
             returns returns no value."
            (when (plusp position)
              (setf character (char code (decf position))))
            (values))
           
           (command-p (candidate)
            "Checks whether the CANDIDATE character designates a
             command, returning the CANDIDATE itself of affirmation,
             and ``NIL'' on refutation."
            (declare (type character candidate))
            (the (or null character)
              (find candidate "><+-.,:;)(01{}" :test #'char=)))
           
           (find-next-command ()
            "Advances the POSITION to the location of the next command
             and returns no value."
            (loop while (and character (not (command-p character))) do
              (advance))
            (values))
           
           (skip-next-command ()
            "Moves the POSITION immediately past the next command and
             returns no value."
            (find-next-command)
            (advance)
            (values)))
        
        (let ((memory                  (make-hash-table :test #'equal))
              (pointer                 0)
              (last-colon-was-true     NIL)
              (last-semicolon-was-true NIL))
          (declare (type (hash-table-of integer integer) memory))
          (declare (type integer                         pointer))
          (declare (type boolean last-colon-was-true))
          (declare (type boolean last-semicolon-was-true))
          
          (loop do
            (case character
              
              ((NIL)
                (loop-finish))
              
              (#\>
                (incf pointer)
                (advance))
              
              (#\<
                (decf pointer)
                (advance))
              
              (#\+
                (incf (gethash pointer memory 0))
                (advance))
              
              (#\-
                (decf (gethash pointer memory 0))
                (advance))
              
              (#\.
                (write-char (code-char (gethash pointer memory 0)))
                (advance))
              
              (#\,
                (format T "~&Please input a character: ")
                (setf (gethash pointer memory) (char-code (read-char)))
                (clear-input)
                (advance))
              
              ;; The current memory cell equals 0?
              ;; => Yes: Advance.
              ;; => No:  Skip the next command.
              (#\:
                (cond
                  ((zerop (gethash pointer memory 0))
                    (setf last-colon-was-true T)
                    (advance))
                  (T
                    (setf last-colon-was-true NIL)
                    (advance)
                    (skip-next-command))))
              
              ;; The current memory cell does not equal 0?
              ;; => Yes: Advance.
              ;; => No:  Skip the next command.
              (#\;
                (cond
                  ((not (zerop (gethash pointer memory 0)))
                    (setf last-semicolon-was-true T)
                    (advance))
                  (T
                    (setf last-semicolon-was-true NIL)
                    (advance)
                    (skip-next-command))))
              
              (#\)
                (advance)
                (loop with level of-type fixnum = 0 do
                  (case character
                    ((NIL)
                      (error "Could not find a matching '(' for ')'."))
                    (#\)
                      (incf level))
                    (#\(
                      (cond
                        ((zerop level)
                          (advance)
                          (loop-finish))
                        (T
                          (decf level)
                          (advance))))
                    (T
                      (advance)))))
              
              (#\(
                (recede)
                (loop with level of-type fixnum = 0 do
                  (case character
                    ((NIL)
                      (error "Could not find a matching ')' for '('."))
                    (#\(
                      (incf level))
                    (#\)
                      (cond
                        ((zerop level)
                          (advance)
                          (loop-finish))
                        (T
                          (decf level)
                          (recede))))
                    (T
                      (recede)))))
              
              ;; Last ";" was false?
              ;; => Yes: Advance.
              ;; => No:  Skip the next command.
              (#\0
                (cond
                  ((not last-semicolon-was-true)
                    (advance))
                  (T
                    (advance)
                    (skip-next-command))))
              
              ;; Last ":" was false?
              ;; => Yes: Advance.
              ;; => No:  Skip the next command.
              (#\1
                (cond
                  ((not last-colon-was-true)
                    (advance))
                  (T
                    (advance)
                    (skip-next-command))))
              
              (#\}
                (cond
                  ((zerop (gethash pointer memory 0))
                    (skip-next-command))
                  (T
                    (advance))))
              
              (#\{
                (cond
                  ((not (zerop (gethash pointer memory 0)))
                    (advance)
                    (skip-next-command))
                  (T
                    (advance))))
              
              (T
                (advance))))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prints the letter "A".
(interpret-++Brainfuck "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.")

;;; -------------------------------------------------------

;; Cat program.
(interpret-++Brainfuck ";),.(")

;;; -------------------------------------------------------

;; Prints "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
;; 
;; In pseudocode:
;;   let printer  <- 65  // Minimum ASCII character code, with 65 = "A" as the incipient letter.
;;   let iterator <- 26  // Number of letters to print, with 26 being the size of the alphabet.
;;   
;;   moveRight           // Move to iterator in order to prepare for moving to printer.
;;   startLoop           // For repeated execution. Is skipped on the first time.
;;   moveLeft            // Move to printer.
;;   print               // Print printer.
;;   moveRight           // Move to iterator.
;;   decrement           // Decrement iterator.
;;   skipIfNonZero       // Check if iterator is zero, breaking the loop if so.
;;   endLoop             // Is skipped if iterator is zero, thus terminating the loop.
(interpret-++Brainfuck
  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   >
   ++++++++++++++++++++++++++
   :
   )
   <
   .
   +
   >
   -
   ;
   (
   ")

;;; -------------------------------------------------------

;; Print the character associated with the ASCII codes from one (1) to
;; the character specified by the user per prompt.
;; Memory layout:
;;   mem[0] = input
;;   mem[1] = copy
(interpret-++Brainfuck "
STORE INPUT IN INPUT CELL
,
SKIP LOOP OBVIATION ON FIRST ENTRANCE
:
)
TO COPY CELL
>
BUILD COPY CELL
+
PRINT COPY CELL
.
BACK TO INPUT CELL
<
UPDATE INPUT CELL VALUE IN ITS AGENCY AS THE LOOP TERMINATION CONDITION
-
SKIP THE LOOP REPETITION IF THE INPUT CELL VALUE HAS DECREASED TO ZERO
;
(
")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The memory layout is the following:
;;   mem[0] = input
;;   mem[1] = copy
;;   mem[2] = test
;; 
(interpret-++Brainfuck "
INPUT
,

SET THE TEST CELL TO MINUS 48 TO LATER INCREMENT IT TO GREATER OR EQUAL ZERO
>>
------------------------------------------------

BACK TO INPUT
<<

SKIP ON FIRST ENTRANCE
:
)
TO COPY CELL
>
BUILD COPY CELL
+
REPLACE THIS TEXT WITH A DOT TO TRACE THE COPY CELL INCREMENTATION
>
COMPUTE TEST CELL
+
BACK TO INPUT CELL
<<
-
REPEAT UNTIL THE INPUT CELL IS ZERO
THE COPY CELL WILL THE EQUAL THE ORIGINAL USER INPUT
THE TEST CELL WILL CONTAIN A NUMBER EQUAL TO OR GREATER THAN ZERO FOR SIMULATING A CONDITIONAL
;
(

TO TEST CELL
>>
IF THE TEST CELL IS ZERO THE FOLLOWING LOOP WILL BE SKIPPED
:
SKIPPING IS REALIZED IF THE TEST CELL EQUALS ZERO
)
TO COPY CELL
<
PRINT COPY CELL
.
TO TEST CELL
>
(


THIS SECTION IS ONLY REACHED IF INPUT IS ZERO AND PRINTS THE COPY CELL ONCE
<
.
")

;;; -------------------------------------------------------

;; An uncommented version of the above truth-machine.
(interpret-++Brainfuck ",>>------------------------------------------------<<:)>+>+<<-;(>>:)<.>(<.")
