;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple parser and interpreter for the
;; esoteric programming language "Coeus" invented by the Esolang user
;; "Tetrapyronia".
;; 
;; 
;; Concept
;; =======
;; Most commands constitute sequences of two characters, the first of
;; which determines the command name, the second the name of one of the
;; four program registers. In addition to the input and output
;; capabilities, operations for incrementing, decrementing, and swapping
;; register values exist. All data maintenance is exclusively restricted
;; to the programs' registers, and all data is defined merely in terms
;; of real-valued scalars. No character input, output, or processing can
;; be operated.
;; 
;; One of the most conspicuous features of Coeus, apart from its very
;; homogeneous command syntax, resides in the processing of code by
;; more than one program, albeit in a subordinate fashion instead of
;; parallel, but, however, endowed with its own set of registers as its
;; state's delegate. This facility is requested only upon the sole
;; iteration instruction.
;; 
;; 
;; Implementation
;; ==============
;; This implementation models each independent program as a structure,
;; the moderate weight and plain appearance being justifications of its
;; choice. A program possesses merely four pieces of data: its four
;; registers A, B, C, and D, each of them a repository for a single
;; real-type number.
;; 
;; The Coeus parsing routine, the chief point of entrance into its
;; services, expects, apart from the source code, a program to regard in
;; its own context as its main program. With the iteration instruction
;; realizing a cascade, this ``parse-coeus'' function operates
;; recursively, such that a subprogram, if initiated by calling
;; ``parse-coeus'' with the code and this subordinate processor,
;; requires knowledge about the position in the code at which to begin
;; its private code section. Hence, a third parameter is necessitated:
;; the start position in the code. In the case of a subprogram's
;; termination, the superordinate program requires knowledge about the
;; end position of its subordinate in order to locate its own code index
;; pointer past the processed code; hence, the parsing function must
;; return its current position in the code. The signature of
;; ``parse-coeus'' thus must conform to:
;;   lambda(code program start) => end
;; Where CODE is the Coeus code string, PROGRAM the responsible
;; ``Program'' object, defaulting to the main program instance, START
;; the non-negative integer index into the CODE demarcating the position
;; at which the PROGRAM shall start reading the same, and END the
;; non-negative integer index into the CODE where the PROGRAM has
;; terminated processing the same.
;; 
;; If encounter necessitates this, a subprogram is initiated by creating
;; a new ``Program'' instance and invoking it. The enclosing program
;; maintains responsibility for the objection instantiation and
;; continuation at the position in the code where the subordinate unit
;; ceased its operation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-06-12
;; 
;; Sources:
;;   [esolang2020Coeus]
;;   The Esolang contributors, "Coeus", November 11th, 2020
;;   URL: "https://esolangs.org/wiki/Coeus"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Program
  "Represents a Coeus program, consisting of four register."
  (register-a 1 :type real :read-only NIL)
  (register-b 0 :type real :read-only NIL)
  (register-c 0 :type real :read-only NIL)
  (register-d 0 :type real :read-only NIL))

;;; -------------------------------------------------------

(defun register-at (program register-name)
  "Returns the value of the PROGRAM register designated by the
   REGISTER-NAME."
  (declare (type Program   program))
  (declare (type character register-name))
  (the real
    (case register-name
      (#\A (program-register-a program))
      (#\B (program-register-b program))
      (#\C (program-register-c program))
      (#\D (program-register-d program))
      (T   (error "Attempt to read invalid register name: ~s.~@
                   The only valid register names are 'A', 'B', 'C' ~
                   and 'D'."
                   register-name)))))

;;; -------------------------------------------------------

(defun (setf register-at) (new-value program register-name)
  "Sets the value of the PROGRAM register designated by the
   REGISTER-NAME to the NEW-VALUE, and returns the modified PROGRAM."
  (declare (type real      new-value))
  (declare (type Program   program))
  (declare (type character register-name))
  (case register-name
    (#\A (setf (program-register-a program) new-value))
    (#\B (setf (program-register-b program) new-value))
    (#\C (setf (program-register-c program) new-value))
    (#\D (setf (program-register-d program) new-value))
    (T   (error "Attempt to write to invalid register name: ~s.~@
                 The only valid register names are 'A', 'B', 'C' ~
                 and 'D'."
                register-name)))
  (the Program program))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-coeus (code &optional (program (make-program)) (start 0))
  "Parses and interprets the CODE, starting at the START position, using
   the PROGRAM, and returns the position in the CODE where the
   processing has terminated."
  (declare (type string  code))
  (declare (type Program program))
  (declare (type fixnum  start))
  (let ((position start))
    (declare (type fixnum position))
    
    (flet ((expect-character (expected-character)
             "Checks whether the character at the current POSITION in
              the CODE equals the EXPECTED-CHARACTER, advancing the
              POSITION to the next index on success, and signaling an
              error on mismatch."
             (declare (type character expected-character))
             (let ((current-character (char code position)))
               (declare (type character current-character))
               (unless (char= current-character expected-character)
                 (error "Expected ~s but received ~s at position ~d."
                        expected-character current-character position))
               (incf position)))
           
           (read-register-name ()
             "Reads from the CODE, starting at the current POSITION, a
              register name, advancing the POSITION past the consumed
              name character, and returning the obtained register name."
             (let ((register-name (char code position)))
               (declare (type character register-name))
               (unless (member register-name '(#\A #\B #\C #\D)
                               :test #'char=)
                 (error "Invalid register name ~s at position ~d."
                        register-name position))
               (incf position)
               (the character register-name)))
           
           (skip-code-section ()
             "Starting at the current POSITION, skips all characters
              until a closing bracket ']' matching in nesting the
              conceited opening one '[' is found, retaining the POSITION
              on that of this ']' character and returning POSITION."
             (loop
               with nesting-level     of-type integer   = 0
               for  character-to-skip of-type character = (char code position)
               do   (case character-to-skip
                      (#\]
                        (cond
                          ((<= nesting-level 0)
                            (loop-finish))
                          (T
                            (decf nesting-level)
                            (incf position))))
                      (#\[
                        (incf nesting-level)
                        (incf position))
                      (otherwise
                        (incf position))))
             (the fixnum position)))
      (loop
        while (< position (length code))
        for   character of-type character = (char code position)
        do
        (case character
          (#\a
            (expect-character #\a)
            (format T "~a" (register-at program (read-register-name))))
          
          (#\b
            (expect-character #\b)
            (let ((register-name (read-register-name)))
              (declare (type character register-name))
              (setf (register-at program #\A)
                    (* (register-at program #\A)
                       (register-at program register-name)))))
          
          (#\c
            (expect-character #\c)
            (let ((register-name (read-register-name)))
              (declare (type character register-name))
              (setf (register-at program register-name)
                    (/ 1 (register-at program register-name)))))
          
          (#\d
            (expect-character #\d)
            (incf (register-at program (read-register-name))))
          
          (#\e
            (expect-character #\e)
            (decf (register-at program (read-register-name))))
          
          (#\f
            (expect-character #\f)
            (rotatef (register-at program #\A)
                     (register-at program (read-register-name))))
          
          ;; @TODO: Check whether the formula is adequately represented
          ;;        by this implementation.
          (#\g
            (expect-character #\g)
            (let ((register-name (read-register-name)))
              (declare (type character register-name))
              (setf (register-at program register-name)
                    (log (register-at program #\A)
                         (register-at program register-name)))))
          
          (#\h
            (expect-character #\h)
            (let ((register-name (read-register-name))
                  (user-input    NIL))
              (declare (type character register-name))
              (declare (type T         user-input))
              (loop do
                (format T "~&Please input a real-valued number: ")
                (setf user-input (read))
                (when (realp user-input)
                  (loop-finish)))
              (setf (register-at program register-name) user-input)))
          
          (#\[
            (expect-character #\[)
            
            (let ((check-register-name   NIL)   ;; X
                  (output-register-name  NIL)   ;; Y
                  (output-register-value NIL))  ;; V = subprogram.Y
              (declare (type (or null character) check-register-name))
              (declare (type (or null character) output-register-name))
              (declare (type (or null real)      output-register-value))
              
              (setf check-register-name  (read-register-name))
              (setf output-register-name (read-register-name))
              (expect-character #\])
              
              ;; Subprogram (``[m]'').
              (expect-character #\[)
              (let ((subprogram (make-program)))
                (declare (type Program subprogram))
                (setf position
                  (parse-coeus code subprogram position))
                (setf output-register-value
                  (register-at subprogram output-register-name)))
              (expect-character #\])
              
              ;; Conditional main program section (``[n]'').
              (expect-character #\[)
              ;; Repeat the commands of ``n'' until the check register
              ;; and the output register match.
              (loop
                until (= (register-at program check-register-name)
                         output-register-value)
                do    (parse-coeus code program position))
              ;; Move to the matching closing bracket of ``[n]'' to skip
              ;; the commands of ``n''.
              (skip-code-section)
              (expect-character #\])))
          
          (#\]
            (loop-finish))
          
          (T
            (error "Invalid character ~s." character)))))
      (the fixnum position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple test expression during the development phase which accepts
;; a number as an input, increases it by one, and prints the new value.
(parse-coeus "hBdBaB")

;;; -------------------------------------------------------

;; Truth machine.
(parse-coeus "hA[AB][][aA]aA")

;; Factorial.
(parse-coeus "hB[BB][][bBeB]aA")

;; Infinite counter (starts from 0):
(parse-coeus "[BB][eB][aBdB]")

;;; -------------------------------------------------------

;; Prints the numbers 1, 2, and 3.
;; Note that it is not possible to introduce separating whitespaces
;; amidst these output digits.
(parse-coeus "[AA][dAdAdA][aAdA]")

;;; -------------------------------------------------------

;; One-time CAT program.
(parse-coeus "hAaA")

;;; -------------------------------------------------------

;; CAT program which ends when the user inputs '0'.
(parse-coeus "[AB][][hAaA]")
