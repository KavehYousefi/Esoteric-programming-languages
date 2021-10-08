
;; Date: 2021-10-08
;; 
;; This file implements an interpreter for the esoteric programming
;; language "DetailedFuck", invented by the Esolang user "Lebster".
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/DetailedFuck"
;;   -> "https://stackoverflow.com/questions/4366668/str-replace-in-common-lisp"
;;       o Implements a substring replacement function in Common Lisp.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type list *translations*))

;;; -------------------------------------------------------

(defparameter *translations*
  '(("MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT" . ">")
    ("MOVE THE MEMORY POINTER ONE CELL TO THE LEFT" . "<")
    ("INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE" . "+")
    ("DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE" . "-")
    ("REPLACE THE CELL UNDER THE MEMORY POINTER'S VALUE WITH THE ASCII CHARACTER CODE OF USER INPUT" . ",")
    ("PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER" . ".")
    ("IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK" . "[")
    ("IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK" . "]"))
  "An association list which maps each DeailedFuck command to the
   corresponding instruction in Brainfuck.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ancillary functions.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-command (source translation)
  "Replaces all occurrences in the SOURCE of the TRANSLATION cons' left
   part by the right part and returns a new string containing the
   translation result."
  (declare (type string               source))
  (declare (type (cons string string) translation))
  (destructuring-bind (detailedFuck-command . brainfuck-command)
      translation
    (declare (type string detailedFuck-command))
    (declare (type string brainfuck-command))
    (let ((detailedFuck-command-length (length detailedFuck-command))
          (source-length               (length source)))
      (declare (type fixnum detailedFuck-command-length))
      (declare (type fixnum source-length))
      (let ((previous-position             0)
            (detailedFuck-command-position 0))
        (declare (type fixnum           previous-position))
        (declare (type (or null fixnum) detailedFuck-command-position))
        (the string
          (with-output-to-string (translated-content)
            (declare (type string-stream translated-content))
            (loop do
              (setf detailedFuck-command-position
                (search detailedFuck-command source
                  :start2 previous-position
                  :test   #'string-equal))
              (write-string source translated-content
                :start previous-position
                :end   (or detailedFuck-command-position source-length))
              (cond
                (detailedFuck-command-position
                  (setf previous-position
                    (+ detailedFuck-command-position
                       detailedFuck-command-length))
                  (write-string brainfuck-command translated-content))
                (T
                  (loop-finish))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-detailedFuck-to-brainfuck (detailedFuck-code)
  "Translates the DETAILEDFUCK-CODE into a piece of brainfuck code
   and returns the resulting string."
  (declare (type string detailedFuck-code))
  (let ((brainfuck-code detailedFuck-code))
    (declare (type string brainfuck-code))
    (dolist (translation *translations*)
      (declare (type (cons string string) translation))
      (setf brainfuck-code (translate-command brainfuck-code translation)))
    (the string brainfuck-code)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *source*
  "INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
   INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE ] COMMAND IN BRAINFUCK
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   IF THE CELL UNDER THE MEMORY POINTER'S VALUE IS NOT ZERO INSTEAD OF READING THE NEXT COMMAND IN THE PROGRAM JUMP TO THE CORRESPONDING COMMAND EQUIVALENT TO THE [ COMMAND IN BRAINFUCK
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   INCREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE RIGHT
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER
   MOVE THE MEMORY POINTER ONE CELL TO THE LEFT
   DECREMENT THE CELL UNDER THE MEMORY POINTER BY ONE
   PRINT THE CELL UNDER THE MEMORY POINTER'S VALUE AS AN ASCII CHARACTER")

;;; -------------------------------------------------------

;; Prints "hello world".
(translate-detailedFuck-to-brainfuck *source*)
