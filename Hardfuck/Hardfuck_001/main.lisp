

(defun interpret-hardfuck (code)
  "Interprets the Hardfuck CODE and returns ``NIL''."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum position) (type (or null character) character))
      (flet ((to-next-character ()
               (if (< position (1- (length code)))
                 (setf character (char code (incf position)))
                 (setf character NIL)))
             (to-previous-character ()
               (if (plusp position)
                 (setf character (char code (decf position)))
                 (setf character NIL))))
        (let ((memory  (make-hash-table :test #'eql))
              (pointer 0))
          (declare (type hash-table memory) (type integer pointer))
          (loop do
            (case character
              ((NIL)
                (loop-finish))
              (#\>
                (incf pointer)
                (to-next-character))
              (#\<
                (decf pointer)
                (to-next-character))
              (#\+
                (incf (gethash pointer memory 0))
                (to-next-character))
              (#\-
                (decf (gethash pointer memory 0))
                (to-next-character))
              (#\.
                (format T "~&Please enter a character: ")
                (let ((input (read-char)))
                  (declare (type character input))
                  (write-char input)
                  (setf (gethash pointer memory) (char-code input)))
                (to-next-character))
              (#\,
                (write-char (code-char (gethash (1- pointer) memory 0)))
                (to-next-character))
              (#\[
                (cond
                  ((zerop (gethash (1- pointer) memory 0))
                    (to-next-character)
                    (loop with level of-type fixnum = 0 do
                      (case character
                        ((NIL)
                          (error "'[' not followed by ']'."))
                        (#\[
                          (incf level))
                        (#\]
                          (cond
                            ((zerop level)
                              (loop-finish))
                            (T
                              (decf level)
                              (to-next-character))))
                        (otherwise
                          (to-next-character)))))
                  (T
                    (to-next-character))))
              (#\]
                (cond
                  ((zerop (gethash (1+ pointer) memory 0))
                    (to-next-character))
                  (T
                    (to-previous-character)
                    (loop with level of-type fixnum = 0 do
                      (case character
                        ((NIL)
                          (error "']' not preceded by '['."))
                        (#\]
                          (incf level))
                        (#\[
                          (cond
                            ((zerop level)
                              (loop-finish))
                            (T
                              (decf level)
                              (to-previous-character))))
                        (otherwise
                          (to-previous-character)))))))
              (#\@
                (setf (gethash (1- pointer) memory 0) (* pointer 4))
                (to-next-character))
              (#\/
                (setf pointer 0)
                (to-next-character))
              (otherwise
                (to-next-character)))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "e".
(interpret-hardfuck
  "
   sets the cell to 18
   >>>>>>>>>>>>>>>>>>
   multiplies 18 by 4 which is equal to 72 and stores it in cell 17
   @
   goes to cell 50
   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   multiplies 50 by 4 which is equal to 200 and stores it in cell 49
   @
   we go to cell 49
   <
   and then do 99 negatives
   ---------------------------------------------------------------------------------------------------
   now cell 49 has the e and we go to cell 50 before printing cell 49
   >
   ,
  ")

;;; -------------------------------------------------------

;; Print "Hello World".
(interpret-hardfuck
  "
 sets the cell to 18
 >>>>>>>>>>>>>>>>>>
 multiplies 18 by 4 which is equal to 72 and stores it in cell 17
 @
 goes to cell 50
 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 multiplies 50 by 4 which is equal to 200 and stores it in cell 49
 @
 we go to cell 49
 <
 and then do 99 negatives
 ---------------------------------------------------------------------------------------------------
 now cell 49 has the e we can go to cell 51
 >>
 now 50 is 204
 @
 we go back
 <
 and then
 ------------------------------------------------------------------------------------------------
 we have our first l
 >
 I'm being lazy
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 second l
 >
 o
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 now for the space
 >
 ++++++++++++++++++++++++++++++++
 now to do the W
 >
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 and the o
 >
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 and the r
 >
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 l
 >
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 d
 >
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 now to print it out
 />>>>>>>>>>>>>>>>>>,>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>,>,>,>,>,>,>,>,>,>,
 ")

;;; -------------------------------------------------------

;; Minified "Hello World".
(interpret-hardfuck
  ">>>>>>>>>>>>>>>>>>@>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>@<--------------------------------------------------------------------------------------------------->>@<------------------------------------------------------------------------------------------------>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++/>>>>>>>>>>>>>>>>>>,>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>,>,>,>,>,>,>,>,>,>,")

;;; -------------------------------------------------------

;; A simple loop test with three cells, labeled as suc:
;;   a = memory[0]
;;   b = memory[1]
;;   c = memory[2]
(interpret-hardfuck
  "
  a = 1
  +
  
  b = 0
  >
  
  test: a = 0?
  [
    ,
    Go to a
    <
    a = 0
    -
    Go to b
    >
  test: c != 0?
  ]
  
  ,
  ")

;;; -------------------------------------------------------

;; Prompts the user for a character and prints the cell containing it.
(interpret-hardfuck ".>,")
