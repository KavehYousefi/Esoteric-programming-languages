;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the furnishment of the ``Parser'' class, an
;; entity entrusted with the assemblage of an executable Celum program
;; from a piece of source code in string form, naited in this
;; enterprise by the lexer's efforts.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-lines (code)
  "Extracts from the piece of Celum source CODE the instruction lines,
   eliding the comment specimens, and returns a list comprehending these
   discoveries."
  (declare (type string code))
  (let ((lines NIL)
        (lexer (make-lexer)))
    (declare (type (list-of Line) lines))
    (declare (type Lexer          lexer))
    (with-input-from-string (input code)
      (declare (type string-stream input))
      (loop
        for input-line
          of-type (or null string)
          =       (read-line input NIL NIL)
        while input-line
        do
          (lexer-set-line    lexer input-line)
          (lexer-skip-spaces lexer)
          
          (unless (lexer-finished-p lexer)
            (let ((prefix     NIL)
                  (label-name "")
                  (commands   NIL))
              (declare (type (or null bit) prefix))
              (declare (type string        label-name))
              (declare (type command-list  commands))
              
              ;; Either a bit (0 or 1), or ``NIL'' for a comment line.
              (setf prefix (lexer-read-prefix lexer))
              
              ;; No comment line?
              (when prefix
                ;; Read label and succeeding colon (":").
                (lexer-skip-spaces lexer)
                (setf label-name   (lexer-read-label-name lexer))
                (lexer-eat-colon   lexer)
                
                ;; Read command list.
                (lexer-skip-spaces lexer)
                (setf commands     (lexer-read-commands lexer))
                
                ;; Expect no content following the commands.
                (lexer-skip-spaces        lexer)
                (lexer-expect-end-of-line lexer)
                
                ;; Store the newly created ``Line'' object.
                (push
                  (make-line prefix label-name commands)
                  lines))))))
    
    (the (list-of Line)
      (nreverse lines))))

;;; -------------------------------------------------------

(defun connect-lines (lines)
  "Connects the elements stored in the LINES list by defining their
   predecessor and successor slots, and returns the modified LINES."
  (declare (type (list-of Line) lines))
  (the (list-of Line)
    (loop
      for previous-line
        of-type (or null Line)
        =       NIL
        then    current-line
      for current-line
        of-type Line
        in      lines
      do
        (setf (line-predecessor current-line) previous-line)
        (when previous-line
          (setf (line-successor previous-line) current-line))
      finally
        (return lines))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Extracts from the piece of Celum source CODE a sequence of its
   command lines and returns these ensconced in a ``Program'' instance."
  (declare (type string code))
  (the Program
    (make-program
      (connect-lines
        (extract-lines code)))))
