;; Date: 2021-12-21
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/QWOP"
;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-QWOP (code)
  "Interprets the piece of QWOP CODE and returns no value.
   ---
   The implementation maintains an adjustable vector COPY-OF-CODE, upon
   processing of each character concomitantly storing the same, and
   printing its contents, in the case of a quine's desideration, to the
   standard output."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position        0)
          (character       (char code 0))
          (copy-of-code    (make-array 0
                             :element-type 'character
                             :adjustable   T
                             :fill-pointer 0))
          (quine-desired-p NIL))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type string              copy-of-code))
      (declare (type boolean             quine-desired-p))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character, if
             possible, updates the current CHARACTER, and returns no
             value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION cursor to the previous character, if
             possible, updates the current CHARACTER, and returns no
             value."
            (setf character
              (when (plusp position)
                (char code (decf position))))
            (values))
           
           (move-to (new-position)
            "Relocates the POSITION cursor to the NEW-POSITION,
             concomitantly updating the CHARACTER, and returning no
             value."
            (declare (type fixnum new-position))
            (setf position  new-position)
            (setf character (char code position))
            (values))
           
           (preceded-by (expected-character)
            "Checks whether the character preceding the current POSITION
             exists and equals the EXPECTED-CHARACTER, returning a
             ``boolean'' value of ``T'' on confirmation, and otherwise
             ``NIL''."
            (declare (type character expected-character))
            (and (plusp position)
                 (char= (char code (1- position))
                        expected-character)))
           
           (command-character-p (subject)
            "Checks whether the SUBJECT constitutes a command character
             name, returning a ``boolean'' value of ``T'' on
             confirmation, and ``NIL'' otherwise."
            (declare (type character subject))
            (not (null (find subject "QWOP" :test #'char=))))
           
           (find-next-command ()
            "Starting at the current POSITION, searches forward for the
             next command identifier, relocating the POSITION cursor to
             its stead."
            (loop
              until (or (null character)
                        (command-character-p character))
              do
                (memorize-current-character)
                (advance)))
            
           (memorize-current-character ()
            (when character
              (vector-push-extend character copy-of-code))
            (values)))
        
        (loop do
          (memorize-current-character)
          
          (cond
            ;; End of file.
            ((null character)
              (loop-finish))
            
            ;; Quine.
            ((char= character #\Q)
              (setf quine-desired-p T)
              (advance))
            
            ;; Query user input and check for match.
            ((char= character #\W)
              (advance)
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (cond
                  ((null character)
                    (error "End of file at position ~d while expecting ~
                            a test character for 'W'."
                      position))
                  ((char= character input)
                    (memorize-current-character)
                    (advance))
                  (T
                    (find-next-command)
                    ;; Skip character following "P" or "W", as these
                    ;; constitute their argument.
                    (when (find character "PW" :test #'char=)
                      (advance)
                      (memorize-current-character))
                    (advance)))))
            
            ;; Loop.
            ((char= character #\O)
              (let ((return-position position))
                (declare (type fixnum return-position))
                (recede)
                (loop do
                  (cond
                    ;; No previous "O" found?
                    ;; => Skip original "O".
                    ((null character)
                      (move-to return-position)
                      (advance)
                      (loop-finish))
                    ;; "O" is an argument to the command "P" or "W"?
                    ;; => "O" is no command but a literal.
                    ;;    => Skip.
                    ((and (char= character #\O)
                          (or (preceded-by #\P)
                              (preceded-by #\W)))
                      (recede)
                      (recede))
                    ;; "O" found?
                    ;; => Move past it.
                    ((char= character #\O)
                      (advance)
                      (loop-finish))
                    ;; Skip any other content.
                    (T
                      (recede))))))
            
            ;; Print character.
            ((char= character #\P)
              (advance)
              (memorize-current-character)
              (if character
                (write-char character)
                (error "Expected a character following the 'P' command,
                        but encountered the end of file at position ~d."
                  position))
              (advance))
            
            ;; Any non-command character is simply ignored.
            (T
              (memorize-current-character)
              (advance))))
        
        (when quine-desired-p
          (format T "~a" copy-of-code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-QWOP "PHPePlPlPoP PWPoPrPlPdP!")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-QWOP "OW1P1W1OW0P0")

;;; -------------------------------------------------------

;; Quine.
(interpret-QWOP "Q")

;;; -------------------------------------------------------

;; Quine involving the "Hello World!" program.
(interpret-QWOP "Q PHPePlPlPoP PWPoPrPlPdP!")
