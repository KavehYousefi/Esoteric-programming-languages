;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Your", presented by the Esolang user "Hex96" in the year
;; 2019, and dioristic in the reliance of the "Your" token's successive
;; occurrences in a preponderance of instructions as a discriminator.
;; 
;; 
;; Instructions
;; ============
;; The Your programming language instruction set's operational syntax
;; bifurcates into two variants:
;; 
;;   (a) Commands represented by a tally of subsequent "Your" tokens.
;;   (b) The unique command "You’re".
;; 
;; == OVERVIEW ==
;; An apercu shall attend to a cursory intelligence of the instructions.
;; 
;;   ------------------------------------------------------------------
;;   Command                       | Effect
;;   ------------------------------+-----------------------------------
;;   Your                          | Prints the text "Hello World".
;;   ..................................................................
;;   Your Your                     | Executes a cat program.
;;   ..................................................................
;;   Your Your Your                | Executes an "R cat" program.
;;   ..................................................................
;;   Your Your Your Your           | Executes the truth-machine
;;                                 | program.
;;   ..................................................................
;;   Your Your Your Your Your      | Prints the lyrics of the song
;;                                 | "99 bottles of beer on the wall".
;;   ..................................................................
;;   Your Your Your Your Your Your | Starts another instance of the
;;                                 | Your interpreter.
;;   ..................................................................
;;   You’re                        | Prints the message
;;                                 | "SCREAMS AT YOU, THIS IS THE INTERNET!!".
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-07
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Your"
;;   -> "https://esolangs.org/wiki/99_bottles_of_beer"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER constitutes a whitespace character,
   returning a ``boolean'' result of ``T'' on confirmation and ``NIL''
   on failure."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun interpret-Your (code)
  "Interprets the piece of Your CODE and returns no value."
  (declare (type string-stream code))
  
  (loop
    for   line of-type (or null string) = (read-line code NIL)
    while line
    do
    (let ((command (remove-if #'whitespace-character-p line)))
      (declare (type string command))
      
      (cond
        ((string= command "")
          NIL)
        
        ;; Hello World.
        ((string= command "Your")
          (format T "~&~a" "Hello World"))
        
        ;; Cat.
        ((string= command "YourYour")
          (loop do
            (format T "~&Please input a character: ")
            (let ((input (read-char)))
              (declare (type character input))
              (clear-input)
              (write-char input))))
        
        ;; R cat.
        ((string= command "YourYourYour")
          (format T "~&Please input some text and terminate with an ~
                       empty line:~%")
          (loop
            for     input of-type string = (read-line NIL)
            while   (plusp (length input))
            collect input
            into    contents
            finally (dolist (line (nreverse contents))
                      (declare (type string line))
                      (format T "~&~a" (reverse line)))))
        
        ;; Truth Machine.
        ((string= command "YourYourYourYour")
          (format T "~&Please input a number: ")
          (let ((input (parse-integer (read-line))))
            (declare (type integer input))
            (clear-input)
            (if (zerop input)
              (format T "~d" input)
              (loop do
                (format T "~d" input)))))
        
        ;; 99 bottles of beer on the wall
        ((string= command "YourYourYourYourYour")
          (loop for bottle-count of-type fixnum from 99 downto 1 do
            (format T "~&~d bottle~:*~p of beer on the wall,"
              bottle-count)
            (format T "~&~d bottle~:*~p of beer." bottle-count)
            (format T "~&Take one down, pass it around,")
            (if (> bottle-count 1)
              (format T "~&~d bottle~:*~p of beer on the wall."
                (1- bottle-count))
              (format T "~&No bottles of beer on the wall."))
            (format T "~2%"))
          (format T "~&No bottles of beer on the wall,")
          (format T "~&No bottles of beer.")
          (format T "~&Go to the store, buy some more,")
          (format T "~&99 bottles of beer on the wall."))
        
        ;; Your interpreter.
        ((string= command "YourYourYourYourYourYour")
          (loop do
            (with-input-from-string (code-line (read-line))
              (declare (type string-stream code-line))
              (interpret-Your code-line))
            (clear-input)
            (terpri)))
        
        ;; Screamer.
        ((string= command "You’re")
          (format T "~&SCREAMS AT YOU, THIS IS THE INTERNET!!")
          (loop-finish))
        
        (T
          (error "Invalid command: ~s." line)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Your-from-string (code)
  "Interprets the piece of Your CODE supplied as a string and returns
   no value."
  (declare (type string code))
  (with-input-from-string (code-stream code)
    (declare (type string-stream code-stream))
    (interpret-Your code-stream))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Your-from-string "Your")
