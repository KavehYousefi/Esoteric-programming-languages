;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Bgt", invented by the Esolang user "Cinnamony" and
;; presented on June 15th, 2023, the notion of which wone in the
;; realization of input, output, and several popular applications from
;; the computer science bailiwick.
;; 
;; 
;; Concept
;; =======
;; The Bgt programming language is presumably founded upon the endavor
;; to produce a joke language in the least amount of time achievable,
;; that is, in a "speed run". Proceeding therefrom, a specimen has been
;; begotten that realizes input, output, and the execution of a set of
;; popular programming tasks.
;; 
;; 
;; Instructions
;; ============
;; A quintuple of elements suffices for the Bgt language operations'
;; exhaustion, the coherence of the same, besides the vinculum of input
;; and output, ostends a restriction to popular tasks that probe a
;; programming language's capabilities.
;; 
;; == OVERVIEW ==
;; The following apercu applies itself to a cursory nortelry's
;; adhibition regarding the Bgt language's available operations.
;; 
;; Please heed that placeholder segments are underlined with asterisks
;; ("*") and intended to be substituted by actual code.
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   Pcharacter | Prints the {character} immediately succeeding the
;;    ********* | "p" command identifier to the standard output.
;;              | If no character exists, which case is established in
;;              | a solitary "p" token constituting the desinent entity
;;              | of the code, an error of an unspecified type is
;;              | signaled.
;;   ..................................................................
;;   K          | Queries the user for a sequence of zero or more
;;              | characters and prints the same verbatim to the
;;              | standard output.
;;              | This command effectively provides a one-time cat
;;              | program.
;;   ..................................................................
;;   L          | Prints the lyrics of the song "99 Bottles of Beer" to
;;              | the standard output.
;;   ..................................................................
;;   J          | Prints the FizzBuzz program, employing a counter from
;;              | inclusive one (1) to inclusive 100, to the standard
;;              | output.
;;   ..................................................................
;;   Z          | Executes a truth-machine.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-17
;; 
;; Sources:
;;   [esolang2023Bgt]
;;   The Esolang contributors, "Bgt", June 15th, 2023
;;   URL: "https://esolangs.org/wiki/Bgt"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adminicular operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-input ()
  "Queries the standard input for a line of text and prints the same
   ipsissima verba to the standard output, omitting the concluding
   newline character."
  (format T "~&Please input a line of text: ")
  (let ((input (read-line *standard-input* NIL "")))
    (declare (type string input))
    (clear-input *standard-input*)
    (format T "~a" input))
  (values))

;;; -------------------------------------------------------

(defun print-99-bottles-of-beer ()
  "Prints the lyrics of the song \"99 Bottles of Beer\" to the standard
   output and returns no value."
  (loop
    for number-of-bottles of-type (integer -1 99) from 99 downto 0
    do
      (format T "~&~d ~:*bottle~p of beer on the wall,~%~
                 ~:*~d ~:*bottle~p of beer.~%~
                 Take one down, pass it around,~%~
                 ~[No~:;~d~] ~:*bottle~p of beer on the wall.~2%"
        number-of-bottles
        (decf number-of-bottles)
        number-of-bottles))
  (values))

;;; -------------------------------------------------------

(defun print-FizzBuzz ()
  "Executes the FizzBuzz program with a counter traversing the closed
   interval [1, 100], prints the messages to the standard output, and
   returns no value."
  (flet ((aliquot-p (dividend divisor)
          "Determines whether the DIVISOR constitutes an aliquot of the
           DIVIDEND, that is, a number which divides the latter without
           a remainder, returning on confirmation a ``boolean'' value of
           ``T'', otherwise ``NIL''."
          (declare (type (integer 1 101) dividend))
          (declare (type (integer 3  15) divisor))
          (the boolean
            (not (null
              (zerop (mod dividend divisor)))))))
    (loop for counter of-type (integer 1 101) from 1 to 100 do
      (format T "~&~a"
        (cond
          ((aliquot-p counter 15) "FizzBuzz")
          ((aliquot-p counter  3) "Fizz")
          ((aliquot-p counter  5) "Buzz")
          (T                       counter)))))
  (values))

;;; -------------------------------------------------------

(defun execute-truth-machine ()
  "Executes a truth-machine which prints its messages to the standard
   output and returns no value."
  (format T "~&Please input a bit: ")
  (let ((input (parse-integer (read-line *standard-input*))))
    (declare (type integer input))
    (clear-input)
    (format T "~%")
    (if (zerop input)
      (format T "0")
      (loop do (format T "1"))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Bgt (code)
  "Interprets the piece of Bgt source CODE and returns no value."
  (declare (type string code))
  (let ((position 0))
    (declare (type fixnum position))
    (symbol-macrolet
        ((current-token
          (the character
            (char code position)))
         (end-of-file-p
          (the boolean
            (not (array-in-bounds-p code position)))))
      
      (loop until end-of-file-p do
        (case current-token
          ;; Print the next character in the CODE.
          (#\P
            (incf position)
            (cond
              (end-of-file-p
                (error "Expected a character to print, but exhausted ~
                        the program at position ~d."
                  position))
              (T
                (format T "~c" current-token)
                (incf position))))
          
          ;; Print input (one-time cat program).
          (#\K
            (print-input)
            (incf position))
          
          ;; 99 Bottles of Beer program.
          (#\L
            (print-99-bottles-of-beer)
            (incf position))
          
          ;; FizzBuzz.
          (#\J
            (print-FizzBuzz)
            (incf position))
          
          ;; Truth-machine.
          (#\Z
            (execute-truth-machine)
            (incf position))
          
          (otherwise
            (error "Invalid character \"~c\" at position ~d."
              current-token position))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Bgt "PHPePlPlPoP,P PWPoPrPlPdP!")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Bgt "K")

;;; -------------------------------------------------------

;; 99 Bottles of Beer program.
(interpret-Bgt "L")

;;; -------------------------------------------------------

;; FizzBuzz program.
(interpret-Bgt "J")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Bgt "Z")
