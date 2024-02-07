;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file provides the test cases against which the interpreter's
;; conformance to the Tuvars language standard may be meted.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program which queries for a character and
;; outputs its decimal ASCII code.
(interpret-Tuvars
  "read a     // Set a to ASCII value of input
   clear      // Clear the console
   number a   // Print a's value
   linebreak  // Print a linebreak
   goto 1     // Restart")

;;; -------------------------------------------------------

;; Print the message "Hello, World!".
(interpret-Tuvars "print Hello, World!")

;;; -------------------------------------------------------

;; Modify the program console's title to "My program".
(interpret-Tuvars "title My program")

;;; -------------------------------------------------------

;; Modify the program console's title to "My program" and subsequently
;; close the console, thus rendering it invisible.
(interpret-Tuvars
  "title My program
   close")
