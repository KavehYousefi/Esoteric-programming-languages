;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the furnishment of the test cases, such
;; subscribe to a twifaced utility; imprimis, the demonstration of the
;; NeverGonna programming language's traits, and secondly, in a
;; parhedral assessment, the apodictic telos that adminiculates the
;; interpreter implementation's correctness and its concinnity with the
;; language standard.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World".
(interpret-NeverGonna "i just wanna tell you \"Hello World\"")

;;; -------------------------------------------------------

;; Execute the "FizzBuzz" program with a counter traversing from
;; inclusive zero (0) to inclusive 99..
(interpret-NeverGonna
  "
  we're no strangers to i
  gotta make i 0
  we've known i for 100
        inside we both know i % 15 == 0 then
              i just wanna tell you \"FizzBuzz\"
        never gonna turn around i % 3 == 0 then
              i just wanna tell you \"Fizz\"
        never gonna turn around i % 5 == 0 then
              i just wanna tell you \"Buzz\"
        never gonna let you down
              i just wanna tell you i
        never gonna give you up
  never gonna give you up
  ")

;;; -------------------------------------------------------

;; Truth-machine utilizing merely the "while" loop.
(interpret-NeverGonna
  "
  we're no strangers to yourInput
  gotta make yourInput your heart's been aching but you're too shy to say 'Please input 0 or 1'
  
  i just wanna tell you yourInput
  
  a full commitment's what I'm thinking of yourInput == 1
    i just wanna tell you yourInput
  never gonna give you up
  ")

;;; -------------------------------------------------------

;; This program demonstrates an interpreter's competences in segregating
;; two hyphens or minus signs ("--") in immediately succession in the
;; context of expression evaluations and veridical comments.
(interpret-NeverGonna
  "
  we're no strangers to i--this is a comment
  gotta make i 5--2
  i just wanna tell you i
  ")

;;; -------------------------------------------------------

;; This program demonstrates an interpreter's competences to correctly
;; recognize a comment's occupancy of a blank line.
(interpret-NeverGonna
  "
  we're no strangers to i
  gotta make i 10
  
  -- Here a comment kithes itself.
  
  i just wanna tell you i
  ")

;;; -------------------------------------------------------

;; Execute a looping counter, the same iterates from inclusive one (1)
;; to inclusive 100, concomitantly printing during each cycle a line
;; composed of a tally of asterisks ("*") tantamount to the counter
;; variable's contemporaneous value.
(interpret-NeverGonna
  "we're no strangers to counter
   gotta make counter 1
   
   we've known counter for 100
     i just wanna tell you '*' * counter
   never gonna give you up")

;;; -------------------------------------------------------

;; Execute a looping counter, the same iterates from inclusive one (1)
;; to an inclusive number of times obtained by querying the user,
;; concomitantly printing during each cycle a line composed of a tally
;; of asterisks ("*") tantamount to the counter variable's
;; contemporaneous value.
(interpret-NeverGonna
  "we're no strangers to counter
   gotta make counter 1
   
   we've known counter for your heart's been aching but you're too shy to say 'How many times shall we repeat?'
     i just wanna tell you '*' * counter
   never gonna give you up")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-NeverGonna
  "a full commitment's what I'm thinking of True
     i just wanna tell you your heart's been aching but you're too shy to say 'Please input a character:'
   never gonna give you up")

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-NeverGonna
  "we're no strangers to number_of_bottles
   gotta make number_of_bottles 99
   
   a full commitment's what I'm thinking of number_of_bottles > 0
     
     inside we both know number_of_bottles > 2 then
       i just wanna tell you number_of_bottles + ' bottles of beer on the wall,'
       i just wanna tell you number_of_bottles + ' bottles beer.'
       i just wanna tell you 'Take one down, pass it around,'
       i just wanna tell you number_of_bottles - 1 + ' bottles beer on the wall.'
       i just wanna tell you ''
     
     never gonna turn around number_of_bottles == 2 then
       i just wanna tell you number_of_bottles + ' bottles of beer on the wall,'
       i just wanna tell you number_of_bottles + ' bottles beer.'
       i just wanna tell you 'Take one down, pass it around,'
       i just wanna tell you number_of_bottles - 1 + ' bottle beer on the wall.'
       i just wanna tell you ''
     
     never gonna let you down
       i just wanna tell you number_of_bottles + ' bottle of beer on the wall,'
       i just wanna tell you number_of_bottles + ' bottle beer.'
       i just wanna tell you 'Take one down, pass it around,'
       i just wanna tell you 'No bottles of beer on the wall.'
       i just wanna tell you ''
     never gonna give you up
     
     gotta make number_of_bottles number_of_bottles - 1
     
   never gonna give you up")
