;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the castaldy of the test cases, accompassing a
;; conjoined efficiency that, as a docimasy applied to the interpreter,
;; delivers an affidavit of its correct operation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
;; 
;; This program constitutes a transliteration from the tantamount
;; Boolfuck example
;;   ;;;+;+;;+;+;
;;   +;+;+;+;;+;;+;
;;   ;;+;;+;+;;+;
;;   ;;+;;+;+;;+;
;;   +;;;;+;+;;+;
;;   ;;+;;+;+;+;;
;;   ;;;;;+;+;;
;;   +;;;+;+;;;+;
;;   +;;;;+;+;;+;
;;   ;+;+;;+;;;+;
;;   ;;+;;+;+;;+;
;;   ;;+;+;;+;;+;
;;   +;+;;;;+;+;;
;;   ;+;+;+;
(interpret-Celum
  "
  0::ooo}
  1::o}
  0::oo}
  1::o}
  0::o}
  1::o}
  0::o}
  1::o}
  0::oo}
  1::oo}
  0::ooo}
  1::oo}
  0::o}
  1::oo}
  0::ooo}
  1::oo}
  0::o}
  1::oo}
  0::o}
  1::oooo}
  0::o}
  1::oo}
  0::ooo}
  1::oo}
  0::o}
  1::o}
  0::ooooooo}
  1::o}
  0::oo}
  1::ooo}
  0::o}
  1::ooo}
  0::o}
  1::oooo}
  0::o}
  1::oo}
  0::oo}
  1::o}
  0::oo}
  1::ooo}
  0::ooo}
  1::oo}
  0::o}
  1::oo}
  0::ooo}
  1::o}
  0::oo}
  1::oo}
  0::o}
  1::o}
  0::oooo}
  1::o}
  0::ooo}
  1::o}
  0::o}
  1::o
  ")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Celum
  "0::ioioioioioioioio")

;;; -------------------------------------------------------

;; Infinitely repeating bit-based cat program.
(interpret-Celum
  "0::io!")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Celum
  "0::i}
   
   C:As the center bit is flipped, an input of 0 generates a 1,
   C:and vice versa. In such a case, go to the line labeled
   C:as 'printZero'.
   1::!printZero
   
   C:As the center bit is flipped, an input of 1 generates a 0,
   C:and vice versa. In such a case, print the input bits, while
   C:concomitantly shifting the 8-bit section to the right and storing
   C:the bits in the correct order on the tape, ...
   0::}
   1:printOneInput:oF0ioF0ioF0ioF0ioF0ioF0ioF0ioF0!shiftBack
   C:... then shift the eight character bits back with the least
   C:significant position in the tape center, ...
   1:shiftBack:AAAAAAAAAAAAAAAA
   C:... and repeat the printing process --- this time eschewing the
   C:input buffer's employment. Finally, repeat with the back-shifting
   C:step above.
   1::oF0oF0oF0oF0oF0oF0oF0oF0!shiftBack
   
   0:printZero:0030oioioioioioioio")

;;; -------------------------------------------------------

;; Label-based infinite loop.
(interpret-Celum
  "0:jumpBack:!jumpForward
   1:jumpForward:!jumpBack")

;;; -------------------------------------------------------

;; Obviate an infinite loop by employing the bit flag in conjunction
;; with the omission command "?", the same, in this concrete case,
;; skips the desinent line's "!start" label jump instruction, and, as a
;; corollary, the program terminates from its exhaustion.
(interpret-Celum
  "0:start:!flagOn
   1:flagOn:!flagOff
   0:flagOff:?!start")
