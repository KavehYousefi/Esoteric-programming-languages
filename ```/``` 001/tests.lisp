;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the test cases, their telos a twifold facette:
;; imprimis, the endeictic and probative entelechy of this interpreter
;; project in regard to its compliance with the ``` language's
;; stipulations; subsequently, but not merely paraveil in its rank, the
;; furnishment of forbisens concerning the language's designment and
;; competences.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
;; 
;; Concept:
;;   Set cells[3] = 1 (set I/O mode to "input").
;;   Set cells[2] = 1 (activate I/O capabilities).
;;   Set cells[3] = 0 (set I/O mode to "output").
;;   Set cells[2] = 1 (activate I/O capabilities).
(|interpret-```|
  "`3`#1
   `2`#1
   `3`#0
   `2`#2")

;;; -------------------------------------------------------

;; Repeating cat program.
;; 
;; Concept:
;;   Set cells[3] = 1 (set I/O mode to "input").
;;   Set cells[2] = 1 (activate I/O capabilities).
;;   Set cells[3] = 0 (set I/O mode to "output").
;;   Set cells[2] = 1 (activate I/O capabilities).
;;   Set cells[0] = 0 (redirect instruction pointer to inchoation).
(|interpret-```|
  "`3`#1
   `2`#1
   `3`#0
   `2`#2
   `0`#0")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Concept:
;;   Set cells[3] = 1 (set I/O mode to "input").
;;   Set cells[2] = 1 (activate I/O capabilities).
;;   Set cells[3] = 0 (set I/O mode to "output").
;;   Set cells[2] = 1 (activate I/O capabilities).
;;   Set cells[1] = lowest bit of user input (0 for "0", 1 for "1";
;;                  enables/disables next command).
;;   Set cells[0] = beyond end of program (executed if input = "0").
;;   Set cells[1] = 0 (disable conditional execution;
;;                     executed if input = "1").
;;   Set cells[0] = 3 (return to printing input;
;;                     executed if input = "1").
(|interpret-```|
  "`3`#1
   `2`#1
   `3`#0
   `2`#2
   `1`24
   `0`#8
   `1`#0
   `0`#3")

;;; -------------------------------------------------------

;; Demonstrates the effect of indirection in the memory destination by
;; skipping an input actuation tmema.
;; 
;; Concept:
;;   Set cells[25]        = 0 (store instruction pointer address in
;;                             cell #25)
;;   Set cells[cells[25]] = 4 (skip following instructions for input
;;                             reception)
;;   Set cells[3]         = 1 (set I/O mode to "input").
;;   Set cells[2]         = 1 (activate I/O capabilities).
(|interpret-```|
  "`25`#0
   ``25`#4
   `3`#1
   `2`#1")
