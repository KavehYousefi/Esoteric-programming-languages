;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "FH", invented by the Esolang user "PrySigneToFry" and
;; presented on October 7th, 2024, its designment expressive of a
;; nonpareil of a mateotechny in its program's perfect neglect.
;; 
;; 
;; Concept
;; =======
;; The FH programming language engages in an adherence to the ludibund
;; species of joke languages, its existency entirely deprived of any
;; competences, ignoring any characters in its source code.
;; 
;; This appliance of neglect renders any program as geason in its
;; afferent data as in its epiphenomenal entelechy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-12-30
;; 
;; Sources:
;;   [esolang2024FH]
;;   The Esolang contributors, "FH", October 7th, 2024
;;   URL: "https://esolangs.org/wiki/FH"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-FH (code)
  "Interprets the piece of FH source CODE and returns no value."
  (declare (type string code))
  (declare (ignore      code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Do nothing".
(interpret-FH "p")

;;; -------------------------------------------------------

;; "Do nothing 2".
(interpret-FH "iiiio")

;;; -------------------------------------------------------

;; Quine.
(interpret-FH "")
