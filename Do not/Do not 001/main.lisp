;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Do not", invented by the Esolang user "BestCoder" and
;; presented on October 13th, 2025, its kenspeckle delineation that of
;; all competences' curtailment to a single causatum: the issuance of
;; the message "Do NOT: " as a prevenience to the program source code's
;; ipsissima verba replication.
;; 
;; 
;; Concept
;; =======
;; The "Do not" programming language constitutes a ludibund subspecies
;; of the esoteric realm, the aefauld competence's adeption wones in
;; the issuance of the message "Do NOT: ", with an affixion of the
;; verbatim source code.
;; 
;; == "Do not": A LANGUAGE OF INTERDICTION ==
;; A single effect's actuation already accomplishes the "Do not"
;; programming language's entelechy: The issuance of the fixed text
;; "Do not: ", succeeded by the source code in its ipsissima verba
;; replication.
;; 
;; In a diction whose notion is begotten by a cambistry's application
;; on compendiousness for enhaused formality, the following diorism
;; appertains to the language's operation:
;; 
;; Given a program "p", the "Do not" interpreter issues the message
;; 
;;   Do NOT: {p}
;; 
;; to the standard output conduit.
;; 
;; If, as a forbisen adduced, the source code states
;; 
;;   eat
;; 
;; the causatum accompassed by its execution amounts to the text:
;; 
;;   Do NOT: eat
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization has been accomplished naiting the
;; Common Lisp programming language, beholden to the "Do not" language's
;; eath conception, operating with a mode of immediacy on the source
;; code string itself.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-28
;; 
;; Sources:
;;   [esolang2025:Do not]
;;   The Esolang contributors, "Do not", October 13th, 2025
;;   URL: "https://esolangs.org/wiki/Do_not"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Do-not (code)
  "Interprets the piece of Do not source CODE and returns no value."
  (declare (type string code))
  (format T "~&Do NOT: ~a" code)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simply print:
;; 
;;   Do NOT: 
(interpret-Do-not "")

;;; -------------------------------------------------------

;; Interdict the program's consumption by printing the message:
;; 
;;   Do NOT: eat
(interpret-Do-not "eat")

;;; -------------------------------------------------------

;; Quine.
(interpret-Do-not "Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:
Do NOT:")
