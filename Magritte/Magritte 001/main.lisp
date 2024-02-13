;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Magritte", invented by Marc Hamann around the year 2005
;; and presented by the Esolang user Ørjan Johansen, also known under
;; their user handle "Oerjan", on December 26th, 2006, the dioristic
;; element of which manifests in an aefauld permitted operations, its
;; name inspired by the work "La trahison des images" of the Belgian
;; artist Magritte, imposed as "Ceci n'est pas un programme.".
;; 
;; 
;; Concept
;; =======
;; The Magritte programming language dwells among a ludibund subspecies
;; of the esoteric realm, its operative recognition restricted to the
;; phrase "Ceci n'est pas un programme.", accompassing no particular
;; epiphenomenon, while never terminating its program.
;; 
;; == MAGRITTE: A LANGUAGE ELICITED FROM AN ARTISTIC ENTHEUS ==
;; The Magritte programming language references a seminal work begotten
;; by the Belgian artist René François Ghislain Magritte, norned
;; "La trahison des images", transcripted into the English tongue as
;; "The Treachery of Images", the same, presented in the year 1929, 
;; depicts a pipe, adorned with the caption "Ceci n'est pas une pipe.",
;; that is, "This is not a pipe.", ostending the subject maugre its
;; concomitant refutation of the eponymous object's presence.
;; 
;; == AN AEFAULD COMMAND: "Ceci n'est pas un programme." ==
;; The sole instruction embraced by the Magritte programming language's
;; capability constitutes the phrase "Ceci n'est pas un programme.",
;; stating "This is not a program.", while accompassing no tangible
;; product during its execution.
;; 
;; Programs in this language never terminate.
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-02-13
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2022Magritte]
;;   The Esolang contributors, "Magritte", March 19th, 2022
;;   URL: "https://esolangs.org/wiki/Magritte"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Magritte (code)
  "Interprets the piece of Magritte source CODE and returns no value.
   ---
   Upon encountering an invalid instruction an error of an unspecified
   type is signaled."
  (declare (type string code))
  (if (string= code "Ceci n'est pas un programme.")
    (loop)
    (error "Invalid program. ~
            Expected \"Ceci n'est pas un programme.\", ~
            but received ~s."
      code)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A correctly operating program.
(interpret-Magritte "Ceci n'est pas un programme.")

;;; -------------------------------------------------------

;; An incorrect program.
(interpret-Magritte "This IS a program!")
