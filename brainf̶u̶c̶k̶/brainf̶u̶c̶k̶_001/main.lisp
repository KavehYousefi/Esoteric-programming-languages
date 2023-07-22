;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainf̶u̶c̶k̶", invented by the Esolang user "D" and presented
;; in the year 2023, founded upon the jocular employment of a single
;; instruction, acquired visually from the provenance of its entheus,
;; Urban Mueller's "brainfuck", which terminates the program.
;; 
;; 
;; Concept
;; =======
;; The brainf̶u̶c̶k̶ programming language, also nevened "You can brain
;; without a fuck", constitutes a vehemently curtailed variety of the
;; far more competent brainfuck, reduced to an aefauld token from its
;; cleronomy's appropriation, "[" --- actually a conditional forward
;; jump instigator ---, which concomitantly experiences a
;; transmogrification into an immediate termination clause.
;; 
;; 
;; Architecture
;; ============
;; The desuetude of operations whose capacitation to perquire or
;; manipulate data would impose a requisitum in this bailiwick
;; effectively renders the inquisition into the language architecture an
;; exercise in mateotechny.
;; 
;; Natheless, stringency in derivation would assign the scion the
;; stock-father's, brainfuck's, construe of the respective castaldy's
;; objects, namely a theoretically infinite expanse of octet-valued
;; cells, bilaterally uncountable in their tally, and initialized to the
;; minimum of zero (0); their existence a cell pointer's target, that at
;; any instant selects the currently active, and thus accessible, unit.
;; 
;; 
;; Data Types
;; ==========
;; All statements' dation that constitute the architecture's (which
;; please see aboon in the eponymous section) treatise may be
;; appropriated, through the validating accommodations, to the
;; language's type hierarchy: Namely, no mandate concerns the lacking
;; competence.
;; 
;; Iterum, the brainfuck specification's adduction redes the parental
;; diorisms' explication: The provenance's data types bifurcate into
;; the paravant unsigned bytes, commorants of the range [0, 255], and
;; the paravail ASCII characters, the latter only experiences its
;; mustering for the input/output commerce.
;; 
;; 
;; Syntax
;; ======
;; All characters except for the jump-to-end token "[" are ignored.
;; 
;; == "[": THE ONLY PURVEYOR OF CAUSATUM ==
;; While every conceivable character's participation in a piece of
;; brainf̶u̶c̶k̶ code is admitted, only the left bracket, "[", endows a
;; program with effect, apportioning all other tokens with negligence,
;; or a no-operation (NOP) role.
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (EBNF) delineation shall be adhibited to
;; the syntaxis --- rather to assuage the pruritus for patration than
;; the desideratum for utile gnarity:
;; 
;;   program   := { jumpToEnd | comment } ;
;;   comment   := character - jumpToEnd ;
;;   jumpToEnd := "[" ;
;; 
;; 
;; Instructions
;; ============
;; brainf̶u̶c̶k̶'s instruction set attends to an aefauld member's
;; accommodation only, a conjunction of unconditional termination clause
;; and relocation imperative. Any other token's tolerance constitutes an
;; equipollent of its neglect.
;; 
;; == OVERVIEW ==
;; The singular operation, the relocation and termination specimen,
;; shall now be elucidated:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   [       | Relocates the instruction pointer (IP) to the end of the
;;           | program, effectively terminating execution.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The simple haecceity of the brainf̶u̶c̶k̶ programming language serves to
;; obviate any inroads of ambiguities.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation has been accomplished in the programming
;; language Common Lisp.
;; 
;; A dependency of advenient encheson, this interpreter's usance of
;; the programming language name "brainf̶u̶c̶k̶" encumbers the executing
;; Common Lisp with the imperative to accept Unicode characters. This
;; nominal requirement can be eradicated by substituting the main
;; operation ``interpret-brainf̶u̶c̶k̶'' by an agnomination content with
;; ASCII characters only, such as
;; "interpret-You-can-brain-without-a-fuck".
;; 
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-22
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023brainf̶u̶c̶k̶]
;;   The Esolang contributors, "brainf̶u̶c̶k̶", 2023
;;   URL: "https://esolangs.org/wiki/Brainf%CC%B6u%CC%B6c%CC%B6k%CC%B6"
;;   
;;   [esolang2023youcanbrainwaf]
;;   The Esolang contributors, "You can brain without a fuck", 2023
;;   URL: "https://esolangs.org/w/
;;         index.php?title=You_can_brain_without_a_fuck&redirect=no"
;;   Notes:
;;     - Redirection page to the official article "brainf̶u̶c̶k̶".
;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-brainf̶u̶c̶k̶ (code)
  "Interprets the piece of brainf̶u̶c̶k̶ source CODE and returns no value."
  (declare (type string code))
  (loop
    with  ip of-type fixnum = 0
    while (< ip (length code))
    do
      (if (char= (char code ip) #\[)
        (setf ip (length code))
        (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-brainf̶u̶c̶k̶ "[")

;;; -------------------------------------------------------

(interpret-brainf̶u̶c̶k̶ "[]")

;;; -------------------------------------------------------

(interpret-brainf̶u̶c̶k̶ "[[][[]][]]")

;;; -------------------------------------------------------

(interpret-brainf̶u̶c̶k̶ ">>>[>>]<<[>]<[>]")
