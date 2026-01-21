;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "1 Bit, an eight byte", invented by the Esolang user
;; "Tommyaweosme" and presented on October 17th, 2025, the fons et origo
;; of its diorism such as to reclude the competences' adit to a twain
;; of binary digit printers only, their accompassing principle one
;; concocted to confound by adminicle of the bit-valued identifiers'
;; exact overthwart commission of the causatum.
;; 
;; 
;; Concept
;; =======
;; The "1 Bit, an eight byte" programming language constitutes a
;; specimen whose devotion may be related of in any mete of tenability
;; as one to mirth, rather than lucre, the compentences' entelechia a
;; twifold exercise to whom merely the printing of binary digits remains
;; vouchsafed.
;; 
;; == NO MEMORY, YET FUN ==
;; A consectary from the language's enker designment, no memory's
;; involvement does accompt for a requisite nor a parergon.
;; 
;; 
;; Syntax
;; ======
;; Eath in its elucidation as its conformation, a "1 Bit, an eight byte"
;; program's syntactical deliverance to one's conspectuity enhalses the
;; bit characters "0" and "1", with any other participation supputated a
;; fremd trial of attrectation, and as such an abortive response's
;; incursion.
;; 
;; == GRAMMAR ==
;; The language's donat shall be an Extended Backus-Naur Form's (EBNF)
;; cynosure:
;; 
;;   program := { "0" | "1" } ;
;; 
;; 
;; Instructions
;; ============
;; The "1 Bit, an eight byte" programming language's pernancy from the
;; vale of conceivable competences does not wist of a supererogation
;; ayond a twissel of printing operations, impounded even therein to
;; the issuance of binary digit.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be satisfied in a requisite mete
;; of nortelry's adhibition concerning the language's operative
;; facilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Prints the digit one (1) to the standard output conduit.
;;   ..................................................................
;;   1       | Prints the digit zero (0) to the standard output
;;           | conduit.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization, an emprise peracted in the
;; programming language Common Lisp, applies itself to a per saltum
;; evaluation of the "1 Bit, an eight byte" source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-21
;; 
;; Sources:
;;   [esolang2025:1 Bit, an eight byte]
;;   The Esolang contributors, "1 Bit, an eight byte",
;;     October 17th, 2025
;;   URL: "https://esolangs.org/wiki/1_Bit,_an_eight_byte"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-|1 Bit, an eight byte|-code (code)
  "Interprets the piece of \"1 Bit, an eight byte\" source CODE and
   returns no value."
  (declare (type string code))
  (dotimes (current-position (length code))
    (declare (type fixnum current-position))
    (let ((current-token (char code current-position)))
      (declare (type character current-token))
      (case current-token
        (#\0
          (format T "1"))
        (#\1
          (format T "0"))
        (otherwise
          (error "The symbol \"~c\", located at the position ~d, ~
                  does not designate a recognized command."
            current-token
            current-position)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate and print the nybble "1011".
(interpret-the-|1 Bit, an eight byte|-code "0100")
