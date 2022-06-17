;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the joke language "OHE",
;; invented by the Esolang user "ResU".
;; 
;; Concept
;; =======
;; Any program authored in the OHE programming language partakes of a
;; Procrustean treatment concerning its code by the same's neglect,
;; instead merely printing the text "Hello, world!".
;; 
;; 
;; Architecture
;; ============
;; OHE neither prescribes nor requires any architectural warklooms.
;; 
;; 
;; Data Types
;; ==========
;; The insignificance apportioned to its source code perforce excludes
;; the participation of data types. Solely the ASCII character set, as a
;; supplying substrate for the requisite text "Hello, world!" may be
;; adduced in this agency. Ensuing from this premise, characters, and
;; their compositions in the form of strings, account for the recognized
;; data types.
;; 
;; 
;; Syntax
;; ======
;; The insignificance and ineffectuality of its source code ascertains
;; any character's admission into a program.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) avails in the
;; language's syntactical explication:
;;   
;;   program   := { character } ;
;;   character := "a" | ... | "z" | "A" | ... | "Z"
;;             |  "0" | "1" | "2" | "3" | "4"
;;             |  "5" | "6" | "7" | "8" | "9"
;;             |  ...
;;             ;
;; 
;; 
;; Instructions
;; ============
;; The language does not administer any construe to the notion of
;; instructions; its perfect tolerance regarding input in all forms
;; compatible to one's apprehension is meted against an equipollence in
;; the matter of its neglect.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/OHE"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-OHE (code)
  "Interprets the piece of OHE CODE and returns no value."
  (declare (type string code))
  (declare (ignore      code))
  (format T "~&Hello, world!")
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-OHE "")

;;; -------------------------------------------------------

(interpret-OHE "input")
