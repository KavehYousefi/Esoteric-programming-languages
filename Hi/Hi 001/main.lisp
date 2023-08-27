;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Hi", invented by the Esolang user "Yes" and presented on
;; March 16th, 2023, the aefauld competence of which wones in its
;; response to any single character in its code with a display of the
;; message "hi", desitute of sepiments or further parerga.
;; 
;; 
;; Concept
;; =======
;; The "Hi" programming language subscribes to the jocular specimens,
;; printing for each character it encounters simply the message "hi",
;; without any separator.
;; 
;; == INTERPRETERS MAY PROCESS OR COUNT ==
;; The dioristic indifference, which assigns no significance to the
;; token other than its presence, elicits a twain of ideations:
;; Imprimis, a traditional rendition shall be produced, that whose
;; adhibition of causata to every character traverses the code; a
;; parhedral implementation, concordant in the phenotype, yet easily
;; designed, participates.
;; 
;; Conformant with a canonical forbisen, one to attend to everichon
;; among the characters in sequence, the first variant, an adherent to
;; assiduity, ostends its principles:
;; 
;;   procedure interpretAssiduousHi (code)
;;     with
;;       code --- the Hi source code string to interpret.
;;     
;;     for each character in code do
;;       print "hi"
;;     end for
;;   end procedure
;; 
;; The Procrustean vista administered by the Hi language to its
;; program's constituents vindicates an alternative approach, the same
;; simply supputates the code length and prints the message "hi" a tally
;; of instances tantamount to this cardinality:
;; 
;;   procedure interpretCountingHi (code)
;;     with
;;       code --- the Hi source code string to interpret.
;;     
;;     let codeLength <- length(code)
;;     
;;     repeat codeLength do
;;       print "hi"
;;     end repeat
;;   end procedure
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-27
;; 
;; Sources:
;;   [esolang2023Hi]
;;   The Esolang contributors, "Hi", March 16th, 2023
;;   URL: "https://esolangs.org/wiki/Hi"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Hi-the-assiduous-way (code)
  "Interprets the piece of Hi source CODE by iterating over every
   character, printing for each instance the message \"hi\", and returns
   no value."
  (declare (type string code))
  (loop for token of-type character across code do
    (format T "hi"))
  (values))

;;; -------------------------------------------------------

(defun interpret-Hi-the-counting-way (code)
  "Interprets the piece of Hi source CODE by tallying its characters and
   printing a tantamount times the message \"hi\", and returns no
   value."
  (let ((number-of-characters (length code)))
    (declare (type fixnum number-of-characters))
    (loop repeat number-of-characters do
      (format T "hi")))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hi" 13 times.
(interpret-Hi-the-assiduous-way "Hello, World!")

;;; -------------------------------------------------------

;; Print "hi" 13 times.
(interpret-Hi-the-counting-way "Hello, World!")
