;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Disan", invented by the Esolang user "A" and presented in
;; the year 2018, the purpose of which is assigned to the aefauld wike
;; of executing the "Disan Count" program, a concept dedicated to the
;; testing of simple programming languages' features, in particular with
;; regard to potential hints towards their Turing-completeness.
;; 
;; 
;; Concept
;; =======
;; The Disan programming language dedicates its competence to a singular
;; telos: the execution of the Disan Count program, an implement for
;; probing certain language features.
;; 
;; == DISAN: DISAN COUNT IN REALITY ==
;; The language being norned "Disan" constitutes a derivation from its
;; entheus and sole purpose, the *Disan* Count program's realization.
;; 
;; == DISAN COUNT: A PROGRAMMING LANGUAGE CAPABILITIES TEST ==
;; The Disan Count offers a program whose capacitation entails a
;; programming language's assessment concerning certain key features,
;; par example, iteration, conditionals, input/output, and simple
;; arithmetics.
;; 
;; == DISAN COUNT: AN IMPUTED WARKLOOM FOR TURING-COMPLETENESS ==
;; The Disan Count concept's provenance records the late portion of the
;; year 2016, under the efforts of Matías Di Santo, the intentions of
;; whom were airted at providing a test for modestly endowed programming
;; languages concerning a proof or refutation of Turing-completeness.
;; 
;; In subsequent times, however, specimens of languages succeeded in
;; proving the test's general incompetence to its desiderated telos by
;; passing its requirements without actual endowment of the
;; Turing-complete attribute.
;; 
;; Natheless, the proposed concept yet entails a negation potential:
;; While its tasks' vanquishment cannot vouch for the imputed
;; characteristic's presence, failure in their accommodation proves the
;; language's incompatibility with the completeness claim.
;; 
;; == DISAN COUNT: INPUT, LOOP, OUTPUT, INCREMENT ==
;; A pseudocode definition of the process shall be advanced as the
;; medium for the program's explication:
;; 
;;   n <- query a non-negative integer number
;;   a <- 0
;;   
;;   while a < n do
;;     if a is even then
;;       print a + " is even!"
;;     end if
;;     
;;     a <- a + 1
;;   end while
;; 
;; == DISAN COUNT: A QUINTUPLE FEATURES TESTED ==
;; The Disan Count program commits to the evaluation of a quintuple
;; feature set:
;; 
;;   ------------------------------------------------------------------
;;   Tested feature | Description
;;   ---------------+--------------------------------------------------
;;   Arithmetics    | A simple arithmetical operation, a non-negative
;;                  | integer variable's incrementation by one, is
;;                  | realized in the probed variable's, "a", context.
;;   ..................................................................
;;   Assignment     | Both the input variable "n" and the iteration
;;                  | variable "a" require an assignment --- the former
;;                  | to a user input, the latter to a literal, both,
;;                  | however, commorant in the integer realm.
;;   ..................................................................
;;   Conditionals   | The indagation of the variable "a" for its value
;;                  | constituting an even integer appertains to the
;;                  | language's equality testing capability.
;;   ..................................................................
;;   Input/Output   | The probed language must be amenable to integer
;;                  | input requests, such as to assign the variable
;;                  | "n", and outputs involving both integers, here
;;                  | related to the storage entity "a", and strings.
;;                  | An advanced facility may be exposed by applying
;;                  | concatenation or extrapolation with respect to
;;                  | the heterogenous objects in the latter case.
;;   ..................................................................
;;   Iteration      | A construct for arbitrary tallies of repetitions
;;                  | based upon an equality test's satisfication is
;;                  | necessitated in order to successfully prosecute
;;                  | the "a" variable's indagation, modification, and
;;                  | termination introduction.
;;                  | Frequently, a "while" loop or the more
;;                  | sophisticated counting ("for") variant may be
;;                  | among the available warklumes. Low-level
;;                  | languages attend with crebritude to at least
;;                  | resort to goto control flow mechanisms.
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; Its simplistic design disencumbers Disan from all architectural
;; inquisitions, in which it engages in an equiparation with its
;; cynosure, the Disan Count program itself.
;; 
;; 
;; Data Type
;; =========
;; The Disan programming language itself does not wist about any type
;; requirements, as a single command partakes of effectiveness in the
;; entire system. The Disan Count program, a commorant and paravant
;; object from this realm, however, appertains to unsigned non-negative
;; integers for simple arithmetics, input, and output. Strings are
;; consigned to the treble's latter wike.
;; 
;; 
;; Syntax
;; ======
;; The Disan programming language's tolerance is exhausted by any
;; content divergent from the letter "D", its sole command's denotation.
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (EBNF) shall apply itself to the
;; throughout simplistic syntactical diorism:
;; 
;;   program := { "D" } ;
;; 
;; 
;; Instructions
;; ============
;; Disan's instruction tallies a single member, the "D" command, which
;; accompasses a Disan Count program's execution.
;; 
;; == OVERVIEW ==
;; The aefauld programmatic implement shall be a presentation's subject:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   D       | Executes the Disan Count program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Disan's absence in intricacy deprieves it from any conceivable
;; inroads of ambiguous passages.
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been realized in the programming language
;; Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-04
;; 
;; Sources:
;;   [esolang2023Disan]
;;   The Esolang contributors, "Disan", 22 April, 2020
;;   URL: "https://esolangs.org/wiki/Disan"
;;   Notes:
;;     - Specification of the Disan programming language.
;;   
;;   [esolang2020DisanCount]
;;   The Esolang contributors, "Disan Count", 23 June, 2020
;;   URL: "https://esolangs.org/wiki/Disan_Count"
;;   Notes:
;;     - Describes the Disan Count program concepts.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-disan-count ()
  "Executes the Disan Count program and returns no value."
  (let ((n (parse-integer (read-line)))
        (a 0))
    (declare (type (integer 0 *) n))
    (declare (type (integer 0 *) a))
    (loop while (< a n) do
      (when (evenp a)
        (format T "~&~d is even!" a))
      (incf a 1)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Disan (code)
  "Interprets the piece of Disan source CODE and returns no value."
  (declare (type string code))
  (loop
    for ip    of-type fixnum from 0 below (length code)
    for token of-type character = (char code ip)
    
    if (char= token #\D) do
      (execute-disan-count)
    else do
      (error "Invalid character \"~c\" at position ~d." token ip))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Disan "D")
