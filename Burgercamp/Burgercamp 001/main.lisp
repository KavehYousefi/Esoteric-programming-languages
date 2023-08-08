;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Burgercamp", invented by the Esolang user "Cinnamony" and
;; presented on June 16th, 2023, the kenspeckle attribute of which is
;; realized in its derivation from Jonathan Todd Skinner's "Deadfish"
;; while introducing a conspicuous extravagance in its commands'
;; causata.
;; 
;; 
;; Concept
;; =======
;; Burgercamp operates on a single register, or "accumulator", the
;; content of which may be incremented, decremented, multiplied, and
;; printed by commands whose intrinsic parameters perform an ilk of
;; supputation defined by kensback operands.
;; 
;; The program itself repeats in an infinite loop, querying upon each
;; cycle's inception the user for a string of commands, whose
;; specification segues into a consequent evaluation.
;; 
;; Non-command tokens do not instigate an error, instead issuing a
;; single newline output.
;; 
;; 
;; Architecture
;; ============
;; Burgercamp's storage model prescribes an aefauld register, also
;; norned the "accumulator", as a scalar integer's salvatory, the range
;; and polarity of its content being a thing not impounded by any
;; constraints, and its default at the program's inchoation equaling
;; zero (0).
;; 
;; While in a theoretical perspective the accumulator may, of course,
;; assume any state, the value 25 introduces an exemption: Its
;; acquisition automatically resets the register to its default of zero
;; (0).
;; 
;; 
;; Data Type
;; =========
;; A sole instrument of supputation resides in Burgercamp's type system:
;; the integer species that comprehends any sign and magnitude.
;; 
;; 
;; Syntax
;; ======
;; Burgercamp's syntaxis amplects any character with tolerance, maugre
;; its operational set's constriction to the four members "i", "d", "m",
;; and "o". The orra symbols are administered the role of newline output
;; instigators.
;; 
;; == INSTRUCTIONS ==
;; Instructions in the language are represented by an aefauld character,
;; maintaining abstinence from the requisites of arguments.
;; 
;; Even non-command symbol are accepted, but incite the issuing of a
;; newline output.
;; 
;; == COMMENTS ==
;; No provision for comments are present in this language rendition.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation appertains
;; to the Burgercamp language:
;; 
;;   program   := { command | nop } ;
;;   command   := increment | decrement | multiply | output ;
;;   nop       := character - ( "i" | "d" | "m" | "o" ) ;
;;   increment := "i" ;
;;   decrement := "d" ;
;;   multiply  := "m" ;
;;   output    := "o" ;
;; 
;; 
;; Instructions
;; ============
;; Burgercamp's instruction set is compact of a quadruple membership,
;; the very niggardly adhibited facilities of which include basic
;; arithmetics and a numeric output, but neither input nor conditional
;; specimens.
;; 
;; == REPL ==
;; The workings of a program in this language relocate it to the wike of
;; a REPL-like processor --- a read-eval-print-loop iterator ---, which
;; consumes a user input of commands, evaluates, and contingently prints
;; the same, ere an iterum secle proceeds.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall adhibit a cursory gnarity anenst the
;; language's commodities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator value by seven (7).
;;           | If the new accumulator state equals 25, it is
;;           | immediately reset to zero (0).
;;   ..................................................................
;;   d       | Decrements the accumulator value by three (3).
;;           | If the new accumulator state equals 25, it is
;;           | immediately reset to zero (0).
;;   ..................................................................
;;   m       | Multiplies the accumulator value by five (5).
;;           | If the new accumulator state equals 25, it is
;;           | immediately reset to zero (0).
;;   ..................................................................
;;   o       | Outputs the accumulator value verbatim.
;;   ------------------------------------------------------------------
;; 
;; Any other character when encountered, albeit no recognized operation,
;; does not raise an error, in lieu of that issuing a single newline
;; output.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The protolog's lucidity obviates the blemishes of a preponderance
;; among the ambiguities admissive to one's ideation; a few extant
;; specimens, however, shall be the following sections' cynosure.
;; 
;; == HOW DOES BURGERCAMP RESPOND TO UNRECOGNIZED TOKENS? ==
;; A descendent of Deadfish, which comments upon non-command tokens'
;; intrusion via a simple newline issuing, Burgercamp abstains from an
;; explicit declamation or refusal of its stock-father's dioristic
;; deportment.
;; 
;; It has been adjudged to transpose this attribute verbatim from the
;; cleronomy, thus responding to invalid input with newline output.
;; 
;; == DOES BURGERCAMP OPERATE INTERACTIVELY? ==
;; A further kenspeckle haecceity of Deadfish, its interpreter, in lieu
;; of mandatory source code provision in a static form, operates in an
;; infinite, or in some variations terminable, iteration, each secle
;; defined by the querying for an input string that amplects zero or
;; instructions, and its subsequent evaluation, similar to a
;; read-eval-print-loop (REPL) facility in some other programming
;; languages, such as Common Lisp. It is not communicated whether the
;; scion Burgercamp subscribes to the same mode of intercourse.
;; 
;; It has been adjudged, in the face of the foundational similarities
;; between the two languages, to replicate Deadfish's interaction
;; principles in Burgercamp. Optionally, an interpreter for the latter
;; may apply itself to the homologation of an initial program's
;; induction.
;; 
;; 
;; Implementation
;; ==============
;; This implementation in Common Lisp enjoys the utilization of the
;; language's goto facility, which, upon an apercu, may ostend an
;; exercise in mateotechny, albeit effectively participates with
;; epideictic motives.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-08
;; 
;; Sources:
;;   [esolang2023Burgercamp]
;;   The Esolang contributors, "Burgercamp", June 20th, 2023
;;   URL: "https://esolangs.org/wiki/Burgercamp"
;;   Notes:
;;     - Specification of the Burgercamp programming language.
;;   
;;   [esolang2023BurgercampConst]
;;   The Esolang contributors, "Burgercamp/Constants", June 20th, 2023
;;   URL: "https://esolangs.org/wiki/Burgercamp/Constants"
;;   Notes:
;;     - Listing of constants defined in the Burgercamp programming
;;       language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-modify-macro multf (multiplier) *
  "Multiplies the argument, expected to be an accessible place, by the
   MULTIPLIER, modifies the former, and returns the product.")

;;; -------------------------------------------------------

(defun interpret-Burgercamp (&optional (initial-code ""
                                        initial-code-supplied-p))
  "Incites the Burgercamp interpreter, contingently proceeding with a
   first execution of the INITIAL-CODE, if such is supplied, in any
   case repeatedly querying for user input until a completely empty line
   of zero characters' tally is supplied, and returns no value."
  (declare (type string initial-code))
  (declare (type T      initial-code-supplied-p))
  (prog ((accumulator 0)
         (input       "")
         (ip          0))
    (declare (type integer accumulator))
    (declare (type string  input))
    (declare (type fixnum  ip))
    
    (cond
      (initial-code-supplied-p
        (setf input initial-code)
        (go   process-command))
      (T
        (go query)))
    
    query
      (format T "~&>> ")
      (setf input (read-line *standard-input* NIL ""))
      (setf ip    0)
      (if (and input (plusp (length input)))
        (go process-command)
        (go end))
    
    process-command
      (case (char input ip)
        (#\i       (go increment))
        (#\d       (go decrement))
        (#\m       (go multiply))
        (#\o       (go output))
        (otherwise (go no-operation)))
    
    increment
      (incf accumulator 7)
      (go normalize-accumulator)
    
    decrement
      (decf accumulator 3)
      (go normalize-accumulator)
    
    multiply
      (multf accumulator 5)
      (go normalize-accumulator)
    
    output
      (format T "~d " accumulator)
      (go advance-ip)
    
    no-operation
      (terpri)
      (go advance-ip)
    
    normalize-accumulator
      (when (= accumulator 25)
        (setf accumulator 0))
      (go advance-ip)
    
    advance-ip
      (incf ip)
      (if (< ip (length input))
        (go process-command)
        (go query))
    
    end
      (values)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exploit the consuetude of Burgercamp to relapse for an accumulator
;; state of 25 into its default value zero (0), and output the ultimity.
(interpret-Burgercamp "ididdmo")

;;; -------------------------------------------------------

;; Print the value 32 to the standard output,
;;   (7 * 5) - 3 = 32.
(interpret-Burgercamp "imdo")
