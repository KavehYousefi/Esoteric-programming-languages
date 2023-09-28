;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Hsamsniarb", presented by the Esolang user identified by
;; "188.67.13.130" on April 22nd, 2014, the proprium of which seems to
;; originate in a derivation from Jonathan Todd Skinner's "Deadfish",
;; with traces of Urban Mueller's "brainfuck" incorporated, manipulating
;; and indagating in this case an aefauld unsigned-byte-valued register
;; by a quadruple instruction set whose commands are norned via single
;; character identifiers.
;; 
;; 
;; Concept
;; =======
;; The Hsamsniarb programming language is founded upon the manipulation
;; of a single register, impounded in its capacity to the unsigned byte
;; range [0, 255], by adminiculum of a quadruple instruction, the
;; competence of whom does furnish any supererogation besides basic
;; arithmetics and the register's verbatim output, while the entire
;; interpreter operates in a perpetual loop repeatedly quering the
;; standard input for a behest.
;; 
;; == HSAMSNIARB: "BRAINSMASH" IN REVERSE ==
;; The agnomination "Hsamsniarb" is explained as a reversal of the term
;; "Brainsmash" --- albeit a phenotypical semblance to Deadfish
;; furnishes the most kenspeckle property, at least two commands, "+"
;; and "-", as well as the restriction of the sole memory cell to an
;; unsigned byte range intimate Urban Mueller's language brainfuck as an
;; at least parergal entheus.
;; 
;; == THE REGISTER: A SINGLE OCTET ==
;; The complete program memory is established in the form of a single
;; register, capacitated to maintain an unsigned byte value from the
;; integral range [0, 255], and initialized at the program's inchoation
;; to the value zero (0).
;; 
;; Instructions exist to increment and decrement the value in
;; graduation, as well to square the same, and output its verbatim
;; state.
;; 
;; Upon its bournes' transgression, the register is automatically reset
;; to the inchoate state of zero (0).
;; 
;; == FOUR COMMANDS, FOUR CHARACTERS ==
;; The language's instruction set tallies only four members, each
;; identified an aefauld character from the set {"+", "-", "s", "w"},
;; administering to any other content no causatum nor responsiveness.
;; 
;; == PROGRAMS OPERATE IN AN INFINITE LOOP ==
;; The Hsamsniarb interpreter, succeeding its optionally induced initial
;; code's procession, obtained via the command line, repeatedly queries
;; the standard input for a character to subject to the selfsame
;; evaluation process, in corollary perpetuating a "REL" --- "Read",
;; "Evaluate", "Loop" --- mechanism.
;; 
;; Every such query is preceded by a fresh line of the form
;; 
;;   >>
;; 
;; that is, two closing angular brackets followed by a single space,
;; at the dextral laterality of which the user input symbol is expected.
;; 
;; 
;; Architecture
;; ============
;; Anenst its architectural department, Hsamsniarb employs a single
;; register whose capacity does not exceeds the unsigned byte range
;; [0, 255], upon these bournes' transgression relapsing to the initial
;; zero (0) state.
;; 
;; == THE MEMORY: A SINGLE UNSIGNED BYTE VALUE ==
;; Hsamsniarb's memory model is desumed from a consilience of Deadfish
;; and brainfuck, appropriating from the former the restriction to an
;; aefauld register, while a concomitant acquisition from the second
;; moeity constrains the storage unit's circumference to the realm of
;; unsigned bytes, that is, the integral range [0, 255].
;; 
;; At the program's inchoation empighted on the default value of zero
;; (0), the register automatically resets its value upon any of its two
;; marches' transgression.
;; 
;; == MEMORY MODEL JUXTAPOSITION ==
;; A tabular illustration shall equiparate the memory models commorant
;; in brainfuck, Deadfish, and its conceptual scion Hsamsniarb:
;; 
;;   ------------------------------------------------------------------
;;                | brainfuck       | Deadfish        | Hsamsniarb     
;;   -------------+-----------------+-----------------+----------------
;;   Topology     | infinite tape   | one register    | one register
;;                | using pointer   |                 | 
;;   ..................................................................
;;   Capacity per | unsigned byte   | unbounded       | unsigned byte
;;   unit         | scalar in range | signed integer  | scalar in range
;;                | [0, 255]        | scalar          | [0, 255]
;;   ..................................................................
;;   Behavior on  | wraps around    | resets to 0 if  | sets to 0 if
;;   boundaries   |                 | and only if     | less than 0 or
;;                |                 | equal to -1 or  | greater than
;;                |                 | 256             | 255
;;   ------------------------------------------------------------------
;; 
;; 
;; Data Type
;; =========
;; The Hsamsniarb programming language wists of one type only: the
;; unsigned byte species, an occupant of the closed integral range
;; [0, 255], ensconced as a scalar in its aefauld register.
;; 
;; 
;; Instructions
;; ============
;; Hsamsniarb's instruction set is compact of a quadruple contingency,
;; in its preponderance consilient will Deadfish's equinumerant potence,
;; however, deviating at most instances in the assigned nomenclature.
;; 
;; == FOUR OPERATIONS ARE AVAILABLE ==
;; Hsamsniarb produces a cardinality of four members, entalented with
;; the governance over its sole register, and invested with the faculty
;; for gradual incrementation, decrementation, squaring, and numeric
;; output.
;; 
;; == EVERY COMMAND IS EXPRESSED BY ONE CHARACTER ==
;; The complete operational circumference apportions to any member of
;; this quartet a single symbol as the unique identifier, borrowed from
;; the set constituted by "+", "-", "s", and "w".
;; 
;; A sepiment does not tally among the requisites for the instructions'
;; distinguishment.
;; 
;; == NON-COMMAND TOKENS ARE IGNORED ==
;; Any character or character sequence not capable of being construed as
;; a command is simply ignored.
;; 
;; This aspect Hsamsniarb deviates from Deadfuck's notion, as the latter
;; responds to every unrecognized or erroneous communication via a fresh
;; newline output, while its descendant simply retains a state of a
;; stoic disregard.
;; 
;; == THE INTERPRETER OPERATES IN A PERPETUAL LOOP ==
;; Ensuing from the evaluation of its zero or more initial tokens,
;; furnished by the command line as its medium, the interpreter enters
;; in an infinite loop, repeatedly prompting the standard input for a
;; character, signified via a message
;; 
;;   >> 
;; 
;; which occupies a line of its own, but appends a single space in lieu
;; of a linebreak to the rightwards directed angular brackets (">>"),
;; and interprets the same.
;; 
;; As an apostille, a second diorism's abode is realized in this reading
;; principle, as Hsamsniarb mandates exactly one character's delivery as
;; the input request's consequence, obverse from Deadfish's admission of
;; a potentially empty string whose circumference might encompass an
;; arbitrary token tally.
;; 
;; == OVERVIEW ==
;; The following apercu shall adhibit a basic piece of nortelry
;; regarding the language's warklumes:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the register value by one (1).
;;           | If the new value exceeds the maximum of 255, the state
;;           | is reset to zero (0).
;;   ..................................................................
;;   -       | Decrements the register value by one (1).
;;           | If the new value descends below the minimum of zero (0),
;;           | the state is reset to zero (0).
;;   ..................................................................
;;   s       | Squares the register value, that is, multiplies it by
;;           | itself.
;;           | If the new value exceeds the maximum of 255, the state
;;           | is reset to zero (0).
;;   ..................................................................
;;   w       | Prints the register value in its verbatim numeric form
;;           | to the standard output, preceded by a linebreak and
;;           | succeeded by another one.
;;   ------------------------------------------------------------------
;; 
;; == HSAMSNIARB'S AND DEADFISH'S INSTRUCTIONS ARE PAREGAL ==
;; The instructions of Hsamsniarb and its entheus Deadfish shall be
;; subjected to an equiparation:
;; 
;;   ------------------------------------------------------------------
;;   Hsamsniarb | Deadfish | Apostille
;;   -----------+----------+-------------------------------------------
;;   +          | i        | Hsamsniarb's incrementation command
;;              |          | resembles that of brainfuck.
;;   ..................................................................
;;   -          | d        | Hsamsniarb' deduction command resembles
;;              |          | that of brainfuck.
;;   ..................................................................
;;   s          | s        | Hsamsniarb programs cannot exceed the
;;              |          | octet range [0, 255], while Deadfish
;;              |          | equivalents only reset if the register
;;              |          | exactly coincides with -1 or 256.
;;   ..................................................................
;;   w          | o        | -
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The simplistic nature of Hsamsniarb endowing it with its indicium,
;; most contingency for detrimental inroads by ambiguities remain
;; extinguished, a single point, however, has been detected to levy
;; dubitation.
;; 
;; == SHALL UNCONDITIONAL LINEBREAKS APPLY TO THE INITIAL CODE? ==
;; The original Hsamsniarb implementation issues linebreaks to the
;; standard output in the wake of any valid command, even in those
;; circumstances not vindicated by the prompt text's requisites, which
;; might pervert the complete display by nimiety in blank lines.
;; 
;; Ere its perpetual evaluation iteration's commencement, Hsamsniarb
;; homologates an initial input string, in the protolog's exemplary
;; implementation manifesting in the entire command line argument, to
;; be furnished for procession. The responsible operational unit, the
;; "Interpret" procedure, associates with any recognized command token
;; a newline output as an epiphenomenon --- most probably a parasceve
;; to the prompt message display, which otherwise would not accommodate
;; the ">> " sequence with a line of its own.
;; 
;; While the sensibility of this approach cannot be assailed in the
;; read-evaluate-loop stages' context, its Procrustean application
;; during the initial command line input's perquisition concludes in
;; supererogative newline printings to the standard output.
;; 
;; The protolog indulges in reticence about the deliberate nature of
;; this response.
;; 
;; It has been adjudged that the issuing of newline characters in the
;; initial code evaluation stage does not impose a component of the
;; language's regulations, and most likely has been installed as a
;; convenience in order to obviate a superimposition of complexity. A
;; Hsamsniarb-conformant interpreter thus does not require to issue
;; newlines, except for those relocating the register's output to a
;; horizontal spatiality of its own.
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
;; Date:   2023-09-20
;; 
;; Sources:
;;   [esolang2023Hsamsniarb]
;;   The Esolang contributors, "Hsamsniarb", May 12th, 2019
;;   URL: "https://esolangs.org/wiki/Hsamsniarb"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight accolent
   bits, and thus a commorant of the closed integral range [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type octet *register*))

;;; -------------------------------------------------------

(defparameter *register* 0
  "The register, an unsigned byte scalar storage which affords the
   program's singular memory component.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of register operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun increment-register ()
  "Increments the register value by one and returns no value.
   ---
   If the new value would exceed the upper march of 255, the register is
   reinstated in its initial state of zero (0)."
  (setf *register*
    (if (>= *register* 255)
      0
      (1+ *register*)))
  (values))

;;; -------------------------------------------------------

(defun decrement-register ()
  "Decrements the register value by one and returns no value.
   ---
   If the new value would violate the lower march of zero (0), the
   register is reinstated in its initial state of zero (0)."
  (setf *register*
    (if (<= *register* 0)
      0
      (1- *register*)))
  (values))

;;; -------------------------------------------------------

(defun square-register ()
  "Squares the register value and returns no value.
   ---
   If the new value would exceed the upper march of 255, the register is
   reinstated in its initial state of zero (0)."
  (let ((new-value (* *register* *register*)))
    (declare (type (integer 0 *) new-value))
    (setf *register*
      (if (> new-value 255)
        0
        new-value)))
  (values))

;;; -------------------------------------------------------

(defun output-register ()
  "Prints the register value in its verbatim form to the standard output
   and returns no value."
  (format T "~&~d" *register*)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-token (token)
  "Evaluates the TOKEN, accompassing its associated causatum in the case
   of its recognition as an instruction, otherwise abstaining from any
   action, and in any case returning no value."
  (declare (type character token))
  (case token
    (#\+       (increment-register))
    (#\-       (decrement-register))
    (#\s       (square-register))
    (#\w       (output-register))
    (otherwise NIL))
  (values))

;;; -------------------------------------------------------

(defun interpret-Hsamsniarb (&optional (initial-code ""))
  "Starts the Hsamsniarb, processing the INITIAL-CODE, if supplied, as
   a first sequence of contingent instructions, ere the perpetual
   evaluation loop's instigation, returning no value in the case of the
   operation's termination."
  (declare (type string initial-code))
  
  ;; Reset the register to its inchoate state of zero (0).
  (setf *register* 0)
  
  ;; Evaluate the INITIAL-CODE.
  (loop for token of-type character across initial-code do
    (interpret-token token))
  
  ;; Perpetually query for a character and evaluate it.
  (loop do
    (format T "~&>> ")
    (finish-output)
    
    (let ((input (read-char)))
      (declare (type character input))
      (clear-input)
      (interpret-token input)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the initial register value, which equals zero (0), increment
;; the register by one, and print the value state of one (1).
(interpret-Hsamsniarb "w+w")

;;; -------------------------------------------------------

;; Demonstrate the register' resetting behavior by augmenting its
;; gradually augmenting its value to 256, while printing the state
;; several times at significant points.
(interpret-Hsamsniarb "++++wswsw")
