;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Chefs Kiss", invented by the Esolang user "PaxtonPenguin"
;; and presented on October 12th, 2023, its gendrure's provenance the
;; entheus of "+-=", a language by the user "Anonymous", cognate in an
;; eath conception by wisting only of an integer-valued accumulator and
;; the warklumes for its incrementation, decrementation, as well as
;; textual presentation.
;; 
;; 
;; Concept
;; =======
;; The Chefs Kiss programming language subsumes into the ilk of ludibund
;; specimens, its menu's offering exhausted already by a treble of
;; instructions which serve in an signed integer-valued accumulator's
;; modulation and output.
;; 
;; == THE MEMORY: A SIGNED INTEGER SCALAR ==
;; The program memory's conformation does not deploy any intricacy
;; beyond an aefauld accumulator's services; thilk lends a salvatory
;; to a signed integer number of any mickleness' admission.
;; 
;; Operations exist for the datum's stillatim incrementation,
;; decrementation, and output in the form of the ASCII character whose
;; code corresponds to the accumulator state.
;; 
;; == THE OPERATIVE COMPETENCES AMPLECTS THREE INSTRUCTIONS ==
;; A triad of operations, each such represented by an emoji alluding to
;; the manus, partakes of the language's competences, admitting the
;; accumulator's incrementation, decrementation, and textual output.
;; 
;; Upon a non-operative symbol's consumption, the program persists in
;; its execution, while exhibiting the epiphenomenal action issuing the
;; display of the message "Error!" to the standard output.
;; 
;; 
;; Instructions
;; ============
;; Chefs Kiss's instruction set enumerates a prial membership, the
;; competences woning inwith thilk such to apply extremely basic
;; arithmetics, as well as printing characters.
;; 
;; Any token not affiliated with an operation instigates the issuance of
;; the message "Error!" on the standard output, without the program
;; execution's concomitant abortion.
;; 
;; == OVERVIEW ==
;; The following tabular exposition's dever shall be the communication
;; of the requisite gnarity with the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   👍       | Increments the accumulator value by one (1).
;;   ..................................................................
;;   👎       | Decrements the accumulator value by one (1).
;;   ..................................................................
;;   👌       | Prints the character whose ASCII code corresponds to
;;           | the accumulator value to the standard output.
;;           |---------------------------------------------------------
;;           | No further content, including newlines, is appended to
;;           | the output.
;;           |---------------------------------------------------------
;;           | The behavior is undefined if the accumulator state does
;;           | not represent a valid ASCII code. Maugre this, a
;;           | reasonable choice resides in the temporary wrapping of
;;           | the accumulator value into the valid ASCII range of
;;           | [0, 255] ere the display, without this modulation's
;;           | perpetuation; that is, in a pseudocode diction:
;;           | 
;;           |   let asciiCode <- accumulator modulo 256
;;           |   print character for asciiCode
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its status as a derivation of an extant specimen, "+-=", and
;; the consequent applicability of the ejusdem generis principles, the
;; Chefs Kiss programming language is targeted by a few inroads that
;; vitiate its complete sickerness in construe. A subset of these
;; deliberations shall be the below tmemata's cynosure.
;; 
;; == SHALL OUTPUT BE CONCLUDED WITH A LINEBREAK? ==
;; The "+-=" programming language expressly states the necessity to
;; append each output, numeric in its dioristic exercise, with a single
;; newline characte, while the Chefs Kiss diorism abstains from this
;; covenant's implication.
;; 
;; It has been adjudged to resolve the conflict with the imputation that
;; no newline, nor any other adscititious content, shall be adhibited
;; to the Chefs Kiss output action's display, forecause any whitespace
;; may be replicated explicitly by the ASCII character repertoire, if
;; conflating with the developer's desiderata.
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been realized in the programming language
;; Common Lisp, its reification an aspirant to the modeled language's
;; simplicity by immediate operation on the source code.
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
;; Date:   2025-04-20
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023ChefsKiss]
;;   The Esolang contributors, "Chefs Kiss", October 13th, 2023
;;   URL: "https://esolangs.org/wiki/Chefs_Kiss"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which amplects, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Chefs-Kiss (code)
  "Interprets the piece of Chefs Kiss source CODE and returns no value."
  (declare (type string code))
  (let ((accumulator 0))
    (declare (type integer accumulator))
    (loop for current-token of-type character across code do
      (case current-token
        (#\👍      (incf accumulator))
        (#\👎      (decf accumulator))
        (#\👌      (format T "~c"
                     (code-char
                       (mod accumulator 256))))
        (otherwise (format T "~&Error!")))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-several-times (character-to-print
                            number-of-repetitions
                            destination)
  "Writes the CHARACTER-TO-PRINT the NUMBER-OF-REPETITIONS times to the
   DESTINATION and returns no value."
  (declare (type character     character-to-print))
  (declare (type (integer 0 *) number-of-repetitions))
  (declare (type destination   destination))
  (format destination "~v@{~a~:*~}"
    number-of-repetitions
    character-to-print)
  (values))

;;; -------------------------------------------------------

(defun generate-text-program (text &key (destination NIL))
  "Generates a Chefs Kiss program whose capacitation amounts to the
   display of the TEXT on the standard output, writes the resulting
   source code to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'', otherwise produces a fresh string
   comprehending the result."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((accumulator 0))
        (declare (type integer accumulator))
        (loop for text-character of-type character across text do
          (let ((new-accumulator (char-code text-character)))
            (declare (type fixnum new-accumulator))
            (cond
              ((< accumulator new-accumulator)
                (print-several-times #\👍
                  (- new-accumulator accumulator)
                  destination))
              ((= accumulator new-accumulator)
                NIL)
              ((> accumulator new-accumulator)
                (print-several-times #\👎
                  (- accumulator new-accumulator)
                  destination))
              (T
                (error "Unexpected relationship betwixt the current ~
                        accumulator state ~d and the intended ~d."
                  accumulator new-accumulator)))
            (format destination "👌")
            (setf accumulator new-accumulator))))
      (with-output-to-string (arrowfuck-code)
        (declare (type string-stream arrowfuck-code))
        (generate-text-program text :destination arrowfuck-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the character associated with the ASCII code 42, that is, "*",
;; to the standard output.
(interpret-Chefs-Kiss
  "👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👌")

;;; -------------------------------------------------------

;; Print the text "Hello, World!" to the standard output.
(interpret-Chefs-Kiss
"👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👌👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👌👍👍👍👍👍👍👍👌👌👍👍👍👌👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👌👎👎👎👎👎👎👎👎👎👎👎👎👌👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👌👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👍👌👍👍👍👌👎👎👎👎👎👎👌👎👎👎👎👎👎👎👎👌👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👌")
