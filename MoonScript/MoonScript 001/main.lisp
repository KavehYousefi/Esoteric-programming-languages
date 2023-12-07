;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "MoonScript", presented by the Esolang user "Hakerh400" on
;; December 1st, 2022, and purported to be a concoction of an advanced
;; GPT AI neural network, the competences of this product ostending a
;; severe restriction to basic arithmetics, input, and output, expressed
;; in single-character identifiers intimating a simulacrum to Urban
;; Mueller's "brainfuck" donat.
;; 
;; 
;; Concept
;; =======
;; The MoonScript programming language is founded upon a single
;; register's signed byte manipulation, imposing this dever to the
;; coeffiency of four instructions to whom the potentials for basic
;; arithmetics and input/output communication are endowed.
;; 
;; == MOONSCRIPT: A BAIRN OF ARTIFICIAL INTELLIGENCE ==
;; The language's most conspicable diorism exists in relation to its
;; parturition, narrated as issuing from an advanced GPT AI neural
;; network as its provenance. The chatlog involved in this human-machine
;; intercourse shall be reiterated in an ipsissima verba fashion, merely
;; deviant in its exposure by a formatting that obeys this
;; documentation's boundaries:
;; 
;;   Q: Can you invent an esoteric programming language, please?
;;   
;;   ChatGPT: Sure, here is one that I just came up with. This language
;;   is called "MoonScript" and it is designed to be as simple and
;;   minimalistic as possible. The only data type in MoonScript is a
;;   number, and the only operation allowed is incrementing or
;;   decrementing a number by one. 
;; 
;; == THE MEMORY: A SINGLE SIGNED BYTE REGISTER ==
;; The memory's perimeter does not dispand beyond an aefauld register's
;; latreutical endeavors, the gamut of its purview's exercise impounded
;; to the integral range [-128, +127], with a wrapping behavior for
;; normalizing deviant numbers.
;; 
;; == FOUR OPERATIONS REALIZE ARITHMETICS AND COMMUNICATIONS ==
;; A quadruple tally of operations exists for the program state's
;; manipulation, enumerating a gradual incrementation, decrementation,
;; the input conduit's query for an ASCII character, and the athwart
;; process of the register's output in this symbolic form.
;; 
;; 
;; Architecture
;; ============
;; The program's memory in its compass follows already a delineation's
;; patration by enumerating its aefauld salvatory, the register, this
;; being a signed byte's storage, the gamut of which, perforce, admits
;; the integral range of [-128, +127].
;; 
;; Its inchoation empights the register at the value zero (0), whence an
;; amenability to gradual incrementations and deductions permits the
;; modulation in compernage to its perquisition's avail.
;; 
;; The state's comportment subscribes to a wrapping fashion, which means
;; that transcendences beyond the proper bournes segue into a
;; continuation on the obverse interval post. In a more concrete
;; diction, if an incrementing would violate the upper march of 255, the
;; register returns to the minimum of -128; proceeding similiter, a
;; descent below the bottom extremum of -128 relocates the value to the
;; maximum of 127.
;; 
;; 
;; Data Types
;; ==========
;; MoonScript relies on one species of data only: signed bytes commorant
;; in the integral range [-128, +127].
;; 
;; 
;; Syntax
;; ======
;; The language's donat constitutes a thing of utmost simplicity in its
;; deployment of a quadruple single-character instruction set.
;; 
;; Whitespaces may be adhibited in any mete of magnanimity, while any
;; further adscititious participation excites an error.
;; 
;; == GRAMMAR ==
;; The language's syntax shall be an Extended Backus-Naur Form (ENBF)
;; formulation's subject:
;; 
;;   program    := { spaces , operation } ;
;;   operation  := "+" | "-" | "." | "," ;
;;   spaces     := { whitespace } ;
;;   whitespace := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; MoonScript's instruction set tallies a mere quadruple membership, the
;; circumference of which admits basic arithmetics, as well as input and
;; output facilities founded upon ASCII characters as the tokens of
;; commerce.
;; 
;; == OVERVIEW ==
;; A cursory mete of nortelry's administration concerning the operative
;; competences shall be elicited as the following apercu's dation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the register by one.
;;           | If its new value transgresses the upper march of +127,
;;           | the state relapses to the minimum of -128.
;;   ..................................................................
;;   -       | Decrements the register by one.
;;           | If its new value transgresses the lower march of -128,
;;           | the state wraps around to the maximum of +127.
;;   ..................................................................
;;   .       | Prints to the standard output the character whose ASCII
;;           | code, expressed in the signed byte range of
;;           | [-128, +127], corresponds to the register value.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code, construed as a signed byte from the
;;           | range [-128, +127], in the register.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; MoonScript's niggardliness in the exposition of its haecceity
;; inflicts the protolog with a certain grade of ambiguous inroads, the
;; most salient among these shall be the following sections' cynosure.
;; 
;; == HOW DOES THE SYSTEM RESPOND ON ITS REGISTER'S BOURNES? ==
;; The program memory's reliance upon an signed byte register's
;; adminiculum naturally coerces a lealty to the interval [-128, +127].
;; The specification, however, exercises abstinence from a treatise on
;; the respondency to attempts that eloign it from this perimeter.
;; 
;; It has been adjudged, following brainfuck's forbisen, a locus
;; classicus whose influence in general, and tenable vinculum in this
;; particular case, homologates a construe ejusdem generis, to impute a
;; wrapping comportment, the same at the upper boundary retreats to the
;; minimum of -128, while this bottom extremum's transgression returns
;; to the maximum of 127.
;; 
;; 
;; Implementation
;; ==============
;; This project's realization has been accompassed in the programming
;; language Common Lisp, its foundation a simple direction evaluation of
;; the MoonScript source code.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-12-07
;; 
;; Sources:
;;   [esolang2023MoonScript]
;;   The Esolang contributors, "MoonScript", February 25th, 2023
;;   URL: "https://esolangs.org/wiki/MoonScript"
;;   
;;   [mkyong2020convbytes]
;;   Yong Mook Kim,
;;     "Java -- Convert bytes to unsigned bytes", June 3rd, 2020
;;   URL: "https://mkyong.com/java/
;;         java-convert-bytes-to-unsigned-bytes/"
;;   Notes:
;;     - Demonstrates the conversion of signed bytes into their unsigned
;;       paregals in the Java programming language.
;;     - This is accomplished via a logical AND-combination with the
;;       value 255, or, in hexadecimal notation, "FF".
;;     - In pseudocode:
;;         function convertToUnsignedByte (signedByte)
;;           return (signedByte AND 255)
;;         end function
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines a signed byte compact of eight accolent
   bits, the commorancy of which resolves to the closed integral range
   [-128, 127]."
  '(signed-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize-into-octet-range (number)
  "Ascertains the NUMBER's lealty to the signed byte range of
   [-128, 127] by contingently wrapping the same around inside of this
   gamut, and returns the normalized datum."
  (declare (type integer number))
  (the octet
    (- (mod (+ number 128) 256) 128)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-character (ascii-code)
  "Returns the character which answers to the signed byte ASCII-CODE."
  (declare (type octet ascii-code))
  (the character
    (code-char
      (logand ascii-code 255))))

;;; -------------------------------------------------------

(defun encode-character (character)
  "Returns the signed byte ASCII code corresponding to the CHARACTER."
  (declare (type character character))
  (the character
    (normalize-into-octet-range
      (char-code character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-MoonScript (code)
  "Interprets the piece of MoonScript source CODE and returns no value."
  (declare (type string code))
  (let ((register 0))
    (declare (type octet register))
    (loop
      for token    of-type character across code
      for position of-type fixnum    from   0
      do
        (case token
          ;; Skip whitespaces.
          ((#\Newline #\Space #\Tab)
            NIL)
          
          ;; Increment the register.
          (#\+
            (setf register
              (normalize-into-octet-range
                (1+ register))))
          
          ;; Decrement the register.
          (#\-
            (setf register
              (normalize-into-octet-range
                (1- register))))
          
          ;; Output the register as an ASCII character.
          (#\.
            (write-char
              (decode-character register)))
          
          ;; Input a character and store its signed ASCII code.
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf register
              (encode-character
                (read-char)))
            (clear-input))
          
          ;; Any other character is inflicted with an interdiction.
          (otherwise
            (error "Invalid character \"~c\" at position ~d."
              token position)))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query the user for a character, print its successor, according to the
;; ASCII code standard, and subsequently the input symbol itself.
(interpret-MoonScript ",+.-.")
