;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements an interpreter, encoder, and decoder for the
;; esoteric programming language "Hell69", also known as "Hell to the
;; sixty-ninth power", and invented by the Esolang user
;; "Areallycoolusername", being pursuant in coalescing an output-only
;; tool with the wanton number 69 as an expression of its perplexity.
;; 
;; Concepts
;; ========
;; Hell69's diorism resides in the fact that, being merely able to print
;; text, the characters' appearance is subjected to severe distortions,
;; producing a pseudo-hexadecimal simulacrum, with the number 69 eath to
;; be agnized in the interstices.
;; 
;; == HELL69 RELATES TO THE JOKE LANGUAGES ==
;; Hell69 ascribes to the particular species of esoteric contingent
;; acquainted to the popular donet as joke languages, such linguistic
;; products whose lusory aspects completely vanquish any orrels of
;; chreotechnics aiblins still extant in the superordinated realm.
;; 
;; == THE LANGUAGE CAN ONLY PRINT TEXT ==
;; An exposition of its peculiar haecceity, the language responds to no
;; true instructions, instead merely output generation is capacitated,
;; the content of the same being defined by the program's tokens as the
;; exclusive operation's arguments.
;; 
;; == HELL69: DIFFICULTY AND THE NUMBER 69 ==
;; The language's tenet derives from an algolagnic attempt at producing
;; little effect with a nimious mete of investment. The code generation
;; proceeds by a series of transformations applied to each character in
;; order to produce an allusion to the licentious number "69", acting
;; especially in the area of hexadecimal representation. The againward
;; motion, when applied to the concept, restores the original data.
;; 
;; == HELL69-ENCODING ==
;; The manifestations of its translation from a plaintext document,
;; compact of ASCII characters, partakes of a several stages, incited by
;; converting each character into its ASCII code's two-digit hexadecimal
;; equivalent. Proceeding from this foundational unit, a treble stage
;; plan fulfils a datum's Hell69-encoding, as adduced by the following
;; delineation:
;;   
;;   (1) TRANSFORM THE DIGITS AND LETTERS
;;       The two characters of the hexadecimal number, each of which
;;       accounts either for a decimal digit or a letter from a to f,
;;       must be transcripted in a first step by multiplying the digit
;;       by 69 and replacing the letter by its successor in the
;;       alphabet; concretely:
;;         (1.a) If the hexadecimal number consists of two digits, the
;;               the result is calculated by regarding this number as a
;;               decimal value --- without conversion from the base-16
;;               to the base-10 system ---, and multiplying the quantity
;;               by the decimal number 69.
;;         (1.b) If the first moeity of the hexadecimal number is a
;;               digit, while the second constitutes a letter, the
;;               result is obtained by regarding the digit as a decimal
;;               number --- without conversion to the base-10
;;               system ---, and multiplying it by 69; to this product
;;               is then appended the letter in the alphabet succeeding
;;               the second hexadecimal character.
;;         (1.c) If the first moeity of the hexadecimal number manifests
;;               in a letter, while the second establishes a decimal
;;               digit, the result is obtained by substituting the
;;               letter by its successor in the alphabet, and adhibiting
;;               to it the product of the digit construed as a decimal
;;               number --- without conversion to base-10 ---,
;;               multiplied by 69.
;;         (1.d) If both constituents of the hexadecimal number embrace
;;               letters, the result is obtained by substituting each
;;               of them by their successor letter in the alphabet.
;;   (2) REPLACE ALL ZERO (0) DIGITS BY 69
;;       The previous step's produce, a two-character string borrowing
;;       its contents from the digits [0, 9] and letters b to g, is
;;       transformed by replacing each zero ("0") by the sequence "69",
;;       concluding in the raw Hell69 number.
;;   (3) PREPEND THE PREFIX "69x"
;;       As a surrogate of the traditional hexadecimal prefix "0x", the
;;       sequence "69x" is prepended to the preceding step's result in
;;       order to generate the final Hell69 token.
;; 
;; Some mete of magnamity shall be exercised now in the Hell69-encoding
;; process' formulation in pseudocode:
;;   
;;   function isDecimalDigit (x : character) : boolean
;;     if x is in { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" } then
;;       return true
;;     else
;;       return false
;;     end if
;;   end function
;;   
;;   function isHexLetter (x : character) : boolean
;;     if x is in { "a", "b", "c", "d", "e", "f", "A", "B", "C", "D", "E", "F" } then
;;       return true
;;     else
;;       return false
;;     end if
;;   end function
;;   
;;   function concatenate (x : character or integer,
;;                         y : character or integer) : string
;;     return make string of x, y
;;   end function
;;   
;;   function asString (x : integer) : string
;;     return x converted to a string
;;   end function
;;   
;;   function asDecimalNumber (x : character, string) : integer
;;     return x parsed as a decimal integer
;;   end function
;;   
;;   function getNextLetter (x : character) : character
;;     if character is "a" or "A" then
;;       return "b"
;;     else if character is "b" or "B" then
;;       return "c"
;;     else if character is "c" or "C" then
;;       return "d"
;;     else if character is "d" or "D" then
;;       return "e"
;;     else if character is "e" or "E" then
;;       return "f"
;;     else if character is "f" or "F" then
;;       return "g"
;;     else
;;       Signal an error: "Invalid hexadecimal digit."
;;     end if
;;   end function
;;   
;;   { The parameter "hexNumber" constitutes a string of two      }
;;   { hexadecimal digits representing an ASCII character's code. }
;;   function convertHexadecimalStringToHell69Token (hexNumber : string) : string
;;     let result         <- 0
;;     let firstHexDigit  <- lower-valued  hexadecimal digit of hexNumber
;;     let secondHexDigit <- higher-valued hexadecimal digit of hexHumber
;;     
;;     if isDigit(firstHexDigit) and isDigit(secondHexDigit) then
;;       result <- asString(asDecimalNumber(hexNumber) * 69)
;;     else if isDigit(firstHexDigit) and isHexLetter(secondHexDigit) then
;;       result <- concatenate(asDecimalNumber(firstHexDigit) * 69,
;;                             getNextLetter(secondHexDigit))
;;     else if isHexLetter(firstHexDigit) and isDigit(secondHexDigit) then
;;       result <- concatenate(getNextLetter(firstHexDigit),
;;                             asDecimalNumber(secondHexDigit) * 69)
;;     else if isHexLetter(firstHexDigit) and isHexLetter(secondHexDigit)
;;       result <- concatenate(getNextLetter(firstHexDigit),
;;                             getNextLetter(secondHexDigit))
;;     else
;;       Signal an error: "Invalid hexNumber."
;;     end if
;;     
;;     { Create the Hell69 number. }
;;     result <- replace all "0" digits in the result by "69"
;;     { Prepend the pseudo-hexadecimal prefix "69x" in order to }
;;     { produce a Hell69 token from the Hell69 number.         }
;;     result <- prepend "69x" to the result
;;     
;;     return result
;;   end function
;; 
;; Any non-empty program must begin with the sepiment or sentinel
;; "/-/31169" and terminate with the same. The intermittent content
;; contains zero or more Hell69 tokens, that is, Hell69 numbers prefixed
;; with "69x"; each two such entities must be discriminated by aid of
;; the "/-/31169" sequence.
;; 
;; == HELL69-DECODING ==
;; The procession in the withershins airt, from an extant Hell69 program
;; to plaintext, constitutes a forbisen of equiparancy. Each Hell69
;; token is subjected to the paregal treatment, a graduated experience
;; of four steps:
;;   
;;   (1) EXTRACT THE HELL69 NUMBER
;;       Segregated by adminiculum of the sentinel "/-/31169", a Hell69
;;       tokens's discernment from its surrounding does not impose a
;;       particular difficulty. Furthermore, such an entity's
;;       composition of two elements, the distorted hexadecimal prefix
;;       "69x" and the pseudo-hexadecimal digits, the latter of which
;;       form the actual Hell69 number, permit an eath excavation of
;;       this significant portion.
;;   (2) REPLACE ALL "69" SUBSEQUENCES BY ZERO (0) DIGITS
;;       With the sequence 69 being a surrogate to the natural zero (0)
;;       digit in the primordial datum, the athwart application ensues
;;       as a rational consectary: Each "69" subsequece must be replaced
;;       with a "0".
;;   (3) TRANSFORM THE DIGITS AND LETTERS
;;       During the decoding having tholed a multiplication of its
;;       digits and translation of its letters, the original hexadecimal
;;       character twain will be restored by a reversion of its
;;       erstwhile principles. In lieu of a single digit, at least a
;;       twofold augmentation has expanded the numerical segment's size,
;;       while the letters' tally remained unaffected.
;;         (3.a) If the Hell69 number consists of decimal digits only,
;;               the represented value is interpreted as a decimal
;;               number --- without conversion from base-16 to
;;               base-10 --- and divided by a decimal 69. The result
;;               amounts to a hexadecimal number of two decimal digits.
;;         (3.b) If the left part consists of decimal digits, while the
;;               dextral contains a letter, the left moeity is construed
;;               as a decimal value --- without conversion to
;;               base-10 --- and divided by a decimal 69, yielding a
;;               single decimal digit as the first compartment of the
;;               two-character hexadecimal output. The letter is
;;               replaced by the letter in the alphabet immediately
;;               preceding it. The result then consists of the sinistral
;;               decimal portion, with the surrogate letter adhibited.
;;         (3.c) If the left part consists of a letter and the right
;;               portion exposes decimal digits, the sinistral entity is
;;               substituted by the letter immediately preceding it in
;;               the alphabet, while the decimal portion is construed as
;;               a decimal number --- without conversion to base-10 ---
;;               and divided by a decimal 69, yielding a single decimal
;;               digit as the second compartment of a two-character
;;               hexadecimal output. The result then comprehends the
;;               sinistral letter extended by the decimal quotient.
;;         (3.d) If the Hell69 number occupies two letters, each of them
;;               is substituted by the letter immediately preceding it
;;               in the alphabet. The result is defined by a hexadecimal
;;               output of two letters' width.
;;   (4) The Hell69 number's transition into the acquainted hexadecimal
;;       system acts as the parasceve to the plaintext ASCII character's
;;       obtention, which is realized in converting the base-16 value
;;       to the base-10 number system, and then ultimately yielding the
;;       ASCII character associated with the decimal code.
;; 
;; The extraction of all ASCII characters from their Hell69 numbers
;; ultimately produces the plaintext sequence.
;; 
;; 
;; Architecture
;; ============
;; As much devoid as of a data storage, Hell69's architecture does not
;; impose the onus of an architectural construction upon the programmer.
;; 
;; 
;; Data Types
;; ==========
;; Hell69's dichotomy applying to its type system, and the vinculum
;; present betwixt these two participants, exists as a germination from
;; the ASCII character's complementary aspects: as character and integer
;; data, with the prior being a warkloom of presentation, and the
;; compernage its medium of internal maintenance.
;; 
;; == INTEGERS REPRESENT ASCII CODES AND HELL69 NUMBERS ==
;; The integer type's woning does not, as defines the preponderance
;; among linguistic cases, apportion excellence to the decimal aspect;
;; Hell69 assign a paravaunt significance to hexadecimal, or base-16,
;; representations. An ASCII character, when transcripted into its code,
;; partakes in the Hell69-encoding process by being molded into the
;; two-character hexadecimal form, with valid entities therein being
;; integers in the range [0, 9], and the letters from "a" to "f". The
;; corollary of the transformation process, yielding a Hell69 number,
;; extends the tally of digits, yet retaining their membership, while
;; the letter range shifts lexicographically forward to ["b", "g"]. The
;; decoding step applies to the principles a boustrophedonic motion.
;; 
;; == ASCII CHARACTERS ARE THE PRODUCTS OF DECODING ==
;; When interpreting a Hell69 program, its kenspeckle design is analyzed
;; and reconfigured into a plaintext ASCII string sequence of
;; characters, their display on the standard output forming the ultimate
;; destination.
;; 
;; 
;; Syntax
;; ======
;; Hell69 relies on a variety of the hexadecimal character set, extended
;; by such content as to provide a number marker "69x" and a separator
;; "/-/31169".
;; 
;; == HELL69 NUMBERS: A VARIETY OF HEXADECIMAL ENTITIES ==
;; Hell69's syntax constitutes a perversion of the hexadecimal system's
;; character repertoire, with the decimal digits' pristine
;; appropriation, concomitant to the letter range's translation from
;; the ordinary base-16 norm with its members {"a", "b", "c", "d", "e",
;; "f"} to the translated set {"b", "c", "d", "e", "f", "g"}. A prefix,
;; iterum as a distortion of the hexadecimal marker "0x", defines the
;; introduction of a Hell69 number.
;; 
;; == "/-/31169": A SEPARATOR ==
;; An introductory as well closing entity, additionally employed in the
;; role as a sepiment betwixt two Hell69 tokens, experiences its
;; expression in the sequence "/-/31169".
;; 
;; == WHITESPACES MAY SURROUND TOKENS ==
;; A rather liberal admission is vouchsafed to whitespaces, including in
;; the diorism the space, tab, and newline characters, and comprehend in
;; their homologation the preceding of the "/-/31169" separator and its
;; succession. A corollary of this design, their presence is embraced by
;; Hell69 tokens, that is, Hell69 digit sequences introduced by the
;; "69x" sequence. These digits and the marker, however, may not be
;; divided by any content.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) grammar defines a
;; program in this language:
;;   
;;   program      := whitespaces ,
;;                   [ hell69Token , whitespaces ,
;;                     { hell69Token , whitespaces } ,
;;                     separator , whitespaces ] ;
;;   hell69Token  := separator , whitespaces , hell69Number ;
;;   separator    := "/-/31169" ;
;;   hell69Number := "69x" , hell69Digits ;
;;   hell69Digits := hell69Digit , { hell69Digit } ;
;;   hell69Digit  := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7"
;;                |  "8" | "9" | "b" | "B" | "c" | "C" | "d" | "D"
;;                |  "e" | "E" | "f" | "F" | "g" | "G" ;
;;   whitespaces  := { whitespace } ;
;;   whitespace   := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Instructions, in any sense of this donat, elude veridical
;; explicitness. Actually, apart from the separator "/-/31169", any
;; non-whitespace token is subsumed into a command role by aid of its
;; identity, being the expression of an ASCII character in its
;; Hell69-encoded format, and intended to be printed to the standard
;; output when transliterated into the plaintext realm.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A propagation of its plain haecceity, Hell69 is inflicted with few
;; occasions of ambivalence and dubiosity only. These scarce detections
;; shall now be subjected to further disquisition.
;; 
;; == DO WHITESPACES ENJOY TOLERANCE? ==
;; With its arcane construction, a Hell69 program, maugre the humble
;; capacity installed in its functions, must be assayed as a paragon
;; of an established forbisen. A concomitant of its specification, the
;; present example, materialized in the shape of a "Hello, World!"
;; program, exhibits a linebreak --- albeit its inclusion cannot be
;; negated as a consequence of improved appearance. In consectary, the
;; admission of whitespaces, embracing spaces, tabs, and newlines as
;; most prominent exponents, remains an object of inquisition. An
;; adjudgement is hence exercised involving the tolerance of such
;; whitespace content betwixt Hell69 numbers and their separators. For
;; instance, the following variants
;;   /-/3116969x3312/-/31169
;;   /-/31169 69x3312 /-/31169
;; enjoy an allotment of tenability, whereas these examples partake of
;; no validity:
;;   /-/ 31169 69x3312/-/31169
;;   /-/31169 69x 3312/-/31169
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-04-03
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Hell69"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines the eligible conduits for output
   operations, including, among others, the ``format'' and
   ``write-char'' functions."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 8) +HELL69-SENTINEL+))

(declaim (type (simple-string 3) +HELL69-NUMBER-PREFIX+))

;;; -------------------------------------------------------

(defparameter +HELL69-SENTINEL+ "/-/31169"
  "The sequence employed to introduce, separate, and terminate a Hell69
   number.")

(defparameter +HELL69-NUMBER-PREFIX+ "69x"
  "The Hell69 number prefix \"69x\", obtained by substituting in the
   standard hexadecimal designator \"0x\" the zero (0) digit by 69,
   and utilized for denoting a Hell69 number.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Hell69 encoder.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-hexadecimal-character-code (character)
  "Returns a hexadecimal string representation of the CHARACTER's ASCII
   code, with any letters ensured to be expressed as minuscules."
  (declare (type character character))
  (the (string 2)
    (format NIL "~(~x~)" (char-code character))))

;;; -------------------------------------------------------

(defun get-next-letter (letter)
  "Returns the character following the LETTER."
  (declare (type character letter))
  (the character
    (code-char (1+ (char-code letter)))))

;;; -------------------------------------------------------

(defun multiply-hexadecimal-number-by-69 (hexadecimal-number)
  "Performs the first stage of Hell69-encoding by multiplying the
   HEXADECIMAL-NUMBER, being a string representation of a plaintext
   character's ASCII code, by the constant 69, and substituting each
   hexadecimal letter by its successor in the alphabet, thus generating
   and returning a new string without modification on the input."
  (declare (type (string 2) hexadecimal-number))
  (the string
    (with-output-to-string (hell69-number)
      (declare (type string-stream hell69-number))
      (let ((first-hexadecimal-digit  (char hexadecimal-number 0))
            (second-hexadecimal-digit (char hexadecimal-number 1)))
        (declare (type character first-hexadecimal-digit))
        (declare (type character second-hexadecimal-digit))
        
        (cond
          ;; digit, digit
          ((and (digit-char-p first-hexadecimal-digit)
                (digit-char-p second-hexadecimal-digit))
            (format hell69-number "~d"
              (* (parse-integer hexadecimal-number) 69)))
          
          ;; digit, letter
          ((digit-char-p first-hexadecimal-digit)
            (format hell69-number "~d~c"
              (* (digit-char-p first-hexadecimal-digit) 69)
              (get-next-letter second-hexadecimal-digit)))
          
          ;; letter, digit
          ((digit-char-p second-hexadecimal-digit)
            (format hell69-number "~c~d"
              (get-next-letter first-hexadecimal-digit)
              (* (digit-char-p second-hexadecimal-digit) 69)))
          
          ;; letter, letter
          (T
            (format hell69-number "~c~c"
              (get-next-letter first-hexadecimal-digit)
              (get-next-letter second-hexadecimal-digit))))))))

;;; -------------------------------------------------------

(defun replace-zeroes-by-69s (string)
  "Returns a new string by replacing all occurrences of the digit zero
   ('0') by the digits '69'.
   ---
   Please note that a fresh string instance will be allocated
   irregardless of the presence and tally of committed substitutions."
  (declare (type string string))
  (the string
    (with-output-to-string (modified-string)
      (declare (type string-stream modified-string))
      (loop for character of-type character across string do
        (format modified-string "~a"
          (if (char= character #\0)
            "69"
            character))))))

;;; -------------------------------------------------------

(defun hell69-encode-character (character &key (destination T))
  "Encodes the plaintext ASCII CHARACTER by its Hell69 number and writes
   the resulting datum to the DESTINATION, returning for a non-``NIL''
   target a value of ``NIL'', otherwise generating and returning a fresh
   string object entailing the content."
  (declare (type character   character))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (format destination "~a"
        (replace-zeroes-by-69s
          (multiply-hexadecimal-number-by-69
            (get-hexadecimal-character-code character))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (hell69-encode-character character :destination output)))))

;;; -------------------------------------------------------

(defun hell69-encode-text (plaintext
                           &key (destination   T)
                                (number-prefix "")
                                (number-suffix ""))
  "Encodes the PLAINTEXT as a Hell69 program and writes the same to the
   DESTINATION, each hell number preceded by NUMBER-PREFIX and succeeded
   by the NUMBER-SUFFIX, returning ``NIL'' if the DESTINATION is
   non-``NIL'', otherwise generating and delivering a fresh string
   containing the output.
   ---
   The NUMBER-PREFIX and NUMBER-SUFFIX, albeit not encumbered by any
   further stipulations regarding their kind and content, will be
   formatted according to the rules appertaining to the \"~a\" format
   directive."
  (declare (type string      plaintext))
  (declare (type destination destination))
  (declare (type T           number-prefix))
  (declare (type T           number-suffix))
  (the (or null string)
    (if destination
      (loop
        for
          character of-type character across plaintext
        do
          (format destination "~a" +HELL69-SENTINEL+)
          (format destination "~a" number-prefix)
          (format destination "~a" +HELL69-NUMBER-PREFIX+)
          (hell69-encode-character character :destination destination)
          (format destination "~a" number-suffix)
        finally
          (format destination "~a" +HELL69-SENTINEL+))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (hell69-encode-text plaintext
          :destination   output
          :number-prefix number-prefix
          :number-suffix number-suffix)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Hell69 decoder.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline #\Return)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun hell69-character-p (character)
  "Checks whether the CHARACTER represents a recognized Hell69 number
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (find character "0123456789bcdefg" :test #'char-equal)))))

;;; -------------------------------------------------------

(defun hell69-digit-p (character)
  "Checks whether the CHARACTER represents a recognized Hell69 number
   digit, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (find character "0123456789" :test #'char-equal)))))

;;; -------------------------------------------------------

(defun hell69-letter-p (character)
  "Checks whether the CHARACTER represents a recognized Hell69 number
   letter, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (find character "bcdefg" :test #'char-equal)))))

;;; -------------------------------------------------------

(defun get-previous-letter (letter)
  "Returns the character preceding the LETTER in the alphabet."
  (declare (type character letter))
  (the character
    (code-char (1- (char-code letter)))))

;;; -------------------------------------------------------

(defun replace-69s-by-zeroes (string)
  "Creates and returns a new string by replacing all occurrences of the
   subsequence '69' in the STRING by a single '0' each.
   ---
   Please note that a fresh string instance will be allocated
   irregardless of the presence and tally of committed substitutions."
  (declare (type string string))
  (flet ((digit-9-follows (position)
          "Checks whether the index into the STRING immediately
           succeeding the POSITION indicates the character '9',
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type fixnum position))
          (let ((next-position (1+ position)))
            (declare (type fixnum next-position))
            (the boolean
              (not (null
                (and (array-in-bounds-p string next-position)
                     (char= (char string next-position) #\9))))))))
    (the string
      (with-output-to-string (modified-string)
        (declare (type string-stream modified-string))
          (loop
            with  position of-type fixnum = 0
            while (< position (length string))
            do
              (let ((character (char string position)))
                (declare (type character character))
                (format modified-string "~a"
                  (cond
                    ;; The subsequence "69" follows?
                    ;; => Write the character "0" instead.
                    ((and (char= character #\6)
                          (digit-9-follows position))
                      (prog1
                        #\0
                        (incf position)))
                    ;; No "69" follows?
                    ;; => Transfer the CHARACTER verbatim.
                    (T
                      character))))
              (incf position))))))

;;; -------------------------------------------------------

(defun decode-Hell-number (hell-number)
  "Returns the hexadecimal number string corresponding to the Hell69
   HELL-NUMBER.
   ---
   The thus produced string represents the plaintext character's ASCII
   code in its hexadecimal form."
  (declare (type string hell-number))
  
  (let ((position  0)
        (character (char hell-number 0)))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    
    (labels
        ((advance ()
          "Moves the POSITION cursor to the next character in the
           HELL-NUMBER, if possible, updates the current CHARACTER, and
           returns no value."
          (setf character
            (when (< position (1- (length hell-number)))
              (char hell-number (incf position))))
          (values))
         
         (read-number ()
          "Starting at the current POSITION, reads a sequence of one or
           more decimal digits, parses the same, and returns the thus
           represented decimal number."
          (the (integer 0 *)
            (parse-integer
              (with-output-to-string (digits)
                (declare (type string-stream digits))
                (loop
                  while (and character (hell69-digit-p character))
                  do
                    (write-char character digits)
                    (advance))))))
         
         (read-letter ()
          "Reads a single Hell69 letter, that is, a letter from the set
           { 'b', 'c', 'd', 'e', 'f', 'g'  }, and returns it."
          (the character
            (prog1
              character
              (advance)))))
      
      (the (string 2)
        (with-output-to-string (hexadecimal-number)
          (declare (type string-stream hexadecimal-number))
          (loop while character do
            (cond
              ;; A sequence of one or more decimal digits follows.
              ;; It must be divided by the special value 69 in order to
              ;; transcript from Hell69's system to the usual number
              ;; system.
              ((hell69-digit-p character)
                (format hexadecimal-number "~d"
                  (/ (read-number) 69)))
              
              ;; A Hell69 letter ("b" through "g") follows.
              ;; It must be replaced by the preceding letter from the
              ;; alphabet in order to map to the traditional hexadecimal
              ;; range of "a" through "f".
              ((hell69-letter-p character)
                (format hexadecimal-number "~a"
                  (get-previous-letter
                    (read-letter))))
              
              ;; Any other character is inflicted with proscription.
              (T
                (error "Invalid character '~c' at position ~d."
                  character position)))))))))

;;; -------------------------------------------------------

(defun extract-Hell-numbers (code)
  "Extracts and returns from the Hell69 CODE a list of the Hell69
   numbers, each represented by a string and cleared of the prefix
   \"69x\"."
  (declare (type string code))
  
  (let ((hell-numbers    NIL)
        (normalized-code (remove-if #'whitespace-character-p code)))
    (declare (type string           normalized-code))
    (declare (type (list-of string) hell-numbers))
    
    (when (plusp (length normalized-code))
      (let ((position  0)
            (character (char normalized-code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character in the
               CODE, if possible, updates the current CHARACTER, and
               returns no value."
              (setf character
                (when (< position (1- (length normalized-code)))
                  (char normalized-code (incf position))))
              (values))
             
             (expect-string (expected-string)
              "Starting at the current POSITION, matches the CODE
               characters against the EXPECTED-STRING while advancing
               the POSITION cursor, returning on ascertainment a
               ``boolean'' value ``T'', otherwise ``NIL''."
              (declare (type string expected-string))
              (the boolean
                (loop
                  for expected-character
                    of-type character
                    across expected-string
                  do
                    (if (and character
                             (char-equal character expected-character))
                      (advance)
                      (return NIL))
                  finally
                    (return T))))
             
             (expect-sentinel ()
              "Starting at the current POSITION, matches the CODE
               characters against the Hell69 sentinel and separator
               token '/-/31169', returning on ascertainment no value,
               otherwise signaling an error."
              (unless (expect-string +HELL69-SENTINEL+)
                (error "Missing Hell69 sentinel \"~a\"."
                  +HELL69-SENTINEL+))
              (values))
             
             (expect-hexadecimal-prefix ()
              "Starting at the current POSITION, matches the CODE
               characters against the Hell69 number prefix '69x',
               returning on ascertainment no value, otherwise signaling
               an error."
              (unless (expect-string +HELL69-NUMBER-PREFIX+)
                (error "Missing Hell69 hexadecimal prefix \"~a\"."
                  +HELL69-NUMBER-PREFIX+))
              (values))
             
             (read-hell69-number ()
              "Starting at the current POSITION, reads a Hell69 number,
               prepares it by replacing all occurrences of the
               subsequence '69' by zeroes, and returns it as a string."
              (the string
                (replace-69s-by-zeroes
                  (with-output-to-string (digits)
                    (declare (type string-stream digits))
                    (loop
                      while (and character
                                 (hell69-character-p character))
                      do
                        (write-char character digits)
                        (advance)))))))
          
          (loop while character do
            (expect-sentinel)
            (cond
              (character
                (expect-hexadecimal-prefix)
                (push (read-hell69-number) hell-numbers))
              (T
                (loop-finish)))))))
    
    (the (list-of string) (nreverse hell-numbers))))

;;; -------------------------------------------------------

(defun hell69-decode-number (hell-number &key (destination T))
  "Decodes the HELL-NUMBER, reproducing the corresponding plaintext
   ASCII character, writes it to the DESTINATION, and returns ``NIL''
   for a non-``NIL'' DESTINATION, otherwise producing a fresh string
   containing the decoded entity."
  (declare (type string      hell-number))
  (declare (type destination destination))
  (the (or null string)
    (format destination "~c"
      (code-char
        (parse-integer
          (decode-Hell-number hell-number) :radix 16)))))

;;; -------------------------------------------------------

(defun hell69-decode-text (code &key (destination T))
  "Decodes the Hell69 CODE, reproducing the entailed plaintext ASCII
   characters, writes them to the DESTINATION, and returns ``NIL'' for a
   non-``NIL'' DESTINATION, otherwise producing a fresh string
   containing the decoded content."
  (declare (type string      code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((hell-numbers (extract-Hell-numbers code)))
        (declare (type (list-of string) hell-numbers))
        (dolist (hell-number hell-numbers)
          (declare (type string hell-number))
          (hell69-decode-number hell-number :destination destination)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (hell69-decode-text code :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate the Hell69 code to print "Hello, World!"
;; 
;;   /-/31169
;;   69x3312/-/31169
;;   69x4485/-/31169
;;   69x414d/-/31169
;;   69x414d/-/31169
;;   69x414g/-/31169
;;   69x138d/-/31169
;;   69x13869/-/31169
;;   69x3933/-/31169
;;   69x414g/-/31169
;;   69x4968/-/31169
;;   69x414d/-/31169
;;   69x4416/-/31169
;;   69x1449/-/31169
;; 
;; and print it to the standard output, separating each Hell69 number
;; by a newline from its "/-/31169" sentinel.
;; 
(hell69-encode-text "Hello, World!"
  :destination   T
  :number-prefix #\Newline)

;;; -------------------------------------------------------

;; Generate the Hell69 code to print "Hello, World!"
;; 
;;   /-/31169
;;   69x3312/-/31169
;;   69x4485/-/31169
;;   69x414d/-/31169
;;   69x414d/-/31169
;;   69x414g/-/31169
;;   69x138d/-/31169
;;   69x13869/-/31169
;;   69x3933/-/31169
;;   69x414g/-/31169
;;   69x4968/-/31169
;;   69x414d/-/31169
;;   69x4416/-/31169
;;   69x1449/-/31169
;; 
;; write it into a dynamic string, and print the string's content to the
;; standard output, separating each Hell69 number by a newline from its
;; "/-/31169" sentinel.
;; 
(let ((output-string
        (make-array 0
          :element-type 'character
          :adjustable   T
          :fill-pointer 0)))
  (declare (type string output-string))
  (hell69-encode-text "Hello, World!"
    :destination   output-string
    :number-prefix #\Newline)
  (format T "~&~a" output-string))

;;; -------------------------------------------------------

;; Print each plaintext character constituting the text "Hello, World!"
;; in conjunction with its Hell69 number equivalent.
(loop
  for plaintext-character of-type character across "Hello, World!"
  do
    (format T "~&~c = ~s" plaintext-character
      (hell69-encode-character plaintext-character :destination NIL)))

;;; -------------------------------------------------------

;; Generate the Hell69 code to print "Hello, World!"
;; 
;;   /-/31169
;;   69x3312/-/31169
;;   69x4485/-/31169
;;   69x414d/-/31169
;;   69x414d/-/31169
;;   69x414g/-/31169
;;   69x138d/-/31169
;;   69x13869/-/31169
;;   69x3933/-/31169
;;   69x414g/-/31169
;;   69x4968/-/31169
;;   69x414d/-/31169
;;   69x4416/-/31169
;;   69x1449/-/31169
;; 
;; and interpret it.
;; 
(hell69-decode-text
  (hell69-encode-text "Hello, World!" :destination NIL))
