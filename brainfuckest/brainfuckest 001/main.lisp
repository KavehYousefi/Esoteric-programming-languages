;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainfuckest", invented by the Esolang user "Nurdle" and
;; presented on December 14th, 2022, its conception an encoding of
;; Urban Mueller's "brainfuck" language in a set of 69 characters which
;; produce by a series of arithmetic transformations the intended
;; brainfuck equivalent.
;; 
;; 
;; Concept
;; =======
;; The brainfuckest programming language's firmament is edified upon a
;; derivation from brainfuck, merely kenspeckle and dioristic in its
;; deployment of an encoding scheme thilk converts a sequence of
;; characters via a certain formula --- involving everyone's nykin, the
;; number 69 in both the character repertoire's enumeration, as well as
;; the arithmetic supputations --- into a tantamount brainfuck program.
;; 
;; == BRAINFUCKEST: A BRAINFUCK ENCODING SCHEME ==
;; The dioristic contribution partaken of by this language constitutes
;; an encoding scheme applied to a brainfuck program, whence issues the
;; requisitum of its original state's extraction in the face of its
;; execution as the ultimity.
;; 
;; Two moeities shall engage in a champarty as a combined prevenience to
;; the treatise on the actual brainfuckest-to-brainfuck decoding, and,
;; as an enacted supererogation, its athwart transformation: the
;; kenspeckle character repertoire and the brainfuck symbols' octal
;; coding.
;; 
;; == BRAINFUCKEST EMPLOYS A CUSTOM CHARACTER REPERTOIRE AND CODING ==
;; Brainfuckest character repertoire establishes a subset of the ASCII
;; standard, the circumference of its deviation extending into
;; kenspeckle second facette in the appropriation of bespoke character
;; codes.
;; 
;; Cognate in its affiliation, brainfuckest deploys integral numbers
;; commencing from the inclusive minimum of zero (0) for the character
;; codes, conflating, however, in no case with the ASCII definitions.
;; 
;; The following table's bailiwick shall be realized in the brainfuckest
;; characters' equiparation with the language's private codes,
;; located in the compernage of the ASCII choice:
;; 
;;   -------------------------------------------
;;   Character | brainfuckest code | ASCII code
;;   ----------+-------------------+------------
;;   0         | 0                 | 48
;;   ...........................................
;;   1         | 1                 | 49
;;   ...........................................
;;   2         | 2                 | 50
;;   ...........................................
;;   3         | 3                 | 51
;;   ...........................................
;;   4         | 4                 | 52
;;   ...........................................
;;   5         | 5                 | 53
;;   ...........................................
;;   6         | 6                 | 54
;;   ...........................................
;;   7         | 7                 | 55
;;   ...........................................
;;   8         | 8                 | 56
;;   ...........................................
;;   9         | 9                 | 57
;;   ...........................................
;;   q         | 10                | 113
;;   ...........................................
;;   w         | 11                | 119
;;   ...........................................
;;   e         | 12                | 101
;;   ...........................................
;;   r         | 13                | 114
;;   ...........................................
;;   t         | 14                | 116
;;   ...........................................
;;   y         | 15                | 121
;;   ...........................................
;;   u         | 16                | 117
;;   ...........................................
;;   i         | 17                | 105
;;   ...........................................
;;   o         | 18                | 111
;;   ...........................................
;;   p         | 19                | 112
;;   ...........................................
;;   a         | 20                | 97
;;   ...........................................
;;   s         | 21                | 115
;;   ...........................................
;;   d         | 22                | 100
;;   ...........................................
;;   f         | 23                | 102
;;   ...........................................
;;   g         | 24                | 103
;;   ...........................................
;;   h         | 25                | 104
;;   ...........................................
;;   j         | 26                | 106
;;   ...........................................
;;   k         | 27                | 107
;;   ...........................................
;;   l         | 28                | 108
;;   ...........................................
;;   z         | 29                | 122
;;   ...........................................
;;   x         | 30                | 120
;;   ...........................................
;;   c         | 31                | 99
;;   ...........................................
;;   v         | 32                | 118
;;   ...........................................
;;   b         | 33                | 98
;;   ...........................................
;;   n         | 34                | 110
;;   ...........................................
;;   m         | 35                | 109
;;   ...........................................
;;   Q         | 36                | 81
;;   ...........................................
;;   W         | 37                | 87
;;   ...........................................
;;   E         | 38                | 69
;;   ...........................................
;;   R         | 39                | 82
;;   ...........................................
;;   T         | 40                | 84
;;   ...........................................
;;   Y         | 41                | 89
;;   ...........................................
;;   U         | 42                | 85
;;   ...........................................
;;   I         | 43                | 73
;;   ...........................................
;;   O         | 44                | 79
;;   ...........................................
;;   P         | 45                | 80
;;   ...........................................
;;   A         | 46                | 65
;;   ...........................................
;;   S         | 47                | 83
;;   ...........................................
;;   D         | 48                | 68
;;   ...........................................
;;   F         | 49                | 70
;;   ...........................................
;;   G         | 50                | 71
;;   ...........................................
;;   H         | 51                | 72
;;   ...........................................
;;   J         | 52                | 74
;;   ...........................................
;;   K         | 53                | 75
;;   ...........................................
;;   L         | 54                | 76
;;   ...........................................
;;   Z         | 55                | 90
;;   ...........................................
;;   X         | 56                | 88
;;   ...........................................
;;   C         | 57                | 67
;;   ...........................................
;;   V         | 58                | 86
;;   ...........................................
;;   B         | 59                | 66
;;   ...........................................
;;   N         | 60                | 78
;;   ...........................................
;;   M         | 61                | 77
;;   ...........................................
;;   (space)   | 62                | 32
;;   ...........................................
;;   ,         | 63                | 44
;;   ...........................................
;;   .         | 64                | 46
;;   ...........................................
;;   ?         | 65                | 63
;;   ...........................................
;;   !         | 66                | 33
;;   ...........................................
;;   ;         | 67                | 59
;;   ...........................................
;;   :         | 68                | 58
;;   -------------------------------------------
;; 
;; == OCTAL DIGITS ENCODE BRAINFUCK INSTRUCTIONS ==
;; Each of the octuple brainfuck digits, is ligated into the alliance
;; with exactly on octal digit as its encoding paregal:
;; 
;;   -----------------------------------
;;   Octal code | brainfuck instruction
;;   -----------+-----------------------
;;   0          | >
;;   ...................................
;;   1          | <
;;   ...................................
;;   2          | +
;;   ...................................
;;   3          | -
;;   ...................................
;;   4          | .
;;   ...................................
;;   5          | ,
;;   ...................................
;;   6          | [
;;   ...................................
;;   7          | ]
;;   -----------------------------------
;; 
;; == DECODING: BRAINFUCKEST TO BRAINFUCK ==
;; The cynosure of all efforts applicable to the execution of a
;; brainfuckest program appertain to the conversion of thilk into the
;; actually encoded brainfuck source code.
;; 
;; The following listing shall serve as an exhaustive treatise on the
;; subject, ere further elucidations, contributed in a graphical as well
;; as a pseudocode guise, shall augment the furnishment:
;; 
;;   (1) BRAINFUCKEST CHARACTER CODE EXTRACTION
;;       Each symbol in the brainfuckest program code is converted into
;;       its zero-based character code, an integral object desumed from
;;       the closed interval [0, 68], and explicated in the table under
;;       the section "BRAINFUCKEST EMPLOYS A CUSTOM CHARACTER REPERTOIRE
;;       AND CODING", which please consult.
;;   
;;   (2) DECIMAL PROGRAM CODE GENERATION:
;;       The character codes are combined utilizing a peculiar formula
;;       into a single non-negative integer number representing the
;;       brainfuckest program in a decimal form.
;;       
;;       Given the character codes c[i], with 0 <= i < n, where n is
;;       tantamount to the tally of extracted character codes, the
;;       decimal sum d is supputated as:
;;       
;;         d =   c[0]   + (69 * 0)
;;             + c[1]   + (69 * 1)
;;             + ...
;;             + c[i]   + (69 * i)
;;             + ...
;;             + c[n-1] + (69 * (n-1))
;;   
;;   (3) OCTAL PROGRAM CODE REPRESENTATION:
;;       The decimal program code d, obtained in the preveninent stage,
;;       is converted into its base-8 numerical representation.
;;   
;;   (4) OCTAL DIGITS DECODING TO BRAINFUCK SYMBOLS
;;       Each octal digit in the numerical brainfuckest program
;;       representation is substituted by its affiliated brainfuck
;;       instruction token, as presented in the table under the section
;;       "OCTAL DIGITS ENCODE BRAINFUCK INSTRUCTIONS", which please
;;       peruse, thus gaining the desiderated brainfuck program code.
;;       
;;       Given the octal program representation P = (p[1], ..., p[m]),
;;       where p[j] represents the j-th octal digit, with 1 <= j <= m,
;;       and m being tantamount to the tally of octal digits comprising
;;       P, each p[j] is substituted by the respective brainfuck symbol.
;; 
;; A graphical illustration shall limn the transformation process
;; from a piece of brainfuckest source code into a brainfuck equivalent
;; in a more aesthetically pleasing guise:
;; 
;;   +---------------------------------------+
;;   |           brainfuckest code           |
;;   +---------------------------------------+
;;                       |
;;                       | Collect each character's braifuckest
;;                       | character code.
;;                       |
;;                       V
;;   +---------------------------------------+
;;   | list of brainfuckest character codes  |
;;   +---------------------------------------+
;;                       |
;;                       | For i from 0 below the number of character
;;                       | codes, with c[i] being the i-th such code,
;;                       | supputate the term
;;                       |   d[i] = c[i] + (69 * i),
;;                       | and sum all d[i] into the decimal code.
;;                       V
;;   +---------------------------------------+
;;   |             decimal code              |
;;   +---------------------------------------+
;;                       |
;;                       | Convert into a base-8 integer number.
;;                       |
;;                       V
;;   +---------------------------------------+
;;   |              octal code               |
;;   +---------------------------------------+
;;                       |
;;                       | Collect for each octal digit the
;;                       | affiliated brainfuck instruction symbol.
;;                       |
;;                       V
;;   +---------------------------------------+
;;   |     brainfuck instruction symbols     |
;;   +---------------------------------------+
;; 
;; A higher mete of formality's furnishment shall be exercised in the
;; following pseudocode treatise:
;; 
;;   let brainfuckestRepertoire <- ("0", "1", "2", "3", "4", "5", "6",
;;                                  "7", "8", "9", "q", "w", "e", "r",
;;                                  "t", "y", "u", "i", "o", "p", "a",
;;                                  "s", "d", "f", "g", "h", "j", "k",
;;                                  "l", "z", "x", "c", "v", "b", "n",
;;                                  "m", "Q", "W", "E", "R", "T", "Y",
;;                                  "U", "I", "O", "P", "A", "S", "D",
;;                                  "F", "G", "H", "J", "K", "L", "Z",
;;                                  "X", "C", "V", "B", "N", "M", " ",
;;                                  ",", ".", "?", "!", ";", ":")
;;   
;;   
;;   function getBrainfuckestCharCode (character)
;;     Input:
;;       character:     The character whose zero-based brainfuckest
;;                      character code shall be retrieved.
;;     
;;     Output:
;;       characterCode: The zero-based character code associated with
;;                      the CHARACTER by brainfuckest. This value
;;                      constitutes a member of the closed integral
;;                      interval [0, 68].
;;     
;;     Process:
;;       let characterCode <- nil
;;       
;;       characterCode <- zero-based index in brainfuckestRepertoire
;;       
;;       return characterCode
;;   end function
;;   
;;   
;;   function decodeOctalDigit (octalDigit)
;;     Input:
;;       octalDigit:    The base-8 digit whose associated brainfuck
;;                      instruction symbol shall be retrieved. This
;;                      digit constitutes an integral number desumed
;;                      from the closed interval [0, 7].
;;     
;;     Output:
;;       bfInstruction: The brainfuck instruction symbol corresponding
;;                      to the OCTAL_DIGIT.
;;     
;;     Process:
;;       bfInstruction <- nil
;;       
;;       if octalDigit = 0 then
;;         bfInstruction <- ">"
;;       else if octalDigit = 1 then
;;         bfInstruction <- "<"
;;       else if octalDigit = 2 then
;;         bfInstruction <- "+"
;;       else if octalDigit = 3 then
;;         bfInstruction <- "-"
;;       else if octalDigit = 4 then
;;         bfInstruction <- "."
;;       else if octalDigit = 5 then
;;         bfInstruction <- ","
;;       else if octalDigit = 6 then
;;         bfInstruction <- "["
;;       else if octalDigit = 7 then
;;         bfInstruction <- "]"
;;       else
;;         error: Invalid octal digit
;;       end if
;;       
;;       return octalDigit
;;   end function
;;   
;;   
;;   function decodeBrainfuckest (program)
;;     Input:
;;       program:       The brainfuckest program to convert into a
;;                      brainfuck equivalent.
;;     
;;     Output:
;;       brainfuckCode: A list of brainfuck instruction symbols
;;                      corresponding to the brainfuckest PROGRAM's
;;                      functionality.
;;     
;;     Process:
;;       let brainfuckCode <- prepare empty list
;;       let decimalCode   <- 0
;;       let octalCode     <- 0
;;       
;;       for position from 0 to (length(program) - 1) do
;;         let encodedChar <- program[position]
;;         let charCode    <- getBrainfuckestCharCode(encodedChar)
;;         let addend      <- charCode + (69 * position)
;;         
;;         decimalCode     <- decimalCode + addend
;;       end for
;;       
;;       octalCode <- convert decimalCode into base-8 number
;;       
;;       for each octalDigit in octalCode do
;;         let brainfuckInstruction <- decodeOctalDigit(octalDigit)
;;         append brainfuckInstruction to brainfuckCode
;;       end for
;;       
;;       return brainfuckCode
;;   end function
;; 
;; == ENCODING: BRAINFUCK TO BRAINFUCKEST ==
;; The withershins airt of conversion, yielding the a brainfuckest
;; program as the produce of a brainfuck provenance, shall be limned in
;; a pseudocode guise.
;; 
;; The function "extractEncodingInformation" serves in the extraction
;; of the requisite data for the encoding process, including the numeric
;; value to cover by the brainfuckest symbols' character codes; while
;; the operation "encodeBrainfuckProgram" is assigned the bailiwick of
;; a simple brainfuckest code string's assemblage, derived from the
;; prevenient stage's information.
;; 
;;   let brainfuckestRepertoire <- ("0", "1", "2", "3", "4", "5", "6",
;;                                  "7", "8", "9", "q", "w", "e", "r",
;;                                  "t", "y", "u", "i", "o", "p", "a",
;;                                  "s", "d", "f", "g", "h", "j", "k",
;;                                  "l", "z", "x", "c", "v", "b", "n",
;;                                  "m", "Q", "W", "E", "R", "T", "Y",
;;                                  "U", "I", "O", "P", "A", "S", "D",
;;                                  "F", "G", "H", "J", "K", "L", "Z",
;;                                  "X", "C", "V", "B", "N", "M", " ",
;;                                  ",", ".", "?", "!", ";", ":")
;;   
;;   
;;   function getBrainfuckestCharacter (characterCode)
;;     Input:
;;       characterCode:    The zero-based character code of the
;;                         brainfuckest character to request. The
;;                         admissible range is specified as:
;;                           0 <= characterCode <= 68. 
;;     
;;     Output:
;;       characterForCode: The character from the
;;                         BRAINFUCKEST_REPERTOIRE amenable to the
;;                         CHARACTER_CODE.
;;     
;;     Process:
;;       let characterForCode <- brainfuckestRepertoire(characterCode)
;;       
;;       return characterForCode
;;   end function
;;   
;;   
;;   function extractEncodingInformation (decimalProgramCode)
;;     Input:
;;       decimalProgramCode:
;;     
;;     Output:
;;       numberOfPositions: The number of symbols which will comprise
;;                          the resulting brainfuckest program as an
;;                          encoding of the original brainfuck code
;;                          communicated in the DECIMAL_PROGRAM_CODE.
;;       aliquotPart:       The "fixed" part of the
;;                          DECIMAL_PROGRAM_CODE, that is, given the
;;                          NUMBER_OF_POSITIONS n, the sum
;;                            (69 * i),
;;                          for 0 <= i <= n.
;;                          This portion cannot be utilized to generate
;;                          the encoding brainfuckest symbols.
;;       aliquantPart:      The "variable" or character part of the
;;                          DECIMAL_PROGRAM_CODE, that it, the amount
;;                          not covered by the ALIQUOT_PART and thus
;;                          required to be filled with the sum of
;;                          encoding brainfuckest character codes in
;;                          order to match the entire
;;                          DECIMAL_PROGRAM_CODE.
;;                          It holds:
;;                            decimalProgramCode =   aliquotPart
;;                                                 + aliquantpart
;;     
;;     Process:
;;       let numberOfPositions <- 1
;;       let aliquotPart       <- 0
;;       let aliquantPart      <- 0
;;       let currentPosition   <- 0
;;       let replicatedCode    <- 0
;;       
;;       repeat do
;;         aliquotPart     <- replicatedCode
;;         
;;         currentPosition <- currentPosition + 1
;;         replicatedCode  <- replicatedCode + (69 * currentPosition)
;;         
;;         if replicatedCode < decimalProgramCode then
;;           terminate loop
;;         else
;;           numberOfPosition <- numberOfPositions + 1
;;         end if
;;       end repeat
;;       
;;       aliquantPart <- decimalProgramCode - aliquotPart
;;       
;;       return (numberOfPositions, aliquotPart, aliquantPart)
;;   end function
;;   
;;   
;;   function encodeBrainfuckProgram (numberOfPositions,
;;                                    aliquotPart,
;;                                    aliquantPart)
;;     Input:
;;       numberOfPositions:   The number of symbols comprising the
;;                            brainfuckest code which encodes the
;;                            original brainfuck program.
;;       aliquotPart:         The part of the original brainfuck
;;                            program's representation as a decimal
;;                            integer number that is occupied by the
;;                            "fixed" multiples of 69, and thus is
;;                            eloigned from the encoding in brainfuckest
;;                            symbols.
;;       aliquantPart:        The part of the original brainfuck
;;                            program's representation as a decimal
;;                            integer number that is reserved for the
;;                            encoding brainfuckest symbols, expected to
;;                            replicate the complete decimal value by
;;                            a summation with the ALIQUOT_PART.
;;     
;;     Output:
;;       brainfuckestProgram: A string composed of NUMBER_OF_POSITIONS
;;                            characters from the brainfuckest
;;                            repertoire which represents an encoding
;;                            of the brainfuck program specified via the
;;                            NUMBER_OF_POSITIONS, ALIQUOT_PART, and
;;                            ALIQUANT_PART.
;;     
;;     Process:
;;       let brainfuckestProgram <- prepare empty string
;;       let remainingAliquant   <- aliquantPart
;;       
;;       repeat numberOfPositions times do
;;         let nextCharCode <- min(68, remainingAliquant)
;;         let encodingChar <- getBrainfuckestCharacter(nextCharCode)
;;         
;;         append encodingChar to brainfuckestProgram
;;         
;;         remainingAliquant <- remainingAliquant - nextCharCode
;;       end repeat
;;       
;;       return brainfuckestProgram
;;   end function
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of brainfuckest's recipiency does not elude brainfuck's
;; architecture, appropriating in an ipsissima verba fashion a
;; bilaterally bourneless dispansion of unsigned byte-valued cells.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; 
;; Instructions
;; ============
;; Its status as a mere encoding of brainfuck entalents the brainfuckest
;; programming language with an equipollence to the entheus meted to
;; patration; in corollary, this cleronomy accounts for an octuple
;; contingency, amplecting in its compass the cell pointer movement,
;; basic arithmetics, input and output facilities, as well as an aefauld
;; construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the brainfuckest programming language's
;; competences, thilk concomitantly concur with the offerings of its
;; brainfuck stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   +       | Increments the current cell value by one (1). If the new
;;           | state transgresses the upper march of 255, the value
;;           | wraps around to the lower extremum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1). If the new
;;           | state transgresses the lower march of zero (0), the
;;           | value wraps around to the upper extremum of 255.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output conduit.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand has been implemented in the programming
;; language Common Lisp, its mode of accompassing operative value
;; directly targeting the brainfuck code string obtained via a
;; compilation from the brainfuckest provenance.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-06-11
;; 
;; Sources:
;;   [esolang2022Brainfuckest]
;;   The Esolang contributors, "Brainfuckest", December 14th, 2022
;;   URL: "https://esolangs.org/wiki/Brainfuckest"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   enhalsing in its diorism, among other specimens, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, everichon partaking of this membership adhering to the
   ELEMENT-TYPE, thilk defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (probed-element)
                  (declare (type T probed-element))
                  (typep probed-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, for both holds the generic
   sentinel ``*'' as a default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Brainfuckest-Error (error)
  ()
  (:documentation
    "The ``Brainfuck-Error'' condition type establishes a common
     substratum for all conditions whose actuation ensues from an
     anomalous encheson during a brainfuckest program's analyzation,
     encoding, decoding, or execution."))

;;; -------------------------------------------------------

(define-condition Invalid-Character-Error (Brainfuckest-Error)
  ((offending-character
    :initarg       :offending-character
    :initform      (error "Missing offending character.")
    :reader        invalid-character-error-offending-character
    :documentation "The character whose absence from the brainfuckest
                    repertoire has instigated this error.")
   (position
    :initarg       :position
    :initform      NIL
    :reader        invalid-character-error-position
    :type          (or null fixnum)
    :documentation "An optional specification of the zero-based position
                    into the SOURCE at which the OFFENDING-CHARACTER was
                    encountered.")
   (source
    :initarg       :source
    :initform      NIL
    :reader        invalid-character-error-source
    :type          (or null string)
    :documentation "An optional specification of the source inwith the
                    same the OFFENDING-CHARACTER was encountered."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Character-Error condition))
      (declare (type destination             stream))
      (format stream
        "The character \"~c\", located at the position ~d in ~
         the string ~s, does not represent a valid member ~
         of the brainfuckest character repertoire."
        (invalid-character-error-offending-character condition)
        (invalid-character-error-position            condition)
        (invalid-character-error-source              condition))))
  (:documentation
    "The ``Invalid-Character-Error'' condition type serves in the
     communication of an anomalous situation whose etiology resides in
     the attempt to decode a character not comprehended in the
     brainfuckest repertoire."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean equivalent thereof, returning for a
   non-``NIL'' input with a ``boolean'' value of ``T''; while responding
   for a ``NIL'' OBJECT with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-simple-base-string (source)
  "Converts the SOURCE into a simple base string and returns thilk."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))

;;; -------------------------------------------------------

(defun concatenate-into-simple-base-string (&rest tmemata)
  "Concatenates the list of TMEMATA strings into a single simple base
   string and returns thilk."
  (declare (type (list-of string) tmemata))
  (the simple-base-string
    (apply #'concatenate 'simple-base-string tmemata)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of number base conversion operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-integer-to-octal-string (integer-number)
  "Converts the decimal INTEGER-NUMBER into its octal tantamount and
   returns thilk as a simple base string."
  (declare (type integer integer-number))
  (the simple-base-string
    (convert-to-simple-base-string
      (format NIL "~8r" integer-number))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of character repertoire.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 69) +CHARACTER-CODE-TABLE+))

;;; -------------------------------------------------------

(defparameter +CHARACTER-CODE-TABLE+
  (concatenate-into-simple-base-string
    "0123456789qwertyuiopasdfghjklzxcvbnm"
    "QWERTYUIOPASDFGHJKLZXCVBNM ,.?!;:")
  "Associates the valid brainfuckest characters with their zero-based
   indices.")

;;; -------------------------------------------------------

(defun get-brainfuckest-character-code (character)
  "Returns the brainfuckest code associated with the CHARACTER, which
   concomitantly conflates with its zero-based index into the
   +CHARACTER-CODE-TABLE+."
  (declare (type character character))
  (the (integer 0 68)
    (or (position character +CHARACTER-CODE-TABLE+ :test #'char=)
        (error 'Invalid-Character-Error
          :offending-character character))))

;;; -------------------------------------------------------

(defun get-brainfuckest-character-by-code (code)
  "Returns the character from the brainfuckest repertoire amenable to
   the zero-based character code as specified by the same language."
  (declare (type (integer 0 68) code))
  (the standard-char
    (schar +CHARACTER-CODE-TABLE+ code)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of octal decoding table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 8) +OCTAL-TABLE+))

;;; -------------------------------------------------------

(defparameter +OCTAL-TABLE+
  (convert-to-simple-base-string "><+-.,[]")
  "Affiliates the octuple brainfuck instruction identifiers with
   octal digits, that is, integral numbers desumed from the closed
   interval [0, 7], by mediation of the symbols' zero-based position
   in the string.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of octal decoding operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-code-to-octal-string (brainfuckest-code)
  "Generates and returns a fresh simple base string which represents the
   piece of BRAINFUCKEST-CODE in its octal number form."
  (declare (type string brainfuckest-code))
  (the simple-base-string
    (convert-integer-to-octal-string
      (loop
        for current-index
          of-type fixnum
          from    0
          below   (length brainfuckest-code)
        and current-token
          of-type character
          across  brainfuckest-code
        sum
          (handler-case
              (+ (get-brainfuckest-character-code current-token)
                 (* 69 current-index))
            (error ()
              (error 'Invalid-Character-Error
                :offending-character current-token
                :position            current-index
                :source              brainfuckest-code)))))))

;;; -------------------------------------------------------

(defun decode-octal-digit (octal-digit)
  "Returns for the OCTAL-DIGIT the corresponding brainfuck instruction
   token."
  (declare (type (integer 0 7) octal-digit))
  (the standard-char
    (schar +OCTAL-TABLE+ octal-digit)))

;;; -------------------------------------------------------

(defun decode-octal-character (octal-character)
  "Returns for the OCTAL-CHARACTER, this constituting an octal digit's
   character representation, the corresponding brainfuck instruction
   token."
  (declare (type standard-char octal-character))
  (the standard-char
    (decode-octal-digit
      (digit-char-p octal-character 8))))

;;; -------------------------------------------------------

(defun decode-octal-string (octal-string &key (destination NIL))
  "Generates the brainfuck tokens corresponding to the OCTAL-STRING's
   entailed digits and writes these to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh simple base string comprehending
   the result."
  (declare (type simple-base-string octal-string))
  (declare (type destination        destination))
  (the (or null simple-base-string)
    (if destination
      (loop for octal-digit of-type standard-char across octal-string do
        (format destination "~c"
          (decode-octal-character octal-digit)))
      (convert-to-simple-base-string
        (with-output-to-string (brainfuck-code)
          (declare (type string-stream brainfuck-code))
          (decode-octal-string octal-string
            :destination brainfuck-code))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of decoder from brainfuckest to brainfuck.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-brainfuckest (brainfuckest-code &key (destination NIL))
  "Converts the piece of BRAINFUCKEST-CODE into a tantamount brainfuck
   program, writes thilk to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh simple base string comprehending
   the output."
  (declare (type string      brainfuckest-code))
  (declare (type destination destination))
  (the (or null simple-base-string)
    (decode-octal-string
      (convert-code-to-octal-string brainfuckest-code)
      :destination destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck character operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-instruction-character-p (candidate)
  "Determines whether the CANDIDATE represents a brainfuck instruction
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate +OCTAL-TABLE+ :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck string operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-non-brainfuck-characters (source)
  "Removes from the SOURCE any character not associated with a brainfuck
   instruction and returns a fresh simple base string representation of
   the purged result."
  (declare (type string source))
  (the simple-base-string
    (convert-to-simple-base-string
      (remove-if-not #'brainfuck-instruction-character-p source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of octal encoding operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encode-brainfuck-symbol-as-octal-digit (brainfuck-symbol)
  "Returns the octal digit associated with the BRAINFUCK-SYMBOL as an
   integer number desumed from the closed interval [0, 7]; or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type standard-char brainfuck-symbol))
  (the (integer 0 7)
    (or (position brainfuck-symbol +OCTAL-TABLE+ :test #'char=)
        (error "No octal digit is associated with the brainfuck ~
                symbol \"~c\"."
          brainfuck-symbol))))

;;; -------------------------------------------------------

(defun encode-brainfuck-code-as-octal-string (brainfuck-code)
  "Encodes the piece of BRAINFUCK-CODE as a sequence of octal digits and
   returns the result as a fresh simple base string."
  (declare (type simple-base-string brainfuck-code))
  (the simple-base-string
    (convert-to-simple-base-string
      (with-output-to-string (octal-string)
        (declare (type string-stream octal-string))
        (loop
          for current-token
            of-type standard-char
            across  brainfuck-code
          do
            (format octal-string "~d"
              (encode-brainfuck-symbol-as-octal-digit
                current-token)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of encoding components extraction operations. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-encoding-components (decimal-program-code)
  "Analyzes the DECIMAL-PROGRAM-CODE, a representation of a brainfuck
   program, translated into octal digits, concatenated, and parsed into
   a decimal integer number, and returns its components as three values:
     (1) The number of characters required to represent the
         DECIMAL-PROGRAM-CODE in its brainfuckest encoding.
     (2) The aliquot component, that is, that part of the
         DECIMAL-PROGRAM-CODE covered by the multiples of the number
         69. This value is always less than or equal to the
         DECIMAL-PROGRAM-CODE, never greater.
     (3) The aliquant component, that is, that part of the
         DECIMAL-PROGRAM-CODE not covered by the multiples of the number
         69, and thus required to be contributed by a sum of character
         codes specified by the brainfuckest standard's repertoire. This
         value is always less than or equal to the DECIMAL-PROGRAM-CODE,
         never greater."
  (declare (type     (integer 0 *) decimal-program-code))
  (declare (optimize (speed  3)
                     (safety 0)))
  (the (values (integer 0 *) (integer 0 *) (integer 0 *))
    (loop
      for prevenient-position
        of-type (integer 0 *)
        =       current-position
      for prevenient-rest
        of-type (integer * *)
        =       remaining-decimal-program-code
      for current-position
        of-type (integer 0 *)
        from    0
        by      1
      for remaining-decimal-program-code
        of-type (integer * *)
        =       decimal-program-code
        then    (- remaining-decimal-program-code
                   (* current-position 69))
      
      until
        (< remaining-decimal-program-code 0)
      
      finally
        (return
          (values
            (1+ prevenient-position)
            (- decimal-program-code prevenient-rest)
            prevenient-rest)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Encoding-State".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Encoding-State
  "The ``Encoding-State'' class serves in the ensconcement of the
   requisite brainfuckest encoding data, providing a firmament for the
   concluding formulation of the program in the brainfuckest character
   repertoire."
  (decimal-program-code (error "Missing program code.")
                        :type      (integer 0 *)
                        :read-only T)
  (number-of-positions  (error "Missing number of positions.")
                        :type      (integer 0 *)
                        :read-only T)
  (aliquot-part         (error "Missing aliquot part.")
                        :type      (integer 0 *)
                        :read-only T)
  (aliquant-part        (error "Missing aliquant part.")
                        :type      (integer 0 *)
                        :read-only T))

;;; -------------------------------------------------------

(defun prepare-encoding-state (decimal-program-code)
  "Creates and returns a fresh ``Encoding-State'' based upon the
   brainfuckest program's decimal integer representation."
  (declare (type (integer 0 *) decimal-program-code))
  (the Encoding-State
    (multiple-value-bind (number-of-positions
                          aliquot-part
                          aliquant-part)
        (extract-encoding-components decimal-program-code)
      (declare (type (integer 0 *) number-of-positions))
      (declare (type (integer 0 *) aliquot-part))
      (declare (type (integer 0 *) aliquant-part))
      (make-encoding-state
        :decimal-program-code decimal-program-code
        :number-of-positions  number-of-positions
        :aliquot-part         aliquot-part
        :aliquant-part        aliquant-part))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Brainfuckest-Encoder".              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Brainfuckest-Encoder
  "The ``Brainfuckest-Encoder'' interface establishes a common foundry
   for all classes in a pursuit to generate from an ``Encoding-State'',
   in particular its part of the sum dedicated to the contribution of
   the symbols, a piece of brainfuckest source code.
   ---
   In its most simple diction, such an encoder must, given the number
   of positions that constitute the ultimate brainfuckest program for
   the input brainfuck program's decimal represntation, an equinumerant
   sequence of symbols desumed from the brainfuckest repertoire whose
   character codes specified by the same language exactly equal the
   so called \"aliquant\" part not covered by the multiples of the
   number 69. With enhaused formality, the following must hold:
   
     Given a decimal integer representation d of the brainfuck program
     to encode as a brainfuckest equivalent, and its supputated number
     of encoded positions n, with the aliquot part f defined as
     
       f = (69 * 0) + (69 * 1) + ... + (69 * (n - 1)),
     
     and the aliquant portion v obtained by
     
       v = d - f,
     
     the encoder class must replicate this exact integral value v
     utilizing a covenable combination of n symbols from the
     brainfuckest character repertoire whose assigned brainfuckest
     character codes contribute the terms of this summation. This
     encoded ordered sequence C follows the diorism
     
       C = (c[1], c[2], ..., c[n]),
     
     where each c[i] represents the character whose brainfuckest code
     s[i] contributes to the sum
     
       v = s[1] + s[2] + ... s[n].")

;;; -------------------------------------------------------

(defgeneric encode-characters (encoder state destination)
  (:documentation
    "Employs the brainfuckest ENCODER in conjunction with the encoding
     STATE in order to produce a brainfuckest program tantamount to the
     original brainfuck source code in its decimal integer
     representation, writes the resulting program to the DESTINATION,
     and returns no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Default-Brainfuckest-Encoder".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Default-Brainfuckest-Encoder
  (:include Brainfuckest-Encoder))
  "The ``Default-Brainfuckest-Encoder'' class provides an encoder which
   generates brainfuckest symbols from the respective decimal program
   representation in an \"eager\" mode; that is, the first positions of
   the encoding sum are covered by characters with the largest
   brainfuckest character codes possible, seguing into decreasing
   code values while the positions progress.
   ---
   This scheme, as an epiphenomenon to the incorporated simplicity,
   tholes a destitution in its aesthetics, forecause no stimulating
   forbisens are aspired.")

;;; -------------------------------------------------------

(defmethod encode-characters ((encoder     Default-Brainfuckest-Encoder)
                              (state       Encoding-State)
                              (destination T))
  (declare (type Default-Brainfuckest-Encoder encoder)
           (ignore                            encoder))
  (declare (type Encoding-State               state))
  (declare (type destination                  destination))
  (loop
    repeat
      (encoding-state-number-of-positions state)
    for remaining-aliquant-part
      of-type (integer * *)
      =       (encoding-state-aliquant-part state)
      then    (- remaining-aliquant-part next-character-code)
    for next-character-code
      of-type (integer 0 68)
      =       (min 68 remaining-aliquant-part)
    do
      (format destination "~c"
        (get-brainfuckest-character-by-code next-character-code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Callback-Brainfuckest-Encoder".     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Callback-Brainfuckest-Encoder
  (:include     Brainfuckest-Encoder)
  (:constructor make-callback-brainfuckest-encoder (processor)))
  "The ``Callback-Brainfuckest-Encoder'' class implements an encoder
   which relays the brainfuckest program's assemblage to a callback
   function's exercise.
   ---
   The responsible function, nevened a \"processor\", expects the
   encoding state, defined in terms of an ``Encoding-State'' instance,
   and the destination that shall receive the generated brainfuckest
   code, returning an ignored result. As a consectary, the following
   signature applies to this functional object:
     lambda (Encoding-State destination) => ignored-result"
  (processor (error "Missing processor function.")
             :type      (function (Encoding-State destination) *)
             :read-only T))

;;; -------------------------------------------------------

(defmethod encode-characters
    ((encoder     Callback-Brainfuckest-Encoder)
     (state       Encoding-State)
     (destination T))
  (declare (type Callback-Brainfuckest-Encoder encoder))
  (declare (type Encoding-State                state))
  (declare (type destination                   destination))
  (funcall
    (callback-brainfuckest-encoder-processor encoder)
    state
    destination)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-brainfuckest encoder.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Default-Brainfuckest-Encoder +DEFAULT-ENCODER+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-ENCODER+
  (make-default-brainfuckest-encoder)
  "The standard brainfuckest encoder, which resorts to a simple
   progression of character codes commencing at the largest possible
   values and decreasing towards the desinent positions.")

;;; -------------------------------------------------------

(defun encode-state
    (state
     &key (encoder     +DEFAULT-ENCODER+)
          (destination NIL))
  "Employing the ENCODER's services, generates for the encoding STATE
   a piece of brainfuckest source code, writes thilk to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, produces a fresh simple base string
   comprehending the brainfuck code."
  (declare (type Encoding-State       state))
  (declare (type Brainfuckest-Encoder encoder))
  (declare (type destination          destination))
  (the (or null simple-base-string)
    (if destination
      (encode-characters encoder state destination)
      (convert-to-simple-base-string
        (with-output-to-string (brainfuckest-code)
          (declare (type string-stream brainfuckest-code))
          (encode-state state
            :encoder     encoder
            :destination brainfuckest-code))))))

;;; -------------------------------------------------------

(defun encode-decimal-program-code
    (decimal-program-code
     &key (encoder     +DEFAULT-ENCODER+)
          (destination NIL))
  "Employing the ENCODER's services, generates for the
   DECIMAL-PROGRAM-CODE, this being an unsigned integer representation
   of the original brainfuck program, a piece of brainfuckest source
   code, writes thilk to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   produces a fresh simple base string comprehending the brainfuck
   code."
  (declare (type (integer 0 *)        decimal-program-code))
  (declare (type Brainfuckest-Encoder encoder))
  (declare (type destination          destination))
  (the (or null simple-base-string)
    (encode-state
      (prepare-encoding-state decimal-program-code)
      :encoder     encoder
      :destination destination)))

;;; -------------------------------------------------------

(defun encode-octal-program-code
    (octal-program-code
     &key (encoder     +DEFAULT-ENCODER+)
          (destination NIL))
  "Employing the ENCODER's services, generates for the
   OCTAL-PROGRAM-CODE, this being a representation of the original
   brainfuck program as a string composed of octal digits, a piece of
   brainfuckest source code, writes thilk to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a
   ``NIL'' DESTINATION, produces a fresh simple base string
   comprehending the brainfuck code.."
  (declare (type string               octal-program-code))
  (declare (type Brainfuckest-Encoder encoder))
  (declare (type destination          destination))
  (the (or null simple-base-string)
    (encode-decimal-program-code
      (parse-integer octal-program-code :radix 8)
      :encoder     encoder
      :destination destination)))

;;; -------------------------------------------------------

(defun encode-brainfuck-code
    (brainfuck-code
     &key (encoder     +DEFAULT-ENCODER+)
          (destination NIL))
  "Employing the ENCODER's services, generates for the BRAINFUCK-CODE a
   piece of brainfuckest source code, writes thilk to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, produces a fresh simple base string
   comprehending the brainfuck code.."
  (declare (type string               brainfuck-code))
  (declare (type Brainfuckest-Encoder encoder))
  (declare (type destination          destination))
  (the (or null simple-base-string)
    (encode-octal-program-code
      (encode-brainfuck-code-as-octal-string
        (remove-non-brainfuck-characters brainfuck-code))
      :encoder     encoder
      :destination destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Navigation-Table".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Navigation-Table
  (:constructor prepare-empty-navigation-table ()))
  "The ``Navigation-Table'' class implements the bidirectional vincula
   betwixt forward and jump points in a brainfuck program, mediated by
   their zero-based indices into the code."
  (connections (make-hash-table :test #'eql)
               :type      (hash-table-of fixnum fixnum)
               :read-only T))

;;; -------------------------------------------------------

(defun connect-jump-points (table source destination)
  "Connects the SOURCE and DESTINATION points, mediated by their
   zero-based indices into the underlying brainfuck program, in a
   bidirectional manner, stores this ligation in the navigation TABLE,
   and returns no value."
  (declare (type Navigation-Table table))
  (declare (type fixnum           source))
  (declare (type fixnum           destination))
  (psetf
    (gethash source      (navigation-table-connections table))
      destination
    (gethash destination (navigation-table-connections table))
      source)
  (values))

;;; -------------------------------------------------------

(defun locate-jump-destination (table point-of-departure)
  "Returns the jump point opposite to the POINT-OF-DEPARTURE as
   governed by the navigation TABLE's castaldy, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type Navigation-Table table))
  (declare (type fixnum           point-of-departure))
  (the fixnum
    (or (gethash point-of-departure
          (navigation-table-connections table))
        (error "No destination associated with the jump point ~d."
          point-of-departure))))

;;; -------------------------------------------------------

(defun build-navigation-table-for-brainfuck-code (brainfuck-code)
  "Creates and returns a fresh ``Navigation-Table'' dedicated to the
   castaldy of the matching jump points in the piece of BRAINFUCK-CODE."
  (declare (type simple-base-string brainfuck-code))
  (let ((navigation-table    (prepare-empty-navigation-table))
        (forward-jump-points NIL))
    (declare (type Navigation-Table navigation-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-position of-type fixnum        from   0 by 1
      for current-token    of-type standard-char across brainfuck-code
      
      if (char= current-token #\[) do
        (push current-position forward-jump-points)
      else if (char= current-token #\]) do
        (if forward-jump-points
          (connect-jump-points navigation-table
            (pop forward-jump-points)
            current-position)
          (error "Missing forward jump points."))
      end
      
      finally
        (when forward-jump-points
          (error "Missing back jump points.")))
    
    (the Navigation-Table navigation-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor prepare-pristine-tape ()))
  "The ``Tape'' class furnishes the implementation of the program tape,
   defined in terms of a bilaterally infinite dispansion of unsigned
   byte-valued cells, upon operates a cell pointer that any instant
   selects the currently active unit."
  (bits                         #b00000000
                                :type      unsigned-byte
                                :read-only NIL)
  (pointer                      0
                                :type      integer
                                :read-only NIL)
  (smallest-accessed-cell-index 0
                                :type      integer
                                :read-only NIL))

;;; -------------------------------------------------------

(defun translate-current-cell-index-into-bit-offset (tape)
  "Returns the unsigned integer offset into the TAPE's bits
   corresponding to its cell pointer's currently selected cell index
   and designating the start position inside of the binary sequence of
   this particular cell."
  (declare (type Tape tape))
  (the (integer 0 *)
    (* (- (tape-pointer                      tape)
          (tape-smallest-accessed-cell-index tape))
       8)))

;;; -------------------------------------------------------

(defun get-byte-specifier-for-current-cell (tape)
  "Returns an implementation-dependent byte specifier which designates
   the eight bits corresponding to the cell under the TAPE's cell
   pointer."
  (declare (type Tape tape))
  (the T
    (byte 8
      (translate-current-cell-index-into-bit-offset tape))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the (unsigned-byte 8)
    (ldb
      (get-byte-specifier-for-current-cell tape)
      (tape-bits                           tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping of its state in order to respect
   the admissible bournes of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (ldb
      (get-byte-specifier-for-current-cell tape)
      (tape-bits                           tape))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (tape-pointer tape))
  (when (< (tape-pointer                      tape)
           (tape-smallest-accessed-cell-index tape))
    (psetf
      (tape-smallest-accessed-cell-index tape)
        (tape-pointer tape)
      (tape-bits tape)
        (ash (tape-bits tape) 8)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-brainfuck (code
                            &key (displays-prompt-p T))
  "Interprets the piece of brainfuck source CODE and returns no value.
   ---
   The DISPLAYS-PROMPT-P flag's configuration homologates the ostention
   or concealment of an input prompt, defaulting to the former, positive
   case."
  (declare (type simple-base-string code))
  (declare (type boolean            displays-prompt-p))
  (let ((ip 0)
        (navigation-table
          (build-navigation-table-for-brainfuck-code code))
        (tape
          (prepare-pristine-tape)))
    (declare (type fixnum           ip))
    (declare (type Navigation-Table navigation-table))
    (declare (type Tape             tape))
    (loop while (< ip (length code)) do
      (case (schar code ip)
        (#\>
          (move-cell-pointer-right tape))
        (#\<
          (move-cell-pointer-left tape))
        (#\+
          (incf (current-cell-value tape)))
        (#\-
          (decf (current-cell-value tape)))
        (#\.
          (format *standard-output* "~c"
            (code-char
              (current-cell-value tape))))
        (#\,
          (when displays-prompt-p
            (format *standard-output* "~&>> "))
          (finish-output *standard-output*)
          (setf (current-cell-value tape)
            (char-code
              (read-char *standard-input* NIL #\Null)))
          (clear-input *standard-input*))
        (#\[
          (when (zerop (current-cell-value tape))
            (setf ip
              (locate-jump-destination navigation-table ip))))
        (#\]
          (unless (zerop (current-cell-value tape))
            (setf ip
              (locate-jump-destination navigation-table ip))))
        (otherwise
          NIL))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-brainfuckest (code &key (displays-prompt-p T))
  "Interprets the piece of brainfuckest source CODE and returns no
   value.
   ---
   The DISPLAYS-PROMPT-P flag's configuration homologates the ostention
   or concealment of an input prompt, defaulting to the former, positive
   case."
  (declare (type string  code))
  (declare (type boolean displays-prompt-p))
  (interpret-brainfuck
    (decode-brainfuckest code)
    :displays-prompt-p displays-prompt-p)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-brainfuckest "ZZZZZZZZZZZZZZZZZZZZZZZZZZ")

;;; -------------------------------------------------------

;; Repeating cat program utilizing a fixed input sequence.
(with-input-from-string (input-stream "Pigeons are welcome here.")
  (declare (type string-stream input-stream))
  (let ((*standard-input* input-stream))
    (interpret-brainfuckest "ZZZZZZZZZZZZZZZZZZZZZZZZZZ"
      :displays-prompt-p NIL)))

;;; -------------------------------------------------------

;; Convert the repeating cat program
;;   ,[.,]
;; into one of its possible brainfuckest encodings
;;    ":::::::::::::::::::::20000"
;; and execute the same.
(interpret-brainfuckest
  (encode-brainfuck-code ",[.,]"))

;;; -------------------------------------------------------

;; Encodes the repeating brainfuck cat program employing an
;; average-based approach, where the maximum character code for each
;; output position amounts to the average of the aliquant part divided
;; by the number of positions to occupy in the resulting brainfuckest
;; program.
(encode-brainfuck-code ",[.,]"
  :encoder
    (make-callback-brainfuckest-encoder
      #'(lambda (state destination)
          (declare (type Encoding-State state))
          (declare (type destination    destination))
          (let ((average-character-code
                  (ceiling
                    (encoding-state-aliquant-part       state)
                    (encoding-state-number-of-positions state))))
            (declare (type (integer 0 *) average-character-code))
            (loop
              repeat
                (encoding-state-number-of-positions state)
              for remaining-aliquant-part
                of-type (integer * *)
                =       (encoding-state-aliquant-part state)
                then    (- remaining-aliquant-part next-character-code)
              for next-character-code
                of-type (integer 0 68)
                =       (min average-character-code
                             remaining-aliquant-part)
              do
                (format destination "~c"
                  (get-brainfuckest-character-by-code
                    next-character-code))))
          (values))))
