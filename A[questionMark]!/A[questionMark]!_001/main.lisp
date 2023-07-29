;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "A?!", presented by the Esolang user "Someone else" in the
;; year 2019, and founded upon the manipulation of a maximum of 62
;; single bit-valued variables for input, output, and control flow
;; conduction.
;; 
;; 
;; Concept
;; =======
;; The A?! programming language's tenet revolves around the manipulation
;; of a set of 62 possible bit-valued variables, pursuing the chevisance
;; of input, output, and control flow governance.
;; 
;; == 62 VARIABLES STAND TO THE PROGRAMMER'S AVAIL ==
;; A?! abstains from a dedicated memory's deployment in favor of a fixed
;; set of variables' wike, a tally of 62 are committed to the cause.
;; 
;; Each variable's agnomination is satisfied by a single character,
;; either borrowed from the 26 Latin majuscle, 26 minuscle, or the ten
;; decimal digits, the competence of these entities restricted to an
;; aefauld bit's memorization.
;; 
;; A tabular listing shall empight cognizance about the available
;; variable identifiers and the categories providing their residence:
;; 
;;   ------------------------------
;;   Variable name | Species
;;   --------------+---------------
;;   A             | Latin majuscle
;;   ..............................
;;   B             | Latin majuscle
;;   ..............................
;;   C             | Latin majuscle
;;   ..............................
;;   D             | Latin majuscle
;;   ..............................
;;   E             | Latin majuscle
;;   ..............................
;;   F             | Latin majuscle
;;   ..............................
;;   G             | Latin majuscle
;;   ..............................
;;   H             | Latin majuscle
;;   ..............................
;;   I             | Latin majuscle
;;   ..............................
;;   J             | Latin majuscle
;;   ..............................
;;   K             | Latin majuscle
;;   ..............................
;;   L             | Latin majuscle
;;   ..............................
;;   M             | Latin majuscle
;;   ..............................
;;   N             | Latin majuscle
;;   ..............................
;;   O             | Latin majuscle
;;   ..............................
;;   P             | Latin majuscle
;;   ..............................
;;   Q             | Latin majuscle
;;   ..............................
;;   R             | Latin majuscle
;;   ..............................
;;   S             | Latin majuscle
;;   ..............................
;;   T             | Latin majuscle
;;   ..............................
;;   U             | Latin majuscle
;;   ..............................
;;   V             | Latin majuscle
;;   ..............................
;;   W             | Latin majuscle
;;   ..............................
;;   X             | Latin majuscle
;;   ..............................
;;   Y             | Latin majuscle
;;   ..............................
;;   Z             | Latin majuscle
;;   ::::::::::::::::::::::::::::::
;;   a             | Latin minuscle
;;   ..............................
;;   b             | Latin minuscle
;;   ..............................
;;   c             | Latin minuscle
;;   ..............................
;;   d             | Latin minuscle
;;   ..............................
;;   e             | Latin minuscle
;;   ..............................
;;   f             | Latin minuscle
;;   ..............................
;;   g             | Latin minuscle
;;   ..............................
;;   h             | Latin minuscle
;;   ..............................
;;   i             | Latin minuscle
;;   ..............................
;;   j             | Latin minuscle
;;   ..............................
;;   k             | Latin minuscle
;;   ..............................
;;   l             | Latin minuscle
;;   ..............................
;;   m             | Latin minuscle
;;   ..............................
;;   n             | Latin minuscle
;;   ..............................
;;   o             | Latin minuscle
;;   ..............................
;;   p             | Latin minuscle
;;   ..............................
;;   q             | Latin minuscle
;;   ..............................
;;   r             | Latin minuscle
;;   ..............................
;;   s             | Latin minuscle
;;   ..............................
;;   t             | Latin minuscle
;;   ..............................
;;   u             | Latin minuscle
;;   ..............................
;;   v             | Latin minuscle
;;   ..............................
;;   w             | Latin minuscle
;;   ..............................
;;   x             | Latin minuscle
;;   ..............................
;;   y             | Latin minuscle
;;   ..............................
;;   z             | Latin minuscle
;;   ::::::::::::::::::::::::::::::
;;   0             | decimal digit
;;   ..............................
;;   1             | decimal digit
;;   ..............................
;;   2             | decimal digit
;;   ..............................
;;   3             | decimal digit
;;   ..............................
;;   4             | decimal digit
;;   ..............................
;;   5             | decimal digit
;;   ..............................
;;   6             | decimal digit
;;   ..............................
;;   7             | decimal digit
;;   ..............................
;;   8             | decimal digit
;;   ..............................
;;   9             | decimal digit
;;   ------------------------------
;; 
;; == INPUT AND OUTPUT PROCEED BITWISE ==
;; A kenspeckle mode of interaction wones in the language's mandate to
;; expect per input request a single bit for its storage in a variable.
;; Upon the inquired source's exhaustion, delineated through a return
;; value that specifies the end-of-file marker (EOF), the program
;; terminates without delay.
;; 
;; Consanguinity governs the athwart conduit, whose commission of output
;; behests, in lieu of immediacy in ostentation, accumulates in a bit
;; buffer, building from the most significant bit (MSB) to the least
;; significant one (LSB) an octet. The print operation whose completion
;; accompasses the eight bit compound's patration produces as a
;; epiphenomenon the ASCII character corresponding in its code to the
;; byte value. A consequence therefrom, the accumulator is reset for an
;; iterum graduation in siclike fashion.
;; 
;; == EACH INSTRUCTION A LINE ==
;; A?! programs are limned as a sequence of zero or more lines, each
;; non-empty instance accommodating exactly one instruction's abode.
;; 
;; Beside variable bit negation, input and output, a conditional and a
;; twain of unconditional jumping commands offer themselves to the
;; programmer's utility.
;; 
;; 
;; Architecture
;; ============
;; No particular concerns relate to the A?! architecture, as a program's
;; state in this language is exhausted by the definitions of its 62
;; variables, every member therein initialized to the default value
;; zero (0).
;; 
;; The absence of a natural ordering among these placeholders and their
;; humble requirements of inquisition and manipulation permit any
;; associative data structure to partake in their castaldy.
;; 
;; 
;; Data Types
;; ==========
;; A bivious governance applies to A?!'s data type system, apportioning
;; to the bit species the most excellent significance, while characters
;; enjoy an aefauld engagement when issuing output.
;; 
;; == BITS: THE CURRENCY OF THE MEMORY ==
;; Exhausting in their participation the paravaunt bit type, the values
;; zero (0) and one (1) are endowed with the most expansive relations,
;; spanning their maintenance in the program's variables, the
;; transmission along the input and output conduits, as well as the
;; bit negation and conditional skipping commodities.
;; 
;; == CHARACTERS: THE CURRENCY OF COMMUNICATION ==
;; The character species' occupancy of a paravail echolon ostends its
;; verification in the exclusivity that refers to the output operation,
;; the commission of the output buffer's eight bits ensuing as an
;; epiphenomenon from a variable value's transmission into the storing
;; entity.
;; 
;; The character repertoire responsible for the binary-to-text
;; conversion resolves to the ASCII standard.
;; 
;; 
;; Syntax
;; ======
;; An A?! program, in the matters of its syntaxis, obeys to the
;; definition as an ordered sequence of zero or more lines, each of
;; which may lend a commorancy to at most one instruction.
;; 
;; == INSTRUCTIONS ==
;; Anenst their syntactical aspects, the instructions' expression
;; proceeds in a bivious manner, the incipient species in relationship
;; with variables, the second a dedication to unconditional navigation.
;; 
;; The variable-based operations are introduced by a one-character
;; variable identifier, and concluded by the instruction designator
;; whose quantitative circumference expands into one or three positions.
;; 
;; The ilk of unconditional navigation deviates from this blueprint,
;; accommodating one or more repetitions of either ">" or "<", but not
;; in admixture.
;; 
;; Each instruction must occupy a line of its own, optionally preceded
;; by zero or more spaces, and contingently reverberating the same
;; administration on the dextral side, the provision for a subsequent
;; comment portion granted in supererogation.
;; 
;; Empty and blank lines, that is, such composed of spaces and
;; horizontal tabs only, in a contingent conjunction with a comment, are
;; adhibited homologation, but utter inefficacy. Such lines do not even
;; participate in the actual program's line enumeration, especially when
;; referring to skips and returns.
;; 
;; == VARIABLES ==
;; Variable names embrace a single character, either a Latin minuscle or
;; majuscle or a decimal digit, thus tallying 62 possibilities --- 26
;; for the lower-case letters, 26 for their upper-case variants, and ten
;; contributed by the numeric entities.
;; 
;; == COMMENTS ==
;; Comments are instigated using the hash sign "#", permitted to occur
;; at any location on a line, and extending to its desinence.
;; 
;; == GRAMMAR ==
;; A formulation of the language's syntax in the Extended Backus-Naur
;; Form (EBNF) shall now be adhibited:
;; 
;;   program              := { innerEmptyLine | innerInstructionLine }
;;                        ,  [ lastEmptyLine  | lastInstructionLine ]
;;                        ;
;;   innerInstructionLine := { spacing }
;;                        ,  command
;;                        ,  [ comment ]
;;                        ,  newline
;;                        ;
;;   lastInstructionLine  := { spacing } , command , [ comment ] ;
;;   innerEmptyLine       := { spacing } , [ comment ] , newline ;
;;   lastEmptyLine        := { spacing } , [ comment ] ;
;;   
;;   comment              := "#" , { character - newline } ;
;;   
;;   command              := negateBit
;;                        |  skipIfFalse
;;                        |  output
;;                        |  input
;;                        |  jumpForward
;;                        |  jumpBack
;;                        ;
;;   negateBit            := variable , "!" ;
;;   skipIfFalse          := variable , "?" ;
;;   output               := variable , "." ;
;;   input                := variable , "..." ;
;;   jumpForward          := ">" , { ">" } ;
;;   jumpBack             := "<" , { "<" } ;
;;   
;;   variable             := "A" | ... | "Z"
;;                        |  "a" | ... | "z"
;;                        |  "0" | ... | "9"
;;                        ;
;;   newline              := "\n" ;
;;   spacing              := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; The A?! language's operational bailiwick assumes six instructions,
;; permitting a restricted manipulation of its variables, a conditional
;; as well as two unconditional jumping actions, in conjunction with
;; bitwise input and output communications.
;; 
;; == OVERVIEW ==
;; A?!'s instruction set tallies a sextuple membership shall now be an
;; apercu's cynosure. Please heed that placeholder portions, demarcated
;; by their ensconcement in braces, "{" and "}", are, together with
;; their marches, intended to be replaced by actual content.
;; 
;;   ------------------------------------------------------------------
;;   Command       | Effect
;;   --------------+---------------------------------------------------
;;   {variable}!   | Negates the bit value stored in the {variable}.
;;                 | A bit value of zero (0) is converted to one (1),
;;                 | and a bit value of one (1) to zero (0).
;;   ..................................................................
;;   {variable}?   | If the {variable} value equals zero (0), skips the
;;                 | next instruction; aliter proceeds as usual.
;;                 | Blank lines and such composed of a comment only
;;                 | do not count in this aspect.
;;   ..................................................................
;;   {variable}.   | Transmits the bit value stored in the {variable}
;;                 | to the output buffer.
;;                 | If the output buffer is rendered complete by this
;;                 | operation, that is, has accumulated eight bits,
;;                 | the ASCII character corresponding to the buffer's
;;                 | decimal value is printed to the standard output,
;;                 | ere the buffer is purged or reset.
;;                 | The output buffer cannot be padded to octet size
;;                 | by the program; in corollary, an attempt at
;;                 | printing its incomplete state will elicit an error
;;                 | of an unspecified type.
;;   ..................................................................
;;   {variable}... | Queries a single bit (0 or 1) from the standard
;;                 | input and stores it in the {variable}.
;;                 | If the input conduit is exhausted, signified by
;;                 | an end-of-file (EOF) response, the program halts
;;                 | immediately.
;;   ..................................................................
;;   >{...}>       | Tallies the number of consecutive ">" occurrences
;;                 | in immediate adjacency and skips this tally of
;;                 | following lines.
;;                 | A single ">", a forbisen being supplied, would
;;                 | skip the next instruction and continue with its
;;                 | successor.
;;                 | Blank lines and such composed of a comment only
;;                 | do not count in this aspect.
;;   ..................................................................
;;   <{...}<       | Tallies the number of consecutive "<" occurrences
;;                 | in immediate adjacency and jumps back this tally
;;                 | of preceding lines.
;;                 | A single "<", as a forbisen, would return to the
;;                 | immediately preceding instruction.
;;                 | Blank lines and such composed of a comment only
;;                 | do not count in this aspect.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The lucidity's incarnation in A?!'s protolog expels many potentials
;; for ambiguities; natheless, a conspicuous subset of presences still
;; encroaches its state and shall be subjected to further disquisition
;; in the following sections.
;; 
;; == ARE ALL VARIABLES DEPLOYABLE BY DEFAULT? ==
;; A?! provides a fixed-sized set of 62 variables, everichon's
;; identification proceeds from a single letter or decimal digit. The
;; nominal resolution is not equipoised by epiky concerning the
;; availability, namely, whether the declaration and initialization of a
;; variable impose a requisitum to its deployment.
;; 
;; The two stages of a variable's communication --- declaration and
;; assignment --- both ensue from the aefauld action of user input
;; obtention; all examples rely on this parasceve ere the variables'
;; employment.
;; 
;; It has been adjudged that all 62 variables may be utilized even if
;; not explicitly declared and assigned, defaulting to a bit value of
;; zero (0) as the initial content.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation manifests in the programming language
;; Common Lisp, its realization delineated by three partly entwined
;; stages:
;; 
;;   (1) The lexical analyzer, or lexer, extracts the significant
;;       objects, such as variable identifiers and instruction types,
;;       from the piece of A?! source code supplied as a string.
;;   (2) The parser's wike embraces the assemblage of a vector of
;;       executable A?! instructions; its amenability to indices admits
;;       the potential for the instruction pointer's (IP) jumping.
;;   (3) The interpreter appropriates the parser's instruction sequence,
;;       each item of which is processed by adminiculum of the
;;       internally maintained instruction pointer.
;; 
;; 
;; Appendices
;; ==========
;; Topics whose remoteness to the A?! language specification eloign them
;; in a physical aspect, but their concomitant pertinence vindicates an
;; amplectation, shall be a disquisition's material in this location.
;; 
;; == APPENDIX A: ASCII CODES ==
;; The dioristic conveyance of input and output in bit form encumbers
;; the invoking entity, be it a human user or a dedicated source or
;; sink, with the imposition to convert the stillatim bit specifications
;; into meaningful ASCII character.
;; 
;; The table produced alow shall adhibit a cursory intelligence
;; concerning the first 127 ASCII characters and their codes in both
;; decimal and binary form, the latter of which invested with particular
;; importance, and presented in sinistrodextral airt from the most
;; significant (MSB) to the least position (LSB):
;; 
;;   ------------------------------
;;   Character | Decimal | Binary
;;   ----------+---------+---------
;;   NUL       |     0   | 00000000
;;   ..............................
;;   SOH       |     1   | 00000001
;;   ..............................
;;   STX       |     2   | 00000010
;;   ..............................
;;   ETX       |     3   | 00000011
;;   ..............................
;;   E0T       |     4   | 00000100
;;   ..............................
;;   ENQ       |     5   | 00000101
;;   ..............................
;;   ACK       |     6   | 00000110
;;   ..............................
;;   BEL       |     7   | 00000111
;;   ..............................
;;   backspace |     8   | 00001000
;;   ..............................
;;   TAB       |     9   | 00001001
;;   ..............................
;;   newline   |    10   | 00001010
;;   ..............................
;;   VT        |    11   | 00001011
;;   ..............................
;;   FF        |    12   | 00001100
;;   ..............................
;;   CR        |    13   | 00001101
;;   ..............................
;;   SO        |    14   | 00001110
;;   ..............................
;;   SI        |    15   | 00001111
;;   ..............................
;;   DLE       |    16   | 00010000
;;   ..............................
;;   DC1       |    17   | 00010001
;;   ..............................
;;   DC2       |    18   | 00010010
;;   ..............................
;;   DC3       |    19   | 00010011
;;   ..............................
;;   DC4       |    20   | 00010100
;;   ..............................
;;   NAK       |    21   | 00010101
;;   ..............................
;;   SYN       |    22   | 00010110
;;   ..............................
;;   ETB       |    23   | 00010111
;;   ..............................
;;   CAN       |    24   | 00011000
;;   ..............................
;;   EM        |    25   | 00011001
;;   ..............................
;;   SUB       |    26   | 00011010
;;   ..............................
;;   ESC       |    27   | 00011011
;;   ..............................
;;   FS        |    28   | 00011100
;;   ..............................
;;   GS        |    29   | 00011101
;;   ..............................
;;   RS        |    30   | 00011110
;;   ..............................
;;   US        |    31   | 00011111
;;   ..............................
;;   Space     |    32   | 00100000
;;   ..............................
;;   !         |    33   | 00100001
;;   ..............................
;;   "         |    34   | 00100010
;;   ..............................
;;   #         |    35   | 00100011
;;   ..............................
;;   $         |    36   | 00100100
;;   ..............................
;;   %         |    37   | 00100101
;;   ..............................
;;   &         |    38   | 00100110
;;   ..............................
;;   '         |    39   | 00100111
;;   ..............................
;;   (         |    40   | 00101000
;;   ..............................
;;   )         |    41   | 00101001
;;   ..............................
;;   *         |    42   | 00101010
;;   ..............................
;;   +         |    43   | 00101011
;;   ..............................
;;   ,         |    44   | 00101100
;;   ..............................
;;   -         |    45   | 00101101
;;   ..............................
;;   .         |    46   | 00101110
;;   ..............................
;;   /         |    47   | 00101111
;;   ..............................
;;   0         |    48   | 00110000
;;   ..............................
;;   1         |    49   | 00110001
;;   ..............................
;;   2         |    50   | 00110010
;;   ..............................
;;   3         |    51   | 00110011
;;   ..............................
;;   4         |    52   | 00110100
;;   ..............................
;;   5         |    53   | 00110101
;;   ..............................
;;   6         |    54   | 00110110
;;   ..............................
;;   7         |    55   | 00110111
;;   ..............................
;;   8         |    56   | 00111000
;;   ..............................
;;   9         |    57   | 00111001
;;   ..............................
;;   :         |    58   | 00111010
;;   ..............................
;;   ;         |    59   | 00111011
;;   ..............................
;;   <         |    60   | 00111100
;;   ..............................
;;   =         |    61   | 00111101
;;   ..............................
;;   >         |    62   | 00111110
;;   ..............................
;;   ?         |    63   | 00111111
;;   ..............................
;;   @         |    64   | 01000000
;;   ..............................
;;   A         |    65   | 01000001
;;   ..............................
;;   B         |    66   | 01000010
;;   ..............................
;;   C         |    67   | 01000011
;;   ..............................
;;   D         |    68   | 01000100
;;   ..............................
;;   E         |    69   | 01000101
;;   ..............................
;;   F         |    70   | 01000110
;;   ..............................
;;   G         |    71   | 01000111
;;   ..............................
;;   H         |    72   | 01001000
;;   ..............................
;;   I         |    73   | 01001001
;;   ..............................
;;   J         |    74   | 01001010
;;   ..............................
;;   K         |    75   | 01001011
;;   ..............................
;;   L         |    76   | 01001100
;;   ..............................
;;   M         |    77   | 01001101
;;   ..............................
;;   N         |    78   | 01001110
;;   ..............................
;;   O         |    79   | 01001111
;;   ..............................
;;   P         |    80   | 01010000
;;   ..............................
;;   Q         |    81   | 01010001
;;   ..............................
;;   R         |    82   | 01010010
;;   ..............................
;;   S         |    83   | 01010011
;;   ..............................
;;   T         |    84   | 01010100
;;   ..............................
;;   U         |    85   | 01010101
;;   ..............................
;;   V         |    86   | 01010110
;;   ..............................
;;   W         |    87   | 01010111
;;   ..............................
;;   X         |    88   | 01011000
;;   ..............................
;;   Y         |    89   | 01011001
;;   ..............................
;;   Z         |    90   | 01011010
;;   ..............................
;;   [         |    91   | 01011011
;;   ..............................
;;   \         |    92   | 01011100
;;   ..............................
;;   ]         |    93   | 01011101
;;   ..............................
;;   ^         |    94   | 01011110
;;   ..............................
;;   _         |    95   | 01011111
;;   ..............................
;;   `         |    96   | 01100000
;;   ..............................
;;   a         |    97   | 01100001
;;   ..............................
;;   b         |    98   | 01100010
;;   ..............................
;;   c         |    99   | 01100011
;;   ..............................
;;   d         |   100   | 01100100
;;   ..............................
;;   e         |   101   | 01100101
;;   ..............................
;;   f         |   102   | 01100110
;;   ..............................
;;   g         |   103   | 01100111
;;   ..............................
;;   h         |   104   | 01101000
;;   ..............................
;;   i         |   105   | 01101001
;;   ..............................
;;   j         |   106   | 01101010
;;   ..............................
;;   k         |   107   | 01101011
;;   ..............................
;;   l         |   108   | 01101100
;;   ..............................
;;   m         |   109   | 01101101
;;   ..............................
;;   n         |   110   | 01101110
;;   ..............................
;;   o         |   111   | 01101111
;;   ..............................
;;   p         |   112   | 01110000
;;   ..............................
;;   q         |   113   | 01110001
;;   ..............................
;;   r         |   114   | 01110010
;;   ..............................
;;   s         |   115   | 01110011
;;   ..............................
;;   t         |   116   | 01110100
;;   ..............................
;;   u         |   117   | 01110101
;;   ..............................
;;   v         |   118   | 01110110
;;   ..............................
;;   w         |   119   | 01110111
;;   ..............................
;;   x         |   120   | 01111000
;;   ..............................
;;   y         |   121   | 01111001
;;   ..............................
;;   z         |   122   | 01111010
;;   ..............................
;;   {         |   123   | 01111011
;;   ..............................
;;   |         |   124   | 01111100
;;   ..............................
;;   }         |   125   | 01111101
;;   ..............................
;;   ~         |   126   | 01111110
;;   ..............................
;;   DEL       |   127   | 01111111
;;   ------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date: 2023-07-03
;; 
;; Sources:
;;   [esolang2019A?!]
;;   The Esolang contributors, "A?!", 2019
;;   URL: "https://esolangs.org/w/index.php?title=A%3F!&oldid=66699"
;;   Notes:
;;     - An older rendition of the standard, from 18 October, 2019,
;;       still authored by the A?! programming language's originator,
;;       whose descriptions are in some passages entalented with greater
;;       lucidity.
;;   
;;   [esolang2022A?!]
;;   The Esolang contributors, "A?!", 2022
;;   URL: "https://esolangs.org/wiki/A%3F!"
;;   
;;   [injosoft2023asciitable]
;;   Injosoft AB,
;;   "ASCII table - Table of ASCII codes, characters and symbols", 2023
;;   URL: "https://www.ascii-code.com/"
;;   Notes:
;;     - Lists the ASCII character codes in decimal and binary form.
;;     - Supplements a list of the extended ASCII codes, covering the
;;       character code range 128 through 255, and targeting the
;;       Windows-1252 (CP-1252) standard.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype instruction-type ()
  "The ``instruction-type'' enumerates the recognized variants of
   commands, augmented by an adscititious member which designates an
   no-operation (NOP), reserved for lines destitute of an effective
   entity, such as blank and comment-only rows."
  '(member
    :negate-variable
    :skip-if-false
    :output
    :input
    :jump-forward
    :jump-back
    :nop))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of zero or more entries, each member of which either
   constitutes the ``NIL'' value or a cons whose left compartment
   conforms to the KEY-TYPE, associated with the VALUE-TYPE in the
   dextral moiety, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for element of-type T in (the list candidate)
              always
                (or
                  (null element)
                  (and
                    (typep element
                      `(cons ,key-type ,value-type))))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer greater than or
   equal to zero (0), but unbridled along the upper bourne."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer greater than zero
   (0), but unbridled along the upper bourne."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype instruction-table ()
  "The ``instruction-table'' type defines an instruction table,
   associating the recognized variable-based operation identifiers with
   keywords, manifesting as an association list which maps command
   string names to ``instruction-type'' keyword symbols."
  '(association-list-of simple-string instruction-type))

;;; -------------------------------------------------------

(deftype A?!-program ()
  "The ``A?!-program'' type defines an A?! program as a vector of zero
   or more ``Instruction'' objects."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype variable-table ()
  "The ``variable-table'' type defines an association of variable names
   to their bit values, realized as an association list which maps the
   variable name characters to bit objects."
  '(association-list-of character bit))

;;; -------------------------------------------------------

(deftype variable-table-entry ()
  "The ``variable-table-entry'' type defines an entry in a
   ``variable-table'', the same being an assocation list, as a cons, the
   first element of which comprehends the variable name as an aefauld
   character, associated with the stored bit value in the second
   compartment."
  '(cons character bit))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, for instance, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of variable names.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 62) +VARIABLE-NAMES+))

;;; -------------------------------------------------------

(defparameter +VARIABLE-NAMES+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "Lists the 62 recognized variable names, each such an aefauld Latin
   letter or a decimal digit.")

;;; -------------------------------------------------------

(defun variable-name-p (candidate)
  "Determines whether the CANDIDATE represents a variable identifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate +VARIABLE-NAMES+ :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction table.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type instruction-table +INSTRUCTION-TYPES+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-TYPES+
  '(("!"   . :negate-variable)
    ("?"   . :skip-if-false)
    ("."   . :output)
    ("..." . :input))
  "Associates with the recognized instruction names the correlated
   instruction type keywords.")

;;; -------------------------------------------------------

(defun instruction-character-p (candidate)
  "Determines whether the CANDIDATE represents an instruction name
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "!?." :test #'char=)))))

;;; -------------------------------------------------------

(defun get-instruction-type (name)
  "Returns the instruction type corresponding to the NAME, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type string name))
  (the instruction-type
    (or (cdr (assoc name +INSTRUCTION-TYPES+ :test #'string=))
        (error "Unrecognized instruction name: ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun end-of-source-p (source position)
  "Determines whether the POSITION violates the SOURCE's boundaries, on
   confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (array-in-bounds-p source position))))

;;; ---------------------------------------------------

(defun character-at-satisfies-p (source position predicate)
  "Determines whether the POSITION constitutes a valid location in the
   SOURCE and whether, if such can be avered, the character at this
   index concomitantly satisfies the PREDICATE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   The PREDICATE is exclusively invoked if the POSITION defines a valid
   location in the SOURCE, and it must be a callback function which
   accepts as its sole input the character at the specified POSITION,
   returning a generalized Boolean of \"true\" upon its satisfaction,
   aliter ``NIL''. Its signature hence must conform to:
     lambda (character-at-position) => generalized-boolean"
  (declare (type string                   source))
  (declare (type fixnum                   position))
  (declare (type (function (character) *) predicate))
  (the boolean
    (not (null
      (and
        (not (end-of-source-p source position))
        (funcall predicate (char source position)))))))

;;; ---------------------------------------------------

(defun character-at-equals-p (source position expected-character)
  "Determines whether the character at the POSITION into the SOURCE
   equals the EXPECTED-CHARACTER, returning on confirmation a
   ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the boolean
    (not (null
      (and
        (not (end-of-source-p source position))
        (char= (char source position) expected-character))))))

;;; ---------------------------------------------------

(defun space-character-at-p (source position)
  "Determines whether the character at the POSITION into the SOURCE
   constitutes a space or horizontal tab, returning on confirmation a
   ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (character-at-satisfies-p source position #'space-character-p)))

;;; ---------------------------------------------------

(defun variable-name-at-p (source position)
  "Determines whether the character at the position into the SOURCE
   constitutes a variable name, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (character-at-satisfies-p source position #'variable-name-p)))

;;; ---------------------------------------------------

(defun instruction-character-at-p (source position)
  "Determines whether the character at the position into the SOURCE
   accounts for a variabled-based instruction's identifier, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (character-at-satisfies-p source position
      #'instruction-character-p)))

;;; ---------------------------------------------------

(defun comment-starts-at-p (source position)
  "Determines whether the character at the POSITION into SOURCE
   constitutes the comment instigator \"#\", returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (character-at-equals-p source position #\#)))

;;; ---------------------------------------------------

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips zero or
   more adjacent spaces and returns the location immediately succeeding
   the desinent consumed space in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start by 1
      while   (space-character-at-p source position)
      finally (return position))))

;;; -------------------------------------------------------

(defun count-repetitions (source start expected-character)
  "Proceeding from the START position in the SOURCE, tallies the number
   of consecutive occurrences of the EXPECTED-CHARACTER, and return two
   values:
     (1) The number of accolent occurrences of the EXPECTED-CHARACTER in
         the SOURCE, beginning from the inclusive START location.
     (2) The position in the SOURCE immediately succeeding the desinent
         occurrence of the EXPECTED-CHARACTER."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type character expected-character))
  (the (values positive-integer fixnum)
    (loop
      for     position of-type fixnum from start by 1
      while   (character-at-equals-p source position expected-character)
      count   1
      into    repetitions
      finally (return (values repetitions position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The abstract ``Instruction'' class established a common foundry for
   the various concrete species of A?! instructions in currency."
  (type (error "Missing instruction type.") :type instruction-type))

;;; -------------------------------------------------------

(defstruct (Variable-Instruction
  (:include     Instruction)
  (:constructor make-variable-instruction (type variable)))
  "The ``Variable-Instruction'' class models an instruction that
   operates on a variable, such a bit negation, conditional line
   skipping, input, and output."
  (variable (error "Missing variable.") :type character))

;;; -------------------------------------------------------

(defstruct (Jump-Instruction
  (:include     Instruction)
  (:constructor make-jump-instruction (type distance)))
  "The ``Jump-Instruction'' class models an instruction that specializes
   on jump operations, which embraces the forward and back navigators."
  (distance (error "Missing distance.") :type positive-integer))

;;; -------------------------------------------------------

(defstruct (NOP-Instruction
  (:include     Instruction)
  (:constructor make-nop-instruction (&aux (type :nop))))
  "The ``NOP-Instruction'' class represents a no-operation, or NOP,
   instruction, that is, either a blank line or one comprehending a
   comment only.
   ---
   NOP instructions are not expected to partake of the ultimate
   instruction sequence, and their insertion may either be obviated
   completely, or they will be deleted ere their transmission to the
   interpreter.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-instruction-type (source start)
  "Proceeding from the START position into the SOURCE, expected to be
   located immediately after a variable name, consumes the characters of
   the instruction type and returns a keyword representation thereof."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position start))
    (declare (type fixnum position))
    (with-open-stream (instruction-name (make-string-output-stream))
      (declare (type string-stream instruction-name))
      (loop while (instruction-character-at-p source position) do
        (write-char (char source position) instruction-name)
        (incf position))
      (the (values instruction-type fixnum)
        (values
          (get-instruction-type
            (get-output-stream-string instruction-name))
          position)))))

;;; -------------------------------------------------------

(defun expect-end-of-line (source start)
  "Proceeding from the START position in the SOURCE, determines whether
   the remaining content is destitute of effective elements, that is,
   no instructions follow, while contingent spaces and a comment may
   participate therein, on confirmation returning no value; otherwise
   signaling an error of an unspecified type.
   ---
   This operation is intended to be invoked in the aftermath of an
   instruction's successful detection, in the pursuit to verify that no
   other instruction shares a row with the preceding one."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-spaces source start)))
    (declare (type fixnum position))
    (unless (or (end-of-source-p source position)
                (comment-starts-at-p source position))
      (error "Expected the end of the source, but encountered \"~c\" ~
              at position ~d."
        (char source position) position)))
  (values))

;;; -------------------------------------------------------

(defun extract-instruction (source)
  "Processes the SOURCE line and returns an ``Instruction''
   representation thereof.
   ---
   Blank and comment lines produce a ``NOP-Instruction'', whose
   induction into the ultimate A?! program sequence may not be
   desiderated."
  (declare (type string source))
  (let ((position (skip-spaces source 0)))
    (declare (type fixnum position))
    (the Instruction
      (cond
        ((end-of-source-p source position)
          (make-nop-instruction))
        
        ((comment-starts-at-p source position)
          (make-nop-instruction))
        
        ((variable-name-at-p source position)
          (let ((variable         (char source position))
                (instruction-type :nop))
            (declare (type character        variable))
            (declare (type instruction-type instruction-type))
            (incf position)
            (setf (values instruction-type position)
              (read-instruction-type source position))
            (expect-end-of-line source position)
            (make-variable-instruction instruction-type variable)))
        
        ((character-at-equals-p source position #\>)
          (let ((repetitions 1))
            (declare (type positive-integer repetitions))
            (setf (values repetitions position)
              (count-repetitions source position #\>))
            (expect-end-of-line source position)
            (make-jump-instruction :jump-forward repetitions)))
        
        ((character-at-equals-p source position #\<)
          (let ((repetitions 1))
            (declare (type positive-integer repetitions))
            (setf (values repetitions position)
              (count-repetitions source position #\<))
            (expect-end-of-line source position)
            (make-jump-instruction :jump-back repetitions)))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            (char source position) position))))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of A?! source CODE a
   one-dimensional simplel array of the incorporated instructions.
   ---
   Only non-blank and non-comment lines contribute to the result."
  (declare (type string code))
  (with-input-from-string (code-stream code)
    (declare (type string-stream code-stream))
    (the A?!-program
      (coerce
        (loop
          for line
            of-type (or null string)
            =       (read-line code-stream NIL NIL)
          while line
          for instruction
            of-type Instruction
            =       (extract-instruction line)
          unless (eq (instruction-type instruction) :nop)
            collect instruction)
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable set.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-variable-table ()
  "Creates and returns a new variable table, this being an association
   list whose keys bear the variable names, each being allied with a
   single bit as its value, default to the zero (0) bit."
  (the variable-table
    (loop
      for     variable of-type character across +VARIABLE-NAMES+
      collect (cons variable 0))))

;;; -------------------------------------------------------

(defstruct (Variable-Set
  (:constructor make-variable-set ()))
  "The ``Variable-Set'' class maintains an associative structure of
   variable name-value pairs, designed as an association list that maps
   each character name to a bit value."
  (variables (build-variable-table) :type variable-table))

;;; -------------------------------------------------------

(defun get-variable-entry (variables name)
  "Returns the entry in the VARIABLES mapping which corresponds to the
   variable NAME, or signals an error an error of an unspecified type
   upon its disrespondency.
   ---
   The ``Variable-Set'' class' reliance upon an association list for its
   entries' stewardship conditions the successfully returned entry to
   be a character-bit cons, delivered as a reference, and thus amenable
   to both indagations and modifications that reflect in the VARIABLES
   registry with immediacy."
  (declare (type Variable-Set variables))
  (declare (type character    name))
  (the variable-table-entry
    (or (assoc name
          (variable-set-variables variables)
          :test #'char=)
        (error "Unrecognized variable name: ~s." name))))

;;; -------------------------------------------------------

(defun negate-variable (variables name)
  "Negates the bit stored in the VARIABLES registry under the NAME and
   returns no value."
  (declare (type Variable-Set variables))
  (declare (type character    name))
  (let ((entry (get-variable-entry variables name)))
    (declare (type variable-table-entry entry))
    (setf (cdr entry)
          (- 1 (cdr entry))))
  (values))

;;; -------------------------------------------------------

(defun get-variable (variables name)
  "Returns the bit stored in the VARIABLES registry under the NAME."
  (declare (type Variable-Set variables))
  (declare (type character    name))
  (the bit
    (cdr (get-variable-entry variables name))))

;;; -------------------------------------------------------

(defun set-variable (variables name new-value)
  "Stores the NEW-VALUE in the VARIABLES registry under the NAME and
   returns no value."
  (declare (type Variable-Set variables))
  (declare (type character    name))
  (declare (type bit          new-value))
  (let ((entry (get-variable-entry variables name)))
    (declare (type variable-table-entry entry))
    (setf (cdr entry) new-value))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of output buffer.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Output-Buffer
  (:constructor make-output-buffer (&key (destination T))))
  "The ``Output-Buffer'' class serves as an output conduit's
   representation, buffering the bits committed to it, and automatically
   issuing the same to the internally managed destination if a complete
   octet is available.
   ---
   The bit sequence's positions are filled from the most significant bit
   (MSB) to the least significant bit (LSB)."
  (bits        #b0 :type (unsigned-byte *))
  (size        0   :type non-negative-integer)
  (destination T   :type destination))

;;; -------------------------------------------------------

(defun output-buffer-complete-p (output-buffer)
  "Determines whether the OUTPUT-BUFFER is complete, that is, all eight
   bits requisite for representing an octet have been committed to the
   same, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Output-Buffer output-buffer))
  (the boolean
    (not (null
      (>= (output-buffer-size output-buffer) 8)))))

;;; -------------------------------------------------------

(defun purge-output-buffer (output-buffer)
  "Writes the OUTPUT-BUFFER's binary content to the internally managed
   destination in the form of the ASCII character whose code constitutes
   a tantamount of the bits' decimal value, resets the buffer's state,
   and returns no value."
  (declare (type Output-Buffer output-buffer))
  (write-char
    (code-char
      (output-buffer-bits output-buffer)))
  (setf (output-buffer-bits output-buffer) #b0)
  (setf (output-buffer-size output-buffer) 0)
  (values))

;;; -------------------------------------------------------

(defun put-bit-into-output-buffer (output-buffer bit)
  "Writes the BIT to the OUTPUT-BUFFER, contingently issuing the display
   of the ASCII character corresponding to the new buffer bits and a
   purging, if the buffer has been completed by this invocation, in any
   case returning no value.
   ---
   Each new bit is inserted nearer to the least significant bit (LSB)
   location in the buffer than it predecessor."
  (declare (type Output-Buffer output-buffer))
  (declare (type bit        bit))
  (setf (ldb (byte 1 (- 7 (output-buffer-size output-buffer)))
             (output-buffer-bits output-buffer))
        bit)
  (incf (output-buffer-size output-buffer))
  (when (output-buffer-complete-p output-buffer)
    (purge-output-buffer output-buffer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 5) +DEFAULT-PROMPT+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-PROMPT+ "~&>> "
  "Specifies the default prompt message for input operations.")

;;; -------------------------------------------------------

(defstruct (Interpreter
  (:constructor make-interpreter
    (instructions
     &key (input-stream *standard-input*)
          (prompt       +DEFAULT-PROMPT+))))
  "The ``Interpreter'' class encapsulates the A?! program and the
   appertaining processing state."
  (instructions  (error "Missing instructions.") :type A?!-program)
  (ip            0                               :type fixnum)
  (variables     (make-variable-set)             :type Variable-Set)
  (output-stream (make-output-buffer)            :type Output-Buffer)
  (input-stream  *standard-input*                :type stream)
  (prompt        +DEFAULT-PROMPT+                :type string))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Moves the INTERPRETER's instruction pointer (IP) to the next location
   and returns no value."
  (declare (type Interpreter interpreter))
  (incf (interpreter-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun move-ip-to (interpreter new-line)
  "Relocates the INTERPRETER's instruction pointer (IP) to the NEW-LINE
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-line))
  (setf (interpreter-ip interpreter) new-line)
  (values))

;;; -------------------------------------------------------

(defun query-bit (interpreter)
  "Queries the INTERPRETER's input stream for a single bit, on success
   either returning the bit, or ``NIL'' upon the inquisition conduit's
   exhaustion.
   ---
   If an invalid object emanates from the response, an error of an
   unspecified type is signaled."
  (declare (type Interpreter interpreter))
  (format T (interpreter-prompt interpreter))
  (let ((input
          (read-char
            (interpreter-input-stream interpreter)
            NIL)))
    (declare (type (or null character) input))
    (clear-input)
    (the (or null bit)
      (case input
        ((NIL)
          NIL)
        ((#\0 #\1)
          (digit-char-p input 2))
        (otherwise
          (error "Invalid input: ~s. Expected a bit (0 or 1) or EOF."
            input))))))

;;; -------------------------------------------------------

(defun terminate-program (interpreter)
  "Terminates the program stored in the INTERPRETER by relocating its
   instruction pointer (IP) beyond the desinent instruction and returns
   no value."
  (declare (type Interpreter interpreter))
  (move-ip-to interpreter
    (length (interpreter-instructions interpreter)))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-instruction (interpreter
                                  instruction-type
                                  instruction)
  (:documentation
    "Processes the INSTRUCTION in the INTERPRETER's context, dispatched
     on the INSTRUCTION-TYPE, and returns no value."))

;;; -------------------------------------------------------

(defun process-instruction (interpreter instruction)
  "Processes the INSTRUCTION in the INTERPRETER's context by invoking
   the eligible ``dispatch-instruction'' generic function implementation
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (dispatch-instruction interpreter
    (instruction-type instruction)
    instruction)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction
    ((interpreter      Interpreter)
     (instruction-type (eql :negate-variable))
     (instruction      Instruction))
  "Negates the value of the bit stored in the INTERPRETER's variable
   designated by the INSTRUCTION's variable name and returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (negate-variable
    (interpreter-variables interpreter)
    (variable-instruction-variable instruction))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :output))
                                 (instruction      Instruction))
  "Writes the value of the bit stored in the INTERPRETER's variable
   designated by the INSTRUCTION's variable name, potentially issuing a
   character to the standard output conduit, and returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (put-bit-into-output-buffer
    (interpreter-output-stream interpreter)
    (get-variable
      (interpreter-variables interpreter)
      (variable-instruction-variable instruction)))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :input))
                                 (instruction      Instruction))
  "Queries the standard input for an aefauld bit, stores the same in the
   INTERPRETER's variable designated by the INSTRUCTION's variable name,
   and returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (let ((input (query-bit interpreter)))
    (declare (type (or null bit) input))
    (cond
      (input
        (set-variable
          (interpreter-variables interpreter)
          (variable-instruction-variable instruction)
          input)
        (advance-ip interpreter))
      (T
        (terminate-program interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :skip-if-false))
                                 (instruction      Instruction))
  "Skips the next instruction if the INTERPRETER's variable designated
   by the INSTRUCTION's variable name constitutes a zero (0) bit, and
   returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (when (zerop (get-variable
                 (interpreter-variables interpreter)
                 (variable-instruction-variable instruction)))
    (advance-ip interpreter))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :jump-forward))
                                 (instruction      Instruction))
  "Skips a number of succeeding instructions mantained by the
   INTERPRETER tantamount to the INSTRUCTION's motion distance and
   returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (move-ip-to interpreter
    (+ (interpreter-ip            interpreter)
       (jump-instruction-distance instruction)
       1))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :jump-back))
                                 (instruction      Instruction))
  "Retrogrades the INTERPRETER's instruction pointer a number of
   positions tantamount to the INSTRUCTION's motion distance and returns
   no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (move-ip-to interpreter
    (- (interpreter-ip            interpreter)
       (jump-instruction-distance instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :nop))
                                 (instruction      Instruction))
  "Processes the no-operation INSTRUCTION in the INTERPRTER's context,
   which amounts to a no causatum, instead merely advancing the
   INTERPRETER's instruction pointer (IP), and returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type instruction-type instruction-type))
  (declare (ignore                instruction-type))
  (declare (type Instruction      instruction))
  (declare (ignore                instruction))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's instruction set is exhausted,
   that is, its instruction pointer (IP) has been relocated beyond the
   instruction set's boundaries, returning on confirmation a ``boolean''
   of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length (interpreter-instructions interpreter)))))))

;;; -------------------------------------------------------

(defun process-instructions (interpreter)
  "Processes the A?! program stored in the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop until (program-exhausted-p interpreter) do
    (process-instruction interpreter
      (aref (interpreter-instructions interpreter)
        (interpreter-ip interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpret-A?! (code
                      &key (input-stream *standard-input*)
                           (prompt       +DEFAULT-PROMPT+))
  "Interprets the piece of A?! source CODE, querying bits from the
   INPUT-STREAM if necessary, preceded by the PROMPT message, and
   returns no value."
  (declare (type string code))
  (process-instructions
    (make-interpreter
      (extract-instructions code)
      :input-stream input-stream
      :prompt       prompt))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
;; 
;; As an example, entering the following eight bits, in sinistrodextral
;; airt
;; 
;;   0 1 0 0 0 0 0 1
;;   ^             ^
;;   |             |
;;   |             | least significant bit
;;   |
;;   most significant bit
;; 
;; defines the decimal number 65, which produces the corresponding
;; ASCII character "A".
(interpret-A?!
  "
  A...
  B...
  C...
  D...
  E...
  F...
  G...
  H...
  A.
  B.
  C.
  D.
  E.
  F.
  G.
  H.
  <<<<<<<<<<<<<<<<
  ")

;;; -------------------------------------------------------

;; Increment a single-digit number, wrapping around on an input that
;; corresponds to the digit nine (9).
(interpret-A?!
  "
  #Input
  A...
  B...
  C...
  D...
  E...
  F...
  G...
  H...

  #Increment
  H?
  >
  >>>>>>>
  G?
  >
  >>>
  F?
  E!
  F!
  G!
  H!

  #Wrap-around
  E?
  >
  >>>>>
  G?
  >
  >>
  E!
  G!

  #Print
  A.
  B.
  C.
  D.
  E.
  F.
  G.
  H.
  ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-A?!
  "
  # The variable 'A' stores the bit value zero (0).
  A...
  A...
  # The variable 'B' stores the bit value one (1).
  B...
  B...
  # The variable 'C', through the last input, stores either 0 or 1.
  C...
  C...
  C...
  C...
  
  # Print the input as an ASCII character.
  A.
  A.
  B.
  B.
  A.
  A. 
  A.
  C.
  
  # If the user input was 0, skip the iterative section.
  C?
  # This instruction, encountered for a 1 input, jumps to the loop.
  >
  # This instruction, encountered for a 0 input, skips the coming loop.
  >>>>>>>>>
  
  # Start of the iterative printing section for an input of 1.
  A.
  A.
  B.
  B.
  A.
  A. 
  A.
  C.
  <<<<<<<<
  ")

;;; -------------------------------------------------------

;; Truth-machine which obtains its input from a predefined string and
;; issues no prompt message.
(with-input-from-string (input "00110001")
  (declare (type string-stream input))
  (interpret-A?!
    "
    # The variable 'A' stores the bit value zero (0).
    A...
    A...
    # The variable 'B' stores the bit value one (1).
    B...
    B...
    # The variable 'C', through the last input, stores either 0 or 1.
    C...
    C...
    C...
    C...
    
    # Print the input as an ASCII character.
    A.
    A.
    B.
    B.
    A.
    A. 
    A.
    C.
    
    # If the user input was 0, skip the iterative section.
    C?
    # This instruction, encountered for a 1 input, jumps to the loop.
    >
    # This instruction, encountered for a 0 input, skips the coming loop.
    >>>>>>>>>
    
    # Start of the iterative printing section for an input of 1.
    A.
    A.
    B.
    B.
    A.
    A. 
    A.
    C.
    <<<<<<<<
    "
    :input-stream input
    :prompt       ""))

;;; -------------------------------------------------------

;; Infinite loop which skips the intermediate bit negation instruction.
(interpret-A?!
  ">
   A! #This instruction will never be executed.
   <<")
