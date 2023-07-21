;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "PTotE", invented by the Esolang user "None1" and presented
;; in the year 2023, the diorism of which is founded upon the encoding
;; of ASCII characters in the form of chemical element symbols.
;; 
;; 
;; Concept
;; =======
;; The PTotE programming language defines a decoding of plaintext ASCII
;; characters in a sequence of zero or more chemical element symbols,
;; delineated by whitespaces.
;; 
;; == THE [P]ERIOD [T]ABLE [O]F [T]HE [E]ELEMENTS ==
;; The language's nevening, "PTotE", ensues from its conceptually
;; foundry, expanding into "The Periodic Table of the Elements" --- an
;; intimation of the chemical elements' properties, especially their
;; abbreviating symbols and atomic numbers, whence the decoding into
;; plaintext advances.
;; 
;; == THE PERIODIC TABLE OF ELEMENTS: AN APERCU IN TWO DIMENSIONS ==
;; The periodic table of the elements, also known in its abbreviated
;; nomenclature as simply the "periodic table", establishes a tabular
;; apercu regarding the chemical elements acquainted with man's
;; current state of knowledge, in conjunction with a subset of its most
;; important properties.
;; 
;; The regular reticulation conditions the appearance of two axes: the
;; rows, or "periods", and the columns, or "groups".
;; 
;; The first species, extending in a horizontal line, ligates with a
;; conceptual conspectuity an electron's highest energy level per
;; element.
;; 
;; The vertically extending column aggregation relates to considerations
;; regarding the valance electron configurations, whence ensues a
;; consanguinity in its members' chemical properties and behavioral
;; expressions.
;; 
;; As stated further aboon, the table's ordonnance appertains in its
;; ultimity to the single elements' cursory perquisition, expressing
;; each such item's componency by a quadruple of attributes:
;; 
;;   (1) Atomic number:
;;       The tally of protons in an atom's nucleus. This serves as the
;;       paravant identification token and ordering criterion for the
;;       table.
;;   
;;   (2) Symbol:
;;       An abbreviation of the name, which please see below, composed
;;       of one or two Latin letters, the incipient constituent of which
;;       always produces a majuscle, followed by the contingent second
;;       moeity in a minuscular format.
;;   
;;   (3) Name:
;;       The element's full name.
;;   
;;   (4) Average atomic mass:
;;       Expresses the sum of the isotopes' masses for an element.
;; 
;; To our ambit's conspectuity merely the atomic number and the symbol
;; contribute significance.
;; 
;; == PTotE: AN ENCODING ==
;; In its most basic interpretation, PTotE establishes an encoding for
;; ASCII characters, substituting these by chemical element symbols.
;; Founded upon its affiliation with the atomic numbers, which occupy at
;; the current state the proton tallies from inclusive one (1) to
;; inclusive 118, the ASCII code range's amenability is contrained to
;; the closed interval [10, 127].
;; 
;; The one- or two-letter element symbols are substituted by respective
;; atomic numbers, succeeded by a translation of this input range of
;; [1, 118] to the replicable ASCII code subrange [10, 127], and
;; concluded with the ASCII code transcriptions into actual letters.
;; 
;; Concretely, the following process applies for each token symbol_i,
;; this being an element symbol extracted from the piece of PTotE code:
;; 
;;   (1) Retrieve for the element symbol symbol_1 the element's atomic
;;       number, an integer in the range [1, 118], in accordance with
;;       the periodic table of the elements. This numeric equivalent
;;       shall be nevened "atomicNumber_i":
;;         atomicNumber_i <- atomic number of element for symbol_i
;;   
;;   (2) Convert the atomicNumber_i into an ASCII character code from
;;       the range [10, 127], designated as "asciiCode_i", by adherence
;;       to the formula
;;         asciiCode_i <- atomicNumber_i + 10 - 1
;;   
;;   (3) Print to the standard output the character, here agnominated
;;       "asciiCharacter_i", corresponding to the ASCII code
;;       asciiCode_i, as according to the ASCII character standard:
;;         asciiCharacter_i <- character for asciiCode_i
;;         print asciiCharacter_i
;; 
;; == DECODING EQUALS INTERPREATION ==
;; The interpretation and decoding of PTotE programs empight a instance
;; of confluence, proceeding by garnering the whitespace-separated
;; tokens, which represent chemical element symbols, resolving these to
;; their atomic numbers, whence a formula's dedication begets an
;; equinumerant sequence of ASCIIs code whose affiliated characters will
;; be displayed on the system's standard input.
;; 
;; With augmented lealty to formal diction, given the table of chemical
;; elements, "ELEMENTS", which unambiguously identifies each chemical
;; element, composed of the triple
;; 
;;   (symbol, name, atomicNumber),
;; 
;; where
;; 
;;   symbol       --- an abbreviating identifier for the element,
;;                    described by a string
;;   name         --- the element's full name, described by a string
;;   atomicNumber --- a positive integer equal to the number of
;;                    protons or electrons in the element.
;; 
;; by its name, the following pseudocode holds for the decoding:
;; 
;;   { Returns the atomic number of the chemical element identified by }
;;   { the "elementSymbol".                                            }
;;   function getAtomicNumber (elementSymbol)
;;     let elementForSymbol <- ELEMENTS(elementSymbol)
;;     let atomicNumber     <- elementForSymbol(atomicNumber)
;;     return atomicNumber
;;   end function
;;   
;;   { Returns the ASCII character code answering to the }
;;   { "atomicNumber".                                   }
;;   function decodeAtomicNumber (atomicNumber)
;;     let asciiCode <- atomicNumber + 10 - 1
;;     return asciiCode
;;   end function
;;   
;;   { Decodes a sequence of chemical element symbols into the        }
;;   { corresponding ASCII characters and prints them to the standard }
;;   { output.                                                        }
;;   procedure decodePTotE (pTotECode)
;;     for elementSymbol in pTotECode do
;;       let atomicNumber   <- getAtomicNumber(elementSymbol)
;;       let asciiCode      <- decodeAtomicNumber(atomicNumber)
;;       let asciiCharacter <- get character for asciiCode
;;       
;;       print asciiCharacter
;;     end for
;;   end procedure
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The particular meticulousness adhibited to the protolog darrains its
;; diorisms from contingent ambiguities.
;; 
;; 
;; Implementation
;; ==============
;; This simple interpreter and its encoder are implemented in the
;; programming language Common Lisp.
;; 
;; 
;; Appendices
;; ==========
;; The following epexegetical topics wone in the conceptual intermede of
;; pertinence, governed by a twifaced assessment, imprimis as attributed
;; with insufficiency in relevancy as to enjoy admission into the main
;; text body; yet, endowed with a mete of significance whose fortitude
;; vindicates some ilk of presentation.
;; 
;; == APPENDIX A: ASCII CHARACTERS TO ELEMENTS MAPPING ==
;; A tabular juxtaposition shall serve in equiparating the ASCII code
;; range tolerated by the PTotE language, namely [10, 127], to the
;; element symbols and their atomic numbers, pursuing an adminiculum's
;; adduction for those interested in the generation of plaintext
;; messages.
;; 
;;   -------------------------------------------------------------
;;   ASCII character | ASCII code | Element symbol | Atomic number
;;   ----------------+------------+----------------+--------------
;;   (line feed)     |     10     |       H        |       1     
;;   ................|............|................|..............
;;   (vertical tab)  |     11     |       He       |       2     
;;   ................|............|................|..............
;;   (form feed)     |     12     |       Li       |       3     
;;   ................|............|................|..............
;;   (carriage ret.) |     13     |       Be       |       4     
;;   ................|............|................|..............
;;   (shift out)     |     14     |       B        |       5     
;;   ................|............|................|..............
;;   (shift in)      |     15     |       C        |       6     
;;   ................|............|................|..............
;;   (data link es.) |     16     |       N        |       7     
;;   ................|............|................|..............
;;   (dev. ctrl. 1)  |     17     |       O        |       8     
;;   ................|............|................|..............
;;   (dev. ctrl. 2)  |     18     |       F        |       9     
;;   ................|............|................|..............
;;   (dev. ctrl. 3)  |     19     |       Ne       |      10     
;;   ................|............|................|..............
;;   (dev. ctrl. 4)  |     20     |       Na       |      11     
;;   ................|............|................|..............
;;   (neg. ack.)     |     21     |       Mg       |      12     
;;   ................|............|................|..............
;;   (syn. idle)     |     22     |       Al       |      13     
;;   ................|............|................|..............
;;   (end of tr. b.) |     23     |       Si       |      14     
;;   ................|............|................|..............
;;   (cancel)        |     24     |       P        |      15     
;;   ................|............|................|..............
;;   (end of medium) |     25     |       S        |      16     
;;   ................|............|................|..............
;;   (substitute)    |     26     |       Cl       |      17     
;;   ................|............|................|..............
;;   (escape)        |     27     |       Ar       |      18     
;;   ................|............|................|..............
;;   (file sep.)     |     28     |       K        |      19     
;;   ................|............|................|..............
;;   (group sep.)    |     29     |       Ca       |      20     
;;   ................|............|................|..............
;;   (record sep.)   |     30     |       Sc       |      21     
;;   ................|............|................|..............
;;   (unit sep.)     |     31     |       Ti       |      22     
;;   ................|............|................|..............
;;   (space)         |     32     |       V        |      23     
;;   ................|............|................|..............
;;   !               |     33     |       Cr       |      24     
;;   ................|............|................|..............
;;   "               |     34     |       Mn       |      25     
;;   ................|............|................|..............
;;   #               |     35     |       Fe       |      26     
;;   ................|............|................|..............
;;   $               |     36     |       Co       |      27     
;;   ................|............|................|..............
;;   %               |     37     |       Ni       |      28     
;;   ................|............|................|..............
;;   &               |     38     |       Cu       |      29     
;;   ................|............|................|..............
;;   '               |     39     |       Zn       |      30     
;;   ................|............|................|..............
;;   (               |     40     |       Ga       |      31     
;;   ................|............|................|..............
;;   )               |     41     |       Ge       |      32     
;;   ................|............|................|..............
;;   *               |     42     |       As       |      33     
;;   ................|............|................|..............
;;   +               |     43     |       Se       |      34     
;;   ................|............|................|..............
;;   ,               |     44     |       Br       |      35     
;;   ................|............|................|..............
;;   -               |     45     |       Kr       |      36     
;;   ................|............|................|..............
;;   .               |     46     |       Rb       |      37     
;;   ................|............|................|..............
;;   /               |     47     |       Sr       |      38     
;;   ................|............|................|..............
;;   0               |     48     |       Y        |      39     
;;   ................|............|................|..............
;;   1               |     49     |       Zr       |      40     
;;   ................|............|................|..............
;;   2               |     50     |       Nb       |      41     
;;   ................|............|................|..............
;;   3               |     51     |       Mo       |      42     
;;   ................|............|................|..............
;;   4               |     52     |       Tc       |      43     
;;   ................|............|................|..............
;;   5               |     53     |       Ru       |      44     
;;   ................|............|................|..............
;;   6               |     54     |       Rh       |      45     
;;   ................|............|................|..............
;;   7               |     55     |       Pd       |      46     
;;   ................|............|................|..............
;;   8               |     56     |       Ag       |      47     
;;   ................|............|................|..............
;;   9               |     57     |       Cd       |      48     
;;   ................|............|................|..............
;;   :               |     58     |       In       |      49     
;;   ................|............|................|..............
;;   ;               |     59     |       Sn       |      50     
;;   ................|............|................|..............
;;   <               |     60     |       Sb       |      51     
;;   ................|............|................|..............
;;   =               |     61     |       Te       |      52     
;;   ................|............|................|..............
;;   >               |     62     |       I        |      53     
;;   ................|............|................|..............
;;   ?               |     63     |       Xe       |      54     
;;   ................|............|................|..............
;;   @               |     64     |       Cs       |      55     
;;   ................|............|................|..............
;;   A               |     65     |       Ba       |      56     
;;   ................|............|................|..............
;;   B               |     66     |       La       |      57     
;;   ................|............|................|..............
;;   C               |     67     |       Ce       |      58     
;;   ................|............|................|..............
;;   D               |     68     |       Pr       |      59     
;;   ................|............|................|..............
;;   E               |     69     |       Nd       |      60     
;;   ................|............|................|..............
;;   F               |     70     |       Pm       |      61     
;;   ................|............|................|..............
;;   G               |     71     |       Sm       |      62     
;;   ................|............|................|..............
;;   H               |     72     |       Eu       |      63     
;;   ................|............|................|..............
;;   I               |     73     |       Gd       |      64     
;;   ................|............|................|..............
;;   J               |     74     |       Tb       |      65     
;;   ................|............|................|..............
;;   K               |     75     |       Dy       |      66     
;;   ................|............|................|..............
;;   L               |     76     |       Ho       |      67     
;;   ................|............|................|..............
;;   M               |     77     |       Er       |      68     
;;   ................|............|................|..............
;;   N               |     78     |       Tm       |      69     
;;   ................|............|................|..............
;;   O               |     79     |       Yb       |      70     
;;   ................|............|................|..............
;;   P               |     80     |       Lu       |      71     
;;   ................|............|................|..............
;;   Q               |     81     |       Hf       |      72     
;;   ................|............|................|..............
;;   R               |     82     |       Ta       |      73     
;;   ................|............|................|..............
;;   S               |     83     |       W        |      74     
;;   ................|............|................|..............
;;   T               |     84     |       Re       |      75     
;;   ................|............|................|..............
;;   U               |     85     |       Os       |      76     
;;   ................|............|................|..............
;;   V               |     86     |       Ir       |      77     
;;   ................|............|................|..............
;;   W               |     87     |       Pt       |      78     
;;   ................|............|................|..............
;;   X               |     88     |       Au       |      79     
;;   ................|............|................|..............
;;   Y               |     89     |       Hg       |      80     
;;   ................|............|................|..............
;;   Z               |     90     |       Tl       |      81     
;;   ................|............|................|..............
;;   [               |     91     |       Pb       |      82     
;;   ................|............|................|..............
;;   \               |     92     |       Bi       |      83     
;;   ................|............|................|..............
;;   ]               |     93     |       Po       |      84     
;;   ................|............|................|..............
;;   ^               |     94     |       At       |      85     
;;   ................|............|................|..............
;;   _               |     95     |       Rn       |      86     
;;   ................|............|................|..............
;;   `               |     96     |       Fr       |      87     
;;   ................|............|................|..............
;;   a               |     97     |       Ra       |      88     
;;   ................|............|................|..............
;;   b               |     98     |       Ac       |      89     
;;   ................|............|................|..............
;;   c               |     99     |       Th       |      90     
;;   ................|............|................|..............
;;   d               |    100     |       Pa       |      91     
;;   ................|............|................|..............
;;   e               |    101     |       U        |      92     
;;   ................|............|................|..............
;;   f               |    102     |       Np       |      93     
;;   ................|............|................|..............
;;   g               |    103     |       Pu       |      94     
;;   ................|............|................|..............
;;   h               |    104     |       Am       |      95     
;;   ................|............|................|..............
;;   i               |    105     |       Cm       |      96     
;;   ................|............|................|..............
;;   j               |    106     |       Bk       |      97     
;;   ................|............|................|..............
;;   k               |    107     |       Cf       |      98     
;;   ................|............|................|..............
;;   l               |    108     |       Es       |      99     
;;   ................|............|................|..............
;;   m               |    109     |       Fm       |     100     
;;   ................|............|................|..............
;;   n               |    110     |       Md       |     101     
;;   ................|............|................|..............
;;   o               |    111     |       No       |     102     
;;   ................|............|................|..............
;;   p               |    112     |       Lr       |     103     
;;   ................|............|................|..............
;;   q               |    113     |       Rf       |     104     
;;   ................|............|................|..............
;;   r               |    114     |       Db       |     105     
;;   ................|............|................|..............
;;   s               |    115     |       Sg       |     106     
;;   ................|............|................|..............
;;   t               |    116     |       Bh       |     107     
;;   ................|............|................|..............
;;   u               |    117     |       Hs       |     108     
;;   ................|............|................|..............
;;   v               |    118     |       Mt       |     109     
;;   ................|............|................|..............
;;   w               |    119     |       Ds       |     110     
;;   ................|............|................|..............
;;   x               |    120     |       Rg       |     111     
;;   ................|............|................|..............
;;   y               |    121     |       Cn       |     112     
;;   ................|............|................|..............
;;   z               |    122     |       Nh       |     113     
;;   ................|............|................|..............
;;   {               |    123     |       Fl       |     114     
;;   ................|............|................|..............
;;   |               |    124     |       Mc       |     115     
;;   ................|............|................|..............
;;   }               |    125     |       Lv       |     116     
;;   ................|............|................|..............
;;   ~               |    126     |       Ts       |     117     
;;   ................|............|................|..............
;;   (DEL)           |    127     |       Og       |     118     
;;   -------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-19
;; 
;; Sources:
;;   [acs2023periodictable]
;;   American Chemical Society (ACS),
;;     "Periodic Table of Elements - American Chemical Society", 2023
;;   URL: "https://www.acs.org/education/whatischemistry/
;;         periodictable.html"
;;   Notes:
;;     - Provides information about the periodic table of the elements,
;;       including the structure of each table cell.
;;     - Presents the same table in several formats for viewing and
;;       downloading.
;;   
;;   [boudreaux2021atomicNumbers]
;;   Kevin A. Boudreaux , "Atomic Numbers", 2021
;;   URL: "https://www.angelo.edu/faculty/kboudrea/periodic/
;;         structure_numbers.htm"
;;   Notes:
;;     - Describes the periodic table of the elements.
;;     - Lists the chemical elements in ascending order of their atomic
;;       numbers.
;;   
;;   [esolang2023PTotE]
;;   The Esolang contributors, "PTotE", 2023
;;   URL: "https://esolangs.org/wiki/PTotE"
;;   
;;   [losAlamos2021ptoeAbout]
;;   Los Alamos National Laboratory,
;;     "Periodic Table of Elements: Los Alamos National Laboratory",
;;     2021
;;   URL: "https://periodic.lanl.gov/about.shtml"
;;   Notes:
;;     - Provides information about the periodic table of the elements.
;;   
;;   [losAlamos2021ptoeList]
;;   Los Alamos National Laboratory,
;;     "Periodic Table of Elements: Los Alamos National Laboratory",
;;     2021
;;   URL: "https://periodic.lanl.gov/list.shtml"
;;   Notes:
;;     - Lists the elements with their symbol and atomic number.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tokenizer ()
  "The ``tokenizer'' type defines a niladic function which upon its
   invocation returns the next token from its source, this either
   constituting a string representation of a whitespace-delimited word
   or, upon the source exhaustion, the ``NIL'' object."
  '(function () (or null string)))

;;; -------------------------------------------------------

(deftype tokenizer-function ()
  "The ``tokenizer-function'' type defines a ``tokenizer'' in a form
   eligible for type specifiers, that is, as a general ``function'',
   omitting the argument list and result type definitions."
  'function)

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer number greater than
   or equal to one (1), fitten, in particular, for the representation of
   the chemical elements' atomic numbers."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype symbol-table ()
  "The ``symbol-table'' type defines an association of chemical element
   symbols to the respective element representations in the form of a
   hash table, mapping string keys to ``Element'' values."
  '(hash-table-of string Element))

;;; -------------------------------------------------------

(deftype atomic-number-table ()
  "The ``atomic-number-table'' type defines an association of chemical
   element atomic numbers to the respective element representations in
   the form of a hash table, mapping positive integer keys to
   ``Element'' values."
  '(hash-table-of positive-integer Element))

;;; -------------------------------------------------------

(deftype ascii-code ()
  "The ``ascii-code'' type defines the range of ASCII character codes
   admissive to the PTotE decoding and encoding process, namely the
   closed integer interval [10, 127]."
  '(integer 10 127))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun end-of-word-p (source position)
  "Determines whether the POSITION into the SOURCE designates the end
   of a word, which encompasses either a location outside of the
   SOURCE's boundaries or a whitespace character, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (null
      (or
        (>= position (length source))
        (whitespace-character-p
          (char source position)))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespace characters and returns the
   position into the SOURCE immediately following the first
   non-whitespace entity."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for position of-type fixnum from start below (length source)
      while (whitespace-character-p (char source position))
      finally
        (return position))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, reads a word,
   delimited by whitespaces or the end of the SOURCE, and returns two
   values:
     (1) The thus consumed word as a string.
     (2) The position into the SOURCE immediately succeeding the
         desinent character of the consumed word."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (word (make-string-output-stream))
      (declare (type string-stream word))
      (loop
        for   position of-type fixnum from start by 1
        until (end-of-word-p source position)
        do    (write-char (char source position) word)
        finally
          (return
            (values
              (get-output-stream-string word)
              position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tokenizer.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tokenizer (source)
  "Returns a ``tokenizer'' which analyzes the SOURCE while pursuing to
   extract its words.
   ---
   The thus created ``tokenizer'' object constitutes a niladic function
   that upon each invocation responds with the next word from the
   SOURCE, these token beings delineated by whitespaces. Upon its
   SOURCE's, any request is answered with the ``NIL'' value. The
   signature hence conforms to:
     lambda () => (or null string)"
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (the tokenizer-function
      #'(lambda ()
          (setf position (skip-whitespaces source position))
          (the (or null string)
            (when (< position (length source))
              (multiple-value-bind (word new-position)
                  (read-word source position)
                (declare (type string word))
                (declare (type fixnum new-position))
                (setf position new-position)
                word)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition PTotE-Error ()
  ()
  (:documentation
    "The ``PTotE-Error'' condition serves as the foundry of all errors
     appertaining to the lexical analyzation, parsing, or interpretation
     of a piece of PTotE source code."))

;;; -------------------------------------------------------

(define-condition Invalid-Element-Symbol-Error (PTotE-Error)
  ((symbol
    :initarg       :symbol
    :initform      (error "Missing element symbol.")
    :reader        invalid-element-symbol-error-symbol
    :type          string
    :documentation "The unrecognized symbol."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Element-Symbol-Error condition))
      (declare (type destination                  stream))
      (format stream "Unrecognized element symbol: ~s."
        (invalid-element-symbol-error-symbol condition))))
  (:documentation
    "The ``Invalid-Element-Symbol-Error'' condition serves to signal an
     attempt to query an element by a symbol unrecognized by the
     employed element table."))

;;; -------------------------------------------------------

(define-condition Invalid-Atomic-Number-Error (PTotE-Error)
  ((atomic-number
    :initarg       :atomic-number
    :initform      (error "Missing atomic number.")
    :reader        invalid-atomic-number-error-atomic-number
    :type          atomic-number
    :documentation "The atomic number which cannot be affiliated with a
                    chemical element."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Atomic-Number-Error condition))
      (declare (type destination                 stream))
      (format stream "Unrecognized atomic number: ~d."
        (invalid-atomic-number-error-atomic-number condition))))
  (:documentation
    "The ``Invalid-Atomic-Number-Error'' conditions serves to signal an
     attempt to query an element by an atomic number unrecognized by the
     employed atomic number table."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of chemical element.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Element
  (:constructor make-element (symbol name atomic-number)))
  "The ``Element'' class represents a chemical element, compact of its
   identifying symbol, name, and atomic number.
   ---
   The fact of its slots' strigent immutability renders any instance of
   this structure class effectively immune to modifications."
  (symbol        (error "Missing symbol.")
                 :type      string
                 :read-only T)
  (name          (error "Missing name.")
                 :type      string
                 :read-only T)
  (atomic-number (error "Missing atomic number.")
                 :type      positive-integer
                 :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of type element table.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type symbol-table        +SYMBOL-TABLE+))
(declaim (type atomic-number-table +ATOMIC-NUMBER-TABLE+))

;;; -------------------------------------------------------

(defparameter +SYMBOL-TABLE+
  (make-hash-table :test #'equal)
  "Associates the recognized element symbols with representative
   chemical ``Element'' instances.")

(defparameter +ATOMIC-NUMBER-TABLE+
  (make-hash-table :test #'eql)
  "Associates the valid atomic numbers with representative chemical
   ``Element'' instances.")

;;; -------------------------------------------------------

(flet ((register-element (symbol name atomic-number)
        "Creates a new ``Element'' composed of the SYMBOL, NAME, and
         ATOMIC-NUMBER, registers the same at the +SYMBOL-TABLE+
         utilizing the SYMBOL as the entry key, and at the
         +ATOMIC-NUMBER-TABLE+ using the ATOMIC-NUMBER as the indicator,
         and returns no value."
        (declare (type string           symbol))
        (declare (type string           name))
        (declare (type positive-integer atomic-number))
        (let ((new-element (make-element symbol name atomic-number)))
          (declare (type Element new-element))
          (setf (gethash symbol +SYMBOL-TABLE+)
                new-element)
          (setf (gethash atomic-number +ATOMIC-NUMBER-TABLE+)
                new-element))
        (values)))
  (register-element "H"  "Hydrogen"      1)
  (register-element "He" "Helium"        2)
  (register-element "Li" "Lithium"       3)
  (register-element "Be" "Beryllium"     4)
  (register-element "B"  "Boron"         5)
  (register-element "C"  "Carbon"        6)
  (register-element "N"  "Nitrogen"      7)
  (register-element "O"  "Oxygen"        8)
  (register-element "F"  "Fluorine"      9)
  (register-element "Ne" "Neon"          10)
  (register-element "Na" "Sodium"        11)
  (register-element "Mg" "Magnesium"     12)
  (register-element "Al" "Aluminum"      13)
  (register-element "Si" "Silicon"       14)
  (register-element "P"  "Phosphorus"    15)
  (register-element "S"  "Sulfur"        16)
  (register-element "Cl" "Chlorine"      17)
  (register-element "Ar" "Argon"         18)
  (register-element "K"  "Potassium"     19)
  (register-element "Ca" "Calcium"       20)
  (register-element "Sc" "Scandium"      21)
  (register-element "Ti" "Titanium"      22)
  (register-element "V"  "Vanadium"      23)
  (register-element "Cr" "Chromium"      24)
  (register-element "Mn" "Manganese"     25)
  (register-element "Fe" "Iron"          26)
  (register-element "Co" "Cobalt"        27)
  (register-element "Ni" "Nickel"        28)
  (register-element "Cu" "Copper"        29)
  (register-element "Zn" "Zinc"          30)
  (register-element "Ga" "Gallium"       31)
  (register-element "Ge" "Germanium"     32)
  (register-element "As" "Arsenic"       33)
  (register-element "Se" "Selenium"      34)
  (register-element "Br" "Bromine"       35)
  (register-element "Kr" "Krypton"       36)
  (register-element "Rb" "Rubidium"      37)
  (register-element "Sr" "Strontium"     38)
  (register-element "Y"  "Yttrium"       39)
  (register-element "Zr" "Zirconium"     40)
  (register-element "Nb" "Niobium"       41)
  (register-element "Mo" "Molybdenum"    42)
  (register-element "Tc" "Technetium"    43)
  (register-element "Ru" "Ruthenium"     44)
  (register-element "Rh" "Rhodium"       45)
  (register-element "Pd" "Palladium"     46)
  (register-element "Ag" "Silver"        47)
  (register-element "Cd" "Cadmium"       48)
  (register-element "In" "Indium"        49)
  (register-element "Sn" "Tin"           50)
  (register-element "Sb" "Antimony"      51)
  (register-element "Te" "Tellurium"     52)
  (register-element "I"  "Iodine"        53)
  (register-element "Xe" "Xenon"         54)
  (register-element "Cs" "Cesium"        55)
  (register-element "Ba" "Barium"        56)
  (register-element "La" "Lanthanum"     57)
  (register-element "Ce" "Cerium"        58)
  (register-element "Pr" "Praseodymium"  59)
  (register-element "Nd" "Neodymium"     60)
  (register-element "Pm" "Promethium"    61)
  (register-element "Sm" "Samarium"      62)
  (register-element "Eu" "Europium"      63)
  (register-element "Gd" "Gadolinium"    64)
  (register-element "Tb" "Terbium"       65)
  (register-element "Dy" "Dysprosium"    66)
  (register-element "Ho" "Holmium"       67)
  (register-element "Er" "Erbium"        68)
  (register-element "Tm" "Thulium"       69)
  (register-element "Yb" "Ytterbium"     70)
  (register-element "Lu" "Lutetium"      71)
  (register-element "Hf" "Hafnium"       72)
  (register-element "Ta" "Tantalum"      73)
  (register-element "W"  "Tungsten"      74)
  (register-element "Re" "Rhenium"       75)
  (register-element "Os" "Osmium"        76)
  (register-element "Ir" "Iridium"       77)
  (register-element "Pt" "Platinum"      78)
  (register-element "Au" "Gold"          79)
  (register-element "Hg" "Mercury"       80)
  (register-element "Tl" "Thallium"      81)
  (register-element "Pb" "Lead"          82)
  (register-element "Bi" "Bismuth"       83)
  (register-element "Po" "Polonium"      84)
  (register-element "At" "Astatine"      85)
  (register-element "Rn" "Radon"         86)
  (register-element "Fr" "Francium"      87)
  (register-element "Ra" "Radium"        88)
  (register-element "Ac" "Actinium"      89)
  (register-element "Th" "Thorium"       90)
  (register-element "Pa" "Protactinium"  91)
  (register-element "U"  "Uranium"       92)
  (register-element "Np" "Neptunium"     93)
  (register-element "Pu" "Plutonium"     94)
  (register-element "Am" "Americium"     95)
  (register-element "Cm" "Curium"        96)
  (register-element "Bk" "Berkelium"     97)
  (register-element "Cf" "Californium"   98)
  (register-element "Es" "Einsteinium"   99)
  (register-element "Fm" "Fermium"       100)
  (register-element "Md" "Mendelevium"   101)
  (register-element "No" "Nobelium"      102)
  (register-element "Lr" "Lawrencium"    103)
  (register-element "Rf" "Rutherfordium" 104)
  (register-element "Db" "Dubnium"       105)
  (register-element "Sg" "Seaborgium"    106)
  (register-element "Ns" "Neilsborium"   107)
  (register-element "Bh" "Bohrium"       107)
  (register-element "Hs" "Hassium"       108)
  (register-element "Mt" "Meitnerium"    109)
  (register-element "Ds" "Darmstadtium"  110)
  (register-element "Rg" "Roentgenium"   111)
  (register-element "Cn" "Copernicium"   112)
  (register-element "Nh" "Nihonium"      113)
  (register-element "Fl" "Flerovium"     114)
  (register-element "Mc" "Moscovium"     115)
  (register-element "Lv" "Livermorium"   116)
  (register-element "Ts" "Tennessine"    117)
  (register-element "Og" "Oganesson"     118)
  (values))

;;; -------------------------------------------------------

(defun get-element-by-symbol (symbol)
  "Returns the element that answers to the SYMBOL, or signals an error
   of the type ``Invalid-Element-Symbol-Error'' upon its
   disrespondency."
  (declare (type string symbol))
  (the Element
    (or (gethash symbol +SYMBOL-TABLE+)
        (error 'Invalid-Element-Symbol-Error :symbol symbol))))

;;; -------------------------------------------------------

(defun get-atomic-number-by-symbol (symbol)
  "Returns the atomic number of the element that answers to the SYMBOL,
   or signals an error of the type ``Invalid-Element-Type-Error'' upon
   its disrespondency."
  (declare (type string symbol))
  (the positive-integer
    (element-atomic-number
      (get-element-by-symbol symbol))))

;;; -------------------------------------------------------

(defun get-element-by-atomic-number (atomic-number)
  "Returns the chemical element corresponding to the atomic number, or
   signals an error of the type ``Invalid-Atomic-Number-Error'' upon its
   disrespondency."
  (declare (type positive-integer atomic-number))
  (the Element
    (or (gethash atomic-number +ATOMIC-NUMBER-TABLE+)
        (error 'Invalid-Atomic-Number-Error
          :atomic-number atomic-number))))

;;; -------------------------------------------------------

(defun get-symbol-by-atomic-number (atomic-number)
  "Returns the abbreviating symbol of the chemical element corresponding
   to the ATOMIC-NUMBER, or signals an error of the type
   ``Invalid-Atomic-Number-Error'' upon its disrespondency."
  (declare (type positive-integer atomic-number))
  (the string
    (element-symbol
      (get-element-by-atomic-number atomic-number))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-ascii-code-for-symbol (symbol)
  "Returns the ASCII character code corresponding to the element symbol,
   or signals an error of the type ``Invalid-Element-Symbol-Error'' upon
   its disrespondency."
  (declare (type string symbol))
  (the positive-integer
    (1- (+ (get-atomic-number-by-symbol symbol) 10))))

;;; -------------------------------------------------------

(defun get-character-for-symbol (symbol)
  "Returns the character whose ASCII code corresponds to the element
   identified by the SYMBOL, following the atomic number decoding
   formula, or signals an error of the type
   ``Invalid-Element-Symbol-Error'' upon its disrespondency."
  (declare (type string symbol))
  (the character
    (code-char
      (get-ascii-code-for-symbol symbol))))

;;; -------------------------------------------------------

(defun interpret-PTotE (code)
  "Interprets the piece of PTotE source CODE and returns no value."
  (declare (type string code))
  (let ((tokenizer (make-tokenizer code)))
    (declare (type tokenizer tokenizer))
    (format T "~&")
    (loop
      for   token of-type (or null string) = (funcall tokenizer)
      while token
      do
        (format T "~c"
          (get-character-for-symbol token))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of encoder.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-atomic-number-for-ascii-code (ascii-code)
  "Returns the atomic number corresponding to the ASCII-CODE."
  (declare (type ascii-code ascii-code))
  (the positive-integer
    (- ascii-code 9)))

;;; -------------------------------------------------------

(defun get-atomic-number-for-character (character)
  "Returns the atomic number corresponding to the CHARACTER's ASCII
   code."
  (declare (type character character))
  (the positive-integer
    (get-atomic-number-for-ascii-code
      (char-code character))))

;;; -------------------------------------------------------

(defun get-symbol-for-character (character)
  "Returns the abbreviating symbol of the chemical element corresponding
   to the atomic number which proceeds from the CHARACTER's ASCII code,
   or signals an error of the type ``Invalid-Atomic-Number-Error'' upon
   its disrespondency."
  (declare (type character character))
  (the string
    (get-symbol-by-atomic-number
      (get-atomic-number-for-character character))))

;;; -------------------------------------------------------

(defun encode-plaintext (message &key (destination NIL))
  "Generates the PTotE program capable of encoding the plaintext MESSAGE
   and writes its source code to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type string      message))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for message-character
          of-type character
          across  message
        and first-character-p
          of-type boolean
          =       T
          then    NIL
        if first-character-p do
          (format destination "~&")
        else do
          (format destination " ")
        end
        do
          (format destination "~a"
            (get-symbol-for-character message-character)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (encode-plaintext message :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-PTotE "Eu U Es Es No V Pt No Db Es Pa Cr")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language "Nope.".
(interpret-PTotE "Tm No Lr U Rb")

;;; -------------------------------------------------------

;; Encodes the plaintext message "Hello World!" in PTotE's format and
;; prints the resulting code to the standard output, producing:
;; 
;;   Eu U Es Es No V Pt No Db Es Pa Cr
(encode-plaintext "Hello World!" :destination T)

;;; -------------------------------------------------------

;; Encodes the plaintext message "Hello World!" in PTotE's format and
;; executes the resulting code.
(interpret-PTotE
  (encode-plaintext "Hello World!"))
