;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Goedelang", invented by the Esolang user "TJC games", being
;; a derivative of the esoteric language "brainfuck" by Urban Mueller.
;; 
;; 
;; Concepts
;; ========
;; Goedelang's dioristic approach designates a program as a single
;; Goedel number, the prime factorization of which generates prime
;; numbers raised to certain exponents that, being nonnegative integers,
;; constitute the language's instructions.
;; 
;; An abridged duction shall be the forthcoming sections' cynosure, and
;; a humble warkloom for inchoate comprehension, with the beseechment of
;; the bibliography sources' further consultation for those readers
;; inspired with augmented curiosity.
;; 
;; == PRIME FACTORIZATION ==
;; Factorization in general describes the possibility to represent any
;; number as the product of factors, that is, split into constituents
;; which, when multiplied, build the original value. For example:
;;   
;;   40 = 1 * 40 = 40 * 1
;;      = 2 * 20 = 20 * 2
;;      = 4 * 10 = 10 * 4
;;      = 5 *  8 =  8 * 5
;; 
;; The same datum may be represented by a combination of different
;; numbers, while commutativity orders that the constituents in
;; different positions still account for an identical combination.
;; 
;; Prime factorization constitutes a specialized instance of this
;; concept by restricted the factors to be prime numbers, which may, of
;; course, occur multiple times. The fundamental theorem of arithmetic
;; postulates that for any positive integer there exists exactly one
;; combination of prime numbers which, when multiplied, generates this
;; original value. Prime numbers are subjects of such division are not
;; excluded, being represented by themselves as the sole factor. An
;; example shall be granted:
;;   
;;   40 = 2 * 2 * 2 * 5
;; 
;; If a prime number contributes multiple times to this product, it may
;; be noted by aid of exponentiation, that is, as the base raised to the
;; tally of its occurrences functioning as the power. In our case:
;;   
;;   40 = 2 * 2 * 2 * 5
;;      = 2^3       * 5
;; 
;; The prime number 2 occurs three (3) times, thus being representable
;; by the exponentiation 2^3 (= prime^{tally}).
;; 
;; == GOEDEL NUMBERS ==
;; The process of Goedel numbering permits the encoding of a plaintext
;; symbol sequence as a single nonnegative integer number, a so called
;; Goedel number. The process, being reversible, includes the subsequent
;; restoration of the original sequence.
;; 
;; == GOEDEL-ENCODING ==
;; In order to encode a plaintext sequence S containing N elements,
;; using an encoding table E, which unambiguously maps to each plaintext
;; symbols a positive integer, the first N prime numbers P are
;; generated. For each plaintext symbol S[i] in the sequence S, the
;; integer symbol code T[i] is produced, utilizing the encoding table
;; E to obtain the code T[i] = E(S[i]), thus establishing a new sequence
;; T containing a tally N of symbol codes (T[1], ..., T[i], ..., T[N]).
;;   
;;   Input:
;;     S - the plaintext sequence of N elements
;;         (S[1], S[2], ..., S[i], ..., S[N]).
;;     E - an encoding table which maps to each plaintext symbol
;;         a positive integer, the symbol code.
;;   
;;   Compute:
;;     N <- number of elements in S
;;     
;;     { A new sequence containing the symbol codes for S. }
;;     T <- empty list
;;     for i from 1 to N
;;       T[i] <- E(S[i])
;;     end for
;;     
;;     { The list of prime numbers is fixed: 2, 3, 5, 7, 9, 11, ...}
;;     P <- generate the first N prime numbers.
;;     { The Goedel number encoding the sequence S. Being subject to }
;;     { multiplications, the neutral element must be chosen as 1.   }
;;     G <- 1
;;     
;;     for i from 1 to N
;;       G <- G * (P[i]^T[i])
;;     end for
;;     
;;     return G
;; 
;; == GOEDEL-DECODING ==
;; Given a Goedel number G, expected to have been encoded an original
;; plaintext sequence S by aid of the encoding table E, its decoding
;; is capacitated using the decoding table D, which contains the entries
;; of the table E in reversed association: Each integer symbol code
;; acts as an unambiguous key to retrieve exactly one plaintext symbol.
;; The decoding process both retrieves the plaintext elements in their
;; correct order, as well as, of course, their tally N.
;; 
;; Starting with the infinite prime number sequence P, each element P[i]
;; is tested for the number of its complete occurrences in the Goedel
;; number G. If this tally T[i] is greater than zero, it constitutes the
;; i-th element of the symbol code sequence T. The first T[i] to equal
;; zero designates the end of this search process, and itself, as well
;; as any potential successor T[i+1], are not included in T. In
;; corollary, the remaining infinite prime numbers are discarded. The
;; number of these positive-valued T[i] not only represent the
;; cardinality of the symbol code sequence T, but also the element count
;; of the plaintext sequence S.
;; 
;; Each symbol code T[i] is finally replaced by the plaintext symbol
;; S[i], using the decoding table D to obtain S[i] = D(T[i]). The thus
;; generated sequence constitutes the desiderated plaintext sequence S.
;;   
;;   Input
;;     G - the nonnegative integer Goedel number to decode.
;;     D - an decoding table which maps to each positive integer
;;         symbol code a plaintext symbol.
;;     P - the infinite sequence of prime numbers.
;;   
;;   Process:
;;     { A new sequence containing the symbol codes for S. }
;;     T <- empty list
;;     
;;     { Build the symbol code sequence T. }
;;     for i from 1
;;       symbolCode <- 0
;;       quotient   <- G
;;       
;;       { Tally the number of repetitions of the prime number P[i] }
;;       { which contribute to the Goedel number G.                 }
;;       while (quotient modulo P[i]) = 0
;;         symbolCode <- symbolCode + 1
;;         quotient   <- quotient / P[i]
;;       end while
;;       
;;       { If the i-th prime number P[i] is contained in the Goedel  }
;;       { number G, its tally of occurrences does not only describe }
;;       { the exponent to which P[i] was raised, but a fortiori it  }
;;       { equals the i-th symbol code T[i] in the code sequence T,  }
;;       { that is, the i-th Goedelang instruction.                  }
;;       if symbolCode > 0 then
;;         T[i] <- symbolCode
;;       else
;;         break for-loop
;;       end if
;;     end for
;;     
;;     { The cardinality of the symbol sequence T equals that of the }
;;     { desired plaintext sequence S.                               }
;;     N <- number of elements in T
;;     { A new sequence containing the plaintext symbols S. }
;;     S <- empty list
;;     
;;     { Restore the plaintext sequence S. }
;;     for i from 1 to N
;;       S[i] <- D(T[i])
;;     end for
;;     
;;     return S
;; 
;; == GOEDELANG: INSTRUCTIONS ARE SYMBOL CODES ==
;; Goedelang, in lieu of single-character commands like brainfuck,
;; employs nonnegative integers as instruction identifiers. A program in
;; this language does not amount to a character sequence representation,
;; but instead manifests in a single nonnegative integer, a Goedel
;; number, which is partially decoded in accord with the above
;; explications, retrieving the ordered symbol code sequence T, but
;; exercising abstinence from its conversion into some plaintext
;; equivalent. These symbol codes, themselves nonnegative integers,
;; already constitute the program instructions in their correct
;; arrangement, necessitating merely a processing to be imbued with
;; actual effect.
;; 
;; == GOEDELANG: A BRAINFUCK ENCODER ==
;; The cognizance about a partial Goedel-decoding applied during the
;; production of the Goedelang instructions from an input number,
;; conjugated with the gnarity about the vinculum to its provenance
;; language, a continuation of the decoding and encoding principles
;; manifests in the transcription to and from brainfuck code.
;; 
;; Concerning the decoding table D and the encoding table E, the
;; following associations hold for the Goedelang version 1:
;; 
;;   ----------------------------
;;   Exponent | brainfuck command
;;   ---------+------------------
;;    0       | end of file
;;    1       | >
;;    2       | <
;;    3       | +
;;    5       | -
;;    7       | ++++++++++++++++
;;    9       | ----------------
;;    10      | .
;;    11      | ,
;;    13      | [
;;    14      | ]
;;   ----------------------------
;; 
;; Analogously, the version 2, erstwhile agnominated 1.1, maintains:
;; 
;;   ----------------------------
;;   Exponent | brainfuck command
;;   ---------+------------------
;;    0       | end of file
;;    1       | >
;;    2       | <
;;    3       | +
;;    5       | -
;;    7       | .
;;    9       | none
;;    10      | ,
;;    11      | none
;;    13      | [
;;    14      | ]
;;    15      | none
;;    17      | none
;;    19      | [-]
;;   ----------------------------
;; 
;; During the processing of a Goedelang program, that is, the decoding
;; of an integer Goedel number into symbol codes, delineating the
;; language's instructions, and then plaintext symbols, being an
;; expression of the brainfuck commands, the elements appertaining to
;; Goedelang assume the role of the symbol code list T, with the
;; brainfuck produces accounting for the desiderated plaintext
;; sequence S. In dependency of the tables adduced aboon, each Goedelang
;; element, if contingency permits, instantiates a destination object.
;; 
;; == GOEDELANG: ALSO A BRAINFUCK DECODER ==
;; The symmetry inhering as a haecceity in the Goedel numbering concept
;; homologates the second moiety's realization: the transliteration from
;; brainfuck to its derivative.
;; 
;; Perusing in an otherthwart airt the decoding tables from the
;; inspiration to the provenance, an association paregal in character
;; is recluded to the reader's conspectuity, thus issuing the encoding
;; tables E for any of the Goedelang renditions' twains.
;; 
;; In concord with the delineations applicable in the Goedel encoding
;; section presented above, the brainfuck program's characters, except
;; for those partaking of a commentary agency, designate in conjunction
;; the plaintext symbol sequence S, the translation of the same proceeds
;; by aid of one of the two encoding tables E to produce a symbol code
;; sequence T --- the integer Goedelang instructions. Their amalgam, the
;; final Goedel number, ultimately establishes the brainfuck equivalent
;; Goedelang program.
;; 
;; == GOEDELANG'S APPROPRIATIONS: MEMORY AND DATA TYPES ==
;; A bipartite resonance of brainfuck's afflation, Goedelang partakes
;; of the exact same architecture and data type definitions.
;; 
;; The program memory's establishment manifests in a potentially
;; infinite tally of cells, each a conditory to a single integer of
;; unbounded range. A pointer, initially marking the cell at the index
;; zero, at any instant designates the current cell, while amenable to
;; sinistral and dextral translations.
;; 
;; All data being stored in numeric guise, the language permits its
;; admittance and expression, proceeding across input/output conduits,
;; in both integer and character form.
;; 
;; 
;; Architecture
;; ============
;; Goedelang's fidelity to its brainfuck heritage promulgates through
;; the architecture and type system. The language operates on an
;; infinite tape of unbounded integer numbers, stored in the cells,
;; a single instance of which at any time constitutes the active entity,
;; designated by a pointer.
;; 
;; == DATA IS STORED IN A TAPE ==
;; The salvatory dedicated to any data management describes the
;; bailiwick of a tape-like series of cells, known as the memory, which
;; bilaterally extends in its tally into infinity. A cell embraces a
;; scalar integer value of paregal liberality, that means the range
;; [-infinity, +infinity], initialized to zero.
;; 
;; == THE POINTER MARKS THE ACTIVE CELL ==
;; At any instant in the program, a single cell is designated as the
;; active or selected member. The mechanism for this emphasis is
;; designed by the memory pointer, a motile reference, amenable to
;; certain operations for its navigation across the tape. Similarly,
;; commands exist to manipulate the active cell.
;; 
;; 
;; Data Types
;; ==========
;; Akin to brainfuck, Goedelang implements a dichotomy of data types
;; into integers for operative purposes and characters as tokens in the
;; communication with the user.
;; 
;; == INTEGERS REPRESENT THE PROGRAM DATA ==
;; The superior role of integers manifests itself in their castaldy by
;; the central memory. Any data under a program's purview subsumes into
;; this category, including the scan operations for their augmentation
;; and deduction. An evolution from the brainfuck origin, Goedelang
;; additionally homologates the input and display of cell values in an
;; immediate numeric fashion, in consectary applying the fact of
;; supersession upon this traditional bailiwick of characters.
;; 
;; == CHARACTERS REPRESENT THE INTERFACE ==
;; Characters as one of the multifarious applications of integers, and
;; the sole inherent beside the verbatim utilization, exert their
;; usefulness in the restricted area of user-program communication:
;; The user, if queried for an input, responds with a character, which
;; by subsequent transformation into its ASCII code finds its way into
;; the memory; in a symmetrical fashion, data output involves integer
;; numbers construed as ASCII codes and printed in this aspect. The
;; lacuna of text manipulation capabilities bewrays this type's inferior
;; consideration.
;; 
;; == THE TAPE AS A COLLECTION ==
;; The diorism administered to the memory as an architectural component
;; reverbs in its definition in the data representation. The cells,
;; linearly ordered, demonstrate a linkage which permits their traversal
;; in both airts. The tape as a data type assumes a one-dimensional
;; realization, usually rendered conrete as a random access vector or
;; a linked list.
;; 
;; 
;; Syntax
;; ======
;; With the exclusive appliance of a Goedel number as an object in the
;; source code's agency, the complete program assumes an utterly
;; homogeneous guise as a nonnegative integer value, itself a mere
;; composite of digits. The concrete mode of purveyance dedicated to
;; this datum's representation is recluded to the programmer's
;; personal delectation.
;; 
;; == GRAMMAR ==
;; The language syntax can be expressed in the following Extended
;; Backus-Naur form (EBNF) description.
;; 
;;   program := digit , { digit } ;
;;   digit   := "0" | ... | "9" ;
;; 
;; 
;; Instructions
;; ============
;; Goedelang's instruction set assumes a design preponderantly founded
;; upon an appropriation of its brainfuck cleronomy, experiencing an
;; augmentation by a few adscititious facilities.
;; 
;; == OVERVIEW ==
;; The following tables shall describe the available instructions,
;; demarcated by the language iteration, and in juxtaposition, if
;; possible, to their brainfuck equivalents. Please note that numbers
;; not included in the instruction set are simply ignored, in the same
;; manner as non-command characters experience tolerance and neglect in
;; a brainfuck program.
;; 
;; == OVERVIEW: GOEDELANG 1 ==
;; The version 1, a nominal rejuvenescence of the language's inchoation,
;; merely procured, beside a terminating statement, two operations
;; intended to modify the current cell value by an amount of 16.
;; 
;;   ------------------------------------------------------------------
;;   Exponent | bf command       | Effect
;;   ---------+------------------+-------------------------------------
;;    0       | none             | Terminates the program.
;;   ..................................................................
;;    1       | >                | Moves the memory pointer one cell to
;;            |                  | the right.
;;   ..................................................................
;;    2       | <                | Moves the memory pointer one cell to
;;            |                  | the left.
;;   ..................................................................
;;    3       | +                | Increases the value of the current
;;            |                  | cell by one.
;;   ..................................................................
;;    5       | -                | Decreases the value of the current
;;            |                  | cell by one.
;;   ..................................................................
;;    7       | ++++++++++++++++ | Increases the value of the current
;;            |                  | cell by 16 (binary value = 10000).
;;   ..................................................................
;;    9       | ---------------- | Decreases the value of the current
;;            |                  | cell by 16 (binary value = 10000).
;;   ..................................................................
;;    10      | .                | Prints to the standard output the
;;            |                  | ASCII character associated with the
;;            |                  | value of the current cell.
;;   ..................................................................
;;    11      | ,                | Prompts the user for a character and
;;            |                  | stores the ASCII code associated
;;            |                  | with the same into the current cell.
;;   ..................................................................
;;    13      | [                | If the current cell value equals
;;            |                  | zero, moves the instruction pointer
;;            |                  | forward to the character immediately
;;            |                  | following the matching 14;
;;            |                  | otherwise simply advances to the
;;            |                  | next character.
;;   ..................................................................
;;    14      | ]                | If the current cell value does not
;;            |                  | equal zero, moves the instruction
;;            |                  | pointer backward to the character
;;            |                  | immediately following the matching
;;            |                  | 13; otherwise simply advances to
;;            |                  | the next character.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW: GOEDELANG 2 ==
;; The second rendition, Goedelang 2, whilom distributed as the version
;; 1.1, rather practises a veridical investment in the department of
;; chreotechnics, with an instruction set the foundry of the same
;; in conclusion of its forisfamiliation embraces operations for input
;; and output as both numbers and characters, and contributes a cell
;; value reset to zero (0).
;; 
;;   ------------------------------------------------------------------
;;   Exponent | bf command       | Effect
;;   ---------+------------------+-------------------------------------
;;    0       | none             | Terminates the program.
;;   ..................................................................
;;    1       | >                | Moves the memory pointer one cell to
;;            |                  | the right.
;;   ..................................................................
;;    2       | <                | Moves the memory pointer one cell to
;;            |                  | the left.
;;   ..................................................................
;;    3       | +                | Increases the value of the current
;;            |                  | cell by one.
;;   ..................................................................
;;    5       | -                | Decreases the value of the current
;;            |                  | cell by one.
;;   ..................................................................
;;    7       | .                | Prints to the standard output the
;;            |                  | ASCII character associated with the
;;            |                  | value of the current cell.
;;   ..................................................................
;;    9       | none             | Prints to the standard output the
;;            |                  | numeric value stored in the current
;;            |                  | cell.
;;   ..................................................................
;;    10      | ,                | Prompts the user for a character and
;;            |                  | stores the ASCII code associated
;;            |                  | with the same into the current cell.
;;   ..................................................................
;;    11      | none             | Prompts the user for a number and
;;            |                  | stores it into the current cell.
;;            |                  | Letters, while tolerated, resolve
;;            |                  | to zero (0).
;;   ..................................................................
;;    13      | [                | If the current cell value equals
;;            |                  | zero, moves the instruction pointer
;;            |                  | forward to the character immediately
;;            |                  | following the matching 14;
;;            |                  | otherwise simply advances to the
;;            |                  | next character.
;;   ..................................................................
;;    14      | ]                | If the current cell value does not
;;            |                  | equal zero, moves the instruction
;;            |                  | pointer backward to the character
;;            |                  | immediately following the matching
;;            |                  | 13; otherwise simply advances to
;;            |                  | the next character.
;;   ..................................................................
;;    15      | none             | If the current cell value equals
;;            |                  | zero, moves the instruction pointer
;;            |                  | forward to the character immediately
;;            |                  | following the matching 14;
;;            |                  | otherwise simply advances to the
;;            |                  | next character.
;;   ..................................................................
;;    17      | none             | If the current cell value does not
;;            |                  | equal zero, moves the instruction
;;            |                  | pointer backward to the character
;;            |                  | immediately following the matching
;;            |                  | 13; otherwise simply advances to
;;            |                  | the next character.
;;   ..................................................................
;;    19      | [-]              | Sets the current cell value to 0.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; Its germination as a scion of simplicity apportions to this
;; realization of Goedelang all traits anticipated for demonstrative
;; utilities, including willful nescience toward efficiency and a design
;; reckoned with few convolutions.
;; 
;; Common Lisp's concinnity for the Goedel numbering task maintains its
;; abode with particular emphasis in the language's capability to
;; represent arbitrary large integers, contained in reality only by the
;; march established through the executing system's memory.
;; 
;; Beside the interpeter, two amenities' adit valorizes the features'
;; compass: a converter betwixt brainfuck and Goedelang, and a second
;; operating in widdershins direction.
;; 
;; The multifarious participants in the accomplishment of a functional
;; unit necessitate a discrimination betwixt adminicular and primary
;; functions. The public interface shall be a delineation of the
;; following operations:
;;   
;;   interpret-Goedelang
;;     Accepts a Goedel number representation of a Goedelang program and
;;     a language version specifier, executing the thus specified code.
;;   
;;   compile-Goedelang-instructions
;;     Accepts a list of Goedelang instructions in any version of the
;;     language and returns the Goedel number corresponding to a program
;;     in the same. The thus produced integer value defines compatible
;;     input for the INTERPRET-GOEDELANG function.
;;   
;;   extract-Goedelang-instructions
;;     Accepts a Goedel number representation of a Goedelang program,
;;     and returns a vector of the contained Goedelang instructions.
;;     This function may be reckoned as the opposite of the operation
;;     COMPILE-GOEDELANG-INSTRUCTIONS, as it disassembles a Goedel
;;     number (or Goedelang program) into its constituents, while the
;;     latter assembles these instructions into a single datum.
;;   
;;   convert-brainfuck-to-Goedelang
;;     Accepts a brainfuck program and a Goedelang version, and returns
;;     a Goedel number representation of an equivalent Goedelang code.
;;     The manufactured object constitutes a valid input for the routine
;;     INTERPRET-GOEDELANG.
;;   
;;   convert-Goedelang-to-brainfuck
;;     Accepts a Goedel number representation of a Goedelang program,
;;     in conjunction with a version of the latter, and writes to the
;;     specified output conduit the corresponding brainfuck code.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-01-19
;; 
;; Sources:
;;   [esolang2021goedelang]
;;   -> "https://esolangs.org/wiki/G%C3%B6delang"
;;       o Specification of the esoteric programming language
;;         "Goedelang", a "brainfuck" derivative based upon Goedel numbers.
;;   
;;   [esolang2021goedelang87404]
;;   -> "https://esolangs.org/w/index.php?title=G%C3%B6delang&oldid=87404"
;;       o An early but partially more lucid revision of the
;;         specification.
;;   
;;   [esolang2021goedelang89761]
;;   -> "https://esolangs.org/w/index.php?title=G%C3%B6delang&oldid=89761"
;;       o An early revision which introduced the differentiation into
;;         the versions 1.0 (old) and 1.1.
;;       o Language 1.0 was later renamed to "1", and 1.1 to "2".
;;   
;;   [meyer2021goedel3]
;;   -> "https://www.jamesrmeyer.com/ffgit/GodelSimplified3.html"
;;       o Main page: "https://www.jamesrmeyer.com/ffgit/GodelSimplified0.html"
;;       o Describes Goedel numbers in a pellucid manner.
;;       o Provides an example.
;;   
;;   [panu20022sepgoedelincompleteness]
;;   -> "https://plato.stanford.edu/entries/goedel-incompleteness/sup1.html"
;;   
;;   [rod2018factoringinalgebra]
;;   -> "https://www.mathsisfun.com/algebra/factoring.html"
;;       o Describes factorization.
;;   
;;   [rod2021fundtheoremofarithm]
;;   -> "https://www.mathsisfun.com/numbers/fundamental-theorem-arithmetic.html"
;;       o Describes the Fundamental Theorem of Arithmetic in simple
;;         terms.
;;       o The theorem states that:
;;           "Any integer greater than 1 is either a prime number, or
;;            can be written as a unique product of prime numbers
;;            (ignoring the order)."
;;   
;;   [rod2021primefactorization]
;;   -> "https://www.mathsisfun.com/prime-factorization.html"
;;       o Describes prime numbers and prime factorization in simple
;;         terms.
;;   
;;   [wikiwikiweb2005goedelnumbering]
;;   -> "https://wiki.c2.com/?GoedelNumbering"
;;   
;;   [wikipedia2021goedelnumbering]
;;   -> "https://en.wikipedia.org/wiki/G%C3%B6del_numbering"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, the same defaults to the
   comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and the
   associated value to the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for writing operations,
   including, without exhaustion, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype prime-number ()
  "The ``prime-number'' type defines an integer value compatible with
   the notion of a prime number.
   ---
   Please note that this type specification does not encompass an
   inquiry into an object's actual realization of the prime predicate;
   instead, a conformity is already imputed if an integer occupies the
   valid range [2, +infinity]. In the light of this, the type specifier
   might more correctly be called a designator for a 'prime candidate'."
  '(integer 2 *))

;;; -------------------------------------------------------

(deftype prime-generator ()
  "The ``prime-generator'' type defines a niladic function which,
   starting at the value two (2), returns upon each invocation the next
   prime number.
   ---
   Unbounded towards the upper march, and fortified by the Common Lisp
   ``integer'' type's artibrary mickleness, this function conceptually
   responds to an infinite quantity of requests, albeit the employed
   system's memory might eventually claim a denial in its services."
  '(function () prime-number))

;;; -------------------------------------------------------

(deftype goedel-number ()
  "The ``goedel-number'' type defines a nonnegative integer in the
   role of an encoding of zero or more symbol codes.
   ---
   Note that this computation model veers from the strict mathematical
   eidolon in that the latter requires a positive integer, that means,
   one excluding the value zero (0), to be employed."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type defines a nonnegative integer in the role
   of a Goedelang instruction, being representative of a symbol code
   incorporated among others of its ilk in a Goedel number."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype decoding-table ()
  "The ``decoding-table'' type defines a mapping betwixt Goedelang
   instructions and their tantamount brainfuck command sequences, its
   manifestation that of a hash table thilk affiliates Goedelang
   ``instruction'' objects with brainfuck operations ensconced in
   simple strings."
  '(hash-table-of instruction simple-string))

;;; -------------------------------------------------------

(deftype language-version ()
  "The ``language-version'' type defines the Goedelang version whose
   conformance shall be ascertained."
  '(member :1.0 :1.1 :2.0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "prime-generator".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prime-p (number)
  "Checks whether the NUMBER constitutes a prime number, returning a
   ``boolean'' value of ``T'' upon confirmation, and otherwise ``NIL''."
  (declare (type integer number))
  (the boolean
    (and
      (>= number 2)
      (loop
        for divisor of-type (integer 1 *) from (1- number) downto 2
        when (zerop (mod number divisor)) do
          (return NIL)
        finally
          (return T)))))

;;; -------------------------------------------------------

(defun make-prime-generator ()
  "Creates and returns a ``prime-generator'', a function which, starting
   with the value two (2), returns upon each invocation the next prime
   number."
  (let ((current-prime 2))
    (declare (type prime-number current-prime))
    (the function
      #'(lambda ()
          (the prime-number
            (prog1 current-prime
              (loop
                do    (incf current-prime)
                until (prime-p current-prime))))))))

;;; -------------------------------------------------------

(defun get-next-prime (prime-generator)
  "Returns the next prime number from the PRIME-GENERATOR."
  (declare (type prime-generator prime-generator))
  (the prime-number (funcall prime-generator)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Goedel decoder.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-instruction-for (goedel-number prime)
  "Returns the instruction contained in the GOEDEL-NUMBER at the index
   corresponding to the PRIME number's location inside of the sequence
   of primes starting with the member two (2).
   ---
   With the prime numbers constituting an infinite sequence starting
   with the items 2, 3, 5, 7, 11, 13, ..., the instruction encoded in
   the GOEDEL-NUMBER at the position i, with i >= 1, equals the PRIME's
   location i in the infinite sequence of prime numbers. The first
   instruction (position i = 1) thus is yielded by PRIME = 2, the
   second instruction (position i = 2) by PRIME = 3, the next by
   PRIME = 5, etc."
  (declare (type goedel-number goedel-number))
  (declare (type prime-number  prime))
  (let ((remainder 0))
    (declare (type (integer 0 *) remainder))
    (the instruction
      (loop
        do    (multiple-value-setq (goedel-number remainder)
                (floor goedel-number prime))
        while (zerop remainder)
        count 1))))

;;; -------------------------------------------------------

(defun extract-Goedelang-instructions (goedel-number)
  "Extracts and returns from the GOEDEL-NUMBER a vector containing the
   numeric Goedelang instruction codes."
  (declare (type goedel-number goedel-number))
  (let ((prime-generator (make-prime-generator)))
    (declare (type prime-generator prime-generator))
    (the (simple-array instruction (*))
      (coerce
        (loop
          for symbol-code
            of-type instruction
            =       (get-instruction-for goedel-number
                      (get-next-prime prime-generator))
          while   (plusp symbol-code)
          collect symbol-code)
        '(simple-array instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Goedelang-1 (code)
  "Interprets the piece of Goedelang CODE according to the rules of the
   version 1 and returns no value."
  (declare (type goedel-number code))
  
  (let ((instructions (extract-Goedelang-instructions code)))
    (declare (type (simple-array instruction (*)) instructions))
    
    (when (plusp (length instructions))
      (let ((position    0)
            (instruction (aref instructions 0)))
        (declare (type fixnum                position))
        (declare (type (or null instruction) instruction))
        
        (let ((memory  (make-hash-table :test #'eql))
              (pointer 0))
          (declare (type (hash-table-of integer integer) memory))
          (declare (type integer                         pointer))
          
          (labels
              ((advance ()
                "Moves the POSITION cursor to the next instruction,
                 if possible, and returns no value."
                (setf instruction
                  (when (< position (1- (length instructions)))
                    (aref instructions (incf position))))
                (values))
               
               (recede ()
                "Moves the POSITION cursor to the previous instruction,
                 if possible, and returns no value."
                (when (plusp position)
                  (setf instruction
                    (aref instructions (decf position))))
                (values)))
            
            (loop do
              (case instruction
                
                ;; Terminate the program.
                ((NIL)
                  (loop-finish))
                
                ;; Terminate the program.
                (0
                  (loop-finish))
                
                ;; Move pointer one cell to the right.
                (1
                  (incf pointer)
                  (advance))
                
                ;; Move pointer one cell to the left.
                (2
                  (decf pointer)
                  (advance))
                
                ;; Increase value at the pointer by one.
                (3
                  (incf (gethash pointer memory 0))
                  (advance))
                
                ;; Decrease value at the pointer by one.
                (5
                  (decf (gethash pointer memory 0))
                  (advance))
                
                (7
                  (incf (gethash pointer memory 0) 16)
                  (advance))
                
                (9
                  (decf (gethash pointer memory 0) 16)
                  (advance))
                
                ;; Output value at the pointer as an ASCII character.
                (10
                  (write-char (code-char (gethash pointer memory 0)))
                  (advance))
                
                ;; Input ASCII character and store in cell at pointer.
                (11
                  (format T "~&Please input a character: ")
                  (let ((input (read-char)))
                    (declare (type character input))
                    (clear-input)
                    (setf (gethash pointer memory) (char-code input)))
                  (advance))
                
                ;; Skip to next 14 (']') if cell at pointer = 0.
                (13
                  (cond
                    ;; Byte at pointer = 0?
                    ;; => Skip to next 14 (']').
                    ((zerop (gethash pointer memory 0))
                      (advance)
                      (loop with level of-type integer = 0 do
                        (case instruction
                          ((NIL)
                            (error "Unmatches instruction 13 at EOF."))
                          (13
                            (incf level)
                            (advance))
                          (14
                            (cond
                              ((zerop level)
                                (advance)
                                (loop-finish))
                              (T
                                (decf level)
                                (advance))))
                          (otherwise
                            (advance)))))
                    ;; Byte at pointer != 0?
                    ;; => Advance.
                    (T
                      (advance))))
                
                ;; Return to previous 13 ('[') if cell at pointer != 0.
                (14
                  (cond
                    ;; Byte at pointer = 0?
                    ;; => Advance.
                    ((zerop (gethash pointer memory 0))
                      (advance))
                    ;; Byte at pointer != 0?
                    ;; => Advance.
                    (T
                      (recede)
                      (loop with level of-type integer = 0 do
                        (case instruction
                          ((NIL)
                            (error "Unmatched instruction 14 at EOF."))
                          (13
                            (cond
                              ((zerop level)
                                (advance)
                                (loop-finish))
                              (T
                                (decf level)
                                (recede))))
                          (14
                            (incf level)
                            (recede))
                          (otherwise
                            (recede)))))))
                
                ;; Ignore undefined instructions.
                (otherwise
                  (advance)))))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Goedelang-2 (code)
  "Interprets the piece of Goedelang CODE according to the rules of the
   version 2, formerly known as version 1.1, and returns no value."
  (declare (type goedel-number code))
  
  (let ((instructions (extract-Goedelang-instructions code)))
    (declare (type (simple-array instruction *) instructions))
    
    (when (plusp (length instructions))
      (let ((position    0)
            (instruction (aref instructions 0)))
        (declare (type fixnum                position))
        (declare (type (or null instruction) instruction))
        
        (let ((memory  (make-hash-table :test #'eql))
              (pointer 0))
          (declare (type (hash-table-of integer integer) memory))
          (declare (type integer                         pointer))
          
          (labels
              ((advance ()
                "Moves the POSITION cursor to the next instruction,
                 if possible, and returns no value."
                (setf instruction
                  (when (< position (1- (length instructions)))
                    (aref instructions (incf position))))
                (values))
               
               (recede ()
                "Moves the POSITION cursor to the previous instruction,
                 if possible, and returns no value."
                (when (plusp position)
                  (setf instruction
                    (aref instructions (decf position))))
                (values)))
            
            (loop do
              (case instruction
                
                ;; Terminate the program.
                ((NIL)
                  (loop-finish))
                
                ;; Terminate the program.
                (0
                  (loop-finish))
                
                ;; Move pointer one cell to the right.
                (1
                  (incf pointer)
                  (advance))
                
                ;; Move pointer one cell to the left.
                (2
                  (decf pointer)
                  (advance))
                
                ;; Increase value at the pointer by one.
                (3
                  (incf (gethash pointer memory 0))
                  (advance))
                
                ;; Decrease value at the pointer by one.
                (5
                  (decf (gethash pointer memory 0))
                  (advance))
                
                ;; Output value at the pointer as an ASCII character.
                (7
                  (write-char (code-char (gethash pointer memory 0)))
                  (advance))
                
                ;; Output value at the pointer as a number.
                (9
                  (write (gethash pointer memory 0))
                  (advance))
                
                ;; Input ASCII character and store in cell at pointer.
                (10
                  (format T "~&Please input a character: ")
                  (let ((input (read-char)))
                    (declare (type character input))
                    (clear-input)
                    (setf (gethash pointer memory) (char-code input)))
                  (advance))
                
                ;; Input value and store in cell at pointer.
                (11
                  (format T "~&Please input a value: ")
                  (let ((input (read-line)))
                    (declare (type string input))
                    (clear-input)
                    (setf (gethash pointer memory)
                          (parse-integer input :junk-allowed T)))
                  (advance))
                
                ;; Skip to next 14 (']') if cell at pointer = 0.
                (13
                  (cond
                    ;; Byte at pointer = 0?
                    ;; => Skip to next 14 (']').
                    ((zerop (gethash pointer memory 0))
                      (advance)
                      (loop with level of-type integer = 0 do
                        (case instruction
                          ((NIL)
                            (error "Unmatches instruction 13 at EOF."))
                          (13
                            (incf level)
                            (advance))
                          (14
                            (cond
                              ((zerop level)
                                (advance)
                                (loop-finish))
                              (T
                                (decf level)
                                (advance))))
                          (otherwise
                            (advance)))))
                    ;; Byte at pointer != 0?
                    ;; => Advance.
                    (T
                      (advance))))
                
                ;; Return to previous 13 ('[') if cell at pointer != 0.
                (14
                  (cond
                    ;; Byte at pointer = 0?
                    ;; => Advance.
                    ((zerop (gethash pointer memory 0))
                      (advance))
                    ;; Byte at pointer != 0?
                    ;; => Advance.
                    (T
                      (recede)
                      (loop with level of-type integer = 0 do
                        (case instruction
                          ((NIL)
                            (error "Unmatched instruction 14 at EOF."))
                          (13
                            (cond
                              ((zerop level)
                                (advance)
                                (loop-finish))
                              (T
                                (decf level)
                                (recede))))
                          (14
                            (incf level)
                            (recede))
                          (otherwise
                            (recede)))))))
                
                ;; If byte at pointer = 0, then skip to next 17.
                (15
                  (cond
                    ;; Byte at pointer = 0?
                    ;; => Skip to next 14 (']').
                    ((zerop (gethash pointer memory 0))
                      (advance)
                      (loop with level of-type integer = 0 do
                        (case instruction
                          ((NIL)
                            (error "Unmatches instruction 13 at EOF."))
                          (15
                            (incf level)
                            (advance))
                          (17
                            (cond
                              ((zerop level)
                                (advance)
                                (loop-finish))
                              (T
                                (decf level)
                                (advance))))
                          (otherwise
                            (advance)))))
                    (T
                      (advance))))
                
                ;; No function, except for use with 15.
                (17
                  (advance))
                
                ;; Set the value at the pointer to zero (0).
                (19
                  (setf (gethash pointer memory) 0)
                  (advance))
                
                ;; Ignore undefined instructions.
                (otherwise
                  (advance)))))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Goedelang (code &key (language-version :2.0))
  "Interprets the piece of Goedelang CODE according to the rules of the
   LANGUAGE-VERSION, defaulting to the rendition 2.0, and returns no
   value."
  (declare (type goedel-number    code))
  (declare (type language-version language-version))
  (case language-version
    (:1.0
      (interpret-Goedelang-1 code))
    ((:1.1 :2.0)
      (interpret-Goedelang-2 code))
    (otherwise
      (error "Invalid language version: ~s." language-version)))
  (values))

;;; -------------------------------------------------------

(defun compile-Goedelang-instructions (symbol-codes)
  "Encodes the list of non-negative integer SYMBOL-CODES as a
   Goedel number, thus producing a Goedelang program, and returns the
   same."
  (declare (type (list-of instruction) symbol-codes))
  (let ((prime-generator (make-prime-generator)))
    (declare (type prime-generator prime-generator))
    (the goedel-number
      (reduce
        #'(lambda (accumulator code)
            (declare (type (integer 1 *) accumulator))
            (declare (type instruction   code))
            (let ((prime (funcall prime-generator)))
              (declare (type (integer 2 *) prime))
              (the (integer 2 *)
                (* accumulator
                   (expt prime code)))))
        symbol-codes
        :initial-value 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Goedelang converter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character instruction)
         *brainfuck-encoding-table-v1*))

(declaim (type (hash-table-of character instruction)
         *brainfuck-encoding-table-v2*))

;;; -------------------------------------------------------

(defparameter *brainfuck-encoding-table-v1*
  (make-hash-table :test #'eql)
  "The encoding table which assigns to each brainfuck instruction the
   corresponding Goedelang command according to the version 1.")

(defparameter *brainfuck-encoding-table-v2*
  (make-hash-table :test #'eql)
  "The encoding table which assigns to each brainfuck instruction the
   corresponding Goedelang command according to the version 2.")

;;; -------------------------------------------------------

(setf (gethash #\> *brainfuck-encoding-table-v1*) 1)
(setf (gethash #\< *brainfuck-encoding-table-v1*) 2)
(setf (gethash #\+ *brainfuck-encoding-table-v1*) 3)
(setf (gethash #\- *brainfuck-encoding-table-v1*) 5)
(setf (gethash #\. *brainfuck-encoding-table-v1*) 10)
(setf (gethash #\, *brainfuck-encoding-table-v1*) 11)
(setf (gethash #\[ *brainfuck-encoding-table-v1*) 13)
(setf (gethash #\] *brainfuck-encoding-table-v1*) 14)

;;; -------------------------------------------------------

(setf (gethash #\> *brainfuck-encoding-table-v2*) 1)
(setf (gethash #\< *brainfuck-encoding-table-v2*) 2)
(setf (gethash #\+ *brainfuck-encoding-table-v2*) 3)
(setf (gethash #\- *brainfuck-encoding-table-v2*) 5)
(setf (gethash #\. *brainfuck-encoding-table-v2*) 7)
(setf (gethash #\, *brainfuck-encoding-table-v2*) 10)
(setf (gethash #\[ *brainfuck-encoding-table-v2*) 13)
(setf (gethash #\] *brainfuck-encoding-table-v2*) 14)

;;; -------------------------------------------------------

(defun compute-symbol-codes (brainfuck-code encoding-table)
  "Generates and returns for the BRAINFUCK-CODE, according to the
   mappings governed by the ENCODING-TABLE, a list of Goedelang
   instructions."
  (declare (type string                                brainfuck-code))
  (declare (type (hash-table-of character instruction) encoding-table))
  (let ((symbol-codes NIL))
    (declare (type (list-of instruction) symbol-codes))
    (loop
      for character of-type character across brainfuck-code
      and position  of-type fixnum    from   0
      do
      (multiple-value-bind (symbol-code contains-character-p)
          (gethash character encoding-table)
        (declare (type (or null instruction) symbol-code))
        (declare (type T                     contains-character-p))
        (when contains-character-p
          (push symbol-code symbol-codes))))
    (the (list-of instruction) (nreverse symbol-codes))))

;;; -------------------------------------------------------

(defun goedel-encode (source encoding-table)
  "Encodes the SOURCE sequence as a Goedel number using the
   ENCODING-TABLE as the mapping from each SOURCE element to an
   identifying non-negative integer symbol code, and returns the
   Goedel number matching the encoded SOURCE."
  (declare (type string                                source))
  (declare (type (hash-table-of character instruction) encoding-table))
  (the goedel-number
    (compile-Goedelang-instructions
      (compute-symbol-codes source encoding-table))))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-Goedelang (brainfuck-code
                                       &key (goedelang-version :2.0))
  "Converts the BRAINFUCK-CODE into a Goedelang program according to
   the GOEDELANG-VERSION, and returns this program as a Goedel number."
  (declare (type string           brainfuck-code))
  (declare (type language-version goedelang-version))
  (the goedel-number
    (goedel-encode brainfuck-code
      (case goedelang-version
        (:1.0
          *brainfuck-encoding-table-v1*)
        ((:1.1 :2.0)
          *brainfuck-encoding-table-v2*)
        (otherwise
          (error "Invalid Goedelang version: ~s."
            goedelang-version))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Goedelang-to-brainfuck converter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type decoding-table *brainfuck-decoding-table-v1*))

(declaim (type decoding-table *brainfuck-decoding-table-v2*))

;;; -------------------------------------------------------

(defparameter *brainfuck-decoding-table-v1*
  (make-hash-table :test #'eq)
  "The decoding table which assigns to each Goedelang version 1 command
   the equivalent brainfuck instruction.")

(defparameter *brainfuck-decoding-table-v2*
  (make-hash-table :test #'eq)
  "The decoding table which assigns to each Goedelang version 2 command
   the equivalent brainfuck instruction.")

;;; -------------------------------------------------------

(setf (gethash  0 *brainfuck-decoding-table-v1*) "")
(setf (gethash  1 *brainfuck-decoding-table-v1*) ">")
(setf (gethash  2 *brainfuck-decoding-table-v1*) "<")
(setf (gethash  3 *brainfuck-decoding-table-v1*) "+")
(setf (gethash  5 *brainfuck-decoding-table-v1*) "-")
(setf (gethash  7 *brainfuck-decoding-table-v1*) "++++++++++++++++")
(setf (gethash  9 *brainfuck-decoding-table-v1*) "----------------")
(setf (gethash 10 *brainfuck-decoding-table-v1*) ".")
(setf (gethash 11 *brainfuck-decoding-table-v1*) ",")
(setf (gethash 13 *brainfuck-decoding-table-v1*) "[")
(setf (gethash 14 *brainfuck-decoding-table-v1*) "]")

;;; -------------------------------------------------------

(setf (gethash  0 *brainfuck-decoding-table-v2*) "")
(setf (gethash  1 *brainfuck-decoding-table-v2*) ">")
(setf (gethash  2 *brainfuck-decoding-table-v2*) "<")
(setf (gethash  3 *brainfuck-decoding-table-v2*) "+")
(setf (gethash  5 *brainfuck-decoding-table-v2*) "-")
(setf (gethash  7 *brainfuck-decoding-table-v2*) ".")
(setf (gethash  9 *brainfuck-decoding-table-v2*) "")
(setf (gethash 10 *brainfuck-decoding-table-v2*) ",")
(setf (gethash 11 *brainfuck-decoding-table-v2*) "")
(setf (gethash 13 *brainfuck-decoding-table-v2*) "[")
(setf (gethash 14 *brainfuck-decoding-table-v2*) "]")
(setf (gethash 15 *brainfuck-decoding-table-v2*) "")
(setf (gethash 17 *brainfuck-decoding-table-v2*) "")
(setf (gethash 19 *brainfuck-decoding-table-v2*) "[-]")

;;; -------------------------------------------------------

(defun goedel-decode (goedel-number
                      decoding-table
                      &key (destination T))
  "Converts the GOEDELANG-CODE, provided as a nonnegative integer,
   according to the nomothesia imposed by the DECODING-TABLE, into an
   equivalent brainfuck program and writes the latter to the
   DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value; otherwise responds with a fresh string comprehending the
   output."
  (declare (type goedel-number  goedel-number))
  (declare (type decoding-table decoding-table))
  (declare (type destination    destination))
  (the (or null string)
    (if destination
      (let ((goedelang-instructions
              (extract-Goedelang-instructions goedel-number)))
        (declare (type (simple-array instruction (*))
                       goedelang-instructions))
        (loop
          for goedelang-instruction
            of-type instruction
            across  goedelang-instructions
          do
            (multiple-value-bind
                (brainfuck-command contains-instruction-p)
                (gethash goedelang-instruction decoding-table)
              (declare (type (or null simple-string)
                             brainfuck-command))
              (declare (type T
                             contains-instruction-p))
              (when contains-instruction-p
                (write-string brainfuck-command destination)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (goedel-decode goedel-number decoding-table
          :destination output)))))

;;; -------------------------------------------------------

(defun convert-Goedelang-to-brainfuck (goedelang-code
                                       &key (goedelang-version :2.0)
                                            (destination       T))
  "Converts the GOEDELANG-CODE, provided as a nonnegative integer,
   interpreted according to the GOEDELANG-VERSION, into an equivalent
   brainfuck program and writes the latter to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise responds
   with a fresh string comprehending the output."
  (declare (type goedel-number    goedelang-code))
  (declare (type language-version goedelang-version))
  (declare (type destination      destination))
  (the (or null string)
    (goedel-decode goedelang-code
      (case goedelang-version
        (:1.0
          *brainfuck-decoding-table-v1*)
        ((:1.1 :2.0)
          *brainfuck-decoding-table-v2*)
        (otherwise
          (error "Invalid Goedelang version: ~s." goedelang-version)))
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goedelang equivalent of the infinitely repeating cat program in
;; brainfuck:
;;   ,.[,.]
(interpret-Goedelang 59250896327476337572570276385712371250000000000)

;;; -------------------------------------------------------

;; Truth-machine.
;; Encodes the Goedelang instructions
;;   11, 13, 9, 14, 9
;; as the single Goedel number
;;   10198658019570622936267207428000000000
;; equivalent to a program in this language.
;; 
;; In addition, this piece of code demonstrates the augmentation applied
;; to the brainfuck instruction set through input and output of direct
;; numeric values in lieu of ASCII characters and their codes.
(interpret-Goedelang
  (compile-Goedelang-instructions (list 11 13 9 14 9)))

;;; -------------------------------------------------------

;; Returns the Goedelang program (Goedel number)
;;   59250896327476337572570276385712371250000000000.
(convert-brainfuck-to-Goedelang ",.[,.]" :goedelang-version :2.0)

;;; -------------------------------------------------------

;; Converts the infinite cat program from brainfuck
;;   ,.[,.]
;; to its Goedelang equivalent, the Goedel number
;;   59250896327476337572570276385712371250000000000
;; and exercises the translation into the widdershins airt, restoring
;; the brainfuck program
;;   ,.[,.]
(convert-Goedelang-to-brainfuck
  (convert-brainfuck-to-Goedelang ",.[,.]" :goedelang-version :2.0)
  :goedelang-version :2.0)

;;; -------------------------------------------------------

;; Converts the infinite cat program from brainfuck
;;   ,.[,.]
;; to its Goedelang equivalent, the Goedel number
;;   59250896327476337572570276385712371250000000000
;; and interprets the latter.
(interpret-Goedelang
  (convert-brainfuck-to-Goedelang ",.[,.]" :goedelang-version :2.0)
  :language-version :2.0)

;;; -------------------------------------------------------

;; Converts the brainfuck "Hello World!" program
;;   ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
;; into its Goedelang version 1 equivalent
;;   3524789677462771726682354401756319853296486547942244888068456057399
;;   1331409359934110627154711086696854881565599545441889761230834522104
;;   7704385005711944230201540652505078182997207962556436207210054068453
;;   3957911896609382797365247359637887733065845025631629233125301961322
;;   4482863756724770230182270994892527604892304741563340103297530882883
;;   4596029120204602462954501788497403372226235425970540419434183418194
;;   2859765587606739699288750331751292508054831248810178466805635121685
;;   0475325658565487146150398636540501906530933859492769081640122367247
;;   8041958441991145211950568591160495515349530064835373571041745895447
;;   3352452153360769852054978477129883423948717124747892819193184416886
;;   8637762894721905413749869560003836983566788475122335759507800574981
;;   1763147278551569323520883663868906324440651332846269934502348150695
;;   5231167597023797481800913156185458374936504406186355500759428825961
;;   3661919932517168417909720286184055289528997656937216922290257834256
;;   5751764479071961339365005563713598332595216319502140502730644637274
;;   7510215240686579219976750461950339661115580455295224898086059416788
;;   524809000
;; and executes the thus produced code.
(interpret-Goedelang
  (convert-brainfuck-to-Goedelang
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    :goedelang-version :1.0)
  :language-version :1.0)
