;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "```", invented by the Esolang user "Xyzzy" and presented
;; on February 19th, 2023, the commorancy of its proprium an aefauld
;; instruction type's deployment whose undecimal variations, in
;; champarty with special cells in the integer-valued program memory
;; that produce epiphenomenal reactions upon modulations, educe the
;; gendrure of input, output, conditional execution, and control flow
;; helming.
;; 
;; 
;; Concept
;; =======
;; The ``` programming language subsumes into the species of
;; "One Instruction Set Computers", abbreviated "OISC", the haecceity
;; of which wones in the elicitation of multifarious causata from a
;; single instruction, which in this case declaims its provenance in
;; combinations of the backtick ("`") and hash sign ("#") symbols,
;; inwith whose interstices two or three operands, desumed from the
;; vale of signed integer numbers, partake, and the signed
;; integer-valued cells of a bilaterally infinite memory, partially
;; nuncupated in certain units to particular purposes.
;; 
;; == ONE INSTRUCTION, ELEVEN VARIANTS THROUGH "`" and "#" ==
;; One moeity of ```'s nature as a multum in parvo acquires its
;; emergence from the undecimal account of variations on forbisens
;; inchoating in backticks, "`", whose tally and distribution, together
;; with the hash sign's ("#") contribution, serves in the specification
;; of the destination memory cell's address, the source value's
;; composition, and both aspect's tiers of indirections.
;; 
;; == THE MEMORY: A BILATERALLY INFINITE DISPANSION OF INTEGERS ==
;; The second, paregal, stratum installing, based on the potential, an
;; entelechy of competences is begotten by the memory and its set of
;; specialized elements.
;; 
;; The program memory's conformation registers a bilaterally infinite
;; catena of cells, amenable to signed integer addresses of any
;; mickleness. Their capacity, a scalar integer, wists of the same
;; magnamity in the polarity and expanse. In the instant of its
;; inchoation, each cell assumes the default state of zero (0).
;; 
;; == CERTAIN MEMORY AREAS ARE ENTALENTED WITH SPECIAL DUTIES ==
;; A particular subset desumed from the bourneless memory's dispansion
;; enjoys a status nuncupated to a particular telos; the following
;; tabulation's cynosure shall thus be the memory layout's explication,
;; expending an emphasis on its reserved segments:
;; 
;;   ------------------------------------------------------------------
;;   Cell  | Role
;;   ------+-----------------------------------------------------------
;;   0     | Instruction pointer (IP).
;;         |-----------------------------------------------------------
;;         | Capacitates control flow executions by maintaining the
;;         | instruction pointer (IP) position.
;;         |-----------------------------------------------------------
;;         | The enumeration is zero-based, that is, the first
;;         | instruction is indexed with zero (0).
;;   ..................................................................
;;   1     | Conditional execution.
;;         |-----------------------------------------------------------
;;         | If the cell value is not zero (0), no instructions, except
;;         | those affecting cell #1, are executed.
;;   ..................................................................
;;   2     | Input/Output activation.
;;         |-----------------------------------------------------------
;;         | If written a non-zero value, the input/output capabilities
;;         | will be performed; ensuing from their patration, the cell
;;         | #2 subsequent recurs to its default state of zero (0).
;;         |-----------------------------------------------------------
;;         | The selection betwixt input and output constitutes the
;;         | bailiwick of cell #3, which please consult alow.
;;   ..................................................................
;;   3     | Input/Output mode selection.
;;         |-----------------------------------------------------------
;;         | Determines whether input should be requested or output
;;         | issued, depending upon the value of cell #3:
;;         |   --------------------------------------------------------
;;         |   Value | Mode   | Effect
;;         |   ------+--------+----------------------------------------
;;         |   0     | Output | Reads the bits in the cells from the
;;         |         |        | address of inclusive four (4) to
;;         |         |        | inclusive 24, assembles these from the
;;         |         |        | most significant position (MSB) to the
;;         |         |        | least significant into a 21-bit
;;         |         |        | unsigned integer number, and prints the
;;         |         |        | character whose Unicode code point
;;         |         |        | conflates with the same to the standard
;;         |         |        | output.
;;         |   ........................................................
;;         |   1     | Input  | Queries the standard input for a
;;         |         |        | Unicode character, writes its 21 bits,
;;         |         |        | from the most significant position
;;         |         |        | (MSB) towards the least significant one
;;         |         |        | (LSB) into the memory cells commencing
;;         |         |        | from the inclusive address four (4) and
;;         |         |        | terminating in the inclusive address
;;         |         |        | 24.
;;         |   --------------------------------------------------------
;;         |-----------------------------------------------------------
;;         | In order to actual incite input/output, cell #2 must be
;;         | configured appropriately, which please consult aboon.
;;   ..................................................................
;;   4--24 | Input/Output buffer.
;;         |-----------------------------------------------------------
;;         | Entails the bits reserved for the performance of input and
;;         | output operations, supporting the entire Unicode range.
;;   ..................................................................
;;   25--  | No epiphenomena associated.
;;         |-----------------------------------------------------------
;;         | These cells abstain from a particular response's
;;         | elicitation; as a corollary, they may be liberally
;;         | employed for personal data management.
;;   ------------------------------------------------------------------
;; 
;; An amplification of the aesthetics in the presentation shall be the
;; coming diagram's cynosure.
;; 
;; Please heed that the spatial impositions bereave the illustration
;; from the entirety of its intended diction's adduction; as such, a
;; system of keys serves in the abbreviation of the cells' agencies:
;; 
;;   ------------------------------------------------------------------
;;   Key | Interpretation
;;   ----+-------------------------------------------------------------
;;   IP  | Instruction pointer.
;;   ..................................................................
;;   CE  | Conditional execution.
;;   ..................................................................
;;   IOS | I/O switch: activates or deactives input/output
;;       | capabilities.
;;   ..................................................................
;;   IOM | I/O mode: determines input/output direction.
;;   ..................................................................
;;   IOB | I/O bits: bits reserved for the input/output Unicode
;;       | representation.
;;   ------------------------------------------------------------------
;; 
;; Endowed with this requisitum of gnarity, the diagram ensues as:
;; 
;;   ------------------------------------------------------------------
;;   Cell # ||  0  |  1  |  2  |  3  | 4--24 | 25--
;;   -------||-----+-----+-----+-----+-------+-------------------------
;;   Role   || IP  | CE  | IOS | IOM |  IOB  | general-purpose
;;   ------------------------------------------------------------------
;; 
;; == INPUT AND OUTPUT: GOVERNED BY A SWITCH AND A MODE ==
;; The instigation of input and output constitutes a causatum whose
;; parentage ensues from the coefficiency of the cell at the address
;; two (2), as the enabling or deactivating force, and the state at the
;; third cell, entalented with the differentiation betwixt the input
;; and output mode's selection.
;; 
;; The vinculum whose purview's perimeter amplects the input and output
;; modes in relation to the memory cells entalented with the respective
;; dedication serves as the below tabulation's material:
;; 
;;   ------------------------------------------------
;;    I/O mode | State of cell #2 | State of cell #3
;;   ----------+------------------+------------------
;;    Output   |     non-zero     |      zero
;;   ................................................
;;    Input    |     non-zero     |      one
;;   ------------------------------------------------
;; 
;; == INPUT AND OUTPUT: A UNICODE CHARACTER ENCODED IN 21 CELLS ==
;; The ``` programming language, reserving a catena of 21 accolent cells
;; to the purpose of the castaldy of a contemporaneously memorized
;; Unicode character's encoded form, commencing from the inclusive cell
;; index four (4), and concluding with the inclusive address 24, applies
;; itself to its replication in the system in a binary representation.
;; 
;; The spatial accommodation concurs with the Unicode standard's
;; allotted range, a compass of 21 bits whose amplectation attends to
;; the closed interval of, expressed in hexadecimal notation,
;; [0, 10FFFF]. The first memory address, empight on the position four
;; (4), references the character code point's most significant bit
;; (MSB), proceeding towards the least significant bit (LSB) in the
;; 24th cell.
;; 
;; Everichon among these 21 cells ought to lend a commorancy to an
;; aefauld bit; a deviation from this covenant instigates an erroneous
;; situation.
;; 
;; == STORAGE OF A UNICODE CODE POINT IN THE PROGRAM MEMORY ==
;; A Unicode character's transfer to the memory ensues from its code
;; point's most significant bit (MSB) and terminates in the least
;; significant one (LSB), the cell addressed accommodated to the former
;; accounting for the index four (4), that for the latter defined as
;; the location 24.
;; 
;; The following pseudocode illustration shall apply to this notion:
;; 
;;   procedure writeCodePointToMemory (memory, codePoint)
;;     Input:
;;       memory:    The program memory which shall admit the CODE_POINT.
;;       codePoint: The code point of the Unicode character to transfer
;;                  to the program MEMORY, defined as integral number
;;                  occupying the closed interval [0, 2^{21} - 1].
;;     
;;     Output:
;;       None.
;;     
;;     Process:
;;       { The current index into the codePoint, commencing with the }
;;       { most significant bit (MSB), and decreasing towards the    }
;;       { least significant one (LSB) at position zero (0).         }
;;       let bitIndex <- 20
;;       
;;       for currentCellIndex from 4 to 24 do
;;         let bitInCodePoint       <- codePoint[bitIndex]
;;         
;;         memory[currentCellIndex] <- bitInCodePoint
;;         bitIndex                 <- bitIndex - 1
;;       end for
;;   end procedure
;; 
;; == READING OF A UNICODE CHARACTER FROM THE PROGRAM MEMORY ==
;; The athwart procession, from the memory state with its 21 cells ---
;; and 21 bits ---, to the Unicode code point, emerges from the selfsame
;; principles, thilk, iterum, shall be a pseudocode treatise's cynosure:
;; 
;;   function readCodePointFromMemory (memory)
;;     Input:
;;       memory:    The program memory whose 21 dedicated cells
;;                  encompass the Unicode code point's 21 bits.
;;     
;;     Output:
;;       codePoint: The code point of the Unicode character to extract
;;                  from the program MEMORY, defined as integral number
;;                  occupying the closed interval [0, 2^{21} - 1].
;;     
;;     Process:
;;       let codePoint <- 0
;;       { The current index into the codePoint, commencing with the }
;;       { most significant bit (MSB), and decreasing towards the    }
;;       { least significant one (LSB) at position zero (0).         }
;;       let bitIndex  <- 20
;;       
;;       for currentCellIndex from 4 to 24 do
;;         let bitForCodePoint <- memory[currentCellIndex]
;;         
;;         codePoint[bitIndex] <- bitForCodePoint
;;         bitIndex            <- bitIndex - 1
;;       end for
;;   end function
;; 
;; 
;; Syntax
;; ======
;; A conspection's application upon the syntaxis bewrays a ``` program
;; as a composition of zero or more variations on the same instruction
;; forbisen, compact of backticks ("`"), hash sign ("#") instances,
;; and signed or unsigned integer numbers.
;; 
;; Each twissel of consecutive operation invocations experiences a
;; segregation via one or more whitespaces in a sepiment's agency.
;; 
;; Any character forinsecal to the aboon mentioned set is a subject of
;; interdiction, and, as a consectary, a fount of an abortive error.
;; 
;; == COMMENTS ==
;; No provision for comments partakes of the language's contemporaneous
;; rendition.
;; 
;; 
;; Instructions
;; ============
;; The ``` instruction set enumerates an aefauld member, the
;; multifarious causata assigned their woning in this unassuming guise
;; emanates from the champarty of an undecimal cardinality in formats
;; and the epiphenomena whose incorporations are edified in
;; special-purpose memory cells.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be the eleven conceivable
;; appearances' elucidation.
;; 
;; Please heed that the Latin minuscules "a", "b" and "c", serves as
;; succedanea, their occupied tmemata intended for the supersession by
;; actual ``` code in the ultimate program. Each such placeholder
;; expects a signed or unsigned integer to supplant its stead.
;; 
;;   ------------------------------------------------------------------
;;   Pattern  | Destination format         | Source format
;;   ---------+----------------------------+---------------------------
;;   `a`#b    | cells[a]                   | b
;;   ..................................................................
;;   `a`b     | cells[a]                   | cells[b]
;;   ..................................................................
;;   `a``b    | cells[a]                   | cells[cells[b]]
;;   ..................................................................
;;   `a``b#c  | cells[a]                   | cells[cells[b] + c]
;;   ..................................................................
;;   `a``b`c  | cells[a]                   | cells[cells[b] + cells[c]]
;;   ..................................................................
;;   ``a`#b   | cells[cells[a]]            | b
;;   ..................................................................
;;   ``a#b`#c | cells[cells[a] + b]        | c
;;   ..................................................................
;;   ``a`b`#c | cells[cells[a] + cells[b]] | c
;;   ..................................................................
;;   ``a`b    | cells[cells[a]]            | cells[b]
;;   ..................................................................
;;   ``a#b`c  | cells[cells[a] + b]        | cells[c]
;;   ..................................................................
;;   ``a`b`c  | cells[cells[a] + cells[b]] | cells[c]
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of a vector of commands
;; from a sequence of tokens.
;; 
;; == PARSERS AND COMBINATORS ARE FUNCTIONS ==
;; In eath diction, the parser combinator approach constructs a complete
;; parser entity from a sequence of interoperating smaller parsers,
;; their coefficiency enabled through combinators.
;; 
;; Both parsers and combinators are, in their pristine diorism,
;; represented by functions, accepting a source to parse and returning
;; in the case of a successful application a composition apprehending at
;; least
;; 
;;   - The remaining portion of the source, curtailed by the consumed
;;     items.
;;     If, for instance, the source represents a string, the first
;;     characters matching the parsing predicate will be removed; for
;;     tokens in lieu of this direct input, the residue following the
;;     accepted token objects are delivered.
;;   - An object designating the parser's or combinator's contribution
;;     to the encompassing whole, that is, usually an AST node.
;; 
;; A failure in the parser's or combinator's operations usually
;; concludes either with a communicative flag or an error signaling.
;; 
;; Conforming to an augmentation in formality, the following signature
;; may be proffered for parsers and combinators:
;; 
;;   function (source : any) -> (newSource : any, output : any)
;; 
;; == PARSERS AND COMBINATORS ARE INTERWOVEN IN SERIES ==
;; Considering the successful case, the modified parser or combinator
;; source is utilized as an input to the subsequent parser/combinator,
;; chaining these into a series of processors that, in concluding in an
;; ultimately empty source, build the output structure, for instance,
;; the abstract syntax tree.
;; 
;; == PARSERS EQUAL COMBINATORS ==
;; The discrepancy betwixt parsers and combinators constitutes a rather
;; puisne question of terminology for most objectives, as both partake
;; of a functional commonality. Parsers are usually "stand-alone"
;; components, responsible for the actual modification of the source,
;; whereas combinators ligate zero or more parsers, or other
;; combinators, in order to accompass a result.
;; 
;; If we have, as an example, a parser "characterOf", defined as
;; 
;;   function characterOf (expectedCharacter : character)
;;     let characterParser <- function (source : string)
;;       if source[0] = expectedCharacter then
;;         return (source.substring (1, source.length),
;;                 makeNode(NODE_TYPE_CHARACTER, source[0])
;;       else
;;         return null
;;       end if
;;     end function
;;     
;;     return characterParser
;;   end function
;; 
;; the requisitum involved in parsing more than one character coerces us
;; to discover a chaining of mandatorily matching "characterOf"
;; invocations. To this end, we define the following combinator:
;; 
;;   function allMatch (parsers : parserFunction[0..*])
;;     let allCombinator <- function (source : string)
;;       let newSource <- source
;;       let nodes     <- empty node list
;;       for every parser currentParser in parsers do
;;         let parserResult <- currentParser(source)
;;         
;;         if parserResult is null then
;;           return null
;;         else
;;           newSource <- parserResult[0]
;;           append parserResult[1] to nodes
;;         end if
;;       end for
;;       
;;       return (newSource, nodes)
;;     end function
;;     
;;     return allCombinator
;;   end function
;; 
;; An exemplary invocation of the combinator "allMatch" with several
;; instances of the "characterOf" parser could involve:
;; 
;;   parse (allMatch (characterOf ('h'),
;;                    characterOf ('e'),
;;                    characterOf ('l'),
;;                    characterOf ('l'),
;;                    characterOf ('o')),
;;          "hello")
;; 
;; == A PARSER COMBINATOR IN AN OBJECT-ORIENTED CONTEXT ==
;; The principal and onomastic substrate derives from Jeffrey Massung's
;; "parse" package for Common Lisp, which please see under
;; [massung2020parse]. A diverging aspect is apportioned its commorancy
;; in the object-oriented variation, substituting the functional notions
;; in order to emphasize the coefficacy partaken of by the several
;; components.
;; 
;; 
;; Appendices
;; ==========
;; A few topics limned in a state of caligation partake in this
;; documentation's periphery, their pertinence neither disranked in such
;; sufficiency vindicating their complete expungement, nor enhaused with
;; a buoyancy whose fortitude redes their inclusion in the main text
;; body.
;; 
;; As a consequence, the following tmemata shall apply themselves as
;; parergons for those readers entalented with a curiosity's mete that
;; intrudes the project's more occluded viscera.
;; 
;; == APPENDIX A: PROJECT FILES ==
;; The extensive perimeter intrinsic to this project has been reckoned
;; to embrace a complexity sufficiently potent as to impose a
;; destructuring into several interrelated and coefficent files. The
;; order of their importing into a main context constitutes a variable
;; of a fixed ordonnance, elucidated in the following table.
;; 
;; Please note that at least one infrastructure of nearly official
;; weight exists for such project management purposes, realized in the
;; definition of file interfaces using packages, and their orders and
;; relationships' enunciation by the "ASDF" system. This simple example,
;; however, has been adjudged as rather inflicted with a digressive
;; cumbrance in an advenient structuring's adhibition, rather than its
;; enjoying - a more serious enterprise certainly would be assayed in an
;; athwart airt.
;; 
;;   ------------------------------------------------------------------
;;   No. | File                      | Role
;;   ----+---------------------------+---------------------------------
;;    1  | types.lisp                | Defines the operations on types
;;       |                           | and the custom types employed in
;;       |                           | the subsequent files, such as
;;       |                           | ``list-of'' and ``destination''.
;;   ..................................................................
;;    2  | logical-operations.lisp   | Implements operations dedicated
;;       |                           | to the handling of actions
;;       |                           | commorant in the Boolean realm,
;;       |                           | most prominently the conversion
;;       |                           | of "generalized boolean" objects
;;       |                           | to actual ``boolean'' sentinels.
;;   ..................................................................
;;    3  | character-operations.lisp | Implements the operations
;;       |                           | targeting the indagation of
;;       |                           | characters.
;;   ..................................................................
;;    4  | token.lisp                | Implements the tokens, which are
;;       |                           | produced by the lexer during the
;;       |                           | scanning of the ``` source code
;;       |                           | string.
;;   ..................................................................
;;    5  | lexer.lisp                | Implements the lexer,
;;       |                           | responsible for extracting the
;;       |                           | tokens from a piece of ```
;;       |                           | source code specified in string
;;       |                           | form.
;;   ..................................................................
;;    6  | memory.lisp               | Implements the program memory as
;;       |                           | a sparse vector integer-valued
;;       |                           | cells, based upon a hash table,
;;       |                           | the keys of which relate to the
;;       |                           | addresses, the values to the
;;       |                           | cell states.
;;   ..................................................................
;;    7  | operands.lisp             | Implements the instruction class
;;       |                           | and its operand representations,
;;       |                           | the latter capacitates a
;;       |                           | discrimination betwixt immediate
;;       |                           | and referential instruction
;;       |                           | components.
;;   ..................................................................
;;    8  | program.lisp              | Implements the operations for
;;       |                           | the creation of executable ```
;;       |                           | program models.
;;   ..................................................................
;;    9  | parser.lisp               | Implements the parser, whose
;;       |                           | onus it is to assemble the
;;       |                           | tokens generated by the lexer by
;;       |                           | adminiculum of parsers and
;;       |                           | combinators into an ordered
;;       |                           | sequence of instructions.
;;   ..................................................................
;;   10  | unicode-operations.lisp   | Implements the facilities for
;;       |                           | the writing of a Unicode code
;;       |                           | points' bits to the memory and
;;       |                           | the athwart obtention of these
;;       |                           | in order to restore from the
;;       |                           | binary cells the encoded value.
;;   ..................................................................
;;   11  | interpreter.lisp          | Implements the interpreter, the
;;       |                           | agent tasked with the evaluation
;;       |                           | of the instruction objects
;;       |                           | elicited from the parser's
;;       |                           | efforts.
;;   ..................................................................
;;   12  | tests.lisp                | Implements the test cases and
;;       |                           | examples for demonstrating the
;;       |                           | interpreter's conformance with
;;       |                           | the ``` programming language.
;;   ..................................................................
;;   13  | main.lisp                 | Establishes the starting point
;;       |                           | into this application, in
;;       |                           | particular loading the
;;       |                           | aforementioned Common Lisp
;;       |                           | source files.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-03-25
;; 
;; Sources:
;;   [cox2025unicodebytes]
;;   Graham Cox, "How Many Bytes Does One Unicode Character Take?",
;;     March 26th, 2025
;;   URL: "https://www.baeldung.com/cs/unicode-character-bytes"
;;   Notes:
;;     - Mentions the fact that Unicode requires 21 bits for its entire
;;       code point range's representation.
;;     - Specifies the code point range, in hexadecimal notation, as
;;       the closed interval [U+000000, U+10FFFF].
;;   
;;   [devanla2021minimalparsecomb]
;;   Guru Devanla, "Minimal Parser Combinator in Python",
;;                 26th October 2021
;;   URL: "https://gdevanla.github.io/posts/
;;         write-a-parser-combinator-in-python.html"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in Python.
;;   
;;   [elouafi2018gentleintroparscomb]
;;   Yassine Elouafi, "A gentle introduction to parser combinators",
;;                    2018
;;   URL: "https://dev.to/yelouafi/
;;         a-gentle-introduction-to-parser-combinators-21a0"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [elouafi2021introparsercomb]
;;   Yassine Elouafi, "introduction-to-parser-combinators.md",
;;                    June 28, 2021 
;;   URL: "https://gist.github.com/yelouafi/
;;         556e5159e869952335e01f6b473c4ec1"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [esolang2023```]
;;   The Esolang contributors, "```", February 19th, 2023
;;   URL: "https://esolangs.org/wiki/%60%60%60"
;;   
;;   [goodrich214datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", sixth edition, 2014,
;;     pages 122--127
;;   Notes:
;;     - Describes the concept and an implementation of the singly
;;       linked list in the Java programming language.
;;     - The pages 276 through 280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;;   
;;   [massung2020parse]
;;   Jeffrey Massung, "The PARSE Package", 2020
;;   URL: "https://github.com/massung/parse"
;;   Notes:
;;     - GitHub repository of the "parse" package, a Common Lisp library
;;       for token parsing which employs parser combinators.
;;   
;;   [mulligan2023unlocking]
;;   Rory Mulligan, "Unlocking the Power of Parser Combinators: A
;;                   Beginner's Guide", February 9, 2023
;;   URL: "https://www.sitepen.com/blog/
;;         unlocking-the-power-of-parser-combinators-a-beginners-guide"
;;   
;;   [stackoverflow2011q6339756]
;;   The Stack Overflow contributors,
;;     "Why UTF-32 exists whereas only 21 bits are necessary to encode
;;      every character?", June 14th, 2011
;;   URL: "https://stackoverflow.com/questions/6339756/
;;         why-utf-32-exists-whereas-only-21-bits-are-necessary-to-
;;         encode-every-character"
;;   Notes:
;;     - Mentions that Unicode code points occupy 21 bits.
;;     - Expresses the covered range, in hexadecimal notation, as
;;       [0, 10FFFF].
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype file-source ()
  "The ``file-source'' type defines the set of objects enjoying the
   homologation of acting as a provenance for a file's adit."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of project file loaders.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type pathname +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "Specifies the directory lending a commorancy to the Common Lisp
   project files.
   ---
   Please substitute this global variable's content by the actual
   directory on your system which comprehends the ``` interpreter's
   source files.
   ---
   Several facilities are enumerated in the contingencies of Common
   Lisp standard library for this kind of specification:
   
     ------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+------------------------------------
     make-pathname    | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                \"Users\"
                      |                \"Kaveh\"
                      |                \"```\"
                      |                \"```_001\"))
     ......................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/```/```_001/\")
     ------------------------------------------------------")

;;; -------------------------------------------------------

(defun import-project-file (source)
  "Loads the Common Lisp SOURCE file, expected to maintain its woning in
   the +PROJECT-DIRECTORY+, and returns no value."
  (declare (type file-source source))
  (load
    (merge-pathnames +PROJECT-DIRECTORY+ source))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of project files.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-project-file "types.lisp")
(import-project-file "logical-operations.lisp")
(import-project-file "character-operations.lisp")
(import-project-file "token.lisp")
(import-project-file "lexer.lisp")
(import-project-file "memory.lisp")
(import-project-file "operands.lisp")
(import-project-file "program.lisp")
(import-project-file "parser.lisp")
(import-project-file "unicode-operations.lisp")
(import-project-file "interpreter.lisp")
(import-project-file "tests.lisp")
