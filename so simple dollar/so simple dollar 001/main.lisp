;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "so simple dollar", presented by the Esolang user
;; "Threesodas" in the year 2021, and based upon the encoding of
;; commands in the tally of adjacent dollar signs ("$").
;; 
;; Concept
;; =======
;; so simple dollar programs consist of dollar sign ("$") sequences,
;; whose lengths associate unambiguously with the 35 available commands,
;; accompanied by whitespaces acting in the agency of demarcations
;; betwixt these encoding constituents, while no other character is
;; invited to contribute to the source code.
;; 
;; The data castaldy is assigned to an infinite tape of arbitrary
;; integer-valued entities, responding to input and output requests.
;; 
;; == SO SIMPLE DOLLAR: A JOKE LANGUAGE ==
;; The so simple dollar programming language belongs to the joke species
;; of the esoteric category, its kenspeckle haecceity being limned by a
;; reliance upon dollar signs ("$") as the exclusive tokens of operative
;; effectiveness.
;; 
;; == DOLLAR SIGN ("$") SEQUENCES ENCODE COMMANDS ==
;; The dioristic entity of the language, dollar signs manifest as the
;; sole sensible constituent, their tally serving to indicate the
;; expressed command among a set of 35 candidates. Any such token is
;; circumscribed by a series of adjacent "$" instances, distinguished
;; from peers by at least one whitespace in the intermede.
;; 
;; == BLOCKS DESCRIBE THE PROGRAM STATE ==
;; A so simple dollar program may be considered as a resident of one
;; among two distinct states at any time: the general and the printing
;; state.
;; 
;; Required for a program's delineation, a start and an end marker
;; bracket the complete instruction sequence, locating the program in
;; the default general state at its inchoation.
;; 
;; Output operations must be embosomed in the second variation, the
;; printing state, also expanding betwixt two particular commands
;; serving as its posts. A restricted command set, tolerating printing
;; instructions only, governs this area.
;; 
;; Decurring from the printing state, the program iterum assumes its
;; general mode.
;; 
;; == THE MEMORY: A COMPOSITION OF INTEGER CELLS ==
;; Its responsibility for the data management is fulfilled by the
;; program's tape-like memory, a linear arrangement of cells, infinite
;; in its bilateral extent, with each component lending storage to a
;; single signed and unbounded integer value.
;; 
;; A cell pointer, capable of traveling sinistrally as well as dextrally
;; by single steps, designates at any instant the currently active cell,
;; amenable to input receipts and output responses.
;; 
;; 
;; Data Types
;; ==========
;; so simple dollar's type system is defined by a bifurcation into the
;; tacitly operating signed integer objects, their woning being the
;; memory's cells, and the character department, its currency relating
;; to input and output concerns.
;; 
;; == CHARACTERS: THE CURRENCY OF COMMUNICATION ==
;; The paravaunt currency of any so simple dollar program, characters
;; respond to the input and output conduits --- both exclusive to the
;; language's capacities.
;; 
;; Twenty-six of the thirty-five operations defining the instruction set
;; are exhausted by the printing of the equinumerant minuscule letters
;; of the Latin alphabet. A further access to the currently selected
;; cell value in an aspect as the code to the character to print serves
;; to bolster the data type's significance.
;; 
;; In the course of user input obtention, a character is expected to
;; provide the basis for the integer code to transmit into the memory's
;; active cell.
;; 
;; == INTEGERS: RESIDENTS OF THE MEMORY ==
;; The numeric moeity of so simple dollar's types embraces signed
;; integers of unbounded magnitude, that is, occupants of the range
;; [-infinity, +infinity].
;; 
;; A composition of signed integer-valued cells, amenable chiefly to
;; input and output conversions, the memory exercises a rather passive
;; role. The transcription of input characters into their numeric codes,
;; and the athwart process from the cell's integer to the
;; character-based display, represent a concept rather eloigned from
;; direction impact on the user.
;; 
;; 
;; Architecture
;; ============
;; Programs in the language are granted the adit to a theoretically
;; infinite number of linearly arranged cells, each such a salvatory to
;; a single signed integer value of unbounded magnitude, and initialized
;; with a default of zero (0).
;; 
;; At any instant, a cell pointer, residing at the first member of the
;; memory, designates the currently active cell. Its amenability to
;; certain operations capacitates a stepwise translation along both
;; axes, sinistrally as well as dextrally. Only the current cell may be
;; inquired and modified.
;; 
;; 
;; Syntax
;; ======
;; The so simple dollar programming language's syntaxis constitutes a
;; sequence of two or more dollar sign "$" sequences, segregated by
;; whitespaces, with the tally of adjacent "$" elements affiliated in a
;; unique way with the 35 distinct commands.
;; 
;; == WHITESPACES ==
;; The significance of whitespaces, embracing both spaces, tabs, and
;; linebreaks, depends upon the concrete context, with their presence
;; betwixt commands, that is, undisturbed "$" strings, constituting a
;; prerequisite for the identifiers' distinguishment. At least one
;; instance is required to separate each two such tokens.
;; 
;; While required in the interstices of tokens, any other occurrence, as
;; well as the tally, does not partake of effectivity. These instances,
;; as a consectary, are encountered with tolerance.
;; 
;; == COMMENTS ==
;; No accoutrements are yet furnished for the introduction of comments
;; into so simple dollar programs.
;; 
;; == GRAMMAR ==
;; The language's donat may be formulated in the following Extended
;; Backus-Naur Form (EBFN) description:
;; 
;;   program      := whitespaces
;;                ,  startProgram
;;                ,  sepiment
;;                ,  { statement , sepiment }
;;                ,  endProgram
;;                ,  whitespaces
;;                ;
;;   statement    := printBlock
;;                |  takeInput
;;                |  storeInput
;;                |  moveLeft
;;                |  moveRight
;;                ;
;;   
;;   startProgram := "$" ;
;;   endProgram   := "$$" ;
;;   printBlock   := startPrint , { printCommand } , endPrint ;
;;   startPrint   := "$$$" ;
;;   endPrint     := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printCommand := printLetterA
;;                |  printLetterB
;;                |  printLetterC
;;                |  printLetterD
;;                |  printLetterE
;;                |  printLetterF
;;                |  printLetterG
;;                |  printLetterH
;;                |  printLetterI
;;                |  printLetterJ
;;                |  printLetterK
;;                |  printLetterL
;;                |  printLetterM
;;                |  printLetterN
;;                |  printLetterO
;;                |  printLetterP
;;                |  printLetterQ
;;                |  printLetterR
;;                |  printLetterS
;;                |  printLetterT
;;                |  printLetterU
;;                |  printLetterV
;;                |  printLetterW
;;                |  printLetterX
;;                |  printLetterY
;;                |  printLetterZ
;;                |  printCell
;;                ;
;;   printLetterA := "$$$$" ;
;;   printLetterB := "$$$$$" ;
;;   printLetterC := "$$$$$$" ;
;;   printLetterD := "$$$$$$$" ;
;;   printLetterE := "$$$$$$$$" ;
;;   printLetterF := "$$$$$$$$$" ;
;;   printLetterG := "$$$$$$$$$$" ;
;;   printLetterH := "$$$$$$$$$$$" ;
;;   printLetterI := "$$$$$$$$$$$$" ;
;;   printLetterJ := "$$$$$$$$$$$$$" ;
;;   printLetterK := "$$$$$$$$$$$$$$" ;
;;   printLetterL := "$$$$$$$$$$$$$$$" ;
;;   printLetterM := "$$$$$$$$$$$$$$$$" ;
;;   printLetterN := "$$$$$$$$$$$$$$$$$" ;
;;   printLetterO := "$$$$$$$$$$$$$$$$$$" ;
;;   printLetterP := "$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterQ := "$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterR := "$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterS := "$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterT := "$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterU := "$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterV := "$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterW := "$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterX := "$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterY := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printLetterZ := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   printCell    := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   
;;   takeInput    := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   storeInput   := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   moveRight    := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   moveLeft     := "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ;
;;   
;;   sepiment     := whitespace , whitspaces ;
;;   whitespaces  := { whitespace } ;
;;   whitespace   := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; so simple dollar furnishes an instruction set whose perimeter
;; embraces 35 members, with 26 specimens deployed in the printing of an
;; equinumerant array of minuscular letters.
;; 
;; Maugre its quantitative expanse, the exclusive facilities appertain
;; to input and output.
;; 
;; == OVERVIEW ==
;; The following apercu aspires to adhibit at least a modest mete of
;; nortelry anenst the commands and their effects:
;; 
;;   ------------------------------------------------------------------
;;   Command identifier                  | Count | Effect
;;   ------------------------------------+-------+---------------------
;;   $                                   |   1   | Starts program.
;;   ..................................................................
;;   $$                                  |   2   | Ends program.
;;   ..................................................................
;;   $$$                                 |   3   | Starts printing.
;;   ..................................................................
;;   $$$$                                |   4   | Prints letter "a".
;;   ..................................................................
;;   $$$$$                               |   5   | Prints letter "b".
;;   ..................................................................
;;   $$$$$$                              |   6   | Prints letter "c".
;;   ..................................................................
;;   $$$$$$$                             |   7   | Prints letter "d".
;;   ..................................................................
;;   $$$$$$$$                            |   8   | Prints letter "e".
;;   ..................................................................
;;   $$$$$$$$$                           |   9   | Prints letter "f".
;;   ..................................................................
;;   $$$$$$$$$$                          |  10   | Prints letter "g".
;;   ..................................................................
;;   $$$$$$$$$$$                         |  11   | Prints letter "h".
;;   ..................................................................
;;   $$$$$$$$$$$$                        |  12   | Prints letter "i".
;;   ..................................................................
;;   $$$$$$$$$$$$$                       |  13   | Prints letter "j".
;;   ..................................................................
;;   $$$$$$$$$$$$$$                      |  14   | Prints letter "k".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$                     |  15   | Prints letter "l".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$                    |  16   | Prints letter "m".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$                   |  17   | Prints letter "n".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$                  |  18   | Prints letter "o".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$                 |  19   | Prints letter "p".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$                |  20   | Prints letter "q".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$               |  21   | Prints letter "r".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$              |  22   | Prints letter "s".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$             |  23   | Prints letter "t".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$            |  24   | Prints letter "u".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$           |  25   | Prints letter "v".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$          |  26   | Prints letter "w".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$         |  27   | Prints letter "x".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$        |  28   | Prints letter "y".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$       |  29   | Prints letter "z".
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      |  30   | Ends printing.
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$     |  31   | Queries user input.
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    |  32   | Stores input.
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   |  33   | Moves pointer right.
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  |  34   | Moves pointer left.
;;   ..................................................................
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ |  35   | Prints current cell.
;;   ------------------------------------------------------------------
;; 
;; Evident in all operative departments, a conspicuous magnanimity
;; governs the account and construe of the dollar sign; its mickleness'
;; alleviation will be assayed with the alow diagram, colocating with
;; the "$" tallies the respective commands:
;; 
;;   1=Begin program
;;   |2=End program
;;   ||3=Begin printing
;;   |||4=Print letter "a"
;;   ||||5=Print letter "b"
;;   |||||6=Print letter "c"
;;   ||||||7=Print letter "d"
;;   |||||||8=Print letter "e"
;;   ||||||||9=Print letter "f"
;;   |||||||||10=Print letter "g"
;;   ||||||||||11=Print letter "h"
;;   |||||||||||12=Print letter "i"
;;   ||||||||||||13=Print letter "j"
;;   |||||||||||||14=Print letter "k"
;;   ||||||||||||||15=Print letter "l"
;;   |||||||||||||||16=Print letter "m"
;;   ||||||||||||||||17=Print letter "n"
;;   |||||||||||||||||18=Print letter "o"
;;   ||||||||||||||||||19=Print letter "p"
;;   |||||||||||||||||||20=Print letter "q"
;;   ||||||||||||||||||||21=Print letter "r"
;;   |||||||||||||||||||||22=Print letter "s"
;;   ||||||||||||||||||||||23=Print letter "t"
;;   |||||||||||||||||||||||24=Print letter "u"
;;   ||||||||||||||||||||||||25=Print letter "v"
;;   |||||||||||||||||||||||||26=Print letter "w"
;;   ||||||||||||||||||||||||||27=Print letter "x"
;;   |||||||||||||||||||||||||||28=Print letter "y"
;;   ||||||||||||||||||||||||||||29=Print letter "z"
;;   |||||||||||||||||||||||||||||30=End printing
;;   ||||||||||||||||||||||||||||||31=Take user input
;;   |||||||||||||||||||||||||||||||32=Store user input in current cell
;;   ||||||||||||||||||||||||||||||||33=Switch to right cell
;;   |||||||||||||||||||||||||||||||||34=Switch to left cell
;;   ||||||||||||||||||||||||||||||||||35=Print current cell
;;   |||||||||||||||||||||||||||||||||||
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;; 
;; == LETTER PRINTING OPERATIONS ==
;; The association applying to the dollar sign tallies and their
;; encounter's causatum with respect to the corresponding letters shall
;; be the following illustration's material:
;; 
;;      4                       29
;;      |                        |
;;   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;;      abcdefghijklmnopqrstuvwxyz
;; 
;; == OUTPUT ==
;; The preponderant potentials of so simple dollar retain their
;; commorancy inside of the output bailiwick, transmitting to the
;; standard conduit either a minuscular letters or the current cell
;; value's associated character when construed as a character code.
;; 
;; A requisitum to the output realization, the printing mode's
;; assumption ought to be established. The commencement is instigated by
;; the "$$$" token's adminiculum, and concluded when administered the
;; 30-positions "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$". Occupying this extent,
;; merely printing operations are apportioned homologation, a set
;; exhausted by the 26 Latin minuscules "$$$$" through
;; "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" and the cell value accessor
;; "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$". Any other ilk of statement
;; will perforce terminate in an error's provocation.
;; 
;; == INPUT ==
;; so simple dollar's input facility comprehends a kenspeckle concept
;; so very much concinnous with its linguistic diorisms; concretely, the
;; user input's receipt is segregated from its actual storage by two
;; disjunct operations, meaningful only when engaged in champarty.
;; 
;; In a first step, the 31-characters command
;; "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" queries the user for a single
;; character, similar to Urban Mueller's "brainfuck" esoteric
;; programming language, the intermediate storage of which resolves to
;; the programmer's own deliberations. Ensuing from the embraced datum,
;; the token "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$", exceeding the former's
;; length by exactly one position, serves to induce the input's
;; integer-valued character code into the current memory cell. For its
;; indagation, and subsequent employment, please consult the preceding
;; section "OUTPUT".
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; so simple dollar's modest capabilities, and its status as a joke
;; language, alleviate the potential for conceptual conflicts.
;; Natheless, a few points condeign of disquisition shall be presented
;; in the coming subsections.
;; 
;; == WHICH ROLE DOES THE MEMORY ASSUME? ==
;; The language's proterotype amplected a treble subset of commands
;; relating to the memory, expressed as consanguineous to brainfuck,
;; and enumerated by the user input storage, a dextral cell pointer
;; translation, as well as the latter's athwart motion. This roster,
;; however, failed to propose any effective expression of the cells'
;; contents, thus rendering the user input, in a mete unfortunately
;; equiponderant to the memory itself, a paragon of mateotechny.
;; 
;; In order to remedy this predicament, an instruction aliunde has been
;; introduced, the 35-positions "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
;; capable of printing the character associated with the current cell
;; value in the perspective of the same's character code.
;; 
;; == WHICH OPERATIONS ARE TOLERATED IN A PRINT BLOCK? ==
;; so simple dollar specifies two commands as the demarcating entities
;; for the message output, "$$$" for the inchoation,
;; "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" latreutical as the desinence. An
;; array of twenty-six operations, everichon a Latin minuscule's display
;; behest, are allotted to the text's assemblage. Yet, no explicit
;; homologation nor proscription incurs upon other commands.
;; 
;; It has been adjudged to encumber any non-output operation with the
;; status of a forinsecal entity, and thus subjected to interdiction.
;; 
;; 
;; Implementation
;; ==============
;; This implementation constitutes a simple solution, designed in the
;; programming language Common Lisp, and based upon the popular
;; distribution of responsibilites across a lexical analyzer (lexer),
;; an assembling parser, and an executing interpreter.
;; 
;; The treble participants shall be a cursory treatise's subject:
;; 
;;   (1) A lexical analyzer, frequently yclept a "lexer" or "scanner",
;;       acts in the latreutic agency of detecting in a piece of so
;;       simple dollar source code the effective objects and extracting
;;       these in the form of tokens, the same subsequently being made
;;       available to contingent interested parties.
;;   (2) The parser applies itself to the assemblage of an abstract
;;       syntax tree (AST) from the lexer's token stream. The AST
;;       provides a representation of the so simple dollar program's
;;       facilities in the form of nodes.
;;   (3) Receiving the abstract syntax tree, the interpreter visits its
;;       nodes, inducing actual effects to these elements.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-01
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/So_simple_dollar"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
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

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   composed of zero or more elements, their total tally ought to be
   even, with an indicator of the INDICATOR-TYPE followed by its
   associated value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (evenp (length (the list object)))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list object)
                by      #'cddr
              always
                (and (typep indicator indicator-type)
                     (typep value     value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-map'' type defines a mapping of attribute names,
   represented by keyword symbols, to at most one attribute value of any
   type."
  '(hash-table-of keyword T))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' type defines an infinite account of cells in the
   form of a hash table, associating with the cell indices, being the
   keys, the cell datum as the corresponding entry value."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant portion detected by the
   analyzation of a piece of so simple dollar source code."
  (type  (error "Missing token type." :type keyword))
  (value NIL                          :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier tokens.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array Token (*)) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (coerce
    (list
      (make-token :begin-program  "$")
      (make-token :end-program    "$$")
      (make-token :begin-printing "$$$")
      (make-token :letter         #\a)
      (make-token :letter         #\b)
      (make-token :letter         #\c)
      (make-token :letter         #\d)
      (make-token :letter         #\e)
      (make-token :letter         #\f)
      (make-token :letter         #\g)
      (make-token :letter         #\h)
      (make-token :letter         #\i)
      (make-token :letter         #\j)
      (make-token :letter         #\k)
      (make-token :letter         #\l)
      (make-token :letter         #\m)
      (make-token :letter         #\n)
      (make-token :letter         #\o)
      (make-token :letter         #\p)
      (make-token :letter         #\q)
      (make-token :letter         #\r)
      (make-token :letter         #\s)
      (make-token :letter         #\t)
      (make-token :letter         #\u)
      (make-token :letter         #\v)
      (make-token :letter         #\w)
      (make-token :letter         #\x)
      (make-token :letter         #\y)
      (make-token :letter         #\z)
      
      (make-token :end-printing "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      (make-token :take-input   "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      (make-token :store-input  "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      (make-token :switch-right "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      (make-token :switch-left  "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      
      (make-token :cell-value   "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"))
    '(simple-array Token (*)))
  "Maintains a one-dimensional simple array of recognized identifiers,
   amenable to dollar signs, with the element at index i of the array
   corresponding to the identifier composed of (i + 1) \"$\" symbols.
   ---
   Please note that, given the zero-based indexing of Common Lisp
   arrays, the one-symbol identifier \"$\" resides at the index zero
   (0), not at one (1), which would actually be more concinnously
   apportioned. All succeeding tokens follow this forbisen.")

;;; -------------------------------------------------------

(defun get-identifier (number-of-dollar-signs)
  "Returns the identifier token associated with the
   NUMBER-OF-DOLLAR-SIGNS, or signals an error of an unspecified type,
   if none such affiliation can be ascertained."
  (declare (type (integer 0 *) number-of-dollar-signs))
  (the Token
    (if (array-in-bounds-p +IDENTIFIERS+ (1- number-of-dollar-signs))
      (aref +IDENTIFIERS+ (1- number-of-dollar-signs))
      (error "Invalid number of dollar signs: ~d."
        number-of-dollar-signs))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer (source
                            &aux
                              (position 0)
                              (character
                                (when (plusp (length source))
                                  (char source 0))))))
  "The ``Lexer'' class models a lexical analyzer, or scanner, which
   extracts from a piece of so simple dollar source code a sequence of
   tokens."
  (source    (error "Missing source.") :type string)
  (position  0                         :type fixnum)
  (character NIL                       :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots to the local symbol macros
   ``source'', ``position'' and ``character, evaluates the BODY forms,
   and returns the last evaluated form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           ((source
             (the string
               (lexer-source ,evaluated-lexer)))
            (position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            (character
             (the (or null character)
               (lexer-character ,evaluated-lexer))))
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         (flet
             ((advance ()
               "Moves the LEXER's position cursor to the next character
                in its SOURCE, if possible, updates the current
                CHARACTER, and returns no value."
               (setf character
                 (when (array-in-bounds-p source (1+ position))
                   (char source (incf position))))
               (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current position into the LEXER's source, skips a
   sequences of zero or more abutting whitespaces, and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and character (whitespace-character-p character)) do
      (advance)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-dollar-signs (lexer)
  "Starting at the current position into the LEXER's source, reads a
   sequence of zero or more dollar signs (\"$\") and returns a token
   representing the corresponding identifier.
   ---
   An error of an unspecified type is signaled if the number of dollar
   signs does not correlate to any known language construct."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (get-identifier
        (loop
          while (and character (char= character #\$))
          do    (advance)
          count 1)))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any query with a
   fresh instance of an end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        
        ((char= character #\$)
          (lexer-read-dollar-signs lexer))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class represents a node in an abstract syntax tree
   (AST)."
  (type       (error "Missing node type.")  :type keyword)
  (attributes (make-hash-table :test #'eql) :type attribute-map))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' of the TYPE, optionally
   prepopulated with the INITIAL-ATTRIBUTES, manifested as a property
   list whose attribute names assume the role of the indicators, each
   succeeded by the affiliated attribute value."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) initial-attributes))
  (let ((node (initialize-node type)))
    (declare (type Node node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (node-attributes node))
              attribute-value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the value associated with the ATTRIBUTE-NAME in the NODE, or
   signals an error if no attribute with the indicator could be
   detected."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (multiple-value-bind (attribute-value contains-attribute-p)
      (gethash attribute-name (node-attributes node))
    (declare (type T attribute-value))
    (declare (type T contains-attribute-p))
    (the T
      (if contains-attribute-p
        attribute-value
        (error "Invalid attribute name ~s for node ~s."
          attribute-name node)))))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (loop
    initially
      (format stream "Node(type=~s" (node-type node))
    for attribute-name
      of-type keyword
      being the hash-keys in (node-attributes node)
    using
      (hash-value attribute-value)
    do
      (format stream ", ~a=>~s" attribute-name attribute-value)
    finally
      (format stream ")")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (lexer
                             &aux (current-token
                                    (lexer-get-next-token lexer)))))
  "The ``Parser'' class applies itself to the assemblage of a so simple
   dollar program's abstract syntax tree (AST) representation based upon
   the tokens supplied by a lexer."
  (lexer         (error "Missing lexer.") :type Lexer)
  (current-token (make-token :eof NIL)    :type Token))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and ``current-token''
   to eponymous local symbol macros, evaluates the BODY forms, and
   returns the last form's results.
   ---
   In addition to the two symbol macros, a local function of fixed
   designation is established:
   
     ------------------------------------------------------------------
     Local function | Effect
     ---------------+--------------------------------------------------
     eat (type)     | Returns the current token and stores the next one
                    | obtained from the parser's lexer, if the current
                    | token's type equals the specified type;
                    | otherwise, signals an error.
     ------------------------------------------------------------------"
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (declare (ignorable   ,evaluated-parser))
       (symbol-macrolet
           ((lexer
             (the Lexer
               (parser-lexer ,evaluated-parser)))
            (current-token
             (the Token
               (parser-current-token ,evaluated-parser))))
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         (declare (ignorable  lexer))
         (declare (ignorable  current-token))
         (flet
             ((eat (expected-token-type)
               "Checks whether the current token type conforms to the
                EXPECTED-TOKEN-TYPE, on confirmation returning the
                current token, while loading and storing the next one
                from the parser's lexer in its stead; otherwise, an
                error of an unspecified type is signaled."
               (declare (type keyword expected-token-type))
               (the Token
                 (if (token-type-p current-token expected-token-type)
                   (prog1 current-token
                     (setf current-token
                       (lexer-get-next-token lexer)))
                   (error "Expected a token of the type ~s, but ~
                           encountered ~s."
                     expected-token-type current-token)))))
           ,@body)))))

;;; -------------------------------------------------------

(defun parse-print-block (parser)
  "Parses a printing block using the PARSER and returns a
  ``:print-block'' type node presentation thereof."
  (declare (type Parser parser))
  (let ((statements NIL))
    (declare (type node-list statements))
    (with-parser (parser)
      (eat :begin-printing)
      
      (loop do
        (case (token-type current-token)
          (:eof
            (error "Unterminated printing block at token ~s."
              current-token))
          
          (:end-printing
            (loop-finish))
          
          (:letter
            (push (make-node :print-letter
                    :value (token-value current-token))
                  statements)
            (eat :letter))
          
          (:cell-value
            (push (make-node :print-user-input) statements)
            (eat :cell-value))
          
          (otherwise
            (error "Unexpected token in print block: ~s."
              current-token))))
      
      (eat :end-printing))
    
    (the Node
      (make-node :print-block
        :arguments (nreverse statements)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the tokens received by the PARSER and returns the root node of
   the abstract syntax tree (AST) representing the assembled so simple
   dollar program."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat :begin-program)
    
    (let ((statements NIL))
      (declare (type node-list statements))
      
      (loop do
        (case (token-type current-token)
          (:eof
            (loop-finish))
          
          (:begin-printing
            (push (parse-print-block parser) statements))
          
          (:take-input
            (eat :take-input)
            (push (make-node :take-input) statements))
          
          (:store-input
            (eat :store-input)
            (push (make-node :store-input) statements))
          
          (:switch-right
            (eat :switch-right)
            (push (make-node :switch-right) statements))
          
          (:switch-left
            (eat :switch-left)
            (push (make-node :switch-left) statements))
          
          (otherwise
            (loop-finish))))
      
      (eat :end-program)
      
      (the Node
        (make-node :program
          :statements (nreverse statements))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class implements a theoretically infinite arrangement
   of integer-scalar-valued cells, amenable to a cell pointer tha marks
   the currently active one."
  (cells   (make-hash-table :test #'eql) :type cell-map)
  (pointer 0                             :type integer))

;;; -------------------------------------------------------

(defmacro with-memory ((memory) &body body)
  "Evaluates the MEMORY, binds its slots ``cells'' and ``pointer'' to
   eponymous local symbol macros, evaluates the BODY forms, and returns
   the last form's results."
  (let ((evaluated-memory (gensym)))
    (declare (type symbol evaluated-memory))
    `(let ((,evaluated-memory ,memory))
       (declare (type Memory ,evaluated-memory))
       (declare (ignorable   ,evaluated-memory))
       (symbol-macrolet
           ((cells
             (the hash-table
               (memory-cells ,evaluated-memory)))
            (pointer
             (the integer
               (memory-pointer ,evaluated-memory))))
         (declare (type cell-map cells))
         (declare (type integer  pointer))
         (declare (ignorable     cells))
         (declare (ignorable     pointer))
         ,@body))))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the current MEMORY cell value."
  (declare (type Memory memory))
  (with-memory (memory)
    (the integer
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the current MEMORY cell and returns the
   modified MEMORY."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-memory (memory)
    (setf (gethash pointer cells 0) new-value))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer sinistrally to the preceding cell and
   returns the modified MEMORY."
  (declare (type Memory  memory))
  (with-memory (memory)
    (decf pointer))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer dextrally to the succeeding cell and
   returns the modified MEMORY."
  (declare (type Memory  memory))
  (with-memory (memory)
    (incf pointer))
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (tree)))
  "The ``Interpreter'' class implements a unit responsible for the
   adhibition of actual effect to an abstract syntax tree (AST)
   representation of a so simple dollar program."
  (tree       (error "Missing AST.") :type Node)
  (memory     (make-memory)          :type Memory)
  (user-input NIL                    :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots ``tree'', ``memory'' and
   ``user-input'' to eponymous local symbol macros, defines another such
   macro for the active memory cell as ``current-cell'', evaluates the
   BODY forms, and returns the last evaluated form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           ((tree
             (the Node
               (interpreter-tree ,evaluated-interpreter)))
            (memory
             (the Memory
               (interpreter-memory ,evaluated-interpreter)))
            (current-cell
             (the integer
               (memory-current-cell
                 (interpreter-memory ,evaluated-interpreter))))
            (user-input
             (the (or null character)
               (interpreter-user-input ,evaluated-interpreter))))
         (declare (type Node                tree))
         (declare (type Memory              memory))
         (declare (type integer             current-cell))
         (declare (type (or null character) user-input))
         (declare (ignorable                tree))
         (declare (ignorable                memory))
         (declare (ignorable                current-cell))
         (declare (ignorable                user-input))
         ,@body))))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE employing the INTERPRETER as its visitor, with
     the concrete method implementation dispatching on the NODE-TYPE,
     usually a keyword symbol equal to the NODE's type."))

;;; -------------------------------------------------------

(defun interpreter-visit-node (interpreter node)
  "Visits the NODE using the INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (interpreter-dispatch-node interpreter (node-type node) node)
  (values))

;;; -------------------------------------------------------

(defmacro define-node-dispatch (node-type
                                (interpreter-variable node-variable)
                                &body body)
  "Implements the ``interpreter-dispatch-node'' generic function by
   providing as its three arguments the INTERPRETER-VARIABLE, the
   node-type, hecht in a fixed manner as ``node-type'' dispatching on
   NODE-TYPE object, and the NODE-VARIABLE, utilizing the BODY forms,
   and returning no value.
   ---
   The thus generated code amounts to
   
     (defmethod interpreter-dispatch-node
         ((INTERPRETER-VARIABLE Interpreter)
          (node-type            (eql NODE-TYPE))
          (NODE-VARIABLE        Node))
       BODY)
   ---
   A valid invocation of this macro would follow a model such as this:
   
     (define-node-dispatch :my-node-type (my-interpreter my-node)
       (interpreter-visit-node my-interpreter
         (node-attribute my-node :value)))"
  `(defmethod interpreter-dispatch-node
       ((,interpreter-variable Interpreter)
        (node-type             (eql ,node-type))
        (,node-variable        Node))
     (declare (type Interpreter ,interpreter-variable))
     (declare (type keyword     node-type))
     (declare (type Node        ,node-variable))
     (declare (ignorable        ,interpreter-variable))
     (declare (ignorable        node-type))
     (declare (ignorable        ,node-variable))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-node-dispatch :program (interpreter node)
  (dolist (statement (node-attribute node :statements))
    (declare (type Node statement))
    (interpreter-visit-node interpreter statement)))

;;; -------------------------------------------------------

(define-node-dispatch :print-block (interpreter node)
  (dolist (argument (node-attribute node :arguments))
    (declare (type Node argument))
    (interpreter-visit-node interpreter argument)))

;;; -------------------------------------------------------

(define-node-dispatch :print-letter (interpreter node)
  (write-char
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :print-user-input (interpreter node)
  (with-interpreter (interpreter)
    (write-char
      (code-char current-cell))))

;;; -------------------------------------------------------

(define-node-dispatch :take-input (interpreter node)
  (with-interpreter (interpreter)
    (format T "~&Please enter an ASCII character: ")
    (setf user-input (read-char NIL NIL NIL))
    (clear-input)))

;;; -------------------------------------------------------

(define-node-dispatch :store-input (interpreter node)
  (with-interpreter (interpreter)
    (if user-input
      (setf current-cell (char-code user-input))
      (error "No user input committed yet."))))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the so simple dollar instructions subjected to the
   INTERPRETER's castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (interpreter-visit-node interpreter tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-so-simple-dollar (code)
  "Interprets the piece of so simple dollar CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "helloworld" to the standard output.
(interpret-so-simple-dollar
  "
  $
  
  $$$
  $$$$$$$$$$$
  $$$$$$$$
  $$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$
  $$$$$$$
  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  $$
  ")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-so-simple-dollar
  "
  $
  
  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  $$$
  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  $$
  ")
